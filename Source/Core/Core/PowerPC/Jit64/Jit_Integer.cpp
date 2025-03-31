// Copyright 2008 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "Core/PowerPC/Jit64/Jit.h"

#include <array>
#include <bit>
#include <limits>

#include "Common/Assert.h"
#include "Common/BitUtils.h"
#include "Common/CPUDetect.h"
#include "Common/CommonTypes.h"
#include "Common/MathUtil.h"
#include "Common/SmallVector.h"
#include "Common/x64Emitter.h"

#include "Core/CoreTiming.h"
#include "Core/PowerPC/ConditionRegister.h"
#include "Core/PowerPC/Interpreter/ExceptionUtils.h"
#include "Core/PowerPC/Interpreter/Interpreter.h"
#include "Core/PowerPC/Jit64/RegCache/JitRegCache.h"
#include "Core/PowerPC/Jit64Common/Jit64PowerPCState.h"
#include "Core/PowerPC/JitCommon/DivUtils.h"
#include "Core/PowerPC/PPCAnalyst.h"
#include "Core/PowerPC/PowerPC.h"

using namespace Gen;
using namespace JitCommon;

void Jit64::GenerateConstantOverflow(s64 val)
{
  GenerateConstantOverflow(val > std::numeric_limits<s32>::max() ||
                           val < std::numeric_limits<s32>::min());
}

void Jit64::GenerateConstantOverflow(bool overflow)
{
  if (overflow)
  {
    // XER[OV/SO] = 1
    MOV(8, PPCSTATE(xer_so_ov), Imm8(XER_OV_MASK | XER_SO_MASK));
  }
  else
  {
    // XER[OV] = 0
    AND(8, PPCSTATE(xer_so_ov), Imm8(~XER_OV_MASK));
  }
}

// We could do overflow branchlessly, but unlike carry it seems to be quite a bit rarer.
void Jit64::GenerateOverflow(Gen::CCFlags cond)
{
  FixupBranch jno = J_CC(cond);
  // XER[OV/SO] = 1
  MOV(8, PPCSTATE(xer_so_ov), Imm8(XER_OV_MASK | XER_SO_MASK));
  FixupBranch exit = J();
  SetJumpTarget(jno);
  // XER[OV] = 0
  // We need to do this without modifying flags so as not to break stuff that assumes flags
  // aren't clobbered (carry, branch merging): speed doesn't really matter here (this is really
  // rare).
  static const std::array<u8, 4> OVTABLE = {{0, 0, XER_SO_MASK, XER_SO_MASK}};
  MOVZX(32, 8, RSCRATCH, PPCSTATE(xer_so_ov));
  LEA(64, RSCRATCH2, MConst(OVTABLE));
  MOV(8, R(RSCRATCH), MRegSum(RSCRATCH, RSCRATCH2));
  MOV(8, PPCSTATE(xer_so_ov), R(RSCRATCH));
  SetJumpTarget(exit);
}

void Jit64::FinalizeCarry(CCFlags cond)
{
  js.carryFlag = CarryFlag::InPPCState;
  if (js.op->wantsCA)
  {
    // Not actually merging instructions, but the effect is equivalent (we can't have
    // breakpoints/etc in between).
    if (CanMergeNextInstructions(1) && js.op[1].wantsCAInFlags)
    {
      if (cond == CC_C)
      {
        js.carryFlag = CarryFlag::InHostCarry;
      }
      else if (cond == CC_NC)
      {
        js.carryFlag = CarryFlag::InHostCarryInverted;
      }
      else
      {
        // convert the condition to a carry flag (is there a better way?)
        SETcc(cond, R(RSCRATCH));
        SHR(8, R(RSCRATCH), Imm8(1));
        js.carryFlag = CarryFlag::InHostCarry;
      }
      LockFlags();
    }
    else
    {
      JitSetCAIf(cond);
    }
  }
}

// Unconditional version
void Jit64::FinalizeCarry(bool ca)
{
  js.carryFlag = CarryFlag::InPPCState;
  if (js.op->wantsCA)
  {
    if (CanMergeNextInstructions(1) && js.op[1].wantsCAInFlags)
    {
      if (ca)
        STC();
      else
        CLC();
      LockFlags();
      js.carryFlag = CarryFlag::InHostCarry;
    }
    else if (ca)
    {
      JitSetCA();
    }
    else
    {
      JitClearCA();
    }
  }
}

void Jit64::FinalizeCarryOverflow(bool oe, bool inv)
{
  if (oe)
  {
    GenerateOverflow();
  }
  // Do carry
  FinalizeCarry(inv ? CC_NC : CC_C);
}

// Be careful; only set needs_test to false if we can be absolutely sure flags don't need
// to be recalculated and haven't been clobbered. Keep in mind not all instructions set
// sufficient flags -- for example, the flags from SHL/SHR are *not* sufficient for LT/GT
// branches, only EQ.
// The flags from any instruction that may set OF (such as ADD/SUB) can not be used for
// LT/GT either.
void Jit64::ComputeRC(preg_t preg, bool needs_test, bool needs_sext)
{
  RCOpArg arg = gpr.Use(preg, RCMode::Read);
  RegCache::Realize(arg);

  if (arg.IsImm())
  {
    MOV(64, PPCSTATE_CR(0), Imm32(arg.SImm32()));
  }
  else if (needs_sext)
  {
    MOVSX(64, 32, RSCRATCH, arg);
    MOV(64, PPCSTATE_CR(0), R(RSCRATCH));
  }
  else
  {
    MOV(64, PPCSTATE_CR(0), arg);
  }

  if (CheckMergedBranch(0))
  {
    if (arg.IsImm())
    {
      s32 offset = arg.SImm32();
      arg.Unlock();
      DoMergedBranchImmediate(offset);
    }
    else
    {
      if (needs_test)
      {
        TEST(32, arg, arg);
        arg.Unlock();
      }
      else
      {
        // If an operand to the cmp/rc op we're merging with the branch isn't used anymore, it'd be
        // better to flush it here so that we don't have to flush it on both sides of the branch.
        // We don't want to do this if a test is needed though, because it would interrupt macro-op
        // fusion.
        arg.Unlock();
        gpr.Flush(~js.op->gprInUse);
      }
      DoMergedBranchCondition();
    }
  }
}

// we can't do this optimization in the emitter because MOVZX and AND have different effects on
// flags.
void Jit64::AndWithMask(X64Reg reg, u32 mask)
{
  if (mask == 0xffffffff)
    return;

  if (mask == 0)
    XOR(32, R(reg), R(reg));
  else if (mask == 0xff)
    MOVZX(32, 8, reg, R(reg));
  else if (mask == 0xffff)
    MOVZX(32, 16, reg, R(reg));
  else
    AND(32, R(reg), Imm32(mask));
}

void Jit64::RotateLeft(int bits, X64Reg regOp, const OpArg& arg, u8 rotate)
{
  const bool is_same_reg = arg.IsSimpleReg(regOp);

  if (cpu_info.bBMI2 && !is_same_reg && rotate != 0)
  {
    RORX(bits, regOp, arg, bits - rotate);
    return;
  }

  if (!is_same_reg)
  {
    MOV(bits, R(regOp), arg);
  }

  if (rotate != 0)
  {
    ROL(bits, R(regOp), Imm8(rotate));
  }
}

// Following static functions are used in conjunction with regimmop
static u32 Add(u32 a, u32 b)
{
  return a + b;
}

static u32 Or(u32 a, u32 b)
{
  return a | b;
}

static u32 And(u32 a, u32 b)
{
  return a & b;
}

static u32 Xor(u32 a, u32 b)
{
  return a ^ b;
}

void Jit64::regimmop(int d, int a, bool binary, u32 value, Operation doop,
                     void (XEmitter::*op)(int, const OpArg&, const OpArg&), bool Rc, bool carry)
{
  bool needs_test = doop == Add;
  // Be careful; addic treats r0 as r0, but addi treats r0 as zero.
  if (a || binary || carry)
  {
    carry &= js.op->wantsCA;
    if (gpr.IsImm(a) && !carry)
    {
      gpr.SetImmediate32(d, doop(gpr.Imm32(a), value));
    }
    else
    {
      RCOpArg ra = gpr.Use(a, RCMode::Read);
      RCX64Reg rd = gpr.Bind(d, RCMode::Write);
      RegCache::Realize(ra, rd);
      if (doop == Add && ra.IsSimpleReg() && !carry && d != a)
      {
        LEA(32, rd, MDisp(ra.GetSimpleReg(), value));
      }
      else
      {
        if (d != a)
          MOV(32, rd, ra);
        (this->*op)(32, rd, Imm32(value));  // m_GPR[d] = m_GPR[_inst.RA] + _inst.SIMM_16;
      }
    }
    if (carry)
      FinalizeCarry(CC_C);
  }
  else if (doop == Add)
  {
    // a == 0, which for these instructions imply value = 0
    gpr.SetImmediate32(d, value);
  }
  else
  {
    ASSERT_MSG(DYNA_REC, 0, "WTF regimmop");
  }
  if (Rc)
    ComputeRC(d, needs_test, doop != And || (value & 0x80000000));
}

void Jit64::reg_imm(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITIntegerOff);
  u32 d = inst.RD, a = inst.RA, s = inst.RS;
  switch (inst.OPCD)
  {
  case 14:  // addi
    // occasionally used as MOV - emulate, with immediate propagation
    if (a != 0 && d != a && gpr.IsImm(a))
    {
      gpr.SetImmediate32(d, gpr.Imm32(a) + (u32)(s32)inst.SIMM_16);
    }
    else if (a != 0 && d != a && inst.SIMM_16 == 0)
    {
      RCOpArg ra = gpr.Use(a, RCMode::Read);
      RCX64Reg rd = gpr.Bind(d, RCMode::Write);
      RegCache::Realize(ra, rd);
      MOV(32, rd, ra);
    }
    else
    {
      regimmop(d, a, false, (u32)(s32)inst.SIMM_16, Add, &XEmitter::ADD);  // addi
    }
    break;
  case 15:  // addis
    regimmop(d, a, false, (u32)inst.SIMM_16 << 16, Add, &XEmitter::ADD);
    break;
  case 24:  // ori
  case 25:  // oris
  {
    // check for nop
    if (a == s && inst.UIMM == 0)
    {
      // Make the nop visible in the generated code. not much use but interesting if we see one.
      NOP();
      return;
    }

    const u32 immediate = inst.OPCD == 24 ? inst.UIMM : inst.UIMM << 16;
    regimmop(a, s, true, immediate, Or, &XEmitter::OR);
    break;
  }
  case 28:  // andi
    regimmop(a, s, true, inst.UIMM, And, &XEmitter::AND, true);
    break;
  case 29:  // andis
    regimmop(a, s, true, inst.UIMM << 16, And, &XEmitter::AND, true);
    break;
  case 26:  // xori
  case 27:  // xoris
  {
    if (s == a && inst.UIMM == 0)
    {
      // Make the nop visible in the generated code.
      NOP();
      return;
    }

    const u32 immediate = inst.OPCD == 26 ? inst.UIMM : inst.UIMM << 16;
    regimmop(a, s, true, immediate, Xor, &XEmitter::XOR, false);
    break;
  }
  case 12:  // addic
    regimmop(d, a, false, (u32)(s32)inst.SIMM_16, Add, &XEmitter::ADD, false, true);
    break;
  case 13:  // addic_rc
    regimmop(d, a, true, (u32)(s32)inst.SIMM_16, Add, &XEmitter::ADD, true, true);
    break;
  default:
    FALLBACK_IF(true);
  }
}

bool Jit64::CheckMergedBranch(u32 crf) const
{
  if (!analyzer.HasOption(PPCAnalyst::PPCAnalyzer::OPTION_BRANCH_MERGE))
    return false;

  if (!CanMergeNextInstructions(1))
    return false;

  const UGeckoInstruction& next = js.op[1].inst;
  return (((next.OPCD == 16 /* bcx */) ||
           ((next.OPCD == 19) && (next.SUBOP10 == 528) /* bcctrx */) ||
           ((next.OPCD == 19) && (next.SUBOP10 == 16) /* bclrx */)) &&
          (next.BO & BO_DONT_DECREMENT_FLAG) && !(next.BO & BO_DONT_CHECK_CONDITION) &&
          static_cast<u32>(next.BI >> 2) == crf);
}

void Jit64::DoMergedBranch()
{
  // Code that handles successful PPC branching.
  const UGeckoInstruction& next = js.op[1].inst;
  const u32 next_pc = js.op[1].address;

  if (js.op[1].branchIsIdleLoop)
  {
    if (next.LK)
      MOV(32, PPCSTATE_SPR(SPR_LR), Imm32(next_pc + 4));

    const u32 destination = js.op[1].branchTo;
    if (IsDebuggingEnabled())
    {
      // ABI_PARAM1 is safe to use after a GPR flush for an optimization in this function.
      WriteBranchWatch<true>(next_pc, destination, next, ABI_PARAM1, RSCRATCH, {});
    }
    WriteIdleExit(destination);
  }
  else if (next.OPCD == 16)  // bcx
  {
    if (next.LK)
      MOV(32, PPCSTATE_SPR(SPR_LR), Imm32(next_pc + 4));

    const u32 destination = js.op[1].branchTo;
    if (IsDebuggingEnabled())
    {
      // ABI_PARAM1 is safe to use after a GPR flush for an optimization in this function.
      WriteBranchWatch<true>(next_pc, destination, next, ABI_PARAM1, RSCRATCH, {});
    }
    WriteExit(destination, next.LK, next_pc + 4);
  }
  else if ((next.OPCD == 19) && (next.SUBOP10 == 528))  // bcctrx
  {
    if (next.LK)
      MOV(32, PPCSTATE_SPR(SPR_LR), Imm32(next_pc + 4));
    MOV(32, R(RSCRATCH), PPCSTATE_SPR(SPR_CTR));
    AND(32, R(RSCRATCH), Imm32(0xFFFFFFFC));
    if (IsDebuggingEnabled())
    {
      // ABI_PARAM1 is safe to use after a GPR flush for an optimization in this function.
      WriteBranchWatchDestInRSCRATCH(next_pc, next, ABI_PARAM1, RSCRATCH2, BitSet32{RSCRATCH});
    }
    WriteExitDestInRSCRATCH(next.LK, next_pc + 4);
  }
  else if ((next.OPCD == 19) && (next.SUBOP10 == 16))  // bclrx
  {
    MOV(32, R(RSCRATCH), PPCSTATE_SPR(SPR_LR));
    if (!m_enable_blr_optimization)
      AND(32, R(RSCRATCH), Imm32(0xFFFFFFFC));
    if (next.LK)
      MOV(32, PPCSTATE_SPR(SPR_LR), Imm32(next_pc + 4));
    if (IsDebuggingEnabled())
    {
      // ABI_PARAM1 is safe to use after a GPR flush for an optimization in this function.
      WriteBranchWatchDestInRSCRATCH(next_pc, next, ABI_PARAM1, RSCRATCH2, BitSet32{RSCRATCH});
    }
    WriteBLRExit();
  }
  else
  {
    PanicAlertFmt("WTF invalid branch");
  }
}

void Jit64::DoMergedBranchCondition()
{
  js.downcountAmount++;
  js.skipInstructions = 1;
  const UGeckoInstruction& next = js.op[1].inst;
  int test_bit = 3 - (next.BI & 3);
  bool condition = !!(next.BO & BO_BRANCH_IF_TRUE);
  const u32 next_pc = js.op[1].address;

  ASSERT(gpr.IsAllUnlocked());

  FixupBranch p_dont_branch;
  switch (test_bit)
  {
  case PowerPC::CR_LT_BIT:
    // Test < 0, so jump over if >= 0.
    p_dont_branch = J_CC(condition ? CC_GE : CC_L, Jump::Near);
    break;
  case PowerPC::CR_GT_BIT:
    // Test > 0, so jump over if <= 0.
    p_dont_branch = J_CC(condition ? CC_LE : CC_G, Jump::Near);
    break;
  case PowerPC::CR_EQ_BIT:
    // Test = 0, so jump over if != 0.
    p_dont_branch = J_CC(condition ? CC_NE : CC_E, Jump::Near);
    break;
  case PowerPC::CR_SO_BIT:
    // SO bit, do not branch (we don't emulate SO for cmp).
    p_dont_branch = J(Jump::Near);
    break;
  }

  {
    RCForkGuard gpr_guard = gpr.Fork();
    RCForkGuard fpr_guard = fpr.Fork();

    gpr.Flush();
    fpr.Flush();

    DoMergedBranch();
  }

  SetJumpTarget(p_dont_branch);

  if (!analyzer.HasOption(PPCAnalyst::PPCAnalyzer::OPTION_CONDITIONAL_CONTINUE))
  {
    gpr.Flush();
    fpr.Flush();
    if (IsDebuggingEnabled())
    {
      // ABI_PARAM1 is safe to use after a GPR flush for an optimization in this function.
      WriteBranchWatch<false>(next_pc, next_pc + 4, next, ABI_PARAM1, RSCRATCH, {});
    }
    WriteExit(next_pc + 4);
  }
  else if (IsDebuggingEnabled())
  {
    WriteBranchWatch<false>(next_pc, next_pc + 4, next, RSCRATCH, RSCRATCH2,
                            CallerSavedRegistersInUse());
  }
}

void Jit64::DoMergedBranchImmediate(s64 val)
{
  js.downcountAmount++;
  js.skipInstructions = 1;
  const UGeckoInstruction& next = js.op[1].inst;
  int test_bit = 3 - (next.BI & 3);
  bool condition = !!(next.BO & BO_BRANCH_IF_TRUE);
  const u32 next_pc = js.op[1].address;

  ASSERT(gpr.IsAllUnlocked());

  bool branch = false;
  switch (test_bit)
  {
  case PowerPC::CR_LT_BIT:
    branch = condition ? val < 0 : val >= 0;
    break;
  case PowerPC::CR_GT_BIT:
    branch = condition ? val > 0 : val <= 0;
    break;
  case PowerPC::CR_EQ_BIT:
    branch = condition ? val == 0 : val != 0;
    break;
  case PowerPC::CR_SO_BIT:
    // SO bit, do not branch (we don't emulate SO for cmp).
    break;
  }

  if (branch)
  {
    gpr.Flush();
    fpr.Flush();
    DoMergedBranch();
  }
  else if (!analyzer.HasOption(PPCAnalyst::PPCAnalyzer::OPTION_CONDITIONAL_CONTINUE))
  {
    gpr.Flush();
    fpr.Flush();
    if (IsDebuggingEnabled())
    {
      // ABI_PARAM1 is safe to use after a GPR flush for an optimization in this function.
      WriteBranchWatch<false>(next_pc, next_pc + 4, next, ABI_PARAM1, RSCRATCH, {});
    }
    WriteExit(next_pc + 4);
  }
  else if (IsDebuggingEnabled())
  {
    WriteBranchWatch<false>(next_pc, next_pc + 4, next, RSCRATCH, RSCRATCH2,
                            CallerSavedRegistersInUse());
  }
}

void Jit64::cmpXX(UGeckoInstruction inst)
{
  // USES_CR
  INSTRUCTION_START
  JITDISABLE(bJITIntegerOff);
  int a = inst.RA;
  int b = inst.RB;
  u32 crf = inst.CRFD;
  bool merge_branch = CheckMergedBranch(crf);

  bool signed_compare;
  RCOpArg comparand;
  switch (inst.OPCD)
  {
  // cmp / cmpl
  case 31:
    signed_compare = (inst.SUBOP10 == 0);
    comparand = signed_compare ? gpr.Use(b, RCMode::Read) : gpr.Bind(b, RCMode::Read);
    RegCache::Realize(comparand);
    break;

  // cmpli
  case 10:
    signed_compare = false;
    comparand = RCOpArg::Imm32((u32)inst.UIMM);
    break;

  // cmpi
  case 11:
    signed_compare = true;
    comparand = RCOpArg::Imm32((u32)(s32)(s16)inst.UIMM);
    break;

  default:
    signed_compare = false;  // silence compiler warning
    PanicAlertFmt("cmpXX");
  }

  if (gpr.IsImm(a) && comparand.IsImm())
  {
    // Both registers contain immediate values, so we can pre-compile the compare result
    s64 compare_result = signed_compare ? (s64)gpr.SImm32(a) - (s64)comparand.SImm32() :
                                        (u64)gpr.Imm32(a) - (u64)comparand.Imm32();
    if (compare_result == (s32)compare_result)
    {
      MOV(64, PPCSTATE_CR(crf), Imm32((u32)compare_result));
    }
    else
    {
      MOV(64, R(RSCRATCH), Imm64(compare_result));
      MOV(64, PPCSTATE_CR(crf), R(RSCRATCH));
    }

    if (merge_branch)
    {
      RegCache::Unlock(comparand);
      DoMergedBranchImmediate(compare_result);
    }

    return;
  }

  if (!gpr.IsImm(a) && !signed_compare && comparand.IsImm() && comparand.Imm32() == 0)
  {
    RCX64Reg ra = gpr.Bind(a, RCMode::Read);
    RegCache::Realize(ra);

    MOV(64, PPCSTATE_CR(crf), ra);
    if (merge_branch)
    {
      TEST(64, ra, ra);
      RegCache::Unlock(comparand, ra);
      DoMergedBranchCondition();
    }
    return;
  }

  const X64Reg input = RSCRATCH;
  if (gpr.IsImm(a))
  {
    if (signed_compare)
      MOV(64, R(input), Imm32(gpr.SImm32(a)));
    else
      MOV(32, R(input), Imm32(gpr.Imm32(a)));
  }
  else
  {
    RCOpArg ra = gpr.Use(a, RCMode::Read);
    RegCache::Realize(ra);
    if (signed_compare)
      MOVSX(64, 32, input, ra);
    else
      MOVZX(64, 32, input, ra);
  }

  if (comparand.IsImm())
  {
    // sign extension will ruin this, so store it in a register
    if (!signed_compare && (comparand.Imm32() & 0x80000000U) != 0)
    {
      MOV(32, R(RSCRATCH2), comparand);
      comparand = RCOpArg::R(RSCRATCH2);
    }
  }
  else
  {
    if (signed_compare)
    {
      MOVSX(64, 32, RSCRATCH2, comparand);
      comparand = RCOpArg::R(RSCRATCH2);
    }
  }

  if (comparand.IsImm() && comparand.Imm32() == 0)
  {
    MOV(64, PPCSTATE_CR(crf), R(input));
    // Place the comparison next to the branch for macro-op fusion
    if (merge_branch)
      TEST(64, R(input), R(input));
  }
  else
  {
    SUB(64, R(input), comparand);
    MOV(64, PPCSTATE_CR(crf), R(input));
  }

  if (merge_branch)
  {
    RegCache::Unlock(comparand);
    DoMergedBranchCondition();
  }
}

void Jit64::boolX(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITIntegerOff);
  int a = inst.RA, s = inst.RS, b = inst.RB;
  bool needs_test = false;
  DEBUG_ASSERT_MSG(DYNA_REC, inst.OPCD == 31, "Invalid boolX");

  if (gpr.IsImm(s, b))
  {
    const u32 rs_offset = gpr.Imm32(s);
    const u32 rb_offset = gpr.Imm32(b);

    if (inst.SUBOP10 == 28)  // andx
      gpr.SetImmediate32(a, rs_offset & rb_offset);
    else if (inst.SUBOP10 == 476)  // nandx
      gpr.SetImmediate32(a, ~(rs_offset & rb_offset));
    else if (inst.SUBOP10 == 60)  // andcx
      gpr.SetImmediate32(a, rs_offset & (~rb_offset));
    else if (inst.SUBOP10 == 444)  // orx
      gpr.SetImmediate32(a, rs_offset | rb_offset);
    else if (inst.SUBOP10 == 124)  // norx
      gpr.SetImmediate32(a, ~(rs_offset | rb_offset));
    else if (inst.SUBOP10 == 412)  // orcx
      gpr.SetImmediate32(a, rs_offset | (~rb_offset));
    else if (inst.SUBOP10 == 316)  // xorx
      gpr.SetImmediate32(a, rs_offset ^ rb_offset);
    else if (inst.SUBOP10 == 284)  // eqvx
      gpr.SetImmediate32(a, ~(rs_offset ^ rb_offset));
  }
  else if (gpr.IsImm(s) || gpr.IsImm(b))
  {
    const auto [i, j] = gpr.IsImm(s) ? std::pair(s, b) : std::pair(b, s);
    u32 imm = gpr.Imm32(i);

    bool complement_b = (inst.SUBOP10 == 60 /* andcx */) || (inst.SUBOP10 == 412 /* orcx */);
    const bool final_not = (inst.SUBOP10 == 476 /* nandx */) || (inst.SUBOP10 == 124 /* norx */);
    const bool is_and = (inst.SUBOP10 == 28 /* andx */) || (inst.SUBOP10 == 60 /* andcx */) ||
                        (inst.SUBOP10 == 476 /* nandx */);
    const bool is_or = (inst.SUBOP10 == 444 /* orx */) || (inst.SUBOP10 == 412 /* orcx */) ||
                       (inst.SUBOP10 == 124 /* norx */);
    const bool is_xor = (inst.SUBOP10 == 316 /* xorx */) || (inst.SUBOP10 == 284 /* eqvx */);

    // Precompute complement when possible
    if ((complement_b && gpr.IsImm(b)) || (inst.SUBOP10 == 284 /* eqvx */))
    {
      imm = ~imm;
      complement_b = false;
    }

    if (is_xor)
    {
      RCOpArg rj = gpr.Use(j, RCMode::Read);
      RCX64Reg ra = gpr.Bind(a, RCMode::Write);
      RegCache::Realize(rj, ra);
      if (imm == 0)
      {
        if (a != j)
          MOV(32, ra, rj);
        needs_test = true;
      }
      else if (imm == 0xFFFFFFFF && !inst.Rc)
      {
        if (a != j)
          MOV(32, ra, rj);
        NOT(32, ra);
      }
      else if (a == j)
      {
        XOR(32, ra, Imm32(imm));
      }
      else if (s32(imm) >= -128 && s32(imm) <= 127)
      {
        MOV(32, ra, rj);
        XOR(32, ra, Imm32(imm));
      }
      else
      {
        MOV(32, ra, Imm32(imm));
        XOR(32, ra, rj);
      }
    }
    else if (is_and)
    {
      if (imm == 0)
      {
        gpr.SetImmediate32(a, final_not ? 0xFFFFFFFF : 0);
      }
      else
      {
        RCOpArg rj = gpr.Use(j, RCMode::Read);
        RCX64Reg ra = gpr.Bind(a, RCMode::Write);
        RegCache::Realize(rj, ra);

        if (imm == 0xFFFFFFFF)
        {
          if (a != j)
            MOV(32, ra, rj);
          if (final_not || complement_b)
            NOT(32, ra);
          needs_test = true;
        }
        else if (complement_b)
        {
          if (a != j)
            MOV(32, ra, rj);
          NOT(32, ra);
          AND(32, ra, Imm32(imm));
        }
        else
        {
          if (a == j)
          {
            AND(32, ra, Imm32(imm));
          }
          else if (s32(imm) >= -128 && s32(imm) <= 127)
          {
            MOV(32, ra, rj);
            AND(32, ra, Imm32(imm));
          }
          else
          {
            MOV(32, ra, Imm32(imm));
            AND(32, ra, rj);
          }

          if (final_not)
          {
            NOT(32, ra);
            needs_test = true;
          }
        }
      }
    }
    else if (is_or)
    {
      RCOpArg rj = gpr.Use(j, RCMode::Read);
      RCX64Reg ra = gpr.Bind(a, RCMode::Write);
      RegCache::Realize(rj, ra);

      if (imm == 0)
      {
        if (a != j)
          MOV(32, ra, rj);
        if (final_not || complement_b)
          NOT(32, ra);
        needs_test = true;
      }
      else if (complement_b)
      {
        if (a != j)
          MOV(32, ra, rj);
        NOT(32, ra);
        OR(32, ra, Imm32(imm));
      }
      else
      {
        if (a == j)
        {
          OR(32, ra, Imm32(imm));
        }
        else if (s32(imm) >= -128 && s32(imm) <= 127)
        {
          MOV(32, ra, rj);
          OR(32, ra, Imm32(imm));
        }
        else
        {
          MOV(32, ra, Imm32(imm));
          OR(32, ra, rj);
        }

        if (final_not)
        {
          NOT(32, ra);
          needs_test = true;
        }
      }
    }
    else
    {
      PanicAlertFmt("WTF!");
    }
  }
  else if (s == b)
  {
    if ((inst.SUBOP10 == 28 /* andx */) || (inst.SUBOP10 == 444 /* orx */))
    {
      if (a != s)
      {
        RCOpArg rs = gpr.Use(s, RCMode::Read);
        RCX64Reg ra = gpr.Bind(a, RCMode::Write);
        RegCache::Realize(rs, ra);
        MOV(32, ra, rs);
      }
      else if (inst.Rc)
      {
        gpr.Bind(a, RCMode::Read).Realize();
      }
      needs_test = true;
    }
    else if ((inst.SUBOP10 == 476 /* nandx */) || (inst.SUBOP10 == 124 /* norx */))
    {
      if (a == s && !inst.Rc)
      {
        RCOpArg ra = gpr.UseNoImm(a, RCMode::ReadWrite);
        RegCache::Realize(ra);
        NOT(32, ra);
      }
      else
      {
        RCOpArg rs = gpr.Use(s, RCMode::Read);
        RCX64Reg ra = gpr.Bind(a, RCMode::Write);
        RegCache::Realize(rs, ra);
        MOV(32, ra, rs);
        NOT(32, ra);
      }
      needs_test = true;
    }
    else if ((inst.SUBOP10 == 412 /* orcx */) || (inst.SUBOP10 == 284 /* eqvx */))
    {
      gpr.SetImmediate32(a, 0xFFFFFFFF);
    }
    else if ((inst.SUBOP10 == 60 /* andcx */) || (inst.SUBOP10 == 316 /* xorx */))
    {
      gpr.SetImmediate32(a, 0);
    }
    else
    {
      PanicAlertFmt("WTF!");
    }
  }
  else if ((a == s) || (a == b))
  {
    RCOpArg rb = gpr.Use(b, RCMode::Read);
    RCOpArg rs = gpr.Use(s, RCMode::Read);
    RCOpArg operand = gpr.Use(a == s ? b : s, RCMode::Read);
    RCX64Reg ra = gpr.Bind(a, RCMode::ReadWrite);
    RegCache::Realize(rb, rs, operand, ra);

    if (inst.SUBOP10 == 28)  // andx
    {
      AND(32, ra, operand);
    }
    else if (inst.SUBOP10 == 476)  // nandx
    {
      AND(32, ra, operand);
      NOT(32, ra);
      needs_test = true;
    }
    else if (inst.SUBOP10 == 60)  // andcx
    {
      if (cpu_info.bBMI1 && rb.IsSimpleReg())
      {
        ANDN(32, ra, rb.GetSimpleReg(), rs);
      }
      else if (a == b)
      {
        NOT(32, ra);
        AND(32, ra, operand);
      }
      else
      {
        MOV(32, R(RSCRATCH), operand);
        NOT(32, R(RSCRATCH));
        AND(32, ra, R(RSCRATCH));
      }
    }
    else if (inst.SUBOP10 == 444)  // orx
    {
      OR(32, ra, operand);
    }
    else if (inst.SUBOP10 == 124)  // norx
    {
      OR(32, ra, operand);
      NOT(32, ra);
      needs_test = true;
    }
    else if (inst.SUBOP10 == 412)  // orcx
    {
      if (a == b)
      {
        NOT(32, ra);
        OR(32, ra, operand);
      }
      else
      {
        MOV(32, R(RSCRATCH), operand);
        NOT(32, R(RSCRATCH));
        OR(32, ra, R(RSCRATCH));
      }
    }
    else if (inst.SUBOP10 == 316)  // xorx
    {
      XOR(32, ra, operand);
    }
    else if (inst.SUBOP10 == 284)  // eqvx
    {
      NOT(32, ra);
      XOR(32, ra, operand);
    }
    else
    {
      PanicAlertFmt("WTF");
    }
  }
  else
  {
    RCOpArg rb = gpr.Use(b, RCMode::Read);
    RCOpArg rs = gpr.Use(s, RCMode::Read);
    RCX64Reg ra = gpr.Bind(a, RCMode::Write);
    RegCache::Realize(rb, rs, ra);

    if (inst.SUBOP10 == 28)  // andx
    {
      MOV(32, ra, rs);
      AND(32, ra, rb);
    }
    else if (inst.SUBOP10 == 476)  // nandx
    {
      MOV(32, ra, rs);
      AND(32, ra, rb);
      NOT(32, ra);
      needs_test = true;
    }
    else if (inst.SUBOP10 == 60)  // andcx
    {
      if (cpu_info.bBMI1 && rb.IsSimpleReg())
      {
        ANDN(32, ra, rb.GetSimpleReg(), rs);
      }
      else
      {
        MOV(32, ra, rb);
        NOT(32, ra);
        AND(32, ra, rs);
      }
    }
    else if (inst.SUBOP10 == 444)  // orx
    {
      MOV(32, ra, rs);
      OR(32, ra, rb);
    }
    else if (inst.SUBOP10 == 124)  // norx
    {
      MOV(32, ra, rs);
      OR(32, ra, rb);
      NOT(32, ra);
      needs_test = true;
    }
    else if (inst.SUBOP10 == 412)  // orcx
    {
      MOV(32, ra, rb);
      NOT(32, ra);
      OR(32, ra, rs);
    }
    else if (inst.SUBOP10 == 316)  // xorx
    {
      MOV(32, ra, rs);
      XOR(32, ra, rb);
    }
    else if (inst.SUBOP10 == 284)  // eqvx
    {
      MOV(32, ra, rs);
      NOT(32, ra);
      XOR(32, ra, rb);
    }
    else
    {
      PanicAlertFmt("WTF!");
    }
  }
  if (inst.Rc)
    ComputeRC(a, needs_test);
}

void Jit64::extsXx(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITIntegerOff);
  int a = inst.RA, s = inst.RS;
  int size = inst.SUBOP10 == 922 ? 16 : 8;

  if (gpr.IsImm(s))
  {
    gpr.SetImmediate32(a, (u32)(s32)(size == 16 ? (s16)gpr.Imm32(s) : (s8)gpr.Imm32(s)));
  }
  else
  {
    RCOpArg rs = gpr.Use(s, RCMode::Read);
    RCX64Reg ra = gpr.Bind(a, RCMode::Write);
    RegCache::Realize(rs, ra);
    MOVSX(32, size, ra, rs);
  }
  if (inst.Rc)
    ComputeRC(a);
}

void Jit64::subfic(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITIntegerOff);
  int a = inst.RA, d = inst.RD, imm = inst.SIMM_16;

  if (gpr.IsImm(a))
  {
    u32 i = imm, j = gpr.Imm32(a);
    gpr.SetImmediate32(d, i - j);
    FinalizeCarry(j == 0 || (i > j - 1));
    return;
  }

  RCOpArg ra = gpr.Use(a, RCMode::Read);
  RCX64Reg rd = gpr.Bind(d, RCMode::Write);
  RegCache::Realize(ra, rd);

  if (imm == 0)
  {
    if (d != a)
      MOV(32, rd, ra);

    // Flags act exactly like subtracting from 0
    NEG(32, rd);
    // Output carry is inverted
    FinalizeCarry(CC_NC);
  }
  else if (imm == -1)
  {
    if (d != a)
      MOV(32, rd, ra);

    NOT(32, rd);
    // CA is always set in this case
    FinalizeCarry(true);
  }
  else if (d == a)
  {
    NOT(32, rd);
    ADD(32, rd, Imm32(imm + 1));
    // Output carry is normal
    FinalizeCarry(CC_C);
  }
  else
  {
    MOV(32, rd, Imm32(imm));
    SUB(32, rd, ra);
    // Output carry is inverted
    FinalizeCarry(CC_NC);
  }
  // This instruction has no RC flag
}

void Jit64::subfx(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITIntegerOff);
  int a = inst.RA, b = inst.RB, d = inst.RD;
  const bool carry = !(inst.SUBOP10 & (1 << 5));

  if (a == b)
  {
    gpr.SetImmediate32(d, 0);
    if (carry)
      FinalizeCarry(true);
    if (inst.OE)
      GenerateConstantOverflow(false);
  }
  else if (gpr.IsImm(a, b))
  {
    s32 i = gpr.SImm32(b), j = gpr.SImm32(a);
    gpr.SetImmediate32(d, i - j);
    if (carry)
      FinalizeCarry(j == 0 || Interpreter::Helper_Carry((u32)i, 0u - (u32)j));
    if (inst.OE)
      GenerateConstantOverflow((s64)i - (s64)j);
  }
  else if (gpr.IsImm(a))
  {
    s32 j = gpr.SImm32(a);
    RCOpArg rb = gpr.Use(b, RCMode::Read);
    RCX64Reg rd = gpr.Bind(d, RCMode::Write);
    RegCache::Realize(rb, rd);

    if (j == 0)
    {
      if (d != b)
        MOV(32, rd, rb);
      if (carry)
        FinalizeCarry(true);
      if (inst.OE)
        GenerateConstantOverflow(false);
    }
    else if (d == b)
    {
      SUB(32, rd, Imm32(j));
      if (carry)
        FinalizeCarry(CC_NC);
      if (inst.OE)
        GenerateOverflow();
    }
    else if (rb.IsSimpleReg() && !carry && !inst.OE)
    {
      LEA(32, rd, MDisp(rb.GetSimpleReg(), -j));
    }
    else
    {
      MOV(32, rd, rb);
      SUB(32, rd, Imm32(j));
      if (carry)
        FinalizeCarry(CC_NC);
      if (inst.OE)
        GenerateOverflow();
    }
  }
  else if (gpr.IsImm(b) && gpr.Imm32(b) == 0)
  {
    RCOpArg ra = gpr.Use(a, RCMode::Read);
    RCX64Reg rd = gpr.Bind(d, RCMode::Write);
    RegCache::Realize(ra, rd);

    if (d != a)
      MOV(32, rd, ra);
    NEG(32, rd);
    if (carry)
      FinalizeCarry(CC_NC);
    if (inst.OE)
      GenerateOverflow();
  }
  else
  {
    RCOpArg ra = gpr.Use(a, RCMode::Read);
    RCOpArg rb = gpr.Use(b, RCMode::Read);
    RCX64Reg rd = gpr.Bind(d, RCMode::Write);
    RegCache::Realize(ra, rb, rd);

    if (d == a && d != b)
    {
      // special case, because sub isn't reversible
      MOV(32, R(RSCRATCH), ra);
      MOV(32, rd, rb);
      SUB(32, rd, R(RSCRATCH));
    }
    else
    {
      if (d != b)
        MOV(32, rd, rb);
      SUB(32, rd, ra);
    }
    if (carry)
      FinalizeCarry(CC_NC);
    if (inst.OE)
      GenerateOverflow();
  }
  if (inst.Rc)
    ComputeRC(d);
}

void Jit64::MultiplyImmediate(u32 imm, int a, int d, bool overflow)
{
  RCOpArg ra = gpr.Use(a, RCMode::Read);
  RCX64Reg rd = gpr.Bind(d, RCMode::Write);
  RegCache::Realize(ra, rd);

  // simplest cases first
  if (imm == 0)
  {
    XOR(32, rd, rd);
    return;
  }

  if (imm == (u32)-1)
  {
    if (d != a)
      MOV(32, rd, ra);
    NEG(32, rd);
    return;
  }

  // skip these if we need to check overflow flag
  if (!overflow)
  {
    // power of 2; just a shift
    if (MathUtil::IsPow2(imm))
    {
      u32 shift = MathUtil::IntLog2(imm);
      // use LEA if it saves an op
      if (d != a && shift <= 3 && shift >= 1 && ra.IsSimpleReg())
      {
        LEA(32, rd, MScaled(ra.GetSimpleReg(), SCALE_1 << shift, 0));
      }
      else
      {
        if (d != a)
          MOV(32, rd, ra);
        if (shift)
          SHL(32, rd, Imm8(shift));
      }
      return;
    }

    // We could handle factors of 2^N*3, 2^N*5, and 2^N*9 using lea+shl, but testing shows
    // it seems to be slower overall.
    static constexpr std::array<u8, 3> LEA_SCALES{{3, 5, 9}};
    for (size_t i = 0; i < LEA_SCALES.size(); i++)
    {
      if (imm == LEA_SCALES[i] && ra.IsSimpleReg())
      {
        LEA(32, rd, MComplex(ra.GetSimpleReg(), ra.GetSimpleReg(), SCALE_2 << i, 0));
        return;
      }
    }
  }

  // if we didn't find any better options
  IMUL(32, rd, ra, Imm32(imm));
}

void Jit64::mulli(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITIntegerOff);
  int a = inst.RA, d = inst.RD;
  u32 imm = inst.SIMM_16;

  if (gpr.IsImm(a))
  {
    gpr.SetImmediate32(d, gpr.Imm32(a) * imm);
  }
  else
  {
    MultiplyImmediate(imm, a, d, false);
  }
}

void Jit64::mullwx(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITIntegerOff);
  int a = inst.RA, b = inst.RB, d = inst.RD;

  if (gpr.IsImm(a, b))
  {
    s32 i = gpr.SImm32(a), j = gpr.SImm32(b);
    gpr.SetImmediate32(d, i * j);
    if (inst.OE)
      GenerateConstantOverflow((s64)i * (s64)j);
  }
  else if (gpr.IsImm(a) || gpr.IsImm(b))
  {
    u32 imm = gpr.IsImm(a) ? gpr.Imm32(a) : gpr.Imm32(b);
    int src = gpr.IsImm(a) ? b : a;
    MultiplyImmediate(imm, src, d, inst.OE);
    if (inst.OE)
      GenerateOverflow();
  }
  else
  {
    RCOpArg ra = gpr.Use(a, RCMode::Read);
    RCOpArg rb = gpr.Use(b, RCMode::Read);
    RCX64Reg rd = gpr.Bind(d, RCMode::Write);
    RegCache::Realize(ra, rb, rd);

    if (d == a)
    {
      IMUL(32, rd, rb);
    }
    else if (d == b)
    {
      IMUL(32, rd, ra);
    }
    else
    {
      MOV(32, rd, rb);
      IMUL(32, rd, ra);
    }
    if (inst.OE)
      GenerateOverflow();
  }
  if (inst.Rc)
    ComputeRC(d);
}

void Jit64::mulhwXx(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITIntegerOff);
  int a = inst.RA, b = inst.RB, d = inst.RD;
  bool sign = inst.SUBOP10 == 75;

  if (gpr.IsImm(a, b))
  {
    if (sign)
      gpr.SetImmediate32(d, (u32)((u64)(((s64)gpr.SImm32(a) * (s64)gpr.SImm32(b))) >> 32));
    else
      gpr.SetImmediate32(d, (u32)(((u64)gpr.Imm32(a) * (u64)gpr.Imm32(b)) >> 32));
  }
  else if (sign)
  {
    RCOpArg ra = gpr.Use(a, RCMode::Read);
    RCOpArg rb = gpr.UseNoImm(b, RCMode::Read);
    RCX64Reg rd = gpr.Bind(d, RCMode::Write);
    RCX64Reg eax = gpr.Scratch(EAX);
    RCX64Reg edx = gpr.Scratch(EDX);
    RegCache::Realize(ra, rb, rd, eax, edx);

    MOV(32, eax, ra);
    IMUL(32, rb);
    MOV(32, rd, edx);
  }
  else
  {
    // Not faster for signed because we'd need two movsx.
    // We need to bind everything to registers since the top 32 bits need to be zero.
    int src = d == b ? a : b;
    int other = src == b ? a : b;

    RCX64Reg rd = gpr.Bind(d, RCMode::Write);
    RCX64Reg rsrc = gpr.Bind(src, RCMode::Read);
    RCOpArg rother = gpr.Use(other, RCMode::Read);
    RegCache::Realize(rd, rsrc, rother);

    if (other != d)
      MOV(32, rd, rother);
    IMUL(64, rd, rsrc);
    SHR(64, rd, Imm8(32));
  }
  if (inst.Rc)
    ComputeRC(d);
}

void Jit64::divwux(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITIntegerOff);
  int a = inst.RA, b = inst.RB, d = inst.RD;

  if (gpr.IsImm(a, b))
  {
    if (gpr.Imm32(b) == 0)
    {
      gpr.SetImmediate32(d, 0);
      if (inst.OE)
        GenerateConstantOverflow(true);
    }
    else
    {
      gpr.SetImmediate32(d, gpr.Imm32(a) / gpr.Imm32(b));
      if (inst.OE)
        GenerateConstantOverflow(false);
    }
  }
  else if (gpr.IsImm(b))
  {
    u32 divisor = gpr.Imm32(b);
    if (divisor == 0)
    {
      gpr.SetImmediate32(d, 0);
      if (inst.OE)
        GenerateConstantOverflow(true);
    }
    else
    {
      if (MathUtil::IsPow2(divisor))
      {
        u32 shift = MathUtil::IntLog2(divisor);

        RCOpArg ra = gpr.Use(a, RCMode::Read);
        RCX64Reg rd = gpr.Bind(d, RCMode::Write);
        RegCache::Realize(ra, rd);

        if (d != a)
          MOV(32, rd, ra);
        if (shift)
          SHR(32, rd, Imm8(shift));
      }
      else
      {
        UnsignedMagic m = UnsignedDivisionConstants(divisor);

        // Test for failure in round-up method
        if (!m.fast)
        {
          // If failed, use slower round-down method
          RCOpArg ra = gpr.Use(a, RCMode::Read);
          RCX64Reg rd = gpr.Bind(d, RCMode::Write);
          RegCache::Realize(ra, rd);

          MOV(32, R(RSCRATCH), Imm32(m.multiplier));
          if (d != a)
            MOV(32, rd, ra);
          IMUL(64, rd, R(RSCRATCH));
          ADD(64, rd, R(RSCRATCH));
          SHR(64, rd, Imm8(m.shift + 32));
        }
        else
        {
          // If success, use faster round-up method
          RCX64Reg ra = gpr.Bind(a, RCMode::Read);
          RCX64Reg rd = gpr.Bind(d, RCMode::Write);
          RegCache::Realize(ra, rd);

          // Three-operand IMUL sign extends the immediate to 64 bits, so we may only
          // use it when the magic number has its most significant bit set to 0
          if ((m.multiplier & 0x80000000) == 0)
          {
            IMUL(64, rd, ra, Imm32(m.multiplier));
          }
          else if (d == a)
          {
            MOV(32, R(RSCRATCH), Imm32(m.multiplier));
            IMUL(64, rd, R(RSCRATCH));
          }
          else
          {
            MOV(32, rd, Imm32(m.multiplier));
            IMUL(64, rd, ra);
          }
          SHR(64, rd, Imm8(m.shift + 32));
        }
      }
      if (inst.OE)
        GenerateConstantOverflow(false);
    }
  }
  else
  {
    RCOpArg ra = gpr.Use(a, RCMode::Read);
    RCX64Reg rb = gpr.Bind(b, RCMode::Read);
    RCX64Reg rd = gpr.Bind(d, RCMode::Write);
    // no register choice (do we need to do this?)
    RCX64Reg eax = gpr.Scratch(EAX);
    RCX64Reg edx = gpr.Scratch(EDX);
    RegCache::Realize(ra, rb, rd, eax, edx);

    MOV(32, eax, ra);
    XOR(32, edx, edx);
    TEST(32, rb, rb);
    FixupBranch not_div_by_zero = J_CC(CC_NZ);
    MOV(32, rd, edx);
    if (inst.OE)
    {
      GenerateConstantOverflow(true);
    }
    FixupBranch end = J();
    SetJumpTarget(not_div_by_zero);
    DIV(32, rb);
    MOV(32, rd, eax);
    if (inst.OE)
    {
      GenerateConstantOverflow(false);
    }
    SetJumpTarget(end);
  }
  if (inst.Rc)
    ComputeRC(d);
}

void Jit64::divwx(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITIntegerOff);
  int a = inst.RA, b = inst.RB, d = inst.RD;

  if (gpr.IsImm(a, b))
  {
    s32 i = gpr.SImm32(a), j = gpr.SImm32(b);
    if (j == 0 || (i == (s32)0x80000000 && j == -1))
    {
      const u32 result = i < 0 ? 0xFFFFFFFF : 0x00000000;
      gpr.SetImmediate32(d, result);
      if (inst.OE)
        GenerateConstantOverflow(true);
    }
    else
    {
      gpr.SetImmediate32(d, i / j);
      if (inst.OE)
        GenerateConstantOverflow(false);
    }
  }
  else if (gpr.IsImm(a))
  {
    // Constant dividend
    const u32 dividend = gpr.Imm32(a);

    if (dividend == 0)
    {
      if (inst.OE)
      {
        RCOpArg rb = gpr.Use(b, RCMode::Read);
        RegCache::Realize(rb);

        CMP_or_TEST(32, rb, Imm32(0));
        GenerateOverflow(CC_NZ);
      }

      // Zero divided by anything is always zero
      gpr.SetImmediate32(d, 0);
    }
    else
    {
      RCX64Reg rb = gpr.Bind(b, RCMode::Read);
      RCX64Reg rd = gpr.Bind(d, RCMode::Write);
      // no register choice
      RCX64Reg eax = gpr.Scratch(EAX);
      RCX64Reg edx = gpr.Scratch(EDX);
      RegCache::Realize(rb, rd, eax, edx);

      // Check for divisor == 0
      TEST(32, rb, rb);

      FixupBranch done;

      if (d == b && (dividend & 0x80000000) == 0 && !inst.OE)
      {
        // Divisor is 0, skip to the end
        // No need to explicitly set destination to 0 due to overlapping registers
        done = J_CC(CC_Z);
        // Otherwise, proceed to normal path
      }
      else
      {
        FixupBranch normal_path;
        if (dividend == 0x80000000)
        {
          // Divisor is 0, proceed to overflow case
          const FixupBranch overflow = J_CC(CC_Z);
          // Otherwise, check for divisor == -1
          CMP(32, rb, Imm32(0xFFFFFFFF));
          normal_path = J_CC(CC_NE);

          SetJumpTarget(overflow);
        }
        else
        {
          // Divisor is not 0, take normal path
          normal_path = J_CC(CC_NZ);
          // Otherwise, proceed to overflow case
        }

        // Set Rd to all ones or all zeroes
        if (dividend & 0x80000000)
          MOV(32, rd, Imm32(0xFFFFFFFF));
        else if (d != b)
          XOR(32, rd, rd);

        if (inst.OE)
          GenerateConstantOverflow(true);

        done = J();

        SetJumpTarget(normal_path);
      }

      MOV(32, eax, Imm32(dividend));
      CDQ();
      IDIV(32, rb);
      MOV(32, rd, eax);

      if (inst.OE)
        GenerateConstantOverflow(false);

      SetJumpTarget(done);
    }
  }
  else if (gpr.IsImm(b))
  {
    // Constant divisor
    const s32 divisor = gpr.SImm32(b);
    RCOpArg ra = gpr.Use(a, RCMode::Read);
    RCX64Reg rd = gpr.Bind(d, RCMode::Write);
    RegCache::Realize(ra, rd);

    // Handle 0, 1, and -1 explicitly
    if (divisor == 0)
    {
      if (d != a)
        MOV(32, rd, ra);
      SAR(32, rd, Imm8(31));
      if (inst.OE)
        GenerateConstantOverflow(true);
    }
    else if (divisor == 1)
    {
      if (d != a)
        MOV(32, rd, ra);
      if (inst.OE)
        GenerateConstantOverflow(false);
    }
    else if (divisor == -1)
    {
      if (d != a)
        MOV(32, rd, ra);

      NEG(32, rd);
      const FixupBranch normal = J_CC(CC_NO);

      MOV(32, rd, Imm32(0xFFFFFFFF));
      if (inst.OE)
        GenerateConstantOverflow(true);
      const FixupBranch done = J();

      SetJumpTarget(normal);
      if (inst.OE)
        GenerateConstantOverflow(false);

      SetJumpTarget(done);
    }
    else if (divisor == 2 || divisor == -2)
    {
      X64Reg tmp = RSCRATCH;
      X64Reg sign = tmp;

      if (!ra.IsSimpleReg())
      {
        // Load dividend from memory
        MOV(32, R(tmp), ra);
        MOV(32, rd, R(tmp));
      }
      else if (d == a)
      {
        // Make a copy of the dividend
        MOV(32, R(tmp), ra);
      }
      else
      {
        // Copy dividend directly into destination
        MOV(32, rd, ra);
        tmp = ra.GetSimpleReg();
        sign = rd;
      }

      SHR(32, R(sign), Imm8(31));
      ADD(32, rd, R(tmp));
      SAR(32, rd, Imm8(1));

      if (divisor < 0)
        NEG(32, rd);

      if (inst.OE)
        GenerateConstantOverflow(false);
    }
    else if (MathUtil::IsPow2(divisor) || MathUtil::IsPow2(-static_cast<s64>(divisor)))
    {
      const u32 abs_val = static_cast<u32>(std::abs(static_cast<s64>(divisor)));

      X64Reg dividend, sum, src;
      CCFlags cond = CC_NS;

      if (!ra.IsSimpleReg())
      {
        dividend = RSCRATCH;
        sum = rd;
        src = RSCRATCH;

        // Load dividend from memory
        MOV(32, R(dividend), ra);
      }
      else if (d == a)
      {
        // Rd holds the dividend, while RSCRATCH holds the sum
        // This is the reverse of the other cases
        dividend = rd;
        sum = RSCRATCH;
        src = RSCRATCH;
        // Negate condition to compensate for the swapped values
        cond = CC_S;
      }
      else
      {
        // Use dividend from register directly
        dividend = ra.GetSimpleReg();
        sum = rd;
        src = dividend;
      }

      TEST(32, R(dividend), R(dividend));
      LEA(32, sum, MDisp(dividend, abs_val - 1));
      CMOVcc(32, rd, R(src), cond);
      SAR(32, rd, Imm8(MathUtil::IntLog2(abs_val)));

      if (divisor < 0)
        NEG(32, rd);

      if (inst.OE)
        GenerateConstantOverflow(false);
    }
    else
    {
      // Optimize signed 32-bit integer division by a constant
      SignedMagic m = SignedDivisionConstants(divisor);

      MOVSX(64, 32, RSCRATCH, ra);

      if (divisor > 0 && m.multiplier < 0)
      {
        IMUL(64, rd, R(RSCRATCH), Imm32(m.multiplier));
        SHR(64, rd, Imm8(32));
        ADD(32, rd, R(RSCRATCH));
        SHR(32, R(RSCRATCH), Imm8(31));
        SAR(32, rd, Imm8(m.shift));
      }
      else if (divisor < 0 && m.multiplier > 0)
      {
        IMUL(64, rd, R(RSCRATCH), Imm32(m.multiplier));
        SHR(64, R(RSCRATCH), Imm8(32));
        SUB(32, R(RSCRATCH), rd);
        MOV(32, rd, R(RSCRATCH));
        SHR(32, rd, Imm8(31));
        SAR(32, R(RSCRATCH), Imm8(m.shift));
      }
      else if (m.multiplier > 0)
      {
        IMUL(64, rd, R(RSCRATCH), Imm32(m.multiplier));
        SHR(32, R(RSCRATCH), Imm8(31));
        SAR(64, R(rd), Imm8(32 + m.shift));
      }
      else
      {
        IMUL(64, RSCRATCH, R(RSCRATCH), Imm32(m.multiplier));
        MOV(64, rd, R(RSCRATCH));
        SHR(64, R(RSCRATCH), Imm8(63));
        SAR(64, R(rd), Imm8(32 + m.shift));
      }

      ADD(32, rd, R(RSCRATCH));

      if (inst.OE)
        GenerateConstantOverflow(false);
    }
  }
  else
  {
    RCOpArg ra = gpr.Use(a, RCMode::Read);
    RCX64Reg rb = gpr.Bind(b, RCMode::Read);
    RCX64Reg rd = gpr.Bind(d, RCMode::Write);
    // no register choice
    RCX64Reg eax = gpr.Scratch(EAX);
    RCX64Reg edx = gpr.Scratch(EDX);
    RegCache::Realize(ra, rb, rd, eax, edx);

    MOV(32, eax, ra);
    TEST(32, rb, rb);
    const FixupBranch overflow = J_CC(CC_E);

    CMP(32, eax, Imm32(0x80000000));
    const FixupBranch normal_path1 = J_CC(CC_NE);

    CMP(32, rb, Imm32(0xFFFFFFFF));
    const FixupBranch normal_path2 = J_CC(CC_NE);

    SetJumpTarget(overflow);
    SAR(32, eax, Imm8(31));
    if (inst.OE)
    {
      GenerateConstantOverflow(true);
    }
    const FixupBranch done = J();

    SetJumpTarget(normal_path1);
    SetJumpTarget(normal_path2);

    CDQ();
    IDIV(32, rb);
    if (inst.OE)
    {
      GenerateConstantOverflow(false);
    }

    SetJumpTarget(done);
    MOV(32, rd, eax);
  }
  if (inst.Rc)
    ComputeRC(d);
}

void Jit64::addx(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITIntegerOff);
  int a = inst.RA, b = inst.RB, d = inst.RD;
  bool carry = !(inst.SUBOP10 & (1 << 8));

  if (gpr.IsImm(a, b))
  {
    const s32 i = gpr.SImm32(a), j = gpr.SImm32(b);
    gpr.SetImmediate32(d, i + j);
    if (carry)
      FinalizeCarry(Interpreter::Helper_Carry(i, j));
    if (inst.OE)
      GenerateConstantOverflow((s64)i + (s64)j);
  }
  else if (gpr.IsImm(a) || gpr.IsImm(b))
  {
    const auto [i, j] = gpr.IsImm(a) ? std::pair(a, b) : std::pair(b, a);
    const s32 imm = gpr.SImm32(i);
    RCOpArg rj = gpr.Use(j, RCMode::Read);
    RCX64Reg rd = gpr.Bind(d, RCMode::Write);
    RegCache::Realize(rj, rd);

    if (imm == 0)
    {
      if (d != j)
        MOV(32, rd, rj);
      if (carry)
        FinalizeCarry(false);
      if (inst.OE)
        GenerateConstantOverflow(false);
    }
    else if (d == j)
    {
      ADD(32, rd, Imm32(imm));
      if (carry)
        FinalizeCarry(CC_C);
      if (inst.OE)
        GenerateOverflow();
    }
    else if (rj.IsSimpleReg() && !carry && !inst.OE)
    {
      LEA(32, rd, MDisp(rj.GetSimpleReg(), imm));
    }
    else if (imm >= -128 && imm <= 127)
    {
      MOV(32, rd, rj);
      ADD(32, rd, Imm32(imm));
      if (carry)
        FinalizeCarry(CC_C);
      if (inst.OE)
        GenerateOverflow();
    }
    else
    {
      MOV(32, rd, Imm32(imm));
      ADD(32, rd, rj);
      if (carry)
        FinalizeCarry(CC_C);
      if (inst.OE)
        GenerateOverflow();
    }
  }
  else
  {
    RCOpArg ra = gpr.Use(a, RCMode::Read);
    RCOpArg rb = gpr.Use(b, RCMode::Read);
    RCX64Reg rd = gpr.Bind(d, RCMode::Write);
    RegCache::Realize(ra, rb, rd);

    if (d == a || d == b)
    {
      RCOpArg& rnotd = (d == a) ? rb : ra;
      ADD(32, rd, rnotd);
    }
    else if (ra.IsSimpleReg() && rb.IsSimpleReg() && !carry && !inst.OE)
    {
      LEA(32, rd, MRegSum(ra.GetSimpleReg(), rb.GetSimpleReg()));
    }
    else
    {
      MOV(32, rd, ra);
      ADD(32, rd, rb);
    }
    if (carry)
      FinalizeCarry(CC_C);
    if (inst.OE)
      GenerateOverflow();
  }
  if (inst.Rc)
    ComputeRC(d);
}

void Jit64::arithXex(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITIntegerOff);
  bool regsource = !(inst.SUBOP10 & 64);  // addex or subfex
  bool mex = !!(inst.SUBOP10 & 32);       // addmex/subfmex or addzex/subfzex
  bool add = !!(inst.SUBOP10 & 2);        // add or sub
  int a = inst.RA;
  int b = regsource ? inst.RB : a;
  int d = inst.RD;
  bool same_input_sub = !add && regsource && a == b;

  if (js.carryFlag == CarryFlag::InPPCState)
    JitGetAndClearCAOV(inst.OE);
  else
    UnlockFlags();

  bool inverted_carry = false;
  // Special case: subfe A, B, B is a common compiler idiom
  if (same_input_sub)
  {
    RCX64Reg rd = gpr.Bind(d, RCMode::Write);
    RegCache::Realize(rd);

    // Convert carry to borrow
    if (js.carryFlag != CarryFlag::InHostCarryInverted)
      CMC();
    SBB(32, rd, rd);
    inverted_carry = true;
  }
  else if (!add && regsource && d == b)
  {
    RCOpArg ra = gpr.Use(a, RCMode::Read);
    RCX64Reg rd = gpr.Bind(d, RCMode::ReadWrite);
    RegCache::Realize(ra, rd);

    if (js.carryFlag != CarryFlag::InHostCarryInverted)
      CMC();
    SBB(32, rd, ra);
    inverted_carry = true;
  }
  else
  {
    RCOpArg ra = gpr.Use(a, RCMode::Read);
    RCOpArg rb = gpr.Use(b, RCMode::Read);
    RCX64Reg rd = gpr.Bind(d, RCMode::Write);
    RCOpArg source =
        regsource ? gpr.Use(d == b ? a : b, RCMode::Read) : RCOpArg::Imm32(mex ? 0xFFFFFFFF : 0);
    RegCache::Realize(ra, rb, rd, source);

    if (d != a && d != b)
      MOV(32, rd, ra);
    if (!add)
      NOT(32, rd);
    // if the source is an immediate, we can invert carry by going from add -> sub and doing src =
    // -1 - src
    if (js.carryFlag == CarryFlag::InHostCarryInverted && source.IsImm())
    {
      SBB(32, rd, Imm32(-1 - source.SImm32()));
      inverted_carry = true;
    }
    else
    {
      if (js.carryFlag == CarryFlag::InHostCarryInverted)
        CMC();
      ADC(32, rd, source);
    }
  }
  FinalizeCarryOverflow(inst.OE, inverted_carry);
  if (inst.Rc)
    ComputeRC(d);
}

void Jit64::rlwinmx(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITIntegerOff);
  int a = inst.RA;
  int s = inst.RS;

  if (gpr.IsImm(s))
  {
    u32 result = gpr.Imm32(s);
    if (inst.SH != 0)
      result = std::rotl(result, inst.SH);
    result &= MakeRotationMask(inst.MB, inst.ME);
    gpr.SetImmediate32(a, result);
    if (inst.Rc)
      ComputeRC(a);
  }
  else
  {
    const bool left_shift = inst.SH && inst.MB == 0 && inst.ME == 31 - inst.SH;
    const bool right_shift = inst.SH && inst.ME == 31 && inst.MB == 32 - inst.SH;
    const bool field_extract = inst.SH && inst.ME == 31 && inst.MB > 32 - inst.SH;
    const u32 mask = MakeRotationMask(inst.MB, inst.ME);
    const u32 prerotate_mask = std::rotr(mask, inst.SH);
    const bool simple_mask = mask == 0xff || mask == 0xffff;
    const bool simple_prerotate_mask = prerotate_mask == 0xff || prerotate_mask == 0xffff;
    // In case of a merged branch, track whether or not we've set flags.
    // If not, we need to do a test later to get them.
    bool needs_test = true;
    // If we know the high bit can't be set, we can avoid doing a sign extend for flag storage.
    bool needs_sext = true;
    int mask_size = inst.ME - inst.MB + 1;

    if (simple_mask && !(inst.SH & (mask_size - 1)) && !gpr.IsBound(s))
    {
      // optimized case: byte/word extract from m_ppc_state

      // Note: If a == s, calling Realize(Ra) will allocate a host register for Rs,
      // so we have to get mem_source from Rs before calling Realize(Ra)

      RCOpArg rs = gpr.Use(s, RCMode::Read);
      RegCache::Realize(rs);
      OpArg mem_source = rs.Location();
      if (inst.SH)
        mem_source.AddMemOffset((32 - inst.SH) >> 3);
      rs.Unlock();

      RCX64Reg ra = gpr.Bind(a, RCMode::Write);
      RegCache::Realize(ra);
      MOVZX(32, mask_size, ra, mem_source);

      needs_sext = false;
    }
    else
    {
      RCOpArg rs = gpr.Use(s, RCMode::Read);
      RCX64Reg ra = gpr.Bind(a, RCMode::Write);
      RegCache::Realize(rs, ra);

      if (a != s && left_shift && rs.IsSimpleReg() && inst.SH <= 3)
      {
        LEA(32, ra, MScaled(rs.GetSimpleReg(), SCALE_1 << inst.SH, 0));
      }
      // optimized case: byte/word extract plus rotate
      else if (simple_prerotate_mask && !left_shift)
      {
        MOVZX(32, prerotate_mask == 0xff ? 8 : 16, ra, rs);
        if (inst.SH)
          ROL(32, ra, Imm8(inst.SH));
        needs_sext = (mask & 0x80000000) != 0;
      }
      // Use BEXTR where possible: Only AMD implements this in one uop
      else if (field_extract && cpu_info.bBMI1 && cpu_info.vendor == CPUVendor::AMD)
      {
        MOV(32, R(RSCRATCH), Imm32((mask_size << 8) | (32 - inst.SH)));
        BEXTR(32, ra, rs, RSCRATCH);
        needs_sext = false;
      }
      else if (left_shift)
      {
        if (a != s)
          MOV(32, ra, rs);

        SHL(32, ra, Imm8(inst.SH));
      }
      else if (right_shift)
      {
        if (a != s)
          MOV(32, ra, rs);

        SHR(32, ra, Imm8(inst.MB));
        needs_sext = false;
      }
      else
      {
        RotateLeft(32, ra, rs, inst.SH);

        if (!(inst.MB == 0 && inst.ME == 31))
        {
          // we need flags if we're merging the branch
          if (inst.Rc && CheckMergedBranch(0))
            AND(32, ra, Imm32(mask));
          else
            AndWithMask(ra, mask);
          needs_sext = inst.MB == 0;
          needs_test = false;
        }
      }
    }

    if (inst.Rc)
      ComputeRC(a, needs_test, needs_sext);
  }
}

void Jit64::rlwimix(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITIntegerOff);
  int a = inst.RA;
  int s = inst.RS;

  const u32 mask = MakeRotationMask(inst.MB, inst.ME);

  if (gpr.IsImm(a, s))
  {
    gpr.SetImmediate32(a, (gpr.Imm32(a) & ~mask) | (std::rotl(gpr.Imm32(s), inst.SH) & mask));
    if (inst.Rc)
      ComputeRC(a);
  }
  else if (gpr.IsImm(s) && mask == 0xFFFFFFFF)
  {
    gpr.SetImmediate32(a, std::rotl(gpr.Imm32(s), inst.SH));

    if (inst.Rc)
      ComputeRC(a);
  }
  else
  {
    const bool left_shift = mask == 0U - (1U << inst.SH);
    const bool right_shift = mask == (1U << inst.SH) - 1;
    bool needs_test = false;

    if (mask == 0 || (a == s && inst.SH == 0))
    {
      needs_test = true;
    }
    else if (mask == 0xFFFFFFFF)
    {
      RCOpArg rs = gpr.Use(s, RCMode::Read);
      RCX64Reg ra = gpr.Bind(a, RCMode::Write);
      RegCache::Realize(rs, ra);
      RotateLeft(32, ra, rs, inst.SH);
      needs_test = true;
    }
    else if (gpr.IsImm(s))
    {
      RCX64Reg ra = gpr.Bind(a, RCMode::ReadWrite);
      RegCache::Realize(ra);
      AndWithMask(ra, ~mask);
      OR(32, ra, Imm32(std::rotl(gpr.Imm32(s), inst.SH) & mask));
    }
    else if (gpr.IsImm(a))
    {
      const u32 mask_a = gpr.Imm32(a) & ~mask;

      RCOpArg rs = gpr.Use(s, RCMode::Read);
      RCX64Reg ra = gpr.Bind(a, RCMode::Write);
      RegCache::Realize(rs, ra);

      if (inst.SH == 0)
      {
        MOV(32, ra, rs);
        AndWithMask(ra, mask);
      }
      else if (left_shift)
      {
        MOV(32, ra, rs);
        SHL(32, ra, Imm8(inst.SH));
      }
      else if (right_shift)
      {
        MOV(32, ra, rs);
        SHR(32, ra, Imm8(32 - inst.SH));
      }
      else
      {
        RotateLeft(32, ra, rs, inst.SH);
        AndWithMask(ra, mask);
      }

      if (mask_a)
        OR(32, ra, Imm32(mask_a));
      else
        needs_test = true;
    }
    else if (inst.SH)
    {
      // TODO: perhaps consider pinsrb or abuse of AH
      RCOpArg rs = gpr.Use(s, RCMode::Read);
      RCX64Reg ra = gpr.Bind(a, RCMode::ReadWrite);
      RegCache::Realize(rs, ra);

      if (left_shift)
      {
        MOV(32, R(RSCRATCH), rs);
        SHL(32, R(RSCRATCH), Imm8(inst.SH));
      }
      else if (right_shift)
      {
        MOV(32, R(RSCRATCH), rs);
        SHR(32, R(RSCRATCH), Imm8(32 - inst.SH));
      }
      else
      {
        RotateLeft(32, RSCRATCH, rs, inst.SH);
      }

      if (mask == 0xFF || mask == 0xFFFF)
      {
        MOV(mask == 0xFF ? 8 : 16, ra, R(RSCRATCH));
        needs_test = true;
      }
      else
      {
        if (!left_shift && !right_shift)
          AndWithMask(RSCRATCH, mask);
        AndWithMask(ra, ~mask);
        OR(32, ra, R(RSCRATCH));
      }
    }
    else
    {
      RCX64Reg rs = gpr.Bind(s, RCMode::Read);
      RCX64Reg ra = gpr.Bind(a, RCMode::ReadWrite);
      RegCache::Realize(rs, ra);

      if (mask == 0xFF || mask == 0xFFFF)
      {
        MOV(mask == 0xFF ? 8 : 16, ra, rs);
        needs_test = true;
      }
      else
      {
        XOR(32, ra, rs);
        AndWithMask(ra, ~mask);
        XOR(32, ra, rs);
      }
    }
    if (inst.Rc)
      ComputeRC(a, needs_test);
  }
}

void Jit64::rlwnmx(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITIntegerOff);
  int a = inst.RA, b = inst.RB, s = inst.RS;

  const u32 mask = MakeRotationMask(inst.MB, inst.ME);
  if (gpr.IsImm(b, s))
  {
    gpr.SetImmediate32(a, std::rotl(gpr.Imm32(s), gpr.Imm32(b) & 0x1F) & mask);
  }
  else if (gpr.IsImm(b))
  {
    u32 amount = gpr.Imm32(b) & 0x1f;
    RCX64Reg ra = gpr.Bind(a, RCMode::Write);
    RCOpArg rs = gpr.Use(s, RCMode::Read);
    RegCache::Realize(ra, rs);

    RotateLeft(32, ra, rs, amount);

    // we need flags if we're merging the branch
    if (inst.Rc && CheckMergedBranch(0))
      AND(32, ra, Imm32(mask));
    else
      AndWithMask(ra, mask);
  }
  else
  {
    RCX64Reg ecx = gpr.Scratch(ECX);  // no register choice
    RCX64Reg ra = gpr.Bind(a, RCMode::Write);
    RCOpArg rb = gpr.Use(b, RCMode::Read);
    RCOpArg rs = gpr.Use(s, RCMode::Read);
    RegCache::Realize(ecx, ra, rb, rs);

    MOV(32, ecx, rb);
    if (a != s)
    {
      MOV(32, ra, rs);
    }
    ROL(32, ra, ecx);
    // we need flags if we're merging the branch
    if (inst.Rc && CheckMergedBranch(0))
      AND(32, ra, Imm32(mask));
    else
      AndWithMask(ra, mask);
  }
  if (inst.Rc)
    ComputeRC(a, false);
}

void Jit64::negx(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITIntegerOff);
  int a = inst.RA;
  int d = inst.RD;

  if (gpr.IsImm(a))
  {
    gpr.SetImmediate32(d, ~(gpr.Imm32(a)) + 1);
    if (inst.OE)
      GenerateConstantOverflow(gpr.Imm32(d) == 0x80000000);
  }
  else
  {
    RCOpArg ra = gpr.Use(a, RCMode::Read);
    RCX64Reg rd = gpr.Bind(d, RCMode::Write);
    RegCache::Realize(ra, rd);

    if (a != d)
      MOV(32, rd, ra);
    NEG(32, rd);
    if (inst.OE)
      GenerateOverflow();
  }
  if (inst.Rc)
    ComputeRC(d, false);
}

void Jit64::srwx(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITIntegerOff);
  int a = inst.RA;
  int b = inst.RB;
  int s = inst.RS;

  if (gpr.IsImm(b, s))
  {
    u32 amount = gpr.Imm32(b);
    gpr.SetImmediate32(a, (amount & 0x20) ? 0 : (gpr.Imm32(s) >> (amount & 0x1f)));
  }
  else if (gpr.IsImm(b))
  {
    u32 amount = gpr.Imm32(b);
    if (amount & 0x20)
    {
      gpr.SetImmediate32(a, 0);
    }
    else
    {
      RCX64Reg ra = gpr.Bind(a, RCMode::Write);
      RCOpArg rs = gpr.Use(s, RCMode::Read);
      RegCache::Realize(ra, rs);

      if (a != s)
        MOV(32, ra, rs);

      amount &= 0x1f;
      if (amount != 0)
        SHR(32, ra, Imm8(amount));
    }
  }
  else if (cpu_info.bBMI2)
  {
    RCX64Reg ra = gpr.Bind(a, RCMode::Write);
    RCX64Reg rb = gpr.Bind(b, RCMode::Read);
    RCX64Reg rs = gpr.Bind(s, RCMode::Read);
    RegCache::Realize(ra, rb, rs);

    // Rs must be in register: This is a 64-bit operation, using an OpArg will have invalid results
    SHRX(64, ra, rs, rb);
  }
  else
  {
    RCX64Reg ecx = gpr.Scratch(ECX);  // no register choice
    RCX64Reg ra = gpr.Bind(a, RCMode::Write);
    RCOpArg rb = gpr.Use(b, RCMode::Read);
    RCOpArg rs = gpr.Use(s, RCMode::Read);
    RegCache::Realize(ecx, ra, rb, rs);

    MOV(32, ecx, rb);
    if (a != s)
      MOV(32, ra, rs);
    SHR(64, ra, ecx);
  }
  // Shift of 0 doesn't update flags, so we need to test just in case
  if (inst.Rc)
    ComputeRC(a);
}

void Jit64::slwx(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITIntegerOff);
  int a = inst.RA;
  int b = inst.RB;
  int s = inst.RS;

  if (gpr.IsImm(b, s))
  {
    u32 amount = gpr.Imm32(b);
    gpr.SetImmediate32(a, (amount & 0x20) ? 0 : gpr.Imm32(s) << (amount & 0x1f));
    if (inst.Rc)
      ComputeRC(a);
  }
  else if (gpr.IsImm(b))
  {
    u32 amount = gpr.Imm32(b);
    if (amount & 0x20)
    {
      gpr.SetImmediate32(a, 0);
    }
    else
    {
      RCX64Reg ra = gpr.Bind(a, RCMode::Write);
      RCOpArg rs = gpr.Use(s, RCMode::Read);
      RegCache::Realize(ra, rs);

      if (a != s)
        MOV(32, ra, rs);

      amount &= 0x1f;
      if (amount != 0)
        SHL(32, ra, Imm8(amount));
    }

    if (inst.Rc)
      ComputeRC(a);
  }
  else if (gpr.IsImm(s) && gpr.Imm32(s) == 0)
  {
    gpr.SetImmediate32(a, 0);
    if (inst.Rc)
      ComputeRC(a);
  }
  else if (cpu_info.bBMI2)
  {
    RCX64Reg ra = gpr.Bind(a, RCMode::Write);
    RCX64Reg rb = gpr.Bind(b, RCMode::Read);
    RCOpArg rs = gpr.UseNoImm(s, RCMode::Read);
    RegCache::Realize(ra, rb, rs);

    SHLX(64, ra, rs, rb);
    if (inst.Rc)
    {
      AND(32, ra, ra);
      RegCache::Unlock(ra, rb, rs);
      ComputeRC(a, false);
    }
    else
    {
      MOVZX(64, 32, ra, ra);
    }
  }
  else
  {
    RCX64Reg ecx = gpr.Scratch(ECX);  // no register choice
    RCX64Reg ra = gpr.Bind(a, RCMode::Write);
    RCOpArg rb = gpr.Use(b, RCMode::Read);
    RCOpArg rs = gpr.Use(s, RCMode::Read);
    RegCache::Realize(ecx, ra, rb, rs);

    MOV(32, ecx, rb);
    if (a != s)
      MOV(32, ra, rs);
    SHL(64, ra, ecx);
    if (inst.Rc)
    {
      AND(32, ra, ra);
      RegCache::Unlock(ecx, ra, rb, rs);
      ComputeRC(a, false);
    }
    else
    {
      MOVZX(64, 32, ra, ra);
    }
  }
}

void Jit64::srawx(UGeckoInstruction inst)
{
  // USES_XER
  INSTRUCTION_START
  JITDISABLE(bJITIntegerOff);
  int a = inst.RA;
  int b = inst.RB;
  int s = inst.RS;

  if (gpr.IsImm(b, s))
  {
    s32 i = gpr.SImm32(s), amount = gpr.SImm32(b);
    if (amount & 0x20)
    {
      gpr.SetImmediate32(a, i & 0x80000000 ? 0xFFFFFFFF : 0);
      FinalizeCarry(i & 0x80000000 ? true : false);
    }
    else
    {
      amount &= 0x1F;
      gpr.SetImmediate32(a, i >> amount);
      FinalizeCarry(amount != 0 && i < 0 && (u32(i) << (32 - amount)));
    }
  }
  else if (gpr.IsImm(b))
  {
    u32 amount = gpr.Imm32(b);
    RCX64Reg ra = gpr.Bind(a, RCMode::Write);
    RCOpArg rs = gpr.Use(s, RCMode::Read);
    RegCache::Realize(ra, rs);

    if (a != s)
      MOV(32, ra, rs);

    bool special = amount & 0x20;
    amount &= 0x1f;

    if (special)
    {
      SAR(32, ra, Imm8(31));
      FinalizeCarry(CC_NZ);
    }
    else if (amount == 0)
    {
      FinalizeCarry(false);
    }
    else if (!js.op->wantsCA)
    {
      SAR(32, ra, Imm8(amount));
      FinalizeCarry(CC_NZ);
    }
    else
    {
      MOV(32, R(RSCRATCH), ra);
      SAR(32, ra, Imm8(amount));
      SHL(32, R(RSCRATCH), Imm8(32 - amount));
      TEST(32, ra, R(RSCRATCH));
      FinalizeCarry(CC_NZ);
    }
  }
  else if (gpr.IsImm(s) && gpr.Imm32(s) == 0)
  {
    gpr.SetImmediate32(a, 0);
    FinalizeCarry(false);
  }
  else if (cpu_info.bBMI2)
  {
    RCX64Reg ra = gpr.Bind(a, RCMode::Write);
    RCX64Reg rb = gpr.Bind(b, RCMode::Read);
    RCOpArg rs = gpr.Use(s, RCMode::Read);
    RegCache::Realize(ra, rb, rs);

    X64Reg tmp = RSCRATCH;
    if (a == s && a != b)
      tmp = ra;
    else
      MOV(32, R(tmp), rs);

    SHL(64, R(tmp), Imm8(32));
    SARX(64, ra, R(tmp), rb);
    if (js.op->wantsCA)
    {
      MOV(32, R(RSCRATCH), ra);
      SHR(64, ra, Imm8(32));
      TEST(32, ra, R(RSCRATCH));
    }
    else
    {
      SHR(64, ra, Imm8(32));
    }
    FinalizeCarry(CC_NZ);
  }
  else
  {
    RCX64Reg ecx = gpr.Scratch(ECX);  // no register choice
    RCX64Reg ra = gpr.Bind(a, RCMode::Write);
    RCOpArg rb = gpr.Use(b, RCMode::Read);
    RCOpArg rs = gpr.Use(s, RCMode::Read);
    RegCache::Realize(ecx, ra, rb, rs);

    MOV(32, ecx, rb);
    if (a != s)
      MOV(32, ra, rs);
    SHL(64, ra, Imm8(32));
    SAR(64, ra, ecx);
    if (js.op->wantsCA)
    {
      MOV(32, R(RSCRATCH), ra);
      SHR(64, ra, Imm8(32));
      TEST(32, ra, R(RSCRATCH));
    }
    else
    {
      SHR(64, ra, Imm8(32));
    }
    FinalizeCarry(CC_NZ);
  }
  if (inst.Rc)
    ComputeRC(a);
}

void Jit64::srawix(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITIntegerOff);
  int a = inst.RA;
  int s = inst.RS;
  int amount = inst.SH;

  if (gpr.IsImm(s))
  {
    s32 imm = gpr.SImm32(s);
    gpr.SetImmediate32(a, imm >> amount);
    FinalizeCarry(amount != 0 && imm < 0 && (u32(imm) << (32 - amount)));
  }
  else if (amount != 0)
  {
    RCX64Reg ra = gpr.Bind(a, RCMode::Write);
    RCOpArg rs = gpr.Use(s, RCMode::Read);
    RegCache::Realize(ra, rs);

    if (!js.op->wantsCA)
    {
      if (a != s)
        MOV(32, ra, rs);
      SAR(32, ra, Imm8(amount));
    }
    else
    {
      MOV(32, R(RSCRATCH), rs);
      if (a != s)
        MOV(32, ra, R(RSCRATCH));
      // some optimized common cases that can be done in slightly fewer ops
      if (amount == 1)
      {
        SHR(32, R(RSCRATCH), Imm8(31));  // sign
        AND(32, R(RSCRATCH), ra);        // (sign && carry)
        SAR(32, ra, Imm8(1));
        MOV(8, PPCSTATE(xer_ca),
            R(RSCRATCH));  // XER.CA = sign && carry, aka (input&0x80000001) == 0x80000001
      }
      else
      {
        SAR(32, ra, Imm8(amount));
        SHL(32, R(RSCRATCH), Imm8(32 - amount));
        TEST(32, R(RSCRATCH), ra);
        FinalizeCarry(CC_NZ);
      }
    }
  }
  else
  {
    FinalizeCarry(false);
    RCX64Reg ra = gpr.Bind(a, RCMode::Write);
    RCOpArg rs = gpr.Use(s, RCMode::Read);
    RegCache::Realize(ra, rs);

    if (a != s)
      MOV(32, ra, rs);
  }
  if (inst.Rc)
    ComputeRC(a);
}

// count leading zeroes
void Jit64::cntlzwx(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITIntegerOff);
  int a = inst.RA;
  int s = inst.RS;
  bool needs_test = false;

  if (gpr.IsImm(s))
  {
    gpr.SetImmediate32(a, static_cast<u32>(std::countl_zero(gpr.Imm32(s))));
  }
  else
  {
    RCX64Reg ra = gpr.Bind(a, RCMode::Write);
    RCOpArg rs = gpr.Use(s, RCMode::Read);
    RegCache::Realize(ra, rs);

    if (cpu_info.bLZCNT)
    {
      LZCNT(32, ra, rs);
      needs_test = true;
    }
    else
    {
      BSR(32, ra, rs);
      FixupBranch gotone = J_CC(CC_NZ);
      MOV(32, ra, Imm32(63));
      SetJumpTarget(gotone);
      XOR(32, ra, Imm8(0x1f));  // flip order
    }
  }

  if (inst.Rc)
    ComputeRC(a, needs_test, false);
}

void Jit64::twX(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITIntegerOff);

  s32 a = inst.RA;

  if (inst.OPCD == 3)  // twi
  {
    RCOpArg ra = gpr.UseNoImm(a, RCMode::Read);
    RegCache::Realize(ra);
    CMP(32, ra, Imm32((s32)(s16)inst.SIMM_16));
  }
  else  // tw
  {
    s32 b = inst.RB;
    RCX64Reg ra = gpr.Bind(a, RCMode::Read);
    RCOpArg rb = gpr.Use(b, RCMode::Read);
    RegCache::Realize(ra, rb);
    CMP(32, ra, rb);
  }

  constexpr std::array<CCFlags, 5> CONDITIONS{{CC_A, CC_B, CC_E, CC_G, CC_L}};
  Common::SmallVector<FixupBranch, CONDITIONS.size()> fixups;

  for (size_t i = 0; i < CONDITIONS.size(); i++)
  {
    if (inst.TO & (1 << i))
    {
      FixupBranch f = J_CC(CONDITIONS[i], Jump::Near);
      fixups.push_back(f);
    }
  }

  if (!fixups.empty())
  {
    SwitchToFarCode();

    RCForkGuard gpr_guard = gpr.Fork();
    RCForkGuard fpr_guard = fpr.Fork();

    for (const FixupBranch& fixup : fixups)
    {
      SetJumpTarget(fixup);
    }
    LOCK();
    OR(32, PPCSTATE(Exceptions), Imm32(EXCEPTION_PROGRAM));
    MOV(32, PPCSTATE_SRR1, Imm32(static_cast<u32>(ProgramExceptionCause::Trap)));

    gpr.Flush();
    fpr.Flush();

    MOV(32, PPCSTATE(pc), Imm32(js.compilerPC));
    WriteExceptionExit();

    SwitchToNearCode();
  }

  if (!analyzer.HasOption(PPCAnalyst::PPCAnalyzer::OPTION_CONDITIONAL_CONTINUE))
  {
    gpr.Flush();
    fpr.Flush();
    WriteExit(js.compilerPC + 4);
  }
}

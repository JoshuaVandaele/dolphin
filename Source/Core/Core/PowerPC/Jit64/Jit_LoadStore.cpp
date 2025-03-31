// Copyright 2008 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

// TODO(ector): Tons of pshufb optimization of the loads/stores, for SSSE3+, possibly SSE4, only.
// Should give a very noticable speed boost to paired single heavy code.

#include "Core/PowerPC/Jit64/Jit.h"

#include "Common/Assert.h"
#include "Common/BitSet.h"
#include "Common/CommonTypes.h"
#include "Common/MsgHandler.h"
#include "Common/x64ABI.h"
#include "Common/x64Emitter.h"

#include "Core/ConfigManager.h"
#include "Core/CoreTiming.h"
#include "Core/Debugger/BranchWatch.h"
#include "Core/HW/CPU.h"
#include "Core/HW/Memmap.h"
#include "Core/PowerPC/Jit64/RegCache/JitRegCache.h"
#include "Core/PowerPC/Jit64Common/Jit64PowerPCState.h"
#include "Core/PowerPC/JitInterface.h"
#include "Core/PowerPC/MMU.h"
#include "Core/PowerPC/PowerPC.h"
#include "Core/System.h"

using namespace Gen;

void Jit64::lXXx(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITLoadStoreOff);

  int a = inst.RA, b = inst.RB, d = inst.RD;

  // Skip disabled JIT instructions
  FALLBACK_IF(bJITLoadStorelbzxOff && (inst.OPCD == 31) && (inst.SUBOP10 == 87));
  FALLBACK_IF(bJITLoadStorelXzOff && ((inst.OPCD == 34) || (inst.OPCD == 40) || (inst.OPCD == 32)));
  FALLBACK_IF(bJITLoadStorelwzOff && (inst.OPCD == 32));

  // Determine memory access size and sign extend
  int access_size = 0;
  bool sign_extend = false;
  bool byte_reversed = false;
  switch (inst.OPCD)
  {
  case 32:  // lwz
  case 33:  // lwzu
    access_size = 32;
    sign_extend = false;
    break;

  case 34:  // lbz
  case 35:  // lbzu
    access_size = 8;
    sign_extend = false;
    break;

  case 40:  // lhz
  case 41:  // lhzu
    access_size = 16;
    sign_extend = false;
    break;

  case 42:  // lha
  case 43:  // lhau
    access_size = 16;
    sign_extend = true;
    break;

  case 31:
    switch (inst.SUBOP10)
    {
    case 534:  // lwbrx
      byte_reversed = true;
      [[fallthrough]];
    case 23:  // lwzx
    case 55:  // lwzux
      access_size = 32;
      sign_extend = false;
      break;

    case 87:   // lbzx
    case 119:  // lbzux
      access_size = 8;
      sign_extend = false;
      break;
    case 790:  // lhbrx
      byte_reversed = true;
      [[fallthrough]];
    case 279:  // lhzx
    case 311:  // lhzux
      access_size = 16;
      sign_extend = false;
      break;

    case 343:  // lhax
    case 375:  // lhaux
      access_size = 16;
      sign_extend = true;
      break;

    default:
      PanicAlertFmt("Invalid instruction");
    }
    break;

  default:
    PanicAlertFmt("Invalid instruction");
  }

  // PowerPC has no 8-bit sign extended load, but x86 does, so merge extsb with the load if we find
  // it.
  if (CanMergeNextInstructions(1) && access_size == 8 && js.op[1].inst.OPCD == 31 &&
      js.op[1].inst.SUBOP10 == 954 && js.op[1].inst.RS == inst.RD && js.op[1].inst.RA == inst.RD &&
      !js.op[1].inst.Rc)
  {
    js.downcountAmount++;
    js.skipInstructions = 1;
    sign_extend = true;
  }

  // Determine whether this instruction updates inst.RA
  bool update;
  if (inst.OPCD == 31)
    update = ((inst.SUBOP10 & 0x20) != 0) && (!gpr.IsImm(b) || gpr.Imm32(b) != 0);
  else
    update = ((inst.OPCD & 1) != 0) && inst.SIMM_16 != 0;

  // Determine whether this instruction indexes with inst.RB
  const bool indexed = inst.OPCD == 31;

  bool store_address = false;
  s32 load_offset = 0;

  // Prepare result
  RCX64Reg rd = jo.memcheck ? gpr.RevertableBind(d, RCMode::Write) : gpr.Bind(d, RCMode::Write);

  // Prepare address operand
  RCOpArg op_address;
  if (!update && !a)
  {
    if (indexed)
    {
      op_address = gpr.BindOrImm(b, RCMode::Read);
    }
    else
    {
      op_address = RCOpArg::Imm32((u32)(s32)inst.SIMM_16);
    }
  }
  else if (update && ((a == 0) || (d == a)))
  {
    PanicAlertFmt("Invalid instruction");
  }
  else
  {
    if (!indexed && gpr.IsImm(a) && !jo.memcheck)
    {
      u32 val = gpr.Imm32(a) + inst.SIMM_16;
      op_address = RCOpArg::Imm32(val);
      if (update)
        gpr.SetImmediate32(a, val);
    }
    else if (indexed && gpr.IsImm(a) && gpr.IsImm(b) && !jo.memcheck)
    {
      u32 val = gpr.Imm32(a) + gpr.Imm32(b);
      op_address = RCOpArg::Imm32(val);
      if (update)
        gpr.SetImmediate32(a, val);
    }
    else
    {
      // If we're using reg+reg mode and b is an immediate, pretend we're using constant offset mode
      const bool use_constant_offset = !indexed || gpr.IsImm(b);

      s32 offset = 0;
      if (use_constant_offset)
        offset = indexed ? gpr.SImm32(b) : (s32)inst.SIMM_16;

      RCOpArg rb = use_constant_offset ? RCOpArg{} : gpr.Use(b, RCMode::Read);

      // Depending on whether we have an immediate and/or update, find the optimum way to calculate
      // the load address.
      if ((update || use_constant_offset) && !jo.memcheck)
      {
        op_address = gpr.Bind(a, update ? RCMode::ReadWrite : RCMode::Read);
        RegCache::Realize(op_address, rb);

        if (!use_constant_offset)
          ADD(32, op_address, rb);
        else if (update)
          ADD(32, op_address, Imm32((u32)offset));
        else
          load_offset = offset;
      }
      else
      {
        store_address = true;
        // In this case we need an extra temporary register.
        op_address = RCOpArg::R(RSCRATCH2);
        RCOpArg ra = gpr.Use(a, RCMode::Read);
        RegCache::Realize(op_address, ra, rb);

        if (use_constant_offset)
          MOV_sum(32, RSCRATCH2, ra, Imm32((u32)offset));
        else
          MOV_sum(32, RSCRATCH2, ra, rb);
      }
    }
  }

  RCX64Reg ra = (update && store_address) ? gpr.Bind(a, RCMode::ReadWrite) : RCX64Reg{};
  RegCache::Realize(op_address, ra, rd);

  BitSet32 registers_in_use = CallerSavedRegistersInUse();
  // We need to save the (usually scratch) address register for the update.
  if (update && store_address)
    registers_in_use[RSCRATCH2] = true;

  SafeLoadToReg(rd, op_address, access_size, load_offset, registers_in_use, sign_extend);

  if (update && store_address)
    MOV(32, ra, op_address);

  // TODO: support no-swap in SafeLoadToReg instead
  if (byte_reversed)
    BSWAP(access_size, rd);
}

void Jit64::dcbx(UGeckoInstruction inst)
{
  FALLBACK_IF(m_accurate_cpu_cache_enabled);

  INSTRUCTION_START
  JITDISABLE(bJITLoadStoreOff);

  // Check if the next instructions match a known looping pattern:
  // - dcbx rX
  // - addi rX,rX,32
  // - bdnz+ -8
  const bool make_loop = inst.RA == 0 && inst.RB != 0 && CanMergeNextInstructions(2) &&
                         (js.op[1].inst.hex & 0xfc00'ffff) == 0x38000020 &&
                         js.op[1].inst.RA_6 == inst.RB && js.op[1].inst.RD_2 == inst.RB &&
                         js.op[2].inst.hex == 0x4200fff8;

  RCOpArg ra = inst.RA ? gpr.Use(inst.RA, RCMode::Read) : RCOpArg::Imm32(0);
  RCX64Reg rb = gpr.Bind(inst.RB, make_loop ? RCMode::ReadWrite : RCMode::Read);
  RegCache::Realize(ra, rb);

  RCX64Reg loop_counter;
  if (make_loop)
  {
    // We'll execute somewhere between one single cacheline invalidation and however many are needed
    // to reduce the downcount to zero, never exceeding the amount requested by the game.
    // To stay consistent with the rest of the code we adjust the involved registers (CTR and Rb)
    // by the amount of cache lines we invalidate minus one -- since we'll run the regular addi and
    // bdnz afterwards! So if we invalidate a single cache line, we don't adjust the registers at
    // all, if we invalidate 2 cachelines we adjust the registers by one step, and so on.

    RCX64Reg reg_cycle_count = gpr.Scratch();
    RCX64Reg reg_downcount = gpr.Scratch();
    loop_counter = gpr.Scratch();
    RegCache::Realize(reg_cycle_count, reg_downcount, loop_counter);

    // This must be true in order for us to pick up the DIV results and not trash any data.
    static_assert(RSCRATCH == Gen::EAX && RSCRATCH2 == Gen::EDX);

    // Alright, now figure out how many loops we want to do.
    const u8 cycle_count_per_loop =
        js.op[0].opinfo->num_cycles + js.op[1].opinfo->num_cycles + js.op[2].opinfo->num_cycles;

    // This is both setting the adjusted loop count to 0 for the downcount <= 0 case and clearing
    // the upper bits for the DIV instruction in the downcount > 0 case.
    XOR(32, R(RSCRATCH2), R(RSCRATCH2));

    MOV(32, R(RSCRATCH), PPCSTATE(downcount));
    TEST(32, R(RSCRATCH), R(RSCRATCH));                       // if (downcount <= 0)
    FixupBranch downcount_is_zero_or_negative = J_CC(CC_LE);  // only do 1 invalidation; else:
    MOV(32, R(loop_counter), PPCSTATE_CTR);
    MOV(32, R(reg_downcount), R(RSCRATCH));
    MOV(32, R(reg_cycle_count), Imm32(cycle_count_per_loop));
    DIV(32, R(reg_cycle_count));                  // RSCRATCH = downcount / cycle_count
    LEA(32, RSCRATCH2, MDisp(loop_counter, -1));  // RSCRATCH2 = CTR - 1
    // ^ Note that this CTR-1 implicitly handles the CTR == 0 case correctly.
    CMP(32, R(RSCRATCH), R(RSCRATCH2));
    CMOVcc(32, RSCRATCH2, R(RSCRATCH), CC_B);  // RSCRATCH2 = min(RSCRATCH, RSCRATCH2)

    // RSCRATCH2 now holds the amount of loops to execute minus 1, which is the amount we need to
    // adjust downcount, CTR, and Rb by to exit the loop construct with the right values in those
    // registers.
    SUB(32, R(loop_counter), R(RSCRATCH2));
    MOV(32, PPCSTATE_CTR, R(loop_counter));  // CTR -= RSCRATCH2
    IMUL(32, reg_cycle_count, R(RSCRATCH2));
    // ^ Note that this cannot overflow because it's limited by (downcount/cycle_count).
    SUB(32, R(reg_downcount), R(reg_cycle_count));
    MOV(32, PPCSTATE(downcount), R(reg_downcount));  // downcount -= (RSCRATCH2 * reg_cycle_count)

    SetJumpTarget(downcount_is_zero_or_negative);

    // Load the loop_counter register with the amount of invalidations to execute.
    LEA(32, loop_counter, MDisp(RSCRATCH2, 1));

    if (IsDebuggingEnabled())
    {
      const X64Reg bw_reg_a = reg_cycle_count, bw_reg_b = reg_downcount;
      const BitSet32 bw_caller_save = (CallerSavedRegistersInUse() | BitSet32{RSCRATCH2}) &
                                      ~BitSet32{int(bw_reg_a), int(bw_reg_b)};

      MOV(64, R(bw_reg_a), ImmPtr(&m_branch_watch));
      MOVZX(32, 8, bw_reg_b, MDisp(bw_reg_a, Core::BranchWatch::GetOffsetOfRecordingActive()));
      TEST(32, R(bw_reg_b), R(bw_reg_b));

      FixupBranch branch_in = J_CC(CC_NZ, Jump::Near);
      SwitchToFarCode();
      SetJumpTarget(branch_in);

      // Assert RSCRATCH2 won't be clobbered before it is moved from.
      static_assert(RSCRATCH2 != ABI_PARAM1);

      ABI_PushRegistersAndAdjustStack(bw_caller_save, 0);
      MOV(64, R(ABI_PARAM1), R(bw_reg_a));
      // RSCRATCH2 holds the amount of faked branch watch hits. Move RSCRATCH2 first, because
      // ABI_PARAM2 clobbers RSCRATCH2 on Windows and ABI_PARAM3 clobbers RSCRATCH2 on Linux!
      MOV(32, R(ABI_PARAM4), R(RSCRATCH2));
      const PPCAnalyst::CodeOp& op = js.op[2];
      MOV(64, R(ABI_PARAM2), Imm64(Core::FakeBranchWatchCollectionKey{op.address, op.branchTo}));
      MOV(32, R(ABI_PARAM3), Imm32(op.inst.hex));
      ABI_CallFunction(m_ppc_state.msr.IR ? &Core::BranchWatch::HitVirtualTrue_fk_n :
                                            &Core::BranchWatch::HitPhysicalTrue_fk_n);
      ABI_PopRegistersAndAdjustStack(bw_caller_save, 0);

      FixupBranch branch_out = J(Jump::Near);
      SwitchToNearCode();
      SetJumpTarget(branch_out);
    }
  }

  X64Reg addr = RSCRATCH;
  MOV_sum(32, addr, ra, rb);

  if (make_loop)
  {
    // This is the best place to adjust Rb to what it should be since RSCRATCH2 still has the
    // adjusted loop count and we're done reading from Rb.
    SHL(32, R(RSCRATCH2), Imm8(5));
    ADD(32, R(rb), R(RSCRATCH2));  // Rb += (RSCRATCH2 * 32)
  }

  X64Reg tmp = RSCRATCH2;
  RCX64Reg effective_address = gpr.Scratch();
  RegCache::Realize(effective_address);

  FixupBranch bat_lookup_failed;
  MOV(32, R(effective_address), R(addr));
  const u8* loop_start = GetCodePtr();
  if (m_ppc_state.feature_flags & FEATURE_FLAG_MSR_IR)
  {
    // Translate effective address to physical address.
    bat_lookup_failed = BATAddressLookup(addr, tmp, m_jit.m_mmu.GetIBATTable().data());
    MOV(32, R(tmp), R(effective_address));
    AND(32, R(tmp), Imm32(0x0001ffff));
    AND(32, R(addr), Imm32(0xfffe0000));
    OR(32, R(addr), R(tmp));
  }

  // Check whether a JIT cache line needs to be invalidated.
  SHR(32, R(addr), Imm8(5 + 5));  // >> 5 for cache line size, >> 5 for width of bitset
  MOV(64, R(tmp), ImmPtr(GetBlockCache()->GetBlockBitSet()));
  MOV(32, R(addr), MComplex(tmp, addr, SCALE_4, 0));
  MOV(32, R(tmp), R(effective_address));
  SHR(32, R(tmp), Imm8(5));
  BT(32, R(addr), R(tmp));
  FixupBranch invalidate_needed = J_CC(CC_C, Jump::Near);

  if (make_loop)
  {
    ADD(32, R(effective_address), Imm8(32));
    MOV(32, R(addr), R(effective_address));
    SUB(32, R(loop_counter), Imm8(1));
    J_CC(CC_NZ, loop_start);
  }

  SwitchToFarCode();
  SetJumpTarget(invalidate_needed);
  if (m_ppc_state.feature_flags & FEATURE_FLAG_MSR_IR)
    SetJumpTarget(bat_lookup_failed);

  BitSet32 registers_in_use = CallerSavedRegistersInUse();
  registers_in_use[X64Reg(tmp)] = false;
  registers_in_use[X64Reg(effective_address)] = false;
  if (make_loop)
    registers_in_use[X64Reg(loop_counter)] = false;
  ABI_PushRegistersAndAdjustStack(registers_in_use, 0);
  if (make_loop)
  {
    ABI_CallFunctionPRR(JitInterface::InvalidateICacheLinesFromJIT, &m_system.GetJitInterface(),
                        effective_address, loop_counter);
  }
  else
  {
    ABI_CallFunctionPR(JitInterface::InvalidateICacheLineFromJIT, &m_system.GetJitInterface(),
                       effective_address);
  }
  ABI_PopRegistersAndAdjustStack(registers_in_use, 0);
  asm_routines.ResetStack(*this);

  FixupBranch done = J(Jump::Near);
  SwitchToNearCode();
  SetJumpTarget(done);
}

void Jit64::dcbt(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITLoadStoreOff);

  // Prefetch. Since we don't emulate the data cache, we don't need to do anything.

  // If a dcbst follows a dcbt, it probably isn't a case of dynamic code
  // modification, so don't bother invalidating the jit block cache.
  // This is important because invalidating the block cache when we don't
  // need to is terrible for performance.
  // (Invalidating the jit block cache on dcbst is a heuristic.)
  if (CanMergeNextInstructions(1) && js.op[1].inst.OPCD == 31 && js.op[1].inst.SUBOP10 == 54 &&
      js.op[1].inst.RA == inst.RA && js.op[1].inst.RB == inst.RB)
  {
    js.skipInstructions = 1;
  }
}

// Zero cache line.
void Jit64::dcbz(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITLoadStoreOff);

  int a = inst.RA;
  int b = inst.RB;

  {
    RCOpArg ra = a ? gpr.Use(a, RCMode::Read) : RCOpArg::Imm32(0);
    RCOpArg rb = gpr.Use(b, RCMode::Read);
    RegCache::Realize(ra, rb);

    MOV_sum(32, RSCRATCH, ra, rb);
    AND(32, R(RSCRATCH), Imm32(~31));
  }

  FixupBranch end_dcbz_hack;
  if (m_low_dcbz_hack)
  {
    // HACK: Don't clear any memory in the [0x8000'0000, 0x8000'8000) region.
    CMP(32, R(RSCRATCH), Imm32(0x8000'8000));
    end_dcbz_hack = J_CC(CC_L);
  }

  bool emit_fast_path = (m_ppc_state.feature_flags & FEATURE_FLAG_MSR_DR) && m_jit.jo.fastmem_arena;

  if (emit_fast_path)
  {
    // Perform lookup to see if we can use fast path.
    MOV(64, R(RSCRATCH2), ImmPtr(m_mmu.GetDBATTable().data()));
    PUSH(RSCRATCH);
    SHR(32, R(RSCRATCH), Imm8(PowerPC::BAT_INDEX_SHIFT));
    TEST(32, MComplex(RSCRATCH2, RSCRATCH, SCALE_4, 0), Imm32(PowerPC::BAT_PHYSICAL_BIT));
    POP(RSCRATCH);
    FixupBranch slow = J_CC(CC_Z, Jump::Near);

    // Fast path: compute full address, then zero out 32 bytes of memory.
    XORPS(XMM0, R(XMM0));
    MOVAPS(MComplex(RMEM, RSCRATCH, SCALE_1, 0), XMM0);
    MOVAPS(MComplex(RMEM, RSCRATCH, SCALE_1, 16), XMM0);

    // Slow path: call the general-case code.
    SwitchToFarCode();
    SetJumpTarget(slow);
  }
  MOV(32, PPCSTATE(pc), Imm32(js.compilerPC));
  BitSet32 registers_in_use = CallerSavedRegistersInUse();
  ABI_PushRegistersAndAdjustStack(registers_in_use, 0);
  ABI_CallFunctionPR(PowerPC::ClearDCacheLineFromJit, &m_mmu, RSCRATCH);
  ABI_PopRegistersAndAdjustStack(registers_in_use, 0);

  if (emit_fast_path)
  {
    FixupBranch end_far_code = J(Jump::Near);
    SwitchToNearCode();
    SetJumpTarget(end_far_code);
  }

  if (m_low_dcbz_hack)
    SetJumpTarget(end_dcbz_hack);
}

void Jit64::stX(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITLoadStoreOff);

  int s = inst.RS;
  int a = inst.RA;
  s32 offset = (s32)(s16)inst.SIMM_16;
  bool update = (inst.OPCD & 1) && offset;

  if (!a && update)
    PanicAlertFmt("Invalid stX");

  int access_size;
  switch (inst.OPCD & ~1)
  {
  case 36:  // stw
    access_size = 32;
    break;
  case 44:  // sth
    access_size = 16;
    break;
  case 38:  // stb
    access_size = 8;
    break;
  default:
    ASSERT_MSG(DYNA_REC, 0, "stX: Invalid access size.");
    return;
  }

  // If we already know the address of the write
  if (!a || gpr.IsImm(a))
  {
    const u32 addr = (a ? gpr.Imm32(a) : 0) + offset;
    const bool exception = [&] {
      RCOpArg rs = gpr.Use(s, RCMode::Read);
      RegCache::Realize(rs);
      return WriteToConstAddress(access_size, rs, addr, CallerSavedRegistersInUse());
    }();
    if (update)
    {
      if (!jo.memcheck || !exception)
      {
        gpr.SetImmediate32(a, addr);
      }
      else
      {
        RCOpArg ra = gpr.RevertableBind(a, RCMode::Write);
        RegCache::Realize(ra);
        MemoryExceptionCheck();
        MOV(32, ra, Imm32(addr));
      }
    }
  }
  else
  {
    RCX64Reg ra = gpr.Bind(a, update ? RCMode::ReadWrite : RCMode::Read);
    RCOpArg reg_value;
    if (!gpr.IsImm(s) && WriteClobbersRegValue(access_size, /* swap */ true))
    {
      RCOpArg rs = gpr.Use(s, RCMode::Read);
      RegCache::Realize(rs);
      reg_value = RCOpArg::R(RSCRATCH2);
      MOV(32, reg_value, rs);
    }
    else
    {
      reg_value = gpr.BindOrImm(s, RCMode::Read);
    }
    RegCache::Realize(ra, reg_value);
    SafeWriteRegToReg(reg_value, ra, access_size, offset, CallerSavedRegistersInUse(),
                      SAFE_LOADSTORE_CLOBBER_RSCRATCH_INSTEAD_OF_ADDR);

    if (update)
      ADD(32, ra, Imm32((u32)offset));
  }
}

void Jit64::stXx(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITLoadStoreOff);

  int a = inst.RA, b = inst.RB, s = inst.RS;
  bool update = !!(inst.SUBOP10 & 32);
  bool byte_reverse = !!(inst.SUBOP10 & 512);
  FALLBACK_IF(!a || (update && a == s) || (update && jo.memcheck && a == b));

  int access_size;
  switch (inst.SUBOP10 & ~32)
  {
  case 151:
  case 662:
    access_size = 32;
    break;
  case 407:
  case 918:
    access_size = 16;
    break;
  case 215:
    access_size = 8;
    break;
  default:
    PanicAlertFmt("stXx: invalid access size");
    access_size = 0;
    break;
  }

  const bool does_clobber = WriteClobbersRegValue(access_size, /* swap */ !byte_reverse);

  RCOpArg ra = update ? gpr.Bind(a, RCMode::ReadWrite) : gpr.Use(a, RCMode::Read);
  RCOpArg rb = gpr.Use(b, RCMode::Read);
  RCOpArg rs = does_clobber ? gpr.Use(s, RCMode::Read) : gpr.BindOrImm(s, RCMode::Read);
  RegCache::Realize(ra, rb, rs);

  MOV_sum(32, RSCRATCH2, ra, rb);

  if (!rs.IsImm() && does_clobber)
  {
    MOV(32, R(RSCRATCH), rs);
    rs = RCOpArg::R(RSCRATCH);
  }
  BitSet32 registers_in_use = CallerSavedRegistersInUse();
  if (update)
    registers_in_use[RSCRATCH2] = true;
  SafeWriteRegToReg(rs, RSCRATCH2, access_size, 0, registers_in_use,
                    byte_reverse ? SAFE_LOADSTORE_NO_SWAP : 0);

  if (update)
    MOV(32, ra, R(RSCRATCH2));
}

// A few games use these heavily in video codecs.
void Jit64::lmw(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITLoadStoreOff);

  int a = inst.RA, d = inst.RD;

  // TODO: This doesn't handle rollback on DSI correctly
  {
    RCOpArg ra = a ? gpr.Use(a, RCMode::Read) : RCOpArg::Imm32(0);
    RegCache::Realize(ra);
    MOV_sum(32, RSCRATCH2, ra, Imm32((u32)(s32)inst.SIMM_16));
  }
  for (int i = d; i < 32; i++)
  {
    SafeLoadToReg(RSCRATCH, R(RSCRATCH2), 32, (i - d) * 4,
                  CallerSavedRegistersInUse() | BitSet32{RSCRATCH2}, false);
    RCOpArg ri = gpr.Bind(i, RCMode::Write);
    RegCache::Realize(ri);
    MOV(32, ri, R(RSCRATCH));
  }
}

void Jit64::stmw(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITLoadStoreOff);

  int a = inst.RA, d = inst.RD;

  // TODO: This doesn't handle rollback on DSI correctly
  for (int i = d; i < 32; i++)
  {
    RCOpArg ra = a ? gpr.Use(a, RCMode::Read) : RCOpArg::Imm32(0);
    RCOpArg ri = gpr.Use(i, RCMode::Read);
    RegCache::Realize(ra, ri);

    if (ra.IsZero())
      XOR(32, R(RSCRATCH), R(RSCRATCH));
    else
      MOV(32, R(RSCRATCH), ra);
    if (!ri.IsImm())
    {
      MOV(32, R(RSCRATCH2), ri);
      ri = RCOpArg::R(RSCRATCH2);
    }
    SafeWriteRegToReg(ri, RSCRATCH, 32, (i - d) * 4 + (u32)(s32)inst.SIMM_16,
                      CallerSavedRegistersInUse());
  }
}

void Jit64::eieio(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITLoadStoreOff);

  // optimizeGatherPipe generally postpones FIFO checks to the end of the JIT block,
  // which is generally safe. However postponing FIFO writes across eieio instructions
  // is incorrect (would crash NBA2K11 strap screen if we improve our FIFO detection).
  if (jo.optimizeGatherPipe && js.fifoBytesSinceCheck > 0)
    js.mustCheckFifo = true;
}

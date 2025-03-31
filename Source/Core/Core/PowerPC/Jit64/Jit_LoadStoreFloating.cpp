// Copyright 2008 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "Core/PowerPC/Jit64/Jit.h"

#include "Common/BitSet.h"
#include "Common/CommonTypes.h"
#include "Common/x64Emitter.h"
#include "Core/PowerPC/Jit64/RegCache/JitRegCache.h"
#include "Core/PowerPC/Jit64Common/Jit64PowerPCState.h"

using namespace Gen;

// TODO: Add peephole optimizations for multiple consecutive lfd/lfs/stfd/stfs since they are so
// common,
// and pshufb could help a lot.

void Jit64::lfXXX(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITLoadStoreFloatingOff);
  bool indexed = inst.OPCD == 31;
  bool update = indexed ? !!(inst.SUBOP10 & 0x20) : !!(inst.OPCD & 1);
  bool single = indexed ? !(inst.SUBOP10 & 0x40) : !(inst.OPCD & 2);
  update &= indexed || (inst.SIMM_16 != 0);

  int d = inst.RD;
  int a = inst.RA;
  int b = inst.RB;

  FALLBACK_IF(!indexed && !a);

  s32 offset = 0;
  RCOpArg addr = gpr.Bind(a, update ? RCMode::ReadWrite : RCMode::Read);
  RegCache::Realize(addr);

  if (update && jo.memcheck)
  {
    MOV(32, R(RSCRATCH2), addr);
    addr = RCOpArg::R(RSCRATCH2);
  }
  if (indexed)
  {
    RCOpArg rb = gpr.Use(b, RCMode::Read);
    RegCache::Realize(rb);
    if (update)
    {
      ADD(32, addr, rb);
    }
    else
    {
      MOV_sum(32, RSCRATCH2, a ? addr.Location() : Imm32(0), rb);
      addr = RCOpArg::R(RSCRATCH2);
    }
  }
  else
  {
    if (update)
      ADD(32, addr, Imm32((s32)(s16)inst.SIMM_16));
    else
      offset = (s16)inst.SIMM_16;
  }

  RCMode rd_mode = !single ? RCMode::ReadWrite : RCMode::Write;
  RCX64Reg rd = jo.memcheck && single ? fpr.RevertableBind(d, rd_mode) : fpr.Bind(d, rd_mode);
  RegCache::Realize(rd);
  BitSet32 registers_in_use = CallerSavedRegistersInUse();
  if (update && jo.memcheck)
    registers_in_use[RSCRATCH2] = true;
  SafeLoadToReg(RSCRATCH, addr, single ? 32 : 64, offset, registers_in_use, false);

  if (single)
  {
    ConvertSingleToDouble(rd, RSCRATCH, true);
  }
  else
  {
    MOVQ_xmm(XMM0, R(RSCRATCH));
    MOVSD(rd, R(XMM0));
  }
  if (update && jo.memcheck)
  {
    RCX64Reg ra = gpr.Bind(a, RCMode::Write);
    RegCache::Realize(ra);
    MOV(32, ra, addr);
  }
}

void Jit64::stfXXX(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITLoadStoreFloatingOff);
  bool indexed = inst.OPCD == 31;
  bool update = indexed ? !!(inst.SUBOP10 & 0x20) : !!(inst.OPCD & 1);
  bool single = indexed ? !(inst.SUBOP10 & 0x40) : !(inst.OPCD & 2);
  update &= indexed || (inst.SIMM_16 != 0);

  int s = inst.RS;
  int a = inst.RA;
  int b = inst.RB;
  s32 imm = (s16)inst.SIMM_16;
  int access_size = single ? 32 : 64;

  FALLBACK_IF(update && jo.memcheck && indexed && a == b);

  if (single)
  {
    if (js.fpr_is_store_safe[s] && js.op->fprIsSingle[s])
    {
      RCOpArg rs = fpr.Use(s, RCMode::Read);
      RegCache::Realize(rs);
      CVTSD2SS(XMM0, rs);
      MOVD_xmm(R(RSCRATCH), XMM0);
    }
    else
    {
      RCX64Reg rs = fpr.Bind(s, RCMode::Read);
      RegCache::Realize(rs);
      MOVAPD(XMM0, rs);
      CALL(asm_routines.cdts);
    }
  }
  else
  {
    RCOpArg rs = fpr.Use(s, RCMode::Read);
    RegCache::Realize(rs);
    if (rs.IsSimpleReg())
      MOVQ_xmm(R(RSCRATCH), rs.GetSimpleReg());
    else
      MOV(64, R(RSCRATCH), rs);
  }

  if (!indexed && (!a || gpr.IsImm(a)))
  {
    u32 addr = (a ? gpr.Imm32(a) : 0) + imm;
    bool exception =
        WriteToConstAddress(access_size, R(RSCRATCH), addr, CallerSavedRegistersInUse());

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
    return;
  }

  s32 offset = 0;
  RCOpArg ra = update ? gpr.Bind(a, RCMode::ReadWrite) : gpr.Use(a, RCMode::Read);
  RegCache::Realize(ra);
  if (indexed)
  {
    RCOpArg rb = gpr.Use(b, RCMode::Read);
    RegCache::Realize(rb);
    MOV_sum(32, RSCRATCH2, a ? ra.Location() : Imm32(0), rb);
  }
  else
  {
    if (update)
    {
      MOV_sum(32, RSCRATCH2, ra, Imm32(imm));
    }
    else
    {
      offset = imm;
      MOV(32, R(RSCRATCH2), ra);
    }
  }

  BitSet32 registers_in_use = CallerSavedRegistersInUse();
  // We need to save the (usually scratch) address register for the update.
  if (update)
    registers_in_use[RSCRATCH2] = true;

  SafeWriteRegToReg(RSCRATCH, RSCRATCH2, access_size, offset, registers_in_use);

  if (update)
    MOV(32, ra, R(RSCRATCH2));
}

// This one is a little bit weird; it stores the low 32 bits of a double without converting it
void Jit64::stfiwx(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITLoadStoreFloatingOff);

  int s = inst.RS;
  int a = inst.RA;
  int b = inst.RB;

  RCOpArg ra = a ? gpr.Use(a, RCMode::Read) : RCOpArg::Imm32(0);
  RCOpArg rb = gpr.Use(b, RCMode::Read);
  RCOpArg rs = fpr.Use(s, RCMode::Read);
  RegCache::Realize(ra, rb, rs);

  MOV_sum(32, RSCRATCH2, ra, rb);

  if (rs.IsSimpleReg())
    MOVD_xmm(R(RSCRATCH), rs.GetSimpleReg());
  else
    MOV(32, R(RSCRATCH), rs);
  SafeWriteRegToReg(RSCRATCH, RSCRATCH2, 32, 0, CallerSavedRegistersInUse());
}

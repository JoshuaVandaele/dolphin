// Copyright 2008 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "Core/PowerPC/Jit64/Jit.h"

#include <optional>

#include "Common/CPUDetect.h"
#include "Common/CommonTypes.h"
#include "Common/MsgHandler.h"
#include "Common/x64Emitter.h"
#include "Core/PowerPC/Jit64/RegCache/JitRegCache.h"
#include "Core/PowerPC/Jit64Common/Jit64Constants.h"

using namespace Gen;

void Jit64::ps_mr(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITPairedOff);
  FALLBACK_IF(inst.Rc);

  int d = inst.FD;
  int b = inst.FB;
  if (d == b)
    return;

  RCOpArg rb = fpr.Use(b, RCMode::Read);
  RCX64Reg rd = fpr.Bind(d, RCMode::Write);
  RegCache::Realize(rb, rd);
  MOVAPD(rd, rb);
}

void Jit64::ps_sum(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITPairedOff);
  FALLBACK_IF(inst.Rc);
  FALLBACK_IF(jo.fp_exceptions);

  int d = inst.FD;
  int a = inst.FA;
  int b = inst.FB;
  int c = inst.FC;

  RCOpArg ra = fpr.Use(a, RCMode::Read);
  RCOpArg rb = fpr.Use(b, RCMode::Read);
  RCOpArg rc = fpr.Use(c, RCMode::Read);
  RCX64Reg rd = fpr.Bind(d, RCMode::Write);
  RegCache::Realize(ra, rb, rc, rd);

  X64Reg tmp = XMM1;
  MOVDDUP(tmp, ra);  // {a.ps0, a.ps0}
  ADDPD(tmp, rb);    // {a.ps0 + b.ps0, a.ps0 + b.ps1}
  switch (inst.SUBOP5)
  {
  case 10:  // ps_sum0: {a.ps0 + b.ps1, c.ps1}
    UNPCKHPD(tmp, rc);
    break;
  case 11:  // ps_sum1: {c.ps0, a.ps0 + b.ps1}
    if (rc.IsSimpleReg())
      MOVSD(tmp, rc);
    else
      MOVLPD(tmp, rc);
    break;
  default:
    PanicAlertFmt("ps_sum WTF!!!");
  }
  // We're intentionally not calling HandleNaNs here.
  // For addition and subtraction specifically, x86's NaN behavior matches PPC's.
  FinalizeSingleResult(rd, R(tmp));
}

void Jit64::ps_muls(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITPairedOff);
  FALLBACK_IF(inst.Rc);
  FALLBACK_IF(jo.fp_exceptions);

  int d = inst.FD;
  int a = inst.FA;
  int c = inst.FC;
  bool round_input = !js.op->fprIsSingle[c];

  RCOpArg ra = fpr.Use(a, RCMode::Read);
  RCOpArg rc = fpr.Use(c, RCMode::Read);
  RCX64Reg rd = fpr.Bind(d, RCMode::Write);
  RCX64Reg rc_duplicated = m_accurate_nans ? fpr.Scratch() : fpr.Scratch(XMM1);
  RegCache::Realize(ra, rc, rd, rc_duplicated);

  switch (inst.SUBOP5)
  {
  case 12:  // ps_muls0
    MOVDDUP(rc_duplicated, rc);
    break;
  case 13:  // ps_muls1
    AVXOP(&XEmitter::VSHUFPD, &XEmitter::SHUFPD, rc_duplicated, rc, rc, 3);
    break;
  default:
    PanicAlertFmt("ps_muls WTF!!!");
  }
  if (round_input)
    Force25BitPrecision(XMM1, R(rc_duplicated), XMM0);
  else if (XMM1 != rc_duplicated)
    MOVAPD(XMM1, rc_duplicated);
  MULPD(XMM1, ra);
  HandleNaNs(inst, XMM1, XMM0, ra, std::nullopt, rc_duplicated);
  FinalizeSingleResult(rd, R(XMM1));
}

void Jit64::ps_mergeXX(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITPairedOff);
  FALLBACK_IF(inst.Rc);

  int d = inst.FD;
  int a = inst.FA;
  int b = inst.FB;

  RCOpArg ra = fpr.Use(a, RCMode::Read);
  RCOpArg rb = fpr.Use(b, RCMode::Read);
  RCX64Reg rd = fpr.Bind(d, RCMode::Write);
  RegCache::Realize(ra, rb, rd);

  switch (inst.SUBOP10)
  {
  case 528:
    AVXOP(&XEmitter::VUNPCKLPD, &XEmitter::UNPCKLPD, rd, ra, rb);
    break;  // 00
  case 560:
    if (d != b)
      AVXOP(&XEmitter::VSHUFPD, &XEmitter::SHUFPD, rd, ra, rb, 2);
    else if (ra.IsSimpleReg())
      MOVSD(rd, ra);
    else
      MOVLPD(rd, ra);
    break;  // 01
  case 592:
    AVXOP(&XEmitter::VSHUFPD, &XEmitter::SHUFPD, rd, ra, rb, 1);
    break;  // 10
  case 624:
    AVXOP(&XEmitter::VUNPCKHPD, &XEmitter::UNPCKHPD, rd, ra, rb);
    break;  // 11
  default:
    ASSERT_MSG(DYNA_REC, 0, "ps_merge - invalid op");
  }
}

void Jit64::ps_rsqrte(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITFloatingPointOff);
  FALLBACK_IF(inst.Rc);
  FALLBACK_IF(jo.fp_exceptions || jo.div_by_zero_exceptions);
  int b = inst.FB;
  int d = inst.FD;

  RCX64Reg scratch_guard = gpr.Scratch(RSCRATCH_EXTRA);
  RCX64Reg rb = fpr.Bind(b, RCMode::Read);
  RCX64Reg rd = fpr.Bind(d, RCMode::Write);
  RegCache::Realize(scratch_guard, rb, rd);

  MOVSD(XMM0, rb);
  CALL(asm_routines.frsqrte);
  MOVSD(rd, XMM0);

  MOVHLPS(XMM0, rb);
  CALL(asm_routines.frsqrte);
  MOVLHPS(rd, XMM0);

  FinalizeSingleResult(rd, rd);
}

void Jit64::ps_res(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITFloatingPointOff);
  FALLBACK_IF(inst.Rc);
  FALLBACK_IF(jo.fp_exceptions || jo.div_by_zero_exceptions);
  int b = inst.FB;
  int d = inst.FD;

  RCX64Reg scratch_guard = gpr.Scratch(RSCRATCH_EXTRA);
  RCX64Reg rb = fpr.Bind(b, RCMode::Read);
  RCX64Reg rd = fpr.Bind(d, RCMode::Write);
  RegCache::Realize(scratch_guard, rb, rd);

  MOVSD(XMM0, rb);
  CALL(asm_routines.fres);
  MOVSD(rd, XMM0);

  MOVHLPS(XMM0, rb);
  CALL(asm_routines.fres);
  MOVLHPS(rd, XMM0);

  FinalizeSingleResult(rd, rd);
}

void Jit64::ps_cmpXX(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITFloatingPointOff);
  FALLBACK_IF(jo.fp_exceptions);

  FloatCompare(inst, !!(inst.SUBOP10 & 64));
}

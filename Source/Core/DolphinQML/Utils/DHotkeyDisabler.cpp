// Copyright 2026 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "DHotkeyDisabler.h"

#include "Core/HotkeyManager.h"

void DHotkeyDisabler::setEnabled(bool enabled)
{
  HotkeyManagerEmu::Enable(enabled);
}

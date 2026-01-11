// Copyright 2026 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "Core/Config/EditPolicy.h"

#include "Core/AchievementManager.h"
#include "Core/Core.h"
#include "Core/NetPlayProto.h"
#include "Core/System.h"

namespace Config
{
bool IsEditPolicySatisfied(EditPolicy policy)
{
  switch (policy)
  {
  case EditPolicy::Unconditional:
    return true;
  case EditPolicy::NotRunning:
    return Core::IsUninitialized(Core::System::GetInstance());
  case EditPolicy::NotInNetplay:
    return !NetPlay::IsNetPlayRunning();
  case EditPolicy::NotInHardcore:
    return !AchievementManager::GetInstance().IsHardcoreModeActive();
  }

  return true;
}
}  // namespace Config

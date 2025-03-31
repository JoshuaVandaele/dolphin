// Copyright 2019 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "Core/HW/WiimoteEmu/Extension/UDrawTablet.h"

#include <array>

#include "Common/Assert.h"
#include "Common/BitUtils.h"
#include "Common/Common.h"
#include "Common/CommonTypes.h"

#include "Core/HW/WiimoteEmu/Extension/DesiredExtensionState.h"
#include "Core/HW/WiimoteEmu/WiimoteEmu.h"

#include "InputCommon/ControllerEmu/Control/Input.h"
#include "InputCommon/ControllerEmu/ControlGroup/AnalogStick.h"
#include "InputCommon/ControllerEmu/ControlGroup/Buttons.h"
#include "InputCommon/ControllerEmu/ControlGroup/Triggers.h"

namespace WiimoteEmu
{
constexpr std::array<u8, 6> UDRAW_TABLET_ID{{0xff, 0x00, 0xa4, 0x20, 0x01, 0x12}};

constexpr std::array<u8, 2> UDRAW_TABLET_BUTTON_BITMASKS{{
    UDrawTablet::BUTTON_ROCKER_UP,
    UDrawTablet::BUTTON_ROCKER_DOWN,
}};

constexpr std::array<const char*, 2> UDRAW_TABLET_BUTTON_NAMES{{
    _trans("Rocker Up"),
    _trans("Rocker Down"),
}};

UDrawTablet::UDrawTablet() : Extension3rdParty("uDraw", _trans("uDraw GameTablet"))
{
  using Translatability = ControllerEmu::Translatability;

  // Buttons
  groups.emplace_back(m_buttons = new ControllerEmu::Buttons(_trans("Buttons")));
  for (auto& button_name : UDRAW_TABLET_BUTTON_NAMES)
  {
    m_buttons->AddInput(Translatability::Translate, button_name);
  }

  // Stylus
  groups.emplace_back(m_stylus = new ControllerEmu::AnalogStick(
                          _trans("Stylus"), std::make_unique<ControllerEmu::SquareStickGate>(1.0)));

  // Touch
  groups.emplace_back(m_touch = new ControllerEmu::Triggers(_trans("Touch")));
  m_touch->AddInput(Translatability::Translate, _trans("Pressure"));
  m_touch->AddInput(Translatability::Translate, _trans("Lift"));
}

void UDrawTablet::BuildDesiredExtensionState(DesiredExtensionState* target_state)
{
  DataFormat tablet_data = {};

  // Pressure:
  // Min/Max values produced on my device: 0x08, 0xf2
  // We're just gonna assume it's an old sensor and use the full byte range:
  // Note: Pressure values are valid even when stylus is lifted.
  constexpr u8 MAX_PRESSURE = 0xff;

  const auto touch_state = m_touch->GetState();
  tablet_data.pressure = static_cast<u8>(touch_state.data[0] * MAX_PRESSURE);

  // Stylus X/Y:
  // Min/Max X values (when touched) produced on my device: 0x4f, 0x7B3
  // Drawing area edge (approx) X values on my device: 0x56, 0x7a5
  // Min/Max Y values (when touched) produced on my device: 0x53, 0x5b4
  // Drawing area edge (approx) Y values on my device: 0x5e, 0x5a5

  // Calibrated for "uDraw Studio: Instant Artist".
  constexpr u16 MIN_X = 0x56;
  constexpr u16 MAX_X = 0x780;
  constexpr u16 MIN_Y = 0x65;
  constexpr u16 MAX_Y = 0x5a5;
  constexpr double CENTER_X = (MAX_X + MIN_X) / 2.0;
  constexpr double CENTER_Y = (MAX_Y + MIN_Y) / 2.0;

  // Neutral (lifted) stylus state:
  u16 stylus_x = 0xfff;
  u16 stylus_y = 0xfff;

  const bool is_stylus_lifted = std::lround(touch_state.data[1]) != 0;

  const auto stylus_state = m_stylus->GetState();

  if (!is_stylus_lifted)
  {
    stylus_x = u16(CENTER_X + stylus_state.x * (MAX_X - CENTER_X));
    stylus_y = u16(CENTER_Y + stylus_state.y * (MAX_Y - CENTER_Y));
  }

  tablet_data.stylus_x1 = stylus_x & 0xff;
  tablet_data.stylus_x2 = stylus_x >> 8;
  tablet_data.stylus_y1 = stylus_y & 0xff;
  tablet_data.stylus_y2 = stylus_y >> 8;

  // Buttons:
  m_buttons->GetState(&tablet_data.buttons, UDRAW_TABLET_BUTTON_BITMASKS.data());

  // Flip button bits
  constexpr u8 BUTTONS_NEUTRAL_STATE = 0xfb;
  tablet_data.buttons ^= BUTTONS_NEUTRAL_STATE;

  // Always 0xff
  tablet_data.unk = 0xff;

  target_state->data = tablet_data;
}

void UDrawTablet::Update(const DesiredExtensionState& target_state)
{
  DefaultExtensionUpdate<DataFormat>(&m_reg, target_state);
}

void UDrawTablet::Reset()
{
  EncryptedExtension::Reset();

  m_reg.identifier = UDRAW_TABLET_ID;

  // Both 0x20 and 0x30 calibration sections are just filled with 0xff on the real tablet:
  m_reg.calibration.fill(0xff);
}

ControllerEmu::ControlGroup* UDrawTablet::GetGroup(UDrawTabletGroup group)
{
  switch (group)
  {
  case UDrawTabletGroup::Buttons:
    return m_buttons;
  case UDrawTabletGroup::Stylus:
    return m_stylus;
  case UDrawTabletGroup::Touch:
    return m_touch;
  default:
    ASSERT(false);
    return nullptr;
  }
}

}  // namespace WiimoteEmu

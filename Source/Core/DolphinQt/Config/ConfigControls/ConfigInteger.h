// Copyright 2019 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#pragma once

#include <QLabel>
#include <QPointer>

#include <concepts>

#include "Common/Config/ConfigInfo.h"
#include "DolphinQt/Config/ConfigControls/ConfigControl.h"
#include "DolphinQt/Config/ToolTipControls/ToolTipSpinBox.h"

class ConfigIntegerBase : public ConfigControl<ToolTipSpinBox>
{
  Q_OBJECT

public:
  ConfigIntegerBase(const Config::Location& location, Config::Layer* layer)
      : ConfigControl<ToolTipSpinBox>(location, layer)
  {
  }
};

template <std::integral T>
class ConfigInteger final : public ConfigIntegerBase
{
public:
  ConfigInteger(const Config::Info<T>& setting, T step = 1)
      : ConfigInteger(std::numeric_limits<T>::min(), std::numeric_limits<T>::max(), setting,
                      nullptr, step)
  {
  }
  ConfigInteger(T minimum, T maximum, const Config::Info<T>& setting, T step = 1)
      : ConfigInteger(minimum, maximum, setting, nullptr, step)
  {
  }
  ConfigInteger(T minimum, T maximum, const Config::Info<T>& setting, Config::Layer* layer,
                T step = 1)
      : ConfigIntegerBase(setting.GetLocation(), layer), m_setting(setting)
  {
    setMinimum(minimum);
    setMaximum(maximum);
    setSingleStep(step);
    setValue(ReadValue(setting));

    connect(this, &ConfigInteger::valueChanged, this, &ConfigInteger::Update);
  }

  void Update(T value) { SaveValue(m_setting, value); }

protected:
  void OnConfigChanged() override { setValue(ReadValue(m_setting)); }

private:
  const Config::Info<T> m_setting;
};

template <std::integral T>
class ConfigIntegerLabel final : public QLabel
{
public:
  ConfigIntegerLabel(const QString& text, ConfigInteger<T>* widget) : QLabel(text), m_widget(widget)
  {
    connect(&Settings::Instance(), &Settings::ConfigChanged, this, [this] {
      if (m_widget)
        setFont(m_widget->font());
    });
  }

private:
  QPointer<ConfigInteger<T>> m_widget;
};

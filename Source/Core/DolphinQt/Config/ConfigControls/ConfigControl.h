// Copyright 2024 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#pragma once

#include <functional>
#include <utility>
#include <vector>

#include <QFont>
#include <QMouseEvent>
#include <QWidget>

#include "Common/Config/Enums.h"
#include "Common/Config/Layer.h"
#include "Core/Config/EditPolicy.h"
#include "DolphinQt/Settings.h"

namespace Config
{
template <typename T>
class Info;
struct Location;

template <typename Func>
void ConnectEditPolicySignals(QObject* receiver, EditPolicy policy, Func callback)
{
  switch (policy)
  {
  case EditPolicy::Unconditional:
    break;
  case EditPolicy::NotRunning:
  case EditPolicy::NotInNetplay:
    QObject::connect(&Settings::Instance(), &Settings::EmulationStateChanged, receiver, callback);
    break;
  case EditPolicy::NotInHardcore:
    QObject::connect(&Settings::Instance(), &Settings::HardcoreModeChanged, receiver, callback);
    break;
  }
}
}  // namespace Config

template <class Derived>
class ConfigControl : public Derived
{
public:
  ConfigControl(const Config::Location& location, Config::Layer* layer,
                Config::EditPolicy edit_policy)
      : m_location(location), m_layer(layer), m_edit_policy(edit_policy)
  {
    ConnectConfig();
  }
  ConfigControl(const QString& label, const Config::Location& location, Config::Layer* layer,
                Config::EditPolicy edit_policy)
      : Derived(label), m_location(location), m_layer(layer), m_edit_policy(edit_policy)
  {
    ConnectConfig();
  }
  ConfigControl(const Qt::Orientation& orient, const Config::Location& location,
                Config::Layer* layer, Config::EditPolicy edit_policy)
      : Derived(orient), m_location(location), m_layer(layer), m_edit_policy(edit_policy)
  {
    ConnectConfig();
  }

  const Config::Location GetLocation() const { return m_location; }

  // Re-evaluates whether this control is enabled. Runs automatically on
  // config/edit policy changes, but needs to be called manually when an edit condition changes.
  void UpdateEnabledState()
  {
    const bool enabled =
        Config::IsEditPolicySatisfied(m_edit_policy) && (!m_edit_condition || m_edit_condition());

    Derived::setEnabled(enabled);
    for (QWidget* widget : m_dependent_widgets)
      widget->setEnabled(enabled);
  }

  // Adds an extra condition on top of the edit policy.
  void SetEditCondition(std::function<bool()> condition)
  {
    m_edit_condition = std::move(condition);
    UpdateEnabledState();
  }

  // Adds a widget, such as a label, that should be enabled and disabled along with this control.
  void AddDependentWidget(QWidget* widget)
  {
    m_dependent_widgets.push_back(widget);
    widget->setEnabled(Derived::isEnabled());
  }

protected:
  void ConnectConfig()
  {
    Derived::connect(&Settings::Instance(), &Settings::ConfigChanged, this, [this] {
      QFont bf = Derived::font();
      bf.setBold(IsConfigLocal());
      Derived::setFont(bf);

      // This isn't signal blocked because the UI may need to be updated.
      m_updating = true;
      OnConfigChanged();
      m_updating = false;

      UpdateEnabledState();
    });

    UpdateEnabledState();
    Config::ConnectEditPolicySignals(this, m_edit_policy, [this] { UpdateEnabledState(); });
  }

  template <typename T>
  void SaveValue(const Config::Info<T>& setting, const T& value)
  {
    // Avoid OnConfigChanged -> option changed to current config's value -> unnecessary save ->
    // ConfigChanged.
    if (m_updating)
      return;

    if (m_layer != nullptr)
    {
      m_layer->Set(m_location, value);
      Config::OnConfigChanged();
      return;
    }

    Config::SetBaseOrCurrent(setting, value);
  }

  template <typename T>
  const T ReadValue(const Config::Info<T>& setting) const
  {
    // For loading game specific settings.  If the game setting doesn't exist, load the current
    // global setting. There's no way to know what game is being edited, so GlobalGame settings
    // can't be shown, but otherwise would be good to include.
    if (m_layer != nullptr)
    {
      if (m_layer->Exists(m_location))
        return m_layer->Get(setting);
      else
        return Config::GetBase(setting);
    }

    return Config::Get(setting);
  }

  virtual void OnConfigChanged() {}

private:
  bool IsConfigLocal() const
  {
    if (m_layer != nullptr)
      return m_layer->Exists(m_location);
    else
      return Config::GetActiveLayerForConfig(m_location) != Config::LayerType::Base;
  }

  void mousePressEvent(QMouseEvent* event) override
  {
    if (m_layer != nullptr && event->button() == Qt::RightButton)
    {
      m_layer->DeleteKey(m_location);
      Config::OnConfigChanged();
    }
    else
    {
      Derived::mousePressEvent(event);
    }
  }

  bool m_updating = false;
  const Config::Location m_location;
  Config::Layer* m_layer;
  const Config::EditPolicy m_edit_policy;
  std::function<bool()> m_edit_condition;
  std::vector<QWidget*> m_dependent_widgets;
};

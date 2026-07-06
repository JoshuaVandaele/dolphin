// Copyright 2026 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#pragma once

#include <QObject>
#include <QtQmlIntegration>

class DHotkeyDisabler : public QObject
{
  Q_OBJECT
  QML_SINGLETON
  QML_ELEMENT

public:
  explicit DHotkeyDisabler(QObject* parent = nullptr) : QObject(parent) {}

  Q_INVOKABLE void setEnabled(bool enabled);
};

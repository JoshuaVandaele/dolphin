// Copyright 2026 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#pragma once

#include <QObject>
#include <QQuickWindow>
#include <QUrl>
#include <QtQmlIntegration>

class QMLUtils : public QObject
{
  Q_OBJECT
  QML_SINGLETON
  QML_ELEMENT

public:
  explicit QMLUtils(QObject* parent = nullptr) : QObject(parent) {}

  Q_INVOKABLE void SetWindowIcon(QQuickWindow* window, const QUrl& iconUrl);
};

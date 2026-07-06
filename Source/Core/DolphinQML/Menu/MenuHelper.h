// Copyright 2026 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#pragma once

#include <QObject>
#include <QtQmlIntegration>

class MenuHelper : public QObject
{
  Q_OBJECT
  QML_SINGLETON
  QML_ELEMENT

public:
  MenuHelper(QObject* parent = nullptr) : QObject(parent) {}

  Q_INVOKABLE void Open(const QUrl& file);

  Q_INVOKABLE void ChangeDisc(const QUrl& file);
  Q_INVOKABLE void EjectDisc();

  Q_INVOKABLE bool ShouldShowConfigFolder();
  Q_INVOKABLE bool ShouldShowCacheFolder();

  Q_INVOKABLE QUrl GetUserFolder();
  Q_INVOKABLE QUrl GetConfigFolder();
  Q_INVOKABLE QUrl GetCacheFolder();
};

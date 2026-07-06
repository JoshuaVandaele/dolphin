// Copyright 2026 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "QMLUtils.h"

#include <QIcon>
#include <QQuickWindow>
#include <QUrl>

void QMLUtils::SetWindowIcon(QQuickWindow* window, const QUrl& iconUrl)
{
  if (!window)
    return;

  QString resourcePath = iconUrl.scheme() == QStringLiteral("qrc") ?
                             QStringLiteral(":") + iconUrl.path() :
                             iconUrl.toLocalFile();

  window->setIcon(QIcon(resourcePath));
}

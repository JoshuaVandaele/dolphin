// Copyright 2026 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "MenuHelper.h"

#include "Common/CommonPaths.h"
#include "Common/FileUtil.h"

#include "Core/Core.h"
#include "Core/HW/DVD/DVDInterface.h"
#include "Core/System.h"

void MenuHelper::Open(const QUrl& file)
{
  if (!file.isLocalFile())
    return;
  QString path = file.toLocalFile();
  // TODO
}

void MenuHelper::ChangeDisc(const QUrl& file)
{
  if (!file.isLocalFile())
    return;
  QString path = file.toLocalFile();
  auto& system = Core::System::GetInstance();
  system.GetDVDInterface().ChangeDisc(Core::CPUThreadGuard{system}, path.toStdString());
}

void MenuHelper::EjectDisc()
{
  auto& system = Core::System::GetInstance();
  system.GetDVDInterface().EjectDisc(Core::CPUThreadGuard{system}, DVD::EjectCause::User);
}

bool MenuHelper::ShouldShowConfigFolder()
{
  const std::string user_path = File::GetUserPath(D_USER_IDX);
  const std::string default_path = user_path + CONFIG_DIR + DIR_SEP;

  const std::string configured_path = File::GetUserPath(D_CONFIG_IDX);

  return default_path != configured_path;
}

bool MenuHelper::ShouldShowCacheFolder()
{
  const std::string user_path = File::GetUserPath(D_USER_IDX);
  const std::string default_path = user_path + CACHE_DIR + DIR_SEP;

  const std::string configured_path = File::GetUserPath(D_CACHE_IDX);

  return default_path != configured_path;
}

QUrl MenuHelper::GetUserFolder()
{
  std::string path = File::GetUserPath(D_USER_IDX);
  return QUrl::fromLocalFile(QString::fromStdString(path));
}

QUrl MenuHelper::GetConfigFolder()
{
  std::string path = File::GetUserPath(D_CONFIG_IDX);
  return QUrl::fromLocalFile(QString::fromStdString(path));
}

QUrl MenuHelper::GetCacheFolder()
{
  std::string path = File::GetUserPath(D_CACHE_IDX);
  return QUrl::fromLocalFile(QString::fromStdString(path));
}

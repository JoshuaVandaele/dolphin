// Copyright 2026 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later
#include "AppInfo.h"
#include "Common/Version.h"

#include <QString>
#include <QtVersion>

QString AppInfo::EmulatorName() const
{
  return QString::fromStdString(Common::GetEmulatorName());
}

QString AppInfo::ScmRevStr() const
{
  return QString::fromStdString(Common::GetScmRevStr());
}

QString AppInfo::ScmRevGitStr() const
{
  return QString::fromStdString(Common::GetScmRevGitStr());
}

QString AppInfo::ScmDescStr() const
{
  return QString::fromStdString(Common::GetScmDescStr());
}

QString AppInfo::ScmBranchStr() const
{
  return QString::fromStdString(Common::GetScmBranchStr());
}

QString AppInfo::UserAgentStr() const
{
  return QString::fromStdString(Common::GetUserAgentStr());
}

QString AppInfo::ScmDistributorStr() const
{
  return QString::fromStdString(Common::GetScmDistributorStr());
}

QString AppInfo::ScmUpdateTrackStr() const
{
  return QString::fromStdString(Common::GetScmUpdateTrackStr());
}

QString AppInfo::NetplayDolphinVer() const
{
  return QString::fromStdString(Common::GetNetplayDolphinVer());
}

int AppInfo::ScmCommitsAheadMaster() const
{
  return Common::GetScmCommitsAheadMaster();
}

QString AppInfo::QtVersion() const
{
  return QString::fromLatin1(qVersion());
}

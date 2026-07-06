// Copyright 2026 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later
#pragma once
#include <QObject>
#include <QString>
#include <QtQmlIntegration>

class AppInfo : public QObject
{
  Q_OBJECT
  QML_SINGLETON
  QML_ELEMENT

  Q_PROPERTY(QString EmulatorName READ EmulatorName CONSTANT)
  Q_PROPERTY(QString ScmRevStr READ ScmRevStr CONSTANT)
  Q_PROPERTY(QString ScmRevGitStr READ ScmRevGitStr CONSTANT)
  Q_PROPERTY(QString ScmDescStr READ ScmDescStr CONSTANT)
  Q_PROPERTY(QString ScmBranchStr READ ScmBranchStr CONSTANT)
  Q_PROPERTY(QString UserAgentStr READ UserAgentStr CONSTANT)
  Q_PROPERTY(QString ScmDistributorStr READ ScmDistributorStr CONSTANT)
  Q_PROPERTY(QString ScmUpdateTrackStr READ ScmUpdateTrackStr CONSTANT)
  Q_PROPERTY(QString NetplayDolphinVer READ NetplayDolphinVer CONSTANT)
  Q_PROPERTY(int ScmCommitsAheadMaster READ ScmCommitsAheadMaster CONSTANT)
  Q_PROPERTY(QString QtVersion READ QtVersion CONSTANT)

public:
  explicit AppInfo(QObject* parent = nullptr) : QObject(parent) {}

  QString EmulatorName() const;
  QString ScmRevStr() const;
  QString ScmRevGitStr() const;
  QString ScmDescStr() const;
  QString ScmBranchStr() const;
  QString UserAgentStr() const;
  QString ScmDistributorStr() const;
  QString ScmUpdateTrackStr() const;
  QString NetplayDolphinVer() const;
  int ScmCommitsAheadMaster() const;
  QString QtVersion() const;
};

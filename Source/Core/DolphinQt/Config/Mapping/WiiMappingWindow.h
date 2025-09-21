// Copyright 2025 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#pragma once

#include <QDialog>
#include <QWidget>

class WiiMappingWindow final : public QDialog
{
  Q_OBJECT
public:
  explicit WiiMappingWindow(QWidget* parent, const int port_num);

private:
  void ConnectWidgets();
  void CreateLayout();
};

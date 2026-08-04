// Copyright 2026 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include <QDialog>
#include <QTabWidget>
#include <QWidget>

class NetPlaySetupDialog : public QDialog
{
  Q_OBJECT
public:
  explicit NetPlaySetupDialog(QWidget* parent = nullptr);

private:
  QTabWidget* m_tab_widget;
};

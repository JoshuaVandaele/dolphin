// Copyright 2026 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "DolphinQt/NetPlay/NetPlaySetupDialog.h"

#include <QDialog>
#include <QVBoxLayout>
#include <QWidget>

#include "DolphinQt/NetPlay/ConnectTab.h"
#include "DolphinQt/Resources.h"

NetPlaySetupDialog::NetPlaySetupDialog(QWidget* parent) : QDialog(parent)
{
  setWindowTitle(tr("NetPlay Setup"));
  setWindowIcon(Resources::GetAppIcon());

  QVBoxLayout* layout = new QVBoxLayout(this);

  m_tab_widget = new QTabWidget;
  m_tab_widget->addTab(new ConnectTab, tr("Connect"));
  m_tab_widget->addTab(new QWidget, tr("Host"));
  layout->addWidget(m_tab_widget);

  setLayout(layout);

  adjustSize();
}

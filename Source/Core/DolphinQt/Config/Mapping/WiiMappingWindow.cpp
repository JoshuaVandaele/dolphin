// Copyright 2025 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "DolphinQt/Config/Mapping/WiiMappingWindow.h"

#include <QDialog>
#include <QHBoxLayout>
#include <QWidget>
#include <qcontainerfwd.h>

#include "DolphinQt/Config/Mapping/WiimoteWidget.h"
#include "DolphinQt/QtUtils/QtUtils.h"

// TODO: Attribute Wiimote SVGs to Maxence Béranger on the Noun Project (CC BY-3.0)

WiiMappingWindow::WiiMappingWindow(QWidget* parent, const int port_num) : QDialog(parent)
{
  setWindowTitle(tr("Wii Remote %1").arg(port_num + 1));

  CreateLayout();

  QtUtils::AdjustSizeWithinScreen(this);
}

void WiiMappingWindow::CreateLayout()
{
  auto* main_layout = new QHBoxLayout(this);
  main_layout->setAlignment(Qt::AlignCenter);
  main_layout->addStretch();
  auto* mapping_widget = new WiimoteWidget(this);
  main_layout->addWidget(mapping_widget);
  main_layout->addStretch();
  setLayout(main_layout);
}

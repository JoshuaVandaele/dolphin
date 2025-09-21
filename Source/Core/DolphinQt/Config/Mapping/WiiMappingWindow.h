// Copyright 2025 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#pragma once

#include <QDialog>
#include <QLabel>
#include <QPoint>
#include <QPushButton>
#include <QWidget>

#include "DolphinQt/QtUtils/ScalableSvgWidget.h"

class WiiMappingWindow final : public QDialog
{
  Q_OBJECT
public:
  explicit WiiMappingWindow(QWidget* parent, const int port_num);

private:
  enum class ButtonPlacement
  {
    Left,
    Right,
    Top,
    Bottom
  };
  enum class LabelPlacement
  {
    Auto,
    Left,
    Right,
    Top,
    Bottom
  };

  struct ButtonInfo
  {
    QPushButton* button;
    ButtonPlacement button_placement;
    QLabel* label;
    LabelPlacement label_placement;
    QString svg_element_name;
    int spacing;
    QPoint offset;
  };

  ScalableSvgWidget* m_svg_widget;
  std::vector<ButtonInfo> m_buttons;

  ButtonInfo CreateButton(std::string name, std::string svg_element_name,
                          ButtonPlacement button_placement,
                          LabelPlacement label_placement = LabelPlacement::Auto, int spacing = 10,
                          QPoint offset = QPoint());

  void CreateButtons();
  void ConnectWidgets();
  void CreateLayout();
  void PositionButtons();
};

// Copyright 2025 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#pragma once

#include <QGraphicsProxyWidget>
#include <QGraphicsScene>
#include <QGraphicsSvgItem>
#include <QGraphicsView>
#include <QSvgRenderer>
#include <QWidget>

#include <vector>

class QLabel;
class QPushButton;

class WiimoteWidget : public QWidget
{
  Q_OBJECT

public:
  explicit WiimoteWidget(QWidget* parent = nullptr, int port_num = 0);

  void resizeEvent(QResizeEvent* event) override;

private:
  struct ButtonInfo
  {
    QWidget* button;
    QString svg_element_name;
  };

  struct ButtonGroup
  {
    std::vector<ButtonInfo> buttons;
    enum class Placement
    {
      Left,
      Right,
      Top,
      Bottom
    } placement;
    QGraphicsProxyWidget* proxy = nullptr;
  };

  std::vector<ButtonGroup> m_button_groups;

  QGraphicsView* m_view;
  QGraphicsScene* m_scene;
  QGraphicsSvgItem* m_svg_item;
  QSvgRenderer* m_renderer;

  ButtonInfo CreateButton(std::string label, std::string svg_element_name);

  void CreateLayout();
  void CreateButtons();
  void CreateGroups();
  void ScaleSvg();
  void PositionGroups();
};

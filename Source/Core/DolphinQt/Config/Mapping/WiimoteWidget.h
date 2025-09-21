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
    enum class ButtonPlacement
    {
      Left,
      Right,
      Top,
      Bottom
    };
    enum class LabelPlacement
    {
      Left,
      Right,
      Top,
      Bottom,
      Auto
    };
    QPushButton* button;
    ButtonPlacement button_placement;
    QLabel* label;
    LabelPlacement label_placement;
    QString svg_element_name;
    int spacing;
    QPoint offset;
    QGraphicsProxyWidget* button_proxy = nullptr;
    QGraphicsProxyWidget* label_proxy = nullptr;
  };

  std::vector<ButtonInfo> m_buttons;

  QGraphicsView* m_view;
  QGraphicsScene* m_scene;
  QGraphicsSvgItem* m_svg_item;
  QSvgRenderer* m_renderer;

  void CreateButton(std::string label, std::string svg_element_name,
                    ButtonInfo::ButtonPlacement button_placement,
                    ButtonInfo::LabelPlacement label_placement = ButtonInfo::LabelPlacement::Auto,
                    int spacing = 0, QPoint offset = QPoint());

  void CreateLayout();
  void CreateButtons();
  void ScaleSvg();
  void PositionButtons();
};

// Copyright 2025 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "WiimoteWidget.h"

#include <QGraphicsEffect>
#include <QGraphicsSvgItem>
#include <QGridLayout>
#include <QHBoxLayout>
#include <QLabel>
#include <QPoint>
#include <QPushButton>
#include <QSize>
#include <QSvgRenderer>

#include <vector>

#include "Common/FileUtil.h"
#include "DolphinQt/QtUtils/NonDefaultQPushButton.h"

WiimoteWidget::WiimoteWidget(QWidget* parent, int port_num) : QWidget(parent)
{
  CreateLayout();
  CreateButtons();
  CreateGroups();
  ScaleSvg();
  PositionGroups();
}

void WiimoteWidget::resizeEvent(QResizeEvent* event)
{
  ScaleSvg();
  PositionGroups();
  QWidget::resizeEvent(event);
}

WiimoteWidget::ButtonInfo WiimoteWidget::CreateButton(std::string label,
                                                      std::string svg_element_name)
{
  QWidget* container = new QWidget();
  QVBoxLayout* layout = new QVBoxLayout(container);
  layout->setContentsMargins(0, 0, 0, 0);
  layout->setSpacing(0);

  QPushButton* button_widget = new NonDefaultQPushButton(QString::fromStdString(svg_element_name));
  QLabel* label_widget = new QLabel(QString::fromStdString(label));
  label_widget->setAlignment(Qt::AlignCenter);

  layout->addWidget(label_widget);
  layout->addWidget(button_widget);

  return ButtonInfo{
      .button = container,
      .svg_element_name = QString::fromStdString(svg_element_name),
  };
}

void WiimoteWidget::CreateButtons()
{
  ButtonGroup power_group;
  power_group.placement = ButtonGroup::Placement::Top;
  power_group.buttons.push_back(CreateButton("Power", "button_power"));
  m_button_groups.push_back(power_group);

  ButtonGroup dpad_group;
  dpad_group.placement = ButtonGroup::Placement::Left;
  dpad_group.buttons.push_back(CreateButton("Up", "dpad_up"));
  dpad_group.buttons.push_back(CreateButton("Left", "dpad_left"));
  dpad_group.buttons.push_back(CreateButton("Right", "dpad_right"));
  dpad_group.buttons.push_back(CreateButton("Down", "dpad_down"));
  m_button_groups.push_back(dpad_group);

  ButtonGroup a_group;
  a_group.placement = ButtonGroup::Placement::Left;
  a_group.buttons.push_back(CreateButton("A", "button_a"));
  m_button_groups.push_back(a_group);

  ButtonGroup menu_group;
  menu_group.placement = ButtonGroup::Placement::Right;
  menu_group.buttons.push_back(CreateButton("+", "button_plus"));
  menu_group.buttons.push_back(CreateButton("HOME", "button_home"));
  menu_group.buttons.push_back(CreateButton("-", "button_minus"));
  m_button_groups.push_back(menu_group);

  ButtonGroup number_group;
  number_group.placement = ButtonGroup::Placement::Bottom;
  number_group.buttons.push_back(CreateButton("1", "button_one"));
  number_group.buttons.push_back(CreateButton("2", "button_two"));
  m_button_groups.push_back(number_group);
}

void WiimoteWidget::CreateGroups()
{
  for (auto& group : m_button_groups)
  {
    QWidget* group_widget = new QWidget();
    QGridLayout* layout = new QGridLayout(group_widget);

    QSet<int> x_values_set;
    QSet<int> y_values_set;
    for (auto& btn : group.buttons)
    {
      if (!m_renderer->elementExists(btn.svg_element_name))
        continue;

      const QRectF elem_bounds = m_renderer->boundsOnElement(btn.svg_element_name);
      const int elem_x = std::round(elem_bounds.x());
      const int elem_y = std::round(elem_bounds.y());
      x_values_set.insert(elem_x);
      y_values_set.insert(elem_y);
    }

    QList<int> x_values = x_values_set.values();
    QList<int> y_values = y_values_set.values();
    std::sort(x_values.begin(), x_values.end());
    std::sort(y_values.begin(), y_values.end());

    for (auto& btn : group.buttons)
    {
      if (!m_renderer->elementExists(btn.svg_element_name))
        continue;

      const QRectF elem_bounds = m_renderer->boundsOnElement(btn.svg_element_name);
      const int elem_x = std::round(elem_bounds.x());
      const int elem_y = std::round(elem_bounds.y());

      int col = x_values.indexOf(elem_x);
      int row = y_values.indexOf(elem_y);

      layout->addWidget(btn.button, row, col);
    }

    group.proxy = m_scene->addWidget(group_widget);
  }
}

void WiimoteWidget::CreateLayout()
{
  auto* layout = new QHBoxLayout(this);

  m_scene = new QGraphicsScene(this);

  m_view = new QGraphicsView(m_scene, this);
  m_view->setRenderHint(QPainter::Antialiasing);
  m_view->setAlignment(Qt::AlignCenter);
  m_view->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);

  const QString svg_path =
      QString::fromStdString(File::GetSysDirectory() + "/Resources/Wiimote-Front.svg");
  m_renderer = new QSvgRenderer(svg_path, this);
  m_svg_item = new QGraphicsSvgItem();
  m_svg_item->setSharedRenderer(m_renderer);
  m_scene->addItem(m_svg_item);
  m_svg_item->setZValue(0);

  // The SVG is assumed to be plain black, make the color white if users are using a dark
  // background
  QColor bg = palette().color(QPalette::Window);
  if (bg.valueF() < 0.5)
  {
    auto* effect = new QGraphicsColorizeEffect();
    effect->setColor(Qt::white);
    effect->setStrength(1.0);
    m_svg_item->setGraphicsEffect(effect);
  }

  layout->addWidget(m_view);
  setLayout(layout);
}

void WiimoteWidget::ScaleSvg()
{
  if (!m_svg_item || !m_renderer)
    return;

  const QSizeF svg_size = m_renderer->defaultSize();
  const QSizeF view_size = m_view->viewport()->size();

  const qreal scale_x = view_size.width() / svg_size.width();
  const qreal scale_y = view_size.height() / svg_size.height();
  const qreal scale = qMin(scale_x, scale_y);

  QTransform transform;
  transform.scale(scale, scale);
  m_svg_item->setTransform(transform);

  m_scene->setSceneRect(0, 0, view_size.width(), view_size.height());

  const qreal x_offset = (view_size.width() - svg_size.width() * scale) / 2;
  const qreal y_offset = (view_size.height() - svg_size.height() * scale) / 2;
  m_svg_item->setPos(x_offset, y_offset);
}

void WiimoteWidget::PositionGroups()
{
  if (!m_svg_item || !m_renderer)
    return;

  for (auto& group : m_button_groups)
  {
    QRectF group_bounds;
    for (auto& btn : group.buttons)
    {
      if (!m_renderer->elementExists(btn.svg_element_name))
        continue;

      QRectF elem_bounds = m_renderer->boundsOnElement(btn.svg_element_name);
      group_bounds = group_bounds.isNull() ? elem_bounds : group_bounds.united(elem_bounds);
    }

    QPointF center_in_svg = group_bounds.center();
    QPointF scene_center = m_svg_item->mapToScene(center_in_svg);
    QRectF proxy_bounds = group.proxy->boundingRect();
    QPointF offset = QPointF(proxy_bounds.width() / 2, proxy_bounds.height() / 2);

    group.proxy->setPos(scene_center - offset);
  }
}

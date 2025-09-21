// Copyright 2025 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "WiimoteWidget.h"

#include <QGraphicsEffect>
#include <QHBoxLayout>
#include <QLabel>
#include <QPushButton>
#include <qgraphicssvgitem.h>
#include <qsvgrenderer.h>

#include "Common/FileUtil.h"
#include "Common/Logging/Log.h"
#include "DolphinQt/QtUtils/NonDefaultQPushButton.h"

WiimoteWidget::WiimoteWidget(QWidget* parent, int port_num) : QWidget(parent)
{
  CreateLayout();
  CreateButtons();
  ScaleSvg();
  PositionButtons();
}

void WiimoteWidget::resizeEvent(QResizeEvent* event)
{
  ScaleSvg();
  PositionButtons();
  QWidget::resizeEvent(event);
}

void WiimoteWidget::CreateButton(std::string label, std::string svg_element_name,
                                 ButtonInfo::ButtonPlacement button_placement,
                                 ButtonInfo::LabelPlacement label_placement, int spacing,
                                 QPoint offset)
{
  QPushButton* button_widget = new NonDefaultQPushButton(QString::fromStdString("Placeholder"));
  QLabel* label_widget = new QLabel(QString::fromStdString(label));
  label_widget->adjustSize();

  auto button_proxy = m_scene->addWidget(button_widget);
  button_proxy->setZValue(1);

  auto label_proxy = m_scene->addWidget(label_widget);
  label_proxy->setParentItem(button_proxy);
  label_proxy->setZValue(2);

  {
    auto effective_label_placement = label_placement;
    if (effective_label_placement == ButtonInfo::LabelPlacement::Auto)
      effective_label_placement = static_cast<ButtonInfo::LabelPlacement>(button_placement);

    const qreal label_gap = 6.0;

    const QSizeF btn_size = button_proxy->size();
    label_widget->adjustSize();
    const QSizeF lbl_size = label_widget->size();

    QPointF local_pos;
    switch (effective_label_placement)
    {
    case ButtonInfo::LabelPlacement::Left:
      local_pos.setX(-lbl_size.width() - label_gap);
      local_pos.setY((btn_size.height() - lbl_size.height()) / 2.0);
      break;
    case ButtonInfo::LabelPlacement::Right:
      local_pos.setX(btn_size.width() + label_gap);
      local_pos.setY((btn_size.height() - lbl_size.height()) / 2.0);
      break;
    case ButtonInfo::LabelPlacement::Top:
      local_pos.setX((btn_size.width() - lbl_size.width()) / 2.0);
      local_pos.setY(-lbl_size.height() - label_gap);
      break;
    case ButtonInfo::LabelPlacement::Bottom:
      local_pos.setX((btn_size.width() - lbl_size.width()) / 2.0);
      local_pos.setY(btn_size.height() + label_gap);
      break;
    default:
      // fallback: place to the right
      local_pos.setX(btn_size.width() + label_gap);
      local_pos.setY((btn_size.height() - lbl_size.height()) / 2.0);
      break;
    }

    label_proxy->setPos(local_pos);
  }

  m_buttons.push_back(ButtonInfo{
      .button = button_widget,
      .button_placement = button_placement,
      .label = label_widget,
      .label_placement = label_placement,
      .svg_element_name = QString::fromStdString(svg_element_name),
      .spacing = spacing,
      .offset = offset,
      .button_proxy = button_proxy,
      .label_proxy = label_proxy,
  });
}

void WiimoteWidget::CreateButtons()
{
  CreateButton("Power", "button_power", ButtonInfo::ButtonPlacement::Left);
  CreateButton("Up", "dpad_up", ButtonInfo::ButtonPlacement::Top, ButtonInfo::LabelPlacement::Auto,
               20);
  CreateButton("Left", "dpad_left", ButtonInfo::ButtonPlacement::Left,
               ButtonInfo::LabelPlacement::Auto, 20);
  CreateButton("Right", "dpad_right", ButtonInfo::ButtonPlacement::Right,
               ButtonInfo::LabelPlacement::Auto, 20);
  CreateButton("Down", "dpad_down", ButtonInfo::ButtonPlacement::Bottom,
               ButtonInfo::LabelPlacement::Left, 20);
  CreateButton("A", "button_a", ButtonInfo::ButtonPlacement::Left);
  CreateButton("+", "button_plus", ButtonInfo::ButtonPlacement::Right);
  CreateButton("Home", "button_home", ButtonInfo::ButtonPlacement::Top);
  CreateButton("-", "button_minus", ButtonInfo::ButtonPlacement::Left);
  CreateButton("1", "button_one", ButtonInfo::ButtonPlacement::Left);
  CreateButton("2", "button_two", ButtonInfo::ButtonPlacement::Left);
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

  // The SVG is assumed to be plain black, make the color white if users are using a dark background
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

void WiimoteWidget::PositionButtons()
{
  if (!m_svg_item || !m_renderer)
    return;

  for (auto& info : m_buttons)
  {
    if (!m_renderer->elementExists(info.svg_element_name))
    {
      ERROR_LOG_FMT(COMMON, "SVG element not found: {}", info.svg_element_name.toStdString());
      continue;
    }

    if (!info.button_proxy)
      continue;

    const QRectF element_bounds = m_renderer->boundsOnElement(info.svg_element_name);
    const QRectF scene_bounds = m_svg_item->mapRectToScene(element_bounds);

    const QSizeF button_size = info.button_proxy->size();

    QPointF scene_pos;
    const qreal gap = info.spacing;
    switch (info.button_placement)
    {
    case ButtonInfo::ButtonPlacement::Left:
      scene_pos.setX(scene_bounds.left() - button_size.width() - gap);
      scene_pos.setY(scene_bounds.top() + (scene_bounds.height() - button_size.height()) / 2.0);
      break;
    case ButtonInfo::ButtonPlacement::Right:
      scene_pos.setX(scene_bounds.right() + gap);
      scene_pos.setY(scene_bounds.top() + (scene_bounds.height() - button_size.height()) / 2.0);
      break;
    case ButtonInfo::ButtonPlacement::Top:
      scene_pos.setX(scene_bounds.left() + (scene_bounds.width() - button_size.width()) / 2.0);
      scene_pos.setY(scene_bounds.top() - button_size.height() - gap);
      break;
    case ButtonInfo::ButtonPlacement::Bottom:
      scene_pos.setX(scene_bounds.left() + (scene_bounds.width() - button_size.width()) / 2.0);
      scene_pos.setY(scene_bounds.bottom() + gap);
      break;
    }

    scene_pos += info.offset;

    info.button_proxy->setPos(scene_pos);
  }
}

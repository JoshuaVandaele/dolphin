// Copyright 2025 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "DolphinQt/Config/Mapping/WiiMappingWindow.h"

#include <QDialog>
#include <QGraphicsEffect>
#include <QGraphicsScene>
#include <QGraphicsSvgItem>
#include <QGraphicsView>
#include <QHBoxLayout>
#include <QLabel>
#include <QPushButton>
#include <QSvgWidget>
#include <QVBoxLayout>
#include <qlabel.h>
#include <qpushbutton.h>

#include "Common/FileUtil.h"
#include "Common/Logging/Log.h"
#include "DolphinQt/QtUtils/NonDefaultQPushButton.h"
#include "DolphinQt/QtUtils/QtUtils.h"
#include "DolphinQt/QtUtils/ScalableSvgWidget.h"

// TODO: Attribute Wiimote SVGs to Maxence Béranger on the Noun Project (CC BY-3.0)

WiiMappingWindow::WiiMappingWindow(QWidget* parent, const int port_num) : QDialog(parent)
{
  setWindowTitle(tr("Wii Remote %1").arg(port_num + 1));

  CreateLayout();
  CreateButtons();
  ConnectWidgets();
  PositionButtons();

  QtUtils::AdjustSizeWithinScreen(this);
}

WiiMappingWindow::ButtonInfo WiiMappingWindow::CreateButton(std::string label,
                                                            std::string svg_element_name,
                                                            ButtonPlacement button_placement,
                                                            LabelPlacement label_placement,
                                                            int spacing, QPoint offset)
{
  QPushButton* button_widget = new NonDefaultQPushButton(QString::fromStdString(""), this);
  QLabel* label_widget = new QLabel(QString::fromStdString(label), this);
  label_widget->adjustSize();

  return ButtonInfo{.button = button_widget,
                    .button_placement = button_placement,
                    .label = label_widget,
                    .label_placement = label_placement,
                    .svg_element_name = QString::fromStdString(svg_element_name),
                    .spacing = spacing,
                    .offset = offset};
}

void WiiMappingWindow::CreateButtons()
{
  m_buttons.push_back(CreateButton("Power", "button_power", ButtonPlacement::Left));
  m_buttons.push_back(
      CreateButton("Up", "dpad_up", ButtonPlacement::Top, LabelPlacement::Auto, 20));
  m_buttons.push_back(
      CreateButton("Left", "dpad_left", ButtonPlacement::Left, LabelPlacement::Auto, 20));
  m_buttons.push_back(
      CreateButton("Right", "dpad_right", ButtonPlacement::Right, LabelPlacement::Auto, 20));
  m_buttons.push_back(
      CreateButton("Down", "dpad_down", ButtonPlacement::Bottom, LabelPlacement::Left, 20));
  m_buttons.push_back(CreateButton("A", "button_a", ButtonPlacement::Left));
  m_buttons.push_back(CreateButton("+", "button_plus", ButtonPlacement::Right));
  m_buttons.push_back(CreateButton("Home", "button_home", ButtonPlacement::Top));
  m_buttons.push_back(CreateButton("-", "button_minus", ButtonPlacement::Left));
  m_buttons.push_back(CreateButton("1", "button_one", ButtonPlacement::Left));
  m_buttons.push_back(CreateButton("2", "button_two", ButtonPlacement::Left));
}

void WiiMappingWindow::CreateLayout()
{
  auto* main_layout = new QHBoxLayout(this);
  main_layout->setAlignment(Qt::AlignCenter);

  const QString svg_path =
      QString::fromStdString(File::GetSysDirectory() + "/Resources/Wiimote-Front.svg");
  m_svg_widget = new ScalableSvgWidget(svg_path, this);
  m_svg_widget->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);

  //   The SVG is assumed to be plain black, make the color white if users are using a dark
  //   background
  QColor bg = palette().color(QPalette::Window);
  if (bg.valueF() < 0.5)
  {
    auto* effect = new QGraphicsColorizeEffect();
    effect->setColor(Qt::white);
    effect->setStrength(1.0);
    m_svg_widget->setGraphicsEffect(effect);
  }
  main_layout->addWidget(m_svg_widget);

  setLayout(main_layout);
}

void WiiMappingWindow::ConnectWidgets()
{
  connect(m_svg_widget, &ScalableSvgWidget::resized, this, &WiiMappingWindow::PositionButtons);
}

void WiiMappingWindow::PositionButtons()
{
  auto svg_renderer = m_svg_widget->getRenderer();
  if (!svg_renderer)
    return;

  const QSize svg_size = svg_renderer->defaultSize();
  const QSize widget_size = m_svg_widget->size();
  const double scale_x = double(widget_size.width()) / svg_size.width();
  const double scale_y = double(widget_size.height()) / svg_size.height();

  for (auto& info : m_buttons)
  {
    if (!svg_renderer->elementExists(info.svg_element_name))
    {
      ERROR_LOG_FMT(COMMON, "Could not locate '{}' in SVG.", info.svg_element_name.toStdString());
      continue;
    }

    const QRectF bounds = svg_renderer->boundsOnElement(info.svg_element_name);
    const QPoint center(std::round(bounds.center().x() * scale_x),
                        std::round(bounds.center().y() * scale_y));
    const QSize size(std::round(bounds.width() * scale_x), std::round(bounds.height() * scale_y));

    QPoint button_pos;
    QPoint label_offset;

    switch (info.button_placement)
    {
    case ButtonPlacement::Left:
      button_pos = center - QPoint(info.button->width() + size.width() / 2 + info.spacing,
                                   info.button->height() / 2);
      break;
    case ButtonPlacement::Right:
      button_pos = center + QPoint(size.width() / 2 + info.spacing, -info.button->height() / 2);
      break;
    case ButtonPlacement::Top:
      button_pos = center - QPoint(info.button->width() / 2,
                                   info.button->height() + size.height() / 2 + info.spacing);
      break;
    case ButtonPlacement::Bottom:
      button_pos = center + QPoint(-info.button->width() / 2, size.height() / 2 + info.spacing);
      break;
    }

    button_pos += m_svg_widget->geometry().topLeft();
    button_pos += info.offset;

    info.button->move(button_pos);

    LabelPlacement label_placement;
    if (info.label_placement == LabelPlacement::Auto)
    {
      switch (info.button_placement)
      {
      case ButtonPlacement::Left:
        label_placement = LabelPlacement::Left;
        break;
      case ButtonPlacement::Right:
        label_placement = LabelPlacement::Right;
        break;
      case ButtonPlacement::Top:
        label_placement = LabelPlacement::Top;
        break;
      case ButtonPlacement::Bottom:
        label_placement = LabelPlacement::Bottom;
        break;
      }
    }
    else
    {
      label_placement = info.label_placement;
    }

    switch (label_placement)
    {
    case LabelPlacement::Left:
      label_offset =
          QPoint(-info.label->width() - 4, (info.button->height() - info.label->height()) / 2);
      break;
    case LabelPlacement::Right:
      label_offset =
          QPoint(info.button->width() + 4, (info.button->height() - info.label->height()) / 2);
      break;
    case LabelPlacement::Top:
      label_offset =
          QPoint((info.button->width() - info.label->width()) / 2, -info.label->height() - 4);
      break;
    case LabelPlacement::Bottom:
      label_offset =
          QPoint((info.button->width() - info.label->width()) / 2, info.button->height() + 4);
      break;
    case LabelPlacement::Auto:
    default:
      break;
    }

    info.label->move(button_pos + label_offset);
  }
}

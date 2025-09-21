// Copyright 2025 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include <DolphinQt/QtUtils/ScalableSvgWidget.h>

#include <QEvent>
#include <QResizeEvent>
#include <QSize>
#include <QString>
#include <QSvgWidget>
#include <QWidget>

ScalableSvgWidget::ScalableSvgWidget(const QString& filePath, QWidget* parent)
    : QSvgWidget(filePath, parent)
{
  m_renderer = renderer();
}

void ScalableSvgWidget::resizeEvent(QResizeEvent* event)
{
  QSize widgetSize = event->size();
  QSize svgSize = this->sizeHint();

  // Calculate scale factor
  double scale = std::min(static_cast<double>(widgetSize.width()) / svgSize.width(),
                          static_cast<double>(widgetSize.height()) / svgSize.height());

  this->resize(svgSize * scale);
  QSvgWidget::resizeEvent(event);
  emit resized();
}

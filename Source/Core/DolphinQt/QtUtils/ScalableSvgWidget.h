// Copyright 2025 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#pragma once

#include <QResizeEvent>
#include <QString>
#include <QSvgRenderer>
#include <QSvgWidget>
#include <QWidget>

class ScalableSvgWidget : public QSvgWidget
{
  Q_OBJECT
public:
  explicit ScalableSvgWidget(const QString& filePath, QWidget* parent = nullptr);

  QSvgRenderer* getRenderer() const { return m_renderer; }

signals:
  void resized();

protected:
  void resizeEvent(QResizeEvent* event) override;

private:
  QSvgRenderer* m_renderer;
};

// Copyright 2026 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include <QWidget>

#include "Common/CommonTypes.h"

template <typename T>
class ConfigChoiceMap;
template <std::integral T>
class ConfigInteger;
class ConfigText;
class QCheckBox;
class QComboBox;
class QGridLayout;
class QLabel;
class QLineEdit;
class QPushButton;
class QRadioButton;
class QSpinBox;
class QTableWidget;

class ConnectTab : public QWidget
{
  Q_OBJECT

public:
  explicit ConnectTab(QWidget* parent = nullptr);

private:
  QTableWidget* m_server_list;

  QGridLayout* m_connection_controls;
  ConfigText* m_nickname_config;
  ConfigChoiceMap<std::string>* m_connection_type_config;
  QLabel* m_ip_label;
  ConfigText* m_ip_config;
  QLabel* m_port_label;
  ConfigInteger<u16>* m_port_config;
  QLabel* m_hostcode_label;
  ConfigText* m_hostcode_config;
  QPushButton* m_filter_toggle;
  QPushButton* m_refresh_button;
  QPushButton* m_connect_button;

  QWidget* m_filters_widget;
  QGridLayout* m_filters;
  QComboBox* m_filter_region;
  QCheckBox* m_filter_hide_incompat;
  QLineEdit* m_filter_game_id;
  QCheckBox* m_filter_hide_ingame;
  QLineEdit* m_filter_name;
  QSpinBox* m_filter_ping;
  QRadioButton* m_filter_private_public;
  QRadioButton* m_filter_public;
  QRadioButton* m_filter_private;
};

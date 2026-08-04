// Copyright 2026 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "DolphinQt/NetPlay/ConnectTab.h"

#include <QButtonGroup>
#include <QCheckBox>
#include <QComboBox>
#include <QGridLayout>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QLabel>
#include <QLineEdit>
#include <QPushButton>
#include <QRadioButton>
#include <QSpinBox>
#include <QSplitter>
#include <QTableWidget>
#include <QVBoxLayout>
#include <QWidget>

#include "Core/Config/NetplaySettings.h"
#include "DolphinQt/Config/ConfigControls/ConfigChoice.h"
#include "DolphinQt/Config/ConfigControls/ConfigInteger.h"
#include "DolphinQt/Config/ConfigControls/ConfigText.h"
#include "UICommon/NetPlayIndex.h"

ConnectTab::ConnectTab(QWidget* parent) : QWidget(parent)
{
  QVBoxLayout* main_layout = new QVBoxLayout(this);

  m_server_list = new QTableWidget(0, 8, this);
  m_server_list->setHorizontalHeaderLabels({QStringLiteral("🔒"), QStringLiteral("🎮"),
                                            tr("Region"), tr("Name"), tr("Game"), tr("Players"),
                                            tr("Ping"), tr("Version")});
  m_server_list->verticalHeader()->setVisible(false);
  m_server_list->setSelectionBehavior(QAbstractItemView::SelectRows);
  m_server_list->setSelectionMode(QAbstractItemView::SingleSelection);
  m_server_list->setEditTriggers(QAbstractItemView::NoEditTriggers);
  m_server_list->horizontalHeader()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
  m_server_list->horizontalHeader()->setSectionResizeMode(1, QHeaderView::ResizeToContents);
  m_server_list->horizontalHeader()->setSectionResizeMode(2, QHeaderView::ResizeToContents);
  m_server_list->horizontalHeader()->setSectionResizeMode(3, QHeaderView::Stretch);
  m_server_list->horizontalHeader()->setSectionResizeMode(4, QHeaderView::Stretch);
  m_server_list->horizontalHeader()->setSectionResizeMode(5, QHeaderView::ResizeToContents);
  m_server_list->horizontalHeader()->setSectionResizeMode(6, QHeaderView::ResizeToContents);
  m_server_list->horizontalHeader()->setSectionResizeMode(7, QHeaderView::ResizeToContents);

  //   connect(m_server_list, &QTableWidget::cellDoubleClicked, this, &OnCellDoubleClicked);
  //   connect(m_server_list, &QTableWidget::itemSelectionChanged, this, &OnSelectionChanged);

  main_layout->addWidget(m_server_list);

  m_connection_controls = new QGridLayout;

  QLabel* nickname_label = new QLabel(tr("Nickname:"));
  nickname_label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
  m_nickname_config = new ConfigText(Config::NETPLAY_NICKNAME);
  QLabel* connection_type_label = new QLabel(tr("Connection Type:"));
  connection_type_label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
  m_connection_type_config = new ConfigChoiceMap<std::string>(
      {{tr("Direct Connection"), "direct"},
       {tr("Traversal Server"), "traversal"},
       {tr("Selected Server"), "server"}},  // TODO: auto-select that when someone clicks a server
      Config::NETPLAY_TRAVERSAL_CHOICE);

  m_ip_label = new QLabel(tr("IP Address:"));
  m_ip_label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
  m_ip_config = new ConfigText(Config::NETPLAY_ADDRESS);
  m_hostcode_label = new QLabel(tr("Host Code:"));
  m_hostcode_label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
  m_hostcode_config = new ConfigText(Config::NETPLAY_HOST_CODE);
  m_port_label = new QLabel(tr("Port:"));
  m_port_label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
  m_port_config = new ConfigInteger<u16>(Config::NETPLAY_HOST_PORT);

  m_filter_toggle = new QPushButton(tr("Filters"));
  m_refresh_button = new QPushButton(tr("Refresh"));
  m_connect_button = new QPushButton(tr("Connect"));

  m_connection_controls->addWidget(nickname_label, 0, 0);
  m_connection_controls->addWidget(m_nickname_config, 0, 1);
  m_connection_controls->addWidget(connection_type_label, 0, 2);
  m_connection_controls->addWidget(m_connection_type_config, 0, 3, 1, 2);

  m_connection_controls->addWidget(m_ip_label, 1, 0);
  m_connection_controls->addWidget(m_hostcode_label, 1, 0);
  m_connection_controls->addWidget(m_ip_config, 1, 1);
  m_connection_controls->addWidget(m_hostcode_config, 1, 1);
  m_connection_controls->addWidget(m_port_label, 1, 2);
  m_connection_controls->addWidget(m_port_config, 1, 3, 1, 2);

  m_connection_controls->addWidget(m_filter_toggle, 2, 0);
  m_connection_controls->addWidget(m_refresh_button, 2, 3);
  m_connection_controls->addWidget(m_connect_button, 2, 4);

  main_layout->addLayout(m_connection_controls);

  m_filters_widget = new QWidget(this);
  m_filters = new QGridLayout(m_filters_widget);

  QFrame* filter_separator = new QFrame;
  filter_separator->setFrameShape(QFrame::HLine);
  filter_separator->setFrameShadow(QFrame::Sunken);

  m_filters->addWidget(filter_separator, 0, 0, 1, 4);

  QLabel* region_label = new QLabel(tr("Region:"));
  region_label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
  m_filter_region = new QComboBox;
  m_filter_region->addItem(tr("Any Region"));
  for (const auto& region : NetPlayIndex::GetRegions())
  {
    m_filter_region->addItem(
        tr("%1 (%2)").arg(tr(region.second.c_str())).arg(QString::fromStdString(region.first)),
        QString::fromStdString(region.first));
  }
  m_filter_hide_incompat = new QCheckBox(tr("Hide Incompatible Sessions"));

  QLabel* gameid_label = new QLabel(tr("Game ID:"));
  gameid_label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
  m_filter_game_id = new QLineEdit;
  m_filter_game_id->setPlaceholderText(QStringLiteral("RMCP01"));
  m_filter_hide_ingame = new QCheckBox(tr("Hide In-Game Sessions"));

  QLabel* name_label = new QLabel(tr("Name:"));
  name_label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
  m_filter_name = new QLineEdit;
  QLabel* ping_label = new QLabel(tr("Ping Limit:"));
  ping_label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
  m_filter_ping = new QSpinBox;
  m_filter_ping->setSuffix(QStringLiteral("ms"));

  QWidget* visibility_radios = new QWidget(this);
  QHBoxLayout* visibility_layout = new QHBoxLayout(visibility_radios);
  QButtonGroup* visibility_group = new QButtonGroup(this);
  m_filter_private_public = new QRadioButton(tr("Private and Public"));
  m_filter_public = new QRadioButton(tr("Public"));
  m_filter_private = new QRadioButton(tr("Private"));
  visibility_group->addButton(m_filter_private_public);
  visibility_group->addButton(m_filter_public);
  visibility_group->addButton(m_filter_private);
  visibility_layout->addWidget(m_filter_private_public);
  visibility_layout->addWidget(m_filter_public);
  visibility_layout->addWidget(m_filter_private);

  m_filters->addWidget(region_label, 1, 0);
  m_filters->addWidget(m_filter_region, 1, 1);
  m_filters->addWidget(m_filter_hide_incompat, 1, 2, 1, 2);

  m_filters->addWidget(gameid_label, 2, 0);
  m_filters->addWidget(m_filter_game_id, 2, 1);
  m_filters->addWidget(m_filter_hide_ingame, 2, 2, 1, 2);

  m_filters->addWidget(name_label, 3, 0);
  m_filters->addWidget(m_filter_name, 3, 1);
  m_filters->addWidget(ping_label, 3, 2);
  m_filters->addWidget(m_filter_ping, 3, 3);

  m_filters->addWidget(visibility_radios, 4, 1, 1, 4);

  main_layout->addWidget(m_filters_widget);
  m_filters_widget->setVisible(false);

  setLayout(main_layout);

  // TODO: Get all this from Qt Settings and set the appropriate default values
  // see NetPlayBrowser::SaveSettings & NetPlayBrowser::RestoreSettings

  m_filter_hide_ingame->setChecked(true);

  connect(m_filter_toggle, &QPushButton::clicked, this,
          [this]() { m_filters_widget->setVisible(!m_filters_widget->isVisible()); });

  m_hostcode_label->setVisible(false);
  m_hostcode_config->setVisible(false);

  auto update_connection_type = [this]() {
    const bool traversal = Config::Get(Config::NETPLAY_TRAVERSAL_CHOICE) == "traversal";
    const bool server = Config::Get(Config::NETPLAY_TRAVERSAL_CHOICE) == "server";

    m_ip_label->setVisible(!traversal && !server);
    m_ip_config->setVisible(!traversal && !server);
    m_port_label->setVisible(!traversal && !server);
    m_port_config->setVisible(!traversal && !server);

    m_hostcode_label->setVisible(traversal && !server);
    m_hostcode_config->setVisible(traversal && !server);
  };

  connect(m_connection_type_config, &QComboBox::currentIndexChanged, this,
          [update_connection_type]() { update_connection_type(); });

  update_connection_type();

  m_filter_private_public->setChecked(true);
}

/*
+-----------------------------------------------------------+
| 🔒 | 🎮 | Region | Name | Game | Players | Ping | Version |
| Ye | No | EU     | Josh | SMG2 |    4    | 50ms | 1234-56 |
|                             ...                           |
+-----------------------------------------------------------+
| Nickname:   [___________]     Connection Type: [Direct v] |
| IP Address: [__________________________]  Port: [_______] |
| [Filters]                             [Refresh] [Connect] |
+-----------------------------------------------------------+

If filters are extended:

+-----------------------------------------------------------+
| 🔒 | 🎮 | Region | Name | Game | Players | Ping | Version |
| Ye | No | EU     | Josh | SMG2 |    4    | 50ms | 1234-56 |
|                             ...                           |
+-----------------------------------------------------------+
| Nickname: [___________]       Connection Type: [Direct v] |
| IP Address: _____________________________   Port: _______ |
| [Filters]                             [Refresh] [Connect] |
| - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
| Region: [Any Region v]     🔳 Hide Incompatible Session   |
| Game ID: [______________]  ☑️ Hide In-Game Sessions       |
| Name: [_________________]  Ping Limit: [500ms +/-]        |
|     (x) Private and Public   (x) Public   (x) Private     |
+-----------------------------------------------------------+
*/

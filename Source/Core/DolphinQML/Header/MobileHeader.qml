// Copyright 2026 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import DolphinEmu

ToolBar {
    Menu {
        id: mobileMenu
        x: parent.width - width
        y: parent.height
        FileMenu {}
        EmulationMenu {}
        MovieMenu {}
        OptionsMenu {}
        ToolsMenu {}
        ViewMenu {}
        HelpMenu {}
    }
    RowLayout {
        anchors.fill: parent

        Column {
            leftPadding: 12
            Layout.fillWidth: true
            Layout.alignment: Qt.AlignVCenter
            spacing: 0

            Label {
                text: AppInfo.EmulatorName
            }
            Label {
                text: AppInfo.ScmDescStr
                font.pixelSize: 10
                font.bold: true
            }
        }
        Item {
            Layout.fillWidth: true
        }
        ToolButton {
            icon.source: DResource.themeIcon("config")
        }
        ToolButton {
            text: "⋮"
            font.pixelSize: 22
            onClicked: mobileMenu.open()
        }
    }
}

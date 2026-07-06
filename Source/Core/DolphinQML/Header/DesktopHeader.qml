// Copyright 2026 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import DolphinEmu

ToolBar {
    id: root

    contentItem: ColumnLayout {
        spacing: 0

        Flickable {
            id: flick
            Layout.fillWidth: true
            Layout.preferredHeight: row.implicitHeight
            boundsBehavior: Flickable.StopAtBounds
            contentWidth: row.implicitWidth

            ScrollBar.horizontal: hBar

            RowLayout {
                id: row

                DesktopToolButton {
                    text: qsTr("Open")
                    icon.source: DResource.themeIcon("open")
                }
                DesktopToolButton {
                    text: qsTr("Refresh")
                    icon.source: DResource.themeIcon("refresh")
                }
                ToolSeparator {}
                DesktopToolButton {
                    text: qsTr("Play")
                    icon.source: DResource.themeIcon("play")
                }
                DesktopToolButton {
                    text: qsTr("Stop")
                    icon.source: DResource.themeIcon("stop")
                }
                DesktopToolButton {
                    text: qsTr("FullScr")
                    icon.source: DResource.themeIcon("fullscreen")
                }
                DesktopToolButton {
                    text: qsTr("ScrShot")
                    icon.source: DResource.themeIcon("screenshot")
                }
                ToolSeparator {}
                DesktopToolButton {
                    text: qsTr("Config")
                    icon.source: DResource.themeIcon("config")
                }
                DesktopToolButton {
                    text: qsTr("Graphics")
                    icon.source: DResource.themeIcon("graphics")
                }
                DesktopToolButton {
                    text: qsTr("Controllers")
                    icon.source: DResource.themeIcon("classic")
                }
            }
        }

        ScrollBar {
            id: hBar
            orientation: Qt.Horizontal
            Layout.fillWidth: true
            policy: ScrollBar.AsNeeded
        }
    }
}

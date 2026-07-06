// Copyright 2026 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

import QtCore
import QtQuick
import QtQuick.Controls
import QtQuick.Dialogs
import DolphinEmu

Item {
    id: root
    property bool refreshing: false

    Label {
        anchors.centerIn: parent
        width: parent.width * 0.9
        horizontalAlignment: Text.AlignHCenter
        wrapMode: Text.WordWrap

        text: root.refreshing ? qsTr("Refreshing...") : qsTr("Dolphin could not find any GameCube/Wii ISOs or WADs.\nDouble-click here to set a games directory...")
    }

    FolderDialog {
        id: gamePickDialog
        title: qsTr("Select a Directory")
        currentFolder: StandardPaths.standardLocations(StandardPaths.HomeLocation)[0]
        onAccepted: {}
        onVisibleChanged: {
            DHotkeyDisabler.setEnabled(!visible);
        }
    }

    MouseArea {
        anchors.fill: parent
        enabled: !root.refreshing
        onDoubleClicked: gamePickDialog.open()
    }
}

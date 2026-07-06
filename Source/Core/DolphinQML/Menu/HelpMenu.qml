// Copyright 2026 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

import QtQuick
import QtQuick.Controls

import DolphinEmu

Menu {
    title: qsTr("Help")
    Action {
        text: qsTr("Website")
        onTriggered: Qt.openUrlExternally("https://dolphin-emu.org/")
    }
    Action {
        text: qsTr("Online Documentation")
        onTriggered: Qt.openUrlExternally("https://wiki.dolphin-emu.org/")
    }
    Action {
        text: qsTr("Git Repository")
        onTriggered: Qt.openUrlExternally("https://github.com/dolphin-emu/dolphin")
    }
    Action {
        text: qsTr("Bug Tracker")
        onTriggered: Qt.openUrlExternally("https://bugs.dolphin-emu.org/")
    }
    MenuSeparator {}
    AboutDialog {
        id: aboutDialog
    }
    Action {
        text: qsTr("About")
        onTriggered: aboutDialog.open()
    }
}

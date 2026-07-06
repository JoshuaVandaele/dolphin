// Copyright 2026 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

import QtQuick
import QtQuick.Controls
import QtQuick.Dialogs
import QtCore

import DolphinEmu

FileDialog {
    id: root
    Settings {
        id: settings
        category: "mainwindow"
        property string lastDir: ""
    }
    title: qsTr("Select a File")
    currentFolder: settings.lastDir
    nameFilters: [qsTr("All GC/Wii files (*.elf *.dol *.gcm *.bin *.iso *.tgc *.wbfs *.ciso *.gcz *.wia *.rvz *.nfs *.wad *.dff *.m3u *.json)"), qsTr("All Files (*)")]
    onAccepted: {
        settings.lastDir = root.currentFolder;
    }
    onVisibleChanged: {
        DHotkeyDisabler.setEnabled(!visible);
    }
}

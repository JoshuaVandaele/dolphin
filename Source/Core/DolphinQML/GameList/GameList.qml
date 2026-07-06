// Copyright 2026 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

import QtQuick
import QtQuick.Layouts

Item {
    id: root
    property string currentView: "empty" // "list", "grid", "empty"
    property bool refreshing: false

    StackLayout {
        anchors.fill: parent
        // currentIndex: root.currentView === "list" ? 0 : root.currentView === "grid" ? 1 : 2
        currentIndex: 0

        GameEmpty {
            id: gameEmpty
            refreshing: root.refreshing
        }
    }
}

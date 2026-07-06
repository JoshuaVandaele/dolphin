// Copyright 2026 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

pragma Singleton

import QtQuick

QtObject {
    // TODO: Load the actual user preference
    property string iconTheme: "Clean"

    function themeIcon(name) {
        return Qt.resolvedUrl("qrc:/qt/qml/DolphinEmu/Data/Sys/Themes/" + iconTheme + "/" + name);
    }

    function icon(name) {
        return Qt.resolvedUrl("qrc:/qt/qml/DolphinEmu/Data/Sys/Resources/" + name);
    }
}

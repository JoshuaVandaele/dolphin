// Copyright 2026 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

pragma Singleton

import QtQuick

QtObject {
    property Window window: null

    readonly property real shortestSide: Math.min(window.width, window.height)

    property bool tvMode: false // TODO: Some button/switch/config for this

    readonly property bool compact: shortestSide < 500 && !tvMode
    readonly property bool regular: !compact && !tvMode
    readonly property bool big: tvMode
}

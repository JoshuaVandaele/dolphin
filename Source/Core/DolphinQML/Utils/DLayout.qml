// Copyright 2026 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

pragma Singleton

import QtQuick

QtObject {
    property Window window: null

    readonly property bool compact: window.width < 400
    readonly property bool regular: !compact && window.width < 1200
    readonly property bool big: !compact && !regular
}

// Copyright 2026 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

import QtQuick
import QtQuick.Controls

Menu {
    title: qsTr("Movie")
    Action {
        text: qsTr("Start Recording Input")
    }
    Action {
        text: qsTr("Play Input Recording...")
    }
    Action {
        text: qsTr("Stop Playing/Recording Input")
    }
    Action {
        text: qsTr("Export Recording...")
    }
    Action {
        text: qsTr("Read-Only Mode")
        checkable: true
    }
    Action {
        text: qsTr("TAS Input")
    }
    MenuSeparator {}

    Action {
        text: qsTr("Pause at End of Movie")
        checkable: true
    }
    Action {
        text: qsTr("Enable Movie Window")
        checkable: true
    }
    Action {
        text: qsTr("Customize Movie Window")
    }
    MenuSeparator {}
    Action {
        text: qsTr("Dump Frames")
        checkable: true
    }
    Action {
        text: qsTr("Dump Audio")
        checkable: true
    }
}

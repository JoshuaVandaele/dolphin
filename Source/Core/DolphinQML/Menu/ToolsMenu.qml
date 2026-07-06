// Copyright 2026 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

import QtQuick
import QtQuick.Controls

Menu {
    title: qsTr("Tools")

    Action {
        text: qsTr("Resource Pack Manager")
    }
    Action {
        text: qsTr("Cheats Manager")
    }
    Action {
        text: qsTr("FIFO Player")
    }
    Menu {
        title: qsTr("Emulated USB Devices")
        Action {
            text: qsTr("Skylanders Portal")
        }
        Action {
            text: qsTr("Infinity Base")
        }
        Action {
            text: qsTr("Wii Speak")
        }
        Action {
            text: qsTr("Logitech USB Microphone")
        }
    }
    MenuSeparator {}
    Action {
        text: qsTr("Start NetPlay...")
    }
    Action {
        text: qsTr("Browser NetPlay Sessions...")
    }
    MenuSeparator {}
    Action {
        text: qsTr("Achievements")
    }
    MenuSeparator {}
    Menu {
        title: qsTr("Load GameCube Main Menu")
        Action {
            text: qsTr("NTSC-J")
        }
        Action {
            text: qsTr("NTSC-U")
        }
        Action {
            text: qsTr("PAL")
        }
        Action {
            text: qsTr("Triforce")
        }
    }
    Action {
        text: qsTr("Memory Card Manager")
    }
    MenuSeparator {}
    Action {
        // TODO: Show Wii/vWii and the system menu version, e.g. "Load vWii System Menu 5.2.0E"
        text: qsTr("Load Wii System Menu")
    }
    Action {
        text: qsTr("Install WAD...")
    }
    Menu {
        title: qsTr("Manage NAND")
        Action {
            text: qsTr("Import BootMii NAND Backup...")
        }
        Action {
            text: qsTr("Check NAND...")
        }
        Action {
            text: qsTr("Extract Certificates from NAND")
        }
    }
    Menu {
        title: qsTr("Perform Online System Update")
        Action {
            text: qsTr("Current Region")
        }
        MenuSeparator {}
        Action {
            text: qsTr("Europe")
        }
        Action {
            text: qsTr("Japan")
        }
        Action {
            text: qsTr("Korea")
        }
        Action {
            text: qsTr("United States")
        }
    }
    MenuSeparator {}
    Action {
        text: qsTr("Import Wii Save...")
    }
    Action {
        text: qsTr("Import Wii Saves...")
    }
    Action {
        text: qsTr("Export All Wii Saves")
    }
    MenuSeparator {}
    Menu {
        title: qsTr("Connect Wii Remotes")
        Action {
            text: qsTr("Connect Wii Remote 1")
            checkable: true
        }
        Action {
            text: qsTr("Connect Wii Remote 2")
            checkable: true
        }
        Action {
            text: qsTr("Connect Wii Remote 3")
            checkable: true
        }
        Action {
            text: qsTr("Connect Wii Remote 4")
            checkable: true
        }
        MenuSeparator {}
        Action {
            text: qsTr("Connect Wii Balance Board")
            checkable: true
        }
    }
}

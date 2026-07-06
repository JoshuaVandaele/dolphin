import QtQuick
import QtQuick.Controls

Menu {
    title: qsTr("View")

    Action {
        text: qsTr("Show Log")
        checkable: true
    }
    Action {
        text: qsTr("Show Log Configuration")
        checkable: true
    }
    Action {
        text: qsTr("Show Toolbar")
        checkable: true
    }
    Action {
        text: qsTr("Lock Widgets in Place")
        checkable: true
    }
    MenuSeparator {}
    ActionGroup {
        id: viewModeGroup
    }
    Action {
        text: qsTr("List View")
        checkable: true
        checked: true
        ActionGroup.group: viewModeGroup
    }
    Action {
        text: qsTr("Grid View")
        checkable: true
        ActionGroup.group: viewModeGroup
    }
    MenuSeparator {}
    Menu {
        title: qsTr("List Columns")
        Action {
            text: "Platform"
            checkable: true
        }
        Action {
            text: "Banner"
            checkable: true
        }
        Action {
            text: "Title"
            checkable: true
        }
        Action {
            text: "Description"
            checkable: true
        }
        Action {
            text: "Maker"
            checkable: true
        }
        Action {
            text: "File Name"
            checkable: true
        }
        Action {
            text: "File Path"
            checkable: true
        }
        Action {
            text: "Game ID"
            checkable: true
        }
        Action {
            text: "Region"
            checkable: true
        }
        Action {
            text: "File Size"
            checkable: true
        }
        Action {
            text: "File Format"
            checkable: true
        }
        Action {
            text: "Block Size"
            checkable: true
        }
        Action {
            text: "Compression"
            checkable: true
        }
        Action {
            text: "Time Played"
            checkable: true
        }
        Action {
            text: "Tags"
            checkable: true
        }
    }
    MenuSeparator {}
    Menu {
        title: qsTr("Show Platforms")
        Action {
            text: "Show Wii"
            checkable: true
        }
        Action {
            text: "Show GameCube"
            checkable: true
        }
        Action {
            text: "Show Triforce"
            checkable: true
        }
        Action {
            text: "Show WAD"
            checkable: true
        }
        Action {
            text: "Show ELF/DOL"
            checkable: true
        }
    }
    Menu {
        title: qsTr("Show Regions")
        Action {
            text: "Show JPN"
            checkable: true
        }
        Action {
            text: "Show PAL"
            checkable: true
        }
        Action {
            text: "Show USA"
            checkable: true
        }
        Action {
            text: "Show Australia"
            checkable: true
        }
        Action {
            text: "Show France"
            checkable: true
        }
        Action {
            text: "Show Germany"
            checkable: true
        }
        Action {
            text: "Show Italy"
            checkable: true
        }
        Action {
            text: "Show Korea"
            checkable: true
        }
        Action {
            text: "Show Netherlands"
            checkable: true
        }
        Action {
            text: "Show Russia"
            checkable: true
        }
        Action {
            text: "Show Spain"
            checkable: true
        }
        Action {
            text: "Show Taiwan"
            checkable: true
        }
        Action {
            text: "Show World"
            checkable: true
        }
        Action {
            text: "Show Unknown"
            checkable: true
        }
    }
    MenuSeparator {}
    Action {
        text: qsTr("Show Game Count")
        checkable: true
    }
    MenuSeparator {}
    Action {
        text: qsTr("Purge Game List Cache")
    }
    MenuSeparator {}
    Action {
        text: qsTr("Search")
        shortcut: StandardKey.Find
    }
}

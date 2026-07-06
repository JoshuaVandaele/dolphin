import QtQuick
import QtQuick.Controls

Menu {
    title: qsTr("Options")
    Action {
        text: qsTr("Configuration")
        shortcut: StandardKey.Preferences
    }
    MenuSeparator {}
    Action {
        text: qsTr("Graphics Settings")
    }
    Action {
        text: qsTr("Audio Settings")
    }
    Action {
        text: qsTr("Controller Settings")
    }
    Action {
        text: qsTr("Hotkey Settings")
    }
    Action {
        text: qsTr("Free Look Settings")
    }
}

import QtQuick
import QtQuick.Controls
import QtQuick.Dialogs
import QtCore

import DolphinEmu

Menu {
    title: qsTr("File")

    GameFileDialog {
        id: openDialog
        Connections {
            target: openDialog
            function onAccepted() {
                MenuHelper.Open(openDialog.selectedFile);
            }
        }
    }
    Action {
        text: qsTr("Open...")
        shortcut: StandardKey.Open
        onTriggered: openDialog.open()
    }
    MenuSeparator {}
    GameFileDialog {
        id: changeDiscDialog
        Connections {
            target: changeDiscDialog
            function onAccepted() {
                MenuHelper.ChangeDisc(changeDiscDialog.selectedFile);
            }
        }
    }
    Action {
        text: qsTr("Change Disc...")
        onTriggered: changeDiscDialog.open()
    }
    Action {
        text: qsTr("Eject Disc")
        onTriggered: MenuHelper.EjectDisc()
    }
    MenuSeparator {}
    MenuItem {
        text: qsTr("Open User Folder")
        onTriggered: Qt.openUrlExternally(MenuHelper.GetUserFolder())
    }
    MenuItem {
        visible: MenuHelper.ShouldShowConfigFolder()
        text: qsTr("Open Config Folder")
        onTriggered: Qt.openUrlExternally(MenuHelper.GetConfigFolder())
    }
    MenuItem {
        visible: MenuHelper.ShouldShowCacheFolder()
        text: qsTr("Open Cache Folder")
        onTriggered: Qt.openUrlExternally(MenuHelper.GetCacheFolder())
    }
    MenuSeparator {}
    Action {
        text: qsTr("Exit")
        onTriggered: Qt.quit()
        shortcut: StandardKey.Quit
    }
}

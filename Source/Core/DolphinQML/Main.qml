pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Controls
import DolphinEmu

ApplicationWindow {
    id: mainWindow
    visible: true
    title: AppInfo.ScmRevStr

    Component.onCompleted: QMLUtils.SetWindowIcon(mainWindow, DResource.icon("dolphin_logo"))

    menuBar: MenuBar {
        visible: DLayout.regular
        FileMenu {}
        EmulationMenu {}
        MovieMenu {}
        OptionsMenu {}
        ToolsMenu {}
        ViewMenu {}
        HelpMenu {}
    }

    header: Loader {
        source: DLayout.compact ? "Header/MobileHeader.qml" : DLayout.regular ? "Header/DesktopHeader.qml" : ""
    }

    GameList {
        id: gameList
        anchors.fill: parent
    }

    Binding {
        target: DLayout
        property: "window"
        value: mainWindow
    }
}

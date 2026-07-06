// Copyright 2026 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import DolphinEmu

Dialog {
    id: root

    title: qsTr("About Dolphin")
    standardButtons: Dialog.NoButton

    property bool isVertical: height > width

    contentItem: ColumnLayout {
        spacing: 16

        GridLayout {
            id: header
            Layout.alignment: root.isVertical ? Qt.AlignHCenter : Qt.AlignLeft
            columns: root.isVertical ? 1 : 2
            rowSpacing: 12
            columnSpacing: 30

            Image {
                source: DResource.icon("dolphin_logo")
                sourceSize: root.isVertical ? Qt.size(120, 120) : Qt.size(200, 200)
                Layout.alignment: Qt.AlignHCenter
                Layout.margins: root.isVertical ? 8 : 16
            }

            ColumnLayout {
                spacing: 6
                Layout.fillWidth: true
                // Only cap the width in the wide layout; in narrow mode let it
                // fill the column (minus a little breathing room).
                Layout.maximumWidth: root.isVertical ? root.width - 32 : -1

                Label {
                    text: AppInfo.EmulatorName
                    font.pointSize: root.isVertical ? 26 : 38
                    font.weight: 400
                    Layout.fillWidth: true
                    wrapMode: Text.Wrap
                    horizontalAlignment: root.isVertical ? Text.AlignHCenter : Text.AlignLeft
                }

                Label {
                    text: AppInfo.ScmDescStr
                    font.pointSize: root.isVertical ? 12 : 18
                    Layout.fillWidth: true
                    wrapMode: Text.Wrap
                    horizontalAlignment: root.isVertical ? Text.AlignHCenter : Text.AlignLeft
                }

                ColumnLayout {
                    spacing: 0
                    Layout.topMargin: 4
                    Layout.alignment: root.isVertical ? Qt.AlignHCenter : Qt.AlignLeft

                    Label {
                        font.pointSize: 9
                        Layout.fillWidth: true
                        wrapMode: Text.Wrap
                        horizontalAlignment: root.isVertical ? Text.AlignHCenter : Text.AlignLeft
                        // i18n: "Branch" means the version control term, not a literal tree branch.
                        text: qsTr("Branch: %1").arg(AppInfo.ScmCommitsAheadMaster > 0 ?
                        // i18n: A positive number of version control commits made compared to some named branch
                        qsTr("%1 (%2 commit(s) ahead of %3)").arg(AppInfo.ScmBranchStr).arg(AppInfo.ScmCommitsAheadMaster).arg("master") : AppInfo.ScmBranchStr)
                    }
                    Label {
                        font.pointSize: 9
                        Layout.fillWidth: true
                        wrapMode: Text.Wrap
                        horizontalAlignment: root.isVertical ? Text.AlignHCenter : Text.AlignLeft
                        text: qsTr("Revision: %1").arg(AppInfo.ScmRevGitStr)
                    }
                }

                Label {
                    font.pointSize: 9
                    Layout.fillWidth: true
                    wrapMode: Text.Wrap
                    horizontalAlignment: root.isVertical ? Text.AlignHCenter : Text.AlignLeft
                    text: qsTr("Using Qt %1").arg(AppInfo.QtVersion)
                }

                Label {
                    Layout.fillWidth: true
                    wrapMode: Text.Wrap
                    horizontalAlignment: root.isVertical ? Text.AlignHCenter : Text.AlignLeft
                    text: qsTr("Check for updates: <a href='https://dolphin-emu.org/download'>dolphin-emu.org/download</a>")
                    onLinkActivated: link => Qt.openUrlExternally(link)
                }

                Label {
                    Layout.fillWidth: true
                    wrapMode: Text.Wrap
                    horizontalAlignment: root.isVertical ? Text.AlignHCenter : Text.AlignLeft
                    // i18n: The word "free" in the standard phrase "free and open source" is
                    // "free" as in "freedom" - it refers to certain properties of the software's
                    // license, not the software's price.
                    text: qsTr("Dolphin is a free and open-source GameCube and Wii emulator.")
                }

                Label {
                    Layout.fillWidth: true
                    wrapMode: Text.Wrap
                    horizontalAlignment: root.isVertical ? Text.AlignHCenter : Text.AlignLeft
                    text: qsTr("This software should not be used to play games you do not legally own.")
                }

                Row {
                    Layout.alignment: root.isVertical ? Qt.AlignHCenter : Qt.AlignLeft
                    spacing: 6

                    Label {
                        textFormat: Text.RichText
                        text: "<a href='https://github.com/dolphin-emu/dolphin/blob/master/COPYING'>%1</a>".arg(qsTr("License"))
                        onLinkActivated: link => Qt.openUrlExternally(link)
                    }
                    Label {
                        text: "|"
                    }
                    Label {
                        textFormat: Text.RichText
                        text: "<a href='https://github.com/dolphin-emu/dolphin/graphs/contributors'>%1</a>".arg(qsTr("Authors"))
                        onLinkActivated: link => Qt.openUrlExternally(link)
                    }
                    Label {
                        text: "|"
                    }
                    Label {
                        textFormat: Text.RichText
                        text: "<a href='https://forums.dolphin-emu.org/'>%1</a>".arg(qsTr("Support"))
                        onLinkActivated: link => Qt.openUrlExternally(link)
                    }
                }
            }
        }

        Label {
            Layout.fillWidth: true
            Layout.alignment: Qt.AlignHCenter
            wrapMode: Text.Wrap
            horizontalAlignment: Text.AlignHCenter
            // i18n: This message uses curly quotes in English. If you want to use curly quotes
            // in your translation, please use the type appropriate for your language.
            text: qsTr("\u00A9 2003-2026+ Dolphin Team. \u201cGameCube\u201d and \u201cWii\u201d are trademarks of Nintendo. Dolphin is not affiliated with Nintendo in any way.")
        }
    }
}

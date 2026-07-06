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

    contentItem: ColumnLayout {
        spacing: 16

        RowLayout {
            spacing: 30
            Layout.alignment: Qt.AlignLeft

            Image {
                source: DResource.icon("dolphin_logo")
                sourceSize: Qt.size(200, 200)
            }

            ColumnLayout {
                spacing: 6
                Layout.maximumWidth: 360

                Label {
                    text: AppInfo.EmulatorName
                    font.pointSize: 38
                    font.weight: 400
                }

                Label {
                    text: AppInfo.ScmDescStr
                    font.pointSize: 18
                }

                ColumnLayout {
                    spacing: 0
                    Layout.topMargin: 4

                    Label {
                        font.pointSize: 9
                        // i18n: "Branch" means the version control term, not a literal tree branch.
                        text: qsTr("Branch: %1").arg(AppInfo.ScmCommitsAheadMaster > 0 ?
                        // i18n: A positive number of version control commits made compared to some named branch
                        qsTr("%1 (%2 commit(s) ahead of %3)").arg(AppInfo.ScmBranchStr).arg(AppInfo.ScmCommitsAheadMaster).arg("master") : AppInfo.ScmBranchStr)
                    }
                    Label {
                        font.pointSize: 9
                        text: qsTr("Revision: %1").arg(AppInfo.ScmRevGitStr)
                    }
                }

                Label {
                    font.pointSize: 9
                    text: qsTr("Using Qt %1").arg(AppInfo.QtVersion)
                }

                Label {
                    text: qsTr("Check for updates: <a href='https://dolphin-emu.org/download'>dolphin-emu.org/download</a>")
                    onLinkActivated: link => Qt.openUrlExternally(link)
                }

                Label {
                    // i18n: The word "free" in the standard phrase "free and open source" is
                    // "free" as in "freedom" - it refers to certain properties of the software's
                    // license, not the software's price.
                    text: qsTr("Dolphin is a free and open-source GameCube and Wii emulator.")
                }

                Label {
                    wrapMode: Text.WordWrap
                    text: qsTr("This software should not be used to play games you do not legally own.")
                }

                RowLayout {
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
            Layout.alignment: Qt.AlignHCenter
            // i18n: This message uses curly quotes in English. If you want to use curly quotes
            // in your translation, please use the type appropriate for your language.
            text: qsTr("\u00A9 2003-2026+ Dolphin Team. \u201cGameCube\u201d and \u201cWii\u201d are trademarks of Nintendo. Dolphin is not affiliated with Nintendo in any way.")
        }
    }
}

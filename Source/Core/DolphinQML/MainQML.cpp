// Copyright 2026 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#ifdef _WIN32
#include <cstdio>
#include <string>
#include <vector>

#include <Windows.h>
#endif

#ifdef __linux__
#include <cstdlib>
#endif

#include <QApplication>
#include <QDebug>
#include <QDirIterator>
#include <QQmlApplicationEngine>

#include "OptionParser.h"

#include "Common/Config/Config.h"
#include "Common/MsgHandler.h"
#include "Common/StringUtil.h"

#include "Core/Boot/Boot.h"
#include "Core/Config/MainSettings.h"
#include "Core/Core.h"
#include "Core/DolphinAnalytics.h"
#include "Core/System.h"

#include "DolphinQML/Host.h"
#include "DolphinQML/Translation.h"

#include "UICommon/CommandLineParse.h"
#include "UICommon/UICommon.h"
#ifdef USE_DISCORD_PRESENCE
#include "UICommon/DiscordPresence.h"
#endif

int main(int argc, char* argv[])
{
  // QDirIterator it(":/qt", QDirIterator::Subdirectories);
  // while (it.hasNext())
  // {
  //   qDebug() << it.next();
  // }
#ifdef _WIN32
  const bool console_attached = AttachConsole(ATTACH_PARENT_PROCESS) != FALSE;
  HANDLE stdout_handle = ::GetStdHandle(STD_OUTPUT_HANDLE);
  if (console_attached && stdout_handle)
  {
    freopen("CONOUT$", "w", stdout);
    freopen("CONOUT$", "w", stderr);
  }
#endif

#ifdef __APPLE__
  // On macOS, a command line option matching the format "-psn_X_XXXXXX" is passed when
  // the application is launched for the first time. This is to set the "ProcessSerialNumber",
  // something used by the legacy Process Manager from Carbon. optparse will fail if it finds
  // this as it isn't a valid Dolphin command line option, so pretend like it doesn't exist
  // if found.
  if (strncmp(argv[argc - 1], "-psn", 4) == 0)
  {
    argc--;
  }
#endif

  // TODO: This lags QML like hell
  // #ifdef __linux__
  //   // Qt 6.3+ has a bug which causes mouse inputs to not be registered in our XInput2 code.
  //   // If we define QT_XCB_NO_XI2, Qt's xcb platform plugin no longer initializes its XInput
  //   // code, which makes mouse inputs work again.
  //   // For more information: https://bugs.dolphin-emu.org/issues/12913
  // #if (QT_VERSION >= QT_VERSION_CHECK(6, 3, 0))
  //   setenv("QT_XCB_NO_XI2", "1", true);
  // #endif
  //   // Dolphin currently doesn't work on Wayland (Only the UI does, games do not launch.) This
  //   makes
  //   // XCB the default and forces it on if the platform is specified to be wayland, to prevent
  //   this
  //   // from happening.
  //   // For more information: https://bugs.dolphin-emu.org/issues/11807
  //   const char* current_qt_platform = getenv("QT_QPA_PLATFORM");
  //   const bool replace_qt_platform = current_qt_platform != nullptr &&
  //                                    Common::CaseInsensitiveContains(current_qt_platform,
  //                                    "wayland");
  //   setenv("QT_QPA_PLATFORM", "xcb", replace_qt_platform);
  // #endif

  // QApplication will parse arguments and remove any it recognizes as targeting Qt
  QApplication app(argc, argv);

  app.setOrganizationName(QStringLiteral("Dolphin Emulator"));
  app.setOrganizationDomain(QStringLiteral("dolphin-emu.org"));
  app.setApplicationName(QStringLiteral("dolphin-emu"));

  auto parser = CommandLineParse::CreateParser(CommandLineParse::ParserOptions::IncludeGUIOptions);
  const optparse::Values& options = CommandLineParse::ParseArguments(parser.get(), argc, argv);
  const std::vector<std::string> args = parser->args();

#ifdef _WIN32
  // QtUtils::InstallWindowDecorationFilter(&app);

  FreeConsole();
#endif

  UICommon::SetUserDirectory(static_cast<const char*>(options.get("user")));
  UICommon::CreateDirectories();
  UICommon::Init();

  // TODO
  // Settings::Instance().SetBatchModeEnabled(options.is_set("batch"));

  // TODO
  // Hook up alerts from core
  // Common::RegisterMsgAlertHandler();

  // Hook up translations
  Translation::Initialize();

  // Whenever the event loop is about to go to sleep, dispatch the jobs
  // queued in the Core first.
  // QObject::connect(QAbstractEventDispatcher::instance(), &QAbstractEventDispatcher::aboutToBlock,
  //                  &app, [] { Core::HostDispatchJobs(Core::System::GetInstance()); });

  std::optional<std::string> save_state_path;
  if (options.is_set("save_state"))
  {
    save_state_path = static_cast<const char*>(options.get("save_state"));
  }

  std::unique_ptr<BootParameters> boot;
  bool game_specified = false;
  if (options.is_set("exec"))
  {
    const std::list<std::string> paths_list = options.all("exec");
    const std::vector<std::string> paths{std::make_move_iterator(std::begin(paths_list)),
                                         std::make_move_iterator(std::end(paths_list))};
    boot = BootParameters::GenerateFromFile(
        paths, BootSessionData(save_state_path, DeleteSavestateAfterBoot::No));
    game_specified = true;
  }
  else if (options.is_set("nand_title"))
  {
    const std::string hex_string = static_cast<const char*>(options.get("nand_title"));
    if (hex_string.length() == 16)
    {
      const u64 title_id = std::stoull(hex_string, nullptr, 16);
      boot = std::make_unique<BootParameters>(BootParameters::NANDTitle{title_id});
    }
    else
    {
      // ModalMessageBox::critical(nullptr, QObject::tr("Error"), QObject::tr("Invalid title ID."));
    }
    game_specified = true;
  }
  else if (!args.empty())
  {
    boot = BootParameters::GenerateFromFile(
        args.front(), BootSessionData(save_state_path, DeleteSavestateAfterBoot::No));
    game_specified = true;
  }

  int retval;

  if (save_state_path && !game_specified)
  {
    // ModalMessageBox::critical(
    //     nullptr, QObject::tr("Error"),
    //     QObject::tr("A save state cannot be loaded without specifying a game to launch."));
    retval = 1;
  }
  // else if (Settings::Instance().IsBatchModeEnabled() && !game_specified)
  // {
  //   ModalMessageBox::critical(
  //       nullptr, QObject::tr("Error"),
  //       QObject::tr("Batch mode cannot be used without specifying a game to launch."));
  //   retval = 1;
  // }
  // else if (!boot && (Settings::Instance().IsBatchModeEnabled() || save_state_path))
  // {
  //   // A game to launch was specified, but it was invalid.
  //   // An error has already been shown by code above, so exit without showing another error.
  //   retval = 1;
  // }
  else
  {
    DolphinAnalytics::Instance().ReportDolphinStart("qt");

    // TODO
    // Settings::Instance().InitDefaultPalette();
    // Settings::Instance().ApplyStyle();

    // TODO
    //     MainWindow win{Core::System::GetInstance(), std::move(boot),
    //                    static_cast<const char*>(options.get("movie"))};

    // #if defined(USE_ANALYTICS) && USE_ANALYTICS
    //     if (!Config::Get(Config::MAIN_ANALYTICS_PERMISSION_ASKED))
    //     {
    //       // To ensure that the analytics prompt appears aligned with the center of the main
    //       window,
    //       // the dialog is only shown after the application is ready, as only then it is
    //       guaranteed that
    //       // the main window has been placed in its final position.
    //       auto* const connection_context = new QObject(&win);
    //       QObject::connect(qApp, &QGuiApplication::applicationStateChanged, connection_context,
    //                        [connection_context, &win](const Qt::ApplicationState state) {
    //                          if (state != Qt::ApplicationState::ApplicationActive)
    //                            return;

    //                          // Severe the connection after the first run.
    //                          delete connection_context;

    //                          ShowAnalyticsPrompt(&win);
    //                        });
    //     }
    // #endif

    // TODO
    // if (!Settings::Instance().IsBatchModeEnabled())
    // {
    //   auto* updater = new Updater(&win, Config::Get(Config::MAIN_AUTOUPDATE_UPDATE_TRACK),
    //                               Config::Get(Config::MAIN_AUTOUPDATE_HASH_OVERRIDE));
    //   updater->start();
    // }

    QQmlApplicationEngine engine;
    engine.loadFromModule("DolphinEmu", "Main");

    retval = app.exec();
  }

  Core::Shutdown(Core::System::GetInstance());
  UICommon::Shutdown();
  Host::GetInstance()->deleteLater();

  return retval;
}

set(CMAKE_AUTOMOC ON)

set(QT_QML_GENERATE_QMLLS_INI ON)
find_package(Qt6 REQUIRED COMPONENTS Quick QuickControls2 Widgets)

message(STATUS "Found Qt version ${Qt6_VERSION}")

qt_policy(SET QTP0004 NEW)
qt_standard_project_setup(REQUIRES 6.5)

qt_add_executable(dolphin-emu
    MainQML.cpp
)

# TODO: There has to be a better way
target_include_directories(dolphin-emu PRIVATE
    ${CMAKE_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/Utils
    ${CMAKE_CURRENT_SOURCE_DIR}/Menu
)

set(PLAIN_QML_FILES
    AboutDialog.qml
    GameList/GameEmpty.qml
    # GameList/GameGrid.qml
    GameList/GameList.qml
    # GameList/GameTable.qml
    Header/DesktopHeader.qml
    Header/DesktopToolButton.qml
    Header/MobileHeader.qml
    Menu/EmulationMenu.qml
    Menu/FileMenu.qml
    Menu/HelpMenu.qml
    Menu/MovieMenu.qml
    Menu/OptionsMenu.qml
    Menu/ToolsMenu.qml
    Menu/ViewMenu.qml
    Main.qml
    Utils/GameFileDialog.qml
)
set(QML_SINGLETONS
    Utils/DLayout.qml
    Utils/DResource.qml
)
set(SOURCES
    AppInfo.cpp
    AppInfo.h
    Host.cpp
    Host.h
    Menu/MenuHelper.cpp
    Menu/MenuHelper.h
    Translation.cpp
    Translation.h
    Utils/DHotkeyDisabler.cpp
    Utils/DHotkeyDisabler.h
    Utils/QMLUtils.cpp
    Utils/QMLUtils.h
)
file(GLOB_RECURSE THEME_RESOURCES
    CONFIGURE_DEPENDS
    RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}"
    "${CMAKE_SOURCE_DIR}/Data/Sys/Themes/*"
)
file(GLOB_RECURSE DOLPHIN_RESOURCES
    CONFIGURE_DEPENDS
    RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}"
    "${CMAKE_SOURCE_DIR}/Data/Sys/Resources/*"
)
set(RESOURCES
    ${THEME_RESOURCES}
    ${DOLPHIN_RESOURCES}
)

set_source_files_properties(${QML_SINGLETONS} PROPERTIES QT_QML_SINGLETON_TYPE TRUE)

qt_add_qml_module(dolphin-emu
    URI DolphinEmu
    VERSION 2701.999
    QML_FILES ${PLAIN_QML_FILES} ${QML_SINGLETONS}
    RESOURCES ${RESOURCES}
    SOURCES ${SOURCES}
)

qt_add_resources(dolphin-emu "resources"
    PREFIX "/"
    FILES ${RESOURCES}
)

target_link_libraries(dolphin-emu
    PRIVATE
        core
        Qt6::Quick
        Qt6::QuickControls2
        Qt6::Widgets
        uicommon
        imgui
        implot
)

# Handle localization
find_package(Gettext)

if(WIN32 AND NOT Gettext_FOUND)
  message(STATUS "Using Gettext from Externals")
  set(GETTEXT_MSGFMT_EXECUTABLE "${CMAKE_SOURCE_DIR}/Externals/gettext/msgfmt.exe")
endif()

if(GETTEXT_MSGFMT_EXECUTABLE)
  set(pot_file "${CMAKE_SOURCE_DIR}/Languages/po/dolphin-emu.pot")
  file(GLOB LINGUAS ${CMAKE_SOURCE_DIR}/Languages/po/*.po)

  target_sources(dolphin-emu PRIVATE ${pot_file} ${LINGUAS})
  source_group("Localization" FILES ${LINGUAS})
  source_group("Localization\\\\Generated" FILES ${pot_file})

  foreach(po ${LINGUAS})
    get_filename_component(lang ${po} NAME_WE)
    if(WIN32)
      set(mo_dir ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/Languages)
      set(mo ${mo_dir}/${lang}.mo)
    else()
      set(mo_dir ${CMAKE_CURRENT_BINARY_DIR}/${lang})
      set(mo ${mo_dir}/dolphin-emu.mo)
    endif()

    target_sources(dolphin-emu PRIVATE ${mo})
    source_group("Localization\\\\Generated" FILES ${mo})

    if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
      set_source_files_properties(${mo} PROPERTIES MACOSX_PACKAGE_LOCATION "Resources/${lang}.lproj")
    else()
      install(FILES ${mo} DESTINATION share/locale/${lang}/LC_MESSAGES)
    endif()

    if(WIN32)
      add_custom_command(OUTPUT ${mo}
        COMMAND ${CMAKE_COMMAND} -E make_directory ${mo_dir}
        COMMAND ${GETTEXT_MSGFMT_EXECUTABLE} -o ${mo} ${po}
        DEPENDS ${po}
      )
    else()
      add_custom_command(OUTPUT ${mo}
        COMMAND ${CMAKE_COMMAND} -E make_directory ${mo_dir}
        COMMAND ${GETTEXT_MSGMERGE_EXECUTABLE} --quiet --update --backup=none ${po} ${pot_file}
        COMMAND ${GETTEXT_MSGFMT_EXECUTABLE} -o ${mo} ${po}
        DEPENDS ${po}
      )
    endif()
  endforeach()
endif()
# TODO:
# if (MSVC)
#   if(_M_ARM_64)
#     list(APPEND CMAKE_PREFIX_PATH "${CMAKE_SOURCE_DIR}/Externals/Qt/Qt6.8.3/ARM64")
#   else()
#     list(APPEND CMAKE_PREFIX_PATH "${CMAKE_SOURCE_DIR}/Externals/Qt/Qt6.8.3/x64")
#   endif()
# endif()

set(ENABLE_SVG ${WIN32})

include(FetchContent)

if(NOT GENERATOR_IS_MULTI_CONFIG)
    unset(CMAKE_CONFIGURATION_TYPES CACHE)
endif()

set(QT_USE_CCACHE ${ENABLE_CCACHE})
set(QT_BUILD_EXAMPLES OFF)
set(QT_BUILD_BENCHMARKS OFF)
set(QT_BUILD_TESTS OFF)
set(QT_INSTALL_EXAMPLES_SOURCES OFF)
set(FEATURE_network OFF)
set(FEATURE_printsupport OFF)
set(FEATURE_sql OFF)
set(FEATURE_vulkan OFF)
set(FEATURE_testlib OFF)
set(FEATURE_xml OFF)
set(FEATURE_concurrent OFF)
set(FEATURE_windeployqt OFF)
set(FEATURE_androiddeployqt OFF)
set(FEATURE_qmake OFF)
set(FEATURE_cxx20 ON)
set(FEATURE_gui ON)
set(FEATURE_widgets ON)
set(FEATURE_opengl OFF)
set(INPUT_opengl no)

FetchContent_Declare(
    qt6
    GIT_REPOSITORY "git://code.qt.io/qt/qt5.git"
    GIT_TAG "v6.11.1"
    GIT_SUBMODULES "qtbase"
    GIT_SUBMODULES_RECURSE ON
    OVERRIDE_FIND_PACKAGE ON
)

FetchContent_MakeAvailable(qt6)
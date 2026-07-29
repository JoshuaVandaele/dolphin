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

# TODO: Actually get those
set(QT_HOST_PATH ${CMAKE_CURRENT_BINARY_DIR})
set(QT_HOST_PATH_CMAKE_DIR ${CMAKE_CURRENT_BINARY_DIR})

set(QT_USE_CCACHE ${ENABLE_CCACHE} CACHE BOOL "" FORCE)
set(QT_BUILD_EXAMPLES OFF CACHE BOOL "" FORCE)
set(QT_BUILD_BENCHMARKS OFF CACHE BOOL "" FORCE)
set(QT_BUILD_TESTS OFF CACHE BOOL "" FORCE)
set(QT_INSTALL_EXAMPLES_SOURCES OFF CACHE BOOL "" FORCE)
set(QT_FORCE_BUILD_TOOLS ON CACHE BOOL "" FORCE)

set(FEATURE_network OFF CACHE BOOL "" FORCE)
set(FEATURE_printsupport OFF CACHE BOOL "" FORCE)
set(FEATURE_sql OFF CACHE BOOL "" FORCE)
set(FEATURE_vulkan OFF CACHE BOOL "" FORCE)
set(FEATURE_testlib OFF CACHE BOOL "" FORCE)
set(FEATURE_xml OFF CACHE BOOL "" FORCE)
set(FEATURE_concurrent OFF CACHE BOOL "" FORCE)
set(FEATURE_windeployqt OFF CACHE BOOL "" FORCE)
set(FEATURE_androiddeployqt OFF CACHE BOOL "" FORCE)
set(FEATURE_qmake OFF CACHE BOOL "" FORCE)
set(FEATURE_cxx20 ON CACHE BOOL "" FORCE)
set(FEATURE_gui ON CACHE BOOL "" FORCE)
set(FEATURE_widgets ON CACHE BOOL "" FORCE)
set(FEATURE_opengl OFF CACHE BOOL "" FORCE)
set(INPUT_opengl no CACHE BOOL "" FORCE)
set(FEATURE_wayland OFF CACHE BOOL "" FORCE)

set(FEATURE_xcb ON CACHE BOOL "" FORCE)

# TODO: Should we instead manually clone qttools and qtbase and build them ourselves? I think that would make a lot of things easier
FetchContent_Declare(
    qt6
    GIT_REPOSITORY "git://code.qt.io/qt/qt5.git"
    GIT_TAG "v6.11.1"
    GIT_SUBMODULES "qtbase;qttools"
    GIT_SUBMODULES_RECURSE ON
    OVERRIDE_FIND_PACKAGE ON
)

FetchContent_MakeAvailable(qt6)
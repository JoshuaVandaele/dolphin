set(BUILD_SHARED_LIBS OFF)
set(XZ_MICROLZMA_ENCODER OFF)
set(XZ_LZIP_DECODER OFF)
set(XZ_TOOL_XZDEC OFF)
set(XZ_TOOL_LZMADEC OFF)
set(XZ_TOOL_LZMAINFO OFF)
set(XZ_TOOL_XZ OFF)
set(XZ_TOOL_SCRIPTS OFF)
set(XZ_DOXYGEN OFF)
set(XZ_DOC OFF)
add_subdirectory(xz EXCLUDE_FROM_ALL)

target_include_directories(liblzma
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/xz/src/liblzma/api>
)

add_library(LibLZMA::LibLZMA ALIAS liblzma)
dolphin_disable_warnings(liblzma)
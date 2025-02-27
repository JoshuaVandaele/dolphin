cmake_minimum_required(VERSION 3.14..4.0)
project(zlib-rs C)
enable_language(C)

find_program(CARGO cargo REQUIRED)
if (NOT CARGO)
    message(FATAL_ERROR "Cargo not found. Please install Rust and Cargo to continue.")
endif()

set(CARGO_TARGET_DIR "${CMAKE_BINARY_DIR}/Externals/zlib-rs")
set(RUSTFLAGS "${RUSTFLAGS} -Awarnings -Cllvm-args=-enable-dfa-jump-thread")

set(ZLIB_RS_LIB_RELEASE "${CARGO_TARGET_DIR}/release/libz_rs.so")
set(ZLIB_RS_LIB_DEBUG "${CARGO_TARGET_DIR}/debug/libz_rs.so")
set(ZLIB_RS_DIR "${CMAKE_SOURCE_DIR}/Externals/zlib-rs/zlib-rs/libz-rs-sys-cdylib")

# Set ZLIB variables for find_package used by other projects
if (CMAKE_BUILD_TYPE STREQUAL "Release")
    set(ZLIB_RS_LIB "${ZLIB_RS_LIB_RELEASE}")
    set(ZLIB_ROOT "${ZLIB_RS_LIB_RELEASE}" PARENT_SCOPE)
    set(ZLIB_INCLUDE_DIR "${ZLIB_RS_LIB_RELEASE}" PARENT_SCOPE)
    set(CARGO_PROFILE "release")
else()
    set(ZLIB_RS_LIB "${ZLIB_RS_LIB_DEBUG}")
    set(ZLIB_ROOT "${ZLIB_RS_LIB_DEBUG}" PARENT_SCOPE)
    set(ZLIB_INCLUDE_DIR "${ZLIB_RS_LIB_DEBUG}" PARENT_SCOPE)
    set(CARGO_PROFILE "debug")
endif()
set(ZLIB_LIBRARY ZLIB::ZLIB CACHE STRING "Path to zlib library" PARENT_SCOPE)

add_custom_target(
    zlib-rs-compat
    COMMAND ${CMAKE_COMMAND} -E env
        "CARGO_TARGET_DIR=${CARGO_TARGET_DIR}"
        "RUSTFLAGS=${RUSTFLAGS}"
        ${CARGO} build --profile=${CARGO_PROFILE}
    WORKING_DIRECTORY ${ZLIB_RS_DIR}
    COMMENT "Building zlib-rs using Cargo"
    BYPRODUCTS "${ZLIB_RS_LIB}"
)

add_library(zlib-rs SHARED IMPORTED GLOBAL)
set_target_properties(zlib-rs PROPERTIES
    IMPORTED_LOCATION_RELEASE "${ZLIB_RS_LIB_RELEASE}"
    IMPORTED_LOCATION_DEBUG "${ZLIB_RS_LIB_DEBUG}"
)

add_dependencies(zlib-rs zlib-rs-compat)

# Setup zlib alias project so FindZLIB doesn't recreate it
add_library(ZLIB::ZLIB ALIAS zlib-rs)
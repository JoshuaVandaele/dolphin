set(BUILD_SHARED_LIBS OFF)
set(BUILD_TESTS OFF)
set(BUILD_TOOLS OFF)
set(USE_SANITIZERS OFF)

# TODO: Waiting on https://github.com/mozilla/cubeb/pull/807 before PR

find_program(CARGO_BIN cargo)
if(CARGO_BIN)
	set(BUILD_RUST_LIBS ON)
endif()

add_subdirectory(cubeb)
dolphin_disable_warnings(cubeb)

add_library(cubeb::cubeb ALIAS cubeb)

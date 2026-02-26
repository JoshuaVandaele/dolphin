set(BUILD_SHARED_LIBS OFF)
set(SIMDJSON_DISABLE_DEPRECATED_API ON)
# TODO C++26: Enable and use reflection where possible
set(SIMDJSON_STATIC_REFLECTION OFF)

add_subdirectory(simdjson EXCLUDE_FROM_ALL)
dolphin_disable_warnings(simdjson)

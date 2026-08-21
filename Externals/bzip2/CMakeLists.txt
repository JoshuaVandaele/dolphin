include(FetchContent)

FetchContent_Declare(
    Corrosion
    GIT_REPOSITORY https://github.com/corrosion-rs/corrosion.git
    GIT_TAG v0.6.1
)
FetchContent_MakeAvailable(Corrosion)

set(BUILD_SHARED_LIBS OFF) # TODO: Static lib instead of shared? (Waiting for upstream)

FetchContent_Declare(
    libbzip2-rs
    GIT_REPOSITORY https://github.com/trifectatechfoundation/libbzip2-rs.git
    GIT_TAG v0.2.5
)
FetchContent_MakeAvailable(libbzip2-rs)

corrosion_import_crate(
    MANIFEST_PATH ${libbzip2-rs_SOURCE_DIR}/libbz2-rs-sys-cdylib/Cargo.toml
)

target_include_directories(bz2_rs INTERFACE ${libbzip2-rs_SOURCE_DIR})
add_library(BZip2::BZip2 ALIAS bz2_rs)

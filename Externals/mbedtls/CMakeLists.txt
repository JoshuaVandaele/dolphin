include(FetchContent)

set(MBEDTLS_VERSION "4.2.0")

FetchContent_Declare(
  mbedtls
  URL      https://github.com/Mbed-TLS/mbedtls/releases/download/mbedtls-${MBEDTLS_VERSION}/mbedtls-${MBEDTLS_VERSION}.tar.bz2
  URL_HASH SHA256=2bed9d713b4668f76553b097e72b8aa30bc8f112a940d7ae228d524bbde6ffea
)

set(ENABLE_PROGRAMS OFF CACHE BOOL "")
set(MBEDTLS_FATAL_WARNINGS OFF CACHE BOOL "")
set(ENABLE_TESTING OFF CACHE BOOL "")
set(GEN_FILES OFF CACHE BOOL "")

FetchContent_MakeAvailable(mbedtls)

dolphin_disable_warnings(mbedtls)
dolphin_disable_warnings(mbedx509)
dolphin_disable_warnings(tfpsacrypto)

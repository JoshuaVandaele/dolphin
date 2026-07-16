/**
 * \file check_config.h
 *
 * \brief Consistency checks for configuration options
 */
/*
 *  Copyright The Mbed TLS Contributors
 *  SPDX-License-Identifier: Apache-2.0
 *
 *  Licensed under the Apache License, Version 2.0 (the "License"); you may
 *  not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 *  WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

/*
 * It is recommended to include this file from your config.h
 * in order to catch dependency issues early.
 */

#ifndef MBEDTLS2_CHECK_CONFIG_H
#define MBEDTLS2_CHECK_CONFIG_H

/*
 * We assume CHAR_BIT is 8 in many places. In practice, this is true on our
 * target platforms, so not an issue, but let's just be extra sure.
 */
#include <limits.h>
#if CHAR_BIT != 8
#error "mbed TLS requires a platform with 8-bit chars"
#endif

#if defined(_WIN32)
#if !defined(MBEDTLS2_PLATFORM_C)
#error "MBEDTLS2_PLATFORM_C is required on Windows"
#endif

/* Fix the config here. Not convenient to put an #ifdef _WIN32 in config.h as
 * it would confuse config.py. */
#if !defined(MBEDTLS2_PLATFORM_SNPRINTF_ALT) && \
    !defined(MBEDTLS2_PLATFORM_SNPRINTF_MACRO)
#define MBEDTLS2_PLATFORM_SNPRINTF_ALT
#endif

#if !defined(MBEDTLS2_PLATFORM_VSNPRINTF_ALT) && \
    !defined(MBEDTLS2_PLATFORM_VSNPRINTF_MACRO)
#define MBEDTLS2_PLATFORM_VSNPRINTF_ALT
#endif
#endif /* _WIN32 */

#if defined(TARGET_LIKE_MBED) && defined(MBEDTLS2_NET_C)
#error "The NET module is not available for mbed OS - please use the network functions provided by Mbed OS"
#endif

#if defined(MBEDTLS2_DEPRECATED_WARNING) && \
    !defined(__GNUC__) && !defined(__clang__)
#error "MBEDTLS2_DEPRECATED_WARNING only works with GCC and Clang"
#endif

#if defined(MBEDTLS2_HAVE_TIME_DATE) && !defined(MBEDTLS2_HAVE_TIME)
#error "MBEDTLS2_HAVE_TIME_DATE without MBEDTLS2_HAVE_TIME does not make sense"
#endif

#if defined(MBEDTLS2_AESNI_C) && !defined(MBEDTLS2_HAVE_ASM)
#error "MBEDTLS2_AESNI_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_CTR_DRBG_C) && !defined(MBEDTLS2_AES_C)
#error "MBEDTLS2_CTR_DRBG_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_DHM_C) && !defined(MBEDTLS2_BIGNUM_C)
#error "MBEDTLS2_DHM_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_SSL_TRUNCATED_HMAC_COMPAT) && !defined(MBEDTLS2_SSL_TRUNCATED_HMAC)
#error "MBEDTLS2_SSL_TRUNCATED_HMAC_COMPAT defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_CMAC_C) && \
    !defined(MBEDTLS2_AES_C) && !defined(MBEDTLS2_DES_C)
#error "MBEDTLS2_CMAC_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_NIST_KW_C) && \
    ( !defined(MBEDTLS2_AES_C) || !defined(MBEDTLS2_CIPHER_C) )
#error "MBEDTLS2_NIST_KW_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_ECDH_C) && !defined(MBEDTLS2_ECP_C)
#error "MBEDTLS2_ECDH_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_ECDSA_C) &&            \
    ( !defined(MBEDTLS2_ECP_C) ||           \
      !( defined(MBEDTLS2_ECP_DP_SECP192R1_ENABLED) || \
         defined(MBEDTLS2_ECP_DP_SECP224R1_ENABLED) || \
         defined(MBEDTLS2_ECP_DP_SECP256R1_ENABLED) || \
         defined(MBEDTLS2_ECP_DP_SECP384R1_ENABLED) || \
         defined(MBEDTLS2_ECP_DP_SECP521R1_ENABLED) || \
         defined(MBEDTLS2_ECP_DP_SECP192K1_ENABLED) || \
         defined(MBEDTLS2_ECP_DP_SECP224K1_ENABLED) || \
         defined(MBEDTLS2_ECP_DP_SECP256K1_ENABLED) || \
         defined(MBEDTLS2_ECP_DP_BP256R1_ENABLED) ||   \
         defined(MBEDTLS2_ECP_DP_BP384R1_ENABLED) ||   \
         defined(MBEDTLS2_ECP_DP_BP512R1_ENABLED) ) || \
      !defined(MBEDTLS2_ASN1_PARSE_C) ||    \
      !defined(MBEDTLS2_ASN1_WRITE_C) )
#error "MBEDTLS2_ECDSA_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_ECJPAKE_C) &&           \
    ( !defined(MBEDTLS2_ECP_C) || !defined(MBEDTLS2_MD_C) )
#error "MBEDTLS2_ECJPAKE_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_ECP_RESTARTABLE)           && \
    ( defined(MBEDTLS2_USE_PSA_CRYPTO)          || \
      defined(MBEDTLS2_ECDH_COMPUTE_SHARED_ALT) || \
      defined(MBEDTLS2_ECDH_GEN_PUBLIC_ALT)     || \
      defined(MBEDTLS2_ECDSA_SIGN_ALT)          || \
      defined(MBEDTLS2_ECDSA_VERIFY_ALT)        || \
      defined(MBEDTLS2_ECDSA_GENKEY_ALT)        || \
      defined(MBEDTLS2_ECP_INTERNAL_ALT)        || \
      defined(MBEDTLS2_ECP_ALT) )
#error "MBEDTLS2_ECP_RESTARTABLE defined, but it cannot coexist with an alternative or PSA-based ECP implementation"
#endif

#if defined(MBEDTLS2_ECP_RESTARTABLE)           && \
    ! defined(MBEDTLS2_ECDH_LEGACY_CONTEXT)
#error "MBEDTLS2_ECP_RESTARTABLE defined, but not MBEDTLS2_ECDH_LEGACY_CONTEXT"
#endif

#if defined(MBEDTLS2_ECDH_VARIANT_EVEREST_ENABLED)           && \
    defined(MBEDTLS2_ECDH_LEGACY_CONTEXT)
#error "MBEDTLS2_ECDH_VARIANT_EVEREST_ENABLED defined, but MBEDTLS2_ECDH_LEGACY_CONTEXT not disabled"
#endif

#if defined(MBEDTLS2_ECDSA_DETERMINISTIC) && !defined(MBEDTLS2_HMAC_DRBG_C)
#error "MBEDTLS2_ECDSA_DETERMINISTIC defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_ECP_C) && ( !defined(MBEDTLS2_BIGNUM_C) || (    \
    !defined(MBEDTLS2_ECP_DP_SECP192R1_ENABLED) &&                  \
    !defined(MBEDTLS2_ECP_DP_SECP224R1_ENABLED) &&                  \
    !defined(MBEDTLS2_ECP_DP_SECP256R1_ENABLED) &&                  \
    !defined(MBEDTLS2_ECP_DP_SECP384R1_ENABLED) &&                  \
    !defined(MBEDTLS2_ECP_DP_SECP521R1_ENABLED) &&                  \
    !defined(MBEDTLS2_ECP_DP_BP256R1_ENABLED)   &&                  \
    !defined(MBEDTLS2_ECP_DP_BP384R1_ENABLED)   &&                  \
    !defined(MBEDTLS2_ECP_DP_BP512R1_ENABLED)   &&                  \
    !defined(MBEDTLS2_ECP_DP_SECP192K1_ENABLED) &&                  \
    !defined(MBEDTLS2_ECP_DP_SECP224K1_ENABLED) &&                  \
    !defined(MBEDTLS2_ECP_DP_SECP256K1_ENABLED) &&                  \
    !defined(MBEDTLS2_ECP_DP_CURVE25519_ENABLED) &&                 \
    !defined(MBEDTLS2_ECP_DP_CURVE448_ENABLED) ) )
#error "MBEDTLS2_ECP_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_ECP_C) && !(            \
    defined(MBEDTLS2_ECP_ALT) ||             \
    defined(MBEDTLS2_CTR_DRBG_C) ||          \
    defined(MBEDTLS2_HMAC_DRBG_C) ||         \
    defined(MBEDTLS2_ECP_NO_INTERNAL_RNG))
#error "MBEDTLS2_ECP_C requires a DRBG module unless MBEDTLS2_ECP_NO_INTERNAL_RNG is defined or an alternative implementation is used"
#endif

#if defined(MBEDTLS2_PK_PARSE_C) && !defined(MBEDTLS2_ASN1_PARSE_C)
#error "MBEDTLS2_PK_PARSE_C defined, but not all prerequesites"
#endif

#if defined(MBEDTLS2_ENTROPY_C) && (!defined(MBEDTLS2_SHA512_C) &&      \
                                    !defined(MBEDTLS2_SHA256_C))
#error "MBEDTLS2_ENTROPY_C defined, but not all prerequisites"
#endif
#if defined(MBEDTLS2_ENTROPY_C) && defined(MBEDTLS2_SHA512_C) &&         \
    defined(MBEDTLS2_CTR_DRBG_ENTROPY_LEN) && (MBEDTLS2_CTR_DRBG_ENTROPY_LEN > 64)
#error "MBEDTLS2_CTR_DRBG_ENTROPY_LEN value too high"
#endif
#if defined(MBEDTLS2_ENTROPY_C) &&                                            \
    ( !defined(MBEDTLS2_SHA512_C) || defined(MBEDTLS2_ENTROPY_FORCE_SHA256) ) \
    && defined(MBEDTLS2_CTR_DRBG_ENTROPY_LEN) && (MBEDTLS2_CTR_DRBG_ENTROPY_LEN > 32)
#error "MBEDTLS2_CTR_DRBG_ENTROPY_LEN value too high"
#endif
#if defined(MBEDTLS2_ENTROPY_C) && \
    defined(MBEDTLS2_ENTROPY_FORCE_SHA256) && !defined(MBEDTLS2_SHA256_C)
#error "MBEDTLS2_ENTROPY_FORCE_SHA256 defined, but not all prerequisites"
#endif

#if defined(__has_feature)
#if __has_feature(memory_sanitizer)
#define MBEDTLS2_HAS_MEMSAN
#endif
#endif
#if defined(MBEDTLS2_TEST_CONSTANT_FLOW_MEMSAN) &&  !defined(MBEDTLS2_HAS_MEMSAN)
#error "MBEDTLS2_TEST_CONSTANT_FLOW_MEMSAN requires building with MemorySanitizer"
#endif
#undef MBEDTLS2_HAS_MEMSAN

#if defined(MBEDTLS2_TEST_NULL_ENTROPY) && \
    ( !defined(MBEDTLS2_ENTROPY_C) || !defined(MBEDTLS2_NO_DEFAULT_ENTROPY_SOURCES) )
#error "MBEDTLS2_TEST_NULL_ENTROPY defined, but not all prerequisites"
#endif
#if defined(MBEDTLS2_TEST_NULL_ENTROPY) && \
     ( defined(MBEDTLS2_ENTROPY_NV_SEED) || defined(MBEDTLS2_ENTROPY_HARDWARE_ALT) || \
    defined(MBEDTLS2_HAVEGE_C) )
#error "MBEDTLS2_TEST_NULL_ENTROPY defined, but entropy sources too"
#endif

#if defined(MBEDTLS2_GCM_C) && (                                        \
        !defined(MBEDTLS2_AES_C) && !defined(MBEDTLS2_CAMELLIA_C) && !defined(MBEDTLS2_ARIA_C) )
#error "MBEDTLS2_GCM_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_ECP_RANDOMIZE_JAC_ALT) && !defined(MBEDTLS2_ECP_INTERNAL_ALT)
#error "MBEDTLS2_ECP_RANDOMIZE_JAC_ALT defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_ECP_ADD_MIXED_ALT) && !defined(MBEDTLS2_ECP_INTERNAL_ALT)
#error "MBEDTLS2_ECP_ADD_MIXED_ALT defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_ECP_DOUBLE_JAC_ALT) && !defined(MBEDTLS2_ECP_INTERNAL_ALT)
#error "MBEDTLS2_ECP_DOUBLE_JAC_ALT defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_ECP_NORMALIZE_JAC_MANY_ALT) && !defined(MBEDTLS2_ECP_INTERNAL_ALT)
#error "MBEDTLS2_ECP_NORMALIZE_JAC_MANY_ALT defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_ECP_NORMALIZE_JAC_ALT) && !defined(MBEDTLS2_ECP_INTERNAL_ALT)
#error "MBEDTLS2_ECP_NORMALIZE_JAC_ALT defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_ECP_DOUBLE_ADD_MXZ_ALT) && !defined(MBEDTLS2_ECP_INTERNAL_ALT)
#error "MBEDTLS2_ECP_DOUBLE_ADD_MXZ_ALT defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_ECP_RANDOMIZE_MXZ_ALT) && !defined(MBEDTLS2_ECP_INTERNAL_ALT)
#error "MBEDTLS2_ECP_RANDOMIZE_MXZ_ALT defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_ECP_NORMALIZE_MXZ_ALT) && !defined(MBEDTLS2_ECP_INTERNAL_ALT)
#error "MBEDTLS2_ECP_NORMALIZE_MXZ_ALT defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_ECP_NO_FALLBACK) && !defined(MBEDTLS2_ECP_INTERNAL_ALT)
#error "MBEDTLS2_ECP_NO_FALLBACK defined, but no alternative implementation enabled"
#endif

#if defined(MBEDTLS2_HAVEGE_C) && !defined(MBEDTLS2_TIMING_C)
#error "MBEDTLS2_HAVEGE_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_HKDF_C) && !defined(MBEDTLS2_MD_C)
#error "MBEDTLS2_HKDF_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_HMAC_DRBG_C) && !defined(MBEDTLS2_MD_C)
#error "MBEDTLS2_HMAC_DRBG_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_KEY_EXCHANGE_ECDH_ECDSA_ENABLED) &&                 \
    ( !defined(MBEDTLS2_ECDH_C) || !defined(MBEDTLS2_ECDSA_C) ||          \
      !defined(MBEDTLS2_X509_CRT_PARSE_C) )
#error "MBEDTLS2_KEY_EXCHANGE_ECDH_ECDSA_ENABLED defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_KEY_EXCHANGE_ECDH_RSA_ENABLED) &&                 \
    ( !defined(MBEDTLS2_ECDH_C) || !defined(MBEDTLS2_RSA_C) ||          \
      !defined(MBEDTLS2_X509_CRT_PARSE_C) )
#error "MBEDTLS2_KEY_EXCHANGE_ECDH_RSA_ENABLED defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_KEY_EXCHANGE_DHE_PSK_ENABLED) && !defined(MBEDTLS2_DHM_C)
#error "MBEDTLS2_KEY_EXCHANGE_DHE_PSK_ENABLED defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_KEY_EXCHANGE_ECDHE_PSK_ENABLED) &&                     \
    !defined(MBEDTLS2_ECDH_C)
#error "MBEDTLS2_KEY_EXCHANGE_ECDHE_PSK_ENABLED defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_KEY_EXCHANGE_DHE_RSA_ENABLED) &&                   \
    ( !defined(MBEDTLS2_DHM_C) || !defined(MBEDTLS2_RSA_C) ||           \
      !defined(MBEDTLS2_X509_CRT_PARSE_C) || !defined(MBEDTLS2_PKCS1_V15) )
#error "MBEDTLS2_KEY_EXCHANGE_DHE_RSA_ENABLED defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_KEY_EXCHANGE_ECDHE_RSA_ENABLED) &&                 \
    ( !defined(MBEDTLS2_ECDH_C) || !defined(MBEDTLS2_RSA_C) ||          \
      !defined(MBEDTLS2_X509_CRT_PARSE_C) || !defined(MBEDTLS2_PKCS1_V15) )
#error "MBEDTLS2_KEY_EXCHANGE_ECDHE_RSA_ENABLED defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_KEY_EXCHANGE_ECDHE_ECDSA_ENABLED) &&                 \
    ( !defined(MBEDTLS2_ECDH_C) || !defined(MBEDTLS2_ECDSA_C) ||          \
      !defined(MBEDTLS2_X509_CRT_PARSE_C) )
#error "MBEDTLS2_KEY_EXCHANGE_ECDHE_ECDSA_ENABLED defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_KEY_EXCHANGE_RSA_PSK_ENABLED) &&                   \
    ( !defined(MBEDTLS2_RSA_C) || !defined(MBEDTLS2_X509_CRT_PARSE_C) || \
      !defined(MBEDTLS2_PKCS1_V15) )
#error "MBEDTLS2_KEY_EXCHANGE_RSA_PSK_ENABLED defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_KEY_EXCHANGE_RSA_ENABLED) &&                       \
    ( !defined(MBEDTLS2_RSA_C) || !defined(MBEDTLS2_X509_CRT_PARSE_C) || \
      !defined(MBEDTLS2_PKCS1_V15) )
#error "MBEDTLS2_KEY_EXCHANGE_RSA_ENABLED defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_KEY_EXCHANGE_ECJPAKE_ENABLED) &&                    \
    ( !defined(MBEDTLS2_ECJPAKE_C) || !defined(MBEDTLS2_SHA256_C) ||      \
      !defined(MBEDTLS2_ECP_DP_SECP256R1_ENABLED) )
#error "MBEDTLS2_KEY_EXCHANGE_ECJPAKE_ENABLED defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_KEY_EXCHANGE_WITH_CERT_ENABLED) &&        \
    !defined(MBEDTLS2_SSL_KEEP_PEER_CERTIFICATE) &&              \
    ( !defined(MBEDTLS2_SHA256_C) &&                             \
      !defined(MBEDTLS2_SHA512_C) &&                             \
      !defined(MBEDTLS2_SHA1_C) )
#error "!MBEDTLS2_SSL_KEEP_PEER_CERTIFICATE requires MBEDTLS2_SHA512_C, MBEDTLS2_SHA256_C or MBEDTLS2_SHA1_C"
#endif

#if defined(MBEDTLS2_MEMORY_BUFFER_ALLOC_C) &&                          \
    ( !defined(MBEDTLS2_PLATFORM_C) || !defined(MBEDTLS2_PLATFORM_MEMORY) )
#error "MBEDTLS2_MEMORY_BUFFER_ALLOC_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_MEMORY_BACKTRACE) && !defined(MBEDTLS2_MEMORY_BUFFER_ALLOC_C)
#error "MBEDTLS2_MEMORY_BACKTRACE defined, but not all prerequesites"
#endif

#if defined(MBEDTLS2_MEMORY_DEBUG) && !defined(MBEDTLS2_MEMORY_BUFFER_ALLOC_C)
#error "MBEDTLS2_MEMORY_DEBUG defined, but not all prerequesites"
#endif

#if defined(MBEDTLS2_PADLOCK_C) && !defined(MBEDTLS2_HAVE_ASM)
#error "MBEDTLS2_PADLOCK_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PEM_PARSE_C) && !defined(MBEDTLS2_BASE64_C)
#error "MBEDTLS2_PEM_PARSE_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PEM_WRITE_C) && !defined(MBEDTLS2_BASE64_C)
#error "MBEDTLS2_PEM_WRITE_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PK_C) && \
    ( !defined(MBEDTLS2_RSA_C) && !defined(MBEDTLS2_ECP_C) )
#error "MBEDTLS2_PK_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PK_PARSE_C) && !defined(MBEDTLS2_PK_C)
#error "MBEDTLS2_PK_PARSE_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PK_WRITE_C) && !defined(MBEDTLS2_PK_C)
#error "MBEDTLS2_PK_WRITE_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PKCS11_C) && !defined(MBEDTLS2_PK_C)
#error "MBEDTLS2_PKCS11_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PKCS11_C)
#if defined(MBEDTLS2_DEPRECATED_REMOVED)
#error "MBEDTLS2_PKCS11_C is deprecated and will be removed in a future version of Mbed TLS"
#elif defined(MBEDTLS2_DEPRECATED_WARNING)
#warning "MBEDTLS2_PKCS11_C is deprecated and will be removed in a future version of Mbed TLS"
#endif
#endif /* MBEDTLS2_PKCS11_C */

#if defined(MBEDTLS2_PLATFORM_EXIT_ALT) && !defined(MBEDTLS2_PLATFORM_C)
#error "MBEDTLS2_PLATFORM_EXIT_ALT defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PLATFORM_EXIT_MACRO) && !defined(MBEDTLS2_PLATFORM_C)
#error "MBEDTLS2_PLATFORM_EXIT_MACRO defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PLATFORM_EXIT_MACRO) &&\
    ( defined(MBEDTLS2_PLATFORM_STD_EXIT) ||\
        defined(MBEDTLS2_PLATFORM_EXIT_ALT) )
#error "MBEDTLS2_PLATFORM_EXIT_MACRO and MBEDTLS2_PLATFORM_STD_EXIT/MBEDTLS2_PLATFORM_EXIT_ALT cannot be defined simultaneously"
#endif

#if defined(MBEDTLS2_PLATFORM_TIME_ALT) &&\
    ( !defined(MBEDTLS2_PLATFORM_C) ||\
        !defined(MBEDTLS2_HAVE_TIME) )
#error "MBEDTLS2_PLATFORM_TIME_ALT defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PLATFORM_TIME_MACRO) &&\
    ( !defined(MBEDTLS2_PLATFORM_C) ||\
        !defined(MBEDTLS2_HAVE_TIME) )
#error "MBEDTLS2_PLATFORM_TIME_MACRO defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PLATFORM_TIME_TYPE_MACRO) &&\
    ( !defined(MBEDTLS2_PLATFORM_C) ||\
        !defined(MBEDTLS2_HAVE_TIME) )
#error "MBEDTLS2_PLATFORM_TIME_TYPE_MACRO defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PLATFORM_TIME_MACRO) &&\
    ( defined(MBEDTLS2_PLATFORM_STD_TIME) ||\
        defined(MBEDTLS2_PLATFORM_TIME_ALT) )
#error "MBEDTLS2_PLATFORM_TIME_MACRO and MBEDTLS2_PLATFORM_STD_TIME/MBEDTLS2_PLATFORM_TIME_ALT cannot be defined simultaneously"
#endif

#if defined(MBEDTLS2_PLATFORM_TIME_TYPE_MACRO) &&\
    ( defined(MBEDTLS2_PLATFORM_STD_TIME) ||\
        defined(MBEDTLS2_PLATFORM_TIME_ALT) )
#error "MBEDTLS2_PLATFORM_TIME_TYPE_MACRO and MBEDTLS2_PLATFORM_STD_TIME/MBEDTLS2_PLATFORM_TIME_ALT cannot be defined simultaneously"
#endif

#if defined(MBEDTLS2_PLATFORM_FPRINTF_ALT) && !defined(MBEDTLS2_PLATFORM_C)
#error "MBEDTLS2_PLATFORM_FPRINTF_ALT defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PLATFORM_FPRINTF_MACRO) && !defined(MBEDTLS2_PLATFORM_C)
#error "MBEDTLS2_PLATFORM_FPRINTF_MACRO defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PLATFORM_FPRINTF_MACRO) &&\
    ( defined(MBEDTLS2_PLATFORM_STD_FPRINTF) ||\
        defined(MBEDTLS2_PLATFORM_FPRINTF_ALT) )
#error "MBEDTLS2_PLATFORM_FPRINTF_MACRO and MBEDTLS2_PLATFORM_STD_FPRINTF/MBEDTLS2_PLATFORM_FPRINTF_ALT cannot be defined simultaneously"
#endif

#if defined(MBEDTLS2_PLATFORM_FREE_MACRO) &&\
    ( !defined(MBEDTLS2_PLATFORM_C) || !defined(MBEDTLS2_PLATFORM_MEMORY) )
#error "MBEDTLS2_PLATFORM_FREE_MACRO defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PLATFORM_FREE_MACRO) &&\
    defined(MBEDTLS2_PLATFORM_STD_FREE)
#error "MBEDTLS2_PLATFORM_FREE_MACRO and MBEDTLS2_PLATFORM_STD_FREE cannot be defined simultaneously"
#endif

#if defined(MBEDTLS2_PLATFORM_FREE_MACRO) && !defined(MBEDTLS2_PLATFORM_CALLOC_MACRO)
#error "MBEDTLS2_PLATFORM_CALLOC_MACRO must be defined if MBEDTLS2_PLATFORM_FREE_MACRO is"
#endif

#if defined(MBEDTLS2_PLATFORM_CALLOC_MACRO) &&\
    ( !defined(MBEDTLS2_PLATFORM_C) || !defined(MBEDTLS2_PLATFORM_MEMORY) )
#error "MBEDTLS2_PLATFORM_CALLOC_MACRO defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PLATFORM_CALLOC_MACRO) &&\
    defined(MBEDTLS2_PLATFORM_STD_CALLOC)
#error "MBEDTLS2_PLATFORM_CALLOC_MACRO and MBEDTLS2_PLATFORM_STD_CALLOC cannot be defined simultaneously"
#endif

#if defined(MBEDTLS2_PLATFORM_CALLOC_MACRO) && !defined(MBEDTLS2_PLATFORM_FREE_MACRO)
#error "MBEDTLS2_PLATFORM_FREE_MACRO must be defined if MBEDTLS2_PLATFORM_CALLOC_MACRO is"
#endif

#if defined(MBEDTLS2_PLATFORM_MEMORY) && !defined(MBEDTLS2_PLATFORM_C)
#error "MBEDTLS2_PLATFORM_MEMORY defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PLATFORM_PRINTF_ALT) && !defined(MBEDTLS2_PLATFORM_C)
#error "MBEDTLS2_PLATFORM_PRINTF_ALT defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PLATFORM_PRINTF_MACRO) && !defined(MBEDTLS2_PLATFORM_C)
#error "MBEDTLS2_PLATFORM_PRINTF_MACRO defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PLATFORM_PRINTF_MACRO) &&\
    ( defined(MBEDTLS2_PLATFORM_STD_PRINTF) ||\
        defined(MBEDTLS2_PLATFORM_PRINTF_ALT) )
#error "MBEDTLS2_PLATFORM_PRINTF_MACRO and MBEDTLS2_PLATFORM_STD_PRINTF/MBEDTLS2_PLATFORM_PRINTF_ALT cannot be defined simultaneously"
#endif

#if defined(MBEDTLS2_PLATFORM_SNPRINTF_ALT) && !defined(MBEDTLS2_PLATFORM_C)
#error "MBEDTLS2_PLATFORM_SNPRINTF_ALT defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PLATFORM_SNPRINTF_MACRO) && !defined(MBEDTLS2_PLATFORM_C)
#error "MBEDTLS2_PLATFORM_SNPRINTF_MACRO defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PLATFORM_SNPRINTF_MACRO) &&\
    ( defined(MBEDTLS2_PLATFORM_STD_SNPRINTF) ||\
        defined(MBEDTLS2_PLATFORM_SNPRINTF_ALT) )
#error "MBEDTLS2_PLATFORM_SNPRINTF_MACRO and MBEDTLS2_PLATFORM_STD_SNPRINTF/MBEDTLS2_PLATFORM_SNPRINTF_ALT cannot be defined simultaneously"
#endif

#if defined(MBEDTLS2_PLATFORM_STD_MEM_HDR) &&\
    !defined(MBEDTLS2_PLATFORM_NO_STD_FUNCTIONS)
#error "MBEDTLS2_PLATFORM_STD_MEM_HDR defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PLATFORM_STD_CALLOC) && !defined(MBEDTLS2_PLATFORM_MEMORY)
#error "MBEDTLS2_PLATFORM_STD_CALLOC defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PLATFORM_STD_FREE) && !defined(MBEDTLS2_PLATFORM_MEMORY)
#error "MBEDTLS2_PLATFORM_STD_FREE defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PLATFORM_STD_EXIT) &&\
    !defined(MBEDTLS2_PLATFORM_EXIT_ALT)
#error "MBEDTLS2_PLATFORM_STD_EXIT defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PLATFORM_STD_TIME) &&\
    ( !defined(MBEDTLS2_PLATFORM_TIME_ALT) ||\
        !defined(MBEDTLS2_HAVE_TIME) )
#error "MBEDTLS2_PLATFORM_STD_TIME defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PLATFORM_STD_FPRINTF) &&\
    !defined(MBEDTLS2_PLATFORM_FPRINTF_ALT)
#error "MBEDTLS2_PLATFORM_STD_FPRINTF defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PLATFORM_STD_PRINTF) &&\
    !defined(MBEDTLS2_PLATFORM_PRINTF_ALT)
#error "MBEDTLS2_PLATFORM_STD_PRINTF defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PLATFORM_STD_SNPRINTF) &&\
    !defined(MBEDTLS2_PLATFORM_SNPRINTF_ALT)
#error "MBEDTLS2_PLATFORM_STD_SNPRINTF defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_ENTROPY_NV_SEED) &&\
    ( !defined(MBEDTLS2_PLATFORM_C) || !defined(MBEDTLS2_ENTROPY_C) )
#error "MBEDTLS2_ENTROPY_NV_SEED defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PLATFORM_NV_SEED_ALT) &&\
    !defined(MBEDTLS2_ENTROPY_NV_SEED)
#error "MBEDTLS2_PLATFORM_NV_SEED_ALT defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PLATFORM_STD_NV_SEED_READ) &&\
    !defined(MBEDTLS2_PLATFORM_NV_SEED_ALT)
#error "MBEDTLS2_PLATFORM_STD_NV_SEED_READ defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PLATFORM_STD_NV_SEED_WRITE) &&\
    !defined(MBEDTLS2_PLATFORM_NV_SEED_ALT)
#error "MBEDTLS2_PLATFORM_STD_NV_SEED_WRITE defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PLATFORM_NV_SEED_READ_MACRO) &&\
    ( defined(MBEDTLS2_PLATFORM_STD_NV_SEED_READ) ||\
      defined(MBEDTLS2_PLATFORM_NV_SEED_ALT) )
#error "MBEDTLS2_PLATFORM_NV_SEED_READ_MACRO and MBEDTLS2_PLATFORM_STD_NV_SEED_READ cannot be defined simultaneously"
#endif

#if defined(MBEDTLS2_PLATFORM_NV_SEED_WRITE_MACRO) &&\
    ( defined(MBEDTLS2_PLATFORM_STD_NV_SEED_WRITE) ||\
      defined(MBEDTLS2_PLATFORM_NV_SEED_ALT) )
#error "MBEDTLS2_PLATFORM_NV_SEED_WRITE_MACRO and MBEDTLS2_PLATFORM_STD_NV_SEED_WRITE cannot be defined simultaneously"
#endif

#if defined(MBEDTLS2_PSA_CRYPTO_C) &&                                    \
    !( ( ( defined(MBEDTLS2_CTR_DRBG_C) || defined(MBEDTLS2_HMAC_DRBG_C) ) && \
         defined(MBEDTLS2_ENTROPY_C) ) ||                                \
       defined(MBEDTLS2_PSA_CRYPTO_EXTERNAL_RNG) )
#error "MBEDTLS2_PSA_CRYPTO_C defined, but not all prerequisites (missing RNG)"
#endif

#if defined(MBEDTLS2_PSA_CRYPTO_SPM) && !defined(MBEDTLS2_PSA_CRYPTO_C)
#error "MBEDTLS2_PSA_CRYPTO_SPM defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PSA_CRYPTO_SE_C) &&    \
    ! ( defined(MBEDTLS2_PSA_CRYPTO_C) && \
        defined(MBEDTLS2_PSA_CRYPTO_STORAGE_C) )
#error "MBEDTLS2_PSA_CRYPTO_SE_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PSA_CRYPTO_STORAGE_C) &&            \
    ! defined(MBEDTLS2_PSA_CRYPTO_C)
#error "MBEDTLS2_PSA_CRYPTO_STORAGE_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PSA_INJECT_ENTROPY) &&      \
    !( defined(MBEDTLS2_PSA_CRYPTO_STORAGE_C) && \
       defined(MBEDTLS2_ENTROPY_NV_SEED) )
#error "MBEDTLS2_PSA_INJECT_ENTROPY defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PSA_INJECT_ENTROPY) &&              \
    !defined(MBEDTLS2_NO_DEFAULT_ENTROPY_SOURCES)
#error "MBEDTLS2_PSA_INJECT_ENTROPY is not compatible with actual entropy sources"
#endif

#if defined(MBEDTLS2_PSA_INJECT_ENTROPY) &&              \
    defined(MBEDTLS2_PSA_CRYPTO_EXTERNAL_RNG)
#error "MBEDTLS2_PSA_INJECT_ENTROPY is not compatible with MBEDTLS2_PSA_CRYPTO_EXTERNAL_RNG"
#endif

#if defined(MBEDTLS2_PSA_ITS_FILE_C) && \
    !defined(MBEDTLS2_FS_IO)
#error "MBEDTLS2_PSA_ITS_FILE_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_PSA_CRYPTO_KEY_ID_ENCODES_OWNER) && \
    defined(MBEDTLS2_USE_PSA_CRYPTO)
#error "MBEDTLS2_PSA_CRYPTO_KEY_ID_ENCODES_OWNER defined, but it cannot coexist with MBEDTLS2_USE_PSA_CRYPTO."
#endif

#if defined(MBEDTLS2_RSA_C) && ( !defined(MBEDTLS2_BIGNUM_C) ||         \
    !defined(MBEDTLS2_OID_C) )
#error "MBEDTLS2_RSA_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_RSA_C) && ( !defined(MBEDTLS2_PKCS1_V21) &&         \
    !defined(MBEDTLS2_PKCS1_V15) )
#error "MBEDTLS2_RSA_C defined, but none of the PKCS1 versions enabled"
#endif

#if defined(MBEDTLS2_X509_RSASSA_PSS_SUPPORT) &&                        \
    ( !defined(MBEDTLS2_RSA_C) || !defined(MBEDTLS2_PKCS1_V21) )
#error "MBEDTLS2_X509_RSASSA_PSS_SUPPORT defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_SHA512_NO_SHA384) && !defined(MBEDTLS2_SHA512_C)
#error "MBEDTLS2_SHA512_NO_SHA384 defined without MBEDTLS2_SHA512_C"
#endif

#if defined(MBEDTLS2_SSL_PROTO_SSL3) && ( !defined(MBEDTLS2_MD5_C) ||     \
    !defined(MBEDTLS2_SHA1_C) )
#error "MBEDTLS2_SSL_PROTO_SSL3 defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_SSL_PROTO_TLS1) && ( !defined(MBEDTLS2_MD5_C) ||     \
    !defined(MBEDTLS2_SHA1_C) )
#error "MBEDTLS2_SSL_PROTO_TLS1 defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_SSL_PROTO_TLS1_1) && ( !defined(MBEDTLS2_MD5_C) ||     \
    !defined(MBEDTLS2_SHA1_C) )
#error "MBEDTLS2_SSL_PROTO_TLS1_1 defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_SSL_PROTO_TLS1_2) && ( !defined(MBEDTLS2_SHA1_C) &&     \
    !defined(MBEDTLS2_SHA256_C) && !defined(MBEDTLS2_SHA512_C) )
#error "MBEDTLS2_SSL_PROTO_TLS1_2 defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_SSL_PROTO_TLS1_3_EXPERIMENTAL) && ( !defined(MBEDTLS2_HKDF_C) && \
    !defined(MBEDTLS2_SHA256_C) && !defined(MBEDTLS2_SHA512_C) )
#error "MBEDTLS2_SSL_PROTO_TLS1_3_EXPERIMENTAL defined, but not all prerequisites"
#endif

#if (defined(MBEDTLS2_SSL_PROTO_SSL3) || defined(MBEDTLS2_SSL_PROTO_TLS1) ||  \
     defined(MBEDTLS2_SSL_PROTO_TLS1_1) || defined(MBEDTLS2_SSL_PROTO_TLS1_2)) && \
    !(defined(MBEDTLS2_KEY_EXCHANGE_RSA_ENABLED) ||                          \
      defined(MBEDTLS2_KEY_EXCHANGE_DHE_RSA_ENABLED) ||                      \
      defined(MBEDTLS2_KEY_EXCHANGE_ECDHE_RSA_ENABLED) ||                    \
      defined(MBEDTLS2_KEY_EXCHANGE_ECDHE_ECDSA_ENABLED) ||                  \
      defined(MBEDTLS2_KEY_EXCHANGE_ECDH_RSA_ENABLED) ||                     \
      defined(MBEDTLS2_KEY_EXCHANGE_ECDH_ECDSA_ENABLED) ||                   \
      defined(MBEDTLS2_KEY_EXCHANGE_PSK_ENABLED) ||                          \
      defined(MBEDTLS2_KEY_EXCHANGE_DHE_PSK_ENABLED) ||                      \
      defined(MBEDTLS2_KEY_EXCHANGE_RSA_PSK_ENABLED) ||                      \
      defined(MBEDTLS2_KEY_EXCHANGE_ECDHE_PSK_ENABLED) ||                    \
      defined(MBEDTLS2_KEY_EXCHANGE_ECJPAKE_ENABLED) )
#error "One or more versions of the TLS protocol are enabled " \
        "but no key exchange methods defined with MBEDTLS2_KEY_EXCHANGE_xxxx"
#endif

#if defined(MBEDTLS2_SSL_PROTO_DTLS)     && \
    !defined(MBEDTLS2_SSL_PROTO_TLS1_1)  && \
    !defined(MBEDTLS2_SSL_PROTO_TLS1_2)
#error "MBEDTLS2_SSL_PROTO_DTLS defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_SSL_CLI_C) && !defined(MBEDTLS2_SSL_TLS_C)
#error "MBEDTLS2_SSL_CLI_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_SSL_TLS_C) && ( !defined(MBEDTLS2_CIPHER_C) ||     \
    !defined(MBEDTLS2_MD_C) )
#error "MBEDTLS2_SSL_TLS_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_SSL_SRV_C) && !defined(MBEDTLS2_SSL_TLS_C)
#error "MBEDTLS2_SSL_SRV_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_SSL_TLS_C) && (!defined(MBEDTLS2_SSL_PROTO_SSL3) && \
    !defined(MBEDTLS2_SSL_PROTO_TLS1) && !defined(MBEDTLS2_SSL_PROTO_TLS1_1) && \
    !defined(MBEDTLS2_SSL_PROTO_TLS1_2))
#error "MBEDTLS2_SSL_TLS_C defined, but no protocols are active"
#endif

#if defined(MBEDTLS2_SSL_TLS_C) && (defined(MBEDTLS2_SSL_PROTO_SSL3) && \
    defined(MBEDTLS2_SSL_PROTO_TLS1_1) && !defined(MBEDTLS2_SSL_PROTO_TLS1))
#error "Illegal protocol selection"
#endif

#if defined(MBEDTLS2_SSL_TLS_C) && (defined(MBEDTLS2_SSL_PROTO_TLS1) && \
    defined(MBEDTLS2_SSL_PROTO_TLS1_2) && !defined(MBEDTLS2_SSL_PROTO_TLS1_1))
#error "Illegal protocol selection"
#endif

#if defined(MBEDTLS2_SSL_TLS_C) && (defined(MBEDTLS2_SSL_PROTO_SSL3) && \
    defined(MBEDTLS2_SSL_PROTO_TLS1_2) && (!defined(MBEDTLS2_SSL_PROTO_TLS1) || \
    !defined(MBEDTLS2_SSL_PROTO_TLS1_1)))
#error "Illegal protocol selection"
#endif

#if defined(MBEDTLS2_SSL_DTLS_HELLO_VERIFY) && !defined(MBEDTLS2_SSL_PROTO_DTLS)
#error "MBEDTLS2_SSL_DTLS_HELLO_VERIFY  defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_SSL_DTLS_CLIENT_PORT_REUSE) && \
    !defined(MBEDTLS2_SSL_DTLS_HELLO_VERIFY)
#error "MBEDTLS2_SSL_DTLS_CLIENT_PORT_REUSE  defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_SSL_DTLS_ANTI_REPLAY) &&                              \
    ( !defined(MBEDTLS2_SSL_TLS_C) || !defined(MBEDTLS2_SSL_PROTO_DTLS) )
#error "MBEDTLS2_SSL_DTLS_ANTI_REPLAY  defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_SSL_DTLS_CONNECTION_ID) &&                              \
    ( !defined(MBEDTLS2_SSL_TLS_C) || !defined(MBEDTLS2_SSL_PROTO_DTLS) )
#error "MBEDTLS2_SSL_DTLS_CONNECTION_ID  defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_SSL_DTLS_CONNECTION_ID)            &&                 \
    defined(MBEDTLS2_SSL_CID_IN_LEN_MAX) &&                 \
    MBEDTLS2_SSL_CID_IN_LEN_MAX > 255
#error "MBEDTLS2_SSL_CID_IN_LEN_MAX too large (max 255)"
#endif

#if defined(MBEDTLS2_SSL_DTLS_CONNECTION_ID)            &&                  \
    defined(MBEDTLS2_SSL_CID_OUT_LEN_MAX) &&                 \
    MBEDTLS2_SSL_CID_OUT_LEN_MAX > 255
#error "MBEDTLS2_SSL_CID_OUT_LEN_MAX too large (max 255)"
#endif

#if defined(MBEDTLS2_SSL_DTLS_BADMAC_LIMIT) &&                              \
    ( !defined(MBEDTLS2_SSL_TLS_C) || !defined(MBEDTLS2_SSL_PROTO_DTLS) )
#error "MBEDTLS2_SSL_DTLS_BADMAC_LIMIT  defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_SSL_ENCRYPT_THEN_MAC) &&   \
    !defined(MBEDTLS2_SSL_PROTO_TLS1)   &&      \
    !defined(MBEDTLS2_SSL_PROTO_TLS1_1) &&      \
    !defined(MBEDTLS2_SSL_PROTO_TLS1_2)
#error "MBEDTLS2_SSL_ENCRYPT_THEN_MAC defined, but not all prerequsites"
#endif

#if defined(MBEDTLS2_SSL_EXTENDED_MASTER_SECRET) && \
    !defined(MBEDTLS2_SSL_PROTO_TLS1)   &&          \
    !defined(MBEDTLS2_SSL_PROTO_TLS1_1) &&          \
    !defined(MBEDTLS2_SSL_PROTO_TLS1_2)
#error "MBEDTLS2_SSL_EXTENDED_MASTER_SECRET defined, but not all prerequsites"
#endif

#if defined(MBEDTLS2_SSL_TICKET_C) && !defined(MBEDTLS2_CIPHER_C)
#error "MBEDTLS2_SSL_TICKET_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_SSL_CBC_RECORD_SPLITTING) && \
    !defined(MBEDTLS2_SSL_PROTO_SSL3) && !defined(MBEDTLS2_SSL_PROTO_TLS1)
#error "MBEDTLS2_SSL_CBC_RECORD_SPLITTING defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_SSL_SERVER_NAME_INDICATION) && \
        !defined(MBEDTLS2_X509_CRT_PARSE_C)
#error "MBEDTLS2_SSL_SERVER_NAME_INDICATION defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_THREADING_PTHREAD)
#if !defined(MBEDTLS2_THREADING_C) || defined(MBEDTLS2_THREADING_IMPL)
#error "MBEDTLS2_THREADING_PTHREAD defined, but not all prerequisites"
#endif
#define MBEDTLS2_THREADING_IMPL
#endif

#if defined(MBEDTLS2_THREADING_ALT)
#if !defined(MBEDTLS2_THREADING_C) || defined(MBEDTLS2_THREADING_IMPL)
#error "MBEDTLS2_THREADING_ALT defined, but not all prerequisites"
#endif
#define MBEDTLS2_THREADING_IMPL
#endif

#if defined(MBEDTLS2_THREADING_C) && !defined(MBEDTLS2_THREADING_IMPL)
#error "MBEDTLS2_THREADING_C defined, single threading implementation required"
#endif
#undef MBEDTLS2_THREADING_IMPL

#if defined(MBEDTLS2_USE_PSA_CRYPTO) && !defined(MBEDTLS2_PSA_CRYPTO_C)
#error "MBEDTLS2_USE_PSA_CRYPTO defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_VERSION_FEATURES) && !defined(MBEDTLS2_VERSION_C)
#error "MBEDTLS2_VERSION_FEATURES defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_X509_USE_C) && ( !defined(MBEDTLS2_BIGNUM_C) ||  \
    !defined(MBEDTLS2_OID_C) || !defined(MBEDTLS2_ASN1_PARSE_C) ||      \
    !defined(MBEDTLS2_PK_PARSE_C) )
#error "MBEDTLS2_X509_USE_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_X509_CREATE_C) && ( !defined(MBEDTLS2_BIGNUM_C) ||  \
    !defined(MBEDTLS2_OID_C) || !defined(MBEDTLS2_ASN1_WRITE_C) ||       \
    !defined(MBEDTLS2_PK_WRITE_C) )
#error "MBEDTLS2_X509_CREATE_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_CERTS_C) && !defined(MBEDTLS2_X509_USE_C)
#error "MBEDTLS2_CERTS_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_X509_CRT_PARSE_C) && ( !defined(MBEDTLS2_X509_USE_C) )
#error "MBEDTLS2_X509_CRT_PARSE_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_X509_CRL_PARSE_C) && ( !defined(MBEDTLS2_X509_USE_C) )
#error "MBEDTLS2_X509_CRL_PARSE_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_X509_CSR_PARSE_C) && ( !defined(MBEDTLS2_X509_USE_C) )
#error "MBEDTLS2_X509_CSR_PARSE_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_X509_CRT_WRITE_C) && ( !defined(MBEDTLS2_X509_CREATE_C) )
#error "MBEDTLS2_X509_CRT_WRITE_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_X509_CSR_WRITE_C) && ( !defined(MBEDTLS2_X509_CREATE_C) )
#error "MBEDTLS2_X509_CSR_WRITE_C defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_HAVE_INT32) && defined(MBEDTLS2_HAVE_INT64)
#error "MBEDTLS2_HAVE_INT32 and MBEDTLS2_HAVE_INT64 cannot be defined simultaneously"
#endif /* MBEDTLS2_HAVE_INT32 && MBEDTLS2_HAVE_INT64 */

#if ( defined(MBEDTLS2_HAVE_INT32) || defined(MBEDTLS2_HAVE_INT64) ) && \
    defined(MBEDTLS2_HAVE_ASM)
#error "MBEDTLS2_HAVE_INT32/MBEDTLS2_HAVE_INT64 and MBEDTLS2_HAVE_ASM cannot be defined simultaneously"
#endif /* (MBEDTLS2_HAVE_INT32 || MBEDTLS2_HAVE_INT64) && MBEDTLS2_HAVE_ASM */

#if defined(MBEDTLS2_SSL_PROTO_SSL3)
#if defined(MBEDTLS2_DEPRECATED_REMOVED)
#error "MBEDTLS2_SSL_PROTO_SSL3 is deprecated and will be removed in a future version of Mbed TLS"
#elif defined(MBEDTLS2_DEPRECATED_WARNING)
#warning "MBEDTLS2_SSL_PROTO_SSL3 is deprecated and will be removed in a future version of Mbed TLS"
#endif
#endif /* MBEDTLS2_SSL_PROTO_SSL3 */

#if defined(MBEDTLS2_SSL_SRV_SUPPORT_SSLV2_CLIENT_HELLO)
#if defined(MBEDTLS2_DEPRECATED_REMOVED)
#error "MBEDTLS2_SSL_SRV_SUPPORT_SSLV2_CLIENT_HELLO is deprecated and will be removed in a future version of Mbed TLS"
#elif defined(MBEDTLS2_DEPRECATED_WARNING)
#warning "MBEDTLS2_SSL_SRV_SUPPORT_SSLV2_CLIENT_HELLO is deprecated and will be removed in a future version of Mbed TLS"
#endif
#endif /* MBEDTLS2_SSL_SRV_SUPPORT_SSLV2_CLIENT_HELLO */

#if defined(MBEDTLS2_SSL_HW_RECORD_ACCEL)
#if defined(MBEDTLS2_DEPRECATED_REMOVED)
#error "MBEDTLS2_SSL_HW_RECORD_ACCEL is deprecated and will be removed in a future version of Mbed TLS"
#elif defined(MBEDTLS2_DEPRECATED_WARNING)
#warning "MBEDTLS2_SSL_HW_RECORD_ACCEL is deprecated and will be removed in a future version of Mbed TLS"
#endif /* MBEDTLS2_DEPRECATED_REMOVED */
#endif /* MBEDTLS2_SSL_HW_RECORD_ACCEL */

#if defined(MBEDTLS2_SSL_DTLS_SRTP) && ( !defined(MBEDTLS2_SSL_PROTO_DTLS) )
#error "MBEDTLS2_SSL_DTLS_SRTP defined, but not all prerequisites"
#endif

#if defined(MBEDTLS2_SSL_VARIABLE_BUFFER_LENGTH) && ( !defined(MBEDTLS2_SSL_MAX_FRAGMENT_LENGTH) )
#error "MBEDTLS2_SSL_VARIABLE_BUFFER_LENGTH defined, but not all prerequisites"
#endif

/*
 * Avoid warning from -pedantic. This is a convenient place for this
 * workaround since this is included by every single file before the
 * #if defined(MBEDTLS2_xxx_C) that results in empty translation units.
 */
typedef int mbedtls2_iso_c_forbids_empty_translation_units;

#endif /* MBEDTLS2_CHECK_CONFIG_H */

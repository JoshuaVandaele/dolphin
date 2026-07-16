/**
 * \file md_internal.h
 *
 * \brief Message digest wrappers.
 *
 * \warning This in an internal header. Do not include directly.
 *
 * \author Adriaan de Jong <dejong@fox-it.com>
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
#ifndef MBEDTLS2_MD_WRAP_H
#define MBEDTLS2_MD_WRAP_H

#if !defined(MBEDTLS2_CONFIG_FILE)
#include "mbedtls2/config.h"
#else
#include MBEDTLS2_CONFIG_FILE
#endif

#include "mbedtls2/md.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Message digest information.
 * Allows message digest functions to be called in a generic way.
 */
struct mbedtls2_md_info_t {
  /** Name of the message digest */
  const char *name;

  /** Digest identifier */
  mbedtls2_md_type_t type;

  /** Output length of the digest function in bytes */
  unsigned char size;

  /** Block length of the digest function in bytes */
  unsigned char block_size;
};

#if defined(MBEDTLS2_MD2_C)
extern const mbedtls2_md_info_t mbedtls2_md2_info;
#endif
#if defined(MBEDTLS2_MD4_C)
extern const mbedtls2_md_info_t mbedtls2_md4_info;
#endif
#if defined(MBEDTLS2_MD5_C)
extern const mbedtls2_md_info_t mbedtls2_md5_info;
#endif
#if defined(MBEDTLS2_RIPEMD160_C)
extern const mbedtls2_md_info_t mbedtls2_ripemd160_info;
#endif
#if defined(MBEDTLS2_SHA1_C)
extern const mbedtls2_md_info_t mbedtls2_sha1_info;
#endif
#if defined(MBEDTLS2_SHA256_C)
extern const mbedtls2_md_info_t mbedtls2_sha224_info;
extern const mbedtls2_md_info_t mbedtls2_sha256_info;
#endif
#if defined(MBEDTLS2_SHA512_C)
#if !defined(MBEDTLS2_SHA512_NO_SHA384)
extern const mbedtls2_md_info_t mbedtls2_sha384_info;
#endif
extern const mbedtls2_md_info_t mbedtls2_sha512_info;
#endif

#ifdef __cplusplus
}
#endif

#endif /* MBEDTLS2_MD_WRAP_H */

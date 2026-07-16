/**
 * \file padlock.h
 *
 * \brief VIA PadLock ACE for HW encryption/decryption supported by some
 *        processors
 *
 * \warning These functions are only for internal use by other library
 *          functions; you must not call them directly.
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
#ifndef MBEDTLS2_PADLOCK_H
#define MBEDTLS2_PADLOCK_H

#if !defined(MBEDTLS2_CONFIG_FILE)
#include "mbedtls2/config.h"
#else
#include MBEDTLS2_CONFIG_FILE
#endif

#include "mbedtls2/aes.h"

/** Input data should be aligned. */
#define MBEDTLS2_ERR_PADLOCK_DATA_MISALIGNED -0x0030

#if defined(__has_feature)
#if __has_feature(address_sanitizer)
#define MBEDTLS2_HAVE_ASAN
#endif
#endif

/* Some versions of ASan result in errors about not enough registers */
#if defined(MBEDTLS2_HAVE_ASM) && defined(__GNUC__) &&                  \
    defined(__i386__) && !defined(MBEDTLS2_HAVE_ASAN)

#ifndef MBEDTLS2_HAVE_X86
#define MBEDTLS2_HAVE_X86
#endif

#include <stdint.h>

#define MBEDTLS2_PADLOCK_RNG 0x000C
#define MBEDTLS2_PADLOCK_ACE 0x00C0
#define MBEDTLS2_PADLOCK_PHE 0x0C00
#define MBEDTLS2_PADLOCK_PMM 0x3000

#define MBEDTLS2_PADLOCK_ALIGN16(x)                                     \
  (uint32_t *)(16 + ((int32_t)(x) & ~15))

#ifdef __cplusplus
extern "C" {
#endif

/**
 * \brief          Internal PadLock detection routine
 *
 * \note           This function is only for internal use by other library
 *                 functions; you must not call it directly.
 *
 * \param feature  The feature to detect
 *
 * \return         non-zero if CPU has support for the feature, 0 otherwise
 */
int mbedtls2_padlock_has_support(int feature);

/**
 * \brief          Internal PadLock AES-ECB block en(de)cryption
 *
 * \note           This function is only for internal use by other library
 *                 functions; you must not call it directly.
 *
 * \param ctx      AES context
 * \param mode     MBEDTLS2_AES_ENCRYPT or MBEDTLS2_AES_DECRYPT
 * \param input    16-byte input block
 * \param output   16-byte output block
 *
 * \return         0 if success, 1 if operation failed
 */
int mbedtls2_padlock_xcryptecb(mbedtls2_aes_context *ctx,
                                      int mode, const unsigned char input[16],
                                      unsigned char output[16]);

/**
 * \brief          Internal PadLock AES-CBC buffer en(de)cryption
 *
 * \note           This function is only for internal use by other library
 *                 functions; you must not call it directly.
 *
 * \param ctx      AES context
 * \param mode     MBEDTLS2_AES_ENCRYPT or MBEDTLS2_AES_DECRYPT
 * \param length   length of the input data
 * \param iv       initialization vector (updated after use)
 * \param input    buffer holding the input data
 * \param output   buffer holding the output data
 *
 * \return         0 if success, 1 if operation failed
 */
int mbedtls2_padlock_xcryptcbc(mbedtls2_aes_context *ctx,
                                      int mode, size_t length,
                                      unsigned char iv[16],
                                      const unsigned char *input,
                                      unsigned char *output);

#ifdef __cplusplus
}
#endif

#endif /* HAVE_X86  */

#endif /* padlock.h */

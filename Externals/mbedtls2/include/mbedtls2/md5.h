/**
 * \file md5.h
 *
 * \brief MD5 message digest algorithm (hash function)
 *
 * \warning   MD5 is considered a weak message digest and its use constitutes a
 *            security risk. We recommend considering stronger message
 *            digests instead.
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
#ifndef MBEDTLS2_MD5_H
#define MBEDTLS2_MD5_H

#if !defined(MBEDTLS2_CONFIG_FILE)
#include "mbedtls2/config.h"
#else
#include MBEDTLS2_CONFIG_FILE
#endif

#include <stddef.h>
#include <stdint.h>

/* MBEDTLS2_ERR_MD5_HW_ACCEL_FAILED is deprecated and should not be used.
 */
/** MD5 hardware accelerator failed */
#define MBEDTLS2_ERR_MD5_HW_ACCEL_FAILED -0x002F

#ifdef __cplusplus
extern "C" {
#endif

#if !defined(MBEDTLS2_MD5_ALT)
// Regular implementation
//

/**
 * \brief          MD5 context structure
 *
 * \warning        MD5 is considered a weak message digest and its use
 *                 constitutes a security risk. We recommend considering
 *                 stronger message digests instead.
 *
 */
typedef struct mbedtls2_md5_context {
  uint32_t total[2];        /*!< number of bytes processed  */
  uint32_t state[4];        /*!< intermediate digest state  */
  unsigned char buffer[64]; /*!< data block being processed */
} mbedtls2_md5_context;

#else /* MBEDTLS2_MD5_ALT */
#include "md5_alt.h"
#endif /* MBEDTLS2_MD5_ALT */

/**
 * \brief          Initialize MD5 context
 *
 * \param ctx      MD5 context to be initialized
 *
 * \warning        MD5 is considered a weak message digest and its use
 *                 constitutes a security risk. We recommend considering
 *                 stronger message digests instead.
 *
 */
void mbedtls2_md5_init(mbedtls2_md5_context *ctx);

/**
 * \brief          Clear MD5 context
 *
 * \param ctx      MD5 context to be cleared
 *
 * \warning        MD5 is considered a weak message digest and its use
 *                 constitutes a security risk. We recommend considering
 *                 stronger message digests instead.
 *
 */
void mbedtls2_md5_free(mbedtls2_md5_context *ctx);

/**
 * \brief          Clone (the state of) an MD5 context
 *
 * \param dst      The destination context
 * \param src      The context to be cloned
 *
 * \warning        MD5 is considered a weak message digest and its use
 *                 constitutes a security risk. We recommend considering
 *                 stronger message digests instead.
 *
 */
void mbedtls2_md5_clone(mbedtls2_md5_context *dst,
                               const mbedtls2_md5_context *src);

/**
 * \brief          MD5 context setup
 *
 * \param ctx      context to be initialized
 *
 * \return         0 if successful
 *
 * \warning        MD5 is considered a weak message digest and its use
 *                 constitutes a security risk. We recommend considering
 *                 stronger message digests instead.
 *
 */
int mbedtls2_md5_starts_ret(mbedtls2_md5_context *ctx);

/**
 * \brief          MD5 process buffer
 *
 * \param ctx      MD5 context
 * \param input    buffer holding the data
 * \param ilen     length of the input data
 *
 * \return         0 if successful
 *
 * \warning        MD5 is considered a weak message digest and its use
 *                 constitutes a security risk. We recommend considering
 *                 stronger message digests instead.
 *
 */
int mbedtls2_md5_update_ret(mbedtls2_md5_context *ctx,
                                   const unsigned char *input, size_t ilen);

/**
 * \brief          MD5 final digest
 *
 * \param ctx      MD5 context
 * \param output   MD5 checksum result
 *
 * \return         0 if successful
 *
 * \warning        MD5 is considered a weak message digest and its use
 *                 constitutes a security risk. We recommend considering
 *                 stronger message digests instead.
 *
 */
int mbedtls2_md5_finish_ret(mbedtls2_md5_context *ctx,
                                   unsigned char output[16]);

/**
 * \brief          MD5 process data block (internal use only)
 *
 * \param ctx      MD5 context
 * \param data     buffer holding one block of data
 *
 * \return         0 if successful
 *
 * \warning        MD5 is considered a weak message digest and its use
 *                 constitutes a security risk. We recommend considering
 *                 stronger message digests instead.
 *
 */
int mbedtls2_internal_md5_process(mbedtls2_md5_context *ctx,
                                         const unsigned char data[64]);

#if !defined(MBEDTLS2_DEPRECATED_REMOVED)
#if defined(MBEDTLS2_DEPRECATED_WARNING)
#define MBEDTLS2_DEPRECATED __attribute__((deprecated))
#else
#define MBEDTLS2_DEPRECATED
#endif
/**
 * \brief          MD5 context setup
 *
 * \deprecated     Superseded by mbedtls2_md5_starts_ret() in 2.7.0
 *
 * \param ctx      context to be initialized
 *
 * \warning        MD5 is considered a weak message digest and its use
 *                 constitutes a security risk. We recommend considering
 *                 stronger message digests instead.
 *
 */
MBEDTLS2_DEPRECATED void
mbedtls2_md5_starts(mbedtls2_md5_context *ctx);

/**
 * \brief          MD5 process buffer
 *
 * \deprecated     Superseded by mbedtls2_md5_update_ret() in 2.7.0
 *
 * \param ctx      MD5 context
 * \param input    buffer holding the data
 * \param ilen     length of the input data
 *
 * \warning        MD5 is considered a weak message digest and its use
 *                 constitutes a security risk. We recommend considering
 *                 stronger message digests instead.
 *
 */
MBEDTLS2_DEPRECATED void
mbedtls2_md5_update(mbedtls2_md5_context *ctx,
                           const unsigned char *input, size_t ilen);

/**
 * \brief          MD5 final digest
 *
 * \deprecated     Superseded by mbedtls2_md5_finish_ret() in 2.7.0
 *
 * \param ctx      MD5 context
 * \param output   MD5 checksum result
 *
 * \warning        MD5 is considered a weak message digest and its use
 *                 constitutes a security risk. We recommend considering
 *                 stronger message digests instead.
 *
 */
MBEDTLS2_DEPRECATED void
mbedtls2_md5_finish(mbedtls2_md5_context *ctx,
                           unsigned char output[16]);

/**
 * \brief          MD5 process data block (internal use only)
 *
 * \deprecated     Superseded by mbedtls2_internal_md5_process() in 2.7.0
 *
 * \param ctx      MD5 context
 * \param data     buffer holding one block of data
 *
 * \warning        MD5 is considered a weak message digest and its use
 *                 constitutes a security risk. We recommend considering
 *                 stronger message digests instead.
 *
 */
MBEDTLS2_DEPRECATED void
mbedtls2_md5_process(mbedtls2_md5_context *ctx,
                            const unsigned char data[64]);

#undef MBEDTLS2_DEPRECATED
#endif /* !MBEDTLS2_DEPRECATED_REMOVED */

/**
 * \brief          Output = MD5( input buffer )
 *
 * \param input    buffer holding the data
 * \param ilen     length of the input data
 * \param output   MD5 checksum result
 *
 * \return         0 if successful
 *
 * \warning        MD5 is considered a weak message digest and its use
 *                 constitutes a security risk. We recommend considering
 *                 stronger message digests instead.
 *
 */
int mbedtls2_md5_ret(const unsigned char *input, size_t ilen,
                            unsigned char output[16]);

#if !defined(MBEDTLS2_DEPRECATED_REMOVED)
#if defined(MBEDTLS2_DEPRECATED_WARNING)
#define MBEDTLS2_DEPRECATED __attribute__((deprecated))
#else
#define MBEDTLS2_DEPRECATED
#endif
/**
 * \brief          Output = MD5( input buffer )
 *
 * \deprecated     Superseded by mbedtls2_md5_ret() in 2.7.0
 *
 * \param input    buffer holding the data
 * \param ilen     length of the input data
 * \param output   MD5 checksum result
 *
 * \warning        MD5 is considered a weak message digest and its use
 *                 constitutes a security risk. We recommend considering
 *                 stronger message digests instead.
 *
 */
MBEDTLS2_DEPRECATED void mbedtls2_md5(const unsigned char *input,
                                                    size_t ilen,
                                                    unsigned char output[16]);

#undef MBEDTLS2_DEPRECATED
#endif /* !MBEDTLS2_DEPRECATED_REMOVED */

#if defined(MBEDTLS2_SELF_TEST)

/**
 * \brief          Checkup routine
 *
 * \return         0 if successful, or 1 if the test failed
 *
 * \warning        MD5 is considered a weak message digest and its use
 *                 constitutes a security risk. We recommend considering
 *                 stronger message digests instead.
 *
 */
int mbedtls2_md5_self_test(int verbose);

#endif /* MBEDTLS2_SELF_TEST */

#ifdef __cplusplus
}
#endif

#endif /* mbedtls2_md5.h */

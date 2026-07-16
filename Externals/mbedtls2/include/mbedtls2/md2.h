/**
 * \file md2.h
 *
 * \brief MD2 message digest algorithm (hash function)
 *
 * \warning MD2 is considered a weak message digest and its use constitutes a
 *          security risk. We recommend considering stronger message digests
 *          instead.
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
 *
 */
#ifndef MBEDTLS2_MD2_H
#define MBEDTLS2_MD2_H

#if !defined(MBEDTLS2_CONFIG_FILE)
#include "mbedtls2/config.h"
#else
#include MBEDTLS2_CONFIG_FILE
#endif

#include <stddef.h>

/* MBEDTLS2_ERR_MD2_HW_ACCEL_FAILED is deprecated and should not be used.
 */
/** MD2 hardware accelerator failed */
#define MBEDTLS2_ERR_MD2_HW_ACCEL_FAILED -0x002B

#ifdef __cplusplus
extern "C" {
#endif

#if !defined(MBEDTLS2_MD2_ALT)
// Regular implementation
//

/**
 * \brief          MD2 context structure
 *
 * \warning        MD2 is considered a weak message digest and its use
 *                 constitutes a security risk. We recommend considering
 *                 stronger message digests instead.
 *
 */
typedef struct mbedtls2_md2_context {
  unsigned char cksum[16];  /*!< checksum of the data block */
  unsigned char state[48];  /*!< intermediate digest state  */
  unsigned char buffer[16]; /*!< data block being processed */
  size_t left;              /*!< amount of data in buffer   */
} mbedtls2_md2_context;

#else /* MBEDTLS2_MD2_ALT */
#include "md2_alt.h"
#endif /* MBEDTLS2_MD2_ALT */

/**
 * \brief          Initialize MD2 context
 *
 * \param ctx      MD2 context to be initialized
 *
 * \warning        MD2 is considered a weak message digest and its use
 *                 constitutes a security risk. We recommend considering
 *                 stronger message digests instead.
 *
 */
void mbedtls2_md2_init(mbedtls2_md2_context *ctx);

/**
 * \brief          Clear MD2 context
 *
 * \param ctx      MD2 context to be cleared
 *
 * \warning        MD2 is considered a weak message digest and its use
 *                 constitutes a security risk. We recommend considering
 *                 stronger message digests instead.
 *
 */
void mbedtls2_md2_free(mbedtls2_md2_context *ctx);

/**
 * \brief          Clone (the state of) an MD2 context
 *
 * \param dst      The destination context
 * \param src      The context to be cloned
 *
 * \warning        MD2 is considered a weak message digest and its use
 *                 constitutes a security risk. We recommend considering
 *                 stronger message digests instead.
 *
 */
void mbedtls2_md2_clone(mbedtls2_md2_context *dst,
                               const mbedtls2_md2_context *src);

/**
 * \brief          MD2 context setup
 *
 * \param ctx      context to be initialized
 *
 * \return         0 if successful
 *
 * \warning        MD2 is considered a weak message digest and its use
 *                 constitutes a security risk. We recommend considering
 *                 stronger message digests instead.
 *
 */
int mbedtls2_md2_starts_ret(mbedtls2_md2_context *ctx);

/**
 * \brief          MD2 process buffer
 *
 * \param ctx      MD2 context
 * \param input    buffer holding the data
 * \param ilen     length of the input data
 *
 * \return         0 if successful
 *
 * \warning        MD2 is considered a weak message digest and its use
 *                 constitutes a security risk. We recommend considering
 *                 stronger message digests instead.
 *
 */
int mbedtls2_md2_update_ret(mbedtls2_md2_context *ctx,
                                   const unsigned char *input, size_t ilen);

/**
 * \brief          MD2 final digest
 *
 * \param ctx      MD2 context
 * \param output   MD2 checksum result
 *
 * \return         0 if successful
 *
 * \warning        MD2 is considered a weak message digest and its use
 *                 constitutes a security risk. We recommend considering
 *                 stronger message digests instead.
 *
 */
int mbedtls2_md2_finish_ret(mbedtls2_md2_context *ctx,
                                   unsigned char output[16]);

/**
 * \brief          MD2 process data block (internal use only)
 *
 * \param ctx      MD2 context
 *
 * \return         0 if successful
 *
 * \warning        MD2 is considered a weak message digest and its use
 *                 constitutes a security risk. We recommend considering
 *                 stronger message digests instead.
 *
 */
int mbedtls2_internal_md2_process(mbedtls2_md2_context *ctx);

#if !defined(MBEDTLS2_DEPRECATED_REMOVED)
#if defined(MBEDTLS2_DEPRECATED_WARNING)
#define MBEDTLS2_DEPRECATED __attribute__((deprecated))
#else
#define MBEDTLS2_DEPRECATED
#endif
/**
 * \brief          MD2 context setup
 *
 * \deprecated     Superseded by mbedtls2_md2_starts_ret() in 2.7.0
 *
 * \param ctx      context to be initialized
 *
 * \warning        MD2 is considered a weak message digest and its use
 *                 constitutes a security risk. We recommend considering
 *                 stronger message digests instead.
 *
 */
MBEDTLS2_DEPRECATED void
mbedtls2_md2_starts(mbedtls2_md2_context *ctx);

/**
 * \brief          MD2 process buffer
 *
 * \deprecated     Superseded by mbedtls2_md2_update_ret() in 2.7.0
 *
 * \param ctx      MD2 context
 * \param input    buffer holding the data
 * \param ilen     length of the input data
 *
 * \warning        MD2 is considered a weak message digest and its use
 *                 constitutes a security risk. We recommend considering
 *                 stronger message digests instead.
 *
 */
MBEDTLS2_DEPRECATED void
mbedtls2_md2_update(mbedtls2_md2_context *ctx,
                           const unsigned char *input, size_t ilen);

/**
 * \brief          MD2 final digest
 *
 * \deprecated     Superseded by mbedtls2_md2_finish_ret() in 2.7.0
 *
 * \param ctx      MD2 context
 * \param output   MD2 checksum result
 *
 * \warning        MD2 is considered a weak message digest and its use
 *                 constitutes a security risk. We recommend considering
 *                 stronger message digests instead.
 *
 */
MBEDTLS2_DEPRECATED void
mbedtls2_md2_finish(mbedtls2_md2_context *ctx,
                           unsigned char output[16]);

/**
 * \brief          MD2 process data block (internal use only)
 *
 * \deprecated     Superseded by mbedtls2_internal_md2_process() in 2.7.0
 *
 * \param ctx      MD2 context
 *
 * \warning        MD2 is considered a weak message digest and its use
 *                 constitutes a security risk. We recommend considering
 *                 stronger message digests instead.
 *
 */
MBEDTLS2_DEPRECATED void
mbedtls2_md2_process(mbedtls2_md2_context *ctx);

#undef MBEDTLS2_DEPRECATED
#endif /* !MBEDTLS2_DEPRECATED_REMOVED */

/**
 * \brief          Output = MD2( input buffer )
 *
 * \param input    buffer holding the data
 * \param ilen     length of the input data
 * \param output   MD2 checksum result
 *
 * \warning        MD2 is considered a weak message digest and its use
 *                 constitutes a security risk. We recommend considering
 *                 stronger message digests instead.
 *
 */
int mbedtls2_md2_ret(const unsigned char *input, size_t ilen,
                            unsigned char output[16]);

#if !defined(MBEDTLS2_DEPRECATED_REMOVED)
#if defined(MBEDTLS2_DEPRECATED_WARNING)
#define MBEDTLS2_DEPRECATED __attribute__((deprecated))
#else
#define MBEDTLS2_DEPRECATED
#endif
/**
 * \brief          Output = MD2( input buffer )
 *
 * \deprecated     Superseded by mbedtls2_md2_ret() in 2.7.0
 *
 * \param input    buffer holding the data
 * \param ilen     length of the input data
 * \param output   MD2 checksum result
 *
 * \warning        MD2 is considered a weak message digest and its use
 *                 constitutes a security risk. We recommend considering
 *                 stronger message digests instead.
 *
 */
MBEDTLS2_DEPRECATED void mbedtls2_md2(const unsigned char *input,
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
 * \warning        MD2 is considered a weak message digest and its use
 *                 constitutes a security risk. We recommend considering
 *                 stronger message digests instead.
 *
 */
int mbedtls2_md2_self_test(int verbose);

#endif /* MBEDTLS2_SELF_TEST */

#ifdef __cplusplus
}
#endif

#endif /* mbedtls2_md2.h */

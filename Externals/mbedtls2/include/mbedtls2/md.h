/**
 * \file md.h
 *
 * \brief This file contains the generic message-digest wrapper.
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

#ifndef MBEDTLS2_MD_H
#define MBEDTLS2_MD_H

#include <stddef.h>

#if !defined(MBEDTLS2_CONFIG_FILE)
#include "mbedtls2/config.h"
#else
#include MBEDTLS2_CONFIG_FILE
#endif
#include "mbedtls2/platform_util.h"

/** The selected feature is not available. */
#define MBEDTLS2_ERR_MD_FEATURE_UNAVAILABLE -0x5080
/** Bad input parameters to function. */
#define MBEDTLS2_ERR_MD_BAD_INPUT_DATA -0x5100
/** Failed to allocate memory. */
#define MBEDTLS2_ERR_MD_ALLOC_FAILED -0x5180
/** Opening or reading of file failed. */
#define MBEDTLS2_ERR_MD_FILE_IO_ERROR -0x5200

/* MBEDTLS2_ERR_MD_HW_ACCEL_FAILED is deprecated and should not be used.
 */
/** MD hardware accelerator failed. */
#define MBEDTLS2_ERR_MD_HW_ACCEL_FAILED -0x5280

#ifdef __cplusplus
extern "C" {
#endif

/**
 * \brief     Supported message digests.
 *
 * \warning   MD2, MD4, MD5 and SHA-1 are considered weak message digests and
 *            their use constitutes a security risk. We recommend considering
 *            stronger message digests instead.
 *
 */
typedef enum {
  MBEDTLS2_MD_NONE = 0,  /**< None. */
  MBEDTLS2_MD_MD2,       /**< The MD2 message digest. */
  MBEDTLS2_MD_MD4,       /**< The MD4 message digest. */
  MBEDTLS2_MD_MD5,       /**< The MD5 message digest. */
  MBEDTLS2_MD_SHA1,      /**< The SHA-1 message digest. */
  MBEDTLS2_MD_SHA224,    /**< The SHA-224 message digest. */
  MBEDTLS2_MD_SHA256,    /**< The SHA-256 message digest. */
  MBEDTLS2_MD_SHA384,    /**< The SHA-384 message digest. */
  MBEDTLS2_MD_SHA512,    /**< The SHA-512 message digest. */
  MBEDTLS2_MD_RIPEMD160, /**< The RIPEMD-160 message digest. */
} mbedtls2_md_type_t;

#if defined(MBEDTLS2_SHA512_C)
#define MBEDTLS2_MD_MAX_SIZE 64 /* longest known is SHA512 */
#else
#define MBEDTLS2_MD_MAX_SIZE 32 /* longest known is SHA256 or less */
#endif

#if defined(MBEDTLS2_SHA512_C)
#define MBEDTLS2_MD_MAX_BLOCK_SIZE 128
#else
#define MBEDTLS2_MD_MAX_BLOCK_SIZE 64
#endif

/**
 * Opaque struct defined in md_internal.h.
 */
typedef struct mbedtls2_md_info_t mbedtls2_md_info_t;

/**
 * The generic message-digest context.
 */
typedef struct mbedtls2_md_context_t {
  /** Information about the associated message digest. */
  const mbedtls2_md_info_t *md_info;

  /** The digest-specific context. */
  void *md_ctx;

  /** The HMAC part of the context. */
  void *hmac_ctx;
} mbedtls2_md_context_t;

/**
 * \brief           This function returns the list of digests supported by the
 *                  generic digest module.
 *
 * \note            The list starts with the strongest available hashes.
 *
 * \return          A statically allocated array of digests. Each element
 *                  in the returned list is an integer belonging to the
 *                  message-digest enumeration #mbedtls2_md_type_t.
 *                  The last entry is 0.
 */
const int *mbedtls2_md_list(void);

/**
 * \brief           This function returns the message-digest information
 *                  associated with the given digest name.
 *
 * \param md_name   The name of the digest to search for.
 *
 * \return          The message-digest information associated with \p md_name.
 * \return          NULL if the associated message-digest information is not
 * found.
 */
const mbedtls2_md_info_t *
mbedtls2_md_info_from_string(const char *md_name);

/**
 * \brief           This function returns the message-digest information
 *                  associated with the given digest type.
 *
 * \param md_type   The type of digest to search for.
 *
 * \return          The message-digest information associated with \p md_type.
 * \return          NULL if the associated message-digest information is not
 * found.
 */
const mbedtls2_md_info_t *
mbedtls2_md_info_from_type(mbedtls2_md_type_t md_type);

/**
 * \brief           This function initializes a message-digest context without
 *                  binding it to a particular message-digest algorithm.
 *
 *                  This function should always be called first. It prepares the
 *                  context for mbedtls2_md_setup() for binding it to a
 *                  message-digest algorithm.
 */
void mbedtls2_md_init(mbedtls2_md_context_t *ctx);

/**
 * \brief           This function clears the internal structure of \p ctx and
 *                  frees any embedded internal structure, but does not free
 *                  \p ctx itself.
 *
 *                  If you have called mbedtls2_md_setup() on \p ctx, you
 * must call mbedtls2_md_free() when you are no longer using the context.
 *                  Calling this function if you have previously
 *                  called mbedtls2_md_init() and nothing else is
 * optional. You must not call this function if you have not called
 *                  mbedtls2_md_init().
 */
void mbedtls2_md_free(mbedtls2_md_context_t *ctx);

#if !defined(MBEDTLS2_DEPRECATED_REMOVED)
#if defined(MBEDTLS2_DEPRECATED_WARNING)
#define MBEDTLS2_DEPRECATED __attribute__((deprecated))
#else
#define MBEDTLS2_DEPRECATED
#endif
/**
 * \brief           This function selects the message digest algorithm to use,
 *                  and allocates internal structures.
 *
 *                  It should be called after mbedtls2_md_init() or
 * mbedtls2_md_free(). Makes it necessary to call
 * mbedtls2_md_free() later.
 *
 * \deprecated      Superseded by mbedtls2_md_setup() in 2.0.0
 *
 * \param ctx       The context to set up.
 * \param md_info   The information structure of the message-digest algorithm
 *                  to use.
 *
 * \return          \c 0 on success.
 * \return          #MBEDTLS2_ERR_MD_BAD_INPUT_DATA on
 * parameter-verification failure.
 * \return          #MBEDTLS2_ERR_MD_ALLOC_FAILED on memory-allocation
 * failure.
 */
int mbedtls2_md_init_ctx(mbedtls2_md_context_t *ctx,
                                const mbedtls2_md_info_t *md_info)
    MBEDTLS2_DEPRECATED;
#undef MBEDTLS2_DEPRECATED
#endif /* MBEDTLS2_DEPRECATED_REMOVED */

/**
 * \brief           This function selects the message digest algorithm to use,
 *                  and allocates internal structures.
 *
 *                  It should be called after mbedtls2_md_init() or
 *                  mbedtls2_md_free(). Makes it necessary to call
 *                  mbedtls2_md_free() later.
 *
 * \param ctx       The context to set up.
 * \param md_info   The information structure of the message-digest algorithm
 *                  to use.
 * \param hmac      Defines if HMAC is used. 0: HMAC is not used (saves some
 * memory), or non-zero: HMAC is used with this context.
 *
 * \return          \c 0 on success.
 * \return          #MBEDTLS2_ERR_MD_BAD_INPUT_DATA on
 * parameter-verification failure.
 * \return          #MBEDTLS2_ERR_MD_ALLOC_FAILED on memory-allocation
 * failure.
 */
MBEDTLS2_CHECK_RETURN_TYPICAL
int mbedtls2_md_setup(mbedtls2_md_context_t *ctx,
                             const mbedtls2_md_info_t *md_info,
                             int hmac);

/**
 * \brief           This function clones the state of an message-digest
 *                  context.
 *
 * \note            You must call mbedtls2_md_setup() on \c dst before
 * calling this function.
 *
 * \note            The two contexts must have the same type,
 *                  for example, both are SHA-256.
 *
 * \warning         This function clones the message-digest state, not the
 *                  HMAC state.
 *
 * \param dst       The destination context.
 * \param src       The context to be cloned.
 *
 * \return          \c 0 on success.
 * \return          #MBEDTLS2_ERR_MD_BAD_INPUT_DATA on
 * parameter-verification failure.
 */
MBEDTLS2_CHECK_RETURN_TYPICAL
int mbedtls2_md_clone(mbedtls2_md_context_t *dst,
                             const mbedtls2_md_context_t *src);

/**
 * \brief           This function extracts the message-digest size from the
 *                  message-digest information structure.
 *
 * \param md_info   The information structure of the message-digest algorithm
 *                  to use.
 *
 * \return          The size of the message-digest output in Bytes.
 */
unsigned char
mbedtls2_md_get_size(const mbedtls2_md_info_t *md_info);

/**
 * \brief           This function extracts the message-digest type from the
 *                  message-digest information structure.
 *
 * \param md_info   The information structure of the message-digest algorithm
 *                  to use.
 *
 * \return          The type of the message digest.
 */
mbedtls2_md_type_t
mbedtls2_md_get_type(const mbedtls2_md_info_t *md_info);

/**
 * \brief           This function extracts the message-digest name from the
 *                  message-digest information structure.
 *
 * \param md_info   The information structure of the message-digest algorithm
 *                  to use.
 *
 * \return          The name of the message digest.
 */
const char *
mbedtls2_md_get_name(const mbedtls2_md_info_t *md_info);

/**
 * \brief           This function starts a message-digest computation.
 *
 *                  You must call this function after setting up the context
 *                  with mbedtls2_md_setup(), and before passing data
 * with mbedtls2_md_update().
 *
 * \param ctx       The generic message-digest context.
 *
 * \return          \c 0 on success.
 * \return          #MBEDTLS2_ERR_MD_BAD_INPUT_DATA on
 * parameter-verification failure.
 */
MBEDTLS2_CHECK_RETURN_TYPICAL
int mbedtls2_md_starts(mbedtls2_md_context_t *ctx);

/**
 * \brief           This function feeds an input buffer into an ongoing
 *                  message-digest computation.
 *
 *                  You must call mbedtls2_md_starts() before calling
 * this function. You may call this function multiple times. Afterwards, call
 * mbedtls2_md_finish().
 *
 * \param ctx       The generic message-digest context.
 * \param input     The buffer holding the input data.
 * \param ilen      The length of the input data.
 *
 * \return          \c 0 on success.
 * \return          #MBEDTLS2_ERR_MD_BAD_INPUT_DATA on
 * parameter-verification failure.
 */
MBEDTLS2_CHECK_RETURN_TYPICAL
int mbedtls2_md_update(mbedtls2_md_context_t *ctx,
                              const unsigned char *input, size_t ilen);

/**
 * \brief           This function finishes the digest operation,
 *                  and writes the result to the output buffer.
 *
 *                  Call this function after a call to
 * mbedtls2_md_starts(), followed by any number of calls to
 * mbedtls2_md_update(). Afterwards, you may either clear the context
 * with mbedtls2_md_free(), or call mbedtls2_md_starts() to reuse
 *                  the context for another digest operation with the same
 *                  algorithm.
 *
 * \param ctx       The generic message-digest context.
 * \param output    The buffer for the generic message-digest checksum result.
 *
 * \return          \c 0 on success.
 * \return          #MBEDTLS2_ERR_MD_BAD_INPUT_DATA on
 * parameter-verification failure.
 */
MBEDTLS2_CHECK_RETURN_TYPICAL
int mbedtls2_md_finish(mbedtls2_md_context_t *ctx,
                              unsigned char *output);

/**
 * \brief          This function calculates the message-digest of a buffer,
 *                 with respect to a configurable message-digest algorithm
 *                 in a single call.
 *
 *                 The result is calculated as
 *                 Output = message_digest(input buffer).
 *
 * \param md_info  The information structure of the message-digest algorithm
 *                 to use.
 * \param input    The buffer holding the data.
 * \param ilen     The length of the input data.
 * \param output   The generic message-digest checksum result.
 *
 * \return         \c 0 on success.
 * \return         #MBEDTLS2_ERR_MD_BAD_INPUT_DATA on
 * parameter-verification failure.
 */
MBEDTLS2_CHECK_RETURN_TYPICAL
int mbedtls2_md(const mbedtls2_md_info_t *md_info,
                       const unsigned char *input, size_t ilen,
                       unsigned char *output);

#if defined(MBEDTLS2_FS_IO)
/**
 * \brief          This function calculates the message-digest checksum
 *                 result of the contents of the provided file.
 *
 *                 The result is calculated as
 *                 Output = message_digest(file contents).
 *
 * \param md_info  The information structure of the message-digest algorithm
 *                 to use.
 * \param path     The input file name.
 * \param output   The generic message-digest checksum result.
 *
 * \return         \c 0 on success.
 * \return         #MBEDTLS2_ERR_MD_FILE_IO_ERROR on an I/O error
 * accessing the file pointed by \p path.
 * \return         #MBEDTLS2_ERR_MD_BAD_INPUT_DATA if \p md_info was
 * NULL.
 */
MBEDTLS2_CHECK_RETURN_TYPICAL
int mbedtls2_md_file(const mbedtls2_md_info_t *md_info,
                            const char *path, unsigned char *output);
#endif /* MBEDTLS2_FS_IO */

/**
 * \brief           This function sets the HMAC key and prepares to
 *                  authenticate a new message.
 *
 *                  Call this function after mbedtls2_md_setup(), to use
 *                  the MD context for an HMAC calculation, then call
 *                  mbedtls2_md_hmac_update() to provide the input data,
 * and mbedtls2_md_hmac_finish() to get the HMAC value.
 *
 * \param ctx       The message digest context containing an embedded HMAC
 *                  context.
 * \param key       The HMAC secret key.
 * \param keylen    The length of the HMAC key in Bytes.
 *
 * \return          \c 0 on success.
 * \return          #MBEDTLS2_ERR_MD_BAD_INPUT_DATA on
 * parameter-verification failure.
 */
MBEDTLS2_CHECK_RETURN_TYPICAL
int mbedtls2_md_hmac_starts(mbedtls2_md_context_t *ctx,
                                   const unsigned char *key, size_t keylen);

/**
 * \brief           This function feeds an input buffer into an ongoing HMAC
 *                  computation.
 *
 *                  Call mbedtls2_md_hmac_starts() or
 * mbedtls2_md_hmac_reset() before calling this function. You may call
 * this function multiple times to pass the input piecewise. Afterwards, call
 * mbedtls2_md_hmac_finish().
 *
 * \param ctx       The message digest context containing an embedded HMAC
 *                  context.
 * \param input     The buffer holding the input data.
 * \param ilen      The length of the input data.
 *
 * \return          \c 0 on success.
 * \return          #MBEDTLS2_ERR_MD_BAD_INPUT_DATA on
 * parameter-verification failure.
 */
MBEDTLS2_CHECK_RETURN_TYPICAL
int mbedtls2_md_hmac_update(mbedtls2_md_context_t *ctx,
                                   const unsigned char *input, size_t ilen);

/**
 * \brief           This function finishes the HMAC operation, and writes
 *                  the result to the output buffer.
 *
 *                  Call this function after mbedtls2_md_hmac_starts()
 * and mbedtls2_md_hmac_update() to get the HMAC value. Afterwards you
 * may either call mbedtls2_md_free() to clear the context, or call
 * mbedtls2_md_hmac_reset() to reuse the context with the same HMAC key.
 *
 * \param ctx       The message digest context containing an embedded HMAC
 *                  context.
 * \param output    The generic HMAC checksum result.
 *
 * \return          \c 0 on success.
 * \return          #MBEDTLS2_ERR_MD_BAD_INPUT_DATA on
 * parameter-verification failure.
 */
MBEDTLS2_CHECK_RETURN_TYPICAL
int mbedtls2_md_hmac_finish(mbedtls2_md_context_t *ctx,
                                   unsigned char *output);

/**
 * \brief           This function prepares to authenticate a new message with
 *                  the same key as the previous HMAC operation.
 *
 *                  You may call this function after
 * mbedtls2_md_hmac_finish(). Afterwards call
 * mbedtls2_md_hmac_update() to pass the new input.
 *
 * \param ctx       The message digest context containing an embedded HMAC
 *                  context.
 *
 * \return          \c 0 on success.
 * \return          #MBEDTLS2_ERR_MD_BAD_INPUT_DATA on
 * parameter-verification failure.
 */
MBEDTLS2_CHECK_RETURN_TYPICAL
int mbedtls2_md_hmac_reset(mbedtls2_md_context_t *ctx);

/**
 * \brief          This function calculates the full generic HMAC
 *                 on the input buffer with the provided key.
 *
 *                 The function allocates the context, performs the
 *                 calculation, and frees the context.
 *
 *                 The HMAC result is calculated as
 *                 output = generic HMAC(hmac key, input buffer).
 *
 * \param md_info  The information structure of the message-digest algorithm
 *                 to use.
 * \param key      The HMAC secret key.
 * \param keylen   The length of the HMAC secret key in Bytes.
 * \param input    The buffer holding the input data.
 * \param ilen     The length of the input data.
 * \param output   The generic HMAC result.
 *
 * \return         \c 0 on success.
 * \return         #MBEDTLS2_ERR_MD_BAD_INPUT_DATA on
 * parameter-verification failure.
 */
MBEDTLS2_CHECK_RETURN_TYPICAL
int mbedtls2_md_hmac(const mbedtls2_md_info_t *md_info,
                            const unsigned char *key, size_t keylen,
                            const unsigned char *input, size_t ilen,
                            unsigned char *output);

/* Internal use */
MBEDTLS2_CHECK_RETURN_TYPICAL
int mbedtls2_md_process(mbedtls2_md_context_t *ctx,
                               const unsigned char *data);

#ifdef __cplusplus
}
#endif

#endif /* MBEDTLS2_MD_H */

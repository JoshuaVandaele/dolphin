/**
 * \file entropy.h
 *
 * \brief Entropy accumulator implementation
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
#ifndef MBEDTLS2_ENTROPY_H
#define MBEDTLS2_ENTROPY_H

#if !defined(MBEDTLS2_CONFIG_FILE)
#include "mbedtls2/config.h"
#else
#include MBEDTLS2_CONFIG_FILE
#endif

#include <stddef.h>

#if defined(MBEDTLS2_SHA512_C) &&                                       \
    !defined(MBEDTLS2_ENTROPY_FORCE_SHA256)
#include "mbedtls2/sha512.h"
#define MBEDTLS2_ENTROPY_SHA512_ACCUMULATOR
#else
#if defined(MBEDTLS2_SHA256_C)
#define MBEDTLS2_ENTROPY_SHA256_ACCUMULATOR
#include "mbedtls2/sha256.h"
#endif
#endif

#if defined(MBEDTLS2_THREADING_C)
#include "mbedtls2/threading.h"
#endif

#if defined(MBEDTLS2_HAVEGE_C)
#include "mbedtls2/havege.h"
#endif

/** Critical entropy source failure. */
#define MBEDTLS2_ERR_ENTROPY_SOURCE_FAILED -0x003C
/** No more sources can be added. */
#define MBEDTLS2_ERR_ENTROPY_MAX_SOURCES -0x003E
/** No sources have been added to poll. */
#define MBEDTLS2_ERR_ENTROPY_NO_SOURCES_DEFINED -0x0040
/** No strong sources have been added to poll. */
#define MBEDTLS2_ERR_ENTROPY_NO_STRONG_SOURCE -0x003D
/** Read/write error in file. */
#define MBEDTLS2_ERR_ENTROPY_FILE_IO_ERROR -0x003F

/**
 * \name SECTION: Module settings
 *
 * The configuration options you can set for this module are in this section.
 * Either change them in config.h or define them on the compiler command line.
 * \{
 */

#if !defined(MBEDTLS2_ENTROPY_MAX_SOURCES)
#define MBEDTLS2_ENTROPY_MAX_SOURCES                                    \
  20 /**< Maximum number of sources supported */
#endif

#if !defined(MBEDTLS2_ENTROPY_MAX_GATHER)
#define MBEDTLS2_ENTROPY_MAX_GATHER                                     \
  128 /**< Maximum amount requested from entropy sources */
#endif

/* \} name SECTION: Module settings */

#if defined(MBEDTLS2_ENTROPY_SHA512_ACCUMULATOR)
#define MBEDTLS2_ENTROPY_BLOCK_SIZE                                     \
  64 /**< Block size of entropy accumulator (SHA-512) */
#else
#define MBEDTLS2_ENTROPY_BLOCK_SIZE                                     \
  32 /**< Block size of entropy accumulator (SHA-256) */
#endif

#define MBEDTLS2_ENTROPY_MAX_SEED_SIZE                                  \
  1024 /**< Maximum size of seed we read from seed file */
#define MBEDTLS2_ENTROPY_SOURCE_MANUAL                                  \
  MBEDTLS2_ENTROPY_MAX_SOURCES

#define MBEDTLS2_ENTROPY_SOURCE_STRONG                                  \
  1                                           /**< Entropy source is strong   */
#define MBEDTLS2_ENTROPY_SOURCE_WEAK 0 /**< Entropy source is weak */

#ifdef __cplusplus
extern "C" {
#endif

/**
 * \brief           Entropy poll callback pointer
 *
 * \param data      Callback-specific data pointer
 * \param output    Data to fill
 * \param len       Maximum size to provide
 * \param olen      The actual amount of bytes put into the buffer (Can be 0)
 *
 * \return          0 if no critical failures occurred,
 *                  MBEDTLS2_ERR_ENTROPY_SOURCE_FAILED otherwise
 */
typedef int (*mbedtls2_entropy_f_source_ptr)(void *data,
                                                    unsigned char *output,
                                                    size_t len, size_t *olen);

/**
 * \brief           Entropy source state
 */
typedef struct mbedtls2_entropy_source_state {
  mbedtls2_entropy_f_source_ptr
      f_source;     /**< The entropy source callback */
  void *p_source;   /**< The callback data pointer */
  size_t size;      /**< Amount received in bytes */
  size_t threshold; /**< Minimum bytes required before release */
  int strong;       /**< Is the source strong? */
} mbedtls2_entropy_source_state;

/**
 * \brief           Entropy context structure
 */
typedef struct mbedtls2_entropy_context {
  int accumulator_started; /* 0 after init.
                            * 1 after the first update.
                            * -1 after free. */
#if defined(MBEDTLS2_ENTROPY_SHA512_ACCUMULATOR)
  mbedtls2_sha512_context accumulator;
#elif defined(MBEDTLS2_ENTROPY_SHA256_ACCUMULATOR)
  mbedtls2_sha256_context accumulator;
#endif
  int source_count; /* Number of entries used in source. */
  mbedtls2_entropy_source_state
      source[MBEDTLS2_ENTROPY_MAX_SOURCES];
#if defined(MBEDTLS2_HAVEGE_C)
  mbedtls2_havege_state havege_data;
#endif
#if defined(MBEDTLS2_THREADING_C)
  mbedtls2_threading_mutex_t mutex; /*!< mutex                  */
#endif
#if defined(MBEDTLS2_ENTROPY_NV_SEED)
  int initial_entropy_run;
#endif
} mbedtls2_entropy_context;

/**
 * \brief           Initialize the context
 *
 * \param ctx       Entropy context to initialize
 */
void mbedtls2_entropy_init(mbedtls2_entropy_context *ctx);

/**
 * \brief           Free the data in the context
 *
 * \param ctx       Entropy context to free
 */
void mbedtls2_entropy_free(mbedtls2_entropy_context *ctx);

/**
 * \brief           Adds an entropy source to poll
 *                  (Thread-safe if MBEDTLS2_THREADING_C is enabled)
 *
 * \param ctx       Entropy context
 * \param f_source  Entropy function
 * \param p_source  Function data
 * \param threshold Minimum required from source before entropy is released
 *                  ( with mbedtls2_entropy_func() ) (in bytes)
 * \param strong    MBEDTLS2_ENTROPY_SOURCE_STRONG or
 *                  MBEDTLS2_ENTROPY_SOURCE_WEAK.
 *                  At least one strong source needs to be added.
 *                  Weaker sources (such as the cycle counter) can be used as
 *                  a complement.
 *
 * \return          0 if successful or MBEDTLS2_ERR_ENTROPY_MAX_SOURCES
 */
int mbedtls2_entropy_add_source(
    mbedtls2_entropy_context *ctx,
    mbedtls2_entropy_f_source_ptr f_source, void *p_source,
    size_t threshold, int strong);

/**
 * \brief           Trigger an extra gather poll for the accumulator
 *                  (Thread-safe if MBEDTLS2_THREADING_C is enabled)
 *
 * \param ctx       Entropy context
 *
 * \return          0 if successful, or
 * MBEDTLS2_ERR_ENTROPY_SOURCE_FAILED
 */
int mbedtls2_entropy_gather(mbedtls2_entropy_context *ctx);

/**
 * \brief           Retrieve entropy from the accumulator
 *                  (Maximum length: MBEDTLS2_ENTROPY_BLOCK_SIZE)
 *                  (Thread-safe if MBEDTLS2_THREADING_C is enabled)
 *
 * \param data      Entropy context
 * \param output    Buffer to fill
 * \param len       Number of bytes desired, must be at most
 * MBEDTLS2_ENTROPY_BLOCK_SIZE
 *
 * \return          0 if successful, or
 * MBEDTLS2_ERR_ENTROPY_SOURCE_FAILED
 */
int mbedtls2_entropy_func(void *data, unsigned char *output, size_t len);

/**
 * \brief           Add data to the accumulator manually
 *                  (Thread-safe if MBEDTLS2_THREADING_C is enabled)
 *
 * \param ctx       Entropy context
 * \param data      Data to add
 * \param len       Length of data
 *
 * \return          0 if successful
 */
int mbedtls2_entropy_update_manual(mbedtls2_entropy_context *ctx,
                                          const unsigned char *data,
                                          size_t len);

#if defined(MBEDTLS2_ENTROPY_NV_SEED)
/**
 * \brief           Trigger an update of the seed file in NV by using the
 *                  current entropy pool.
 *
 * \param ctx       Entropy context
 *
 * \return          0 if successful
 */
int mbedtls2_entropy_update_nv_seed(
    mbedtls2_entropy_context *ctx);
#endif /* MBEDTLS2_ENTROPY_NV_SEED */

#if defined(MBEDTLS2_FS_IO)
/**
 * \brief               Write a seed file
 *
 * \param ctx           Entropy context
 * \param path          Name of the file
 *
 * \return              0 if successful,
 *                      MBEDTLS2_ERR_ENTROPY_FILE_IO_ERROR on file error,
 * or MBEDTLS2_ERR_ENTROPY_SOURCE_FAILED
 */
int mbedtls2_entropy_write_seed_file(
    mbedtls2_entropy_context *ctx, const char *path);

/**
 * \brief               Read and update a seed file. Seed is added to this
 *                      instance. No more than
 * MBEDTLS2_ENTROPY_MAX_SEED_SIZE bytes are read from the seed file. The
 * rest is ignored.
 *
 * \param ctx           Entropy context
 * \param path          Name of the file
 *
 * \return              0 if successful,
 *                      MBEDTLS2_ERR_ENTROPY_FILE_IO_ERROR on file error,
 *                      MBEDTLS2_ERR_ENTROPY_SOURCE_FAILED
 */
int mbedtls2_entropy_update_seed_file(
    mbedtls2_entropy_context *ctx, const char *path);
#endif /* MBEDTLS2_FS_IO */

#if defined(MBEDTLS2_SELF_TEST)
/**
 * \brief          Checkup routine
 *
 *                 This module self-test also calls the entropy self-test,
 *                 mbedtls2_entropy_source_self_test();
 *
 * \return         0 if successful, or 1 if a test failed
 */
int mbedtls2_entropy_self_test(int verbose);

#if defined(MBEDTLS2_ENTROPY_HARDWARE_ALT)
/**
 * \brief          Checkup routine
 *
 *                 Verifies the integrity of the hardware entropy source
 *                 provided by the function 'mbedtls2_hardware_poll()'.
 *
 *                 Note this is the only hardware entropy source that is known
 *                 at link time, and other entropy sources configured
 *                 dynamically at runtime by the function
 *                 mbedtls2_entropy_add_source() will not be tested.
 *
 * \return         0 if successful, or 1 if a test failed
 */
int mbedtls2_entropy_source_self_test(int verbose);
#endif /* MBEDTLS2_ENTROPY_HARDWARE_ALT */
#endif /* MBEDTLS2_SELF_TEST */

#ifdef __cplusplus
}
#endif

#endif /* entropy.h */

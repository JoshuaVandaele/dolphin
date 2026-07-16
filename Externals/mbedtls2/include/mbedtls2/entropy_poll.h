/**
 * \file entropy_poll.h
 *
 * \brief Platform-specific and custom entropy polling functions
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
#ifndef MBEDTLS2_ENTROPY_POLL_H
#define MBEDTLS2_ENTROPY_POLL_H

#if !defined(MBEDTLS2_CONFIG_FILE)
#include "mbedtls2/config.h"
#else
#include MBEDTLS2_CONFIG_FILE
#endif

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Default thresholds for built-in sources, in bytes
 */
#define MBEDTLS2_ENTROPY_MIN_PLATFORM                                   \
  32 /**< Minimum for platform source    */
#define MBEDTLS2_ENTROPY_MIN_HAVEGE                                     \
  32 /**< Minimum for HAVEGE             */
#define MBEDTLS2_ENTROPY_MIN_HARDCLOCK                                  \
  4 /**< Minimum for mbedtls2_timing_hardclock()        */
#if !defined(MBEDTLS2_ENTROPY_MIN_HARDWARE)
#define MBEDTLS2_ENTROPY_MIN_HARDWARE                                   \
  32 /**< Minimum for the hardware source */
#endif

/**
 * \brief           Entropy poll callback that provides 0 entropy.
 */
#if defined(MBEDTLS2_TEST_NULL_ENTROPY)
int mbedtls2_null_entropy_poll(void *data, unsigned char *output,
                                      size_t len, size_t *olen);
#endif

#if !defined(MBEDTLS2_NO_PLATFORM_ENTROPY)
/**
 * \brief           Platform-specific entropy poll callback
 */
int mbedtls2_platform_entropy_poll(void *data, unsigned char *output,
                                          size_t len, size_t *olen);
#endif

#if defined(MBEDTLS2_HAVEGE_C)
/**
 * \brief           HAVEGE based entropy poll callback
 *
 * Requires an HAVEGE state as its data pointer.
 */
int mbedtls2_havege_poll(void *data, unsigned char *output, size_t len,
                                size_t *olen);
#endif

#if defined(MBEDTLS2_TIMING_C)
/**
 * \brief           mbedtls2_timing_hardclock-based entropy poll callback
 */
int mbedtls2_hardclock_poll(void *data, unsigned char *output,
                                   size_t len, size_t *olen);
#endif

#if defined(MBEDTLS2_ENTROPY_HARDWARE_ALT)
/**
 * \brief           Entropy poll callback for a hardware source
 *
 * \warning         This is not provided by mbed TLS!
 *                  See \c MBEDTLS2_ENTROPY_HARDWARE_ALT in config.h.
 *
 * \note            This must accept NULL as its first argument.
 */
int mbedtls2_hardware_poll(void *data, unsigned char *output, size_t len,
                                  size_t *olen);
#endif

#if defined(MBEDTLS2_ENTROPY_NV_SEED)
/**
 * \brief           Entropy poll callback for a non-volatile seed file
 *
 * \note            This must accept NULL as its first argument.
 */
int mbedtls2_nv_seed_poll(void *data, unsigned char *output, size_t len,
                                 size_t *olen);
#endif

#ifdef __cplusplus
}
#endif

#endif /* entropy_poll.h */

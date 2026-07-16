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
 *  This file is part of mbed TLS (https://tls.mbed.org)
 */

/**
 * \file mps_trace.h
 *
 * \brief Tracing module for MPS
 */

#ifndef MBEDTLS2_MPS_MBEDTLS2_MPS_TRACE_H
#define MBEDTLS2_MPS_MBEDTLS2_MPS_TRACE_H

#include "common.h"
#include "mps_common.h"
#include "mps_trace.h"

#if defined(MBEDTLS2_PLATFORM_C)
#include "mbedtls2/platform.h"
#else
#include <stdio.h>
#define mbedtls2_printf printf
#define mbedtls2_vsnprintf vsnprintf
#endif /* MBEDTLS2_PLATFORM_C */

#if defined(MBEDTLS2_MPS_ENABLE_TRACE)

/*
 * Adapt this to enable/disable tracing output
 * from the various layers of the MPS.
 */

#define MBEDTLS2_MPS_TRACE_ENABLE_LAYER_1
#define MBEDTLS2_MPS_TRACE_ENABLE_LAYER_2
#define MBEDTLS2_MPS_TRACE_ENABLE_LAYER_3
#define MBEDTLS2_MPS_TRACE_ENABLE_LAYER_4
#define MBEDTLS2_MPS_TRACE_ENABLE_READER
#define MBEDTLS2_MPS_TRACE_ENABLE_WRITER

/*
 * To use the existing trace module, only change
 * MBEDTLS2_MPS_TRACE_ENABLE_XXX above, but don't modify the
 * rest of this file.
 */

typedef enum {
  MBEDTLS2_MPS_TRACE_TYPE_COMMENT,
  MBEDTLS2_MPS_TRACE_TYPE_CALL,
  MBEDTLS2_MPS_TRACE_TYPE_ERROR,
  MBEDTLS2_MPS_TRACE_TYPE_RETURN
} mbedtls2_mps_trace_type;

#define MBEDTLS2_MPS_TRACE_BIT_LAYER_1 1
#define MBEDTLS2_MPS_TRACE_BIT_LAYER_2 2
#define MBEDTLS2_MPS_TRACE_BIT_LAYER_3 3
#define MBEDTLS2_MPS_TRACE_BIT_LAYER_4 4
#define MBEDTLS2_MPS_TRACE_BIT_WRITER 5
#define MBEDTLS2_MPS_TRACE_BIT_READER 6

#if defined(MBEDTLS2_MPS_TRACE_ENABLE_LAYER_1)
#define MBEDTLS2_MPS_TRACE_MASK_LAYER_1                                 \
  (1u << MBEDTLS2_MPS_TRACE_BIT_LAYER_1)
#else
#define MBEDTLS2_MPS_TRACE_MASK_LAYER_1 0
#endif

#if defined(MBEDTLS2_MPS_TRACE_ENABLE_LAYER_2)
#define MBEDTLS2_MPS_TRACE_MASK_LAYER_2                                 \
  (1u << MBEDTLS2_MPS_TRACE_BIT_LAYER_2)
#else
#define MBEDTLS2_MPS_TRACE_MASK_LAYER_2 0
#endif

#if defined(MBEDTLS2_MPS_TRACE_ENABLE_LAYER_3)
#define MBEDTLS2_MPS_TRACE_MASK_LAYER_3                                 \
  (1u << MBEDTLS2_MPS_TRACE_BIT_LAYER_3)
#else
#define MBEDTLS2_MPS_TRACE_MASK_LAYER_3 0
#endif

#if defined(MBEDTLS2_MPS_TRACE_ENABLE_LAYER_4)
#define MBEDTLS2_MPS_TRACE_MASK_LAYER_4                                 \
  (1u << MBEDTLS2_MPS_TRACE_BIT_LAYER_4)
#else
#define MBEDTLS2_MPS_TRACE_MASK_LAYER_4 0
#endif

#if defined(MBEDTLS2_MPS_TRACE_ENABLE_READER)
#define MBEDTLS2_MPS_TRACE_MASK_READER                                  \
  (1u << MBEDTLS2_MPS_TRACE_BIT_READER)
#else
#define MBEDTLS2_MPS_TRACE_MASK_READER 0
#endif

#if defined(MBEDTLS2_MPS_TRACE_ENABLE_WRITER)
#define MBEDTLS2_MPS_TRACE_MASK_WRITER                                  \
  (1u << MBEDTLS2_MPS_TRACE_BIT_WRITER)
#else
#define MBEDTLS2_MPS_TRACE_MASK_WRITER 0
#endif

#define MBEDTLS2_MPS_TRACE_MASK                                         \
  (MBEDTLS2_MPS_TRACE_MASK_LAYER_1 |                                    \
   MBEDTLS2_MPS_TRACE_MASK_LAYER_2 |                                    \
   MBEDTLS2_MPS_TRACE_MASK_LAYER_3 |                                    \
   MBEDTLS2_MPS_TRACE_MASK_LAYER_4 |                                    \
   MBEDTLS2_MPS_TRACE_MASK_READER |                                     \
   MBEDTLS2_MPS_TRACE_MASK_WRITER)

/* We have to avoid globals because E-ACSL chokes on them...
 * Wrap everything in stub functions. */
int mbedtls2_mps_trace_get_depth(void);
void mbedtls2_mps_trace_inc_depth(void);
void mbedtls2_mps_trace_dec_depth(void);

void mbedtls2_mps_trace_color(int id);
void mbedtls2_mps_trace_indent(int level,
                                      mbedtls2_mps_trace_type ty);

void mbedtls2_mps_trace_print_msg(int id, int line, const char *format,
                                         ...);

#define MBEDTLS2_MPS_TRACE(type, ...)                                   \
  do {                                                                         \
    if (!(MBEDTLS2_MPS_TRACE_MASK &                                     \
          (1u << mbedtls2_mps_trace_id)))                               \
      break;                                                                   \
    mbedtls2_mps_trace_indent(mbedtls2_mps_trace_get_depth(),    \
                                     type);                                    \
    mbedtls2_mps_trace_color(mbedtls2_mps_trace_id);             \
    mbedtls2_mps_trace_print_msg(mbedtls2_mps_trace_id,          \
                                        __LINE__, __VA_ARGS__);                \
    mbedtls2_mps_trace_color(0);                                        \
  } while (0)

#define MBEDTLS2_MPS_TRACE_INIT(...)                                    \
  do {                                                                         \
    if (!(MBEDTLS2_MPS_TRACE_MASK &                                     \
          (1u << mbedtls2_mps_trace_id)))                               \
      break;                                                                   \
    MBEDTLS2_MPS_TRACE(MBEDTLS2_MPS_TRACE_TYPE_CALL,             \
                              __VA_ARGS__);                                    \
    mbedtls2_mps_trace_inc_depth();                                     \
  } while (0)

#define MBEDTLS2_MPS_TRACE_END(val)                                     \
  do {                                                                         \
    if (!(MBEDTLS2_MPS_TRACE_MASK &                                     \
          (1u << mbedtls2_mps_trace_id)))                               \
      break;                                                                   \
    MBEDTLS2_MPS_TRACE(MBEDTLS2_MPS_TRACE_TYPE_RETURN,           \
                              "%d (-%#04x)", (int)(val), -((unsigned)(val)));  \
    mbedtls2_mps_trace_dec_depth();                                     \
  } while (0)

#define MBEDTLS2_MPS_TRACE_RETURN(val)                                  \
  do {                                                                         \
    /* Breaks tail recursion. */                                               \
    int ret__ = val;                                                           \
    MBEDTLS2_MPS_TRACE_END(ret__);                                      \
    return (ret__);                                                            \
  } while (0)

#else /* MBEDTLS2_MPS_TRACE */

#define MBEDTLS2_MPS_TRACE(type, ...)                                   \
  do {                                                                         \
  } while (0)
#define MBEDTLS2_MPS_TRACE_INIT(...)                                    \
  do {                                                                         \
  } while (0)
#define MBEDTLS2_MPS_TRACE_END                                          \
  do {                                                                         \
  } while (0)

#define MBEDTLS2_MPS_TRACE_RETURN(val) return (val);

#endif /* MBEDTLS2_MPS_TRACE */

#endif /* MBEDTLS2_MPS_MBEDTLS2_MPS_TRACE_H */

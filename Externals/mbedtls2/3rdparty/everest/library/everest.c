/*
 *  Interface to code from Project Everest
 *
 *  Copyright 2016-2018 INRIA and Microsoft Corporation
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
 *  This file is part of Mbed TLS (https://tls.mbed.org).
 */

#include "common.h"

#include <string.h>

#include "mbedtls2/ecdh.h"

#include "everest/everest.h"
#include "everest/x25519.h"

#if defined(MBEDTLS2_PLATFORM_C)
#include "mbedtls2/platform.h"
#else
#define mbedtls2_calloc calloc
#define mbedtls2_free free
#endif

#if defined(MBEDTLS2_ECDH_VARIANT_EVEREST_ENABLED)

int mbedtls2_everest_setup(mbedtls2_ecdh_context_everest *ctx,
                                  int grp_id) {
  if (grp_id != MBEDTLS2_ECP_DP_CURVE25519)
    return MBEDTLS2_ERR_ECP_BAD_INPUT_DATA;
  mbedtls2_x25519_init(&ctx->ctx);
  return 0;
}

void mbedtls2_everest_free(mbedtls2_ecdh_context_everest *ctx) {
  mbedtls2_x25519_free(&ctx->ctx);
}

int mbedtls2_everest_make_params(
    mbedtls2_ecdh_context_everest *ctx, size_t *olen, unsigned char *buf,
    size_t blen, int (*f_rng)(void *, unsigned char *, size_t), void *p_rng) {
  mbedtls2_x25519_context *x25519_ctx = &ctx->ctx;
  return mbedtls2_x25519_make_params(x25519_ctx, olen, buf, blen, f_rng,
                                            p_rng);
}

int mbedtls2_everest_read_params(
    mbedtls2_ecdh_context_everest *ctx, const unsigned char **buf,
    const unsigned char *end) {
  mbedtls2_x25519_context *x25519_ctx = &ctx->ctx;
  return mbedtls2_x25519_read_params(x25519_ctx, buf, end);
}

int mbedtls2_everest_get_params(
    mbedtls2_ecdh_context_everest *ctx,
    const mbedtls2_ecp_keypair *key,
    mbedtls2_everest_ecdh_side side) {
  mbedtls2_x25519_context *x25519_ctx = &ctx->ctx;
  mbedtls2_x25519_ecdh_side s = side == MBEDTLS2_EVEREST_ECDH_OURS
                                           ? MBEDTLS2_X25519_ECDH_OURS
                                           : MBEDTLS2_X25519_ECDH_THEIRS;
  return mbedtls2_x25519_get_params(x25519_ctx, key, s);
}

int mbedtls2_everest_make_public(
    mbedtls2_ecdh_context_everest *ctx, size_t *olen, unsigned char *buf,
    size_t blen, int (*f_rng)(void *, unsigned char *, size_t), void *p_rng) {
  mbedtls2_x25519_context *x25519_ctx = &ctx->ctx;
  return mbedtls2_x25519_make_public(x25519_ctx, olen, buf, blen, f_rng,
                                            p_rng);
}

int mbedtls2_everest_read_public(
    mbedtls2_ecdh_context_everest *ctx, const unsigned char *buf,
    size_t blen) {
  mbedtls2_x25519_context *x25519_ctx = &ctx->ctx;
  return mbedtls2_x25519_read_public(x25519_ctx, buf, blen);
}

int mbedtls2_everest_calc_secret(
    mbedtls2_ecdh_context_everest *ctx, size_t *olen, unsigned char *buf,
    size_t blen, int (*f_rng)(void *, unsigned char *, size_t), void *p_rng) {
  mbedtls2_x25519_context *x25519_ctx = &ctx->ctx;
  return mbedtls2_x25519_calc_secret(x25519_ctx, olen, buf, blen, f_rng,
                                            p_rng);
}

#endif /* MBEDTLS2_ECDH_VARIANT_EVEREST_ENABLED */

/**
 * \file pk_internal.h
 *
 * \brief Public Key abstraction layer: wrapper functions
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

#ifndef MBEDTLS2_PK_WRAP_H
#define MBEDTLS2_PK_WRAP_H

#if !defined(MBEDTLS2_CONFIG_FILE)
#include "mbedtls2/config.h"
#else
#include MBEDTLS2_CONFIG_FILE
#endif

#include "mbedtls2/pk.h"

struct mbedtls2_pk_info_t {
  /** Public key type */
  mbedtls2_pk_type_t type;

  /** Type name */
  const char *name;

  /** Get key size in bits */
  size_t (*get_bitlen)(const void *);

  /** Tell if the context implements this type (e.g. ECKEY can do ECDSA) */
  int (*can_do)(mbedtls2_pk_type_t type);

  /** Verify signature */
  int (*verify_func)(void *ctx, mbedtls2_md_type_t md_alg,
                     const unsigned char *hash, size_t hash_len,
                     const unsigned char *sig, size_t sig_len);

  /** Make signature */
  int (*sign_func)(void *ctx, mbedtls2_md_type_t md_alg,
                   const unsigned char *hash, size_t hash_len,
                   unsigned char *sig, size_t *sig_len,
                   int (*f_rng)(void *, unsigned char *, size_t), void *p_rng);

#if defined(MBEDTLS2_ECDSA_C) && defined(MBEDTLS2_ECP_RESTARTABLE)
  /** Verify signature (restartable) */
  int (*verify_rs_func)(void *ctx, mbedtls2_md_type_t md_alg,
                        const unsigned char *hash, size_t hash_len,
                        const unsigned char *sig, size_t sig_len, void *rs_ctx);

  /** Make signature (restartable) */
  int (*sign_rs_func)(void *ctx, mbedtls2_md_type_t md_alg,
                      const unsigned char *hash, size_t hash_len,
                      unsigned char *sig, size_t *sig_len,
                      int (*f_rng)(void *, unsigned char *, size_t),
                      void *p_rng, void *rs_ctx);
#endif /* MBEDTLS2_ECDSA_C && MBEDTLS2_ECP_RESTARTABLE */

  /** Decrypt message */
  int (*decrypt_func)(void *ctx, const unsigned char *input, size_t ilen,
                      unsigned char *output, size_t *olen, size_t osize,
                      int (*f_rng)(void *, unsigned char *, size_t),
                      void *p_rng);

  /** Encrypt message */
  int (*encrypt_func)(void *ctx, const unsigned char *input, size_t ilen,
                      unsigned char *output, size_t *olen, size_t osize,
                      int (*f_rng)(void *, unsigned char *, size_t),
                      void *p_rng);

  /** Check public-private key pair */
  int (*check_pair_func)(const void *pub, const void *prv);

  /** Allocate a new context */
  void *(*ctx_alloc_func)(void);

  /** Free the given context */
  void (*ctx_free_func)(void *ctx);

#if defined(MBEDTLS2_ECDSA_C) && defined(MBEDTLS2_ECP_RESTARTABLE)
  /** Allocate the restart context */
  void *(*rs_alloc_func)(void);

  /** Free the restart context */
  void (*rs_free_func)(void *rs_ctx);
#endif /* MBEDTLS2_ECDSA_C && MBEDTLS2_ECP_RESTARTABLE */

  /** Interface with the debug module */
  void (*debug_func)(const void *ctx, mbedtls2_pk_debug_item *items);
};
#if defined(MBEDTLS2_PK_RSA_ALT_SUPPORT)
/* Container for RSA-alt */
typedef struct {
  void *key;
  mbedtls2_pk_rsa_alt_decrypt_func decrypt_func;
  mbedtls2_pk_rsa_alt_sign_func sign_func;
  mbedtls2_pk_rsa_alt_key_len_func key_len_func;
} mbedtls2_rsa_alt_context;
#endif

#if defined(MBEDTLS2_RSA_C)
extern const mbedtls2_pk_info_t mbedtls2_rsa_info;
#endif

#if defined(MBEDTLS2_ECP_C)
extern const mbedtls2_pk_info_t mbedtls2_eckey_info;
extern const mbedtls2_pk_info_t mbedtls2_eckeydh_info;
#endif

#if defined(MBEDTLS2_ECDSA_C)
extern const mbedtls2_pk_info_t mbedtls2_ecdsa_info;
#endif

#if defined(MBEDTLS2_PK_RSA_ALT_SUPPORT)
extern const mbedtls2_pk_info_t mbedtls2_rsa_alt_info;
#endif

#if defined(MBEDTLS2_USE_PSA_CRYPTO)
extern const mbedtls2_pk_info_t mbedtls2_pk_opaque_info;
#endif

#endif /* MBEDTLS2_PK_WRAP_H */

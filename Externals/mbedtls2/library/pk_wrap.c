/*
 *  Public Key abstraction layer: wrapper functions
 *
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

#include "common.h"

#if defined(MBEDTLS2_PK_C)
#include "mbedtls2/error.h"
#include "mbedtls2/pk_internal.h"

/* Even if RSA not activated, for the sake of RSA-alt */
#include "mbedtls2/rsa.h"

#include <string.h>

#if defined(MBEDTLS2_ECP_C)
#include "mbedtls2/ecp.h"
#endif

#if defined(MBEDTLS2_ECDSA_C)
#include "mbedtls2/ecdsa.h"
#endif

#if defined(MBEDTLS2_USE_PSA_CRYPTO)
#include "mbedtls2/asn1write.h"
#endif

#if defined(MBEDTLS2_PK_RSA_ALT_SUPPORT)
#include "mbedtls2/platform_util.h"
#endif

#if defined(MBEDTLS2_USE_PSA_CRYPTO)
#include "mbedtls2/asn1.h"
#include "mbedtls2/psa_util.h"
#include "psa/crypto.h"
#endif

#if defined(MBEDTLS2_PLATFORM_C)
#include "mbedtls2/platform.h"
#else
#include <stdlib.h>
#define mbedtls2_calloc calloc
#define mbedtls2_free free
#endif

#include <limits.h>
#include <stdint.h>

#if defined(MBEDTLS2_RSA_C)
static int rsa_can_do(mbedtls2_pk_type_t type) {
  return (type == MBEDTLS2_PK_RSA ||
          type == MBEDTLS2_PK_RSASSA_PSS);
}

static size_t rsa_get_bitlen(const void *ctx) {
  const mbedtls2_rsa_context *rsa =
      (const mbedtls2_rsa_context *)ctx;
  return (8 * mbedtls2_rsa_get_len(rsa));
}

static int rsa_verify_wrap(void *ctx, mbedtls2_md_type_t md_alg,
                           const unsigned char *hash, size_t hash_len,
                           const unsigned char *sig, size_t sig_len) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  mbedtls2_rsa_context *rsa = (mbedtls2_rsa_context *)ctx;
  size_t rsa_len = mbedtls2_rsa_get_len(rsa);

#if SIZE_MAX > UINT_MAX
  if (md_alg == MBEDTLS2_MD_NONE && UINT_MAX < hash_len)
    return (MBEDTLS2_ERR_PK_BAD_INPUT_DATA);
#endif /* SIZE_MAX > UINT_MAX */

  if (sig_len < rsa_len)
    return (MBEDTLS2_ERR_RSA_VERIFY_FAILED);

  if ((ret = mbedtls2_rsa_pkcs1_verify(
           rsa, NULL, NULL, MBEDTLS2_RSA_PUBLIC, md_alg,
           (unsigned int)hash_len, hash, sig)) != 0)
    return (ret);

  /* The buffer contains a valid signature followed by extra data.
   * We have a special error code for that so that so that callers can
   * use mbedtls2_pk_verify() to check "Does the buffer start with a
   * valid signature?" and not just "Does the buffer contain a valid
   * signature?". */
  if (sig_len > rsa_len)
    return (MBEDTLS2_ERR_PK_SIG_LEN_MISMATCH);

  return (0);
}

static int rsa_sign_wrap(void *ctx, mbedtls2_md_type_t md_alg,
                         const unsigned char *hash, size_t hash_len,
                         unsigned char *sig, size_t *sig_len,
                         int (*f_rng)(void *, unsigned char *, size_t),
                         void *p_rng) {
  mbedtls2_rsa_context *rsa = (mbedtls2_rsa_context *)ctx;

#if SIZE_MAX > UINT_MAX
  if (md_alg == MBEDTLS2_MD_NONE && UINT_MAX < hash_len)
    return (MBEDTLS2_ERR_PK_BAD_INPUT_DATA);
#endif /* SIZE_MAX > UINT_MAX */

  *sig_len = mbedtls2_rsa_get_len(rsa);

  return (mbedtls2_rsa_pkcs1_sign(rsa, f_rng, p_rng,
                                         MBEDTLS2_RSA_PRIVATE, md_alg,
                                         (unsigned int)hash_len, hash, sig));
}

static int rsa_decrypt_wrap(void *ctx, const unsigned char *input, size_t ilen,
                            unsigned char *output, size_t *olen, size_t osize,
                            int (*f_rng)(void *, unsigned char *, size_t),
                            void *p_rng) {
  mbedtls2_rsa_context *rsa = (mbedtls2_rsa_context *)ctx;

  if (ilen != mbedtls2_rsa_get_len(rsa))
    return (MBEDTLS2_ERR_RSA_BAD_INPUT_DATA);

  return (mbedtls2_rsa_pkcs1_decrypt(rsa, f_rng, p_rng,
                                            MBEDTLS2_RSA_PRIVATE, olen,
                                            input, output, osize));
}

static int rsa_encrypt_wrap(void *ctx, const unsigned char *input, size_t ilen,
                            unsigned char *output, size_t *olen, size_t osize,
                            int (*f_rng)(void *, unsigned char *, size_t),
                            void *p_rng) {
  mbedtls2_rsa_context *rsa = (mbedtls2_rsa_context *)ctx;
  *olen = mbedtls2_rsa_get_len(rsa);

  if (*olen > osize)
    return (MBEDTLS2_ERR_RSA_OUTPUT_TOO_LARGE);

  return (mbedtls2_rsa_pkcs1_encrypt(
      rsa, f_rng, p_rng, MBEDTLS2_RSA_PUBLIC, ilen, input, output));
}

static int rsa_check_pair_wrap(const void *pub, const void *prv) {
  return (mbedtls2_rsa_check_pub_priv(
      (const mbedtls2_rsa_context *)pub,
      (const mbedtls2_rsa_context *)prv));
}

static void *rsa_alloc_wrap(void) {
  void *ctx = mbedtls2_calloc(1, sizeof(mbedtls2_rsa_context));

  if (ctx != NULL)
    mbedtls2_rsa_init((mbedtls2_rsa_context *)ctx, 0, 0);

  return (ctx);
}

static void rsa_free_wrap(void *ctx) {
  mbedtls2_rsa_free((mbedtls2_rsa_context *)ctx);
  mbedtls2_free(ctx);
}

static void rsa_debug(const void *ctx, mbedtls2_pk_debug_item *items) {
  items->type = MBEDTLS2_PK_DEBUG_MPI;
  items->name = "rsa.N";
  items->value = &(((mbedtls2_rsa_context *)ctx)->N);

  items++;

  items->type = MBEDTLS2_PK_DEBUG_MPI;
  items->name = "rsa.E";
  items->value = &(((mbedtls2_rsa_context *)ctx)->E);
}

const mbedtls2_pk_info_t mbedtls2_rsa_info = {
    MBEDTLS2_PK_RSA,
    "RSA",
    rsa_get_bitlen,
    rsa_can_do,
    rsa_verify_wrap,
    rsa_sign_wrap,
#if defined(MBEDTLS2_ECDSA_C) && defined(MBEDTLS2_ECP_RESTARTABLE)
    NULL,
    NULL,
#endif
    rsa_decrypt_wrap,
    rsa_encrypt_wrap,
    rsa_check_pair_wrap,
    rsa_alloc_wrap,
    rsa_free_wrap,
#if defined(MBEDTLS2_ECDSA_C) && defined(MBEDTLS2_ECP_RESTARTABLE)
    NULL,
    NULL,
#endif
    rsa_debug,
};
#endif /* MBEDTLS2_RSA_C */

#if defined(MBEDTLS2_ECP_C)
/*
 * Generic EC key
 */
static int eckey_can_do(mbedtls2_pk_type_t type) {
  return (type == MBEDTLS2_PK_ECKEY ||
          type == MBEDTLS2_PK_ECKEY_DH ||
          type == MBEDTLS2_PK_ECDSA);
}

static size_t eckey_get_bitlen(const void *ctx) {
  return (((mbedtls2_ecp_keypair *)ctx)->grp.pbits);
}

#if defined(MBEDTLS2_ECDSA_C)
/* Forward declarations */
static int ecdsa_verify_wrap(void *ctx, mbedtls2_md_type_t md_alg,
                             const unsigned char *hash, size_t hash_len,
                             const unsigned char *sig, size_t sig_len);

static int ecdsa_sign_wrap(void *ctx, mbedtls2_md_type_t md_alg,
                           const unsigned char *hash, size_t hash_len,
                           unsigned char *sig, size_t *sig_len,
                           int (*f_rng)(void *, unsigned char *, size_t),
                           void *p_rng);

static int eckey_verify_wrap(void *ctx, mbedtls2_md_type_t md_alg,
                             const unsigned char *hash, size_t hash_len,
                             const unsigned char *sig, size_t sig_len) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  mbedtls2_ecdsa_context ecdsa;

  mbedtls2_ecdsa_init(&ecdsa);

  if ((ret = mbedtls2_ecdsa_from_keypair(&ecdsa, ctx)) == 0)
    ret = ecdsa_verify_wrap(&ecdsa, md_alg, hash, hash_len, sig, sig_len);

  mbedtls2_ecdsa_free(&ecdsa);

  return (ret);
}

static int eckey_sign_wrap(void *ctx, mbedtls2_md_type_t md_alg,
                           const unsigned char *hash, size_t hash_len,
                           unsigned char *sig, size_t *sig_len,
                           int (*f_rng)(void *, unsigned char *, size_t),
                           void *p_rng) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  mbedtls2_ecdsa_context ecdsa;

  mbedtls2_ecdsa_init(&ecdsa);

  if ((ret = mbedtls2_ecdsa_from_keypair(&ecdsa, ctx)) == 0)
    ret = ecdsa_sign_wrap(&ecdsa, md_alg, hash, hash_len, sig, sig_len, f_rng,
                          p_rng);

  mbedtls2_ecdsa_free(&ecdsa);

  return (ret);
}

#if defined(MBEDTLS2_ECP_RESTARTABLE)
/* Forward declarations */
static int ecdsa_verify_rs_wrap(void *ctx, mbedtls2_md_type_t md_alg,
                                const unsigned char *hash, size_t hash_len,
                                const unsigned char *sig, size_t sig_len,
                                void *rs_ctx);

static int ecdsa_sign_rs_wrap(void *ctx, mbedtls2_md_type_t md_alg,
                              const unsigned char *hash, size_t hash_len,
                              unsigned char *sig, size_t *sig_len,
                              int (*f_rng)(void *, unsigned char *, size_t),
                              void *p_rng, void *rs_ctx);

/*
 * Restart context for ECDSA operations with ECKEY context
 *
 * We need to store an actual ECDSA context, as we need to pass the same to
 * the underlying ecdsa function, so we can't create it on the fly every time.
 */
typedef struct {
  mbedtls2_ecdsa_restart_ctx ecdsa_rs;
  mbedtls2_ecdsa_context ecdsa_ctx;
} eckey_restart_ctx;

static void *eckey_rs_alloc(void) {
  eckey_restart_ctx *rs_ctx;

  void *ctx = mbedtls2_calloc(1, sizeof(eckey_restart_ctx));

  if (ctx != NULL) {
    rs_ctx = ctx;
    mbedtls2_ecdsa_restart_init(&rs_ctx->ecdsa_rs);
    mbedtls2_ecdsa_init(&rs_ctx->ecdsa_ctx);
  }

  return (ctx);
}

static void eckey_rs_free(void *ctx) {
  eckey_restart_ctx *rs_ctx;

  if (ctx == NULL)
    return;

  rs_ctx = ctx;
  mbedtls2_ecdsa_restart_free(&rs_ctx->ecdsa_rs);
  mbedtls2_ecdsa_free(&rs_ctx->ecdsa_ctx);

  mbedtls2_free(ctx);
}

static int eckey_verify_rs_wrap(void *ctx, mbedtls2_md_type_t md_alg,
                                const unsigned char *hash, size_t hash_len,
                                const unsigned char *sig, size_t sig_len,
                                void *rs_ctx) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  eckey_restart_ctx *rs = rs_ctx;

  /* Should never happen */
  if (rs == NULL)
    return (MBEDTLS2_ERR_PK_BAD_INPUT_DATA);

  /* set up our own sub-context if needed (that is, on first run) */
  if (rs->ecdsa_ctx.grp.pbits == 0)
    MBEDTLS2_MPI_CHK(
        mbedtls2_ecdsa_from_keypair(&rs->ecdsa_ctx, ctx));

  MBEDTLS2_MPI_CHK(ecdsa_verify_rs_wrap(
      &rs->ecdsa_ctx, md_alg, hash, hash_len, sig, sig_len, &rs->ecdsa_rs));

cleanup:
  return (ret);
}

static int eckey_sign_rs_wrap(void *ctx, mbedtls2_md_type_t md_alg,
                              const unsigned char *hash, size_t hash_len,
                              unsigned char *sig, size_t *sig_len,
                              int (*f_rng)(void *, unsigned char *, size_t),
                              void *p_rng, void *rs_ctx) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  eckey_restart_ctx *rs = rs_ctx;

  /* Should never happen */
  if (rs == NULL)
    return (MBEDTLS2_ERR_PK_BAD_INPUT_DATA);

  /* set up our own sub-context if needed (that is, on first run) */
  if (rs->ecdsa_ctx.grp.pbits == 0)
    MBEDTLS2_MPI_CHK(
        mbedtls2_ecdsa_from_keypair(&rs->ecdsa_ctx, ctx));

  MBEDTLS2_MPI_CHK(ecdsa_sign_rs_wrap(&rs->ecdsa_ctx, md_alg, hash,
                                             hash_len, sig, sig_len, f_rng,
                                             p_rng, &rs->ecdsa_rs));

cleanup:
  return (ret);
}
#endif /* MBEDTLS2_ECP_RESTARTABLE */
#endif /* MBEDTLS2_ECDSA_C */

static int eckey_check_pair(const void *pub, const void *prv) {
  return (mbedtls2_ecp_check_pub_priv(
      (const mbedtls2_ecp_keypair *)pub,
      (const mbedtls2_ecp_keypair *)prv));
}

static void *eckey_alloc_wrap(void) {
  void *ctx = mbedtls2_calloc(1, sizeof(mbedtls2_ecp_keypair));

  if (ctx != NULL)
    mbedtls2_ecp_keypair_init(ctx);

  return (ctx);
}

static void eckey_free_wrap(void *ctx) {
  mbedtls2_ecp_keypair_free((mbedtls2_ecp_keypair *)ctx);
  mbedtls2_free(ctx);
}

static void eckey_debug(const void *ctx, mbedtls2_pk_debug_item *items) {
  items->type = MBEDTLS2_PK_DEBUG_ECP;
  items->name = "eckey.Q";
  items->value = &(((mbedtls2_ecp_keypair *)ctx)->Q);
}

const mbedtls2_pk_info_t mbedtls2_eckey_info = {
    MBEDTLS2_PK_ECKEY,
    "EC",
    eckey_get_bitlen,
    eckey_can_do,
#if defined(MBEDTLS2_ECDSA_C)
    eckey_verify_wrap,
    eckey_sign_wrap,
#if defined(MBEDTLS2_ECP_RESTARTABLE)
    eckey_verify_rs_wrap,
    eckey_sign_rs_wrap,
#endif
#else  /* MBEDTLS2_ECDSA_C */
    NULL,
    NULL,
#endif /* MBEDTLS2_ECDSA_C */
    NULL,
    NULL,
    eckey_check_pair,
    eckey_alloc_wrap,
    eckey_free_wrap,
#if defined(MBEDTLS2_ECDSA_C) && defined(MBEDTLS2_ECP_RESTARTABLE)
    eckey_rs_alloc,
    eckey_rs_free,
#endif
    eckey_debug,
};

/*
 * EC key restricted to ECDH
 */
static int eckeydh_can_do(mbedtls2_pk_type_t type) {
  return (type == MBEDTLS2_PK_ECKEY ||
          type == MBEDTLS2_PK_ECKEY_DH);
}

const mbedtls2_pk_info_t mbedtls2_eckeydh_info = {
    MBEDTLS2_PK_ECKEY_DH,
    "EC_DH",
    eckey_get_bitlen, /* Same underlying key structure */
    eckeydh_can_do,
    NULL,
    NULL,
#if defined(MBEDTLS2_ECDSA_C) && defined(MBEDTLS2_ECP_RESTARTABLE)
    NULL,
    NULL,
#endif
    NULL,
    NULL,
    eckey_check_pair,
    eckey_alloc_wrap, /* Same underlying key structure */
    eckey_free_wrap,  /* Same underlying key structure */
#if defined(MBEDTLS2_ECDSA_C) && defined(MBEDTLS2_ECP_RESTARTABLE)
    NULL,
    NULL,
#endif
    eckey_debug, /* Same underlying key structure */
};
#endif /* MBEDTLS2_ECP_C */

#if defined(MBEDTLS2_ECDSA_C)
static int ecdsa_can_do(mbedtls2_pk_type_t type) {
  return (type == MBEDTLS2_PK_ECDSA);
}

#if defined(MBEDTLS2_USE_PSA_CRYPTO)
/*
 * An ASN.1 encoded signature is a sequence of two ASN.1 integers. Parse one of
 * those integers and convert it to the fixed-length encoding expected by PSA.
 */
static int extract_ecdsa_sig_int(unsigned char **from, const unsigned char *end,
                                 unsigned char *to, size_t to_len) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  size_t unpadded_len, padding_len;

  if ((ret = mbedtls2_asn1_get_tag(from, end, &unpadded_len,
                                          MBEDTLS2_ASN1_INTEGER)) != 0) {
    return (ret);
  }

  while (unpadded_len > 0 && **from == 0x00) {
    (*from)++;
    unpadded_len--;
  }

  if (unpadded_len > to_len || unpadded_len == 0)
    return (MBEDTLS2_ERR_ASN1_LENGTH_MISMATCH);

  padding_len = to_len - unpadded_len;
  memset(to, 0x00, padding_len);
  memcpy(to + padding_len, *from, unpadded_len);
  (*from) += unpadded_len;

  return (0);
}

/*
 * Convert a signature from an ASN.1 sequence of two integers
 * to a raw {r,s} buffer. Note: the provided sig buffer must be at least
 * twice as big as int_size.
 */
static int extract_ecdsa_sig(unsigned char **p, const unsigned char *end,
                             unsigned char *sig, size_t int_size) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  size_t tmp_size;

  if ((ret = mbedtls2_asn1_get_tag(p, end, &tmp_size,
                                          MBEDTLS2_ASN1_CONSTRUCTED |
                                              MBEDTLS2_ASN1_SEQUENCE)) !=
      0)
    return (ret);

  /* Extract r */
  if ((ret = extract_ecdsa_sig_int(p, end, sig, int_size)) != 0)
    return (ret);
  /* Extract s */
  if ((ret = extract_ecdsa_sig_int(p, end, sig + int_size, int_size)) != 0)
    return (ret);

  return (0);
}

static int ecdsa_verify_wrap(void *ctx_arg, mbedtls2_md_type_t md_alg,
                             const unsigned char *hash, size_t hash_len,
                             const unsigned char *sig, size_t sig_len) {
  mbedtls2_ecdsa_context *ctx = ctx_arg;
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  psa_key_attributes_t attributes = PSA_KEY_ATTRIBUTES_INIT;
  psa_key_id_t key_id = 0;
  psa_status_t status;
  mbedtls2_pk_context key;
  int key_len;
  /* see ECP_PUB_DER_MAX_BYTES in pkwrite.c */
  unsigned char buf[30 + 2 * MBEDTLS2_ECP_MAX_BYTES];
  unsigned char *p;
  mbedtls2_pk_info_t pk_info = mbedtls2_eckey_info;
  psa_algorithm_t psa_sig_md = PSA_ALG_ECDSA_ANY;
  size_t curve_bits;
  psa_ecc_family_t curve =
      mbedtls2_ecc_group_to_psa(ctx->grp.id, &curve_bits);
  const size_t signature_part_size = (ctx->grp.nbits + 7) / 8;
  ((void)md_alg);

  if (curve == 0)
    return (MBEDTLS2_ERR_PK_BAD_INPUT_DATA);

  /* mbedtls2_pk_write_pubkey() expects a full PK context;
   * re-construct one to make it happy */
  key.pk_info = &pk_info;
  key.pk_ctx = ctx;
  p = buf + sizeof(buf);
  key_len = mbedtls2_pk_write_pubkey(&p, buf, &key);
  if (key_len <= 0)
    return (MBEDTLS2_ERR_PK_BAD_INPUT_DATA);

  psa_set_key_type(&attributes, PSA_KEY_TYPE_ECC_PUBLIC_KEY(curve));
  psa_set_key_usage_flags(&attributes, PSA_KEY_USAGE_VERIFY_HASH);
  psa_set_key_algorithm(&attributes, psa_sig_md);

  status = psa_import_key(&attributes, buf + sizeof(buf) - key_len, key_len,
                          &key_id);
  if (status != PSA_SUCCESS) {
    ret = mbedtls2_psa_err_translate_pk(status);
    goto cleanup;
  }

  /* We don't need the exported key anymore and can
   * reuse its buffer for signature extraction. */
  if (2 * signature_part_size > sizeof(buf)) {
    ret = MBEDTLS2_ERR_PK_BAD_INPUT_DATA;
    goto cleanup;
  }

  p = (unsigned char *)sig;
  if ((ret = extract_ecdsa_sig(&p, sig + sig_len, buf, signature_part_size)) !=
      0) {
    goto cleanup;
  }

  if (psa_verify_hash(key_id, psa_sig_md, hash, hash_len, buf,
                      2 * signature_part_size) != PSA_SUCCESS) {
    ret = MBEDTLS2_ERR_ECP_VERIFY_FAILED;
    goto cleanup;
  }

  if (p != sig + sig_len) {
    ret = MBEDTLS2_ERR_PK_SIG_LEN_MISMATCH;
    goto cleanup;
  }
  ret = 0;

cleanup:
  psa_destroy_key(key_id);
  return (ret);
}
#else  /* MBEDTLS2_USE_PSA_CRYPTO */
static int ecdsa_verify_wrap(void *ctx, mbedtls2_md_type_t md_alg,
                             const unsigned char *hash, size_t hash_len,
                             const unsigned char *sig, size_t sig_len) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  ((void)md_alg);

  ret = mbedtls2_ecdsa_read_signature(
      (mbedtls2_ecdsa_context *)ctx, hash, hash_len, sig, sig_len);

  if (ret == MBEDTLS2_ERR_ECP_SIG_LEN_MISMATCH)
    return (MBEDTLS2_ERR_PK_SIG_LEN_MISMATCH);

  return (ret);
}
#endif /* MBEDTLS2_USE_PSA_CRYPTO */

static int ecdsa_sign_wrap(void *ctx, mbedtls2_md_type_t md_alg,
                           const unsigned char *hash, size_t hash_len,
                           unsigned char *sig, size_t *sig_len,
                           int (*f_rng)(void *, unsigned char *, size_t),
                           void *p_rng) {
  return (mbedtls2_ecdsa_write_signature(
      (mbedtls2_ecdsa_context *)ctx, md_alg, hash, hash_len, sig,
      sig_len, f_rng, p_rng));
}

#if defined(MBEDTLS2_ECP_RESTARTABLE)
static int ecdsa_verify_rs_wrap(void *ctx, mbedtls2_md_type_t md_alg,
                                const unsigned char *hash, size_t hash_len,
                                const unsigned char *sig, size_t sig_len,
                                void *rs_ctx) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  ((void)md_alg);

  ret = mbedtls2_ecdsa_read_signature_restartable(
      (mbedtls2_ecdsa_context *)ctx, hash, hash_len, sig, sig_len,
      (mbedtls2_ecdsa_restart_ctx *)rs_ctx);

  if (ret == MBEDTLS2_ERR_ECP_SIG_LEN_MISMATCH)
    return (MBEDTLS2_ERR_PK_SIG_LEN_MISMATCH);

  return (ret);
}

static int ecdsa_sign_rs_wrap(void *ctx, mbedtls2_md_type_t md_alg,
                              const unsigned char *hash, size_t hash_len,
                              unsigned char *sig, size_t *sig_len,
                              int (*f_rng)(void *, unsigned char *, size_t),
                              void *p_rng, void *rs_ctx) {
  return (mbedtls2_ecdsa_write_signature_restartable(
      (mbedtls2_ecdsa_context *)ctx, md_alg, hash, hash_len, sig,
      sig_len, f_rng, p_rng, (mbedtls2_ecdsa_restart_ctx *)rs_ctx));
}
#endif /* MBEDTLS2_ECP_RESTARTABLE */

static void *ecdsa_alloc_wrap(void) {
  void *ctx = mbedtls2_calloc(1, sizeof(mbedtls2_ecdsa_context));

  if (ctx != NULL)
    mbedtls2_ecdsa_init((mbedtls2_ecdsa_context *)ctx);

  return (ctx);
}

static void ecdsa_free_wrap(void *ctx) {
  mbedtls2_ecdsa_free((mbedtls2_ecdsa_context *)ctx);
  mbedtls2_free(ctx);
}

#if defined(MBEDTLS2_ECP_RESTARTABLE)
static void *ecdsa_rs_alloc(void) {
  void *ctx =
      mbedtls2_calloc(1, sizeof(mbedtls2_ecdsa_restart_ctx));

  if (ctx != NULL)
    mbedtls2_ecdsa_restart_init(ctx);

  return (ctx);
}

static void ecdsa_rs_free(void *ctx) {
  mbedtls2_ecdsa_restart_free(ctx);
  mbedtls2_free(ctx);
}
#endif /* MBEDTLS2_ECP_RESTARTABLE */

const mbedtls2_pk_info_t mbedtls2_ecdsa_info = {
    MBEDTLS2_PK_ECDSA,
    "ECDSA",
    eckey_get_bitlen, /* Compatible key structures */
    ecdsa_can_do,
    ecdsa_verify_wrap,
    ecdsa_sign_wrap,
#if defined(MBEDTLS2_ECP_RESTARTABLE)
    ecdsa_verify_rs_wrap,
    ecdsa_sign_rs_wrap,
#endif
    NULL,
    NULL,
    eckey_check_pair, /* Compatible key structures */
    ecdsa_alloc_wrap,
    ecdsa_free_wrap,
#if defined(MBEDTLS2_ECP_RESTARTABLE)
    ecdsa_rs_alloc,
    ecdsa_rs_free,
#endif
    eckey_debug, /* Compatible key structures */
};
#endif /* MBEDTLS2_ECDSA_C */

#if defined(MBEDTLS2_PK_RSA_ALT_SUPPORT)
/*
 * Support for alternative RSA-private implementations
 */

static int rsa_alt_can_do(mbedtls2_pk_type_t type) {
  return (type == MBEDTLS2_PK_RSA);
}

static size_t rsa_alt_get_bitlen(const void *ctx) {
  const mbedtls2_rsa_alt_context *rsa_alt =
      (const mbedtls2_rsa_alt_context *)ctx;

  return (8 * rsa_alt->key_len_func(rsa_alt->key));
}

static int rsa_alt_sign_wrap(void *ctx, mbedtls2_md_type_t md_alg,
                             const unsigned char *hash, size_t hash_len,
                             unsigned char *sig, size_t *sig_len,
                             int (*f_rng)(void *, unsigned char *, size_t),
                             void *p_rng) {
  mbedtls2_rsa_alt_context *rsa_alt =
      (mbedtls2_rsa_alt_context *)ctx;

#if SIZE_MAX > UINT_MAX
  if (UINT_MAX < hash_len)
    return (MBEDTLS2_ERR_PK_BAD_INPUT_DATA);
#endif /* SIZE_MAX > UINT_MAX */

  *sig_len = rsa_alt->key_len_func(rsa_alt->key);
  if (*sig_len > MBEDTLS2_PK_SIGNATURE_MAX_SIZE)
    return (MBEDTLS2_ERR_PK_BAD_INPUT_DATA);

  return (rsa_alt->sign_func(rsa_alt->key, f_rng, p_rng,
                             MBEDTLS2_RSA_PRIVATE, md_alg,
                             (unsigned int)hash_len, hash, sig));
}

static int rsa_alt_decrypt_wrap(void *ctx, const unsigned char *input,
                                size_t ilen, unsigned char *output,
                                size_t *olen, size_t osize,
                                int (*f_rng)(void *, unsigned char *, size_t),
                                void *p_rng) {
  mbedtls2_rsa_alt_context *rsa_alt =
      (mbedtls2_rsa_alt_context *)ctx;

  ((void)f_rng);
  ((void)p_rng);

  if (ilen != rsa_alt->key_len_func(rsa_alt->key))
    return (MBEDTLS2_ERR_RSA_BAD_INPUT_DATA);

  return (rsa_alt->decrypt_func(rsa_alt->key, MBEDTLS2_RSA_PRIVATE, olen,
                                input, output, osize));
}

#if defined(MBEDTLS2_RSA_C)
static int rsa_alt_check_pair(const void *pub, const void *prv) {
  unsigned char sig[MBEDTLS2_MPI_MAX_SIZE];
  unsigned char hash[32];
  size_t sig_len = 0;
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;

  if (rsa_alt_get_bitlen(prv) != rsa_get_bitlen(pub))
    return (MBEDTLS2_ERR_RSA_KEY_CHECK_FAILED);

  memset(hash, 0x2a, sizeof(hash));

  if ((ret = rsa_alt_sign_wrap((void *)prv, MBEDTLS2_MD_NONE, hash,
                               sizeof(hash), sig, &sig_len, NULL, NULL)) != 0) {
    return (ret);
  }

  if (rsa_verify_wrap((void *)pub, MBEDTLS2_MD_NONE, hash, sizeof(hash),
                      sig, sig_len) != 0) {
    return (MBEDTLS2_ERR_RSA_KEY_CHECK_FAILED);
  }

  return (0);
}
#endif /* MBEDTLS2_RSA_C */

static void *rsa_alt_alloc_wrap(void) {
  void *ctx =
      mbedtls2_calloc(1, sizeof(mbedtls2_rsa_alt_context));

  if (ctx != NULL)
    memset(ctx, 0, sizeof(mbedtls2_rsa_alt_context));

  return (ctx);
}

static void rsa_alt_free_wrap(void *ctx) {
  mbedtls2_platform_zeroize(ctx,
                                   sizeof(mbedtls2_rsa_alt_context));
  mbedtls2_free(ctx);
}

const mbedtls2_pk_info_t mbedtls2_rsa_alt_info = {
    MBEDTLS2_PK_RSA_ALT,
    "RSA-alt",
    rsa_alt_get_bitlen,
    rsa_alt_can_do,
    NULL,
    rsa_alt_sign_wrap,
#if defined(MBEDTLS2_ECDSA_C) && defined(MBEDTLS2_ECP_RESTARTABLE)
    NULL,
    NULL,
#endif
    rsa_alt_decrypt_wrap,
    NULL,
#if defined(MBEDTLS2_RSA_C)
    rsa_alt_check_pair,
#else
    NULL,
#endif
    rsa_alt_alloc_wrap,
    rsa_alt_free_wrap,
#if defined(MBEDTLS2_ECDSA_C) && defined(MBEDTLS2_ECP_RESTARTABLE)
    NULL,
    NULL,
#endif
    NULL,
};

#endif /* MBEDTLS2_PK_RSA_ALT_SUPPORT */

#if defined(MBEDTLS2_USE_PSA_CRYPTO)

static void *pk_opaque_alloc_wrap(void) {
  void *ctx = mbedtls2_calloc(1, sizeof(psa_key_id_t));

  /* no _init() function to call, an calloc() already zeroized */

  return (ctx);
}

static void pk_opaque_free_wrap(void *ctx) {
  mbedtls2_platform_zeroize(ctx, sizeof(psa_key_id_t));
  mbedtls2_free(ctx);
}

static size_t pk_opaque_get_bitlen(const void *ctx) {
  const psa_key_id_t *key = (const psa_key_id_t *)ctx;
  size_t bits;
  psa_key_attributes_t attributes = PSA_KEY_ATTRIBUTES_INIT;

  if (PSA_SUCCESS != psa_get_key_attributes(*key, &attributes))
    return (0);

  bits = psa_get_key_bits(&attributes);
  psa_reset_key_attributes(&attributes);
  return (bits);
}

static int pk_opaque_can_do(mbedtls2_pk_type_t type) {
  /* For now opaque PSA keys can only wrap ECC keypairs,
   * as checked by setup_psa().
   * Also, ECKEY_DH does not really make sense with the current API. */
  return (type == MBEDTLS2_PK_ECKEY || type == MBEDTLS2_PK_ECDSA);
}

#if defined(MBEDTLS2_ECDSA_C)

/*
 * Simultaneously convert and move raw MPI from the beginning of a buffer
 * to an ASN.1 MPI at the end of the buffer.
 * See also mbedtls2_asn1_write_mpi().
 *
 * p: pointer to the end of the output buffer
 * start: start of the output buffer, and also of the mpi to write at the end
 * n_len: length of the mpi to read from start
 */
static int asn1_write_mpibuf(unsigned char **p, unsigned char *start,
                             size_t n_len) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  size_t len = 0;

  if ((size_t)(*p - start) < n_len)
    return (MBEDTLS2_ERR_ASN1_BUF_TOO_SMALL);

  len = n_len;
  *p -= len;
  memmove(*p, start, len);

  /* ASN.1 DER encoding requires minimal length, so skip leading 0s.
   * Neither r nor s should be 0, but as a failsafe measure, still detect
   * that rather than overflowing the buffer in case of a PSA error. */
  while (len > 0 && **p == 0x00) {
    ++(*p);
    --len;
  }

  /* this is only reached if the signature was invalid */
  if (len == 0)
    return (MBEDTLS2_ERR_PK_HW_ACCEL_FAILED);

  /* if the msb is 1, ASN.1 requires that we prepend a 0.
   * Neither r nor s can be 0, so we can assume len > 0 at all times. */
  if (**p & 0x80) {
    if (*p - start < 1)
      return (MBEDTLS2_ERR_ASN1_BUF_TOO_SMALL);

    *--(*p) = 0x00;
    len += 1;
  }

  MBEDTLS2_ASN1_CHK_ADD(len,
                               mbedtls2_asn1_write_len(p, start, len));
  MBEDTLS2_ASN1_CHK_ADD(
      len,
      mbedtls2_asn1_write_tag(p, start, MBEDTLS2_ASN1_INTEGER));

  return ((int)len);
}

/* Transcode signature from PSA format to ASN.1 sequence.
 * See ecdsa_signature_to_asn1 in ecdsa.c, but with byte buffers instead of
 * MPIs, and in-place.
 *
 * [in/out] sig: the signature pre- and post-transcoding
 * [in/out] sig_len: signature length pre- and post-transcoding
 * [int] buf_len: the available size the in/out buffer
 */
static int pk_ecdsa_sig_asn1_from_psa(unsigned char *sig, size_t *sig_len,
                                      size_t buf_len) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  size_t len = 0;
  const size_t rs_len = *sig_len / 2;
  unsigned char *p = sig + buf_len;

  MBEDTLS2_ASN1_CHK_ADD(len,
                               asn1_write_mpibuf(&p, sig + rs_len, rs_len));
  MBEDTLS2_ASN1_CHK_ADD(len, asn1_write_mpibuf(&p, sig, rs_len));

  MBEDTLS2_ASN1_CHK_ADD(len,
                               mbedtls2_asn1_write_len(&p, sig, len));
  MBEDTLS2_ASN1_CHK_ADD(
      len, mbedtls2_asn1_write_tag(&p, sig,
                                          MBEDTLS2_ASN1_CONSTRUCTED |
                                              MBEDTLS2_ASN1_SEQUENCE));

  memmove(sig, p, len);
  *sig_len = len;

  return (0);
}

#endif /* MBEDTLS2_ECDSA_C */

static int pk_opaque_sign_wrap(void *ctx, mbedtls2_md_type_t md_alg,
                               const unsigned char *hash, size_t hash_len,
                               unsigned char *sig, size_t *sig_len,
                               int (*f_rng)(void *, unsigned char *, size_t),
                               void *p_rng) {
#if !defined(MBEDTLS2_ECDSA_C)
  ((void)ctx);
  ((void)md_alg);
  ((void)hash);
  ((void)hash_len);
  ((void)sig);
  ((void)sig_len);
  ((void)f_rng);
  ((void)p_rng);
  return (MBEDTLS2_ERR_PK_FEATURE_UNAVAILABLE);
#else  /* !MBEDTLS2_ECDSA_C */
  const psa_key_id_t *key = (const psa_key_id_t *)ctx;
  psa_key_attributes_t attributes = PSA_KEY_ATTRIBUTES_INIT;
  psa_algorithm_t alg = PSA_ALG_ECDSA(mbedtls2_psa_translate_md(md_alg));
  size_t buf_len;
  psa_status_t status;

  /* PSA has its own RNG */
  (void)f_rng;
  (void)p_rng;

  /* PSA needs an output buffer of known size, but our API doesn't provide
   * that information. Assume that the buffer is large enough for a
   * maximal-length signature with that key (otherwise the application is
   * buggy anyway). */
  status = psa_get_key_attributes(*key, &attributes);
  if (status != PSA_SUCCESS)
    return (mbedtls2_psa_err_translate_pk(status));
  buf_len = MBEDTLS2_ECDSA_MAX_SIG_LEN(psa_get_key_bits(&attributes));
  psa_reset_key_attributes(&attributes);
  if (buf_len > MBEDTLS2_PK_SIGNATURE_MAX_SIZE)
    return (MBEDTLS2_ERR_PK_BAD_INPUT_DATA);

  /* make the signature */
  status = psa_sign_hash(*key, alg, hash, hash_len, sig, buf_len, sig_len);
  if (status != PSA_SUCCESS)
    return (mbedtls2_psa_err_translate_pk(status));

  /* transcode it to ASN.1 sequence */
  return (pk_ecdsa_sig_asn1_from_psa(sig, sig_len, buf_len));
#endif /* !MBEDTLS2_ECDSA_C */
}

const mbedtls2_pk_info_t mbedtls2_pk_opaque_info = {
    MBEDTLS2_PK_OPAQUE,
    "Opaque",
    pk_opaque_get_bitlen,
    pk_opaque_can_do,
    NULL, /* verify - will be done later */
    pk_opaque_sign_wrap,
#if defined(MBEDTLS2_ECDSA_C) && defined(MBEDTLS2_ECP_RESTARTABLE)
    NULL, /* restartable verify - not relevant */
    NULL, /* restartable sign - not relevant */
#endif
    NULL, /* decrypt - will be done later */
    NULL, /* encrypt - will be done later */
    NULL, /* check_pair - could be done later or left NULL */
    pk_opaque_alloc_wrap,
    pk_opaque_free_wrap,
#if defined(MBEDTLS2_ECDSA_C) && defined(MBEDTLS2_ECP_RESTARTABLE)
    NULL, /* restart alloc - not relevant */
    NULL, /* restart free - not relevant */
#endif
    NULL, /* debug - could be done later, or even left NULL */
};

#endif /* MBEDTLS2_USE_PSA_CRYPTO */

#endif /* MBEDTLS2_PK_C */

/*
 *  PSA RSA layer on top of Mbed TLS crypto
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

#include "common.h"

#if defined(MBEDTLS2_PSA_CRYPTO_C)

#include "psa_crypto_core.h"
#include "psa_crypto_hash.h"
#include "psa_crypto_random_impl.h"
#include "psa_crypto_rsa.h"
#include <psa/crypto.h>

#include "mbedtls2/platform.h"
#include <stdlib.h>
#include <string.h>
#if !defined(MBEDTLS2_PLATFORM_C)
#define mbedtls2_calloc calloc
#define mbedtls2_free free
#endif

#include <mbedtls2/error.h>
#include <mbedtls2/pk.h>
#include <mbedtls2/pk_internal.h>
#include <mbedtls2/rsa.h>

#if defined(MBEDTLS2_PSA_BUILTIN_ALG_RSA_PKCS1V15_CRYPT) ||             \
    defined(MBEDTLS2_PSA_BUILTIN_ALG_RSA_OAEP) ||                       \
    defined(MBEDTLS2_PSA_BUILTIN_ALG_RSA_PKCS1V15_SIGN) ||              \
    defined(MBEDTLS2_PSA_BUILTIN_ALG_RSA_PSS) ||                        \
    defined(MBEDTLS2_PSA_BUILTIN_KEY_TYPE_RSA_KEY_PAIR) ||              \
    defined(MBEDTLS2_PSA_BUILTIN_KEY_TYPE_RSA_PUBLIC_KEY)

/* Mbed TLS doesn't support non-byte-aligned key sizes (i.e. key sizes
 * that are not a multiple of 8) well. For example, there is only
 * mbedtls2_rsa_get_len(), which returns a number of bytes, and no
 * way to return the exact bit size of a key.
 * To keep things simple, reject non-byte-aligned key sizes. */
static psa_status_t
psa_check_rsa_key_byte_aligned(const mbedtls2_rsa_context *rsa) {
  mbedtls2_mpi n;
  psa_status_t status;
  mbedtls2_mpi_init(&n);
  status = mbedtls2_to_psa_error(
      mbedtls2_rsa_export(rsa, &n, NULL, NULL, NULL, NULL));
  if (status == PSA_SUCCESS) {
    if (mbedtls2_mpi_bitlen(&n) % 8 != 0)
      status = PSA_ERROR_NOT_SUPPORTED;
  }
  mbedtls2_mpi_free(&n);
  return (status);
}

psa_status_t mbedtls2_psa_rsa_load_representation(
    psa_key_type_t type, const uint8_t *data, size_t data_length,
    mbedtls2_rsa_context **p_rsa) {
  psa_status_t status;
  mbedtls2_pk_context ctx;
  size_t bits;
  mbedtls2_pk_init(&ctx);

  /* Parse the data. */
  if (PSA_KEY_TYPE_IS_KEY_PAIR(type))
    status = mbedtls2_to_psa_error(
        mbedtls2_pk_parse_key(&ctx, data, data_length, NULL, 0));
  else
    status = mbedtls2_to_psa_error(
        mbedtls2_pk_parse_public_key(&ctx, data, data_length));
  if (status != PSA_SUCCESS)
    goto exit;

  /* We have something that the pkparse module recognizes. If it is a
   * valid RSA key, store it. */
  if (mbedtls2_pk_get_type(&ctx) != MBEDTLS2_PK_RSA) {
    status = PSA_ERROR_INVALID_ARGUMENT;
    goto exit;
  }

  /* The size of an RSA key doesn't have to be a multiple of 8. Mbed TLS
   * supports non-byte-aligned key sizes, but not well. For example,
   * mbedtls2_rsa_get_len() returns the key size in bytes, not in bits.
   */
  bits = PSA_BYTES_TO_BITS(
      mbedtls2_rsa_get_len(mbedtls2_pk_rsa(ctx)));
  if (bits > PSA_VENDOR_RSA_MAX_KEY_BITS) {
    status = PSA_ERROR_NOT_SUPPORTED;
    goto exit;
  }
  status = psa_check_rsa_key_byte_aligned(mbedtls2_pk_rsa(ctx));
  if (status != PSA_SUCCESS)
    goto exit;

  /* Copy out the pointer to the RSA context, and reset the PK context
   * such that pk_free doesn't free the RSA context we just grabbed. */
  *p_rsa = mbedtls2_pk_rsa(ctx);
  ctx.pk_info = NULL;

exit:
  mbedtls2_pk_free(&ctx);
  return (status);
}
#endif /* defined(MBEDTLS2_PSA_BUILTIN_ALG_RSA_PKCS1V15_CRYPT) ||       \
        * defined(MBEDTLS2_PSA_BUILTIN_ALG_RSA_OAEP) ||                 \
        * defined(MBEDTLS2_PSA_BUILTIN_ALG_RSA_PKCS1V15_SIGN) ||        \
        * defined(MBEDTLS2_PSA_BUILTIN_ALG_RSA_PSS) ||                  \
        * defined(MBEDTLS2_PSA_BUILTIN_KEY_TYPE_RSA_KEY_PAIR) ||        \
        * defined(MBEDTLS2_PSA_BUILTIN_KEY_TYPE_RSA_PUBLIC_KEY) */

#if defined(MBEDTLS2_PSA_BUILTIN_KEY_TYPE_RSA_KEY_PAIR) ||              \
    defined(MBEDTLS2_PSA_BUILTIN_KEY_TYPE_RSA_PUBLIC_KEY)

psa_status_t
mbedtls2_psa_rsa_import_key(const psa_key_attributes_t *attributes,
                                   const uint8_t *data, size_t data_length,
                                   uint8_t *key_buffer, size_t key_buffer_size,
                                   size_t *key_buffer_length, size_t *bits) {
  psa_status_t status;
  mbedtls2_rsa_context *rsa = NULL;

  /* Parse input */
  status = mbedtls2_psa_rsa_load_representation(attributes->core.type,
                                                       data, data_length, &rsa);
  if (status != PSA_SUCCESS)
    goto exit;

  *bits = (psa_key_bits_t)PSA_BYTES_TO_BITS(mbedtls2_rsa_get_len(rsa));

  /* Re-export the data to PSA export format, such that we can store export
   * representation in the key slot. Export representation in case of RSA is
   * the smallest representation that's allowed as input, so a straight-up
   * allocation of the same size as the input buffer will be large enough. */
  status =
      mbedtls2_psa_rsa_export_key(attributes->core.type, rsa, key_buffer,
                                         key_buffer_size, key_buffer_length);
exit:
  /* Always free the RSA object */
  mbedtls2_rsa_free(rsa);
  mbedtls2_free(rsa);

  return (status);
}

psa_status_t mbedtls2_psa_rsa_export_key(
    psa_key_type_t type, mbedtls2_rsa_context *rsa, uint8_t *data,
    size_t data_size, size_t *data_length) {
#if defined(MBEDTLS2_PK_WRITE_C)
  int ret;
  mbedtls2_pk_context pk;
  uint8_t *pos = data + data_size;

  mbedtls2_pk_init(&pk);
  pk.pk_info = &mbedtls2_rsa_info;
  pk.pk_ctx = rsa;

  /* PSA Crypto API defines the format of an RSA key as a DER-encoded
   * representation of the non-encrypted PKCS#1 RSAPrivateKey for a
   * private key and of the RFC3279 RSAPublicKey for a public key. */
  if (PSA_KEY_TYPE_IS_KEY_PAIR(type))
    ret = mbedtls2_pk_write_key_der(&pk, data, data_size);
  else
    ret = mbedtls2_pk_write_pubkey(&pos, data, &pk);

  if (ret < 0) {
    /* Clean up in case pk_write failed halfway through. */
    memset(data, 0, data_size);
    return (mbedtls2_to_psa_error(ret));
  }

  /* The mbedtls2_pk_xxx functions write to the end of the buffer.
   * Move the data to the beginning and erase remaining data
   * at the original location. */
  if (2 * (size_t)ret <= data_size) {
    memcpy(data, data + data_size - ret, ret);
    memset(data + data_size - ret, 0, ret);
  } else if ((size_t)ret < data_size) {
    memmove(data, data + data_size - ret, ret);
    memset(data + ret, 0, data_size - ret);
  }

  *data_length = ret;
  return (PSA_SUCCESS);
#else
  (void)type;
  (void)rsa;
  (void)data;
  (void)data_size;
  (void)data_length;
  return (PSA_ERROR_NOT_SUPPORTED);
#endif /* MBEDTLS2_PK_WRITE_C */
}

psa_status_t mbedtls2_psa_rsa_export_public_key(
    const psa_key_attributes_t *attributes, const uint8_t *key_buffer,
    size_t key_buffer_size, uint8_t *data, size_t data_size,
    size_t *data_length) {
  psa_status_t status = PSA_ERROR_CORRUPTION_DETECTED;
  mbedtls2_rsa_context *rsa = NULL;

  status = mbedtls2_psa_rsa_load_representation(
      attributes->core.type, key_buffer, key_buffer_size, &rsa);
  if (status != PSA_SUCCESS)
    return (status);

  status = mbedtls2_psa_rsa_export_key(PSA_KEY_TYPE_RSA_PUBLIC_KEY, rsa,
                                              data, data_size, data_length);

  mbedtls2_rsa_free(rsa);
  mbedtls2_free(rsa);

  return (status);
}
#endif /* defined(MBEDTLS2_PSA_BUILTIN_KEY_TYPE_RSA_KEY_PAIR) ||        \
        * defined(MBEDTLS2_PSA_BUILTIN_KEY_TYPE_RSA_PUBLIC_KEY) */

#if defined(MBEDTLS2_PSA_BUILTIN_KEY_TYPE_RSA_KEY_PAIR) &&              \
    defined(MBEDTLS2_GENPRIME)
static psa_status_t psa_rsa_read_exponent(const uint8_t *domain_parameters,
                                          size_t domain_parameters_size,
                                          int *exponent) {
  size_t i;
  uint32_t acc = 0;

  if (domain_parameters_size == 0) {
    *exponent = 65537;
    return (PSA_SUCCESS);
  }

  /* Mbed TLS encodes the public exponent as an int. For simplicity, only
   * support values that fit in a 32-bit integer, which is larger than
   * int on just about every platform anyway. */
  if (domain_parameters_size > sizeof(acc))
    return (PSA_ERROR_NOT_SUPPORTED);
  for (i = 0; i < domain_parameters_size; i++)
    acc = (acc << 8) | domain_parameters[i];
  if (acc > INT_MAX)
    return (PSA_ERROR_NOT_SUPPORTED);
  *exponent = acc;
  return (PSA_SUCCESS);
}

psa_status_t mbedtls2_psa_rsa_generate_key(
    const psa_key_attributes_t *attributes, uint8_t *key_buffer,
    size_t key_buffer_size, size_t *key_buffer_length) {
  psa_status_t status;
  mbedtls2_rsa_context rsa;
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  int exponent;

  status = psa_rsa_read_exponent(attributes->domain_parameters,
                                 attributes->domain_parameters_size, &exponent);
  if (status != PSA_SUCCESS)
    return (status);

  mbedtls2_rsa_init(&rsa, MBEDTLS2_RSA_PKCS_V15,
                           MBEDTLS2_MD_NONE);
  ret = mbedtls2_rsa_gen_key(
      &rsa, mbedtls2_psa_get_random, MBEDTLS2_PSA_RANDOM_STATE,
      (unsigned int)attributes->core.bits, exponent);
  if (ret != 0)
    return (mbedtls2_to_psa_error(ret));

  status = mbedtls2_psa_rsa_export_key(attributes->core.type, &rsa,
                                              key_buffer, key_buffer_size,
                                              key_buffer_length);
  mbedtls2_rsa_free(&rsa);

  return (status);
}
#endif /* defined(MBEDTLS2_PSA_BUILTIN_KEY_TYPE_RSA_KEY_PAIR)           \
        * defined(MBEDTLS2_GENPRIME) */

/****************************************************************/
/* Sign/verify hashes */
/****************************************************************/

#if defined(MBEDTLS2_PSA_BUILTIN_ALG_RSA_PKCS1V15_SIGN) ||              \
    defined(MBEDTLS2_PSA_BUILTIN_ALG_RSA_PSS)

/* Decode the hash algorithm from alg and store the mbedtls encoding in
 * md_alg. Verify that the hash length is acceptable. */
static psa_status_t psa_rsa_decode_md_type(psa_algorithm_t alg,
                                           size_t hash_length,
                                           mbedtls2_md_type_t *md_alg) {
  psa_algorithm_t hash_alg = PSA_ALG_SIGN_GET_HASH(alg);
  const mbedtls2_md_info_t *md_info =
      mbedtls2_md_info_from_psa(hash_alg);
  *md_alg = mbedtls2_md_get_type(md_info);

  /* The Mbed TLS RSA module uses an unsigned int for hash length
   * parameters. Validate that it fits so that we don't risk an
   * overflow later. */
#if SIZE_MAX > UINT_MAX
  if (hash_length > UINT_MAX)
    return (PSA_ERROR_INVALID_ARGUMENT);
#endif

  /* For signatures using a hash, the hash length must be correct. */
  if (alg != PSA_ALG_RSA_PKCS1V15_SIGN_RAW) {
    if (md_info == NULL)
      return (PSA_ERROR_NOT_SUPPORTED);
    if (mbedtls2_md_get_size(md_info) != hash_length)
      return (PSA_ERROR_INVALID_ARGUMENT);
  }

  return (PSA_SUCCESS);
}

psa_status_t mbedtls2_psa_rsa_sign_hash(
    const psa_key_attributes_t *attributes, const uint8_t *key_buffer,
    size_t key_buffer_size, psa_algorithm_t alg, const uint8_t *hash,
    size_t hash_length, uint8_t *signature, size_t signature_size,
    size_t *signature_length) {
  psa_status_t status = PSA_ERROR_CORRUPTION_DETECTED;
  mbedtls2_rsa_context *rsa = NULL;
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  mbedtls2_md_type_t md_alg;

  status = mbedtls2_psa_rsa_load_representation(
      attributes->core.type, key_buffer, key_buffer_size, &rsa);
  if (status != PSA_SUCCESS)
    return (status);

  status = psa_rsa_decode_md_type(alg, hash_length, &md_alg);
  if (status != PSA_SUCCESS)
    goto exit;

  if (signature_size < mbedtls2_rsa_get_len(rsa)) {
    status = PSA_ERROR_BUFFER_TOO_SMALL;
    goto exit;
  }

#if defined(MBEDTLS2_PSA_BUILTIN_ALG_RSA_PKCS1V15_SIGN)
  if (PSA_ALG_IS_RSA_PKCS1V15_SIGN(alg)) {
    mbedtls2_rsa_set_padding(rsa, MBEDTLS2_RSA_PKCS_V15,
                                    MBEDTLS2_MD_NONE);
    ret = mbedtls2_rsa_pkcs1_sign(
        rsa, mbedtls2_psa_get_random, MBEDTLS2_PSA_RANDOM_STATE,
        MBEDTLS2_RSA_PRIVATE, md_alg, (unsigned int)hash_length, hash,
        signature);
  } else
#endif /* MBEDTLS2_PSA_BUILTIN_ALG_RSA_PKCS1V15_SIGN */
#if defined(MBEDTLS2_PSA_BUILTIN_ALG_RSA_PSS)
      if (PSA_ALG_IS_RSA_PSS(alg)) {
    mbedtls2_rsa_set_padding(rsa, MBEDTLS2_RSA_PKCS_V21, md_alg);
    ret = mbedtls2_rsa_rsassa_pss_sign(
        rsa, mbedtls2_psa_get_random, MBEDTLS2_PSA_RANDOM_STATE,
        MBEDTLS2_RSA_PRIVATE, MBEDTLS2_MD_NONE,
        (unsigned int)hash_length, hash, signature);
  } else
#endif /* MBEDTLS2_PSA_BUILTIN_ALG_RSA_PSS */
  {
    status = PSA_ERROR_INVALID_ARGUMENT;
    goto exit;
  }

  if (ret == 0)
    *signature_length = mbedtls2_rsa_get_len(rsa);
  status = mbedtls2_to_psa_error(ret);

exit:
  mbedtls2_rsa_free(rsa);
  mbedtls2_free(rsa);

  return (status);
}

#if defined(MBEDTLS2_PSA_BUILTIN_ALG_RSA_PSS)
static int rsa_pss_expected_salt_len(psa_algorithm_t alg,
                                     const mbedtls2_rsa_context *rsa,
                                     size_t hash_length) {
  if (PSA_ALG_IS_RSA_PSS_ANY_SALT(alg))
    return (MBEDTLS2_RSA_SALT_LEN_ANY);
  /* Otherwise: standard salt length, i.e. largest possible salt length
   * up to the hash length. */
  int klen = (int)mbedtls2_rsa_get_len(rsa); // known to fit
  int hlen = (int)hash_length;                      // known to fit
  int room = klen - 2 - hlen;
  if (room < 0)
    return (0); // there is no valid signature in this case anyway
  else if (room > hlen)
    return (hlen);
  else
    return (room);
}
#endif /* MBEDTLS2_PSA_BUILTIN_ALG_RSA_PSS */

psa_status_t mbedtls2_psa_rsa_verify_hash(
    const psa_key_attributes_t *attributes, const uint8_t *key_buffer,
    size_t key_buffer_size, psa_algorithm_t alg, const uint8_t *hash,
    size_t hash_length, const uint8_t *signature, size_t signature_length) {
  psa_status_t status = PSA_ERROR_CORRUPTION_DETECTED;
  mbedtls2_rsa_context *rsa = NULL;
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  mbedtls2_md_type_t md_alg;

  status = mbedtls2_psa_rsa_load_representation(
      attributes->core.type, key_buffer, key_buffer_size, &rsa);
  if (status != PSA_SUCCESS)
    goto exit;

  status = psa_rsa_decode_md_type(alg, hash_length, &md_alg);
  if (status != PSA_SUCCESS)
    goto exit;

  if (signature_length != mbedtls2_rsa_get_len(rsa)) {
    status = PSA_ERROR_INVALID_SIGNATURE;
    goto exit;
  }

#if defined(MBEDTLS2_PSA_BUILTIN_ALG_RSA_PKCS1V15_SIGN)
  if (PSA_ALG_IS_RSA_PKCS1V15_SIGN(alg)) {
    mbedtls2_rsa_set_padding(rsa, MBEDTLS2_RSA_PKCS_V15,
                                    MBEDTLS2_MD_NONE);
    ret = mbedtls2_rsa_pkcs1_verify(
        rsa, mbedtls2_psa_get_random, MBEDTLS2_PSA_RANDOM_STATE,
        MBEDTLS2_RSA_PUBLIC, md_alg, (unsigned int)hash_length, hash,
        signature);
  } else
#endif /* MBEDTLS2_PSA_BUILTIN_ALG_RSA_PKCS1V15_SIGN */
#if defined(MBEDTLS2_PSA_BUILTIN_ALG_RSA_PSS)
      if (PSA_ALG_IS_RSA_PSS(alg)) {
    int slen = rsa_pss_expected_salt_len(alg, rsa, hash_length);
    mbedtls2_rsa_set_padding(rsa, MBEDTLS2_RSA_PKCS_V21, md_alg);
    ret = mbedtls2_rsa_rsassa_pss_verify_ext(
        rsa, mbedtls2_psa_get_random, MBEDTLS2_PSA_RANDOM_STATE,
        MBEDTLS2_RSA_PUBLIC, md_alg, (unsigned int)hash_length, hash,
        md_alg, slen, signature);
  } else
#endif /* MBEDTLS2_PSA_BUILTIN_ALG_RSA_PSS */
  {
    status = PSA_ERROR_INVALID_ARGUMENT;
    goto exit;
  }

  /* Mbed TLS distinguishes "invalid padding" from "valid padding but
   * the rest of the signature is invalid". This has little use in
   * practice and PSA doesn't report this distinction. */
  status = (ret == MBEDTLS2_ERR_RSA_INVALID_PADDING)
               ? PSA_ERROR_INVALID_SIGNATURE
               : mbedtls2_to_psa_error(ret);

exit:
  mbedtls2_rsa_free(rsa);
  mbedtls2_free(rsa);

  return (status);
}

#endif /* defined(MBEDTLS2_PSA_BUILTIN_ALG_RSA_PKCS1V15_SIGN) ||        \
        * defined(MBEDTLS2_PSA_BUILTIN_ALG_RSA_PSS) */

#endif /* MBEDTLS2_PSA_CRYPTO_C */

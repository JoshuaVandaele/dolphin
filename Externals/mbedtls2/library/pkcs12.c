/*
 *  PKCS#12 Personal Information Exchange Syntax
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
/*
 *  The PKCS #12 Personal Information Exchange Syntax Standard v1.1
 *
 *  http://www.rsa.com/rsalabs/pkcs/files/h11301-wp-pkcs-12v1-1-personal-information-exchange-syntax.pdf
 *  ftp://ftp.rsasecurity.com/pub/pkcs/pkcs-12/pkcs-12v1-1.asn
 */

#include "common.h"

#if defined(MBEDTLS2_PKCS12_C)

#include "mbedtls2/asn1.h"
#include "mbedtls2/cipher.h"
#include "mbedtls2/error.h"
#include "mbedtls2/pkcs12.h"
#include "mbedtls2/platform_util.h"

#include <string.h>

#if defined(MBEDTLS2_ARC4_C)
#include "mbedtls2/arc4.h"
#endif

#if defined(MBEDTLS2_DES_C)
#include "mbedtls2/des.h"
#endif

#if defined(MBEDTLS2_ASN1_PARSE_C)

static int pkcs12_parse_pbe_params(mbedtls2_asn1_buf *params,
                                   mbedtls2_asn1_buf *salt,
                                   int *iterations) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  unsigned char **p = &params->p;
  const unsigned char *end = params->p + params->len;

  /*
   *  pkcs-12PbeParams ::= SEQUENCE {
   *    salt          OCTET STRING,
   *    iterations    INTEGER
   *  }
   *
   */
  if (params->tag !=
      (MBEDTLS2_ASN1_CONSTRUCTED | MBEDTLS2_ASN1_SEQUENCE))
    return (
        MBEDTLS2_ERROR_ADD(MBEDTLS2_ERR_PKCS12_PBE_INVALID_FORMAT,
                                  MBEDTLS2_ERR_ASN1_UNEXPECTED_TAG));

  if ((ret = mbedtls2_asn1_get_tag(
           p, end, &salt->len, MBEDTLS2_ASN1_OCTET_STRING)) != 0)
    return (MBEDTLS2_ERROR_ADD(
        MBEDTLS2_ERR_PKCS12_PBE_INVALID_FORMAT, ret));

  salt->p = *p;
  *p += salt->len;

  if ((ret = mbedtls2_asn1_get_int(p, end, iterations)) != 0)
    return (MBEDTLS2_ERROR_ADD(
        MBEDTLS2_ERR_PKCS12_PBE_INVALID_FORMAT, ret));

  if (*p != end)
    return (
        MBEDTLS2_ERROR_ADD(MBEDTLS2_ERR_PKCS12_PBE_INVALID_FORMAT,
                                  MBEDTLS2_ERR_ASN1_LENGTH_MISMATCH));

  return (0);
}

#define PKCS12_MAX_PWDLEN 128

static int pkcs12_pbe_derive_key_iv(mbedtls2_asn1_buf *pbe_params,
                                    mbedtls2_md_type_t md_type,
                                    const unsigned char *pwd, size_t pwdlen,
                                    unsigned char *key, size_t keylen,
                                    unsigned char *iv, size_t ivlen) {
  int ret, iterations = 0;
  mbedtls2_asn1_buf salt;
  size_t i;
  unsigned char unipwd[PKCS12_MAX_PWDLEN * 2 + 2];

  if (pwdlen > PKCS12_MAX_PWDLEN)
    return (MBEDTLS2_ERR_PKCS12_BAD_INPUT_DATA);

  memset(&salt, 0, sizeof(mbedtls2_asn1_buf));
  memset(&unipwd, 0, sizeof(unipwd));

  if ((ret = pkcs12_parse_pbe_params(pbe_params, &salt, &iterations)) != 0)
    return (ret);

  for (i = 0; i < pwdlen; i++)
    unipwd[i * 2 + 1] = pwd[i];

  if ((ret = mbedtls2_pkcs12_derivation(
           key, keylen, unipwd, pwdlen * 2 + 2, salt.p, salt.len, md_type,
           MBEDTLS2_PKCS12_DERIVE_KEY, iterations)) != 0) {
    return (ret);
  }

  if (iv == NULL || ivlen == 0)
    return (0);

  if ((ret = mbedtls2_pkcs12_derivation(
           iv, ivlen, unipwd, pwdlen * 2 + 2, salt.p, salt.len, md_type,
           MBEDTLS2_PKCS12_DERIVE_IV, iterations)) != 0) {
    return (ret);
  }
  return (0);
}

#undef PKCS12_MAX_PWDLEN

int mbedtls2_pkcs12_pbe_sha1_rc4_128(
    mbedtls2_asn1_buf *pbe_params, int mode, const unsigned char *pwd,
    size_t pwdlen, const unsigned char *data, size_t len,
    unsigned char *output) {
#if !defined(MBEDTLS2_ARC4_C)
  ((void)pbe_params);
  ((void)mode);
  ((void)pwd);
  ((void)pwdlen);
  ((void)data);
  ((void)len);
  ((void)output);
  return (MBEDTLS2_ERR_PKCS12_FEATURE_UNAVAILABLE);
#else
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  unsigned char key[16];
  mbedtls2_arc4_context ctx;
  ((void)mode);

  mbedtls2_arc4_init(&ctx);

  if ((ret = pkcs12_pbe_derive_key_iv(pbe_params, MBEDTLS2_MD_SHA1, pwd,
                                      pwdlen, key, 16, NULL, 0)) != 0) {
    return (ret);
  }

  mbedtls2_arc4_setup(&ctx, key, 16);
  if ((ret = mbedtls2_arc4_crypt(&ctx, len, data, output)) != 0)
    goto exit;

exit:
  mbedtls2_platform_zeroize(key, sizeof(key));
  mbedtls2_arc4_free(&ctx);

  return (ret);
#endif /* MBEDTLS2_ARC4_C */
}

int mbedtls2_pkcs12_pbe(mbedtls2_asn1_buf *pbe_params, int mode,
                               mbedtls2_cipher_type_t cipher_type,
                               mbedtls2_md_type_t md_type,
                               const unsigned char *pwd, size_t pwdlen,
                               const unsigned char *data, size_t len,
                               unsigned char *output) {
  int ret, keylen = 0;
  unsigned char key[32];
  unsigned char iv[16];
  const mbedtls2_cipher_info_t *cipher_info;
  mbedtls2_cipher_context_t cipher_ctx;
  size_t olen = 0;

  if (pwd == NULL && pwdlen != 0)
    return (MBEDTLS2_ERR_PKCS12_BAD_INPUT_DATA);

  cipher_info = mbedtls2_cipher_info_from_type(cipher_type);
  if (cipher_info == NULL)
    return (MBEDTLS2_ERR_PKCS12_FEATURE_UNAVAILABLE);

  keylen = cipher_info->key_bitlen / 8;

  if ((ret = pkcs12_pbe_derive_key_iv(pbe_params, md_type, pwd, pwdlen, key,
                                      keylen, iv, cipher_info->iv_size)) != 0) {
    return (ret);
  }

  mbedtls2_cipher_init(&cipher_ctx);

  if ((ret = mbedtls2_cipher_setup(&cipher_ctx, cipher_info)) != 0)
    goto exit;

  if ((ret = mbedtls2_cipher_setkey(
           &cipher_ctx, key, 8 * keylen, (mbedtls2_operation_t)mode)) !=
      0)
    goto exit;

  if ((ret = mbedtls2_cipher_set_iv(&cipher_ctx, iv,
                                           cipher_info->iv_size)) != 0)
    goto exit;

  if ((ret = mbedtls2_cipher_reset(&cipher_ctx)) != 0)
    goto exit;

  if ((ret = mbedtls2_cipher_update(&cipher_ctx, data, len, output,
                                           &olen)) != 0) {
    goto exit;
  }

  if ((ret = mbedtls2_cipher_finish(&cipher_ctx, output + olen,
                                           &olen)) != 0)
    ret = MBEDTLS2_ERR_PKCS12_PASSWORD_MISMATCH;

exit:
  mbedtls2_platform_zeroize(key, sizeof(key));
  mbedtls2_platform_zeroize(iv, sizeof(iv));
  mbedtls2_cipher_free(&cipher_ctx);

  return (ret);
}

#endif /* MBEDTLS2_ASN1_PARSE_C */

static void pkcs12_fill_buffer(unsigned char *data, size_t data_len,
                               const unsigned char *filler, size_t fill_len) {
  unsigned char *p = data;
  size_t use_len;

  if (filler != NULL && fill_len != 0) {
    while (data_len > 0) {
      use_len = (data_len > fill_len) ? fill_len : data_len;
      memcpy(p, filler, use_len);
      p += use_len;
      data_len -= use_len;
    }
  } else {
    /* If either of the above are not true then clearly there is nothing
     * that this function can do. The function should *not* be called
     * under either of those circumstances, as you could end up with an
     * incorrect output but for safety's sake, leaving the check in as
     * otherwise we could end up with memory corruption.*/
  }
}

int mbedtls2_pkcs12_derivation(unsigned char *data, size_t datalen,
                                      const unsigned char *pwd, size_t pwdlen,
                                      const unsigned char *salt, size_t saltlen,
                                      mbedtls2_md_type_t md_type, int id,
                                      int iterations) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  unsigned int j;

  unsigned char diversifier[128];
  unsigned char salt_block[128], pwd_block[128], hash_block[128];
  unsigned char hash_output[MBEDTLS2_MD_MAX_SIZE];
  unsigned char *p;
  unsigned char c;
  int use_password = 0;
  int use_salt = 0;

  size_t hlen, use_len, v, i;

  const mbedtls2_md_info_t *md_info;
  mbedtls2_md_context_t md_ctx;

  // This version only allows max of 64 bytes of password or salt
  if (datalen > 128 || pwdlen > 64 || saltlen > 64)
    return (MBEDTLS2_ERR_PKCS12_BAD_INPUT_DATA);

  if (pwd == NULL && pwdlen != 0)
    return (MBEDTLS2_ERR_PKCS12_BAD_INPUT_DATA);

  if (salt == NULL && saltlen != 0)
    return (MBEDTLS2_ERR_PKCS12_BAD_INPUT_DATA);

  use_password = (pwd && pwdlen != 0);
  use_salt = (salt && saltlen != 0);

  md_info = mbedtls2_md_info_from_type(md_type);
  if (md_info == NULL)
    return (MBEDTLS2_ERR_PKCS12_FEATURE_UNAVAILABLE);

  mbedtls2_md_init(&md_ctx);

  if ((ret = mbedtls2_md_setup(&md_ctx, md_info, 0)) != 0)
    return (ret);
  hlen = mbedtls2_md_get_size(md_info);

  if (hlen <= 32)
    v = 64;
  else
    v = 128;

  memset(diversifier, (unsigned char)id, v);

  if (use_salt != 0) {
    pkcs12_fill_buffer(salt_block, v, salt, saltlen);
  }

  if (use_password != 0) {
    pkcs12_fill_buffer(pwd_block, v, pwd, pwdlen);
  }

  p = data;
  while (datalen > 0) {
    // Calculate hash( diversifier || salt_block || pwd_block )
    if ((ret = mbedtls2_md_starts(&md_ctx)) != 0)
      goto exit;

    if ((ret = mbedtls2_md_update(&md_ctx, diversifier, v)) != 0)
      goto exit;

    if (use_salt != 0) {
      if ((ret = mbedtls2_md_update(&md_ctx, salt_block, v)) != 0)
        goto exit;
    }

    if (use_password != 0) {
      if ((ret = mbedtls2_md_update(&md_ctx, pwd_block, v)) != 0)
        goto exit;
    }

    if ((ret = mbedtls2_md_finish(&md_ctx, hash_output)) != 0)
      goto exit;

    // Perform remaining ( iterations - 1 ) recursive hash calculations
    for (i = 1; i < (size_t)iterations; i++) {
      if ((ret = mbedtls2_md(md_info, hash_output, hlen, hash_output)) !=
          0)
        goto exit;
    }

    use_len = (datalen > hlen) ? hlen : datalen;
    memcpy(p, hash_output, use_len);
    datalen -= use_len;
    p += use_len;

    if (datalen == 0)
      break;

    // Concatenating copies of hash_output into hash_block (B)
    pkcs12_fill_buffer(hash_block, v, hash_output, hlen);

    // B += 1
    for (i = v; i > 0; i--)
      if (++hash_block[i - 1] != 0)
        break;

    if (use_salt != 0) {
      // salt_block += B
      c = 0;
      for (i = v; i > 0; i--) {
        j = salt_block[i - 1] + hash_block[i - 1] + c;
        c = MBEDTLS2_BYTE_1(j);
        salt_block[i - 1] = MBEDTLS2_BYTE_0(j);
      }
    }

    if (use_password != 0) {
      // pwd_block  += B
      c = 0;
      for (i = v; i > 0; i--) {
        j = pwd_block[i - 1] + hash_block[i - 1] + c;
        c = MBEDTLS2_BYTE_1(j);
        pwd_block[i - 1] = MBEDTLS2_BYTE_0(j);
      }
    }
  }

  ret = 0;

exit:
  mbedtls2_platform_zeroize(salt_block, sizeof(salt_block));
  mbedtls2_platform_zeroize(pwd_block, sizeof(pwd_block));
  mbedtls2_platform_zeroize(hash_block, sizeof(hash_block));
  mbedtls2_platform_zeroize(hash_output, sizeof(hash_output));

  mbedtls2_md_free(&md_ctx);

  return (ret);
}

#endif /* MBEDTLS2_PKCS12_C */

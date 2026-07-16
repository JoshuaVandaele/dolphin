/*
 *  X.509 Certificate Signing Request writing
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
 * References:
 * - CSRs: PKCS#10 v1.7 aka RFC 2986
 * - attributes: PKCS#9 v2.0 aka RFC 2985
 */

#include "common.h"

#if defined(MBEDTLS2_X509_CSR_WRITE_C)

#include "mbedtls2/asn1write.h"
#include "mbedtls2/error.h"
#include "mbedtls2/oid.h"
#include "mbedtls2/platform_util.h"
#include "mbedtls2/x509_csr.h"

#if defined(MBEDTLS2_USE_PSA_CRYPTO)
#include "mbedtls2/psa_util.h"
#include "psa/crypto.h"
#endif

#include <stdlib.h>
#include <string.h>

#if defined(MBEDTLS2_PEM_WRITE_C)
#include "mbedtls2/pem.h"
#endif

#if defined(MBEDTLS2_PLATFORM_C)
#include "mbedtls2/platform.h"
#else
#include <stdlib.h>
#define mbedtls2_calloc calloc
#define mbedtls2_free free
#endif

void mbedtls2_x509write_csr_init(mbedtls2_x509write_csr *ctx) {
  memset(ctx, 0, sizeof(mbedtls2_x509write_csr));
}

void mbedtls2_x509write_csr_free(mbedtls2_x509write_csr *ctx) {
  mbedtls2_asn1_free_named_data_list(&ctx->subject);
  mbedtls2_asn1_free_named_data_list(&ctx->extensions);

  mbedtls2_platform_zeroize(ctx, sizeof(mbedtls2_x509write_csr));
}

void mbedtls2_x509write_csr_set_md_alg(
    mbedtls2_x509write_csr *ctx, mbedtls2_md_type_t md_alg) {
  ctx->md_alg = md_alg;
}

void mbedtls2_x509write_csr_set_key(mbedtls2_x509write_csr *ctx,
                                           mbedtls2_pk_context *key) {
  ctx->key = key;
}

int mbedtls2_x509write_csr_set_subject_name(
    mbedtls2_x509write_csr *ctx, const char *subject_name) {
  return mbedtls2_x509_string_to_names(&ctx->subject, subject_name);
}

int mbedtls2_x509write_csr_set_extension(
    mbedtls2_x509write_csr *ctx, const char *oid, size_t oid_len,
    const unsigned char *val, size_t val_len) {
  return mbedtls2_x509_set_extension(&ctx->extensions, oid, oid_len, 0,
                                            val, val_len);
}

int mbedtls2_x509write_csr_set_key_usage(
    mbedtls2_x509write_csr *ctx, unsigned char key_usage) {
  unsigned char buf[4] = {0};
  unsigned char *c;
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;

  c = buf + 4;

  ret = mbedtls2_asn1_write_named_bitstring(&c, buf, &key_usage, 8);
  if (ret < 3 || ret > 4)
    return (ret);

  ret = mbedtls2_x509write_csr_set_extension(
      ctx, MBEDTLS2_OID_KEY_USAGE,
      MBEDTLS2_OID_SIZE(MBEDTLS2_OID_KEY_USAGE), c, (size_t)ret);
  if (ret != 0)
    return (ret);

  return (0);
}

int mbedtls2_x509write_csr_set_ns_cert_type(
    mbedtls2_x509write_csr *ctx, unsigned char ns_cert_type) {
  unsigned char buf[4] = {0};
  unsigned char *c;
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;

  c = buf + 4;

  ret = mbedtls2_asn1_write_named_bitstring(&c, buf, &ns_cert_type, 8);
  if (ret < 3 || ret > 4)
    return (ret);

  ret = mbedtls2_x509write_csr_set_extension(
      ctx, MBEDTLS2_OID_NS_CERT_TYPE,
      MBEDTLS2_OID_SIZE(MBEDTLS2_OID_NS_CERT_TYPE), c,
      (size_t)ret);
  if (ret != 0)
    return (ret);

  return (0);
}

static int
x509write_csr_der_internal(mbedtls2_x509write_csr *ctx,
                           unsigned char *buf, size_t size, unsigned char *sig,
                           int (*f_rng)(void *, unsigned char *, size_t),
                           void *p_rng) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  const char *sig_oid;
  size_t sig_oid_len = 0;
  unsigned char *c, *c2;
  unsigned char hash[64];
  size_t pub_len = 0, sig_and_oid_len = 0, sig_len;
  size_t len = 0;
  mbedtls2_pk_type_t pk_alg;
#if defined(MBEDTLS2_USE_PSA_CRYPTO)
  psa_hash_operation_t hash_operation = PSA_HASH_OPERATION_INIT;
  size_t hash_len;
  psa_algorithm_t hash_alg = mbedtls2_psa_translate_md(ctx->md_alg);
#endif /* MBEDTLS2_USE_PSA_CRYPTO */

  /* Write the CSR backwards starting from the end of buf */
  c = buf + size;

  MBEDTLS2_ASN1_CHK_ADD(
      len, mbedtls2_x509_write_extensions(&c, buf, ctx->extensions));

  if (len) {
    MBEDTLS2_ASN1_CHK_ADD(len,
                                 mbedtls2_asn1_write_len(&c, buf, len));
    MBEDTLS2_ASN1_CHK_ADD(
        len, mbedtls2_asn1_write_tag(&c, buf,
                                            MBEDTLS2_ASN1_CONSTRUCTED |
                                                MBEDTLS2_ASN1_SEQUENCE));

    MBEDTLS2_ASN1_CHK_ADD(len,
                                 mbedtls2_asn1_write_len(&c, buf, len));
    MBEDTLS2_ASN1_CHK_ADD(
        len, mbedtls2_asn1_write_tag(&c, buf,
                                            MBEDTLS2_ASN1_CONSTRUCTED |
                                                MBEDTLS2_ASN1_SET));

    MBEDTLS2_ASN1_CHK_ADD(
        len,
        mbedtls2_asn1_write_oid(
            &c, buf, MBEDTLS2_OID_PKCS9_CSR_EXT_REQ,
            MBEDTLS2_OID_SIZE(MBEDTLS2_OID_PKCS9_CSR_EXT_REQ)));

    MBEDTLS2_ASN1_CHK_ADD(len,
                                 mbedtls2_asn1_write_len(&c, buf, len));
    MBEDTLS2_ASN1_CHK_ADD(
        len, mbedtls2_asn1_write_tag(&c, buf,
                                            MBEDTLS2_ASN1_CONSTRUCTED |
                                                MBEDTLS2_ASN1_SEQUENCE));
  }

  MBEDTLS2_ASN1_CHK_ADD(len,
                               mbedtls2_asn1_write_len(&c, buf, len));
  MBEDTLS2_ASN1_CHK_ADD(len,
                               mbedtls2_asn1_write_tag(
                                   &c, buf,
                                   MBEDTLS2_ASN1_CONSTRUCTED |
                                       MBEDTLS2_ASN1_CONTEXT_SPECIFIC));

  MBEDTLS2_ASN1_CHK_ADD(
      pub_len, mbedtls2_pk_write_pubkey_der(ctx->key, buf, c - buf));
  c -= pub_len;
  len += pub_len;

  /*
   *  Subject  ::=  Name
   */
  MBEDTLS2_ASN1_CHK_ADD(
      len, mbedtls2_x509_write_names(&c, buf, ctx->subject));

  /*
   *  Version  ::=  INTEGER  {  v1(0), v2(1), v3(2)  }
   */
  MBEDTLS2_ASN1_CHK_ADD(len, mbedtls2_asn1_write_int(&c, buf, 0));

  MBEDTLS2_ASN1_CHK_ADD(len,
                               mbedtls2_asn1_write_len(&c, buf, len));
  MBEDTLS2_ASN1_CHK_ADD(
      len, mbedtls2_asn1_write_tag(&c, buf,
                                          MBEDTLS2_ASN1_CONSTRUCTED |
                                              MBEDTLS2_ASN1_SEQUENCE));

  /*
   * Sign the written CSR data into the sig buffer
   * Note: hash errors can happen only after an internal error
   */
#if defined(MBEDTLS2_USE_PSA_CRYPTO)
  if (psa_hash_setup(&hash_operation, hash_alg) != PSA_SUCCESS)
    return (MBEDTLS2_ERR_X509_FATAL_ERROR);

  if (psa_hash_update(&hash_operation, c, len) != PSA_SUCCESS)
    return (MBEDTLS2_ERR_X509_FATAL_ERROR);

  if (psa_hash_finish(&hash_operation, hash, sizeof(hash), &hash_len) !=
      PSA_SUCCESS) {
    return (MBEDTLS2_ERR_X509_FATAL_ERROR);
  }
#else /* MBEDTLS2_USE_PSA_CRYPTO */
  ret = mbedtls2_md(mbedtls2_md_info_from_type(ctx->md_alg), c,
                           len, hash);
  if (ret != 0)
    return (ret);
#endif
  if ((ret = mbedtls2_pk_sign(ctx->key, ctx->md_alg, hash, 0, sig,
                                     &sig_len, f_rng, p_rng)) != 0) {
    return (ret);
  }

  if (mbedtls2_pk_can_do(ctx->key, MBEDTLS2_PK_RSA))
    pk_alg = MBEDTLS2_PK_RSA;
  else if (mbedtls2_pk_can_do(ctx->key, MBEDTLS2_PK_ECDSA))
    pk_alg = MBEDTLS2_PK_ECDSA;
  else
    return (MBEDTLS2_ERR_X509_INVALID_ALG);

  if ((ret = mbedtls2_oid_get_oid_by_sig_alg(
           pk_alg, ctx->md_alg, &sig_oid, &sig_oid_len)) != 0) {
    return (ret);
  }

  /*
   * Move the written CSR data to the start of buf to create space for
   * writing the signature into buf.
   */
  memmove(buf, c, len);

  /*
   * Write sig and its OID into buf backwards from the end of buf.
   * Note: mbedtls2_x509_write_sig will check for c2 - ( buf + len ) <
   * sig_len and return MBEDTLS2_ERR_ASN1_BUF_TOO_SMALL if needed.
   */
  c2 = buf + size;
  MBEDTLS2_ASN1_CHK_ADD(
      sig_and_oid_len, mbedtls2_x509_write_sig(
                           &c2, buf + len, sig_oid, sig_oid_len, sig, sig_len));

  /*
   * Compact the space between the CSR data and signature by moving the
   * CSR data to the start of the signature.
   */
  c2 -= len;
  memmove(c2, buf, len);

  /* ASN encode the total size and tag the CSR data with it. */
  len += sig_and_oid_len;
  MBEDTLS2_ASN1_CHK_ADD(len,
                               mbedtls2_asn1_write_len(&c2, buf, len));
  MBEDTLS2_ASN1_CHK_ADD(
      len, mbedtls2_asn1_write_tag(&c2, buf,
                                          MBEDTLS2_ASN1_CONSTRUCTED |
                                              MBEDTLS2_ASN1_SEQUENCE));

  /* Zero the unused bytes at the start of buf */
  memset(buf, 0, c2 - buf);

  return ((int)len);
}

int mbedtls2_x509write_csr_der(
    mbedtls2_x509write_csr *ctx, unsigned char *buf, size_t size,
    int (*f_rng)(void *, unsigned char *, size_t), void *p_rng) {
  int ret;
  unsigned char *sig;

  if ((sig = mbedtls2_calloc(
           1, MBEDTLS2_PK_SIGNATURE_MAX_SIZE)) == NULL) {
    return (MBEDTLS2_ERR_X509_ALLOC_FAILED);
  }

  ret = x509write_csr_der_internal(ctx, buf, size, sig, f_rng, p_rng);

  mbedtls2_free(sig);

  return (ret);
}

#define PEM_BEGIN_CSR "-----BEGIN CERTIFICATE REQUEST-----\n"
#define PEM_END_CSR "-----END CERTIFICATE REQUEST-----\n"

#if defined(MBEDTLS2_PEM_WRITE_C)
int mbedtls2_x509write_csr_pem(
    mbedtls2_x509write_csr *ctx, unsigned char *buf, size_t size,
    int (*f_rng)(void *, unsigned char *, size_t), void *p_rng) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  size_t olen = 0;

  if ((ret = mbedtls2_x509write_csr_der(ctx, buf, size, f_rng, p_rng)) <
      0) {
    return (ret);
  }

  if ((ret = mbedtls2_pem_write_buffer(PEM_BEGIN_CSR, PEM_END_CSR,
                                              buf + size - ret, ret, buf, size,
                                              &olen)) != 0) {
    return (ret);
  }

  return (0);
}
#endif /* MBEDTLS2_PEM_WRITE_C */

#endif /* MBEDTLS2_X509_CSR_WRITE_C */

/*
 *  Public Key layer for writing key files and structures
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

#if defined(MBEDTLS2_PK_WRITE_C)

#include "mbedtls2/asn1write.h"
#include "mbedtls2/error.h"
#include "mbedtls2/oid.h"
#include "mbedtls2/pk.h"
#include "mbedtls2/platform_util.h"

#include <string.h>

#if defined(MBEDTLS2_RSA_C)
#include "mbedtls2/rsa.h"
#endif
#if defined(MBEDTLS2_ECP_C)
#include "mbedtls2/bignum.h"
#include "mbedtls2/ecp.h"
#include "mbedtls2/platform_util.h"
#endif
#if defined(MBEDTLS2_ECDSA_C)
#include "mbedtls2/ecdsa.h"
#endif
#if defined(MBEDTLS2_PEM_WRITE_C)
#include "mbedtls2/pem.h"
#endif

#if defined(MBEDTLS2_USE_PSA_CRYPTO)
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

/* Parameter validation macros based on platform_util.h */
#define PK_VALIDATE_RET(cond)                                                  \
  MBEDTLS2_INTERNAL_VALIDATE_RET(cond,                                  \
                                        MBEDTLS2_ERR_PK_BAD_INPUT_DATA)
#define PK_VALIDATE(cond) MBEDTLS2_INTERNAL_VALIDATE(cond)

#if defined(MBEDTLS2_RSA_C)
/*
 *  RSAPublicKey ::= SEQUENCE {
 *      modulus           INTEGER,  -- n
 *      publicExponent    INTEGER   -- e
 *  }
 */
static int pk_write_rsa_pubkey(unsigned char **p, unsigned char *start,
                               mbedtls2_rsa_context *rsa) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  size_t len = 0;
  mbedtls2_mpi T;

  mbedtls2_mpi_init(&T);

  /* Export E */
  if ((ret = mbedtls2_rsa_export(rsa, NULL, NULL, NULL, NULL, &T)) !=
          0 ||
      (ret = mbedtls2_asn1_write_mpi(p, start, &T)) < 0)
    goto end_of_export;
  len += ret;

  /* Export N */
  if ((ret = mbedtls2_rsa_export(rsa, &T, NULL, NULL, NULL, NULL)) !=
          0 ||
      (ret = mbedtls2_asn1_write_mpi(p, start, &T)) < 0)
    goto end_of_export;
  len += ret;

end_of_export:

  mbedtls2_mpi_free(&T);
  if (ret < 0)
    return (ret);

  MBEDTLS2_ASN1_CHK_ADD(len,
                               mbedtls2_asn1_write_len(p, start, len));
  MBEDTLS2_ASN1_CHK_ADD(
      len, mbedtls2_asn1_write_tag(p, start,
                                          MBEDTLS2_ASN1_CONSTRUCTED |
                                              MBEDTLS2_ASN1_SEQUENCE));

  return ((int)len);
}
#endif /* MBEDTLS2_RSA_C */

#if defined(MBEDTLS2_ECP_C)
/*
 * EC public key is an EC point
 */
static int pk_write_ec_pubkey(unsigned char **p, unsigned char *start,
                              mbedtls2_ecp_keypair *ec) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  size_t len = 0;
  unsigned char buf[MBEDTLS2_ECP_MAX_PT_LEN];

  if ((ret = mbedtls2_ecp_point_write_binary(
           &ec->grp, &ec->Q, MBEDTLS2_ECP_PF_UNCOMPRESSED, &len, buf,
           sizeof(buf))) != 0) {
    return (ret);
  }

  if (*p < start || (size_t)(*p - start) < len)
    return (MBEDTLS2_ERR_ASN1_BUF_TOO_SMALL);

  *p -= len;
  memcpy(*p, buf, len);

  return ((int)len);
}

/*
 * ECParameters ::= CHOICE {
 *   namedCurve         OBJECT IDENTIFIER
 * }
 */
static int pk_write_ec_param(unsigned char **p, unsigned char *start,
                             mbedtls2_ecp_keypair *ec) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  size_t len = 0;
  const char *oid;
  size_t oid_len;

  if ((ret = mbedtls2_oid_get_oid_by_ec_grp(ec->grp.id, &oid,
                                                   &oid_len)) != 0)
    return (ret);

  MBEDTLS2_ASN1_CHK_ADD(
      len, mbedtls2_asn1_write_oid(p, start, oid, oid_len));

  return ((int)len);
}

/*
 * privateKey  OCTET STRING -- always of length ceil(log2(n)/8)
 */
static int pk_write_ec_private(unsigned char **p, unsigned char *start,
                               mbedtls2_ecp_keypair *ec) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  size_t byte_length = (ec->grp.pbits + 7) / 8;
  unsigned char tmp[MBEDTLS2_ECP_MAX_BYTES];

  ret = mbedtls2_ecp_write_key(ec, tmp, byte_length);
  if (ret != 0)
    goto exit;
  ret = mbedtls2_asn1_write_octet_string(p, start, tmp, byte_length);

exit:
  mbedtls2_platform_zeroize(tmp, byte_length);
  return (ret);
}
#endif /* MBEDTLS2_ECP_C */

int mbedtls2_pk_write_pubkey(unsigned char **p, unsigned char *start,
                                    const mbedtls2_pk_context *key) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  size_t len = 0;

  PK_VALIDATE_RET(p != NULL);
  PK_VALIDATE_RET(*p != NULL);
  PK_VALIDATE_RET(start != NULL);
  PK_VALIDATE_RET(key != NULL);

#if defined(MBEDTLS2_RSA_C)
  if (mbedtls2_pk_get_type(key) == MBEDTLS2_PK_RSA)
    MBEDTLS2_ASN1_CHK_ADD(
        len, pk_write_rsa_pubkey(p, start, mbedtls2_pk_rsa(*key)));
  else
#endif
#if defined(MBEDTLS2_ECP_C)
      if (mbedtls2_pk_get_type(key) == MBEDTLS2_PK_ECKEY)
    MBEDTLS2_ASN1_CHK_ADD(
        len, pk_write_ec_pubkey(p, start, mbedtls2_pk_ec(*key)));
  else
#endif
#if defined(MBEDTLS2_USE_PSA_CRYPTO)
      if (mbedtls2_pk_get_type(key) == MBEDTLS2_PK_OPAQUE) {
    size_t buffer_size;
    psa_key_id_t *key_id = (psa_key_id_t *)key->pk_ctx;

    if (*p < start)
      return (MBEDTLS2_ERR_PK_BAD_INPUT_DATA);

    buffer_size = (size_t)(*p - start);
    if (psa_export_public_key(*key_id, start, buffer_size, &len) !=
        PSA_SUCCESS) {
      return (MBEDTLS2_ERR_PK_BAD_INPUT_DATA);
    } else {
      *p -= len;
      memmove(*p, start, len);
    }
  } else
#endif /* MBEDTLS2_USE_PSA_CRYPTO */
    return (MBEDTLS2_ERR_PK_FEATURE_UNAVAILABLE);

  return ((int)len);
}

int mbedtls2_pk_write_pubkey_der(mbedtls2_pk_context *key,
                                        unsigned char *buf, size_t size) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  unsigned char *c;
  size_t len = 0, par_len = 0, oid_len;
  mbedtls2_pk_type_t pk_type;
  const char *oid;

  PK_VALIDATE_RET(key != NULL);
  if (size == 0)
    return (MBEDTLS2_ERR_ASN1_BUF_TOO_SMALL);
  PK_VALIDATE_RET(buf != NULL);

  c = buf + size;

  MBEDTLS2_ASN1_CHK_ADD(len,
                               mbedtls2_pk_write_pubkey(&c, buf, key));

  if (c - buf < 1)
    return (MBEDTLS2_ERR_ASN1_BUF_TOO_SMALL);

  /*
   *  SubjectPublicKeyInfo  ::=  SEQUENCE  {
   *       algorithm            AlgorithmIdentifier,
   *       subjectPublicKey     BIT STRING }
   */
  *--c = 0;
  len += 1;

  MBEDTLS2_ASN1_CHK_ADD(len,
                               mbedtls2_asn1_write_len(&c, buf, len));
  MBEDTLS2_ASN1_CHK_ADD(
      len,
      mbedtls2_asn1_write_tag(&c, buf, MBEDTLS2_ASN1_BIT_STRING));

  pk_type = mbedtls2_pk_get_type(key);
#if defined(MBEDTLS2_ECP_C)
  if (pk_type == MBEDTLS2_PK_ECKEY) {
    MBEDTLS2_ASN1_CHK_ADD(
        par_len, pk_write_ec_param(&c, buf, mbedtls2_pk_ec(*key)));
  }
#endif
#if defined(MBEDTLS2_USE_PSA_CRYPTO)
  if (pk_type == MBEDTLS2_PK_OPAQUE) {
    psa_key_attributes_t attributes = PSA_KEY_ATTRIBUTES_INIT;
    psa_key_type_t key_type;
    psa_key_id_t key_id;
    psa_ecc_family_t curve;
    size_t bits;

    key_id = *((psa_key_id_t *)key->pk_ctx);
    if (PSA_SUCCESS != psa_get_key_attributes(key_id, &attributes))
      return (MBEDTLS2_ERR_PK_HW_ACCEL_FAILED);
    key_type = psa_get_key_type(&attributes);
    bits = psa_get_key_bits(&attributes);
    psa_reset_key_attributes(&attributes);

    curve = PSA_KEY_TYPE_ECC_GET_FAMILY(key_type);
    if (curve == 0)
      return (MBEDTLS2_ERR_PK_FEATURE_UNAVAILABLE);

    ret = mbedtls2_psa_get_ecc_oid_from_id(curve, bits, &oid, &oid_len);
    if (ret != 0)
      return (MBEDTLS2_ERR_PK_FEATURE_UNAVAILABLE);

    /* Write EC algorithm parameters; that's akin
     * to pk_write_ec_param() above. */
    MBEDTLS2_ASN1_CHK_ADD(
        par_len, mbedtls2_asn1_write_oid(&c, buf, oid, oid_len));

    /* The rest of the function works as for legacy EC contexts. */
    pk_type = MBEDTLS2_PK_ECKEY;
  }
#endif /* MBEDTLS2_USE_PSA_CRYPTO */

  if ((ret = mbedtls2_oid_get_oid_by_pk_alg(pk_type, &oid, &oid_len)) !=
      0) {
    return (ret);
  }

  MBEDTLS2_ASN1_CHK_ADD(
      len, mbedtls2_asn1_write_algorithm_identifier(&c, buf, oid,
                                                           oid_len, par_len));

  MBEDTLS2_ASN1_CHK_ADD(len,
                               mbedtls2_asn1_write_len(&c, buf, len));
  MBEDTLS2_ASN1_CHK_ADD(
      len, mbedtls2_asn1_write_tag(&c, buf,
                                          MBEDTLS2_ASN1_CONSTRUCTED |
                                              MBEDTLS2_ASN1_SEQUENCE));

  return ((int)len);
}

int mbedtls2_pk_write_key_der(mbedtls2_pk_context *key,
                                     unsigned char *buf, size_t size) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  unsigned char *c;
  size_t len = 0;

  PK_VALIDATE_RET(key != NULL);
  if (size == 0)
    return (MBEDTLS2_ERR_ASN1_BUF_TOO_SMALL);
  PK_VALIDATE_RET(buf != NULL);

  c = buf + size;

#if defined(MBEDTLS2_RSA_C)
  if (mbedtls2_pk_get_type(key) == MBEDTLS2_PK_RSA) {
    mbedtls2_mpi T; /* Temporary holding the exported parameters */
    mbedtls2_rsa_context *rsa = mbedtls2_pk_rsa(*key);

    /*
     * Export the parameters one after another to avoid simultaneous copies.
     */

    mbedtls2_mpi_init(&T);

    /* Export QP */
    if ((ret = mbedtls2_rsa_export_crt(rsa, NULL, NULL, &T)) != 0 ||
        (ret = mbedtls2_asn1_write_mpi(&c, buf, &T)) < 0)
      goto end_of_export;
    len += ret;

    /* Export DQ */
    if ((ret = mbedtls2_rsa_export_crt(rsa, NULL, &T, NULL)) != 0 ||
        (ret = mbedtls2_asn1_write_mpi(&c, buf, &T)) < 0)
      goto end_of_export;
    len += ret;

    /* Export DP */
    if ((ret = mbedtls2_rsa_export_crt(rsa, &T, NULL, NULL)) != 0 ||
        (ret = mbedtls2_asn1_write_mpi(&c, buf, &T)) < 0)
      goto end_of_export;
    len += ret;

    /* Export Q */
    if ((ret = mbedtls2_rsa_export(rsa, NULL, NULL, &T, NULL, NULL)) !=
            0 ||
        (ret = mbedtls2_asn1_write_mpi(&c, buf, &T)) < 0)
      goto end_of_export;
    len += ret;

    /* Export P */
    if ((ret = mbedtls2_rsa_export(rsa, NULL, &T, NULL, NULL, NULL)) !=
            0 ||
        (ret = mbedtls2_asn1_write_mpi(&c, buf, &T)) < 0)
      goto end_of_export;
    len += ret;

    /* Export D */
    if ((ret = mbedtls2_rsa_export(rsa, NULL, NULL, NULL, &T, NULL)) !=
            0 ||
        (ret = mbedtls2_asn1_write_mpi(&c, buf, &T)) < 0)
      goto end_of_export;
    len += ret;

    /* Export E */
    if ((ret = mbedtls2_rsa_export(rsa, NULL, NULL, NULL, NULL, &T)) !=
            0 ||
        (ret = mbedtls2_asn1_write_mpi(&c, buf, &T)) < 0)
      goto end_of_export;
    len += ret;

    /* Export N */
    if ((ret = mbedtls2_rsa_export(rsa, &T, NULL, NULL, NULL, NULL)) !=
            0 ||
        (ret = mbedtls2_asn1_write_mpi(&c, buf, &T)) < 0)
      goto end_of_export;
    len += ret;

  end_of_export:

    mbedtls2_mpi_free(&T);
    if (ret < 0)
      return (ret);

    MBEDTLS2_ASN1_CHK_ADD(len,
                                 mbedtls2_asn1_write_int(&c, buf, 0));
    MBEDTLS2_ASN1_CHK_ADD(len,
                                 mbedtls2_asn1_write_len(&c, buf, len));
    MBEDTLS2_ASN1_CHK_ADD(
        len, mbedtls2_asn1_write_tag(&c, buf,
                                            MBEDTLS2_ASN1_CONSTRUCTED |
                                                MBEDTLS2_ASN1_SEQUENCE));
  } else
#endif /* MBEDTLS2_RSA_C */
#if defined(MBEDTLS2_ECP_C)
      if (mbedtls2_pk_get_type(key) == MBEDTLS2_PK_ECKEY) {
    mbedtls2_ecp_keypair *ec = mbedtls2_pk_ec(*key);
    size_t pub_len = 0, par_len = 0;

    /*
     * RFC 5915, or SEC1 Appendix C.4
     *
     * ECPrivateKey ::= SEQUENCE {
     *      version        INTEGER { ecPrivkeyVer1(1) } (ecPrivkeyVer1),
     *      privateKey     OCTET STRING,
     *      parameters [0] ECParameters {{ NamedCurve }} OPTIONAL,
     *      publicKey  [1] BIT STRING OPTIONAL
     *    }
     */

    /* publicKey */
    MBEDTLS2_ASN1_CHK_ADD(pub_len, pk_write_ec_pubkey(&c, buf, ec));

    if (c - buf < 1)
      return (MBEDTLS2_ERR_ASN1_BUF_TOO_SMALL);
    *--c = 0;
    pub_len += 1;

    MBEDTLS2_ASN1_CHK_ADD(
        pub_len, mbedtls2_asn1_write_len(&c, buf, pub_len));
    MBEDTLS2_ASN1_CHK_ADD(pub_len,
                                 mbedtls2_asn1_write_tag(
                                     &c, buf, MBEDTLS2_ASN1_BIT_STRING));

    MBEDTLS2_ASN1_CHK_ADD(
        pub_len, mbedtls2_asn1_write_len(&c, buf, pub_len));
    MBEDTLS2_ASN1_CHK_ADD(pub_len,
                                 mbedtls2_asn1_write_tag(
                                     &c, buf,
                                     MBEDTLS2_ASN1_CONTEXT_SPECIFIC |
                                         MBEDTLS2_ASN1_CONSTRUCTED | 1));
    len += pub_len;

    /* parameters */
    MBEDTLS2_ASN1_CHK_ADD(par_len, pk_write_ec_param(&c, buf, ec));

    MBEDTLS2_ASN1_CHK_ADD(
        par_len, mbedtls2_asn1_write_len(&c, buf, par_len));
    MBEDTLS2_ASN1_CHK_ADD(par_len,
                                 mbedtls2_asn1_write_tag(
                                     &c, buf,
                                     MBEDTLS2_ASN1_CONTEXT_SPECIFIC |
                                         MBEDTLS2_ASN1_CONSTRUCTED | 0));
    len += par_len;

    /* privateKey */
    MBEDTLS2_ASN1_CHK_ADD(len, pk_write_ec_private(&c, buf, ec));

    /* version */
    MBEDTLS2_ASN1_CHK_ADD(len,
                                 mbedtls2_asn1_write_int(&c, buf, 1));

    MBEDTLS2_ASN1_CHK_ADD(len,
                                 mbedtls2_asn1_write_len(&c, buf, len));
    MBEDTLS2_ASN1_CHK_ADD(
        len, mbedtls2_asn1_write_tag(&c, buf,
                                            MBEDTLS2_ASN1_CONSTRUCTED |
                                                MBEDTLS2_ASN1_SEQUENCE));
  } else
#endif /* MBEDTLS2_ECP_C */
    return (MBEDTLS2_ERR_PK_FEATURE_UNAVAILABLE);

  return ((int)len);
}

#if defined(MBEDTLS2_PEM_WRITE_C)

#define PEM_BEGIN_PUBLIC_KEY "-----BEGIN PUBLIC KEY-----\n"
#define PEM_END_PUBLIC_KEY "-----END PUBLIC KEY-----\n"

#define PEM_BEGIN_PRIVATE_KEY_RSA "-----BEGIN RSA PRIVATE KEY-----\n"
#define PEM_END_PRIVATE_KEY_RSA "-----END RSA PRIVATE KEY-----\n"
#define PEM_BEGIN_PRIVATE_KEY_EC "-----BEGIN EC PRIVATE KEY-----\n"
#define PEM_END_PRIVATE_KEY_EC "-----END EC PRIVATE KEY-----\n"

/*
 * Max sizes of key per types. Shown as tag + len (+ content).
 */

#if defined(MBEDTLS2_RSA_C)
/*
 * RSA public keys:
 *  SubjectPublicKeyInfo  ::=  SEQUENCE  {          1 + 3
 *       algorithm            AlgorithmIdentifier,  1 + 1 (sequence)
 *                                                + 1 + 1 + 9 (rsa oid)
 *                                                + 1 + 1 (params null)
 *       subjectPublicKey     BIT STRING }          1 + 3 + (1 + below)
 *  RSAPublicKey ::= SEQUENCE {                     1 + 3
 *      modulus           INTEGER,  -- n            1 + 3 + MPI_MAX + 1
 *      publicExponent    INTEGER   -- e            1 + 3 + MPI_MAX + 1
 *  }
 */
#define RSA_PUB_DER_MAX_BYTES (38 + 2 * MBEDTLS2_MPI_MAX_SIZE)

/*
 * RSA private keys:
 *  RSAPrivateKey ::= SEQUENCE {                    1 + 3
 *      version           Version,                  1 + 1 + 1
 *      modulus           INTEGER,                  1 + 3 + MPI_MAX + 1
 *      publicExponent    INTEGER,                  1 + 3 + MPI_MAX + 1
 *      privateExponent   INTEGER,                  1 + 3 + MPI_MAX + 1
 *      prime1            INTEGER,                  1 + 3 + MPI_MAX / 2 + 1
 *      prime2            INTEGER,                  1 + 3 + MPI_MAX / 2 + 1
 *      exponent1         INTEGER,                  1 + 3 + MPI_MAX / 2 + 1
 *      exponent2         INTEGER,                  1 + 3 + MPI_MAX / 2 + 1
 *      coefficient       INTEGER,                  1 + 3 + MPI_MAX / 2 + 1
 *      otherPrimeInfos   OtherPrimeInfos OPTIONAL  0 (not supported)
 *  }
 */
#define MPI_MAX_SIZE_2                                                         \
  (MBEDTLS2_MPI_MAX_SIZE / 2 + MBEDTLS2_MPI_MAX_SIZE % 2)
#define RSA_PRV_DER_MAX_BYTES                                                  \
  (47 + 3 * MBEDTLS2_MPI_MAX_SIZE + 5 * MPI_MAX_SIZE_2)

#else /* MBEDTLS2_RSA_C */

#define RSA_PUB_DER_MAX_BYTES 0
#define RSA_PRV_DER_MAX_BYTES 0

#endif /* MBEDTLS2_RSA_C */

#if defined(MBEDTLS2_ECP_C)
/*
 * EC public keys:
 *  SubjectPublicKeyInfo  ::=  SEQUENCE  {      1 + 2
 *    algorithm         AlgorithmIdentifier,    1 + 1 (sequence)
 *                                            + 1 + 1 + 7 (ec oid)
 *                                            + 1 + 1 + 9 (namedCurve oid)
 *    subjectPublicKey  BIT STRING              1 + 2 + 1               [1]
 *                                            + 1 (point format)        [1]
 *                                            + 2 * ECP_MAX (coords)    [1]
 *  }
 */
#define ECP_PUB_DER_MAX_BYTES (30 + 2 * MBEDTLS2_ECP_MAX_BYTES)

/*
 * EC private keys:
 * ECPrivateKey ::= SEQUENCE {                  1 + 2
 *      version        INTEGER ,                1 + 1 + 1
 *      privateKey     OCTET STRING,            1 + 1 + ECP_MAX
 *      parameters [0] ECParameters OPTIONAL,   1 + 1 + (1 + 1 + 9)
 *      publicKey  [1] BIT STRING OPTIONAL      1 + 2 + [1] above
 *    }
 */
#define ECP_PRV_DER_MAX_BYTES (29 + 3 * MBEDTLS2_ECP_MAX_BYTES)

#else /* MBEDTLS2_ECP_C */

#define ECP_PUB_DER_MAX_BYTES 0
#define ECP_PRV_DER_MAX_BYTES 0

#endif /* MBEDTLS2_ECP_C */

#define PUB_DER_MAX_BYTES                                                      \
  (RSA_PUB_DER_MAX_BYTES > ECP_PUB_DER_MAX_BYTES ? RSA_PUB_DER_MAX_BYTES       \
                                                 : ECP_PUB_DER_MAX_BYTES)
#define PRV_DER_MAX_BYTES                                                      \
  (RSA_PRV_DER_MAX_BYTES > ECP_PRV_DER_MAX_BYTES ? RSA_PRV_DER_MAX_BYTES       \
                                                 : ECP_PRV_DER_MAX_BYTES)

int mbedtls2_pk_write_pubkey_pem(mbedtls2_pk_context *key,
                                        unsigned char *buf, size_t size) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  unsigned char output_buf[PUB_DER_MAX_BYTES];
  size_t olen = 0;

  PK_VALIDATE_RET(key != NULL);
  PK_VALIDATE_RET(buf != NULL || size == 0);

  if ((ret = mbedtls2_pk_write_pubkey_der(key, output_buf,
                                                 sizeof(output_buf))) < 0) {
    return (ret);
  }

  if ((ret = mbedtls2_pem_write_buffer(
           PEM_BEGIN_PUBLIC_KEY, PEM_END_PUBLIC_KEY,
           output_buf + sizeof(output_buf) - ret, ret, buf, size, &olen)) !=
      0) {
    return (ret);
  }

  return (0);
}

int mbedtls2_pk_write_key_pem(mbedtls2_pk_context *key,
                                     unsigned char *buf, size_t size) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  unsigned char output_buf[PRV_DER_MAX_BYTES];
  const char *begin, *end;
  size_t olen = 0;

  PK_VALIDATE_RET(key != NULL);
  PK_VALIDATE_RET(buf != NULL || size == 0);

  if ((ret = mbedtls2_pk_write_key_der(key, output_buf,
                                              sizeof(output_buf))) < 0)
    return (ret);

#if defined(MBEDTLS2_RSA_C)
  if (mbedtls2_pk_get_type(key) == MBEDTLS2_PK_RSA) {
    begin = PEM_BEGIN_PRIVATE_KEY_RSA;
    end = PEM_END_PRIVATE_KEY_RSA;
  } else
#endif
#if defined(MBEDTLS2_ECP_C)
      if (mbedtls2_pk_get_type(key) == MBEDTLS2_PK_ECKEY) {
    begin = PEM_BEGIN_PRIVATE_KEY_EC;
    end = PEM_END_PRIVATE_KEY_EC;
  } else
#endif
    return (MBEDTLS2_ERR_PK_FEATURE_UNAVAILABLE);

  if ((ret = mbedtls2_pem_write_buffer(
           begin, end, output_buf + sizeof(output_buf) - ret, ret, buf, size,
           &olen)) != 0) {
    return (ret);
  }

  return (0);
}
#endif /* MBEDTLS2_PEM_WRITE_C */

#endif /* MBEDTLS2_PK_WRITE_C */

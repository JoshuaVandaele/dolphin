/*
 *  Diffie-Hellman-Merkle key exchange
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
 *  The following sources were referenced in the design of this implementation
 *  of the Diffie-Hellman-Merkle algorithm:
 *
 *  [1] Handbook of Applied Cryptography - 1997, Chapter 12
 *      Menezes, van Oorschot and Vanstone
 *
 */

#include "common.h"

#if defined(MBEDTLS2_DHM_C)

#include "mbedtls2/dhm.h"
#include "mbedtls2/error.h"
#include "mbedtls2/platform_util.h"

#include <string.h>

#if defined(MBEDTLS2_PEM_PARSE_C)
#include "mbedtls2/pem.h"
#endif

#if defined(MBEDTLS2_ASN1_PARSE_C)
#include "mbedtls2/asn1.h"
#endif

#if defined(MBEDTLS2_PLATFORM_C)
#include "mbedtls2/platform.h"
#else
#include <stdio.h>
#include <stdlib.h>
#define mbedtls2_printf printf
#define mbedtls2_calloc calloc
#define mbedtls2_free free
#endif

#if !defined(MBEDTLS2_DHM_ALT)

#define DHM_VALIDATE_RET(cond)                                                 \
  MBEDTLS2_INTERNAL_VALIDATE_RET(                                       \
      cond, MBEDTLS2_ERR_DHM_BAD_INPUT_DATA)
#define DHM_VALIDATE(cond) MBEDTLS2_INTERNAL_VALIDATE(cond)

/*
 * helper to validate the mbedtls2_mpi size and import it
 */
static int dhm_read_bignum(mbedtls2_mpi *X, unsigned char **p,
                           const unsigned char *end) {
  int ret, n;

  if (end - *p < 2)
    return (MBEDTLS2_ERR_DHM_BAD_INPUT_DATA);

  n = ((*p)[0] << 8) | (*p)[1];
  (*p) += 2;

  if ((int)(end - *p) < n)
    return (MBEDTLS2_ERR_DHM_BAD_INPUT_DATA);

  if ((ret = mbedtls2_mpi_read_binary(X, *p, n)) != 0)
    return (MBEDTLS2_ERROR_ADD(
        MBEDTLS2_ERR_DHM_READ_PARAMS_FAILED, ret));

  (*p) += n;

  return (0);
}

/*
 * Verify sanity of parameter with regards to P
 *
 * Parameter should be: 2 <= public_param <= P - 2
 *
 * This means that we need to return an error if
 *              public_param < 2 or public_param > P-2
 *
 * For more information on the attack, see:
 *  http://www.cl.cam.ac.uk/~rja14/Papers/psandqs.pdf
 *  http://web.nvd.nist.gov/view/vuln/detail?vulnId=CVE-2005-2643
 */
static int dhm_check_range(const mbedtls2_mpi *param,
                           const mbedtls2_mpi *P) {
  mbedtls2_mpi U;
  int ret = 0;

  mbedtls2_mpi_init(&U);

  MBEDTLS2_MPI_CHK(mbedtls2_mpi_sub_int(&U, P, 2));

  if (mbedtls2_mpi_cmp_int(param, 2) < 0 ||
      mbedtls2_mpi_cmp_mpi(param, &U) > 0) {
    ret = MBEDTLS2_ERR_DHM_BAD_INPUT_DATA;
  }

cleanup:
  mbedtls2_mpi_free(&U);
  return (ret);
}

void mbedtls2_dhm_init(mbedtls2_dhm_context *ctx) {
  DHM_VALIDATE(ctx != NULL);
  memset(ctx, 0, sizeof(mbedtls2_dhm_context));
}

/*
 * Parse the ServerKeyExchange parameters
 */
int mbedtls2_dhm_read_params(mbedtls2_dhm_context *ctx,
                                    unsigned char **p,
                                    const unsigned char *end) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  DHM_VALIDATE_RET(ctx != NULL);
  DHM_VALIDATE_RET(p != NULL && *p != NULL);
  DHM_VALIDATE_RET(end != NULL);

  if ((ret = dhm_read_bignum(&ctx->P, p, end)) != 0 ||
      (ret = dhm_read_bignum(&ctx->G, p, end)) != 0 ||
      (ret = dhm_read_bignum(&ctx->GY, p, end)) != 0)
    return (ret);

  if ((ret = dhm_check_range(&ctx->GY, &ctx->P)) != 0)
    return (ret);

  ctx->len = mbedtls2_mpi_size(&ctx->P);

  return (0);
}

/*
 * Pick a random R in the range [2, M-2] for blinding or key generation.
 */
static int dhm_random_below(mbedtls2_mpi *R,
                            const mbedtls2_mpi *M,
                            int (*f_rng)(void *, unsigned char *, size_t),
                            void *p_rng) {
  int ret;

  MBEDTLS2_MPI_CHK(mbedtls2_mpi_random(R, 3, M, f_rng, p_rng));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_sub_int(R, R, 1));

cleanup:
  return (ret);
}

static int dhm_make_common(mbedtls2_dhm_context *ctx, int x_size,
                           int (*f_rng)(void *, unsigned char *, size_t),
                           void *p_rng) {
  int ret = 0;

  if (mbedtls2_mpi_cmp_int(&ctx->P, 0) == 0)
    return (MBEDTLS2_ERR_DHM_BAD_INPUT_DATA);
  if (x_size < 0)
    return (MBEDTLS2_ERR_DHM_BAD_INPUT_DATA);

  if ((unsigned)x_size < mbedtls2_mpi_size(&ctx->P)) {
    MBEDTLS2_MPI_CHK(
        mbedtls2_mpi_fill_random(&ctx->X, x_size, f_rng, p_rng));
  } else {
    /* Generate X as large as possible ( <= P - 2 ) */
    ret = dhm_random_below(&ctx->X, &ctx->P, f_rng, p_rng);
    if (ret == MBEDTLS2_ERR_MPI_NOT_ACCEPTABLE)
      return (MBEDTLS2_ERR_DHM_MAKE_PARAMS_FAILED);
    if (ret != 0)
      return (ret);
  }

  /*
   * Calculate GX = G^X mod P
   */
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_exp_mod(
      &ctx->GX, &ctx->G, &ctx->X, &ctx->P, &ctx->RP));

  if ((ret = dhm_check_range(&ctx->GX, &ctx->P)) != 0)
    return (ret);

cleanup:
  return (ret);
}

/*
 * Setup and write the ServerKeyExchange parameters
 */
int mbedtls2_dhm_make_params(
    mbedtls2_dhm_context *ctx, int x_size, unsigned char *output,
    size_t *olen, int (*f_rng)(void *, unsigned char *, size_t), void *p_rng) {
  int ret;
  size_t n1, n2, n3;
  unsigned char *p;
  DHM_VALIDATE_RET(ctx != NULL);
  DHM_VALIDATE_RET(output != NULL);
  DHM_VALIDATE_RET(olen != NULL);
  DHM_VALIDATE_RET(f_rng != NULL);

  ret = dhm_make_common(ctx, x_size, f_rng, p_rng);
  if (ret != 0)
    goto cleanup;

  /*
   * Export P, G, GX. RFC 5246 §4.4 states that "leading zero octets are
   * not required". We omit leading zeros for compactness.
   */
#define DHM_MPI_EXPORT(X, n)                                                   \
  do {                                                                         \
    MBEDTLS2_MPI_CHK(                                                   \
        mbedtls2_mpi_write_binary((X), p + 2, (n)));                    \
    *p++ = MBEDTLS2_BYTE_1(n);                                          \
    *p++ = MBEDTLS2_BYTE_0(n);                                          \
    p += (n);                                                                  \
  } while (0)

  n1 = mbedtls2_mpi_size(&ctx->P);
  n2 = mbedtls2_mpi_size(&ctx->G);
  n3 = mbedtls2_mpi_size(&ctx->GX);

  p = output;
  DHM_MPI_EXPORT(&ctx->P, n1);
  DHM_MPI_EXPORT(&ctx->G, n2);
  DHM_MPI_EXPORT(&ctx->GX, n3);

  *olen = p - output;

  ctx->len = n1;

cleanup:
  if (ret != 0 && ret > -128)
    ret = MBEDTLS2_ERROR_ADD(MBEDTLS2_ERR_DHM_MAKE_PARAMS_FAILED,
                                    ret);
  return (ret);
}

/*
 * Set prime modulus and generator
 */
int mbedtls2_dhm_set_group(mbedtls2_dhm_context *ctx,
                                  const mbedtls2_mpi *P,
                                  const mbedtls2_mpi *G) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  DHM_VALIDATE_RET(ctx != NULL);
  DHM_VALIDATE_RET(P != NULL);
  DHM_VALIDATE_RET(G != NULL);

  if ((ret = mbedtls2_mpi_copy(&ctx->P, P)) != 0 ||
      (ret = mbedtls2_mpi_copy(&ctx->G, G)) != 0) {
    return (MBEDTLS2_ERROR_ADD(MBEDTLS2_ERR_DHM_SET_GROUP_FAILED,
                                      ret));
  }

  ctx->len = mbedtls2_mpi_size(&ctx->P);
  return (0);
}

/*
 * Import the peer's public value G^Y
 */
int mbedtls2_dhm_read_public(mbedtls2_dhm_context *ctx,
                                    const unsigned char *input, size_t ilen) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  DHM_VALIDATE_RET(ctx != NULL);
  DHM_VALIDATE_RET(input != NULL);

  if (ilen < 1 || ilen > ctx->len)
    return (MBEDTLS2_ERR_DHM_BAD_INPUT_DATA);

  if ((ret = mbedtls2_mpi_read_binary(&ctx->GY, input, ilen)) != 0)
    return (MBEDTLS2_ERROR_ADD(
        MBEDTLS2_ERR_DHM_READ_PUBLIC_FAILED, ret));

  return (0);
}

/*
 * Create own private value X and export G^X
 */
int mbedtls2_dhm_make_public(
    mbedtls2_dhm_context *ctx, int x_size, unsigned char *output,
    size_t olen, int (*f_rng)(void *, unsigned char *, size_t), void *p_rng) {
  int ret;
  DHM_VALIDATE_RET(ctx != NULL);
  DHM_VALIDATE_RET(output != NULL);
  DHM_VALIDATE_RET(f_rng != NULL);

  if (olen < 1 || olen > ctx->len)
    return (MBEDTLS2_ERR_DHM_BAD_INPUT_DATA);

  ret = dhm_make_common(ctx, x_size, f_rng, p_rng);
  if (ret == MBEDTLS2_ERR_DHM_MAKE_PARAMS_FAILED)
    return (MBEDTLS2_ERR_DHM_MAKE_PUBLIC_FAILED);
  if (ret != 0)
    goto cleanup;

  MBEDTLS2_MPI_CHK(
      mbedtls2_mpi_write_binary(&ctx->GX, output, olen));

cleanup:
  if (ret != 0 && ret > -128)
    ret = MBEDTLS2_ERROR_ADD(MBEDTLS2_ERR_DHM_MAKE_PUBLIC_FAILED,
                                    ret);
  return (ret);
}

/*
 * Use the blinding method and optimisation suggested in section 10 of:
 *  KOCHER, Paul C. Timing attacks on implementations of Diffie-Hellman, RSA,
 *  DSS, and other systems. In : Advances in Cryptology-CRYPTO'96. Springer
 *  Berlin Heidelberg, 1996. p. 104-113.
 */
static int dhm_update_blinding(mbedtls2_dhm_context *ctx,
                               int (*f_rng)(void *, unsigned char *, size_t),
                               void *p_rng) {
  int ret;
  mbedtls2_mpi R;

  mbedtls2_mpi_init(&R);

  /*
   * Don't use any blinding the first time a particular X is used,
   * but remember it to use blinding next time.
   */
  if (mbedtls2_mpi_cmp_mpi(&ctx->X, &ctx->pX) != 0) {
    MBEDTLS2_MPI_CHK(mbedtls2_mpi_copy(&ctx->pX, &ctx->X));
    MBEDTLS2_MPI_CHK(mbedtls2_mpi_lset(&ctx->Vi, 1));
    MBEDTLS2_MPI_CHK(mbedtls2_mpi_lset(&ctx->Vf, 1));

    return (0);
  }

  /*
   * Ok, we need blinding. Can we re-use existing values?
   * If yes, just update them by squaring them.
   */
  if (mbedtls2_mpi_cmp_int(&ctx->Vi, 1) != 0) {
    MBEDTLS2_MPI_CHK(
        mbedtls2_mpi_mul_mpi(&ctx->Vi, &ctx->Vi, &ctx->Vi));
    MBEDTLS2_MPI_CHK(
        mbedtls2_mpi_mod_mpi(&ctx->Vi, &ctx->Vi, &ctx->P));

    MBEDTLS2_MPI_CHK(
        mbedtls2_mpi_mul_mpi(&ctx->Vf, &ctx->Vf, &ctx->Vf));
    MBEDTLS2_MPI_CHK(
        mbedtls2_mpi_mod_mpi(&ctx->Vf, &ctx->Vf, &ctx->P));

    return (0);
  }

  /*
   * We need to generate blinding values from scratch
   */

  /* Vi = random( 2, P-2 ) */
  MBEDTLS2_MPI_CHK(dhm_random_below(&ctx->Vi, &ctx->P, f_rng, p_rng));

  /* Vf = Vi^-X mod P
   * First compute Vi^-1 = R * (R Vi)^-1, (avoiding leaks from inv_mod),
   * then elevate to the Xth power. */
  MBEDTLS2_MPI_CHK(dhm_random_below(&R, &ctx->P, f_rng, p_rng));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mpi(&ctx->Vf, &ctx->Vi, &R));
  MBEDTLS2_MPI_CHK(
      mbedtls2_mpi_mod_mpi(&ctx->Vf, &ctx->Vf, &ctx->P));
  MBEDTLS2_MPI_CHK(
      mbedtls2_mpi_inv_mod(&ctx->Vf, &ctx->Vf, &ctx->P));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mpi(&ctx->Vf, &ctx->Vf, &R));
  MBEDTLS2_MPI_CHK(
      mbedtls2_mpi_mod_mpi(&ctx->Vf, &ctx->Vf, &ctx->P));

  MBEDTLS2_MPI_CHK(mbedtls2_mpi_exp_mod(
      &ctx->Vf, &ctx->Vf, &ctx->X, &ctx->P, &ctx->RP));

cleanup:
  mbedtls2_mpi_free(&R);

  return (ret);
}

/*
 * Derive and export the shared secret (G^Y)^X mod P
 */
int mbedtls2_dhm_calc_secret(
    mbedtls2_dhm_context *ctx, unsigned char *output, size_t output_size,
    size_t *olen, int (*f_rng)(void *, unsigned char *, size_t), void *p_rng) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  mbedtls2_mpi GYb;
  DHM_VALIDATE_RET(ctx != NULL);
  DHM_VALIDATE_RET(output != NULL);
  DHM_VALIDATE_RET(olen != NULL);

  if (output_size < ctx->len)
    return (MBEDTLS2_ERR_DHM_BAD_INPUT_DATA);

  if ((ret = dhm_check_range(&ctx->GY, &ctx->P)) != 0)
    return (ret);

  mbedtls2_mpi_init(&GYb);

  /* Blind peer's value */
  if (f_rng != NULL) {
    MBEDTLS2_MPI_CHK(dhm_update_blinding(ctx, f_rng, p_rng));
    MBEDTLS2_MPI_CHK(
        mbedtls2_mpi_mul_mpi(&GYb, &ctx->GY, &ctx->Vi));
    MBEDTLS2_MPI_CHK(mbedtls2_mpi_mod_mpi(&GYb, &GYb, &ctx->P));
  } else
    MBEDTLS2_MPI_CHK(mbedtls2_mpi_copy(&GYb, &ctx->GY));

  /* Do modular exponentiation */
  MBEDTLS2_MPI_CHK(
      mbedtls2_mpi_exp_mod(&ctx->K, &GYb, &ctx->X, &ctx->P, &ctx->RP));

  /* Unblind secret value */
  if (f_rng != NULL) {
    MBEDTLS2_MPI_CHK(
        mbedtls2_mpi_mul_mpi(&ctx->K, &ctx->K, &ctx->Vf));
    MBEDTLS2_MPI_CHK(
        mbedtls2_mpi_mod_mpi(&ctx->K, &ctx->K, &ctx->P));
  }

  /* Output the secret without any leading zero byte. This is mandatory
   * for TLS per RFC 5246 §8.1.2. */
  *olen = mbedtls2_mpi_size(&ctx->K);
  MBEDTLS2_MPI_CHK(
      mbedtls2_mpi_write_binary(&ctx->K, output, *olen));

cleanup:
  mbedtls2_mpi_free(&GYb);

  if (ret != 0)
    return (MBEDTLS2_ERROR_ADD(
        MBEDTLS2_ERR_DHM_CALC_SECRET_FAILED, ret));

  return (0);
}

/*
 * Free the components of a DHM key
 */
void mbedtls2_dhm_free(mbedtls2_dhm_context *ctx) {
  if (ctx == NULL)
    return;

  mbedtls2_mpi_free(&ctx->pX);
  mbedtls2_mpi_free(&ctx->Vf);
  mbedtls2_mpi_free(&ctx->Vi);
  mbedtls2_mpi_free(&ctx->RP);
  mbedtls2_mpi_free(&ctx->K);
  mbedtls2_mpi_free(&ctx->GY);
  mbedtls2_mpi_free(&ctx->GX);
  mbedtls2_mpi_free(&ctx->X);
  mbedtls2_mpi_free(&ctx->G);
  mbedtls2_mpi_free(&ctx->P);

  mbedtls2_platform_zeroize(ctx, sizeof(mbedtls2_dhm_context));
}

#if defined(MBEDTLS2_ASN1_PARSE_C)
/*
 * Parse DHM parameters
 */
int mbedtls2_dhm_parse_dhm(mbedtls2_dhm_context *dhm,
                                  const unsigned char *dhmin, size_t dhminlen) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  size_t len;
  unsigned char *p, *end;
#if defined(MBEDTLS2_PEM_PARSE_C)
  mbedtls2_pem_context pem;
#endif /* MBEDTLS2_PEM_PARSE_C */

  DHM_VALIDATE_RET(dhm != NULL);
  DHM_VALIDATE_RET(dhmin != NULL);

#if defined(MBEDTLS2_PEM_PARSE_C)
  mbedtls2_pem_init(&pem);

  /* Avoid calling mbedtls2_pem_read_buffer() on non-null-terminated
   * string */
  if (dhminlen == 0 || dhmin[dhminlen - 1] != '\0')
    ret = MBEDTLS2_ERR_PEM_NO_HEADER_FOOTER_PRESENT;
  else
    ret = mbedtls2_pem_read_buffer(&pem, "-----BEGIN DH PARAMETERS-----",
                                          "-----END DH PARAMETERS-----", dhmin,
                                          NULL, 0, &dhminlen);

  if (ret == 0) {
    /*
     * Was PEM encoded
     */
    dhminlen = pem.buflen;
  } else if (ret != MBEDTLS2_ERR_PEM_NO_HEADER_FOOTER_PRESENT)
    goto exit;

  p = (ret == 0) ? pem.buf : (unsigned char *)dhmin;
#else
  p = (unsigned char *)dhmin;
#endif /* MBEDTLS2_PEM_PARSE_C */
  end = p + dhminlen;

  /*
   *  DHParams ::= SEQUENCE {
   *      prime              INTEGER,  -- P
   *      generator          INTEGER,  -- g
   *      privateValueLength INTEGER OPTIONAL
   *  }
   */
  if ((ret = mbedtls2_asn1_get_tag(&p, end, &len,
                                          MBEDTLS2_ASN1_CONSTRUCTED |
                                              MBEDTLS2_ASN1_SEQUENCE)) !=
      0) {
    ret =
        MBEDTLS2_ERROR_ADD(MBEDTLS2_ERR_DHM_INVALID_FORMAT, ret);
    goto exit;
  }

  end = p + len;

  if ((ret = mbedtls2_asn1_get_mpi(&p, end, &dhm->P)) != 0 ||
      (ret = mbedtls2_asn1_get_mpi(&p, end, &dhm->G)) != 0) {
    ret =
        MBEDTLS2_ERROR_ADD(MBEDTLS2_ERR_DHM_INVALID_FORMAT, ret);
    goto exit;
  }

  if (p != end) {
    /* This might be the optional privateValueLength.
     * If so, we can cleanly discard it */
    mbedtls2_mpi rec;
    mbedtls2_mpi_init(&rec);
    ret = mbedtls2_asn1_get_mpi(&p, end, &rec);
    mbedtls2_mpi_free(&rec);
    if (ret != 0) {
      ret = MBEDTLS2_ERROR_ADD(MBEDTLS2_ERR_DHM_INVALID_FORMAT,
                                      ret);
      goto exit;
    }
    if (p != end) {
      ret = MBEDTLS2_ERROR_ADD(MBEDTLS2_ERR_DHM_INVALID_FORMAT,
                                      MBEDTLS2_ERR_ASN1_LENGTH_MISMATCH);
      goto exit;
    }
  }

  ret = 0;

  dhm->len = mbedtls2_mpi_size(&dhm->P);

exit:
#if defined(MBEDTLS2_PEM_PARSE_C)
  mbedtls2_pem_free(&pem);
#endif
  if (ret != 0)
    mbedtls2_dhm_free(dhm);

  return (ret);
}

#if defined(MBEDTLS2_FS_IO)
/*
 * Load all data from a file into a given buffer.
 *
 * The file is expected to contain either PEM or DER encoded data.
 * A terminating null byte is always appended. It is included in the announced
 * length only if the data looks like it is PEM encoded.
 */
static int load_file(const char *path, unsigned char **buf, size_t *n) {
  FILE *f;
  long size;

  if ((f = fopen(path, "rb")) == NULL)
    return (MBEDTLS2_ERR_DHM_FILE_IO_ERROR);

  fseek(f, 0, SEEK_END);
  if ((size = ftell(f)) == -1) {
    fclose(f);
    return (MBEDTLS2_ERR_DHM_FILE_IO_ERROR);
  }
  fseek(f, 0, SEEK_SET);

  *n = (size_t)size;

  if (*n + 1 == 0 || (*buf = mbedtls2_calloc(1, *n + 1)) == NULL) {
    fclose(f);
    return (MBEDTLS2_ERR_DHM_ALLOC_FAILED);
  }

  if (fread(*buf, 1, *n, f) != *n) {
    fclose(f);

    mbedtls2_platform_zeroize(*buf, *n + 1);
    mbedtls2_free(*buf);

    return (MBEDTLS2_ERR_DHM_FILE_IO_ERROR);
  }

  fclose(f);

  (*buf)[*n] = '\0';

  if (strstr((const char *)*buf, "-----BEGIN ") != NULL)
    ++*n;

  return (0);
}

/*
 * Load and parse DHM parameters
 */
int mbedtls2_dhm_parse_dhmfile(mbedtls2_dhm_context *dhm,
                                      const char *path) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  size_t n;
  unsigned char *buf;
  DHM_VALIDATE_RET(dhm != NULL);
  DHM_VALIDATE_RET(path != NULL);

  if ((ret = load_file(path, &buf, &n)) != 0)
    return (ret);

  ret = mbedtls2_dhm_parse_dhm(dhm, buf, n);

  mbedtls2_platform_zeroize(buf, n);
  mbedtls2_free(buf);

  return (ret);
}
#endif /* MBEDTLS2_FS_IO */
#endif /* MBEDTLS2_ASN1_PARSE_C */
#endif /* MBEDTLS2_DHM_ALT */

#if defined(MBEDTLS2_SELF_TEST)

#if defined(MBEDTLS2_PEM_PARSE_C)
static const char mbedtls2_test_dhm_params[] =
    "-----BEGIN DH PARAMETERS-----\r\n"
    "MIGHAoGBAJ419DBEOgmQTzo5qXl5fQcN9TN455wkOL7052HzxxRVMyhYmwQcgJvh\r\n"
    "1sa18fyfR9OiVEMYglOpkqVoGLN7qd5aQNNi5W7/C+VBdHTBJcGZJyyP5B3qcz32\r\n"
    "9mLJKudlVudV0Qxk5qUJaPZ/xupz0NyoVpviuiBOI1gNi8ovSXWzAgEC\r\n"
    "-----END DH PARAMETERS-----\r\n";
#else  /* MBEDTLS2_PEM_PARSE_C */
static const char mbedtls2_test_dhm_params[] = {
    0x30, 0x81, 0x87, 0x02, 0x81, 0x81, 0x00, 0x9e, 0x35, 0xf4, 0x30, 0x44,
    0x3a, 0x09, 0x90, 0x4f, 0x3a, 0x39, 0xa9, 0x79, 0x79, 0x7d, 0x07, 0x0d,
    0xf5, 0x33, 0x78, 0xe7, 0x9c, 0x24, 0x38, 0xbe, 0xf4, 0xe7, 0x61, 0xf3,
    0xc7, 0x14, 0x55, 0x33, 0x28, 0x58, 0x9b, 0x04, 0x1c, 0x80, 0x9b, 0xe1,
    0xd6, 0xc6, 0xb5, 0xf1, 0xfc, 0x9f, 0x47, 0xd3, 0xa2, 0x54, 0x43, 0x18,
    0x82, 0x53, 0xa9, 0x92, 0xa5, 0x68, 0x18, 0xb3, 0x7b, 0xa9, 0xde, 0x5a,
    0x40, 0xd3, 0x62, 0xe5, 0x6e, 0xff, 0x0b, 0xe5, 0x41, 0x74, 0x74, 0xc1,
    0x25, 0xc1, 0x99, 0x27, 0x2c, 0x8f, 0xe4, 0x1d, 0xea, 0x73, 0x3d, 0xf6,
    0xf6, 0x62, 0xc9, 0x2a, 0xe7, 0x65, 0x56, 0xe7, 0x55, 0xd1, 0x0c, 0x64,
    0xe6, 0xa5, 0x09, 0x68, 0xf6, 0x7f, 0xc6, 0xea, 0x73, 0xd0, 0xdc, 0xa8,
    0x56, 0x9b, 0xe2, 0xba, 0x20, 0x4e, 0x23, 0x58, 0x0d, 0x8b, 0xca, 0x2f,
    0x49, 0x75, 0xb3, 0x02, 0x01, 0x02};
#endif /* MBEDTLS2_PEM_PARSE_C */

static const size_t mbedtls2_test_dhm_params_len =
    sizeof(mbedtls2_test_dhm_params);

/*
 * Checkup routine
 */
int mbedtls2_dhm_self_test(int verbose) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  mbedtls2_dhm_context dhm;

  mbedtls2_dhm_init(&dhm);

  if (verbose != 0)
    mbedtls2_printf("  DHM parameter load: ");

  if ((ret = mbedtls2_dhm_parse_dhm(
           &dhm, (const unsigned char *)mbedtls2_test_dhm_params,
           mbedtls2_test_dhm_params_len)) != 0) {
    if (verbose != 0)
      mbedtls2_printf("failed\n");

    ret = 1;
    goto exit;
  }

  if (verbose != 0)
    mbedtls2_printf("passed\n\n");

exit:
  mbedtls2_dhm_free(&dhm);

  return (ret);
}

#endif /* MBEDTLS2_SELF_TEST */

#endif /* MBEDTLS2_DHM_C */

/*
 *  X.509 Certificate Signing Request (CSR) parsing
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
 *  The ITU-T X.509 standard defines a certificate format for PKI.
 *
 *  http://www.ietf.org/rfc/rfc5280.txt (Certificates and CRLs)
 *  http://www.ietf.org/rfc/rfc3279.txt (Alg IDs for CRLs)
 *  http://www.ietf.org/rfc/rfc2986.txt (CSRs, aka PKCS#10)
 *
 *  http://www.itu.int/ITU-T/studygroups/com17/languages/X.680-0207.pdf
 *  http://www.itu.int/ITU-T/studygroups/com17/languages/X.690-0207.pdf
 */

#include "common.h"

#if defined(MBEDTLS2_X509_CSR_PARSE_C)

#include "mbedtls2/error.h"
#include "mbedtls2/oid.h"
#include "mbedtls2/platform_util.h"
#include "mbedtls2/x509_csr.h"

#include <string.h>

#if defined(MBEDTLS2_PEM_PARSE_C)
#include "mbedtls2/pem.h"
#endif

#if defined(MBEDTLS2_PLATFORM_C)
#include "mbedtls2/platform.h"
#else
#include <stdio.h>
#include <stdlib.h>
#define mbedtls2_free free
#define mbedtls2_calloc calloc
#define mbedtls2_snprintf snprintf
#endif

#if defined(MBEDTLS2_FS_IO) || defined(EFIX64) || defined(EFI32)
#include <stdio.h>
#endif

/*
 *  Version  ::=  INTEGER  {  v1(0)  }
 */
static int x509_csr_get_version(unsigned char **p, const unsigned char *end,
                                int *ver) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;

  if ((ret = mbedtls2_asn1_get_int(p, end, ver)) != 0) {
    if (ret == MBEDTLS2_ERR_ASN1_UNEXPECTED_TAG) {
      *ver = 0;
      return (0);
    }

    return (MBEDTLS2_ERROR_ADD(MBEDTLS2_ERR_X509_INVALID_VERSION,
                                      ret));
  }

  return (0);
}

/*
 * Parse a CSR in DER format
 */
int mbedtls2_x509_csr_parse_der(mbedtls2_x509_csr *csr,
                                       const unsigned char *buf,
                                       size_t buflen) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  size_t len;
  unsigned char *p, *end;
  mbedtls2_x509_buf sig_params;

  memset(&sig_params, 0, sizeof(mbedtls2_x509_buf));

  /*
   * Check for valid input
   */
  if (csr == NULL || buf == NULL || buflen == 0)
    return (MBEDTLS2_ERR_X509_BAD_INPUT_DATA);

  mbedtls2_x509_csr_init(csr);

  /*
   * first copy the raw DER data
   */
  p = mbedtls2_calloc(1, len = buflen);

  if (p == NULL)
    return (MBEDTLS2_ERR_X509_ALLOC_FAILED);

  memcpy(p, buf, buflen);

  csr->raw.p = p;
  csr->raw.len = len;
  end = p + len;

  /*
   *  CertificationRequest ::= SEQUENCE {
   *       certificationRequestInfo CertificationRequestInfo,
   *       signatureAlgorithm AlgorithmIdentifier,
   *       signature          BIT STRING
   *  }
   */
  if ((ret = mbedtls2_asn1_get_tag(&p, end, &len,
                                          MBEDTLS2_ASN1_CONSTRUCTED |
                                              MBEDTLS2_ASN1_SEQUENCE)) !=
      0) {
    mbedtls2_x509_csr_free(csr);
    return (MBEDTLS2_ERR_X509_INVALID_FORMAT);
  }

  if (len != (size_t)(end - p)) {
    mbedtls2_x509_csr_free(csr);
    return (
        MBEDTLS2_ERROR_ADD(MBEDTLS2_ERR_X509_INVALID_FORMAT,
                                  MBEDTLS2_ERR_ASN1_LENGTH_MISMATCH));
  }

  /*
   *  CertificationRequestInfo ::= SEQUENCE {
   */
  csr->cri.p = p;

  if ((ret = mbedtls2_asn1_get_tag(&p, end, &len,
                                          MBEDTLS2_ASN1_CONSTRUCTED |
                                              MBEDTLS2_ASN1_SEQUENCE)) !=
      0) {
    mbedtls2_x509_csr_free(csr);
    return (MBEDTLS2_ERROR_ADD(MBEDTLS2_ERR_X509_INVALID_FORMAT,
                                      ret));
  }

  end = p + len;
  csr->cri.len = end - csr->cri.p;

  /*
   *  Version  ::=  INTEGER {  v1(0) }
   */
  if ((ret = x509_csr_get_version(&p, end, &csr->version)) != 0) {
    mbedtls2_x509_csr_free(csr);
    return (ret);
  }

  if (csr->version != 0) {
    mbedtls2_x509_csr_free(csr);
    return (MBEDTLS2_ERR_X509_UNKNOWN_VERSION);
  }

  csr->version++;

  /*
   *  subject               Name
   */
  csr->subject_raw.p = p;

  if ((ret = mbedtls2_asn1_get_tag(&p, end, &len,
                                          MBEDTLS2_ASN1_CONSTRUCTED |
                                              MBEDTLS2_ASN1_SEQUENCE)) !=
      0) {
    mbedtls2_x509_csr_free(csr);
    return (MBEDTLS2_ERROR_ADD(MBEDTLS2_ERR_X509_INVALID_FORMAT,
                                      ret));
  }

  if ((ret = mbedtls2_x509_get_name(&p, p + len, &csr->subject)) != 0) {
    mbedtls2_x509_csr_free(csr);
    return (ret);
  }

  csr->subject_raw.len = p - csr->subject_raw.p;

  /*
   *  subjectPKInfo SubjectPublicKeyInfo
   */
  if ((ret = mbedtls2_pk_parse_subpubkey(&p, end, &csr->pk)) != 0) {
    mbedtls2_x509_csr_free(csr);
    return (ret);
  }

  /*
   *  attributes    [0] Attributes
   *
   *  The list of possible attributes is open-ended, though RFC 2985
   *  (PKCS#9) defines a few in section 5.4. We currently don't support any,
   *  so we just ignore them. This is a safe thing to do as the worst thing
   *  that could happen is that we issue a certificate that does not match
   *  the requester's expectations - this cannot cause a violation of our
   *  signature policies.
   */
  if ((ret = mbedtls2_asn1_get_tag(
           &p, end, &len,
           MBEDTLS2_ASN1_CONSTRUCTED |
               MBEDTLS2_ASN1_CONTEXT_SPECIFIC)) != 0) {
    mbedtls2_x509_csr_free(csr);
    return (MBEDTLS2_ERROR_ADD(MBEDTLS2_ERR_X509_INVALID_FORMAT,
                                      ret));
  }

  p += len;

  end = csr->raw.p + csr->raw.len;

  /*
   *  signatureAlgorithm   AlgorithmIdentifier,
   *  signature            BIT STRING
   */
  if ((ret = mbedtls2_x509_get_alg(&p, end, &csr->sig_oid,
                                          &sig_params)) != 0) {
    mbedtls2_x509_csr_free(csr);
    return (ret);
  }

  if ((ret = mbedtls2_x509_get_sig_alg(&csr->sig_oid, &sig_params,
                                              &csr->sig_md, &csr->sig_pk,
                                              &csr->sig_opts)) != 0) {
    mbedtls2_x509_csr_free(csr);
    return (MBEDTLS2_ERR_X509_UNKNOWN_SIG_ALG);
  }

  if ((ret = mbedtls2_x509_get_sig(&p, end, &csr->sig)) != 0) {
    mbedtls2_x509_csr_free(csr);
    return (ret);
  }

  if (p != end) {
    mbedtls2_x509_csr_free(csr);
    return (
        MBEDTLS2_ERROR_ADD(MBEDTLS2_ERR_X509_INVALID_FORMAT,
                                  MBEDTLS2_ERR_ASN1_LENGTH_MISMATCH));
  }

  return (0);
}

/*
 * Parse a CSR, allowing for PEM or raw DER encoding
 */
int mbedtls2_x509_csr_parse(mbedtls2_x509_csr *csr,
                                   const unsigned char *buf, size_t buflen) {
#if defined(MBEDTLS2_PEM_PARSE_C)
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  size_t use_len;
  mbedtls2_pem_context pem;
#endif

  /*
   * Check for valid input
   */
  if (csr == NULL || buf == NULL || buflen == 0)
    return (MBEDTLS2_ERR_X509_BAD_INPUT_DATA);

#if defined(MBEDTLS2_PEM_PARSE_C)
  /* Avoid calling mbedtls2_pem_read_buffer() on non-null-terminated
   * string */
  if (buf[buflen - 1] == '\0') {
    mbedtls2_pem_init(&pem);
    ret = mbedtls2_pem_read_buffer(
        &pem, "-----BEGIN CERTIFICATE REQUEST-----",
        "-----END CERTIFICATE REQUEST-----", buf, NULL, 0, &use_len);
    if (ret == MBEDTLS2_ERR_PEM_NO_HEADER_FOOTER_PRESENT) {
      ret = mbedtls2_pem_read_buffer(
          &pem, "-----BEGIN NEW CERTIFICATE REQUEST-----",
          "-----END NEW CERTIFICATE REQUEST-----", buf, NULL, 0, &use_len);
    }

    if (ret == 0) {
      /*
       * Was PEM encoded, parse the result
       */
      ret = mbedtls2_x509_csr_parse_der(csr, pem.buf, pem.buflen);
    }

    mbedtls2_pem_free(&pem);
    if (ret != MBEDTLS2_ERR_PEM_NO_HEADER_FOOTER_PRESENT)
      return (ret);
  }
#endif /* MBEDTLS2_PEM_PARSE_C */
  return (mbedtls2_x509_csr_parse_der(csr, buf, buflen));
}

#if defined(MBEDTLS2_FS_IO)
/*
 * Load a CSR into the structure
 */
int mbedtls2_x509_csr_parse_file(mbedtls2_x509_csr *csr,
                                        const char *path) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  size_t n;
  unsigned char *buf;

  if ((ret = mbedtls2_pk_load_file(path, &buf, &n)) != 0)
    return (ret);

  ret = mbedtls2_x509_csr_parse(csr, buf, n);

  mbedtls2_platform_zeroize(buf, n);
  mbedtls2_free(buf);

  return (ret);
}
#endif /* MBEDTLS2_FS_IO */

#define BEFORE_COLON 14
#define BC "14"
/*
 * Return an informational string about the CSR.
 */
int mbedtls2_x509_csr_info(char *buf, size_t size, const char *prefix,
                                  const mbedtls2_x509_csr *csr) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  size_t n;
  char *p;
  char key_size_str[BEFORE_COLON];

  p = buf;
  n = size;

  ret = mbedtls2_snprintf(p, n, "%sCSR version   : %d", prefix,
                                 csr->version);
  MBEDTLS2_X509_SAFE_SNPRINTF;

  ret = mbedtls2_snprintf(p, n, "\n%ssubject name  : ", prefix);
  MBEDTLS2_X509_SAFE_SNPRINTF;
  ret = mbedtls2_x509_dn_gets(p, n, &csr->subject);
  MBEDTLS2_X509_SAFE_SNPRINTF;

  ret = mbedtls2_snprintf(p, n, "\n%ssigned using  : ", prefix);
  MBEDTLS2_X509_SAFE_SNPRINTF;

  ret = mbedtls2_x509_sig_alg_gets(p, n, &csr->sig_oid, csr->sig_pk,
                                          csr->sig_md, csr->sig_opts);
  MBEDTLS2_X509_SAFE_SNPRINTF;

  if ((ret = mbedtls2_x509_key_size_helper(
           key_size_str, BEFORE_COLON,
           mbedtls2_pk_get_name(&csr->pk))) != 0) {
    return (ret);
  }

  ret = mbedtls2_snprintf(p, n, "\n%s%-" BC "s: %d bits\n", prefix,
                                 key_size_str,
                                 (int)mbedtls2_pk_get_bitlen(&csr->pk));
  MBEDTLS2_X509_SAFE_SNPRINTF;

  return ((int)(size - n));
}

/*
 * Initialize a CSR
 */
void mbedtls2_x509_csr_init(mbedtls2_x509_csr *csr) {
  memset(csr, 0, sizeof(mbedtls2_x509_csr));
}

/*
 * Unallocate all CSR data
 */
void mbedtls2_x509_csr_free(mbedtls2_x509_csr *csr) {
  mbedtls2_x509_name *name_cur;
  mbedtls2_x509_name *name_prv;

  if (csr == NULL)
    return;

  mbedtls2_pk_free(&csr->pk);

#if defined(MBEDTLS2_X509_RSASSA_PSS_SUPPORT)
  mbedtls2_free(csr->sig_opts);
#endif

  name_cur = csr->subject.next;
  while (name_cur != NULL) {
    name_prv = name_cur;
    name_cur = name_cur->next;
    mbedtls2_platform_zeroize(name_prv,
                                     sizeof(mbedtls2_x509_name));
    mbedtls2_free(name_prv);
  }

  if (csr->raw.p != NULL) {
    mbedtls2_platform_zeroize(csr->raw.p, csr->raw.len);
    mbedtls2_free(csr->raw.p);
  }

  mbedtls2_platform_zeroize(csr, sizeof(mbedtls2_x509_csr));
}

#endif /* MBEDTLS2_X509_CSR_PARSE_C */

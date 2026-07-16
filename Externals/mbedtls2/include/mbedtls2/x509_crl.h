/**
 * \file x509_crl.h
 *
 * \brief X.509 certificate revocation list parsing
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
#ifndef MBEDTLS2_X509_CRL_H
#define MBEDTLS2_X509_CRL_H

#if !defined(MBEDTLS2_CONFIG_FILE)
#include "mbedtls2/config.h"
#else
#include MBEDTLS2_CONFIG_FILE
#endif

#include "mbedtls2/x509.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * \addtogroup x509_module
 * \{ */

/**
 * \name Structures and functions for parsing CRLs
 * \{
 */

/**
 * Certificate revocation list entry.
 * Contains the CA-specific serial numbers and revocation dates.
 */
typedef struct mbedtls2_x509_crl_entry {
  mbedtls2_x509_buf raw;

  mbedtls2_x509_buf serial;

  mbedtls2_x509_time revocation_date;

  mbedtls2_x509_buf entry_ext;

  struct mbedtls2_x509_crl_entry *next;
} mbedtls2_x509_crl_entry;

/**
 * Certificate revocation list structure.
 * Every CRL may have multiple entries.
 */
typedef struct mbedtls2_x509_crl {
  mbedtls2_x509_buf raw; /**< The raw certificate data (DER). */
  mbedtls2_x509_buf tbs; /**< The raw certificate body (DER). The part
                                   that is To Be Signed. */

  int version;                      /**< CRL version (1=v1, 2=v2) */
  mbedtls2_x509_buf sig_oid; /**< CRL signature type identifier */

  mbedtls2_x509_buf issuer_raw; /**< The raw issuer data (DER). */

  mbedtls2_x509_name
      issuer; /**< The parsed issuer data (named information object). */

  mbedtls2_x509_time this_update;
  mbedtls2_x509_time next_update;

  mbedtls2_x509_crl_entry
      entry; /**< The CRL entries containing the certificate revocation times
                for this CA. */

  mbedtls2_x509_buf crl_ext;

  mbedtls2_x509_buf sig_oid2;
  mbedtls2_x509_buf sig;
  mbedtls2_md_type_t
      sig_md; /**< Internal representation of the MD algorithm of the signature
                 algorithm, e.g. MBEDTLS2_MD_SHA256 */
  mbedtls2_pk_type_t
      sig_pk; /**< Internal representation of the Public Key algorithm of the
                 signature algorithm, e.g. MBEDTLS2_PK_RSA */
  void *sig_opts; /**< Signature options to be passed to
                     mbedtls2_pk_verify_ext(), e.g. for RSASSA-PSS */

  struct mbedtls2_x509_crl *next;
} mbedtls2_x509_crl;

/**
 * \brief          Parse a DER-encoded CRL and append it to the chained list
 *
 * \param chain    points to the start of the chain
 * \param buf      buffer holding the CRL data in DER format
 * \param buflen   size of the buffer
 *                 (including the terminating null byte for PEM data)
 *
 * \return         0 if successful, or a specific X509 or PEM error code
 */
int mbedtls2_x509_crl_parse_der(mbedtls2_x509_crl *chain,
                                       const unsigned char *buf, size_t buflen);
/**
 * \brief          Parse one or more CRLs and append them to the chained list
 *
 * \note           Multiple CRLs are accepted only if using PEM format
 *
 * \param chain    points to the start of the chain
 * \param buf      buffer holding the CRL data in PEM or DER format
 * \param buflen   size of the buffer
 *                 (including the terminating null byte for PEM data)
 *
 * \return         0 if successful, or a specific X509 or PEM error code
 */
int mbedtls2_x509_crl_parse(mbedtls2_x509_crl *chain,
                                   const unsigned char *buf, size_t buflen);

#if defined(MBEDTLS2_FS_IO)
/**
 * \brief          Load one or more CRLs and append them to the chained list
 *
 * \note           Multiple CRLs are accepted only if using PEM format
 *
 * \param chain    points to the start of the chain
 * \param path     filename to read the CRLs from (in PEM or DER encoding)
 *
 * \return         0 if successful, or a specific X509 or PEM error code
 */
int mbedtls2_x509_crl_parse_file(mbedtls2_x509_crl *chain,
                                        const char *path);
#endif /* MBEDTLS2_FS_IO */

/**
 * \brief          Returns an informational string about the CRL.
 *
 * \param buf      Buffer to write to
 * \param size     Maximum size of buffer
 * \param prefix   A line prefix
 * \param crl      The X509 CRL to represent
 *
 * \return         The length of the string written (not including the
 *                 terminated nul byte), or a negative error code.
 */
int mbedtls2_x509_crl_info(char *buf, size_t size, const char *prefix,
                                  const mbedtls2_x509_crl *crl);

/**
 * \brief          Initialize a CRL (chain)
 *
 * \param crl      CRL chain to initialize
 */
void mbedtls2_x509_crl_init(mbedtls2_x509_crl *crl);

/**
 * \brief          Unallocate all CRL data
 *
 * \param crl      CRL chain to free
 */
void mbedtls2_x509_crl_free(mbedtls2_x509_crl *crl);

/* \} name */
/* \} addtogroup x509_module */

#ifdef __cplusplus
}
#endif

#endif /* mbedtls2_x509_crl.h */

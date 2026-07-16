/*
 *  X.509 base functions for creating certificates / CSRs
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

#if defined(MBEDTLS2_X509_CREATE_C)

#include "mbedtls2/asn1write.h"
#include "mbedtls2/error.h"
#include "mbedtls2/oid.h"
#include "mbedtls2/x509.h"

#include <string.h>

/* Structure linking OIDs for X.509 DN AttributeTypes to their
 * string representations and default string encodings used by Mbed TLS. */
typedef struct {
  const char *name; /* String representation of AttributeType, e.g.
                     * "CN" or "emailAddress". */
  size_t name_len;  /* Length of 'name', without trailing 0 byte. */
  const char *oid;  /* String representation of OID of AttributeType,
                     * as per RFC 5280, Appendix A.1. */
  int default_tag;  /* The default character encoding used for the
                     * given attribute type, e.g.
                     * MBEDTLS2_ASN1_UTF8_STRING for UTF-8. */
} x509_attr_descriptor_t;

#define ADD_STRLEN(s) s, sizeof(s) - 1

/* X.509 DN attributes from RFC 5280, Appendix A.1. */
static const x509_attr_descriptor_t x509_attrs[] = {
    {ADD_STRLEN("CN"), MBEDTLS2_OID_AT_CN,
     MBEDTLS2_ASN1_UTF8_STRING},
    {ADD_STRLEN("commonName"), MBEDTLS2_OID_AT_CN,
     MBEDTLS2_ASN1_UTF8_STRING},
    {ADD_STRLEN("C"), MBEDTLS2_OID_AT_COUNTRY,
     MBEDTLS2_ASN1_PRINTABLE_STRING},
    {ADD_STRLEN("countryName"), MBEDTLS2_OID_AT_COUNTRY,
     MBEDTLS2_ASN1_PRINTABLE_STRING},
    {ADD_STRLEN("O"), MBEDTLS2_OID_AT_ORGANIZATION,
     MBEDTLS2_ASN1_UTF8_STRING},
    {ADD_STRLEN("organizationName"), MBEDTLS2_OID_AT_ORGANIZATION,
     MBEDTLS2_ASN1_UTF8_STRING},
    {ADD_STRLEN("L"), MBEDTLS2_OID_AT_LOCALITY,
     MBEDTLS2_ASN1_UTF8_STRING},
    {ADD_STRLEN("locality"), MBEDTLS2_OID_AT_LOCALITY,
     MBEDTLS2_ASN1_UTF8_STRING},
    {ADD_STRLEN("R"), MBEDTLS2_OID_PKCS9_EMAIL,
     MBEDTLS2_ASN1_IA5_STRING},
    {ADD_STRLEN("OU"), MBEDTLS2_OID_AT_ORG_UNIT,
     MBEDTLS2_ASN1_UTF8_STRING},
    {ADD_STRLEN("organizationalUnitName"), MBEDTLS2_OID_AT_ORG_UNIT,
     MBEDTLS2_ASN1_UTF8_STRING},
    {ADD_STRLEN("ST"), MBEDTLS2_OID_AT_STATE,
     MBEDTLS2_ASN1_UTF8_STRING},
    {ADD_STRLEN("stateOrProvinceName"), MBEDTLS2_OID_AT_STATE,
     MBEDTLS2_ASN1_UTF8_STRING},
    {ADD_STRLEN("emailAddress"), MBEDTLS2_OID_PKCS9_EMAIL,
     MBEDTLS2_ASN1_IA5_STRING},
    {ADD_STRLEN("serialNumber"), MBEDTLS2_OID_AT_SERIAL_NUMBER,
     MBEDTLS2_ASN1_PRINTABLE_STRING},
    {ADD_STRLEN("postalAddress"), MBEDTLS2_OID_AT_POSTAL_ADDRESS,
     MBEDTLS2_ASN1_PRINTABLE_STRING},
    {ADD_STRLEN("postalCode"), MBEDTLS2_OID_AT_POSTAL_CODE,
     MBEDTLS2_ASN1_PRINTABLE_STRING},
    {ADD_STRLEN("dnQualifier"), MBEDTLS2_OID_AT_DN_QUALIFIER,
     MBEDTLS2_ASN1_PRINTABLE_STRING},
    {ADD_STRLEN("title"), MBEDTLS2_OID_AT_TITLE,
     MBEDTLS2_ASN1_UTF8_STRING},
    {ADD_STRLEN("surName"), MBEDTLS2_OID_AT_SUR_NAME,
     MBEDTLS2_ASN1_UTF8_STRING},
    {ADD_STRLEN("SN"), MBEDTLS2_OID_AT_SUR_NAME,
     MBEDTLS2_ASN1_UTF8_STRING},
    {ADD_STRLEN("givenName"), MBEDTLS2_OID_AT_GIVEN_NAME,
     MBEDTLS2_ASN1_UTF8_STRING},
    {ADD_STRLEN("GN"), MBEDTLS2_OID_AT_GIVEN_NAME,
     MBEDTLS2_ASN1_UTF8_STRING},
    {ADD_STRLEN("initials"), MBEDTLS2_OID_AT_INITIALS,
     MBEDTLS2_ASN1_UTF8_STRING},
    {ADD_STRLEN("pseudonym"), MBEDTLS2_OID_AT_PSEUDONYM,
     MBEDTLS2_ASN1_UTF8_STRING},
    {ADD_STRLEN("generationQualifier"),
     MBEDTLS2_OID_AT_GENERATION_QUALIFIER,
     MBEDTLS2_ASN1_UTF8_STRING},
    {ADD_STRLEN("domainComponent"), MBEDTLS2_OID_DOMAIN_COMPONENT,
     MBEDTLS2_ASN1_IA5_STRING},
    {ADD_STRLEN("DC"), MBEDTLS2_OID_DOMAIN_COMPONENT,
     MBEDTLS2_ASN1_IA5_STRING},
    {NULL, 0, NULL, MBEDTLS2_ASN1_NULL}};

static const x509_attr_descriptor_t *
x509_attr_descr_from_name(const char *name, size_t name_len) {
  const x509_attr_descriptor_t *cur;

  for (cur = x509_attrs; cur->name != NULL; cur++)
    if (cur->name_len == name_len && strncmp(cur->name, name, name_len) == 0)
      break;

  if (cur->name == NULL)
    return (NULL);

  return (cur);
}

int mbedtls2_x509_string_to_names(mbedtls2_asn1_named_data **head,
                                         const char *name) {
  int ret = 0;
  const char *s = name, *c = s;
  const char *end = s + strlen(s);
  const char *oid = NULL;
  const x509_attr_descriptor_t *attr_descr = NULL;
  int in_tag = 1;
  char data[MBEDTLS2_X509_MAX_DN_NAME_SIZE];
  char *d = data;

  /* Clear existing chain if present */
  mbedtls2_asn1_free_named_data_list(head);

  while (c <= end) {
    if (in_tag && *c == '=') {
      if ((attr_descr = x509_attr_descr_from_name(s, c - s)) == NULL) {
        ret = MBEDTLS2_ERR_X509_UNKNOWN_OID;
        goto exit;
      }

      oid = attr_descr->oid;
      s = c + 1;
      in_tag = 0;
      d = data;
    }

    if (!in_tag && *c == '\\' && c != end) {
      c++;

      /* Check for valid escaped characters */
      if (c == end || *c != ',') {
        ret = MBEDTLS2_ERR_X509_INVALID_NAME;
        goto exit;
      }
    } else if (!in_tag && (*c == ',' || c == end)) {
      mbedtls2_asn1_named_data *cur =
          mbedtls2_asn1_store_named_data(
              head, oid, strlen(oid), (unsigned char *)data, d - data);

      if (cur == NULL) {
        return (MBEDTLS2_ERR_X509_ALLOC_FAILED);
      }

      // set tagType
      cur->val.tag = attr_descr->default_tag;

      while (c < end && *(c + 1) == ' ')
        c++;

      s = c + 1;
      in_tag = 1;
    }

    if (!in_tag && s != c + 1) {
      *(d++) = *c;

      if (d - data == MBEDTLS2_X509_MAX_DN_NAME_SIZE) {
        ret = MBEDTLS2_ERR_X509_INVALID_NAME;
        goto exit;
      }
    }

    c++;
  }

exit:

  return (ret);
}

/* The first byte of the value in the mbedtls2_asn1_named_data structure
 * is reserved to store the critical boolean for us
 */
int mbedtls2_x509_set_extension(mbedtls2_asn1_named_data **head,
                                       const char *oid, size_t oid_len,
                                       int critical, const unsigned char *val,
                                       size_t val_len) {
  mbedtls2_asn1_named_data *cur;

  if ((cur = mbedtls2_asn1_store_named_data(head, oid, oid_len, NULL,
                                                   val_len + 1)) == NULL) {
    return (MBEDTLS2_ERR_X509_ALLOC_FAILED);
  }

  cur->val.p[0] = critical;
  memcpy(cur->val.p + 1, val, val_len);

  return (0);
}

/*
 *  RelativeDistinguishedName ::=
 *    SET OF AttributeTypeAndValue
 *
 *  AttributeTypeAndValue ::= SEQUENCE {
 *    type     AttributeType,
 *    value    AttributeValue }
 *
 *  AttributeType ::= OBJECT IDENTIFIER
 *
 *  AttributeValue ::= ANY DEFINED BY AttributeType
 */
static int x509_write_name(unsigned char **p, unsigned char *start,
                           mbedtls2_asn1_named_data *cur_name) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  size_t len = 0;
  const char *oid = (const char *)cur_name->oid.p;
  size_t oid_len = cur_name->oid.len;
  const unsigned char *name = cur_name->val.p;
  size_t name_len = cur_name->val.len;

  // Write correct string tag and value
  MBEDTLS2_ASN1_CHK_ADD(
      len, mbedtls2_asn1_write_tagged_string(
               p, start, cur_name->val.tag, (const char *)name, name_len));
  // Write OID
  //
  MBEDTLS2_ASN1_CHK_ADD(
      len, mbedtls2_asn1_write_oid(p, start, oid, oid_len));

  MBEDTLS2_ASN1_CHK_ADD(len,
                               mbedtls2_asn1_write_len(p, start, len));
  MBEDTLS2_ASN1_CHK_ADD(
      len, mbedtls2_asn1_write_tag(p, start,
                                          MBEDTLS2_ASN1_CONSTRUCTED |
                                              MBEDTLS2_ASN1_SEQUENCE));

  MBEDTLS2_ASN1_CHK_ADD(len,
                               mbedtls2_asn1_write_len(p, start, len));
  MBEDTLS2_ASN1_CHK_ADD(
      len, mbedtls2_asn1_write_tag(p, start,
                                          MBEDTLS2_ASN1_CONSTRUCTED |
                                              MBEDTLS2_ASN1_SET));

  return ((int)len);
}

int mbedtls2_x509_write_names(unsigned char **p, unsigned char *start,
                                     mbedtls2_asn1_named_data *first) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  size_t len = 0;
  mbedtls2_asn1_named_data *cur = first;

  while (cur != NULL) {
    MBEDTLS2_ASN1_CHK_ADD(len, x509_write_name(p, start, cur));
    cur = cur->next;
  }

  MBEDTLS2_ASN1_CHK_ADD(len,
                               mbedtls2_asn1_write_len(p, start, len));
  MBEDTLS2_ASN1_CHK_ADD(
      len, mbedtls2_asn1_write_tag(p, start,
                                          MBEDTLS2_ASN1_CONSTRUCTED |
                                              MBEDTLS2_ASN1_SEQUENCE));

  return ((int)len);
}

int mbedtls2_x509_write_sig(unsigned char **p, unsigned char *start,
                                   const char *oid, size_t oid_len,
                                   unsigned char *sig, size_t size) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  size_t len = 0;

  if (*p < start || (size_t)(*p - start) < size)
    return (MBEDTLS2_ERR_ASN1_BUF_TOO_SMALL);

  len = size;
  (*p) -= len;
  memcpy(*p, sig, len);

  if (*p - start < 1)
    return (MBEDTLS2_ERR_ASN1_BUF_TOO_SMALL);

  *--(*p) = 0;
  len += 1;

  MBEDTLS2_ASN1_CHK_ADD(len,
                               mbedtls2_asn1_write_len(p, start, len));
  MBEDTLS2_ASN1_CHK_ADD(
      len, mbedtls2_asn1_write_tag(p, start,
                                          MBEDTLS2_ASN1_BIT_STRING));

  // Write OID
  //
  MBEDTLS2_ASN1_CHK_ADD(
      len, mbedtls2_asn1_write_algorithm_identifier(p, start, oid,
                                                           oid_len, 0));

  return ((int)len);
}

static int x509_write_extension(unsigned char **p, unsigned char *start,
                                mbedtls2_asn1_named_data *ext) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  size_t len = 0;

  MBEDTLS2_ASN1_CHK_ADD(
      len, mbedtls2_asn1_write_raw_buffer(p, start, ext->val.p + 1,
                                                 ext->val.len - 1));
  MBEDTLS2_ASN1_CHK_ADD(
      len, mbedtls2_asn1_write_len(p, start, ext->val.len - 1));
  MBEDTLS2_ASN1_CHK_ADD(
      len, mbedtls2_asn1_write_tag(p, start,
                                          MBEDTLS2_ASN1_OCTET_STRING));

  if (ext->val.p[0] != 0) {
    MBEDTLS2_ASN1_CHK_ADD(len,
                                 mbedtls2_asn1_write_bool(p, start, 1));
  }

  MBEDTLS2_ASN1_CHK_ADD(len, mbedtls2_asn1_write_raw_buffer(
                                        p, start, ext->oid.p, ext->oid.len));
  MBEDTLS2_ASN1_CHK_ADD(
      len, mbedtls2_asn1_write_len(p, start, ext->oid.len));
  MBEDTLS2_ASN1_CHK_ADD(
      len, mbedtls2_asn1_write_tag(p, start, MBEDTLS2_ASN1_OID));

  MBEDTLS2_ASN1_CHK_ADD(len,
                               mbedtls2_asn1_write_len(p, start, len));
  MBEDTLS2_ASN1_CHK_ADD(
      len, mbedtls2_asn1_write_tag(p, start,
                                          MBEDTLS2_ASN1_CONSTRUCTED |
                                              MBEDTLS2_ASN1_SEQUENCE));

  return ((int)len);
}

/*
 * Extension  ::=  SEQUENCE  {
 *     extnID      OBJECT IDENTIFIER,
 *     critical    BOOLEAN DEFAULT FALSE,
 *     extnValue   OCTET STRING
 *                 -- contains the DER encoding of an ASN.1 value
 *                 -- corresponding to the extension type identified
 *                 -- by extnID
 *     }
 */
int mbedtls2_x509_write_extensions(
    unsigned char **p, unsigned char *start,
    mbedtls2_asn1_named_data *first) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  size_t len = 0;
  mbedtls2_asn1_named_data *cur_ext = first;

  while (cur_ext != NULL) {
    MBEDTLS2_ASN1_CHK_ADD(len, x509_write_extension(p, start, cur_ext));
    cur_ext = cur_ext->next;
  }

  return ((int)len);
}

#endif /* MBEDTLS2_X509_CREATE_C */

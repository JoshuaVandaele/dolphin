/**
 * \file ssl_ticket.h
 *
 * \brief TLS server ticket callbacks implementation
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
#ifndef MBEDTLS2_SSL_TICKET_H
#define MBEDTLS2_SSL_TICKET_H

#if !defined(MBEDTLS2_CONFIG_FILE)
#include "mbedtls2/config.h"
#else
#include MBEDTLS2_CONFIG_FILE
#endif

/*
 * This implementation of the session ticket callbacks includes key
 * management, rotating the keys periodically in order to preserve forward
 * secrecy, when MBEDTLS2_HAVE_TIME is defined.
 */

#include "mbedtls2/cipher.h"
#include "mbedtls2/ssl.h"

#if defined(MBEDTLS2_THREADING_C)
#include "mbedtls2/threading.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

/**
 * \brief   Information for session ticket protection
 */
typedef struct mbedtls2_ssl_ticket_key {
  unsigned char name[4];    /*!< random key identifier              */
  uint32_t generation_time; /*!< key generation timestamp (seconds) */
  mbedtls2_cipher_context_t ctx; /*!< context for auth enc/decryption */
} mbedtls2_ssl_ticket_key;

/**
 * \brief   Context for session ticket handling functions
 */
typedef struct mbedtls2_ssl_ticket_context {
  mbedtls2_ssl_ticket_key keys[2]; /*!< ticket protection keys */
  unsigned char active; /*!< index of the currently active key  */

  uint32_t ticket_lifetime; /*!< lifetime of tickets in seconds     */

  /** Callback for getting (pseudo-)random numbers                        */
  int (*f_rng)(void *, unsigned char *, size_t);
  void *p_rng; /*!< context for the RNG function       */

#if defined(MBEDTLS2_THREADING_C)
  mbedtls2_threading_mutex_t mutex;
#endif
} mbedtls2_ssl_ticket_context;

/**
 * \brief           Initialize a ticket context.
 *                  (Just make it ready for mbedtls2_ssl_ticket_setup()
 *                  or mbedtls2_ssl_ticket_free().)
 *
 * \param ctx       Context to be initialized
 */
void mbedtls2_ssl_ticket_init(mbedtls2_ssl_ticket_context *ctx);

/**
 * \brief           Prepare context to be actually used
 *
 * \param ctx       Context to be set up
 * \param f_rng     RNG callback function
 * \param p_rng     RNG callback context
 * \param cipher    AEAD cipher to use for ticket protection.
 *                  Recommended value: MBEDTLS2_CIPHER_AES_256_GCM.
 * \param lifetime  Tickets lifetime in seconds
 *                  Recommended value: 86400 (one day).
 *
 * \note            It is highly recommended to select a cipher that is at
 *                  least as strong as the strongest ciphersuite
 *                  supported. Usually that means a 256-bit key.
 *
 * \note            The lifetime of the keys is twice the lifetime of tickets.
 *                  It is recommended to pick a reasonnable lifetime so as not
 *                  to negate the benefits of forward secrecy.
 *
 * \return          0 if successful,
 *                  or a specific MBEDTLS2_ERR_XXX error code
 */
int mbedtls2_ssl_ticket_setup(
    mbedtls2_ssl_ticket_context *ctx,
    int (*f_rng)(void *, unsigned char *, size_t), void *p_rng,
    mbedtls2_cipher_type_t cipher, uint32_t lifetime);

/**
 * \brief           Implementation of the ticket write callback
 *
 * \note            See \c mbedtls2_ssl_ticket_write_t for description
 */
mbedtls2_ssl_ticket_write_t mbedtls2_ssl_ticket_write;

/**
 * \brief           Implementation of the ticket parse callback
 *
 * \note            See \c mbedtls2_ssl_ticket_parse_t for description
 */
mbedtls2_ssl_ticket_parse_t mbedtls2_ssl_ticket_parse;

/**
 * \brief           Free a context's content and zeroize it.
 *
 * \param ctx       Context to be cleaned up
 */
void mbedtls2_ssl_ticket_free(mbedtls2_ssl_ticket_context *ctx);

#ifdef __cplusplus
}
#endif

#endif /* ssl_ticket.h */

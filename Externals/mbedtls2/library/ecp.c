/*
 *  Elliptic curves over GF(p): generic functions
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
 *
 * SEC1 http://www.secg.org/index.php?action=secg,docs_secg
 * GECC = Guide to Elliptic Curve Cryptography - Hankerson, Menezes, Vanstone
 * FIPS 186-3 http://csrc.nist.gov/publications/fips/fips186-3/fips_186-3.pdf
 * RFC 4492 for the related TLS structures and constants
 * RFC 7748 for the Curve448 and Curve25519 curve definitions
 *
 * [Curve25519] http://cr.yp.to/ecdh/curve25519-20060209.pdf
 *
 * [2] CORON, Jean-S'ebastien. Resistance against differential power analysis
 *     for elliptic curve cryptosystems. In : Cryptographic Hardware and
 *     Embedded Systems. Springer Berlin Heidelberg, 1999. p. 292-302.
 *     <http://link.springer.com/chapter/10.1007/3-540-48059-5_25>
 *
 * [3] HEDABOU, Mustapha, PINEL, Pierre, et B'EN'ETEAU, Lucien. A comb method to
 *     render ECC resistant against Side Channel Attacks. IACR Cryptology
 *     ePrint Archive, 2004, vol. 2004, p. 342.
 *     <http://eprint.iacr.org/2004/342.pdf>
 */

#include "common.h"

/**
 * \brief Function level alternative implementation.
 *
 * The MBEDTLS2_ECP_INTERNAL_ALT macro enables alternative
 * implementations to replace certain functions in this module. The alternative
 * implementations are typically hardware accelerators and need to activate the
 * hardware before the computation starts and deactivate it after it finishes.
 * The mbedtls2_internal_ecp_init() and
 * mbedtls2_internal_ecp_free() functions serve this purpose.
 *
 * To preserve the correct functionality the following conditions must hold:
 *
 * - The alternative implementation must be activated by
 *   mbedtls2_internal_ecp_init() before any of the replaceable functions
 * is called.
 * - mbedtls2_internal_ecp_free() must \b only be called when the
 * alternative implementation is activated.
 * - mbedtls2_internal_ecp_init() must \b not be called when the
 * alternative implementation is activated.
 * - Public functions must not return while the alternative implementation is
 *   activated.
 * - Replaceable functions are guarded by \c MBEDTLS2_ECP_XXX_ALT macros
 * and before calling them an \code if(
 * mbedtls2_internal_ecp_grp_capable( grp ) )
 *   \endcode ensures that the alternative implementation supports the current
 *   group.
 */
#if defined(MBEDTLS2_ECP_INTERNAL_ALT)
#endif

#if defined(MBEDTLS2_ECP_C)

#include "mbedtls2/bn_mul.h"
#include "mbedtls2/ecp.h"
#include "mbedtls2/error.h"
#include "mbedtls2/platform_util.h"
#include "mbedtls2/threading.h"

#include "ecp_invasive.h"

#include <string.h>

#if !defined(MBEDTLS2_ECP_ALT)

/* Parameter validation macros based on platform_util.h */
#define ECP_VALIDATE_RET(cond)                                                 \
  MBEDTLS2_INTERNAL_VALIDATE_RET(                                       \
      cond, MBEDTLS2_ERR_ECP_BAD_INPUT_DATA)
#define ECP_VALIDATE(cond) MBEDTLS2_INTERNAL_VALIDATE(cond)

#if defined(MBEDTLS2_PLATFORM_C)
#include "mbedtls2/platform.h"
#else
#include <stdio.h>
#include <stdlib.h>
#define mbedtls2_printf printf
#define mbedtls2_calloc calloc
#define mbedtls2_free free
#endif

#include "mbedtls2/ecp_internal.h"

#if !defined(MBEDTLS2_ECP_NO_INTERNAL_RNG)
#if defined(MBEDTLS2_HMAC_DRBG_C)
#include "mbedtls2/hmac_drbg.h"
#elif defined(MBEDTLS2_CTR_DRBG_C)
#include "mbedtls2/ctr_drbg.h"
#else
#error                                                                         \
    "Invalid configuration detected. Include check_config.h to ensure that the configuration is valid."
#endif
#endif /* MBEDTLS2_ECP_NO_INTERNAL_RNG */

#if (defined(__ARMCC_VERSION) || defined(_MSC_VER)) && !defined(inline) &&     \
    !defined(__cplusplus)
#define inline __inline
#endif

#if defined(MBEDTLS2_SELF_TEST)
/*
 * Counts of point addition and doubling, and field multiplications.
 * Used to test resistance of point multiplication to simple timing attacks.
 */
static unsigned long add_count, dbl_count, mul_count;
#endif

#if !defined(MBEDTLS2_ECP_NO_INTERNAL_RNG)
/*
 * Currently ecp_mul() takes a RNG function as an argument, used for
 * side-channel protection, but it can be NULL. The initial reasoning was
 * that people will pass non-NULL RNG when they care about side-channels, but
 * unfortunately we have some APIs that call ecp_mul() with a NULL RNG, with
 * no opportunity for the user to do anything about it.
 *
 * The obvious strategies for addressing that include:
 * - change those APIs so that they take RNG arguments;
 * - require a global RNG to be available to all crypto modules.
 *
 * Unfortunately those would break compatibility. So what we do instead is
 * have our own internal DRBG instance, seeded from the secret scalar.
 *
 * The following is a light-weight abstraction layer for doing that with
 * HMAC_DRBG (first choice) or CTR_DRBG.
 */

#if defined(MBEDTLS2_HMAC_DRBG_C)

/* DRBG context type */
typedef mbedtls2_hmac_drbg_context ecp_drbg_context;

/* DRBG context init */
static inline void ecp_drbg_init(ecp_drbg_context *ctx) {
  mbedtls2_hmac_drbg_init(ctx);
}

/* DRBG context free */
static inline void ecp_drbg_free(ecp_drbg_context *ctx) {
  mbedtls2_hmac_drbg_free(ctx);
}

/* DRBG function */
static inline int ecp_drbg_random(void *p_rng, unsigned char *output,
                                  size_t output_len) {
  return (mbedtls2_hmac_drbg_random(p_rng, output, output_len));
}

/* DRBG context seeding */
static int ecp_drbg_seed(ecp_drbg_context *ctx,
                         const mbedtls2_mpi *secret, size_t secret_len) {
  int ret;
  unsigned char secret_bytes[MBEDTLS2_ECP_MAX_BYTES];
  /* The list starts with strong hashes */
  const mbedtls2_md_type_t md_type = mbedtls2_md_list()[0];
  const mbedtls2_md_info_t *md_info =
      mbedtls2_md_info_from_type(md_type);

  if (secret_len > MBEDTLS2_ECP_MAX_BYTES) {
    ret = MBEDTLS2_ERR_ECP_RANDOM_FAILED;
    goto cleanup;
  }

  MBEDTLS2_MPI_CHK(
      mbedtls2_mpi_write_binary(secret, secret_bytes, secret_len));

  ret = mbedtls2_hmac_drbg_seed_buf(ctx, md_info, secret_bytes,
                                           secret_len);

cleanup:
  mbedtls2_platform_zeroize(secret_bytes, secret_len);

  return (ret);
}

#elif defined(MBEDTLS2_CTR_DRBG_C)

/* DRBG context type */
typedef mbedtls2_ctr_drbg_context ecp_drbg_context;

/* DRBG context init */
static inline void ecp_drbg_init(ecp_drbg_context *ctx) {
  mbedtls2_ctr_drbg_init(ctx);
}

/* DRBG context free */
static inline void ecp_drbg_free(ecp_drbg_context *ctx) {
  mbedtls2_ctr_drbg_free(ctx);
}

/* DRBG function */
static inline int ecp_drbg_random(void *p_rng, unsigned char *output,
                                  size_t output_len) {
  return (mbedtls2_ctr_drbg_random(p_rng, output, output_len));
}

/*
 * Since CTR_DRBG doesn't have a seed_buf() function the way HMAC_DRBG does,
 * we need to pass an entropy function when seeding. So we use a dummy
 * function for that, and pass the actual entropy as customisation string.
 * (During seeding of CTR_DRBG the entropy input and customisation string are
 * concatenated before being used to update the secret state.)
 */
static int ecp_ctr_drbg_null_entropy(void *ctx, unsigned char *out,
                                     size_t len) {
  (void)ctx;
  memset(out, 0, len);
  return (0);
}

/* DRBG context seeding */
static int ecp_drbg_seed(ecp_drbg_context *ctx,
                         const mbedtls2_mpi *secret, size_t secret_len) {
  int ret;
  unsigned char secret_bytes[MBEDTLS2_ECP_MAX_BYTES];

  if (secret_len > MBEDTLS2_ECP_MAX_BYTES) {
    ret = MBEDTLS2_ERR_ECP_RANDOM_FAILED;
    goto cleanup;
  }

  MBEDTLS2_MPI_CHK(
      mbedtls2_mpi_write_binary(secret, secret_bytes, secret_len));

  ret = mbedtls2_ctr_drbg_seed(ctx, ecp_ctr_drbg_null_entropy, NULL,
                                      secret_bytes, secret_len);

cleanup:
  mbedtls2_platform_zeroize(secret_bytes, secret_len);

  return (ret);
}

#else
#error                                                                         \
    "Invalid configuration detected. Include check_config.h to ensure that the configuration is valid."
#endif /* DRBG modules */
#endif /* MBEDTLS2_ECP_NO_INTERNAL_RNG */

#if defined(MBEDTLS2_ECP_RESTARTABLE)
/*
 * Maximum number of "basic operations" to be done in a row.
 *
 * Default value 0 means that ECC operations will not yield.
 * Note that regardless of the value of ecp_max_ops, always at
 * least one step is performed before yielding.
 *
 * Setting ecp_max_ops=1 can be suitable for testing purposes
 * as it will interrupt computation at all possible points.
 */
static unsigned ecp_max_ops = 0;

/*
 * Set ecp_max_ops
 */
void mbedtls2_ecp_set_max_ops(unsigned max_ops) {
  ecp_max_ops = max_ops;
}

/*
 * Check if restart is enabled
 */
int mbedtls2_ecp_restart_is_enabled(void) { return (ecp_max_ops != 0); }

/*
 * Restart sub-context for ecp_mul_comb()
 */
struct mbedtls2_ecp_restart_mul {
  mbedtls2_ecp_point R; /* current intermediate result */
  size_t i; /* current index in various loops, 0 outside    */
  mbedtls2_ecp_point *T; /* table for precomputed points */
  unsigned char T_size;    /* number of points in table T                  */
  enum {                   /* what were we doing last time we returned?    */
         ecp_rsm_init = 0, /* nothing so far, dummy initial state      */
         ecp_rsm_pre_dbl,  /* precompute 2^n multiples                 */
         ecp_rsm_pre_norm_dbl, /* normalize precomputed 2^n multiples      */
         ecp_rsm_pre_add,      /* precompute remaining points by adding    */
         ecp_rsm_pre_norm_add, /* normalize all precomputed points         */
         ecp_rsm_comb_core,    /* ecp_mul_comb_core()                      */
         ecp_rsm_final_norm,   /* do the final normalization               */
  } state;
#if !defined(MBEDTLS2_ECP_NO_INTERNAL_RNG)
  ecp_drbg_context drbg_ctx;
  unsigned char drbg_seeded;
#endif
};

/*
 * Init restart_mul sub-context
 */
static void ecp_restart_rsm_init(mbedtls2_ecp_restart_mul_ctx *ctx) {
  mbedtls2_ecp_point_init(&ctx->R);
  ctx->i = 0;
  ctx->T = NULL;
  ctx->T_size = 0;
  ctx->state = ecp_rsm_init;
#if !defined(MBEDTLS2_ECP_NO_INTERNAL_RNG)
  ecp_drbg_init(&ctx->drbg_ctx);
  ctx->drbg_seeded = 0;
#endif
}

/*
 * Free the components of a restart_mul sub-context
 */
static void ecp_restart_rsm_free(mbedtls2_ecp_restart_mul_ctx *ctx) {
  unsigned char i;

  if (ctx == NULL)
    return;

  mbedtls2_ecp_point_free(&ctx->R);

  if (ctx->T != NULL) {
    for (i = 0; i < ctx->T_size; i++)
      mbedtls2_ecp_point_free(ctx->T + i);
    mbedtls2_free(ctx->T);
  }

#if !defined(MBEDTLS2_ECP_NO_INTERNAL_RNG)
  ecp_drbg_free(&ctx->drbg_ctx);
#endif

  ecp_restart_rsm_init(ctx);
}

/*
 * Restart context for ecp_muladd()
 */
struct mbedtls2_ecp_restart_muladd {
  mbedtls2_ecp_point mP; /* mP value                             */
  mbedtls2_ecp_point R;  /* R intermediate result                */
  enum {                        /* what should we do next?              */
         ecp_rsma_mul1 = 0,     /* first multiplication                 */
         ecp_rsma_mul2,         /* second multiplication                */
         ecp_rsma_add,          /* addition                             */
         ecp_rsma_norm,         /* normalization                        */
  } state;
};

/*
 * Init restart_muladd sub-context
 */
static void ecp_restart_ma_init(mbedtls2_ecp_restart_muladd_ctx *ctx) {
  mbedtls2_ecp_point_init(&ctx->mP);
  mbedtls2_ecp_point_init(&ctx->R);
  ctx->state = ecp_rsma_mul1;
}

/*
 * Free the components of a restart_muladd sub-context
 */
static void ecp_restart_ma_free(mbedtls2_ecp_restart_muladd_ctx *ctx) {
  if (ctx == NULL)
    return;

  mbedtls2_ecp_point_free(&ctx->mP);
  mbedtls2_ecp_point_free(&ctx->R);

  ecp_restart_ma_init(ctx);
}

/*
 * Initialize a restart context
 */
void mbedtls2_ecp_restart_init(mbedtls2_ecp_restart_ctx *ctx) {
  ECP_VALIDATE(ctx != NULL);
  ctx->ops_done = 0;
  ctx->depth = 0;
  ctx->rsm = NULL;
  ctx->ma = NULL;
}

/*
 * Free the components of a restart context
 */
void mbedtls2_ecp_restart_free(mbedtls2_ecp_restart_ctx *ctx) {
  if (ctx == NULL)
    return;

  ecp_restart_rsm_free(ctx->rsm);
  mbedtls2_free(ctx->rsm);

  ecp_restart_ma_free(ctx->ma);
  mbedtls2_free(ctx->ma);

  mbedtls2_ecp_restart_init(ctx);
}

/*
 * Check if we can do the next step
 */
int mbedtls2_ecp_check_budget(const mbedtls2_ecp_group *grp,
                                     mbedtls2_ecp_restart_ctx *rs_ctx,
                                     unsigned ops) {
  ECP_VALIDATE_RET(grp != NULL);

  if (rs_ctx != NULL && ecp_max_ops != 0) {
    /* scale depending on curve size: the chosen reference is 256-bit,
     * and multiplication is quadratic. Round to the closest integer. */
    if (grp->pbits >= 512)
      ops *= 4;
    else if (grp->pbits >= 384)
      ops *= 2;

    /* Avoid infinite loops: always allow first step.
     * Because of that, however, it's not generally true
     * that ops_done <= ecp_max_ops, so the check
     * ops_done > ecp_max_ops below is mandatory. */
    if ((rs_ctx->ops_done != 0) && (rs_ctx->ops_done > ecp_max_ops ||
                                    ops > ecp_max_ops - rs_ctx->ops_done)) {
      return (MBEDTLS2_ERR_ECP_IN_PROGRESS);
    }

    /* update running count */
    rs_ctx->ops_done += ops;
  }

  return (0);
}

/* Call this when entering a function that needs its own sub-context */
#define ECP_RS_ENTER(SUB)                                                      \
  do {                                                                         \
    /* reset ops count for this call if top-level */                           \
    if (rs_ctx != NULL && rs_ctx->depth++ == 0)                                \
      rs_ctx->ops_done = 0;                                                    \
                                                                               \
    /* set up our own sub-context if needed */                                 \
    if (mbedtls2_ecp_restart_is_enabled() && rs_ctx != NULL &&          \
        rs_ctx->SUB == NULL) {                                                 \
      rs_ctx->SUB = mbedtls2_calloc(1, sizeof(*rs_ctx->SUB));           \
      if (rs_ctx->SUB == NULL)                                                 \
        return (MBEDTLS2_ERR_ECP_ALLOC_FAILED);                         \
                                                                               \
      ecp_restart_##SUB##_init(rs_ctx->SUB);                                   \
    }                                                                          \
  } while (0)

/* Call this when leaving a function that needs its own sub-context */
#define ECP_RS_LEAVE(SUB)                                                      \
  do {                                                                         \
    /* clear our sub-context when not in progress (done or error) */           \
    if (rs_ctx != NULL && rs_ctx->SUB != NULL &&                               \
        ret != MBEDTLS2_ERR_ECP_IN_PROGRESS) {                          \
      ecp_restart_##SUB##_free(rs_ctx->SUB);                                   \
      mbedtls2_free(rs_ctx->SUB);                                       \
      rs_ctx->SUB = NULL;                                                      \
    }                                                                          \
                                                                               \
    if (rs_ctx != NULL)                                                        \
      rs_ctx->depth--;                                                         \
  } while (0)

#else /* MBEDTLS2_ECP_RESTARTABLE */

#define ECP_RS_ENTER(sub) (void)rs_ctx;
#define ECP_RS_LEAVE(sub) (void)rs_ctx;

#endif /* MBEDTLS2_ECP_RESTARTABLE */

/*
 * List of supported curves:
 *  - internal ID
 *  - TLS NamedCurve ID (RFC 4492 sec. 5.1.1, RFC 7071 sec. 2, RFC 8446
 * sec. 4.2.7)
 *  - size in bits
 *  - readable name
 *
 * Curves are listed in order: largest curves first, and for a given size,
 * fastest curves first. This provides the default order for the SSL module.
 *
 * Reminder: update profiles in x509_crt.c when adding a new curves!
 */
static const mbedtls2_ecp_curve_info ecp_supported_curves[] = {
#if defined(MBEDTLS2_ECP_DP_SECP521R1_ENABLED)
    {MBEDTLS2_ECP_DP_SECP521R1, 25, 521, "secp521r1"},
#endif
#if defined(MBEDTLS2_ECP_DP_BP512R1_ENABLED)
    {MBEDTLS2_ECP_DP_BP512R1, 28, 512, "brainpoolP512r1"},
#endif
#if defined(MBEDTLS2_ECP_DP_SECP384R1_ENABLED)
    {MBEDTLS2_ECP_DP_SECP384R1, 24, 384, "secp384r1"},
#endif
#if defined(MBEDTLS2_ECP_DP_BP384R1_ENABLED)
    {MBEDTLS2_ECP_DP_BP384R1, 27, 384, "brainpoolP384r1"},
#endif
#if defined(MBEDTLS2_ECP_DP_SECP256R1_ENABLED)
    {MBEDTLS2_ECP_DP_SECP256R1, 23, 256, "secp256r1"},
#endif
#if defined(MBEDTLS2_ECP_DP_SECP256K1_ENABLED)
    {MBEDTLS2_ECP_DP_SECP256K1, 22, 256, "secp256k1"},
#endif
#if defined(MBEDTLS2_ECP_DP_BP256R1_ENABLED)
    {MBEDTLS2_ECP_DP_BP256R1, 26, 256, "brainpoolP256r1"},
#endif
#if defined(MBEDTLS2_ECP_DP_SECP224R1_ENABLED)
    {MBEDTLS2_ECP_DP_SECP224R1, 21, 224, "secp224r1"},
#endif
#if defined(MBEDTLS2_ECP_DP_SECP224K1_ENABLED)
    {MBEDTLS2_ECP_DP_SECP224K1, 20, 224, "secp224k1"},
#endif
#if defined(MBEDTLS2_ECP_DP_SECP192R1_ENABLED)
    {MBEDTLS2_ECP_DP_SECP192R1, 19, 192, "secp192r1"},
#endif
#if defined(MBEDTLS2_ECP_DP_SECP192K1_ENABLED)
    {MBEDTLS2_ECP_DP_SECP192K1, 18, 192, "secp192k1"},
#endif
#if defined(MBEDTLS2_ECP_DP_CURVE25519_ENABLED)
    {MBEDTLS2_ECP_DP_CURVE25519, 29, 256, "x25519"},
#endif
#if defined(MBEDTLS2_ECP_DP_CURVE448_ENABLED)
    {MBEDTLS2_ECP_DP_CURVE448, 30, 448, "x448"},
#endif
    {MBEDTLS2_ECP_DP_NONE, 0, 0, NULL},
};

#define ECP_NB_CURVES                                                          \
  sizeof(ecp_supported_curves) / sizeof(ecp_supported_curves[0])

static mbedtls2_ecp_group_id ecp_supported_grp_id[ECP_NB_CURVES];

/*
 * List of supported curves and associated info
 */
const mbedtls2_ecp_curve_info *mbedtls2_ecp_curve_list(void) {
  return (ecp_supported_curves);
}

/*
 * List of supported curves, group ID only
 */
const mbedtls2_ecp_group_id *mbedtls2_ecp_grp_id_list(void) {
  static int init_done = 0;

  if (!init_done) {
    size_t i = 0;
    const mbedtls2_ecp_curve_info *curve_info;

    for (curve_info = mbedtls2_ecp_curve_list();
         curve_info->grp_id != MBEDTLS2_ECP_DP_NONE; curve_info++) {
      ecp_supported_grp_id[i++] = curve_info->grp_id;
    }
    ecp_supported_grp_id[i] = MBEDTLS2_ECP_DP_NONE;

    init_done = 1;
  }

  return (ecp_supported_grp_id);
}

/*
 * Get the curve info for the internal identifier
 */
const mbedtls2_ecp_curve_info *
mbedtls2_ecp_curve_info_from_grp_id(
    mbedtls2_ecp_group_id grp_id) {
  const mbedtls2_ecp_curve_info *curve_info;

  for (curve_info = mbedtls2_ecp_curve_list();
       curve_info->grp_id != MBEDTLS2_ECP_DP_NONE; curve_info++) {
    if (curve_info->grp_id == grp_id)
      return (curve_info);
  }

  return (NULL);
}

/*
 * Get the curve info from the TLS identifier
 */
const mbedtls2_ecp_curve_info *
mbedtls2_ecp_curve_info_from_tls_id(uint16_t tls_id) {
  const mbedtls2_ecp_curve_info *curve_info;

  for (curve_info = mbedtls2_ecp_curve_list();
       curve_info->grp_id != MBEDTLS2_ECP_DP_NONE; curve_info++) {
    if (curve_info->tls_id == tls_id)
      return (curve_info);
  }

  return (NULL);
}

/*
 * Get the curve info from the name
 */
const mbedtls2_ecp_curve_info *
mbedtls2_ecp_curve_info_from_name(const char *name) {
  const mbedtls2_ecp_curve_info *curve_info;

  if (name == NULL)
    return (NULL);

  for (curve_info = mbedtls2_ecp_curve_list();
       curve_info->grp_id != MBEDTLS2_ECP_DP_NONE; curve_info++) {
    if (strcmp(curve_info->name, name) == 0)
      return (curve_info);
  }

  return (NULL);
}

/*
 * Get the type of a curve
 */
mbedtls2_ecp_curve_type
mbedtls2_ecp_get_type(const mbedtls2_ecp_group *grp) {
  if (grp->G.X.p == NULL)
    return (MBEDTLS2_ECP_TYPE_NONE);

  if (grp->G.Y.p == NULL)
    return (MBEDTLS2_ECP_TYPE_MONTGOMERY);
  else
    return (MBEDTLS2_ECP_TYPE_SHORT_WEIERSTRASS);
}

/*
 * Initialize (the components of) a point
 */
void mbedtls2_ecp_point_init(mbedtls2_ecp_point *pt) {
  ECP_VALIDATE(pt != NULL);

  mbedtls2_mpi_init(&pt->X);
  mbedtls2_mpi_init(&pt->Y);
  mbedtls2_mpi_init(&pt->Z);
}

/*
 * Initialize (the components of) a group
 */
void mbedtls2_ecp_group_init(mbedtls2_ecp_group *grp) {
  ECP_VALIDATE(grp != NULL);

  grp->id = MBEDTLS2_ECP_DP_NONE;
  mbedtls2_mpi_init(&grp->P);
  mbedtls2_mpi_init(&grp->A);
  mbedtls2_mpi_init(&grp->B);
  mbedtls2_ecp_point_init(&grp->G);
  mbedtls2_mpi_init(&grp->N);
  grp->pbits = 0;
  grp->nbits = 0;
  grp->h = 0;
  grp->modp = NULL;
  grp->t_pre = NULL;
  grp->t_post = NULL;
  grp->t_data = NULL;
  grp->T = NULL;
  grp->T_size = 0;
}

/*
 * Initialize (the components of) a key pair
 */
void mbedtls2_ecp_keypair_init(mbedtls2_ecp_keypair *key) {
  ECP_VALIDATE(key != NULL);

  mbedtls2_ecp_group_init(&key->grp);
  mbedtls2_mpi_init(&key->d);
  mbedtls2_ecp_point_init(&key->Q);
}

/*
 * Unallocate (the components of) a point
 */
void mbedtls2_ecp_point_free(mbedtls2_ecp_point *pt) {
  if (pt == NULL)
    return;

  mbedtls2_mpi_free(&(pt->X));
  mbedtls2_mpi_free(&(pt->Y));
  mbedtls2_mpi_free(&(pt->Z));
}

/*
 * Unallocate (the components of) a group
 */
void mbedtls2_ecp_group_free(mbedtls2_ecp_group *grp) {
  size_t i;

  if (grp == NULL)
    return;

  if (grp->h != 1) {
    mbedtls2_mpi_free(&grp->P);
    mbedtls2_mpi_free(&grp->A);
    mbedtls2_mpi_free(&grp->B);
    mbedtls2_ecp_point_free(&grp->G);
    mbedtls2_mpi_free(&grp->N);
  }

  if (grp->T != NULL) {
    for (i = 0; i < grp->T_size; i++)
      mbedtls2_ecp_point_free(&grp->T[i]);
    mbedtls2_free(grp->T);
  }

  mbedtls2_platform_zeroize(grp, sizeof(mbedtls2_ecp_group));
}

/*
 * Unallocate (the components of) a key pair
 */
void mbedtls2_ecp_keypair_free(mbedtls2_ecp_keypair *key) {
  if (key == NULL)
    return;

  mbedtls2_ecp_group_free(&key->grp);
  mbedtls2_mpi_free(&key->d);
  mbedtls2_ecp_point_free(&key->Q);
}

/*
 * Copy the contents of a point
 */
int mbedtls2_ecp_copy(mbedtls2_ecp_point *P,
                             const mbedtls2_ecp_point *Q) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  ECP_VALIDATE_RET(P != NULL);
  ECP_VALIDATE_RET(Q != NULL);

  MBEDTLS2_MPI_CHK(mbedtls2_mpi_copy(&P->X, &Q->X));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_copy(&P->Y, &Q->Y));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_copy(&P->Z, &Q->Z));

cleanup:
  return (ret);
}

/*
 * Copy the contents of a group object
 */
int mbedtls2_ecp_group_copy(mbedtls2_ecp_group *dst,
                                   const mbedtls2_ecp_group *src) {
  ECP_VALIDATE_RET(dst != NULL);
  ECP_VALIDATE_RET(src != NULL);

  return (mbedtls2_ecp_group_load(dst, src->id));
}

/*
 * Set point to zero
 */
int mbedtls2_ecp_set_zero(mbedtls2_ecp_point *pt) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  ECP_VALIDATE_RET(pt != NULL);

  MBEDTLS2_MPI_CHK(mbedtls2_mpi_lset(&pt->X, 1));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_lset(&pt->Y, 1));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_lset(&pt->Z, 0));

cleanup:
  return (ret);
}

/*
 * Tell if a point is zero
 */
int mbedtls2_ecp_is_zero(mbedtls2_ecp_point *pt) {
  ECP_VALIDATE_RET(pt != NULL);

  return (mbedtls2_mpi_cmp_int(&pt->Z, 0) == 0);
}

/*
 * Compare two points lazily
 */
int mbedtls2_ecp_point_cmp(const mbedtls2_ecp_point *P,
                                  const mbedtls2_ecp_point *Q) {
  ECP_VALIDATE_RET(P != NULL);
  ECP_VALIDATE_RET(Q != NULL);

  if (mbedtls2_mpi_cmp_mpi(&P->X, &Q->X) == 0 &&
      mbedtls2_mpi_cmp_mpi(&P->Y, &Q->Y) == 0 &&
      mbedtls2_mpi_cmp_mpi(&P->Z, &Q->Z) == 0) {
    return (0);
  }

  return (MBEDTLS2_ERR_ECP_BAD_INPUT_DATA);
}

/*
 * Import a non-zero point from ASCII strings
 */
int mbedtls2_ecp_point_read_string(mbedtls2_ecp_point *P,
                                          int radix, const char *x,
                                          const char *y) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  ECP_VALIDATE_RET(P != NULL);
  ECP_VALIDATE_RET(x != NULL);
  ECP_VALIDATE_RET(y != NULL);

  MBEDTLS2_MPI_CHK(mbedtls2_mpi_read_string(&P->X, radix, x));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_read_string(&P->Y, radix, y));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_lset(&P->Z, 1));

cleanup:
  return (ret);
}

/*
 * Export a point into unsigned binary data (SEC1 2.3.3 and RFC7748)
 */
int mbedtls2_ecp_point_write_binary(const mbedtls2_ecp_group *grp,
                                           const mbedtls2_ecp_point *P,
                                           int format, size_t *olen,
                                           unsigned char *buf, size_t buflen) {
  int ret = MBEDTLS2_ERR_ECP_FEATURE_UNAVAILABLE;
  size_t plen;
  ECP_VALIDATE_RET(grp != NULL);
  ECP_VALIDATE_RET(P != NULL);
  ECP_VALIDATE_RET(olen != NULL);
  ECP_VALIDATE_RET(buf != NULL);
  ECP_VALIDATE_RET(format == MBEDTLS2_ECP_PF_UNCOMPRESSED ||
                   format == MBEDTLS2_ECP_PF_COMPRESSED);

  plen = mbedtls2_mpi_size(&grp->P);

#if defined(MBEDTLS2_ECP_MONTGOMERY_ENABLED)
  (void)format; /* Montgomery curves always use the same point format */
  if (mbedtls2_ecp_get_type(grp) ==
      MBEDTLS2_ECP_TYPE_MONTGOMERY) {
    *olen = plen;
    if (buflen < *olen)
      return (MBEDTLS2_ERR_ECP_BUFFER_TOO_SMALL);

    MBEDTLS2_MPI_CHK(
        mbedtls2_mpi_write_binary_le(&P->X, buf, plen));
  }
#endif
#if defined(MBEDTLS2_ECP_SHORT_WEIERSTRASS_ENABLED)
  if (mbedtls2_ecp_get_type(grp) ==
      MBEDTLS2_ECP_TYPE_SHORT_WEIERSTRASS) {
    /*
     * Common case: P == 0
     */
    if (mbedtls2_mpi_cmp_int(&P->Z, 0) == 0) {
      if (buflen < 1)
        return (MBEDTLS2_ERR_ECP_BUFFER_TOO_SMALL);

      buf[0] = 0x00;
      *olen = 1;

      return (0);
    }

    if (format == MBEDTLS2_ECP_PF_UNCOMPRESSED) {
      *olen = 2 * plen + 1;

      if (buflen < *olen)
        return (MBEDTLS2_ERR_ECP_BUFFER_TOO_SMALL);

      buf[0] = 0x04;
      MBEDTLS2_MPI_CHK(
          mbedtls2_mpi_write_binary(&P->X, buf + 1, plen));
      MBEDTLS2_MPI_CHK(
          mbedtls2_mpi_write_binary(&P->Y, buf + 1 + plen, plen));
    } else if (format == MBEDTLS2_ECP_PF_COMPRESSED) {
      *olen = plen + 1;

      if (buflen < *olen)
        return (MBEDTLS2_ERR_ECP_BUFFER_TOO_SMALL);

      buf[0] = 0x02 + mbedtls2_mpi_get_bit(&P->Y, 0);
      MBEDTLS2_MPI_CHK(
          mbedtls2_mpi_write_binary(&P->X, buf + 1, plen));
    }
  }
#endif

cleanup:
  return (ret);
}

/*
 * Import a point from unsigned binary data (SEC1 2.3.4 and RFC7748)
 */
int mbedtls2_ecp_point_read_binary(const mbedtls2_ecp_group *grp,
                                          mbedtls2_ecp_point *pt,
                                          const unsigned char *buf,
                                          size_t ilen) {
  int ret = MBEDTLS2_ERR_ECP_FEATURE_UNAVAILABLE;
  size_t plen;
  ECP_VALIDATE_RET(grp != NULL);
  ECP_VALIDATE_RET(pt != NULL);
  ECP_VALIDATE_RET(buf != NULL);

  if (ilen < 1)
    return (MBEDTLS2_ERR_ECP_BAD_INPUT_DATA);

  plen = mbedtls2_mpi_size(&grp->P);

#if defined(MBEDTLS2_ECP_MONTGOMERY_ENABLED)
  if (mbedtls2_ecp_get_type(grp) ==
      MBEDTLS2_ECP_TYPE_MONTGOMERY) {
    if (plen != ilen)
      return (MBEDTLS2_ERR_ECP_BAD_INPUT_DATA);

    MBEDTLS2_MPI_CHK(
        mbedtls2_mpi_read_binary_le(&pt->X, buf, plen));
    mbedtls2_mpi_free(&pt->Y);

    if (grp->id == MBEDTLS2_ECP_DP_CURVE25519)
      /* Set most significant bit to 0 as prescribed in RFC7748 §5 */
      MBEDTLS2_MPI_CHK(
          mbedtls2_mpi_set_bit(&pt->X, plen * 8 - 1, 0));

    MBEDTLS2_MPI_CHK(mbedtls2_mpi_lset(&pt->Z, 1));
  }
#endif
#if defined(MBEDTLS2_ECP_SHORT_WEIERSTRASS_ENABLED)
  if (mbedtls2_ecp_get_type(grp) ==
      MBEDTLS2_ECP_TYPE_SHORT_WEIERSTRASS) {
    if (buf[0] == 0x00) {
      if (ilen == 1)
        return (mbedtls2_ecp_set_zero(pt));
      else
        return (MBEDTLS2_ERR_ECP_BAD_INPUT_DATA);
    }

    if (buf[0] != 0x04)
      return (MBEDTLS2_ERR_ECP_FEATURE_UNAVAILABLE);

    if (ilen != 2 * plen + 1)
      return (MBEDTLS2_ERR_ECP_BAD_INPUT_DATA);

    MBEDTLS2_MPI_CHK(
        mbedtls2_mpi_read_binary(&pt->X, buf + 1, plen));
    MBEDTLS2_MPI_CHK(
        mbedtls2_mpi_read_binary(&pt->Y, buf + 1 + plen, plen));
    MBEDTLS2_MPI_CHK(mbedtls2_mpi_lset(&pt->Z, 1));
  }
#endif

cleanup:
  return (ret);
}

/*
 * Import a point from a TLS ECPoint record (RFC 4492)
 *      struct {
 *          opaque point <1..2^8-1>;
 *      } ECPoint;
 */
int mbedtls2_ecp_tls_read_point(const mbedtls2_ecp_group *grp,
                                       mbedtls2_ecp_point *pt,
                                       const unsigned char **buf,
                                       size_t buf_len) {
  unsigned char data_len;
  const unsigned char *buf_start;
  ECP_VALIDATE_RET(grp != NULL);
  ECP_VALIDATE_RET(pt != NULL);
  ECP_VALIDATE_RET(buf != NULL);
  ECP_VALIDATE_RET(*buf != NULL);

  /*
   * We must have at least two bytes (1 for length, at least one for data)
   */
  if (buf_len < 2)
    return (MBEDTLS2_ERR_ECP_BAD_INPUT_DATA);

  data_len = *(*buf)++;
  if (data_len < 1 || data_len > buf_len - 1)
    return (MBEDTLS2_ERR_ECP_BAD_INPUT_DATA);

  /*
   * Save buffer start for read_binary and update buf
   */
  buf_start = *buf;
  *buf += data_len;

  return (mbedtls2_ecp_point_read_binary(grp, pt, buf_start, data_len));
}

/*
 * Export a point as a TLS ECPoint record (RFC 4492)
 *      struct {
 *          opaque point <1..2^8-1>;
 *      } ECPoint;
 */
int mbedtls2_ecp_tls_write_point(const mbedtls2_ecp_group *grp,
                                        const mbedtls2_ecp_point *pt,
                                        int format, size_t *olen,
                                        unsigned char *buf, size_t blen) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  ECP_VALIDATE_RET(grp != NULL);
  ECP_VALIDATE_RET(pt != NULL);
  ECP_VALIDATE_RET(olen != NULL);
  ECP_VALIDATE_RET(buf != NULL);
  ECP_VALIDATE_RET(format == MBEDTLS2_ECP_PF_UNCOMPRESSED ||
                   format == MBEDTLS2_ECP_PF_COMPRESSED);

  /*
   * buffer length must be at least one, for our length byte
   */
  if (blen < 1)
    return (MBEDTLS2_ERR_ECP_BAD_INPUT_DATA);

  if ((ret = mbedtls2_ecp_point_write_binary(grp, pt, format, olen,
                                                    buf + 1, blen - 1)) != 0)
    return (ret);

  /*
   * write length to the first byte and update total length
   */
  buf[0] = (unsigned char)*olen;
  ++*olen;

  return (0);
}

/*
 * Set a group from an ECParameters record (RFC 4492)
 */
int mbedtls2_ecp_tls_read_group(mbedtls2_ecp_group *grp,
                                       const unsigned char **buf, size_t len) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  mbedtls2_ecp_group_id grp_id;
  ECP_VALIDATE_RET(grp != NULL);
  ECP_VALIDATE_RET(buf != NULL);
  ECP_VALIDATE_RET(*buf != NULL);

  if ((ret = mbedtls2_ecp_tls_read_group_id(&grp_id, buf, len)) != 0)
    return (ret);

  return (mbedtls2_ecp_group_load(grp, grp_id));
}

/*
 * Read a group id from an ECParameters record (RFC 4492) and convert it to
 * mbedtls2_ecp_group_id.
 */
int mbedtls2_ecp_tls_read_group_id(mbedtls2_ecp_group_id *grp,
                                          const unsigned char **buf,
                                          size_t len) {
  uint16_t tls_id;
  const mbedtls2_ecp_curve_info *curve_info;
  ECP_VALIDATE_RET(grp != NULL);
  ECP_VALIDATE_RET(buf != NULL);
  ECP_VALIDATE_RET(*buf != NULL);

  /*
   * We expect at least three bytes (see below)
   */
  if (len < 3)
    return (MBEDTLS2_ERR_ECP_BAD_INPUT_DATA);

  /*
   * First byte is curve_type; only named_curve is handled
   */
  if (*(*buf)++ != MBEDTLS2_ECP_TLS_NAMED_CURVE)
    return (MBEDTLS2_ERR_ECP_BAD_INPUT_DATA);

  /*
   * Next two bytes are the namedcurve value
   */
  tls_id = *(*buf)++;
  tls_id <<= 8;
  tls_id |= *(*buf)++;

  if ((curve_info = mbedtls2_ecp_curve_info_from_tls_id(tls_id)) == NULL)
    return (MBEDTLS2_ERR_ECP_FEATURE_UNAVAILABLE);

  *grp = curve_info->grp_id;

  return (0);
}

/*
 * Write the ECParameters record corresponding to a group (RFC 4492)
 */
int mbedtls2_ecp_tls_write_group(const mbedtls2_ecp_group *grp,
                                        size_t *olen, unsigned char *buf,
                                        size_t blen) {
  const mbedtls2_ecp_curve_info *curve_info;
  ECP_VALIDATE_RET(grp != NULL);
  ECP_VALIDATE_RET(buf != NULL);
  ECP_VALIDATE_RET(olen != NULL);

  if ((curve_info = mbedtls2_ecp_curve_info_from_grp_id(grp->id)) ==
      NULL)
    return (MBEDTLS2_ERR_ECP_BAD_INPUT_DATA);

  /*
   * We are going to write 3 bytes (see below)
   */
  *olen = 3;
  if (blen < *olen)
    return (MBEDTLS2_ERR_ECP_BUFFER_TOO_SMALL);

  /*
   * First byte is curve_type, always named_curve
   */
  *buf++ = MBEDTLS2_ECP_TLS_NAMED_CURVE;

  /*
   * Next two bytes are the namedcurve value
   */
  MBEDTLS2_PUT_UINT16_BE(curve_info->tls_id, buf, 0);

  return (0);
}

/*
 * Wrapper around fast quasi-modp functions, with fall-back to
 * mbedtls2_mpi_mod_mpi. See the documentation of struct
 * mbedtls2_ecp_group.
 *
 * This function is in the critial loop for mbedtls2_ecp_mul, so pay
 * attention to perf.
 */
static int ecp_modp(mbedtls2_mpi *N,
                    const mbedtls2_ecp_group *grp) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;

  if (grp->modp == NULL)
    return (mbedtls2_mpi_mod_mpi(N, N, &grp->P));

  /* N->s < 0 is a much faster test, which fails only if N is 0 */
  if ((N->s < 0 && mbedtls2_mpi_cmp_int(N, 0) != 0) ||
      mbedtls2_mpi_bitlen(N) > 2 * grp->pbits) {
    return (MBEDTLS2_ERR_ECP_BAD_INPUT_DATA);
  }

  MBEDTLS2_MPI_CHK(grp->modp(N));

  /* N->s < 0 is a much faster test, which fails only if N is 0 */
  while (N->s < 0 && mbedtls2_mpi_cmp_int(N, 0) != 0)
    MBEDTLS2_MPI_CHK(mbedtls2_mpi_add_mpi(N, N, &grp->P));

  while (mbedtls2_mpi_cmp_mpi(N, &grp->P) >= 0)
    /* we known P, N and the result are positive */
    MBEDTLS2_MPI_CHK(mbedtls2_mpi_sub_abs(N, N, &grp->P));

cleanup:
  return (ret);
}

/*
 * Fast mod-p functions expect their argument to be in the 0..p^2 range.
 *
 * In order to guarantee that, we need to ensure that operands of
 * mbedtls2_mpi_mul_mpi are in the 0..p range. So, after each operation
 * we will bring the result back to this range.
 *
 * The following macros are shortcuts for doing that.
 */

/*
 * Reduce a mbedtls2_mpi mod p in-place, general case, to use after
 * mbedtls2_mpi_mul_mpi
 */
#if defined(MBEDTLS2_SELF_TEST)
#define INC_MUL_COUNT mul_count++;
#else
#define INC_MUL_COUNT
#endif

#define MOD_MUL(N)                                                             \
  do {                                                                         \
    MBEDTLS2_MPI_CHK(ecp_modp(&(N), grp));                              \
    INC_MUL_COUNT                                                              \
  } while (0)

static inline int mbedtls2_mpi_mul_mod(
    const mbedtls2_ecp_group *grp, mbedtls2_mpi *X,
    const mbedtls2_mpi *A, const mbedtls2_mpi *B) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mpi(X, A, B));
  MOD_MUL(*X);
cleanup:
  return (ret);
}

/*
 * Reduce a mbedtls2_mpi mod p in-place, to use after
 * mbedtls2_mpi_sub_mpi N->s < 0 is a very fast test, which fails only if
 * N is 0
 */
#define MOD_SUB(N)                                                             \
  while ((N).s < 0 && mbedtls2_mpi_cmp_int(&(N), 0) != 0)               \
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_add_mpi(&(N), &(N), &grp->P))

#if (defined(MBEDTLS2_ECP_SHORT_WEIERSTRASS_ENABLED) &&                 \
     !(defined(MBEDTLS2_ECP_NO_FALLBACK) &&                             \
       defined(MBEDTLS2_ECP_DOUBLE_JAC_ALT) &&                          \
       defined(MBEDTLS2_ECP_ADD_MIXED_ALT))) ||                         \
    (defined(MBEDTLS2_ECP_MONTGOMERY_ENABLED) &&                        \
     !(defined(MBEDTLS2_ECP_NO_FALLBACK) &&                             \
       defined(MBEDTLS2_ECP_DOUBLE_ADD_MXZ_ALT)))
static inline int mbedtls2_mpi_sub_mod(
    const mbedtls2_ecp_group *grp, mbedtls2_mpi *X,
    const mbedtls2_mpi *A, const mbedtls2_mpi *B) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_sub_mpi(X, A, B));
  MOD_SUB(*X);
cleanup:
  return (ret);
}
#endif /* All functions referencing mbedtls2_mpi_sub_mod() are          \
          alt-implemented without fallback */

/*
 * Reduce a mbedtls2_mpi mod p in-place, to use after
 * mbedtls2_mpi_add_mpi and mbedtls2_mpi_mul_int. We known P, N
 * and the result are positive, so sub_abs is correct, and a bit faster.
 */
#define MOD_ADD(N)                                                             \
  while (mbedtls2_mpi_cmp_mpi(&(N), &grp->P) >= 0)                      \
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_sub_abs(&(N), &(N), &grp->P))

static inline int mbedtls2_mpi_add_mod(
    const mbedtls2_ecp_group *grp, mbedtls2_mpi *X,
    const mbedtls2_mpi *A, const mbedtls2_mpi *B) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_add_mpi(X, A, B));
  MOD_ADD(*X);
cleanup:
  return (ret);
}

#if defined(MBEDTLS2_ECP_SHORT_WEIERSTRASS_ENABLED) &&                  \
    !(defined(MBEDTLS2_ECP_NO_FALLBACK) &&                              \
      defined(MBEDTLS2_ECP_DOUBLE_JAC_ALT) &&                           \
      defined(MBEDTLS2_ECP_ADD_MIXED_ALT))
static inline int
mbedtls2_mpi_shift_l_mod(const mbedtls2_ecp_group *grp,
                                mbedtls2_mpi *X, size_t count) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_shift_l(X, count));
  MOD_ADD(*X);
cleanup:
  return (ret);
}
#endif /* All functions referencing mbedtls2_mpi_shift_l_mod() are      \
          alt-implemented without fallback */

#if defined(MBEDTLS2_ECP_SHORT_WEIERSTRASS_ENABLED)
/*
 * For curves in short Weierstrass form, we do all the internal operations in
 * Jacobian coordinates.
 *
 * For multiplication, we'll use a comb method with coutermeasueres against
 * SPA, hence timing attacks.
 */

/*
 * Normalize jacobian coordinates so that Z == 0 || Z == 1  (GECC 3.2.1)
 * Cost: 1N := 1I + 3M + 1S
 */
static int ecp_normalize_jac(const mbedtls2_ecp_group *grp,
                             mbedtls2_ecp_point *pt) {
  if (mbedtls2_mpi_cmp_int(&pt->Z, 0) == 0)
    return (0);

#if defined(MBEDTLS2_ECP_NORMALIZE_JAC_ALT)
  if (mbedtls2_internal_ecp_grp_capable(grp))
    return (mbedtls2_internal_ecp_normalize_jac(grp, pt));
#endif /* MBEDTLS2_ECP_NORMALIZE_JAC_ALT */

#if defined(MBEDTLS2_ECP_NO_FALLBACK) &&                                \
    defined(MBEDTLS2_ECP_NORMALIZE_JAC_ALT)
  return (MBEDTLS2_ERR_ECP_FEATURE_UNAVAILABLE);
#else
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  mbedtls2_mpi Zi, ZZi;
  mbedtls2_mpi_init(&Zi);
  mbedtls2_mpi_init(&ZZi);

  /*
   * X = X / Z^2  mod p
   */
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_inv_mod(&Zi, &pt->Z, &grp->P));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &ZZi, &Zi, &Zi));
  MBEDTLS2_MPI_CHK(
      mbedtls2_mpi_mul_mod(grp, &pt->X, &pt->X, &ZZi));

  /*
   * Y = Y / Z^3  mod p
   */
  MBEDTLS2_MPI_CHK(
      mbedtls2_mpi_mul_mod(grp, &pt->Y, &pt->Y, &ZZi));
  MBEDTLS2_MPI_CHK(
      mbedtls2_mpi_mul_mod(grp, &pt->Y, &pt->Y, &Zi));

  /*
   * Z = 1
   */
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_lset(&pt->Z, 1));

cleanup:

  mbedtls2_mpi_free(&Zi);
  mbedtls2_mpi_free(&ZZi);

  return (ret);
#endif /* !defined(MBEDTLS2_ECP_NO_FALLBACK) ||                         \
          !defined(MBEDTLS2_ECP_NORMALIZE_JAC_ALT) */
}

/*
 * Normalize jacobian coordinates of an array of (pointers to) points,
 * using Montgomery's trick to perform only one inversion mod P.
 * (See for example Cohen's "A Course in Computational Algebraic Number
 * Theory", Algorithm 10.3.4.)
 *
 * Warning: fails (returning an error) if one of the points is zero!
 * This should never happen, see choice of w in ecp_mul_comb().
 *
 * Cost: 1N(t) := 1I + (6t - 3)M + 1S
 */
static int ecp_normalize_jac_many(const mbedtls2_ecp_group *grp,
                                  mbedtls2_ecp_point *T[],
                                  size_t T_size) {
  if (T_size < 2)
    return (ecp_normalize_jac(grp, *T));

#if defined(MBEDTLS2_ECP_NORMALIZE_JAC_MANY_ALT)
  if (mbedtls2_internal_ecp_grp_capable(grp))
    return (mbedtls2_internal_ecp_normalize_jac_many(grp, T, T_size));
#endif

#if defined(MBEDTLS2_ECP_NO_FALLBACK) &&                                \
    defined(MBEDTLS2_ECP_NORMALIZE_JAC_MANY_ALT)
  return (MBEDTLS2_ERR_ECP_FEATURE_UNAVAILABLE);
#else
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  size_t i;
  mbedtls2_mpi *c, u, Zi, ZZi;

  if ((c = mbedtls2_calloc(T_size, sizeof(mbedtls2_mpi))) == NULL)
    return (MBEDTLS2_ERR_ECP_ALLOC_FAILED);

  for (i = 0; i < T_size; i++)
    mbedtls2_mpi_init(&c[i]);

  mbedtls2_mpi_init(&u);
  mbedtls2_mpi_init(&Zi);
  mbedtls2_mpi_init(&ZZi);

  /*
   * c[i] = Z_0 * ... * Z_i
   */
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_copy(&c[0], &T[0]->Z));
  for (i = 1; i < T_size; i++) {
    MBEDTLS2_MPI_CHK(
        mbedtls2_mpi_mul_mod(grp, &c[i], &c[i - 1], &T[i]->Z));
  }

  /*
   * u = 1 / (Z_0 * ... * Z_n) mod P
   */
  MBEDTLS2_MPI_CHK(
      mbedtls2_mpi_inv_mod(&u, &c[T_size - 1], &grp->P));

  for (i = T_size - 1;; i--) {
    /*
     * Zi = 1 / Z_i mod p
     * u = 1 / (Z_0 * ... * Z_i) mod P
     */
    if (i == 0) {
      MBEDTLS2_MPI_CHK(mbedtls2_mpi_copy(&Zi, &u));
    } else {
      MBEDTLS2_MPI_CHK(
          mbedtls2_mpi_mul_mod(grp, &Zi, &u, &c[i - 1]));
      MBEDTLS2_MPI_CHK(
          mbedtls2_mpi_mul_mod(grp, &u, &u, &T[i]->Z));
    }

    /*
     * proceed as in normalize()
     */
    MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &ZZi, &Zi, &Zi));
    MBEDTLS2_MPI_CHK(
        mbedtls2_mpi_mul_mod(grp, &T[i]->X, &T[i]->X, &ZZi));
    MBEDTLS2_MPI_CHK(
        mbedtls2_mpi_mul_mod(grp, &T[i]->Y, &T[i]->Y, &ZZi));
    MBEDTLS2_MPI_CHK(
        mbedtls2_mpi_mul_mod(grp, &T[i]->Y, &T[i]->Y, &Zi));

    /*
     * Post-precessing: reclaim some memory by shrinking coordinates
     * - not storing Z (always 1)
     * - shrinking other coordinates, but still keeping the same number of
     *   limbs as P, as otherwise it will too likely be regrown too fast.
     */
    MBEDTLS2_MPI_CHK(mbedtls2_mpi_shrink(&T[i]->X, grp->P.n));
    MBEDTLS2_MPI_CHK(mbedtls2_mpi_shrink(&T[i]->Y, grp->P.n));
    mbedtls2_mpi_free(&T[i]->Z);

    if (i == 0)
      break;
  }

cleanup:

  mbedtls2_mpi_free(&u);
  mbedtls2_mpi_free(&Zi);
  mbedtls2_mpi_free(&ZZi);
  for (i = 0; i < T_size; i++)
    mbedtls2_mpi_free(&c[i]);
  mbedtls2_free(c);

  return (ret);
#endif /* !defined(MBEDTLS2_ECP_NO_FALLBACK) ||                         \
          !defined(MBEDTLS2_ECP_NORMALIZE_JAC_MANY_ALT) */
}

/*
 * Conditional point inversion: Q -> -Q = (Q.X, -Q.Y, Q.Z) without leak.
 * "inv" must be 0 (don't invert) or 1 (invert) or the result will be invalid
 */
static int ecp_safe_invert_jac(const mbedtls2_ecp_group *grp,
                               mbedtls2_ecp_point *Q,
                               unsigned char inv) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  unsigned char nonzero;
  mbedtls2_mpi mQY;

  mbedtls2_mpi_init(&mQY);

  /* Use the fact that -Q.Y mod P = P - Q.Y unless Q.Y == 0 */
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_sub_mpi(&mQY, &grp->P, &Q->Y));
  nonzero = mbedtls2_mpi_cmp_int(&Q->Y, 0) != 0;
  MBEDTLS2_MPI_CHK(
      mbedtls2_mpi_safe_cond_assign(&Q->Y, &mQY, inv & nonzero));

cleanup:
  mbedtls2_mpi_free(&mQY);

  return (ret);
}

/*
 * Point doubling R = 2 P, Jacobian coordinates
 *
 * Based on
 * http://www.hyperelliptic.org/EFD/g1p/auto-shortw-jacobian.html#doubling-dbl-1998-cmo-2
 * .
 *
 * We follow the variable naming fairly closely. The formula variations that
 * trade a MUL for a SQR (plus a few ADDs) aren't useful as our bignum
 * implementation doesn't distinguish squaring.
 *
 * Standard optimizations are applied when curve parameter A is one of { 0, -3
 * }.
 *
 * Cost: 1D := 3M + 4S          (A ==  0)
 *             4M + 4S          (A == -3)
 *             3M + 6S + 1a     otherwise
 */
static int ecp_double_jac(const mbedtls2_ecp_group *grp,
                          mbedtls2_ecp_point *R,
                          const mbedtls2_ecp_point *P) {
#if defined(MBEDTLS2_SELF_TEST)
  dbl_count++;
#endif

#if defined(MBEDTLS2_ECP_DOUBLE_JAC_ALT)
  if (mbedtls2_internal_ecp_grp_capable(grp))
    return (mbedtls2_internal_ecp_double_jac(grp, R, P));
#endif /* MBEDTLS2_ECP_DOUBLE_JAC_ALT */

#if defined(MBEDTLS2_ECP_NO_FALLBACK) &&                                \
    defined(MBEDTLS2_ECP_DOUBLE_JAC_ALT)
  return (MBEDTLS2_ERR_ECP_FEATURE_UNAVAILABLE);
#else
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  mbedtls2_mpi M, S, T, U;

  mbedtls2_mpi_init(&M);
  mbedtls2_mpi_init(&S);
  mbedtls2_mpi_init(&T);
  mbedtls2_mpi_init(&U);

  /* Special case for A = -3 */
  if (grp->A.p == NULL) {
    /* M = 3(X + Z^2)(X - Z^2) */
    MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &S, &P->Z, &P->Z));
    MBEDTLS2_MPI_CHK(mbedtls2_mpi_add_mod(grp, &T, &P->X, &S));
    MBEDTLS2_MPI_CHK(mbedtls2_mpi_sub_mod(grp, &U, &P->X, &S));
    MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &S, &T, &U));
    MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_int(&M, &S, 3));
    MOD_ADD(M);
  } else {
    /* M = 3.X^2 */
    MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &S, &P->X, &P->X));
    MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_int(&M, &S, 3));
    MOD_ADD(M);

    /* Optimize away for "koblitz" curves with A = 0 */
    if (mbedtls2_mpi_cmp_int(&grp->A, 0) != 0) {
      /* M += A.Z^4 */
      MBEDTLS2_MPI_CHK(
          mbedtls2_mpi_mul_mod(grp, &S, &P->Z, &P->Z));
      MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &T, &S, &S));
      MBEDTLS2_MPI_CHK(
          mbedtls2_mpi_mul_mod(grp, &S, &T, &grp->A));
      MBEDTLS2_MPI_CHK(mbedtls2_mpi_add_mod(grp, &M, &M, &S));
    }
  }

  /* S = 4.X.Y^2 */
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &T, &P->Y, &P->Y));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_shift_l_mod(grp, &T, 1));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &S, &P->X, &T));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_shift_l_mod(grp, &S, 1));

  /* U = 8.Y^4 */
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &U, &T, &T));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_shift_l_mod(grp, &U, 1));

  /* T = M^2 - 2.S */
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &T, &M, &M));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_sub_mod(grp, &T, &T, &S));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_sub_mod(grp, &T, &T, &S));

  /* S = M(S - T) - U */
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_sub_mod(grp, &S, &S, &T));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &S, &S, &M));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_sub_mod(grp, &S, &S, &U));

  /* U = 2.Y.Z */
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &U, &P->Y, &P->Z));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_shift_l_mod(grp, &U, 1));

  MBEDTLS2_MPI_CHK(mbedtls2_mpi_copy(&R->X, &T));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_copy(&R->Y, &S));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_copy(&R->Z, &U));

cleanup:
  mbedtls2_mpi_free(&M);
  mbedtls2_mpi_free(&S);
  mbedtls2_mpi_free(&T);
  mbedtls2_mpi_free(&U);

  return (ret);
#endif /* !defined(MBEDTLS2_ECP_NO_FALLBACK) ||                         \
          !defined(MBEDTLS2_ECP_DOUBLE_JAC_ALT) */
}

/*
 * Addition: R = P + Q, mixed affine-Jacobian coordinates (GECC 3.22)
 *
 * The coordinates of Q must be normalized (= affine),
 * but those of P don't need to. R is not normalized.
 *
 * Special cases: (1) P or Q is zero, (2) R is zero, (3) P == Q.
 * None of these cases can happen as intermediate step in ecp_mul_comb():
 * - at each step, P, Q and R are multiples of the base point, the factor
 *   being less than its order, so none of them is zero;
 * - Q is an odd multiple of the base point, P an even multiple,
 *   due to the choice of precomputed points in the modified comb method.
 * So branches for these cases do not leak secret information.
 *
 * We accept Q->Z being unset (saving memory in tables) as meaning 1.
 *
 * Cost: 1A := 8M + 3S
 */
static int ecp_add_mixed(const mbedtls2_ecp_group *grp,
                         mbedtls2_ecp_point *R,
                         const mbedtls2_ecp_point *P,
                         const mbedtls2_ecp_point *Q) {
#if defined(MBEDTLS2_SELF_TEST)
  add_count++;
#endif

#if defined(MBEDTLS2_ECP_ADD_MIXED_ALT)
  if (mbedtls2_internal_ecp_grp_capable(grp))
    return (mbedtls2_internal_ecp_add_mixed(grp, R, P, Q));
#endif /* MBEDTLS2_ECP_ADD_MIXED_ALT */

#if defined(MBEDTLS2_ECP_NO_FALLBACK) &&                                \
    defined(MBEDTLS2_ECP_ADD_MIXED_ALT)
  return (MBEDTLS2_ERR_ECP_FEATURE_UNAVAILABLE);
#else
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  mbedtls2_mpi T1, T2, T3, T4, X, Y, Z;

  /*
   * Trivial cases: P == 0 or Q == 0 (case 1)
   */
  if (mbedtls2_mpi_cmp_int(&P->Z, 0) == 0)
    return (mbedtls2_ecp_copy(R, Q));

  if (Q->Z.p != NULL && mbedtls2_mpi_cmp_int(&Q->Z, 0) == 0)
    return (mbedtls2_ecp_copy(R, P));

  /*
   * Make sure Q coordinates are normalized
   */
  if (Q->Z.p != NULL && mbedtls2_mpi_cmp_int(&Q->Z, 1) != 0)
    return (MBEDTLS2_ERR_ECP_BAD_INPUT_DATA);

  mbedtls2_mpi_init(&T1);
  mbedtls2_mpi_init(&T2);
  mbedtls2_mpi_init(&T3);
  mbedtls2_mpi_init(&T4);
  mbedtls2_mpi_init(&X);
  mbedtls2_mpi_init(&Y);
  mbedtls2_mpi_init(&Z);

  MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &T1, &P->Z, &P->Z));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &T2, &T1, &P->Z));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &T1, &T1, &Q->X));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &T2, &T2, &Q->Y));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_sub_mod(grp, &T1, &T1, &P->X));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_sub_mod(grp, &T2, &T2, &P->Y));

  /* Special cases (2) and (3) */
  if (mbedtls2_mpi_cmp_int(&T1, 0) == 0) {
    if (mbedtls2_mpi_cmp_int(&T2, 0) == 0) {
      ret = ecp_double_jac(grp, R, P);
      goto cleanup;
    } else {
      ret = mbedtls2_ecp_set_zero(R);
      goto cleanup;
    }
  }

  MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &Z, &P->Z, &T1));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &T3, &T1, &T1));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &T4, &T3, &T1));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &T3, &T3, &P->X));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_copy(&T1, &T3));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_shift_l_mod(grp, &T1, 1));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &X, &T2, &T2));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_sub_mod(grp, &X, &X, &T1));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_sub_mod(grp, &X, &X, &T4));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_sub_mod(grp, &T3, &T3, &X));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &T3, &T3, &T2));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &T4, &T4, &P->Y));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_sub_mod(grp, &Y, &T3, &T4));

  MBEDTLS2_MPI_CHK(mbedtls2_mpi_copy(&R->X, &X));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_copy(&R->Y, &Y));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_copy(&R->Z, &Z));

cleanup:

  mbedtls2_mpi_free(&T1);
  mbedtls2_mpi_free(&T2);
  mbedtls2_mpi_free(&T3);
  mbedtls2_mpi_free(&T4);
  mbedtls2_mpi_free(&X);
  mbedtls2_mpi_free(&Y);
  mbedtls2_mpi_free(&Z);

  return (ret);
#endif /* !defined(MBEDTLS2_ECP_NO_FALLBACK) ||                         \
          !defined(MBEDTLS2_ECP_ADD_MIXED_ALT) */
}

/*
 * Randomize jacobian coordinates:
 * (X, Y, Z) -> (l^2 X, l^3 Y, l Z) for random l
 * This is sort of the reverse operation of ecp_normalize_jac().
 *
 * This countermeasure was first suggested in [2].
 */
static int ecp_randomize_jac(const mbedtls2_ecp_group *grp,
                             mbedtls2_ecp_point *pt,
                             int (*f_rng)(void *, unsigned char *, size_t),
                             void *p_rng) {
#if defined(MBEDTLS2_ECP_RANDOMIZE_JAC_ALT)
  if (mbedtls2_internal_ecp_grp_capable(grp))
    return (mbedtls2_internal_ecp_randomize_jac(grp, pt, f_rng, p_rng));
#endif /* MBEDTLS2_ECP_RANDOMIZE_JAC_ALT */

#if defined(MBEDTLS2_ECP_NO_FALLBACK) &&                                \
    defined(MBEDTLS2_ECP_RANDOMIZE_JAC_ALT)
  return (MBEDTLS2_ERR_ECP_FEATURE_UNAVAILABLE);
#else
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  mbedtls2_mpi l, ll;

  mbedtls2_mpi_init(&l);
  mbedtls2_mpi_init(&ll);

  /* Generate l such that 1 < l < p */
  MBEDTLS2_MPI_CHK(
      mbedtls2_mpi_random(&l, 2, &grp->P, f_rng, p_rng));

  /* Z = l * Z */
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &pt->Z, &pt->Z, &l));

  /* X = l^2 * X */
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &ll, &l, &l));
  MBEDTLS2_MPI_CHK(
      mbedtls2_mpi_mul_mod(grp, &pt->X, &pt->X, &ll));

  /* Y = l^3 * Y */
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &ll, &ll, &l));
  MBEDTLS2_MPI_CHK(
      mbedtls2_mpi_mul_mod(grp, &pt->Y, &pt->Y, &ll));

cleanup:
  mbedtls2_mpi_free(&l);
  mbedtls2_mpi_free(&ll);

  if (ret == MBEDTLS2_ERR_MPI_NOT_ACCEPTABLE)
    ret = MBEDTLS2_ERR_ECP_RANDOM_FAILED;
  return (ret);
#endif /* !defined(MBEDTLS2_ECP_NO_FALLBACK) ||                         \
          !defined(MBEDTLS2_ECP_RANDOMIZE_JAC_ALT) */
}

/*
 * Check and define parameters used by the comb method (see below for details)
 */
#if MBEDTLS2_ECP_WINDOW_SIZE < 2 || MBEDTLS2_ECP_WINDOW_SIZE > 7
#error "MBEDTLS2_ECP_WINDOW_SIZE out of bounds"
#endif

/* d = ceil( n / w ) */
#define COMB_MAX_D (MBEDTLS2_ECP_MAX_BITS + 1) / 2

/* number of precomputed points */
#define COMB_MAX_PRE (1 << (MBEDTLS2_ECP_WINDOW_SIZE - 1))

/*
 * Compute the representation of m that will be used with our comb method.
 *
 * The basic comb method is described in GECC 3.44 for example. We use a
 * modified version that provides resistance to SPA by avoiding zero
 * digits in the representation as in [3]. We modify the method further by
 * requiring that all K_i be odd, which has the small cost that our
 * representation uses one more K_i, due to carries, but saves on the size of
 * the precomputed table.
 *
 * Summary of the comb method and its modifications:
 *
 * - The goal is to compute m*P for some w*d-bit integer m.
 *
 * - The basic comb method splits m into the w-bit integers
 *   x[0] .. x[d-1] where x[i] consists of the bits in m whose
 *   index has residue i modulo d, and computes m * P as
 *   S[x[0]] + 2 * S[x[1]] + .. + 2^(d-1) S[x[d-1]], where
 *   S[i_{w-1} .. i_0] := i_{w-1} 2^{(w-1)d} P + ... + i_1 2^d P + i_0 P.
 *
 * - If it happens that, say, x[i+1]=0 (=> S[x[i+1]]=0), one can replace the sum
 * by
 *    .. + 2^{i-1} S[x[i-1]] - 2^i S[x[i]] + 2^{i+1} S[x[i]] + 2^{i+2} S[x[i+2]]
 * .., thereby successively converting it into a form where all summands are
 * nonzero, at the cost of negative summands. This is the basic idea of [3].
 *
 * - More generally, even if x[i+1] != 0, we can first transform the sum as
 *   .. - 2^i S[x[i]] + 2^{i+1} ( S[x[i]] + S[x[i+1]] ) + 2^{i+2} S[x[i+2]] ..,
 *   and then replace S[x[i]] + S[x[i+1]] = S[x[i] ^ x[i+1]] + 2 S[x[i] &
 * x[i+1]]. Performing and iterating this procedure for those x[i] that are even
 *   (keeping track of carry), we can transform the original sum into one of the
 * form S[x'[0]] +- 2 S[x'[1]] +- .. +- 2^{d-1} S[x'[d-1]] + 2^d S[x'[d]] with
 * all x'[i] odd. It is therefore only necessary to know S at odd indices, which
 * is why we are only computing half of it in the first place in
 *   ecp_precompute_comb and accessing it with index abs(i) / 2 in
 * ecp_select_comb.
 *
 * - For the sake of compactness, only the seven low-order bits of x[i]
 *   are used to represent its absolute value (K_i in the paper), and the msb
 *   of x[i] encodes the sign (s_i in the paper): it is set if and only if
 *   if s_i == -1;
 *
 * Calling conventions:
 * - x is an array of size d + 1
 * - w is the size, ie number of teeth, of the comb, and must be between
 *   2 and 7 (in practice, between 2 and MBEDTLS2_ECP_WINDOW_SIZE)
 * - m is the MPI, expected to be odd and such that bitlength(m) <= w * d
 *   (the result will be incorrect if these assumptions are not satisfied)
 */
static void ecp_comb_recode_core(unsigned char x[], size_t d, unsigned char w,
                                 const mbedtls2_mpi *m) {
  size_t i, j;
  unsigned char c, cc, adjust;

  memset(x, 0, d + 1);

  /* First get the classical comb values (except for x_d = 0) */
  for (i = 0; i < d; i++)
    for (j = 0; j < w; j++)
      x[i] |= mbedtls2_mpi_get_bit(m, i + d * j) << j;

  /* Now make sure x_1 .. x_d are odd */
  c = 0;
  for (i = 1; i <= d; i++) {
    /* Add carry and update it */
    cc = x[i] & c;
    x[i] = x[i] ^ c;
    c = cc;

    /* Adjust if needed, avoiding branches */
    adjust = 1 - (x[i] & 0x01);
    c |= x[i] & (x[i - 1] * adjust);
    x[i] = x[i] ^ (x[i - 1] * adjust);
    x[i - 1] |= adjust << 7;
  }
}

/*
 * Precompute points for the adapted comb method
 *
 * Assumption: T must be able to hold 2^{w - 1} elements.
 *
 * Operation: If i = i_{w-1} ... i_1 is the binary representation of i,
 *            sets T[i] = i_{w-1} 2^{(w-1)d} P + ... + i_1 2^d P + P.
 *
 * Cost: d(w-1) D + (2^{w-1} - 1) A + 1 N(w-1) + 1 N(2^{w-1} - 1)
 *
 * Note: Even comb values (those where P would be omitted from the
 *       sum defining T[i] above) are not needed in our adaption
 *       the comb method. See ecp_comb_recode_core().
 *
 * This function currently works in four steps:
 * (1) [dbl]      Computation of intermediate T[i] for 2-power values of i
 * (2) [norm_dbl] Normalization of coordinates of these T[i]
 * (3) [add]      Computation of all T[i]
 * (4) [norm_add] Normalization of all T[i]
 *
 * Step 1 can be interrupted but not the others; together with the final
 * coordinate normalization they are the largest steps done at once, depending
 * on the window size. Here are operation counts for P-256:
 *
 * step     (2)     (3)     (4)
 * w = 5    142     165     208
 * w = 4    136      77     160
 * w = 3    130      33     136
 * w = 2    124      11     124
 *
 * So if ECC operations are blocking for too long even with a low max_ops
 * value, it's useful to set MBEDTLS2_ECP_WINDOW_SIZE to a lower value in
 * order to minimize maximum blocking time.
 */
static int ecp_precompute_comb(const mbedtls2_ecp_group *grp,
                               mbedtls2_ecp_point T[],
                               const mbedtls2_ecp_point *P,
                               unsigned char w, size_t d,
                               mbedtls2_ecp_restart_ctx *rs_ctx) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  unsigned char i;
  size_t j = 0;
  const unsigned char T_size = 1U << (w - 1);
  mbedtls2_ecp_point *cur, *TT[COMB_MAX_PRE - 1];

#if defined(MBEDTLS2_ECP_RESTARTABLE)
  if (rs_ctx != NULL && rs_ctx->rsm != NULL) {
    if (rs_ctx->rsm->state == ecp_rsm_pre_dbl)
      goto dbl;
    if (rs_ctx->rsm->state == ecp_rsm_pre_norm_dbl)
      goto norm_dbl;
    if (rs_ctx->rsm->state == ecp_rsm_pre_add)
      goto add;
    if (rs_ctx->rsm->state == ecp_rsm_pre_norm_add)
      goto norm_add;
  }
#else
  (void)rs_ctx;
#endif

#if defined(MBEDTLS2_ECP_RESTARTABLE)
  if (rs_ctx != NULL && rs_ctx->rsm != NULL) {
    rs_ctx->rsm->state = ecp_rsm_pre_dbl;

    /* initial state for the loop */
    rs_ctx->rsm->i = 0;
  }

dbl:
#endif
  /*
   * Set T[0] = P and
   * T[2^{l-1}] = 2^{dl} P for l = 1 .. w-1 (this is not the final value)
   */
  MBEDTLS2_MPI_CHK(mbedtls2_ecp_copy(&T[0], P));

#if defined(MBEDTLS2_ECP_RESTARTABLE)
  if (rs_ctx != NULL && rs_ctx->rsm != NULL && rs_ctx->rsm->i != 0)
    j = rs_ctx->rsm->i;
  else
#endif
    j = 0;

  for (; j < d * (w - 1); j++) {
    MBEDTLS2_ECP_BUDGET(MBEDTLS2_ECP_OPS_DBL);

    i = 1U << (j / d);
    cur = T + i;

    if (j % d == 0)
      MBEDTLS2_MPI_CHK(mbedtls2_ecp_copy(cur, T + (i >> 1)));

    MBEDTLS2_MPI_CHK(ecp_double_jac(grp, cur, cur));
  }

#if defined(MBEDTLS2_ECP_RESTARTABLE)
  if (rs_ctx != NULL && rs_ctx->rsm != NULL)
    rs_ctx->rsm->state = ecp_rsm_pre_norm_dbl;

norm_dbl:
#endif
  /*
   * Normalize current elements in T. As T has holes,
   * use an auxiliary array of pointers to elements in T.
   */
  j = 0;
  for (i = 1; i < T_size; i <<= 1)
    TT[j++] = T + i;

  MBEDTLS2_ECP_BUDGET(MBEDTLS2_ECP_OPS_INV + 6 * j - 2);

  MBEDTLS2_MPI_CHK(ecp_normalize_jac_many(grp, TT, j));

#if defined(MBEDTLS2_ECP_RESTARTABLE)
  if (rs_ctx != NULL && rs_ctx->rsm != NULL)
    rs_ctx->rsm->state = ecp_rsm_pre_add;

add:
#endif
  /*
   * Compute the remaining ones using the minimal number of additions
   * Be careful to update T[2^l] only after using it!
   */
  MBEDTLS2_ECP_BUDGET((T_size - 1) * MBEDTLS2_ECP_OPS_ADD);

  for (i = 1; i < T_size; i <<= 1) {
    j = i;
    while (j--)
      MBEDTLS2_MPI_CHK(ecp_add_mixed(grp, &T[i + j], &T[j], &T[i]));
  }

#if defined(MBEDTLS2_ECP_RESTARTABLE)
  if (rs_ctx != NULL && rs_ctx->rsm != NULL)
    rs_ctx->rsm->state = ecp_rsm_pre_norm_add;

norm_add:
#endif
  /*
   * Normalize final elements in T. Even though there are no holes now, we
   * still need the auxiliary array for homogeneity with the previous
   * call. Also, skip T[0] which is already normalised, being a copy of P.
   */
  for (j = 0; j + 1 < T_size; j++)
    TT[j] = T + j + 1;

  MBEDTLS2_ECP_BUDGET(MBEDTLS2_ECP_OPS_INV + 6 * j - 2);

  MBEDTLS2_MPI_CHK(ecp_normalize_jac_many(grp, TT, j));

cleanup:
#if defined(MBEDTLS2_ECP_RESTARTABLE)
  if (rs_ctx != NULL && rs_ctx->rsm != NULL &&
      ret == MBEDTLS2_ERR_ECP_IN_PROGRESS) {
    if (rs_ctx->rsm->state == ecp_rsm_pre_dbl)
      rs_ctx->rsm->i = j;
  }
#endif

  return (ret);
}

/*
 * Select precomputed point: R = sign(i) * T[ abs(i) / 2 ]
 *
 * See ecp_comb_recode_core() for background
 */
static int ecp_select_comb(const mbedtls2_ecp_group *grp,
                           mbedtls2_ecp_point *R,
                           const mbedtls2_ecp_point T[],
                           unsigned char T_size, unsigned char i) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  unsigned char ii, j;

  /* Ignore the "sign" bit and scale down */
  ii = (i & 0x7Fu) >> 1;

  /* Read the whole table to thwart cache-based timing attacks */
  for (j = 0; j < T_size; j++) {
    MBEDTLS2_MPI_CHK(
        mbedtls2_mpi_safe_cond_assign(&R->X, &T[j].X, j == ii));
    MBEDTLS2_MPI_CHK(
        mbedtls2_mpi_safe_cond_assign(&R->Y, &T[j].Y, j == ii));
  }

  /* Safely invert result if i is "negative" */
  MBEDTLS2_MPI_CHK(ecp_safe_invert_jac(grp, R, i >> 7));

cleanup:
  return (ret);
}

/*
 * Core multiplication algorithm for the (modified) comb method.
 * This part is actually common with the basic comb method (GECC 3.44)
 *
 * Cost: d A + d D + 1 R
 */
static int ecp_mul_comb_core(const mbedtls2_ecp_group *grp,
                             mbedtls2_ecp_point *R,
                             const mbedtls2_ecp_point T[],
                             unsigned char T_size, const unsigned char x[],
                             size_t d,
                             int (*f_rng)(void *, unsigned char *, size_t),
                             void *p_rng,
                             mbedtls2_ecp_restart_ctx *rs_ctx) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  mbedtls2_ecp_point Txi;
  size_t i;

  mbedtls2_ecp_point_init(&Txi);

#if !defined(MBEDTLS2_ECP_RESTARTABLE)
  (void)rs_ctx;
#endif

#if defined(MBEDTLS2_ECP_RESTARTABLE)
  if (rs_ctx != NULL && rs_ctx->rsm != NULL &&
      rs_ctx->rsm->state != ecp_rsm_comb_core) {
    rs_ctx->rsm->i = 0;
    rs_ctx->rsm->state = ecp_rsm_comb_core;
  }

  /* new 'if' instead of nested for the sake of the 'else' branch */
  if (rs_ctx != NULL && rs_ctx->rsm != NULL && rs_ctx->rsm->i != 0) {
    /* restore current index (R already pointing to rs_ctx->rsm->R) */
    i = rs_ctx->rsm->i;
  } else
#endif
  {
    /* Start with a non-zero point and randomize its coordinates */
    i = d;
    MBEDTLS2_MPI_CHK(ecp_select_comb(grp, R, T, T_size, x[i]));
    MBEDTLS2_MPI_CHK(mbedtls2_mpi_lset(&R->Z, 1));
#if defined(MBEDTLS2_ECP_NO_INTERNAL_RNG)
    if (f_rng != 0)
#endif
      MBEDTLS2_MPI_CHK(ecp_randomize_jac(grp, R, f_rng, p_rng));
  }

  while (i != 0) {
    MBEDTLS2_ECP_BUDGET(MBEDTLS2_ECP_OPS_DBL +
                               MBEDTLS2_ECP_OPS_ADD);
    --i;

    MBEDTLS2_MPI_CHK(ecp_double_jac(grp, R, R));
    MBEDTLS2_MPI_CHK(ecp_select_comb(grp, &Txi, T, T_size, x[i]));
    MBEDTLS2_MPI_CHK(ecp_add_mixed(grp, R, R, &Txi));
  }

cleanup:

  mbedtls2_ecp_point_free(&Txi);

#if defined(MBEDTLS2_ECP_RESTARTABLE)
  if (rs_ctx != NULL && rs_ctx->rsm != NULL &&
      ret == MBEDTLS2_ERR_ECP_IN_PROGRESS) {
    rs_ctx->rsm->i = i;
    /* no need to save R, already pointing to rs_ctx->rsm->R */
  }
#endif

  return (ret);
}

/*
 * Recode the scalar to get constant-time comb multiplication
 *
 * As the actual scalar recoding needs an odd scalar as a starting point,
 * this wrapper ensures that by replacing m by N - m if necessary, and
 * informs the caller that the result of multiplication will be negated.
 *
 * This works because we only support large prime order for Short Weierstrass
 * curves, so N is always odd hence either m or N - m is.
 *
 * See ecp_comb_recode_core() for background.
 */
static int ecp_comb_recode_scalar(const mbedtls2_ecp_group *grp,
                                  const mbedtls2_mpi *m,
                                  unsigned char k[COMB_MAX_D + 1], size_t d,
                                  unsigned char w,
                                  unsigned char *parity_trick) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  mbedtls2_mpi M, mm;

  mbedtls2_mpi_init(&M);
  mbedtls2_mpi_init(&mm);

  /* N is always odd (see above), just make extra sure */
  if (mbedtls2_mpi_get_bit(&grp->N, 0) != 1)
    return (MBEDTLS2_ERR_ECP_BAD_INPUT_DATA);

  /* do we need the parity trick? */
  *parity_trick = (mbedtls2_mpi_get_bit(m, 0) == 0);

  /* execute parity fix in constant time */
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_copy(&M, m));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_sub_mpi(&mm, &grp->N, m));
  MBEDTLS2_MPI_CHK(
      mbedtls2_mpi_safe_cond_assign(&M, &mm, *parity_trick));

  /* actual scalar recoding */
  ecp_comb_recode_core(k, d, w, &M);

cleanup:
  mbedtls2_mpi_free(&mm);
  mbedtls2_mpi_free(&M);

  return (ret);
}

/*
 * Perform comb multiplication (for short Weierstrass curves)
 * once the auxiliary table has been pre-computed.
 *
 * Scalar recoding may use a parity trick that makes us compute -m * P,
 * if that is the case we'll need to recover m * P at the end.
 */
static int ecp_mul_comb_after_precomp(
    const mbedtls2_ecp_group *grp, mbedtls2_ecp_point *R,
    const mbedtls2_mpi *m, const mbedtls2_ecp_point *T,
    unsigned char T_size, unsigned char w, size_t d,
    int (*f_rng)(void *, unsigned char *, size_t), void *p_rng,
    mbedtls2_ecp_restart_ctx *rs_ctx) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  unsigned char parity_trick;
  unsigned char k[COMB_MAX_D + 1];
  mbedtls2_ecp_point *RR = R;

#if defined(MBEDTLS2_ECP_RESTARTABLE)
  if (rs_ctx != NULL && rs_ctx->rsm != NULL) {
    RR = &rs_ctx->rsm->R;

    if (rs_ctx->rsm->state == ecp_rsm_final_norm)
      goto final_norm;
  }
#endif

  MBEDTLS2_MPI_CHK(
      ecp_comb_recode_scalar(grp, m, k, d, w, &parity_trick));
  MBEDTLS2_MPI_CHK(
      ecp_mul_comb_core(grp, RR, T, T_size, k, d, f_rng, p_rng, rs_ctx));
  MBEDTLS2_MPI_CHK(ecp_safe_invert_jac(grp, RR, parity_trick));

#if defined(MBEDTLS2_ECP_RESTARTABLE)
  if (rs_ctx != NULL && rs_ctx->rsm != NULL)
    rs_ctx->rsm->state = ecp_rsm_final_norm;

final_norm:
  MBEDTLS2_ECP_BUDGET(MBEDTLS2_ECP_OPS_INV);
#endif
  /*
   * Knowledge of the jacobian coordinates may leak the last few bits of the
   * scalar [1], and since our MPI implementation isn't constant-flow,
   * inversion (used for coordinate normalization) may leak the full value
   * of its input via side-channels [2].
   *
   * [1] https://eprint.iacr.org/2003/191
   * [2] https://eprint.iacr.org/2020/055
   *
   * Avoid the leak by randomizing coordinates before we normalize them.
   */
#if defined(MBEDTLS2_ECP_NO_INTERNAL_RNG)
  if (f_rng != 0)
#endif
    MBEDTLS2_MPI_CHK(ecp_randomize_jac(grp, RR, f_rng, p_rng));

  MBEDTLS2_MPI_CHK(ecp_normalize_jac(grp, RR));

#if defined(MBEDTLS2_ECP_RESTARTABLE)
  if (rs_ctx != NULL && rs_ctx->rsm != NULL)
    MBEDTLS2_MPI_CHK(mbedtls2_ecp_copy(R, RR));
#endif

cleanup:
  return (ret);
}

/*
 * Pick window size based on curve size and whether we optimize for base point
 */
static unsigned char ecp_pick_window_size(const mbedtls2_ecp_group *grp,
                                          unsigned char p_eq_g) {
  unsigned char w;

  /*
   * Minimize the number of multiplications, that is minimize
   * 10 * d * w + 18 * 2^(w-1) + 11 * d + 7 * w, with d = ceil( nbits / w )
   * (see costs of the various parts, with 1S = 1M)
   */
  w = grp->nbits >= 384 ? 5 : 4;

  /*
   * If P == G, pre-compute a bit more, since this may be re-used later.
   * Just adding one avoids upping the cost of the first mul too much,
   * and the memory cost too.
   */
  if (p_eq_g)
    w++;

  /*
   * Make sure w is within bounds.
   * (The last test is useful only for very small curves in the test suite.)
   */
#if (MBEDTLS2_ECP_WINDOW_SIZE < 6)
  if (w > MBEDTLS2_ECP_WINDOW_SIZE)
    w = MBEDTLS2_ECP_WINDOW_SIZE;
#endif
  if (w >= grp->nbits)
    w = 2;

  return (w);
}

/*
 * Multiplication using the comb method - for curves in short Weierstrass form
 *
 * This function is mainly responsible for administrative work:
 * - managing the restart context if enabled
 * - managing the table of precomputed points (passed between the below two
 *   functions): allocation, computation, ownership tranfer, freeing.
 *
 * It delegates the actual arithmetic work to:
 *      ecp_precompute_comb() and ecp_mul_comb_with_precomp()
 *
 * See comments on ecp_comb_recode_core() regarding the computation strategy.
 */
static int ecp_mul_comb(mbedtls2_ecp_group *grp,
                        mbedtls2_ecp_point *R,
                        const mbedtls2_mpi *m,
                        const mbedtls2_ecp_point *P,
                        int (*f_rng)(void *, unsigned char *, size_t),
                        void *p_rng, mbedtls2_ecp_restart_ctx *rs_ctx) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  unsigned char w, p_eq_g, i;
  size_t d;
  unsigned char T_size = 0, T_ok = 0;
  mbedtls2_ecp_point *T = NULL;
#if !defined(MBEDTLS2_ECP_NO_INTERNAL_RNG)
  ecp_drbg_context drbg_ctx;

  ecp_drbg_init(&drbg_ctx);
#endif

  ECP_RS_ENTER(rsm);

#if !defined(MBEDTLS2_ECP_NO_INTERNAL_RNG)
  if (f_rng == NULL) {
    /* Adjust pointers */
    f_rng = &ecp_drbg_random;
#if defined(MBEDTLS2_ECP_RESTARTABLE)
    if (rs_ctx != NULL && rs_ctx->rsm != NULL)
      p_rng = &rs_ctx->rsm->drbg_ctx;
    else
#endif
      p_rng = &drbg_ctx;

    /* Initialize internal DRBG if necessary */
#if defined(MBEDTLS2_ECP_RESTARTABLE)
    if (rs_ctx == NULL || rs_ctx->rsm == NULL || rs_ctx->rsm->drbg_seeded == 0)
#endif
    {
      const size_t m_len = (grp->nbits + 7) / 8;
      MBEDTLS2_MPI_CHK(ecp_drbg_seed(p_rng, m, m_len));
    }
#if defined(MBEDTLS2_ECP_RESTARTABLE)
    if (rs_ctx != NULL && rs_ctx->rsm != NULL)
      rs_ctx->rsm->drbg_seeded = 1;
#endif
  }
#endif /* !MBEDTLS2_ECP_NO_INTERNAL_RNG */

  /* Is P the base point ? */
#if MBEDTLS2_ECP_FIXED_POINT_OPTIM == 1
  p_eq_g = (mbedtls2_mpi_cmp_mpi(&P->Y, &grp->G.Y) == 0 &&
            mbedtls2_mpi_cmp_mpi(&P->X, &grp->G.X) == 0);
#else
  p_eq_g = 0;
#endif

  /* Pick window size and deduce related sizes */
  w = ecp_pick_window_size(grp, p_eq_g);
  T_size = 1U << (w - 1);
  d = (grp->nbits + w - 1) / w;

  /* Pre-computed table: do we have it already for the base point? */
  if (p_eq_g && grp->T != NULL) {
    /* second pointer to the same table, will be deleted on exit */
    T = grp->T;
    T_ok = 1;
  } else
#if defined(MBEDTLS2_ECP_RESTARTABLE)
    /* Pre-computed table: do we have one in progress? complete? */
    if (rs_ctx != NULL && rs_ctx->rsm != NULL && rs_ctx->rsm->T != NULL) {
      /* transfer ownership of T from rsm to local function */
      T = rs_ctx->rsm->T;
      rs_ctx->rsm->T = NULL;
      rs_ctx->rsm->T_size = 0;

      /* This effectively jumps to the call to mul_comb_after_precomp() */
      T_ok = rs_ctx->rsm->state >= ecp_rsm_comb_core;
    } else
#endif
    /* Allocate table if we didn't have any */
    {
      T = mbedtls2_calloc(T_size, sizeof(mbedtls2_ecp_point));
      if (T == NULL) {
        ret = MBEDTLS2_ERR_ECP_ALLOC_FAILED;
        goto cleanup;
      }

      for (i = 0; i < T_size; i++)
        mbedtls2_ecp_point_init(&T[i]);

      T_ok = 0;
    }

  /* Compute table (or finish computing it) if not done already */
  if (!T_ok) {
    MBEDTLS2_MPI_CHK(ecp_precompute_comb(grp, T, P, w, d, rs_ctx));

    if (p_eq_g) {
      /* almost transfer ownership of T to the group, but keep a copy of
       * the pointer to use for calling the next function more easily */
      grp->T = T;
      grp->T_size = T_size;
    }
  }

  /* Actual comb multiplication using precomputed points */
  MBEDTLS2_MPI_CHK(ecp_mul_comb_after_precomp(grp, R, m, T, T_size, w, d,
                                                     f_rng, p_rng, rs_ctx));

cleanup:

#if !defined(MBEDTLS2_ECP_NO_INTERNAL_RNG)
  ecp_drbg_free(&drbg_ctx);
#endif

  /* does T belong to the group? */
  if (T == grp->T)
    T = NULL;

  /* does T belong to the restart context? */
#if defined(MBEDTLS2_ECP_RESTARTABLE)
  if (rs_ctx != NULL && rs_ctx->rsm != NULL &&
      ret == MBEDTLS2_ERR_ECP_IN_PROGRESS && T != NULL) {
    /* transfer ownership of T from local function to rsm */
    rs_ctx->rsm->T_size = T_size;
    rs_ctx->rsm->T = T;
    T = NULL;
  }
#endif

  /* did T belong to us? then let's destroy it! */
  if (T != NULL) {
    for (i = 0; i < T_size; i++)
      mbedtls2_ecp_point_free(&T[i]);
    mbedtls2_free(T);
  }

  /* don't free R while in progress in case R == P */
#if defined(MBEDTLS2_ECP_RESTARTABLE)
  if (ret != MBEDTLS2_ERR_ECP_IN_PROGRESS)
#endif
    /* prevent caller from using invalid value */
    if (ret != 0)
      mbedtls2_ecp_point_free(R);

  ECP_RS_LEAVE(rsm);

  return (ret);
}

#endif /* MBEDTLS2_ECP_SHORT_WEIERSTRASS_ENABLED */

#if defined(MBEDTLS2_ECP_MONTGOMERY_ENABLED)
/*
 * For Montgomery curves, we do all the internal arithmetic in projective
 * coordinates. Import/export of points uses only the x coordinates, which is
 * internaly represented as X / Z.
 *
 * For scalar multiplication, we'll use a Montgomery ladder.
 */

/*
 * Normalize Montgomery x/z coordinates: X = X/Z, Z = 1
 * Cost: 1M + 1I
 */
static int ecp_normalize_mxz(const mbedtls2_ecp_group *grp,
                             mbedtls2_ecp_point *P) {
#if defined(MBEDTLS2_ECP_NORMALIZE_MXZ_ALT)
  if (mbedtls2_internal_ecp_grp_capable(grp))
    return (mbedtls2_internal_ecp_normalize_mxz(grp, P));
#endif /* MBEDTLS2_ECP_NORMALIZE_MXZ_ALT */

#if defined(MBEDTLS2_ECP_NO_FALLBACK) &&                                \
    defined(MBEDTLS2_ECP_NORMALIZE_MXZ_ALT)
  return (MBEDTLS2_ERR_ECP_FEATURE_UNAVAILABLE);
#else
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_inv_mod(&P->Z, &P->Z, &grp->P));
  MBEDTLS2_MPI_CHK(
      mbedtls2_mpi_mul_mod(grp, &P->X, &P->X, &P->Z));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_lset(&P->Z, 1));

cleanup:
  return (ret);
#endif /* !defined(MBEDTLS2_ECP_NO_FALLBACK) ||                         \
          !defined(MBEDTLS2_ECP_NORMALIZE_MXZ_ALT) */
}

/*
 * Randomize projective x/z coordinates:
 * (X, Z) -> (l X, l Z) for random l
 * This is sort of the reverse operation of ecp_normalize_mxz().
 *
 * This countermeasure was first suggested in [2].
 * Cost: 2M
 */
static int ecp_randomize_mxz(const mbedtls2_ecp_group *grp,
                             mbedtls2_ecp_point *P,
                             int (*f_rng)(void *, unsigned char *, size_t),
                             void *p_rng) {
#if defined(MBEDTLS2_ECP_RANDOMIZE_MXZ_ALT)
  if (mbedtls2_internal_ecp_grp_capable(grp))
    return (mbedtls2_internal_ecp_randomize_mxz(grp, P, f_rng, p_rng));
#endif /* MBEDTLS2_ECP_RANDOMIZE_MXZ_ALT */

#if defined(MBEDTLS2_ECP_NO_FALLBACK) &&                                \
    defined(MBEDTLS2_ECP_RANDOMIZE_MXZ_ALT)
  return (MBEDTLS2_ERR_ECP_FEATURE_UNAVAILABLE);
#else
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  mbedtls2_mpi l;
  mbedtls2_mpi_init(&l);

  /* Generate l such that 1 < l < p */
  MBEDTLS2_MPI_CHK(
      mbedtls2_mpi_random(&l, 2, &grp->P, f_rng, p_rng));

  MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &P->X, &P->X, &l));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &P->Z, &P->Z, &l));

cleanup:
  mbedtls2_mpi_free(&l);

  if (ret == MBEDTLS2_ERR_MPI_NOT_ACCEPTABLE)
    ret = MBEDTLS2_ERR_ECP_RANDOM_FAILED;
  return (ret);
#endif /* !defined(MBEDTLS2_ECP_NO_FALLBACK) ||                         \
          !defined(MBEDTLS2_ECP_RANDOMIZE_MXZ_ALT) */
}

/*
 * Double-and-add: R = 2P, S = P + Q, with d = X(P - Q),
 * for Montgomery curves in x/z coordinates.
 *
 * http://www.hyperelliptic.org/EFD/g1p/auto-code/montgom/xz/ladder/mladd-1987-m.op3
 * with
 * d =  X1
 * P = (X2, Z2)
 * Q = (X3, Z3)
 * R = (X4, Z4)
 * S = (X5, Z5)
 * and eliminating temporary variables tO, ..., t4.
 *
 * Cost: 5M + 4S
 */
static int ecp_double_add_mxz(const mbedtls2_ecp_group *grp,
                              mbedtls2_ecp_point *R,
                              mbedtls2_ecp_point *S,
                              const mbedtls2_ecp_point *P,
                              const mbedtls2_ecp_point *Q,
                              const mbedtls2_mpi *d) {
#if defined(MBEDTLS2_ECP_DOUBLE_ADD_MXZ_ALT)
  if (mbedtls2_internal_ecp_grp_capable(grp))
    return (mbedtls2_internal_ecp_double_add_mxz(grp, R, S, P, Q, d));
#endif /* MBEDTLS2_ECP_DOUBLE_ADD_MXZ_ALT */

#if defined(MBEDTLS2_ECP_NO_FALLBACK) &&                                \
    defined(MBEDTLS2_ECP_DOUBLE_ADD_MXZ_ALT)
  return (MBEDTLS2_ERR_ECP_FEATURE_UNAVAILABLE);
#else
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  mbedtls2_mpi A, AA, B, BB, E, C, D, DA, CB;

  mbedtls2_mpi_init(&A);
  mbedtls2_mpi_init(&AA);
  mbedtls2_mpi_init(&B);
  mbedtls2_mpi_init(&BB);
  mbedtls2_mpi_init(&E);
  mbedtls2_mpi_init(&C);
  mbedtls2_mpi_init(&D);
  mbedtls2_mpi_init(&DA);
  mbedtls2_mpi_init(&CB);

  MBEDTLS2_MPI_CHK(mbedtls2_mpi_add_mod(grp, &A, &P->X, &P->Z));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &AA, &A, &A));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_sub_mod(grp, &B, &P->X, &P->Z));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &BB, &B, &B));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_sub_mod(grp, &E, &AA, &BB));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_add_mod(grp, &C, &Q->X, &Q->Z));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_sub_mod(grp, &D, &Q->X, &Q->Z));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &DA, &D, &A));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &CB, &C, &B));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_add_mod(grp, &S->X, &DA, &CB));
  MBEDTLS2_MPI_CHK(
      mbedtls2_mpi_mul_mod(grp, &S->X, &S->X, &S->X));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_sub_mod(grp, &S->Z, &DA, &CB));
  MBEDTLS2_MPI_CHK(
      mbedtls2_mpi_mul_mod(grp, &S->Z, &S->Z, &S->Z));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &S->Z, d, &S->Z));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &R->X, &AA, &BB));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &R->Z, &grp->A, &E));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_add_mod(grp, &R->Z, &BB, &R->Z));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &R->Z, &E, &R->Z));

cleanup:
  mbedtls2_mpi_free(&A);
  mbedtls2_mpi_free(&AA);
  mbedtls2_mpi_free(&B);
  mbedtls2_mpi_free(&BB);
  mbedtls2_mpi_free(&E);
  mbedtls2_mpi_free(&C);
  mbedtls2_mpi_free(&D);
  mbedtls2_mpi_free(&DA);
  mbedtls2_mpi_free(&CB);

  return (ret);
#endif /* !defined(MBEDTLS2_ECP_NO_FALLBACK) ||                         \
          !defined(MBEDTLS2_ECP_DOUBLE_ADD_MXZ_ALT) */
}

/*
 * Multiplication with Montgomery ladder in x/z coordinates,
 * for curves in Montgomery form
 */
static int
ecp_mul_mxz(mbedtls2_ecp_group *grp, mbedtls2_ecp_point *R,
            const mbedtls2_mpi *m, const mbedtls2_ecp_point *P,
            int (*f_rng)(void *, unsigned char *, size_t), void *p_rng) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  size_t i;
  unsigned char b;
  mbedtls2_ecp_point RP;
  mbedtls2_mpi PX;
#if !defined(MBEDTLS2_ECP_NO_INTERNAL_RNG)
  ecp_drbg_context drbg_ctx;

  ecp_drbg_init(&drbg_ctx);
#endif
  mbedtls2_ecp_point_init(&RP);
  mbedtls2_mpi_init(&PX);

#if !defined(MBEDTLS2_ECP_NO_INTERNAL_RNG)
  if (f_rng == NULL) {
    const size_t m_len = (grp->nbits + 7) / 8;
    MBEDTLS2_MPI_CHK(ecp_drbg_seed(&drbg_ctx, m, m_len));
    f_rng = &ecp_drbg_random;
    p_rng = &drbg_ctx;
  }
#endif /* !MBEDTLS2_ECP_NO_INTERNAL_RNG */

  /* Save PX and read from P before writing to R, in case P == R */
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_copy(&PX, &P->X));
  MBEDTLS2_MPI_CHK(mbedtls2_ecp_copy(&RP, P));

  /* Set R to zero in modified x/z coordinates */
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_lset(&R->X, 1));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_lset(&R->Z, 0));
  mbedtls2_mpi_free(&R->Y);

  /* RP.X might be sligtly larger than P, so reduce it */
  MOD_ADD(RP.X);

  /* Randomize coordinates of the starting point */
#if defined(MBEDTLS2_ECP_NO_INTERNAL_RNG)
  if (f_rng != NULL)
#endif
    MBEDTLS2_MPI_CHK(ecp_randomize_mxz(grp, &RP, f_rng, p_rng));

  /* Loop invariant: R = result so far, RP = R + P */
  i = mbedtls2_mpi_bitlen(
      m); /* one past the (zero-based) most significant bit */
  while (i-- > 0) {
    b = mbedtls2_mpi_get_bit(m, i);
    /*
     *  if (b) R = 2R + P else R = 2R,
     * which is:
     *  if (b) double_add( RP, R, RP, R )
     *  else   double_add( R, RP, R, RP )
     * but using safe conditional swaps to avoid leaks
     */
    MBEDTLS2_MPI_CHK(
        mbedtls2_mpi_safe_cond_swap(&R->X, &RP.X, b));
    MBEDTLS2_MPI_CHK(
        mbedtls2_mpi_safe_cond_swap(&R->Z, &RP.Z, b));
    MBEDTLS2_MPI_CHK(ecp_double_add_mxz(grp, R, &RP, R, &RP, &PX));
    MBEDTLS2_MPI_CHK(
        mbedtls2_mpi_safe_cond_swap(&R->X, &RP.X, b));
    MBEDTLS2_MPI_CHK(
        mbedtls2_mpi_safe_cond_swap(&R->Z, &RP.Z, b));
  }

  /*
   * Knowledge of the projective coordinates may leak the last few bits of the
   * scalar [1], and since our MPI implementation isn't constant-flow,
   * inversion (used for coordinate normalization) may leak the full value
   * of its input via side-channels [2].
   *
   * [1] https://eprint.iacr.org/2003/191
   * [2] https://eprint.iacr.org/2020/055
   *
   * Avoid the leak by randomizing coordinates before we normalize them.
   */
#if defined(MBEDTLS2_ECP_NO_INTERNAL_RNG)
  if (f_rng != NULL)
#endif
    MBEDTLS2_MPI_CHK(ecp_randomize_mxz(grp, R, f_rng, p_rng));

  MBEDTLS2_MPI_CHK(ecp_normalize_mxz(grp, R));

cleanup:
#if !defined(MBEDTLS2_ECP_NO_INTERNAL_RNG)
  ecp_drbg_free(&drbg_ctx);
#endif

  mbedtls2_ecp_point_free(&RP);
  mbedtls2_mpi_free(&PX);

  return (ret);
}

#endif /* MBEDTLS2_ECP_MONTGOMERY_ENABLED */

/*
 * Restartable multiplication R = m * P
 */
int mbedtls2_ecp_mul_restartable(
    mbedtls2_ecp_group *grp, mbedtls2_ecp_point *R,
    const mbedtls2_mpi *m, const mbedtls2_ecp_point *P,
    int (*f_rng)(void *, unsigned char *, size_t), void *p_rng,
    mbedtls2_ecp_restart_ctx *rs_ctx) {
  int ret = MBEDTLS2_ERR_ECP_BAD_INPUT_DATA;
#if defined(MBEDTLS2_ECP_INTERNAL_ALT)
  char is_grp_capable = 0;
#endif
  ECP_VALIDATE_RET(grp != NULL);
  ECP_VALIDATE_RET(R != NULL);
  ECP_VALIDATE_RET(m != NULL);
  ECP_VALIDATE_RET(P != NULL);

#if defined(MBEDTLS2_ECP_RESTARTABLE)
  /* reset ops count for this call if top-level */
  if (rs_ctx != NULL && rs_ctx->depth++ == 0)
    rs_ctx->ops_done = 0;
#else
  (void)rs_ctx;
#endif

#if defined(MBEDTLS2_ECP_INTERNAL_ALT)
  if ((is_grp_capable = mbedtls2_internal_ecp_grp_capable(grp)))
    MBEDTLS2_MPI_CHK(mbedtls2_internal_ecp_init(grp));
#endif /* MBEDTLS2_ECP_INTERNAL_ALT */

#if defined(MBEDTLS2_ECP_RESTARTABLE)
  /* skip argument check when restarting */
  if (rs_ctx == NULL || rs_ctx->rsm == NULL)
#endif
  {
    /* check_privkey is free */
    MBEDTLS2_ECP_BUDGET(MBEDTLS2_ECP_OPS_CHK);

    /* Common sanity checks */
    MBEDTLS2_MPI_CHK(mbedtls2_ecp_check_privkey(grp, m));
    MBEDTLS2_MPI_CHK(mbedtls2_ecp_check_pubkey(grp, P));
  }

  ret = MBEDTLS2_ERR_ECP_BAD_INPUT_DATA;
#if defined(MBEDTLS2_ECP_MONTGOMERY_ENABLED)
  if (mbedtls2_ecp_get_type(grp) == MBEDTLS2_ECP_TYPE_MONTGOMERY)
    MBEDTLS2_MPI_CHK(ecp_mul_mxz(grp, R, m, P, f_rng, p_rng));
#endif
#if defined(MBEDTLS2_ECP_SHORT_WEIERSTRASS_ENABLED)
  if (mbedtls2_ecp_get_type(grp) ==
      MBEDTLS2_ECP_TYPE_SHORT_WEIERSTRASS)
    MBEDTLS2_MPI_CHK(ecp_mul_comb(grp, R, m, P, f_rng, p_rng, rs_ctx));
#endif

cleanup:

#if defined(MBEDTLS2_ECP_INTERNAL_ALT)
  if (is_grp_capable)
    mbedtls2_internal_ecp_free(grp);
#endif /* MBEDTLS2_ECP_INTERNAL_ALT */

#if defined(MBEDTLS2_ECP_RESTARTABLE)
  if (rs_ctx != NULL)
    rs_ctx->depth--;
#endif

  return (ret);
}

/*
 * Multiplication R = m * P
 */
int mbedtls2_ecp_mul(mbedtls2_ecp_group *grp,
                            mbedtls2_ecp_point *R,
                            const mbedtls2_mpi *m,
                            const mbedtls2_ecp_point *P,
                            int (*f_rng)(void *, unsigned char *, size_t),
                            void *p_rng) {
  ECP_VALIDATE_RET(grp != NULL);
  ECP_VALIDATE_RET(R != NULL);
  ECP_VALIDATE_RET(m != NULL);
  ECP_VALIDATE_RET(P != NULL);
  return (
      mbedtls2_ecp_mul_restartable(grp, R, m, P, f_rng, p_rng, NULL));
}

#if defined(MBEDTLS2_ECP_SHORT_WEIERSTRASS_ENABLED)
/*
 * Check that an affine point is valid as a public key,
 * short weierstrass curves (SEC1 3.2.3.1)
 */
static int ecp_check_pubkey_sw(const mbedtls2_ecp_group *grp,
                               const mbedtls2_ecp_point *pt) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  mbedtls2_mpi YY, RHS;

  /* pt coordinates must be normalized for our checks */
  if (mbedtls2_mpi_cmp_int(&pt->X, 0) < 0 ||
      mbedtls2_mpi_cmp_int(&pt->Y, 0) < 0 ||
      mbedtls2_mpi_cmp_mpi(&pt->X, &grp->P) >= 0 ||
      mbedtls2_mpi_cmp_mpi(&pt->Y, &grp->P) >= 0)
    return (MBEDTLS2_ERR_ECP_INVALID_KEY);

  mbedtls2_mpi_init(&YY);
  mbedtls2_mpi_init(&RHS);

  /*
   * YY = Y^2
   * RHS = X (X^2 + A) + B = X^3 + A X + B
   */
  MBEDTLS2_MPI_CHK(
      mbedtls2_mpi_mul_mod(grp, &YY, &pt->Y, &pt->Y));
  MBEDTLS2_MPI_CHK(
      mbedtls2_mpi_mul_mod(grp, &RHS, &pt->X, &pt->X));

  /* Special case for A = -3 */
  if (grp->A.p == NULL) {
    MBEDTLS2_MPI_CHK(mbedtls2_mpi_sub_int(&RHS, &RHS, 3));
    MOD_SUB(RHS);
  } else {
    MBEDTLS2_MPI_CHK(
        mbedtls2_mpi_add_mod(grp, &RHS, &RHS, &grp->A));
  }

  MBEDTLS2_MPI_CHK(mbedtls2_mpi_mul_mod(grp, &RHS, &RHS, &pt->X));
  MBEDTLS2_MPI_CHK(
      mbedtls2_mpi_add_mod(grp, &RHS, &RHS, &grp->B));

  if (mbedtls2_mpi_cmp_mpi(&YY, &RHS) != 0)
    ret = MBEDTLS2_ERR_ECP_INVALID_KEY;

cleanup:

  mbedtls2_mpi_free(&YY);
  mbedtls2_mpi_free(&RHS);

  return (ret);
}
#endif /* MBEDTLS2_ECP_SHORT_WEIERSTRASS_ENABLED */

#if defined(MBEDTLS2_ECP_SHORT_WEIERSTRASS_ENABLED)
/*
 * R = m * P with shortcuts for m == 0, m == 1 and m == -1
 * NOT constant-time - ONLY for short Weierstrass!
 */
static int mbedtls2_ecp_mul_shortcuts(
    mbedtls2_ecp_group *grp, mbedtls2_ecp_point *R,
    const mbedtls2_mpi *m, const mbedtls2_ecp_point *P,
    mbedtls2_ecp_restart_ctx *rs_ctx) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;

  if (mbedtls2_mpi_cmp_int(m, 0) == 0) {
    MBEDTLS2_MPI_CHK(mbedtls2_ecp_set_zero(R));
  } else if (mbedtls2_mpi_cmp_int(m, 1) == 0) {
    MBEDTLS2_MPI_CHK(mbedtls2_ecp_copy(R, P));
  } else if (mbedtls2_mpi_cmp_int(m, -1) == 0) {
    MBEDTLS2_MPI_CHK(mbedtls2_ecp_copy(R, P));
    if (mbedtls2_mpi_cmp_int(&R->Y, 0) != 0)
      MBEDTLS2_MPI_CHK(
          mbedtls2_mpi_sub_mpi(&R->Y, &grp->P, &R->Y));
  } else {
    MBEDTLS2_MPI_CHK(
        mbedtls2_ecp_mul_restartable(grp, R, m, P, NULL, NULL, rs_ctx));
  }

cleanup:
  return (ret);
}

/*
 * Restartable linear combination
 * NOT constant-time
 */
int mbedtls2_ecp_muladd_restartable(
    mbedtls2_ecp_group *grp, mbedtls2_ecp_point *R,
    const mbedtls2_mpi *m, const mbedtls2_ecp_point *P,
    const mbedtls2_mpi *n, const mbedtls2_ecp_point *Q,
    mbedtls2_ecp_restart_ctx *rs_ctx) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  mbedtls2_ecp_point mP;
  mbedtls2_ecp_point *pmP = &mP;
  mbedtls2_ecp_point *pR = R;
#if defined(MBEDTLS2_ECP_INTERNAL_ALT)
  char is_grp_capable = 0;
#endif
  ECP_VALIDATE_RET(grp != NULL);
  ECP_VALIDATE_RET(R != NULL);
  ECP_VALIDATE_RET(m != NULL);
  ECP_VALIDATE_RET(P != NULL);
  ECP_VALIDATE_RET(n != NULL);
  ECP_VALIDATE_RET(Q != NULL);

  if (mbedtls2_ecp_get_type(grp) !=
      MBEDTLS2_ECP_TYPE_SHORT_WEIERSTRASS)
    return (MBEDTLS2_ERR_ECP_FEATURE_UNAVAILABLE);

  mbedtls2_ecp_point_init(&mP);

  ECP_RS_ENTER(ma);

#if defined(MBEDTLS2_ECP_RESTARTABLE)
  if (rs_ctx != NULL && rs_ctx->ma != NULL) {
    /* redirect intermediate results to restart context */
    pmP = &rs_ctx->ma->mP;
    pR = &rs_ctx->ma->R;

    /* jump to next operation */
    if (rs_ctx->ma->state == ecp_rsma_mul2)
      goto mul2;
    if (rs_ctx->ma->state == ecp_rsma_add)
      goto add;
    if (rs_ctx->ma->state == ecp_rsma_norm)
      goto norm;
  }
#endif /* MBEDTLS2_ECP_RESTARTABLE */

  MBEDTLS2_MPI_CHK(
      mbedtls2_ecp_mul_shortcuts(grp, pmP, m, P, rs_ctx));
#if defined(MBEDTLS2_ECP_RESTARTABLE)
  if (rs_ctx != NULL && rs_ctx->ma != NULL)
    rs_ctx->ma->state = ecp_rsma_mul2;

mul2:
#endif
  MBEDTLS2_MPI_CHK(
      mbedtls2_ecp_mul_shortcuts(grp, pR, n, Q, rs_ctx));

#if defined(MBEDTLS2_ECP_INTERNAL_ALT)
  if ((is_grp_capable = mbedtls2_internal_ecp_grp_capable(grp)))
    MBEDTLS2_MPI_CHK(mbedtls2_internal_ecp_init(grp));
#endif /* MBEDTLS2_ECP_INTERNAL_ALT */

#if defined(MBEDTLS2_ECP_RESTARTABLE)
  if (rs_ctx != NULL && rs_ctx->ma != NULL)
    rs_ctx->ma->state = ecp_rsma_add;

add:
#endif
  MBEDTLS2_ECP_BUDGET(MBEDTLS2_ECP_OPS_ADD);
  MBEDTLS2_MPI_CHK(ecp_add_mixed(grp, pR, pmP, pR));
#if defined(MBEDTLS2_ECP_RESTARTABLE)
  if (rs_ctx != NULL && rs_ctx->ma != NULL)
    rs_ctx->ma->state = ecp_rsma_norm;

norm:
#endif
  MBEDTLS2_ECP_BUDGET(MBEDTLS2_ECP_OPS_INV);
  MBEDTLS2_MPI_CHK(ecp_normalize_jac(grp, pR));

#if defined(MBEDTLS2_ECP_RESTARTABLE)
  if (rs_ctx != NULL && rs_ctx->ma != NULL)
    MBEDTLS2_MPI_CHK(mbedtls2_ecp_copy(R, pR));
#endif

cleanup:
#if defined(MBEDTLS2_ECP_INTERNAL_ALT)
  if (is_grp_capable)
    mbedtls2_internal_ecp_free(grp);
#endif /* MBEDTLS2_ECP_INTERNAL_ALT */

  mbedtls2_ecp_point_free(&mP);

  ECP_RS_LEAVE(ma);

  return (ret);
}

/*
 * Linear combination
 * NOT constant-time
 */
int mbedtls2_ecp_muladd(mbedtls2_ecp_group *grp,
                               mbedtls2_ecp_point *R,
                               const mbedtls2_mpi *m,
                               const mbedtls2_ecp_point *P,
                               const mbedtls2_mpi *n,
                               const mbedtls2_ecp_point *Q) {
  ECP_VALIDATE_RET(grp != NULL);
  ECP_VALIDATE_RET(R != NULL);
  ECP_VALIDATE_RET(m != NULL);
  ECP_VALIDATE_RET(P != NULL);
  ECP_VALIDATE_RET(n != NULL);
  ECP_VALIDATE_RET(Q != NULL);
  return (mbedtls2_ecp_muladd_restartable(grp, R, m, P, n, Q, NULL));
}
#endif /* MBEDTLS2_ECP_SHORT_WEIERSTRASS_ENABLED */

#if defined(MBEDTLS2_ECP_MONTGOMERY_ENABLED)
#if defined(MBEDTLS2_ECP_DP_CURVE25519_ENABLED)
#define ECP_MPI_INIT(s, n, p) {s, (n), (mbedtls2_mpi_uint *)(p)}
#define ECP_MPI_INIT_ARRAY(x)                                                  \
  ECP_MPI_INIT(1, sizeof(x) / sizeof(mbedtls2_mpi_uint), x)
/*
 * Constants for the two points other than 0, 1, -1 (mod p) in
 * https://cr.yp.to/ecdh.html#validate
 * See ecp_check_pubkey_x25519().
 */
static const mbedtls2_mpi_uint x25519_bad_point_1[] = {
    MBEDTLS2_BYTES_TO_T_UINT_8(0xe0, 0xeb, 0x7a, 0x7c, 0x3b, 0x41, 0xb8,
                                      0xae),
    MBEDTLS2_BYTES_TO_T_UINT_8(0x16, 0x56, 0xe3, 0xfa, 0xf1, 0x9f, 0xc4,
                                      0x6a),
    MBEDTLS2_BYTES_TO_T_UINT_8(0xda, 0x09, 0x8d, 0xeb, 0x9c, 0x32, 0xb1,
                                      0xfd),
    MBEDTLS2_BYTES_TO_T_UINT_8(0x86, 0x62, 0x05, 0x16, 0x5f, 0x49, 0xb8,
                                      0x00),
};
static const mbedtls2_mpi_uint x25519_bad_point_2[] = {
    MBEDTLS2_BYTES_TO_T_UINT_8(0x5f, 0x9c, 0x95, 0xbc, 0xa3, 0x50, 0x8c,
                                      0x24),
    MBEDTLS2_BYTES_TO_T_UINT_8(0xb1, 0xd0, 0xb1, 0x55, 0x9c, 0x83, 0xef,
                                      0x5b),
    MBEDTLS2_BYTES_TO_T_UINT_8(0x04, 0x44, 0x5c, 0xc4, 0x58, 0x1c, 0x8e,
                                      0x86),
    MBEDTLS2_BYTES_TO_T_UINT_8(0xd8, 0x22, 0x4e, 0xdd, 0xd0, 0x9f, 0x11,
                                      0x57),
};
static const mbedtls2_mpi ecp_x25519_bad_point_1 =
    ECP_MPI_INIT_ARRAY(x25519_bad_point_1);
static const mbedtls2_mpi ecp_x25519_bad_point_2 =
    ECP_MPI_INIT_ARRAY(x25519_bad_point_2);
#endif /* MBEDTLS2_ECP_DP_CURVE25519_ENABLED */

/*
 * Check that the input point is not one of the low-order points.
 * This is recommended by the "May the Fourth" paper:
 * https://eprint.iacr.org/2017/806.pdf
 * Those points are never sent by an honest peer.
 */
static int ecp_check_bad_points_mx(const mbedtls2_mpi *X,
                                   const mbedtls2_mpi *P,
                                   const mbedtls2_ecp_group_id grp_id) {
  int ret;
  mbedtls2_mpi XmP;

  mbedtls2_mpi_init(&XmP);

  /* Reduce X mod P so that we only need to check values less than P.
   * We know X < 2^256 so we can proceed by subtraction. */
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_copy(&XmP, X));
  while (mbedtls2_mpi_cmp_mpi(&XmP, P) >= 0)
    MBEDTLS2_MPI_CHK(mbedtls2_mpi_sub_mpi(&XmP, &XmP, P));

  /* Check against the known bad values that are less than P. For Curve448
   * these are 0, 1 and -1. For Curve25519 we check the values less than P
   * from the following list: https://cr.yp.to/ecdh.html#validate */
  if (mbedtls2_mpi_cmp_int(&XmP, 1) <= 0) /* takes care of 0 and 1 */
  {
    ret = MBEDTLS2_ERR_ECP_INVALID_KEY;
    goto cleanup;
  }

#if defined(MBEDTLS2_ECP_DP_CURVE25519_ENABLED)
  if (grp_id == MBEDTLS2_ECP_DP_CURVE25519) {
    if (mbedtls2_mpi_cmp_mpi(&XmP, &ecp_x25519_bad_point_1) == 0) {
      ret = MBEDTLS2_ERR_ECP_INVALID_KEY;
      goto cleanup;
    }

    if (mbedtls2_mpi_cmp_mpi(&XmP, &ecp_x25519_bad_point_2) == 0) {
      ret = MBEDTLS2_ERR_ECP_INVALID_KEY;
      goto cleanup;
    }
  }
#else
  (void)grp_id;
#endif

  /* Final check: check if XmP + 1 is P (final because it changes XmP!) */
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_add_int(&XmP, &XmP, 1));
  if (mbedtls2_mpi_cmp_mpi(&XmP, P) == 0) {
    ret = MBEDTLS2_ERR_ECP_INVALID_KEY;
    goto cleanup;
  }

  ret = 0;

cleanup:
  mbedtls2_mpi_free(&XmP);

  return (ret);
}

/*
 * Check validity of a public key for Montgomery curves with x-only schemes
 */
static int ecp_check_pubkey_mx(const mbedtls2_ecp_group *grp,
                               const mbedtls2_ecp_point *pt) {
  /* [Curve25519 p. 5] Just check X is the correct number of bytes */
  /* Allow any public value, if it's too big then we'll just reduce it mod p
   * (RFC 7748 sec. 5 para. 3). */
  if (mbedtls2_mpi_size(&pt->X) > (grp->nbits + 7) / 8)
    return (MBEDTLS2_ERR_ECP_INVALID_KEY);

  /* Implicit in all standards (as they don't consider negative numbers):
   * X must be non-negative. This is normally ensured by the way it's
   * encoded for transmission, but let's be extra sure. */
  if (mbedtls2_mpi_cmp_int(&pt->X, 0) < 0)
    return (MBEDTLS2_ERR_ECP_INVALID_KEY);

  return (ecp_check_bad_points_mx(&pt->X, &grp->P, grp->id));
}
#endif /* MBEDTLS2_ECP_MONTGOMERY_ENABLED */

/*
 * Check that a point is valid as a public key
 */
int mbedtls2_ecp_check_pubkey(const mbedtls2_ecp_group *grp,
                                     const mbedtls2_ecp_point *pt) {
  ECP_VALIDATE_RET(grp != NULL);
  ECP_VALIDATE_RET(pt != NULL);

  /* Must use affine coordinates */
  if (mbedtls2_mpi_cmp_int(&pt->Z, 1) != 0)
    return (MBEDTLS2_ERR_ECP_INVALID_KEY);

#if defined(MBEDTLS2_ECP_MONTGOMERY_ENABLED)
  if (mbedtls2_ecp_get_type(grp) == MBEDTLS2_ECP_TYPE_MONTGOMERY)
    return (ecp_check_pubkey_mx(grp, pt));
#endif
#if defined(MBEDTLS2_ECP_SHORT_WEIERSTRASS_ENABLED)
  if (mbedtls2_ecp_get_type(grp) ==
      MBEDTLS2_ECP_TYPE_SHORT_WEIERSTRASS)
    return (ecp_check_pubkey_sw(grp, pt));
#endif
  return (MBEDTLS2_ERR_ECP_BAD_INPUT_DATA);
}

/*
 * Check that an mbedtls2_mpi is valid as a private key
 */
int mbedtls2_ecp_check_privkey(const mbedtls2_ecp_group *grp,
                                      const mbedtls2_mpi *d) {
  ECP_VALIDATE_RET(grp != NULL);
  ECP_VALIDATE_RET(d != NULL);

#if defined(MBEDTLS2_ECP_MONTGOMERY_ENABLED)
  if (mbedtls2_ecp_get_type(grp) ==
      MBEDTLS2_ECP_TYPE_MONTGOMERY) {
    /* see RFC 7748 sec. 5 para. 5 */
    if (mbedtls2_mpi_get_bit(d, 0) != 0 ||
        mbedtls2_mpi_get_bit(d, 1) != 0 ||
        mbedtls2_mpi_bitlen(d) - 1 !=
            grp->nbits) /* mbedtls2_mpi_bitlen is one-based! */
      return (MBEDTLS2_ERR_ECP_INVALID_KEY);

    /* see [Curve25519] page 5 */
    if (grp->nbits == 254 && mbedtls2_mpi_get_bit(d, 2) != 0)
      return (MBEDTLS2_ERR_ECP_INVALID_KEY);

    return (0);
  }
#endif /* MBEDTLS2_ECP_MONTGOMERY_ENABLED */
#if defined(MBEDTLS2_ECP_SHORT_WEIERSTRASS_ENABLED)
  if (mbedtls2_ecp_get_type(grp) ==
      MBEDTLS2_ECP_TYPE_SHORT_WEIERSTRASS) {
    /* see SEC1 3.2 */
    if (mbedtls2_mpi_cmp_int(d, 1) < 0 ||
        mbedtls2_mpi_cmp_mpi(d, &grp->N) >= 0)
      return (MBEDTLS2_ERR_ECP_INVALID_KEY);
    else
      return (0);
  }
#endif /* MBEDTLS2_ECP_SHORT_WEIERSTRASS_ENABLED */

  return (MBEDTLS2_ERR_ECP_BAD_INPUT_DATA);
}

#if defined(MBEDTLS2_ECP_MONTGOMERY_ENABLED)
MBEDTLS2_STATIC_TESTABLE
int mbedtls2_ecp_gen_privkey_mx(size_t high_bit, mbedtls2_mpi *d,
                                       int (*f_rng)(void *, unsigned char *,
                                                    size_t),
                                       void *p_rng) {
  int ret = MBEDTLS2_ERR_ECP_BAD_INPUT_DATA;
  size_t n_random_bytes = high_bit / 8 + 1;

  /* [Curve25519] page 5 */
  /* Generate a (high_bit+1)-bit random number by generating just enough
   * random bytes, then shifting out extra bits from the top (necessary
   * when (high_bit+1) is not a multiple of 8). */
  MBEDTLS2_MPI_CHK(
      mbedtls2_mpi_fill_random(d, n_random_bytes, f_rng, p_rng));
  MBEDTLS2_MPI_CHK(
      mbedtls2_mpi_shift_r(d, 8 * n_random_bytes - high_bit - 1));

  MBEDTLS2_MPI_CHK(mbedtls2_mpi_set_bit(d, high_bit, 1));

  /* Make sure the last two bits are unset for Curve448, three bits for
     Curve25519 */
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_set_bit(d, 0, 0));
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_set_bit(d, 1, 0));
  if (high_bit == 254) {
    MBEDTLS2_MPI_CHK(mbedtls2_mpi_set_bit(d, 2, 0));
  }

cleanup:
  return (ret);
}
#endif /* MBEDTLS2_ECP_MONTGOMERY_ENABLED */

#if defined(MBEDTLS2_ECP_SHORT_WEIERSTRASS_ENABLED)
static int mbedtls2_ecp_gen_privkey_sw(
    const mbedtls2_mpi *N, mbedtls2_mpi *d,
    int (*f_rng)(void *, unsigned char *, size_t), void *p_rng) {
  int ret = mbedtls2_mpi_random(d, 1, N, f_rng, p_rng);
  switch (ret) {
  case MBEDTLS2_ERR_MPI_NOT_ACCEPTABLE:
    return (MBEDTLS2_ERR_ECP_RANDOM_FAILED);
  default:
    return (ret);
  }
}
#endif /* MBEDTLS2_ECP_SHORT_WEIERSTRASS_ENABLED */

/*
 * Generate a private key
 */
int mbedtls2_ecp_gen_privkey(
    const mbedtls2_ecp_group *grp, mbedtls2_mpi *d,
    int (*f_rng)(void *, unsigned char *, size_t), void *p_rng) {
  ECP_VALIDATE_RET(grp != NULL);
  ECP_VALIDATE_RET(d != NULL);
  ECP_VALIDATE_RET(f_rng != NULL);

#if defined(MBEDTLS2_ECP_MONTGOMERY_ENABLED)
  if (mbedtls2_ecp_get_type(grp) == MBEDTLS2_ECP_TYPE_MONTGOMERY)
    return (mbedtls2_ecp_gen_privkey_mx(grp->nbits, d, f_rng, p_rng));
#endif /* MBEDTLS2_ECP_MONTGOMERY_ENABLED */

#if defined(MBEDTLS2_ECP_SHORT_WEIERSTRASS_ENABLED)
  if (mbedtls2_ecp_get_type(grp) ==
      MBEDTLS2_ECP_TYPE_SHORT_WEIERSTRASS)
    return (mbedtls2_ecp_gen_privkey_sw(&grp->N, d, f_rng, p_rng));
#endif /* MBEDTLS2_ECP_SHORT_WEIERSTRASS_ENABLED */

  return (MBEDTLS2_ERR_ECP_BAD_INPUT_DATA);
}

/*
 * Generate a keypair with configurable base point
 */
int mbedtls2_ecp_gen_keypair_base(
    mbedtls2_ecp_group *grp, const mbedtls2_ecp_point *G,
    mbedtls2_mpi *d, mbedtls2_ecp_point *Q,
    int (*f_rng)(void *, unsigned char *, size_t), void *p_rng) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  ECP_VALIDATE_RET(grp != NULL);
  ECP_VALIDATE_RET(d != NULL);
  ECP_VALIDATE_RET(G != NULL);
  ECP_VALIDATE_RET(Q != NULL);
  ECP_VALIDATE_RET(f_rng != NULL);

  MBEDTLS2_MPI_CHK(
      mbedtls2_ecp_gen_privkey(grp, d, f_rng, p_rng));
  MBEDTLS2_MPI_CHK(mbedtls2_ecp_mul(grp, Q, d, G, f_rng, p_rng));

cleanup:
  return (ret);
}

/*
 * Generate key pair, wrapper for conventional base point
 */
int mbedtls2_ecp_gen_keypair(
    mbedtls2_ecp_group *grp, mbedtls2_mpi *d,
    mbedtls2_ecp_point *Q, int (*f_rng)(void *, unsigned char *, size_t),
    void *p_rng) {
  ECP_VALIDATE_RET(grp != NULL);
  ECP_VALIDATE_RET(d != NULL);
  ECP_VALIDATE_RET(Q != NULL);
  ECP_VALIDATE_RET(f_rng != NULL);

  return (
      mbedtls2_ecp_gen_keypair_base(grp, &grp->G, d, Q, f_rng, p_rng));
}

/*
 * Generate a keypair, prettier wrapper
 */
int mbedtls2_ecp_gen_key(mbedtls2_ecp_group_id grp_id,
                                mbedtls2_ecp_keypair *key,
                                int (*f_rng)(void *, unsigned char *, size_t),
                                void *p_rng) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  ECP_VALIDATE_RET(key != NULL);
  ECP_VALIDATE_RET(f_rng != NULL);

  if ((ret = mbedtls2_ecp_group_load(&key->grp, grp_id)) != 0)
    return (ret);

  return (mbedtls2_ecp_gen_keypair(&key->grp, &key->d, &key->Q, f_rng,
                                          p_rng));
}

#define ECP_CURVE25519_KEY_SIZE 32
/*
 * Read a private key.
 */
int mbedtls2_ecp_read_key(mbedtls2_ecp_group_id grp_id,
                                 mbedtls2_ecp_keypair *key,
                                 const unsigned char *buf, size_t buflen) {
  int ret = 0;

  ECP_VALIDATE_RET(key != NULL);
  ECP_VALIDATE_RET(buf != NULL);

  if ((ret = mbedtls2_ecp_group_load(&key->grp, grp_id)) != 0)
    return (ret);

  ret = MBEDTLS2_ERR_ECP_FEATURE_UNAVAILABLE;

#if defined(MBEDTLS2_ECP_MONTGOMERY_ENABLED)
  if (mbedtls2_ecp_get_type(&key->grp) ==
      MBEDTLS2_ECP_TYPE_MONTGOMERY) {
    /*
     * If it is Curve25519 curve then mask the key as mandated by RFC7748
     */
    if (grp_id == MBEDTLS2_ECP_DP_CURVE25519) {
      if (buflen != ECP_CURVE25519_KEY_SIZE)
        return MBEDTLS2_ERR_ECP_INVALID_KEY;

      MBEDTLS2_MPI_CHK(
          mbedtls2_mpi_read_binary_le(&key->d, buf, buflen));

      /* Set the three least significant bits to 0 */
      MBEDTLS2_MPI_CHK(mbedtls2_mpi_set_bit(&key->d, 0, 0));
      MBEDTLS2_MPI_CHK(mbedtls2_mpi_set_bit(&key->d, 1, 0));
      MBEDTLS2_MPI_CHK(mbedtls2_mpi_set_bit(&key->d, 2, 0));

      /* Set the most significant bit to 0 */
      MBEDTLS2_MPI_CHK(mbedtls2_mpi_set_bit(
          &key->d, ECP_CURVE25519_KEY_SIZE * 8 - 1, 0));

      /* Set the second most significant bit to 1 */
      MBEDTLS2_MPI_CHK(mbedtls2_mpi_set_bit(
          &key->d, ECP_CURVE25519_KEY_SIZE * 8 - 2, 1));
    } else
      ret = MBEDTLS2_ERR_ECP_FEATURE_UNAVAILABLE;
  }

#endif
#if defined(MBEDTLS2_ECP_SHORT_WEIERSTRASS_ENABLED)
  if (mbedtls2_ecp_get_type(&key->grp) ==
      MBEDTLS2_ECP_TYPE_SHORT_WEIERSTRASS) {
    MBEDTLS2_MPI_CHK(
        mbedtls2_mpi_read_binary(&key->d, buf, buflen));

    MBEDTLS2_MPI_CHK(
        mbedtls2_ecp_check_privkey(&key->grp, &key->d));
  }

#endif
cleanup:

  if (ret != 0)
    mbedtls2_mpi_free(&key->d);

  return (ret);
}

/*
 * Write a private key.
 */
int mbedtls2_ecp_write_key(mbedtls2_ecp_keypair *key,
                                  unsigned char *buf, size_t buflen) {
  int ret = MBEDTLS2_ERR_ECP_FEATURE_UNAVAILABLE;

  ECP_VALIDATE_RET(key != NULL);
  ECP_VALIDATE_RET(buf != NULL);

#if defined(MBEDTLS2_ECP_MONTGOMERY_ENABLED)
  if (mbedtls2_ecp_get_type(&key->grp) ==
      MBEDTLS2_ECP_TYPE_MONTGOMERY) {
    if (key->grp.id == MBEDTLS2_ECP_DP_CURVE25519) {
      if (buflen < ECP_CURVE25519_KEY_SIZE)
        return MBEDTLS2_ERR_ECP_BUFFER_TOO_SMALL;

      MBEDTLS2_MPI_CHK(
          mbedtls2_mpi_write_binary_le(&key->d, buf, buflen));
    } else
      ret = MBEDTLS2_ERR_ECP_FEATURE_UNAVAILABLE;
  }

#endif
#if defined(MBEDTLS2_ECP_SHORT_WEIERSTRASS_ENABLED)
  if (mbedtls2_ecp_get_type(&key->grp) ==
      MBEDTLS2_ECP_TYPE_SHORT_WEIERSTRASS) {
    MBEDTLS2_MPI_CHK(
        mbedtls2_mpi_write_binary(&key->d, buf, buflen));
  }

#endif
cleanup:

  return (ret);
}

/*
 * Check a public-private key pair
 */
int mbedtls2_ecp_check_pub_priv(const mbedtls2_ecp_keypair *pub,
                                       const mbedtls2_ecp_keypair *prv) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  mbedtls2_ecp_point Q;
  mbedtls2_ecp_group grp;
  ECP_VALIDATE_RET(pub != NULL);
  ECP_VALIDATE_RET(prv != NULL);

  if (pub->grp.id == MBEDTLS2_ECP_DP_NONE ||
      pub->grp.id != prv->grp.id ||
      mbedtls2_mpi_cmp_mpi(&pub->Q.X, &prv->Q.X) ||
      mbedtls2_mpi_cmp_mpi(&pub->Q.Y, &prv->Q.Y) ||
      mbedtls2_mpi_cmp_mpi(&pub->Q.Z, &prv->Q.Z)) {
    return (MBEDTLS2_ERR_ECP_BAD_INPUT_DATA);
  }

  mbedtls2_ecp_point_init(&Q);
  mbedtls2_ecp_group_init(&grp);

  /* mbedtls2_ecp_mul() needs a non-const group... */
  mbedtls2_ecp_group_copy(&grp, &prv->grp);

  /* Also checks d is valid */
  MBEDTLS2_MPI_CHK(
      mbedtls2_ecp_mul(&grp, &Q, &prv->d, &prv->grp.G, NULL, NULL));

  if (mbedtls2_mpi_cmp_mpi(&Q.X, &prv->Q.X) ||
      mbedtls2_mpi_cmp_mpi(&Q.Y, &prv->Q.Y) ||
      mbedtls2_mpi_cmp_mpi(&Q.Z, &prv->Q.Z)) {
    ret = MBEDTLS2_ERR_ECP_BAD_INPUT_DATA;
    goto cleanup;
  }

cleanup:
  mbedtls2_ecp_point_free(&Q);
  mbedtls2_ecp_group_free(&grp);

  return (ret);
}

#if defined(MBEDTLS2_SELF_TEST)

/* Adjust the exponent to be a valid private point for the specified curve.
 * This is sometimes necessary because we use a single set of exponents
 * for all curves but the validity of values depends on the curve. */
static int self_test_adjust_exponent(const mbedtls2_ecp_group *grp,
                                     mbedtls2_mpi *m) {
  int ret = 0;
  switch (grp->id) {
    /* If Curve25519 is available, then that's what we use for the
     * Montgomery test, so we don't need the adjustment code. */
#if !defined(MBEDTLS2_ECP_DP_CURVE25519_ENABLED)
#if defined(MBEDTLS2_ECP_DP_CURVE448_ENABLED)
  case MBEDTLS2_ECP_DP_CURVE448:
    /* Move highest bit from 254 to N-1. Setting bit N-1 is
     * necessary to enforce the highest-bit-set constraint. */
    MBEDTLS2_MPI_CHK(mbedtls2_mpi_set_bit(m, 254, 0));
    MBEDTLS2_MPI_CHK(mbedtls2_mpi_set_bit(m, grp->nbits, 1));
    /* Copy second-highest bit from 253 to N-2. This is not
     * necessary but improves the test variety a bit. */
    MBEDTLS2_MPI_CHK(mbedtls2_mpi_set_bit(
        m, grp->nbits - 1, mbedtls2_mpi_get_bit(m, 253)));
    break;
#endif
#endif /* ! defined(MBEDTLS2_ECP_DP_CURVE25519_ENABLED) */
  default:
    /* Non-Montgomery curves and Curve25519 need no adjustment. */
    (void)grp;
    (void)m;
    goto cleanup;
  }
cleanup:
  return (ret);
}

/* Calculate R = m.P for each m in exponents. Check that the number of
 * basic operations doesn't depend on the value of m. */
static int self_test_point(int verbose, mbedtls2_ecp_group *grp,
                           mbedtls2_ecp_point *R, mbedtls2_mpi *m,
                           const mbedtls2_ecp_point *P,
                           const char *const *exponents, size_t n_exponents) {
  int ret = 0;
  size_t i = 0;
  unsigned long add_c_prev, dbl_c_prev, mul_c_prev;
  add_count = 0;
  dbl_count = 0;
  mul_count = 0;

  MBEDTLS2_MPI_CHK(mbedtls2_mpi_read_string(m, 16, exponents[0]));
  MBEDTLS2_MPI_CHK(self_test_adjust_exponent(grp, m));
  MBEDTLS2_MPI_CHK(mbedtls2_ecp_mul(grp, R, m, P, NULL, NULL));

  for (i = 1; i < n_exponents; i++) {
    add_c_prev = add_count;
    dbl_c_prev = dbl_count;
    mul_c_prev = mul_count;
    add_count = 0;
    dbl_count = 0;
    mul_count = 0;

    MBEDTLS2_MPI_CHK(
        mbedtls2_mpi_read_string(m, 16, exponents[i]));
    MBEDTLS2_MPI_CHK(self_test_adjust_exponent(grp, m));
    MBEDTLS2_MPI_CHK(mbedtls2_ecp_mul(grp, R, m, P, NULL, NULL));

    if (add_count != add_c_prev || dbl_count != dbl_c_prev ||
        mul_count != mul_c_prev) {
      ret = 1;
      break;
    }
  }

cleanup:
  if (verbose != 0) {
    if (ret != 0)
      mbedtls2_printf("failed (%u)\n", (unsigned int)i);
    else
      mbedtls2_printf("passed\n");
  }
  return (ret);
}

/*
 * Checkup routine
 */
int mbedtls2_ecp_self_test(int verbose) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  mbedtls2_ecp_group grp;
  mbedtls2_ecp_point R, P;
  mbedtls2_mpi m;

#if defined(MBEDTLS2_ECP_SHORT_WEIERSTRASS_ENABLED)
  /* Exponents especially adapted for secp192k1, which has the lowest
   * order n of all supported curves (secp192r1 is in a slightly larger
   * field but the order of its base point is slightly smaller). */
  const char *sw_exponents[] = {
      "000000000000000000000000000000000000000000000001", /* one */
      "FFFFFFFFFFFFFFFFFFFFFFFE26F2FC170F69466A74DEFD8C", /* n - 1 */
      "5EA6F389A38B8BC81E767753B15AA5569E1782E30ABE7D25", /* random */
      "400000000000000000000000000000000000000000000000", /* one and zeros */
      "7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", /* all ones */
      "555555555555555555555555555555555555555555555555", /* 101010... */
  };
#endif /* MBEDTLS2_ECP_SHORT_WEIERSTRASS_ENABLED */
#if defined(MBEDTLS2_ECP_MONTGOMERY_ENABLED)
  const char *m_exponents[] = {
      /* Valid private values for Curve25519. In a build with Curve448
       * but not Curve25519, they will be adjusted in
       * self_test_adjust_exponent(). */
      "4000000000000000000000000000000000000000000000000000000000000000",
      "5C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C30",
      "5715ECCE24583F7A7023C24164390586842E816D7280A49EF6DF4EAE6B280BF8",
      "41A2B017516F6D254E1F002BCCBADD54BE30F8CEC737A0E912B4963B6BA74460",
      "5555555555555555555555555555555555555555555555555555555555555550",
      "7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8",
  };
#endif /* MBEDTLS2_ECP_MONTGOMERY_ENABLED */

  mbedtls2_ecp_group_init(&grp);
  mbedtls2_ecp_point_init(&R);
  mbedtls2_ecp_point_init(&P);
  mbedtls2_mpi_init(&m);

#if defined(MBEDTLS2_ECP_SHORT_WEIERSTRASS_ENABLED)
  /* Use secp192r1 if available, or any available curve */
#if defined(MBEDTLS2_ECP_DP_SECP192R1_ENABLED)
  MBEDTLS2_MPI_CHK(
      mbedtls2_ecp_group_load(&grp, MBEDTLS2_ECP_DP_SECP192R1));
#else
  MBEDTLS2_MPI_CHK(mbedtls2_ecp_group_load(
      &grp, mbedtls2_ecp_curve_list()->grp_id));
#endif

  if (verbose != 0)
    mbedtls2_printf(
        "  ECP SW test #1 (constant op_count, base point G): ");
  /* Do a dummy multiplication first to trigger precomputation */
  MBEDTLS2_MPI_CHK(mbedtls2_mpi_lset(&m, 2));
  MBEDTLS2_MPI_CHK(
      mbedtls2_ecp_mul(&grp, &P, &m, &grp.G, NULL, NULL));
  ret = self_test_point(verbose, &grp, &R, &m, &grp.G, sw_exponents,
                        sizeof(sw_exponents) / sizeof(sw_exponents[0]));
  if (ret != 0)
    goto cleanup;

  if (verbose != 0)
    mbedtls2_printf(
        "  ECP SW test #2 (constant op_count, other point): ");
  /* We computed P = 2G last time, use it */
  ret = self_test_point(verbose, &grp, &R, &m, &P, sw_exponents,
                        sizeof(sw_exponents) / sizeof(sw_exponents[0]));
  if (ret != 0)
    goto cleanup;

  mbedtls2_ecp_group_free(&grp);
  mbedtls2_ecp_point_free(&R);
#endif /* MBEDTLS2_ECP_SHORT_WEIERSTRASS_ENABLED */

#if defined(MBEDTLS2_ECP_MONTGOMERY_ENABLED)
  if (verbose != 0)
    mbedtls2_printf("  ECP Montgomery test (constant op_count): ");
#if defined(MBEDTLS2_ECP_DP_CURVE25519_ENABLED)
  MBEDTLS2_MPI_CHK(
      mbedtls2_ecp_group_load(&grp, MBEDTLS2_ECP_DP_CURVE25519));
#elif defined(MBEDTLS2_ECP_DP_CURVE448_ENABLED)
  MBEDTLS2_MPI_CHK(
      mbedtls2_ecp_group_load(&grp, MBEDTLS2_ECP_DP_CURVE448));
#else
#error                                                                         \
    "MBEDTLS2_ECP_MONTGOMERY_ENABLED is defined, but no curve is supported for self-test"
#endif
  ret = self_test_point(verbose, &grp, &R, &m, &grp.G, m_exponents,
                        sizeof(m_exponents) / sizeof(m_exponents[0]));
  if (ret != 0)
    goto cleanup;
#endif /* MBEDTLS2_ECP_MONTGOMERY_ENABLED */

cleanup:

  if (ret < 0 && verbose != 0)
    mbedtls2_printf("Unexpected error, return code = %08X\n",
                           (unsigned int)ret);

  mbedtls2_ecp_group_free(&grp);
  mbedtls2_ecp_point_free(&R);
  mbedtls2_ecp_point_free(&P);
  mbedtls2_mpi_free(&m);

  if (verbose != 0)
    mbedtls2_printf("\n");

  return (ret);
}

#endif /* MBEDTLS2_SELF_TEST */

#endif /* !MBEDTLS2_ECP_ALT */

#endif /* MBEDTLS2_ECP_C */

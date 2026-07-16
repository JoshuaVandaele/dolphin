/*
 *  Entropy accumulator implementation
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

#if defined(MBEDTLS2_ENTROPY_C)

#if defined(MBEDTLS2_TEST_NULL_ENTROPY)
#warning "**** WARNING!  MBEDTLS2_TEST_NULL_ENTROPY defined! "
#warning "**** THIS BUILD HAS NO DEFINED ENTROPY SOURCES "
#warning "**** THIS BUILD IS *NOT* SUITABLE FOR PRODUCTION USE "
#endif

#include "mbedtls2/entropy.h"
#include "mbedtls2/entropy_poll.h"
#include "mbedtls2/error.h"
#include "mbedtls2/platform_util.h"

#include <string.h>

#if defined(MBEDTLS2_FS_IO)
#include <stdio.h>
#endif

#if defined(MBEDTLS2_ENTROPY_NV_SEED)
#include "mbedtls2/platform.h"
#endif

#if defined(MBEDTLS2_SELF_TEST)
#if defined(MBEDTLS2_PLATFORM_C)
#include "mbedtls2/platform.h"
#else
#include <stdio.h>
#define mbedtls2_printf printf
#endif /* MBEDTLS2_PLATFORM_C */
#endif /* MBEDTLS2_SELF_TEST */

#if defined(MBEDTLS2_HAVEGE_C)
#include "mbedtls2/havege.h"
#endif

#define ENTROPY_MAX_LOOP 256 /**< Maximum amount to loop before error */

void mbedtls2_entropy_init(mbedtls2_entropy_context *ctx) {
  ctx->source_count = 0;
  memset(ctx->source, 0, sizeof(ctx->source));

#if defined(MBEDTLS2_THREADING_C)
  mbedtls2_mutex_init(&ctx->mutex);
#endif

  ctx->accumulator_started = 0;
#if defined(MBEDTLS2_ENTROPY_SHA512_ACCUMULATOR)
  mbedtls2_sha512_init(&ctx->accumulator);
#else
  mbedtls2_sha256_init(&ctx->accumulator);
#endif
#if defined(MBEDTLS2_HAVEGE_C)
  mbedtls2_havege_init(&ctx->havege_data);
#endif

  /* Reminder: Update ENTROPY_HAVE_STRONG in the test files
   *           when adding more strong entropy sources here. */

#if defined(MBEDTLS2_TEST_NULL_ENTROPY)
  mbedtls2_entropy_add_source(ctx, mbedtls2_null_entropy_poll,
                                     NULL, 1,
                                     MBEDTLS2_ENTROPY_SOURCE_STRONG);
#endif

#if !defined(MBEDTLS2_NO_DEFAULT_ENTROPY_SOURCES)
#if !defined(MBEDTLS2_NO_PLATFORM_ENTROPY)
  mbedtls2_entropy_add_source(ctx, mbedtls2_platform_entropy_poll,
                                     NULL, MBEDTLS2_ENTROPY_MIN_PLATFORM,
                                     MBEDTLS2_ENTROPY_SOURCE_STRONG);
#endif
#if defined(MBEDTLS2_TIMING_C)
  mbedtls2_entropy_add_source(ctx, mbedtls2_hardclock_poll, NULL,
                                     MBEDTLS2_ENTROPY_MIN_HARDCLOCK,
                                     MBEDTLS2_ENTROPY_SOURCE_WEAK);
#endif
#if defined(MBEDTLS2_HAVEGE_C)
  mbedtls2_entropy_add_source(ctx, mbedtls2_havege_poll,
                                     &ctx->havege_data,
                                     MBEDTLS2_ENTROPY_MIN_HAVEGE,
                                     MBEDTLS2_ENTROPY_SOURCE_STRONG);
#endif
#if defined(MBEDTLS2_ENTROPY_HARDWARE_ALT)
  mbedtls2_entropy_add_source(ctx, mbedtls2_hardware_poll, NULL,
                                     MBEDTLS2_ENTROPY_MIN_HARDWARE,
                                     MBEDTLS2_ENTROPY_SOURCE_STRONG);
#endif
#if defined(MBEDTLS2_ENTROPY_NV_SEED)
  mbedtls2_entropy_add_source(ctx, mbedtls2_nv_seed_poll, NULL,
                                     MBEDTLS2_ENTROPY_BLOCK_SIZE,
                                     MBEDTLS2_ENTROPY_SOURCE_STRONG);
  ctx->initial_entropy_run = 0;
#endif
#endif /* MBEDTLS2_NO_DEFAULT_ENTROPY_SOURCES */
}

void mbedtls2_entropy_free(mbedtls2_entropy_context *ctx) {
  /* If the context was already free, don't call free() again.
   * This is important for mutexes which don't allow double-free. */
  if (ctx->accumulator_started == -1)
    return;

#if defined(MBEDTLS2_HAVEGE_C)
  mbedtls2_havege_free(&ctx->havege_data);
#endif
#if defined(MBEDTLS2_THREADING_C)
  mbedtls2_mutex_free(&ctx->mutex);
#endif
#if defined(MBEDTLS2_ENTROPY_SHA512_ACCUMULATOR)
  mbedtls2_sha512_free(&ctx->accumulator);
#else
  mbedtls2_sha256_free(&ctx->accumulator);
#endif
#if defined(MBEDTLS2_ENTROPY_NV_SEED)
  ctx->initial_entropy_run = 0;
#endif
  ctx->source_count = 0;
  mbedtls2_platform_zeroize(ctx->source, sizeof(ctx->source));
  ctx->accumulator_started = -1;
}

int mbedtls2_entropy_add_source(
    mbedtls2_entropy_context *ctx,
    mbedtls2_entropy_f_source_ptr f_source, void *p_source,
    size_t threshold, int strong) {
  int idx, ret = 0;

#if defined(MBEDTLS2_THREADING_C)
  if ((ret = mbedtls2_mutex_lock(&ctx->mutex)) != 0)
    return (ret);
#endif

  idx = ctx->source_count;
  if (idx >= MBEDTLS2_ENTROPY_MAX_SOURCES) {
    ret = MBEDTLS2_ERR_ENTROPY_MAX_SOURCES;
    goto exit;
  }

  ctx->source[idx].f_source = f_source;
  ctx->source[idx].p_source = p_source;
  ctx->source[idx].threshold = threshold;
  ctx->source[idx].strong = strong;

  ctx->source_count++;

exit:
#if defined(MBEDTLS2_THREADING_C)
  if (mbedtls2_mutex_unlock(&ctx->mutex) != 0)
    return (MBEDTLS2_ERR_THREADING_MUTEX_ERROR);
#endif

  return (ret);
}

/*
 * Entropy accumulator update
 */
static int entropy_update(mbedtls2_entropy_context *ctx,
                          unsigned char source_id, const unsigned char *data,
                          size_t len) {
  unsigned char header[2];
  unsigned char tmp[MBEDTLS2_ENTROPY_BLOCK_SIZE];
  size_t use_len = len;
  const unsigned char *p = data;
  int ret = 0;

  if (use_len > MBEDTLS2_ENTROPY_BLOCK_SIZE) {
#if defined(MBEDTLS2_ENTROPY_SHA512_ACCUMULATOR)
    if ((ret = mbedtls2_sha512_ret(data, len, tmp, 0)) != 0)
      goto cleanup;
#else
    if ((ret = mbedtls2_sha256_ret(data, len, tmp, 0)) != 0)
      goto cleanup;
#endif
    p = tmp;
    use_len = MBEDTLS2_ENTROPY_BLOCK_SIZE;
  }

  header[0] = source_id;
  header[1] = use_len & 0xFF;

  /*
   * Start the accumulator if this has not already happened. Note that
   * it is sufficient to start the accumulator here only because all calls to
   * gather entropy eventually execute this code.
   */
#if defined(MBEDTLS2_ENTROPY_SHA512_ACCUMULATOR)
  if (ctx->accumulator_started == 0 &&
      (ret = mbedtls2_sha512_starts_ret(&ctx->accumulator, 0)) != 0)
    goto cleanup;
  else
    ctx->accumulator_started = 1;
  if ((ret = mbedtls2_sha512_update_ret(&ctx->accumulator, header, 2)) !=
      0)
    goto cleanup;
  ret = mbedtls2_sha512_update_ret(&ctx->accumulator, p, use_len);
#else
  if (ctx->accumulator_started == 0 &&
      (ret = mbedtls2_sha256_starts_ret(&ctx->accumulator, 0)) != 0)
    goto cleanup;
  else
    ctx->accumulator_started = 1;
  if ((ret = mbedtls2_sha256_update_ret(&ctx->accumulator, header, 2)) !=
      0)
    goto cleanup;
  ret = mbedtls2_sha256_update_ret(&ctx->accumulator, p, use_len);
#endif

cleanup:
  mbedtls2_platform_zeroize(tmp, sizeof(tmp));

  return (ret);
}

int mbedtls2_entropy_update_manual(mbedtls2_entropy_context *ctx,
                                          const unsigned char *data,
                                          size_t len) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;

#if defined(MBEDTLS2_THREADING_C)
  if ((ret = mbedtls2_mutex_lock(&ctx->mutex)) != 0)
    return (ret);
#endif

  ret = entropy_update(ctx, MBEDTLS2_ENTROPY_SOURCE_MANUAL, data, len);

#if defined(MBEDTLS2_THREADING_C)
  if (mbedtls2_mutex_unlock(&ctx->mutex) != 0)
    return (MBEDTLS2_ERR_THREADING_MUTEX_ERROR);
#endif

  return (ret);
}

/*
 * Run through the different sources to add entropy to our accumulator
 */
static int entropy_gather_internal(mbedtls2_entropy_context *ctx) {
  int ret = MBEDTLS2_ERR_ENTROPY_SOURCE_FAILED;
  int i;
  int have_one_strong = 0;
  unsigned char buf[MBEDTLS2_ENTROPY_MAX_GATHER];
  size_t olen;

  if (ctx->source_count == 0)
    return (MBEDTLS2_ERR_ENTROPY_NO_SOURCES_DEFINED);

  /*
   * Run through our entropy sources
   */
  for (i = 0; i < ctx->source_count; i++) {
    if (ctx->source[i].strong == MBEDTLS2_ENTROPY_SOURCE_STRONG)
      have_one_strong = 1;

    olen = 0;
    if ((ret = ctx->source[i].f_source(ctx->source[i].p_source, buf,
                                       MBEDTLS2_ENTROPY_MAX_GATHER,
                                       &olen)) != 0) {
      goto cleanup;
    }

    /*
     * Add if we actually gathered something
     */
    if (olen > 0) {
      if ((ret = entropy_update(ctx, (unsigned char)i, buf, olen)) != 0)
        return (ret);
      ctx->source[i].size += olen;
    }
  }

  if (have_one_strong == 0)
    ret = MBEDTLS2_ERR_ENTROPY_NO_STRONG_SOURCE;

cleanup:
  mbedtls2_platform_zeroize(buf, sizeof(buf));

  return (ret);
}

/*
 * Thread-safe wrapper for entropy_gather_internal()
 */
int mbedtls2_entropy_gather(mbedtls2_entropy_context *ctx) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;

#if defined(MBEDTLS2_THREADING_C)
  if ((ret = mbedtls2_mutex_lock(&ctx->mutex)) != 0)
    return (ret);
#endif

  ret = entropy_gather_internal(ctx);

#if defined(MBEDTLS2_THREADING_C)
  if (mbedtls2_mutex_unlock(&ctx->mutex) != 0)
    return (MBEDTLS2_ERR_THREADING_MUTEX_ERROR);
#endif

  return (ret);
}

int mbedtls2_entropy_func(void *data, unsigned char *output,
                                 size_t len) {
  int ret, count = 0, i, thresholds_reached;
  size_t strong_size;
  mbedtls2_entropy_context *ctx =
      (mbedtls2_entropy_context *)data;
  unsigned char buf[MBEDTLS2_ENTROPY_BLOCK_SIZE];

  if (len > MBEDTLS2_ENTROPY_BLOCK_SIZE)
    return (MBEDTLS2_ERR_ENTROPY_SOURCE_FAILED);

#if defined(MBEDTLS2_ENTROPY_NV_SEED)
  /* Update the NV entropy seed before generating any entropy for outside
   * use.
   */
  if (ctx->initial_entropy_run == 0) {
    ctx->initial_entropy_run = 1;
    if ((ret = mbedtls2_entropy_update_nv_seed(ctx)) != 0)
      return (ret);
  }
#endif

#if defined(MBEDTLS2_THREADING_C)
  if ((ret = mbedtls2_mutex_lock(&ctx->mutex)) != 0)
    return (ret);
#endif

  /*
   * Always gather extra entropy before a call
   */
  do {
    if (count++ > ENTROPY_MAX_LOOP) {
      ret = MBEDTLS2_ERR_ENTROPY_SOURCE_FAILED;
      goto exit;
    }

    if ((ret = entropy_gather_internal(ctx)) != 0)
      goto exit;

    thresholds_reached = 1;
    strong_size = 0;
    for (i = 0; i < ctx->source_count; i++) {
      if (ctx->source[i].size < ctx->source[i].threshold)
        thresholds_reached = 0;
      if (ctx->source[i].strong == MBEDTLS2_ENTROPY_SOURCE_STRONG)
        strong_size += ctx->source[i].size;
    }
  } while (!thresholds_reached ||
           strong_size < MBEDTLS2_ENTROPY_BLOCK_SIZE);

  memset(buf, 0, MBEDTLS2_ENTROPY_BLOCK_SIZE);

#if defined(MBEDTLS2_ENTROPY_SHA512_ACCUMULATOR)
  /*
   * Note that at this stage it is assumed that the accumulator was started
   * in a previous call to entropy_update(). If this is not guaranteed, the
   * code below will fail.
   */
  if ((ret = mbedtls2_sha512_finish_ret(&ctx->accumulator, buf)) != 0)
    goto exit;

  /*
   * Reset accumulator and counters and recycle existing entropy
   */
  mbedtls2_sha512_free(&ctx->accumulator);
  mbedtls2_sha512_init(&ctx->accumulator);
  if ((ret = mbedtls2_sha512_starts_ret(&ctx->accumulator, 0)) != 0)
    goto exit;
  if ((ret = mbedtls2_sha512_update_ret(
           &ctx->accumulator, buf, MBEDTLS2_ENTROPY_BLOCK_SIZE)) != 0)
    goto exit;

  /*
   * Perform second SHA-512 on entropy
   */
  if ((ret = mbedtls2_sha512_ret(buf, MBEDTLS2_ENTROPY_BLOCK_SIZE,
                                        buf, 0)) != 0)
    goto exit;
#else  /* MBEDTLS2_ENTROPY_SHA512_ACCUMULATOR */
  if ((ret = mbedtls2_sha256_finish_ret(&ctx->accumulator, buf)) != 0)
    goto exit;

  /*
   * Reset accumulator and counters and recycle existing entropy
   */
  mbedtls2_sha256_free(&ctx->accumulator);
  mbedtls2_sha256_init(&ctx->accumulator);
  if ((ret = mbedtls2_sha256_starts_ret(&ctx->accumulator, 0)) != 0)
    goto exit;
  if ((ret = mbedtls2_sha256_update_ret(
           &ctx->accumulator, buf, MBEDTLS2_ENTROPY_BLOCK_SIZE)) != 0)
    goto exit;

  /*
   * Perform second SHA-256 on entropy
   */
  if ((ret = mbedtls2_sha256_ret(buf, MBEDTLS2_ENTROPY_BLOCK_SIZE,
                                        buf, 0)) != 0)
    goto exit;
#endif /* MBEDTLS2_ENTROPY_SHA512_ACCUMULATOR */

  for (i = 0; i < ctx->source_count; i++)
    ctx->source[i].size = 0;

  memcpy(output, buf, len);

  ret = 0;

exit:
  mbedtls2_platform_zeroize(buf, sizeof(buf));

#if defined(MBEDTLS2_THREADING_C)
  if (mbedtls2_mutex_unlock(&ctx->mutex) != 0)
    return (MBEDTLS2_ERR_THREADING_MUTEX_ERROR);
#endif

  return (ret);
}

#if defined(MBEDTLS2_ENTROPY_NV_SEED)
int mbedtls2_entropy_update_nv_seed(
    mbedtls2_entropy_context *ctx) {
  int ret = MBEDTLS2_ERR_ENTROPY_FILE_IO_ERROR;
  unsigned char buf[MBEDTLS2_ENTROPY_BLOCK_SIZE];

  /* Read new seed  and write it to NV */
  if ((ret = mbedtls2_entropy_func(
           ctx, buf, MBEDTLS2_ENTROPY_BLOCK_SIZE)) != 0)
    return (ret);

  if (mbedtls2_nv_seed_write(buf, MBEDTLS2_ENTROPY_BLOCK_SIZE) <
      0)
    return (MBEDTLS2_ERR_ENTROPY_FILE_IO_ERROR);

  /* Manually update the remaining stream with a separator value to diverge */
  memset(buf, 0, MBEDTLS2_ENTROPY_BLOCK_SIZE);
  ret = mbedtls2_entropy_update_manual(
      ctx, buf, MBEDTLS2_ENTROPY_BLOCK_SIZE);

  return (ret);
}
#endif /* MBEDTLS2_ENTROPY_NV_SEED */

#if defined(MBEDTLS2_FS_IO)
int mbedtls2_entropy_write_seed_file(
    mbedtls2_entropy_context *ctx, const char *path) {
  int ret = MBEDTLS2_ERR_ERROR_CORRUPTION_DETECTED;
  FILE *f = NULL;
  unsigned char buf[MBEDTLS2_ENTROPY_BLOCK_SIZE];

  if ((ret = mbedtls2_entropy_func(
           ctx, buf, MBEDTLS2_ENTROPY_BLOCK_SIZE)) != 0) {
    ret = MBEDTLS2_ERR_ENTROPY_SOURCE_FAILED;
    goto exit;
  }

  if ((f = fopen(path, "wb")) == NULL) {
    ret = MBEDTLS2_ERR_ENTROPY_FILE_IO_ERROR;
    goto exit;
  }

  if (fwrite(buf, 1, MBEDTLS2_ENTROPY_BLOCK_SIZE, f) !=
      MBEDTLS2_ENTROPY_BLOCK_SIZE) {
    ret = MBEDTLS2_ERR_ENTROPY_FILE_IO_ERROR;
    goto exit;
  }

  ret = 0;

exit:
  mbedtls2_platform_zeroize(buf, sizeof(buf));

  if (f != NULL)
    fclose(f);

  return (ret);
}

int mbedtls2_entropy_update_seed_file(
    mbedtls2_entropy_context *ctx, const char *path) {
  int ret = 0;
  FILE *f;
  size_t n;
  unsigned char buf[MBEDTLS2_ENTROPY_MAX_SEED_SIZE];

  if ((f = fopen(path, "rb")) == NULL)
    return (MBEDTLS2_ERR_ENTROPY_FILE_IO_ERROR);

  fseek(f, 0, SEEK_END);
  n = (size_t)ftell(f);
  fseek(f, 0, SEEK_SET);

  if (n > MBEDTLS2_ENTROPY_MAX_SEED_SIZE)
    n = MBEDTLS2_ENTROPY_MAX_SEED_SIZE;

  if (fread(buf, 1, n, f) != n)
    ret = MBEDTLS2_ERR_ENTROPY_FILE_IO_ERROR;
  else
    ret = mbedtls2_entropy_update_manual(ctx, buf, n);

  fclose(f);

  mbedtls2_platform_zeroize(buf, sizeof(buf));

  if (ret != 0)
    return (ret);

  return (mbedtls2_entropy_write_seed_file(ctx, path));
}
#endif /* MBEDTLS2_FS_IO */

#if defined(MBEDTLS2_SELF_TEST)
#if !defined(MBEDTLS2_TEST_NULL_ENTROPY)
/*
 * Dummy source function
 */
static int entropy_dummy_source(void *data, unsigned char *output, size_t len,
                                size_t *olen) {
  ((void)data);

  memset(output, 0x2a, len);
  *olen = len;

  return (0);
}
#endif /* !MBEDTLS2_TEST_NULL_ENTROPY */

#if defined(MBEDTLS2_ENTROPY_HARDWARE_ALT)

static int mbedtls2_entropy_source_self_test_gather(unsigned char *buf,
                                                           size_t buf_len) {
  int ret = 0;
  size_t entropy_len = 0;
  size_t olen = 0;
  size_t attempts = buf_len;

  while (attempts > 0 && entropy_len < buf_len) {
    if ((ret = mbedtls2_hardware_poll(
             NULL, buf + entropy_len, buf_len - entropy_len, &olen)) != 0)
      return (ret);

    entropy_len += olen;
    attempts--;
  }

  if (entropy_len < buf_len) {
    ret = 1;
  }

  return (ret);
}

static int
mbedtls2_entropy_source_self_test_check_bits(const unsigned char *buf,
                                                    size_t buf_len) {
  unsigned char set = 0xFF;
  unsigned char unset = 0x00;
  size_t i;

  for (i = 0; i < buf_len; i++) {
    set &= buf[i];
    unset |= buf[i];
  }

  return (set == 0xFF || unset == 0x00);
}

/*
 * A test to ensure hat the entropy sources are functioning correctly
 * and there is no obvious failure. The test performs the following checks:
 *  - The entropy source is not providing only 0s (all bits unset) or 1s (all
 *    bits set).
 *  - The entropy source is not providing values in a pattern. Because the
 *    hardware could be providing data in an arbitrary length, this check polls
 *    the hardware entropy source twice and compares the result to ensure they
 *    are not equal.
 *  - The error code returned by the entropy source is not an error.
 */
int mbedtls2_entropy_source_self_test(int verbose) {
  int ret = 0;
  unsigned char buf0[2 * sizeof(unsigned long long int)];
  unsigned char buf1[2 * sizeof(unsigned long long int)];

  if (verbose != 0)
    mbedtls2_printf("  ENTROPY_BIAS test: ");

  memset(buf0, 0x00, sizeof(buf0));
  memset(buf1, 0x00, sizeof(buf1));

  if ((ret = mbedtls2_entropy_source_self_test_gather(
           buf0, sizeof(buf0))) != 0)
    goto cleanup;
  if ((ret = mbedtls2_entropy_source_self_test_gather(
           buf1, sizeof(buf1))) != 0)
    goto cleanup;

  /* Make sure that the returned values are not all 0 or 1 */
  if ((ret = mbedtls2_entropy_source_self_test_check_bits(
           buf0, sizeof(buf0))) != 0)
    goto cleanup;
  if ((ret = mbedtls2_entropy_source_self_test_check_bits(
           buf1, sizeof(buf1))) != 0)
    goto cleanup;

  /* Make sure that the entropy source is not returning values in a
   * pattern */
  ret = memcmp(buf0, buf1, sizeof(buf0)) == 0;

cleanup:
  if (verbose != 0) {
    if (ret != 0)
      mbedtls2_printf("failed\n");
    else
      mbedtls2_printf("passed\n");

    mbedtls2_printf("\n");
  }

  return (ret != 0);
}

#endif /* MBEDTLS2_ENTROPY_HARDWARE_ALT */

/*
 * The actual entropy quality is hard to test, but we can at least
 * test that the functions don't cause errors and write the correct
 * amount of data to buffers.
 */
int mbedtls2_entropy_self_test(int verbose) {
  int ret = 1;
#if !defined(MBEDTLS2_TEST_NULL_ENTROPY)
  mbedtls2_entropy_context ctx;
  unsigned char buf[MBEDTLS2_ENTROPY_BLOCK_SIZE] = {0};
  unsigned char acc[MBEDTLS2_ENTROPY_BLOCK_SIZE] = {0};
  size_t i, j;
#endif /* !MBEDTLS2_TEST_NULL_ENTROPY */

  if (verbose != 0)
    mbedtls2_printf("  ENTROPY test: ");

#if !defined(MBEDTLS2_TEST_NULL_ENTROPY)
  mbedtls2_entropy_init(&ctx);

  /* First do a gather to make sure we have default sources */
  if ((ret = mbedtls2_entropy_gather(&ctx)) != 0)
    goto cleanup;

  ret = mbedtls2_entropy_add_source(&ctx, entropy_dummy_source, NULL, 16,
                                           MBEDTLS2_ENTROPY_SOURCE_WEAK);
  if (ret != 0)
    goto cleanup;

  if ((ret = mbedtls2_entropy_update_manual(&ctx, buf, sizeof buf)) != 0)
    goto cleanup;

  /*
   * To test that mbedtls2_entropy_func writes correct number of bytes:
   * - use the whole buffer and rely on ASan to detect overruns
   * - collect entropy 8 times and OR the result in an accumulator:
   *   any byte should then be 0 with probably 2^(-64), so requiring
   *   each of the 32 or 64 bytes to be non-zero has a false failure rate
   *   of at most 2^(-58) which is acceptable.
   */
  for (i = 0; i < 8; i++) {
    if ((ret = mbedtls2_entropy_func(&ctx, buf, sizeof(buf))) != 0)
      goto cleanup;

    for (j = 0; j < sizeof(buf); j++)
      acc[j] |= buf[j];
  }

  for (j = 0; j < sizeof(buf); j++) {
    if (acc[j] == 0) {
      ret = 1;
      goto cleanup;
    }
  }

#if defined(MBEDTLS2_ENTROPY_HARDWARE_ALT)
  if ((ret = mbedtls2_entropy_source_self_test(0)) != 0)
    goto cleanup;
#endif

cleanup:
  mbedtls2_entropy_free(&ctx);
#endif /* !MBEDTLS2_TEST_NULL_ENTROPY */

  if (verbose != 0) {
    if (ret != 0)
      mbedtls2_printf("failed\n");
    else
      mbedtls2_printf("passed\n");

    mbedtls2_printf("\n");
  }

  return (ret != 0);
}
#endif /* MBEDTLS2_SELF_TEST */

#endif /* MBEDTLS2_ENTROPY_C */

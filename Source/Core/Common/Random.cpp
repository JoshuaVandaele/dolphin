// Copyright 2018 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "Common/Random.h"

#include <mbedtls2/entropy.h>
#include <mbedtls2/hmac_drbg.h>

#include "Common/Assert.h"
#include "Common/CommonTypes.h"

namespace Common::Random
{
class EntropySeededPRNG final
{
public:
  EntropySeededPRNG()
  {
    mbedtls2_entropy_init(&m_entropy);
    mbedtls2_hmac_drbg_init(&m_context);
    const int ret = mbedtls2_hmac_drbg_seed(
        &m_context, mbedtls2_md_info_from_type(MBEDTLS2_MD_SHA256),
        mbedtls2_entropy_func, &m_entropy, nullptr, 0);
    ASSERT(ret == 0);
  }

  ~EntropySeededPRNG()
  {
    mbedtls2_hmac_drbg_free(&m_context);
    mbedtls2_entropy_free(&m_entropy);
  }

  void Generate(void* buffer, std::size_t size)
  {
    const int ret = mbedtls2_hmac_drbg_random(&m_context, static_cast<u8*>(buffer), size);
    ASSERT(ret == 0);
  }

private:
  mbedtls2_entropy_context m_entropy;
  mbedtls2_hmac_drbg_context m_context;
};

static thread_local EntropySeededPRNG s_esprng;

void Generate(void* buffer, std::size_t size)
{
  s_esprng.Generate(buffer, size);
}
}  // namespace Common::Random

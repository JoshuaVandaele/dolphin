// Copyright 2023 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "Common/Crypto/HMAC.h"

#include <mbedtls2/md.h>

#include "Common/ScopeGuard.h"

namespace Common::HMAC
{
bool HMACWithSHA1(std::span<const u8> key, std::span<const u8> msg, u8* out)
{
  mbedtls2_md_context_t ctx;
  Common::ScopeGuard guard{[&ctx] { mbedtls2_md_free(&ctx); }};
  mbedtls2_md_init(&ctx);
  if (mbedtls2_md_setup(&ctx, mbedtls2_md_info_from_type(MBEDTLS2_MD_SHA1), 1))
    return false;

  if (mbedtls2_md_hmac_starts(&ctx, key.data(), key.size()) ||
      mbedtls2_md_hmac_update(&ctx, msg.data(), msg.size()) ||
      mbedtls2_md_hmac_finish(&ctx, out))
  {
    return false;
  }

  return true;
}
}  // namespace Common::HMAC

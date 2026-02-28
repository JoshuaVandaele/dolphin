// Copyright 2024 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "Common/JsonUtil.h"

#include <expected>
#include <memory>

#include <simdjson.h>

#include "Common/FileUtil.h"
#include "simdjson/error.h"

namespace JsonUtil
{

simdjson::error_code JsonToFile(const std::string& filename, const simdjson::dom::element& root,
                                bool prettify)
{
  std::string json;
  if (prettify)
    json = simdjson::prettify(root);
  else
    json = simdjson::minify(root);
  if (!File::WriteStringToFile(filename, json))
    return simdjson::IO_ERROR;
  return simdjson::SUCCESS;
}

std::expected<std::unique_ptr<DocumentContext>, simdjson::error_code>
JsonFromFile(std::string_view filename, simdjson::ondemand::document& doc)
{
  auto ctx = std::make_unique<DocumentContext>();

  simdjson::error_code err;

  if (err = simdjson::padded_string::load(filename).get(ctx->json); err)
    return std::unexpected{err};

  if (err = ctx->parser.iterate(ctx->json).get(ctx->doc); err)
    return std::unexpected{err};

  return ctx;
}
}  // namespace JsonUtil

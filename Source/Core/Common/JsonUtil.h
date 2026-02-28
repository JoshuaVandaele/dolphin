// Copyright 2024 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#pragma once

#include <expected>
#include <optional>
#include <string>

#include <simdjson.h>

namespace JsonUtil
{
struct DocumentContext
{
  simdjson::padded_string json;
  simdjson::ondemand::parser parser;
  simdjson::ondemand::document doc;

  DocumentContext() = default;
  DocumentContext(DocumentContext&&) = default;
  DocumentContext& operator=(DocumentContext&&) = default;
  DocumentContext(const DocumentContext&) = delete;
  DocumentContext& operator=(const DocumentContext&) = delete;
};

template <typename Type>
std::optional<Type> ReadValueFromJson(const simdjson::dom::element& obj, std::string_view key)
{
  auto el = obj[key];
  if (el.error() != simdjson::SUCCESS)
    return std::nullopt;

  Type value;
  if (el.get(value) != simdjson::SUCCESS)
    return std::nullopt;

  return value;
}

simdjson::error_code JsonToFile(const std::string& filename, const simdjson::dom::element& root,
                                bool prettify = false);

std::expected<std::unique_ptr<DocumentContext>, simdjson::error_code>
JsonFromFile(std::string_view filename, simdjson::ondemand::document& doc);
}  // namespace JsonUtil

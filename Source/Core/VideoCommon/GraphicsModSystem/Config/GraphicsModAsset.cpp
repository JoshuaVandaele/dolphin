// Copyright 2023 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "VideoCommon/GraphicsModSystem/Config/GraphicsModAsset.h"

#include <simdjson.h>

namespace simdjson
{

template <typename builder_type>
void tag_invoke(serialize_tag, builder_type& builder, const GraphicsModAssetConfig& cfg)
{
  builder.start_object();
  builder.append_key_value("name", cfg.m_asset_id);
  builder.append_comma();

  builder.append_key_value("data", cfg.m_map);
  builder.end_object();
}

template <typename simdjson_value>
auto tag_invoke(deserialize_tag, simdjson_value& val, GraphicsModAssetConfig& cfg)
{
  simdjson::ondemand::object obj;
  simdjson::error_code err;

  if (err = val.get_object().get(obj); err)
    return err;

  if (err = obj["name"].get(cfg.m_asset_id); err)
    return err;

  cfg.m_map.clear();
  if (err = obj["data"].get(cfg.m_map); err)
    return err;

  return simdjson::SUCCESS;
}

}  // namespace simdjson

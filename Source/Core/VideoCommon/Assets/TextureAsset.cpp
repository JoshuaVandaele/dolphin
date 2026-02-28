// Copyright 2023 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "VideoCommon/Assets/TextureAsset.h"

#include <simdjson.h>

namespace VideoCommon
{

CustomAssetLibrary::LoadInfo TextureAsset::LoadImpl(const CustomAssetLibrary::AssetID& asset_id)
{
  auto potential_data = std::make_shared<CustomTextureData>();
  const auto loaded_info = m_owning_library->LoadTexture(asset_id, potential_data.get());
  if (loaded_info.bytes_loaded == 0)
    return {};
  {
    std::lock_guard lk(m_data_lock);
    m_loaded = true;
    m_data = std::move(potential_data);
  }
  return loaded_info;
}

CustomAssetLibrary::LoadInfo
TextureAndSamplerAsset::LoadImpl(const CustomAssetLibrary::AssetID& asset_id)
{
  auto potential_data = std::make_shared<TextureAndSamplerData>();
  const auto loaded_info = m_owning_library->LoadTexture(asset_id, potential_data.get());
  if (loaded_info.bytes_loaded == 0)
    return {};
  {
    std::lock_guard lk(m_data_lock);
    m_loaded = true;
    m_data = std::move(potential_data);
  }
  return loaded_info;
}
}  // namespace VideoCommon

namespace simdjson
{

template <typename builder_type>
void tag_invoke(serialize_tag, builder_type& builder,
                const VideoCommon::TextureAndSamplerData& data)
{
  builder.start_object();

  builder.append_key_value("type", data.type);
  builder.append_comma();

  builder.append_key_value("texture_data", data.texture_data);
  builder.append_comma();

  builder.append_key_value("sampler", data.sampler);

  builder.end_object();
}

template <typename simdjson_value>
auto tag_invoke(deserialize_tag, simdjson_value& val, VideoCommon::TextureAndSamplerData& data)
{
  simdjson::ondemand::object obj;
  simdjson::error_code err;

  if (err = val.get_object().get(obj); err)
    return err;

  if (err = obj["type"].get(data.type); err)
    return err;

  if (err = obj["texture_data"].get(data.texture_data); err)
    return err;

  if (err = obj["sampler"].get(data.sampler); err)
    return err;

  return simdjson::SUCCESS;
}

}  // namespace simdjson

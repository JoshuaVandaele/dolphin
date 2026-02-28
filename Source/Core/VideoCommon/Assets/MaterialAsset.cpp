// Copyright 2023 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "VideoCommon/Assets/MaterialAsset.h"

#include <algorithm>
#include <string_view>
#include <vector>

#include <simdjson.h>

#include "Common/Logging/Log.h"
#include "VideoCommon/Assets/CustomAssetLibrary.h"

namespace VideoCommon
{
namespace
{
// While not optimal, we pad our data to match std140 shader requirements
// this memory constant indicates the memory stride for a single uniform
// regardless of data type
constexpr std::size_t MemorySize = sizeof(float) * 4;

template <typename ElementType, std::size_t ElementCount>
bool ParseNumeric(const CustomAssetLibrary::AssetID& asset_id, const nlohmann::json& json_value,
                  MaterialProperty::Value* value)
{
  static_assert(ElementCount <= 4, "Numeric data expected to be four elements or less");
  if constexpr (ElementCount == 1)
  {
    if (!json_value.is_number())
    {
      ERROR_LOG_FMT(VIDEO,
                    "Asset id '{}' material has attribute where "
                    "a double was expected but not provided.",
                    asset_id);
      return false;
    }

    *value = static_cast<ElementType>(json_value.get<double>());
  }
  else
  {
    if (!json_value.is_array())
    {
      ERROR_LOG_FMT(VIDEO,
                    "Asset id '{}' material has attribute where "
                    "an array was expected but not provided.",
                    asset_id);
      return false;
    }

    if (json_value.size() != ElementCount)
    {
      ERROR_LOG_FMT(VIDEO,
                    "Asset id '{}' material has attribute with incorrect number "
                    "of elements, expected {}",
                    asset_id, ElementCount);
      return false;
    }

    if (!std::ranges::all_of(json_value, &nlohmann::json::is_number))
    {
      ERROR_LOG_FMT(VIDEO,
                    "Asset id '{}' material has attribute where "
                    "all elements are not of type double.",
                    asset_id);
      return false;
    }

    std::array<ElementType, ElementCount> data;
    for (std::size_t i = 0; i < ElementCount; i++)
    {
      data[i] = static_cast<ElementType>(json_value[i].get<double>());
    }
    *value = std::move(data);
  }

  return true;
}
bool ParsePropertyValue(const CustomAssetLibrary::AssetID& asset_id,
                        const nlohmann::json& json_value, std::string_view type,
                        MaterialProperty::Value* value)
{
  if (type == "int")
  {
    return ParseNumeric<s32, 1>(asset_id, json_value, value);
  }
  else if (type == "int2")
  {
    return ParseNumeric<s32, 2>(asset_id, json_value, value);
  }
  else if (type == "int3")
  {
    return ParseNumeric<s32, 3>(asset_id, json_value, value);
  }
  else if (type == "int4")
  {
    return ParseNumeric<s32, 4>(asset_id, json_value, value);
  }
  else if (type == "float")
  {
    return ParseNumeric<float, 1>(asset_id, json_value, value);
  }
  else if (type == "float2")
  {
    return ParseNumeric<float, 2>(asset_id, json_value, value);
  }
  else if (type == "float3")
  {
    return ParseNumeric<float, 3>(asset_id, json_value, value);
  }
  else if (type == "float4")
  {
    return ParseNumeric<float, 4>(asset_id, json_value, value);
  }
  else if (type == "bool")
  {
    if (json_value.is_boolean())
    {
      *value = json_value.get<bool>();
      return true;
    }
  }

  ERROR_LOG_FMT(VIDEO, "Asset '{}' failed to parse the json, value is not valid for type '{}'",
                asset_id, type);
  return false;
}

bool ParseMaterialProperties(const CustomAssetLibrary::AssetID& asset_id,
                             const nlohmann::json& values_data,
                             std::vector<MaterialProperty>* material_property)
{
  for (const auto& value_data : values_data)
  {
    VideoCommon::MaterialProperty property;
    if (!value_data.is_object())
    {
      ERROR_LOG_FMT(VIDEO, "Asset '{}' failed to parse the json, value is not the right json type",
                    asset_id);
      return false;
    }

    auto type_it = value_data.find("type");
    if (type_it == value_data.end())
    {
      ERROR_LOG_FMT(VIDEO, "Asset '{}' failed to parse the json, value entry 'type' not found",
                    asset_id);
      return false;
    }
    else if (!type_it->is_string())
    {
      ERROR_LOG_FMT(VIDEO,
                    "Asset '{}' failed to parse the json, value entry 'type' is not "
                    "the right json type",
                    asset_id);
      return false;
    }
    std::string type = type_it->get<std::string>();
    Common::ToLower(&type);

    if (auto value_it = value_data.find("value"); type_it != value_data.end())
    {
      if (!ParsePropertyValue(asset_id, *value_it, type, &property.m_value))
        return false;
    }

    material_property->push_back(std::move(property));
  }

  return true;
}
}  // namespace

void MaterialProperty::WriteToMemory(u8*& buffer, const MaterialProperty& property)
{
  const auto write_memory = [&](const void* raw_value, std::size_t data_size) {
    std::memcpy(buffer, raw_value, data_size);
    std::memset(buffer + data_size, 0, MemorySize - data_size);
    buffer += MemorySize;
  };
  std::visit(
      overloaded{
          [&](s32 value) { write_memory(&value, sizeof(s32)); },
          [&](const std::array<s32, 2>& value) { write_memory(value.data(), sizeof(s32) * 2); },
          [&](const std::array<s32, 3>& value) { write_memory(value.data(), sizeof(s32) * 3); },
          [&](const std::array<s32, 4>& value) { write_memory(value.data(), sizeof(s32) * 4); },
          [&](float value) { write_memory(&value, sizeof(float)); },
          [&](const std::array<float, 2>& value) { write_memory(value.data(), sizeof(float) * 2); },
          [&](const std::array<float, 3>& value) { write_memory(value.data(), sizeof(float) * 3); },
          [&](const std::array<float, 4>& value) { write_memory(value.data(), sizeof(float) * 4); },

          // Bool has the size of an int in the shader
          [&](bool value) {
            u32 val = static_cast<u32>(value);
            write_memory(&val, sizeof(u32));
          }},
      property.m_value);
}

std::size_t MaterialProperty::GetMemorySize(const MaterialProperty& property)
{
  std::size_t result = 0;
  std::visit(overloaded{[&](s32 value) { result = MemorySize; },
                        [&](const std::array<s32, 2>&) { result = MemorySize; },
                        [&](const std::array<s32, 3>&) { result = MemorySize; },
                        [&](const std::array<s32, 4>&) { result = MemorySize; },
                        [&](float) { result = MemorySize; },
                        [&](const std::array<float, 2>&) { result = MemorySize; },
                        [&](const std::array<float, 3>&) { result = MemorySize; },
                        [&](const std::array<float, 4>&) { result = MemorySize; },
                        [&](bool) { result = MemorySize; }},
             property.m_value);

  return result;
}

CustomAssetLibrary::LoadInfo MaterialAsset::LoadImpl(const CustomAssetLibrary::AssetID& asset_id)
{
  auto potential_data = std::make_shared<MaterialData>();
  const auto loaded_info = m_owning_library->LoadMaterial(asset_id, potential_data.get());
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
void tag_invoke(serialize_tag, builder_type& builder, const MaterialProperty& property)
{
  builder.start_object();

  auto write = [&](std::string_view type, const auto& value) {
    builder.append_key_value("type", type);
    builder.append_comma();
    builder.append_key_value("value", value);
  };

  std::visit(overloaded{[&](s32 v) { write("int", v); },
                        [&](const std::array<s32, 2>& v) { write("int2", v); },
                        [&](const std::array<s32, 3>& v) { write("int3", v); },
                        [&](const std::array<s32, 4>& v) { write("int4", v); },
                        [&](float v) { write("float", v); },
                        [&](const std::array<float, 2>& v) { write("float2", v); },
                        [&](const std::array<float, 3>& v) { write("float3", v); },
                        [&](const std::array<float, 4>& v) { write("float4", v); },
                        [&](bool v) { write("bool", v); }},
             property.m_value);

  builder.end_object();
}

template <typename simdjson_value>
auto tag_invoke(deserialize_tag, simdjson_value& val, MaterialProperty& property)
{
  simdjson::ondemand::object obj;
  simdjson::error_code err;

  if (err = val.get_object().get(obj); err)
    return err;

  std::string_view type;
  if (err = obj["type"].get_string().get(type); err)
    return err;

  auto get_value = [&](auto type_tag) -> simdjson::error_code {
    using T = decltype(type_tag);
    T temp{};
    if (auto err = obj["value"].get(temp); err)
      return err;
    property.m_value = temp;
    return simdjson::SUCCESS;
  };

  if (type == "int")
    return get_value(int32_t{});
  if (type == "int2")
    return get_value(std::array<int32_t, 2>{});
  if (type == "int3")
    return get_value(std::array<int32_t, 3>{});
  if (type == "int4")
    return get_value(std::array<int32_t, 4>{});
  if (type == "float")
    return get_value(float{});
  if (type == "float2")
    return get_value(std::array<float, 2>{});
  if (type == "float3")
    return get_value(std::array<float, 3>{});
  if (type == "float4")
    return get_value(std::array<float, 4>{});
  if (type == "bool")
    return get_value(bool{});

  return simdjson::INCORRECT_TYPE;
}

template <typename builder_type>
void tag_invoke(serialize_tag, builder_type& builder, const MaterialData& data)
{
  builder.start_object();

  builder.append_key_value("shader_asset", data.shader_asset);
  builder.append_comma();

  builder.append_key_value("next_material_asset", data.next_material_asset);
  builder.append_comma();

  builder.append_key_value("properties", data.properties);
  builder.append_comma();

  builder.append_key_value("textures", m.textures);

  if (data.cull_mode)
  {
    builder.append_comma();
    builder.append_key_value("cull_mode", *m.cull_mode);
  }

  if (data.depth_state)
  {
    builder.append_comma();
    builder.append_key_value("depth_state", *m.depth_state);
  }

  if (data.blending_state)
  {
    builder.append_comma();
    builder.append_key_value("blending_state", *m.blending_state);
  }

  builder.end_object();
}

template <typename simdjson_value>
auto tag_invoke(deserialize_tag, simdjson_value& val, MaterialData& data)
{
  simdjson::ondemand::object obj;
  simdjson::error_code err;

  if (err = val.get_object().get(obj); err)
    return err;

  if (err = obj["shader_asset"].get(data.shader_asset); err)
    return err;

  if (err = obj["next_material_asset"].get(data.next_material_asset); err)
    return err;

  if (err = obj["properties"].get(data.properties); err)
    return err;

  if (err = obj["textures"].get(data.textures); err)
    return err;

  err = obj["cull_mode"].get(data.cull_mode);
  if (err == simdjson::NO_SUCH_FIELD)
    data.cull_mode = std::nullopt;
  else if (err)
    return err;

  err = obj["depth_state"].get(data.depth_state);
  if (err == simdjson::NO_SUCH_FIELD)
    data.depth_state = std::nullopt;
  else if (err)
    return err;

  err = obj["blending_state"].get(data.blending_state);
  if (err == simdjson::NO_SUCH_FIELD)
    data.blending_state = std::nullopt;
  else if (err)
    return err;

  return simdjson::SUCCESS;
}

}  // namespace simdjson

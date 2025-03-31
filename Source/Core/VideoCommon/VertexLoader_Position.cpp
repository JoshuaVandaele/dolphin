// Copyright 2008 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "VideoCommon/VertexLoader_Position.h"

#include <limits>
#include <type_traits>

#include "Common/CommonTypes.h"
#include "Common/EnumMap.h"
#include "Common/Swap.h"

#include "VideoCommon/VertexLoader.h"
#include "VideoCommon/VertexLoaderManager.h"
#include "VideoCommon/VertexLoaderUtils.h"
#include "VideoCommon/VideoCommon.h"

namespace
{
template <typename T>
constexpr float PosScale(T val, float scale)
{
  return val * scale;
}

template <>
constexpr float PosScale(float val, [[maybe_unused]] float scale)
{
  return val;
}

template <typename T, int N>
void PosReadDirect(VertexLoader* loader)
{
  static_assert(N <= 3, "N > 3 is not sane!");
  const auto scale = loader->m_posScale;

  for (int i = 0; i < N; ++i)
  {
    const float value = PosScale(DataRead<T>(), scale);
    if (loader->m_remaining < 3)
      VertexLoaderManager::position_cache[loader->m_remaining][i] = value;
    DataWrite(value);
  }

  LOG_VTX();
}

template <typename I, typename T, int N>
void PosReadIndex(VertexLoader* loader)
{
  static_assert(std::is_unsigned<I>::value, "Only unsigned I is sane!");
  static_assert(N <= 3, "N > 3 is not sane!");

  const auto index = DataRead<I>();
  loader->m_vertexSkip = index == std::numeric_limits<I>::max();
  const auto data =
      reinterpret_cast<const T*>(VertexLoaderManager::cached_arraybases[CPArray::Position] +
                                 (index * g_main_cp_state.array_strides[CPArray::Position]));
  const auto scale = loader->m_posScale;

  for (int i = 0; i < N; ++i)
  {
    const float value = PosScale(Common::FromBigEndian(data[i]), scale);
    if (loader->m_remaining < 3 && !loader->m_vertexSkip)
      VertexLoaderManager::position_cache[loader->m_remaining][i] = value;
    DataWrite(value);
  }

  LOG_VTX();
}

using ComponentCountRow = Common::EnumMap<TPipelineFunction, CoordComponentCount::XYZ>;
using ComponentFormatTable = Common::EnumMap<ComponentCountRow, ComponentFormat::InvalidFloat7>;
using Table = Common::EnumMap<ComponentFormatTable, VertexComponentFormat::Index16>;

constexpr Table S_TABLE_READ_POSITION = {
    ComponentFormatTable({
        ComponentCountRow(nullptr, nullptr),
        ComponentCountRow(nullptr, nullptr),
        ComponentCountRow(nullptr, nullptr),
        ComponentCountRow(nullptr, nullptr),
        ComponentCountRow(nullptr, nullptr),
        ComponentCountRow(nullptr, nullptr),
        ComponentCountRow(nullptr, nullptr),
        ComponentCountRow(nullptr, nullptr),
    }),
    ComponentFormatTable({
        ComponentCountRow(PosReadDirect<u8, 2>, PosReadDirect<u8, 3>),
        ComponentCountRow(PosReadDirect<s8, 2>, PosReadDirect<s8, 3>),
        ComponentCountRow(PosReadDirect<u16, 2>, PosReadDirect<u16, 3>),
        ComponentCountRow(PosReadDirect<s16, 2>, PosReadDirect<s16, 3>),
        ComponentCountRow(PosReadDirect<float, 2>, PosReadDirect<float, 3>),
        ComponentCountRow(PosReadDirect<float, 2>, PosReadDirect<float, 3>),
        ComponentCountRow(PosReadDirect<float, 2>, PosReadDirect<float, 3>),
        ComponentCountRow(PosReadDirect<float, 2>, PosReadDirect<float, 3>),
    }),
    ComponentFormatTable({
        ComponentCountRow(PosReadIndex<u8, u8, 2>, PosReadIndex<u8, u8, 3>),
        ComponentCountRow(PosReadIndex<u8, s8, 2>, PosReadIndex<u8, s8, 3>),
        ComponentCountRow(PosReadIndex<u8, u16, 2>, PosReadIndex<u8, u16, 3>),
        ComponentCountRow(PosReadIndex<u8, s16, 2>, PosReadIndex<u8, s16, 3>),
        ComponentCountRow(PosReadIndex<u8, float, 2>, PosReadIndex<u8, float, 3>),
        ComponentCountRow(PosReadIndex<u8, float, 2>, PosReadIndex<u8, float, 3>),
        ComponentCountRow(PosReadIndex<u8, float, 2>, PosReadIndex<u8, float, 3>),
        ComponentCountRow(PosReadIndex<u8, float, 2>, PosReadIndex<u8, float, 3>),
    }),
    ComponentFormatTable({
        ComponentCountRow(PosReadIndex<u16, u8, 2>, PosReadIndex<u16, u8, 3>),
        ComponentCountRow(PosReadIndex<u16, s8, 2>, PosReadIndex<u16, s8, 3>),
        ComponentCountRow(PosReadIndex<u16, u16, 2>, PosReadIndex<u16, u16, 3>),
        ComponentCountRow(PosReadIndex<u16, s16, 2>, PosReadIndex<u16, s16, 3>),
        ComponentCountRow(PosReadIndex<u16, float, 2>, PosReadIndex<u16, float, 3>),
        ComponentCountRow(PosReadIndex<u16, float, 2>, PosReadIndex<u16, float, 3>),
        ComponentCountRow(PosReadIndex<u16, float, 2>, PosReadIndex<u16, float, 3>),
        ComponentCountRow(PosReadIndex<u16, float, 2>, PosReadIndex<u16, float, 3>),
    }),
};
}  // Anonymous namespace

TPipelineFunction VertexLoader_Position::GetFunction(VertexComponentFormat type,
                                                     ComponentFormat format,
                                                     CoordComponentCount elements)
{
  return S_TABLE_READ_POSITION[type][format][elements];
}

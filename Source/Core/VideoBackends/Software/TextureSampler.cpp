// Copyright 2009 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "VideoBackends/Software/TextureSampler.h"

#include <algorithm>
#include <cmath>
#include <span>

#include "Common/CommonTypes.h"
#include "Common/MsgHandler.h"
#include "Common/SpanUtils.h"
#include "Core/HW/Memmap.h"
#include "Core/System.h"

#include "VideoCommon/BPMemory.h"
#include "VideoCommon/TextureDecoder.h"

#define ALLOW_MIPMAP 1

namespace TextureSampler
{
static inline void WrapCoord(int* coordp, WrapMode wrap_mode, int image_size)
{
  int coord = *coordp;
  switch (wrap_mode)
  {
  case WrapMode::Clamp:
    coord = std::clamp(coord, 0, image_size - 1);
    break;
  case WrapMode::Repeat:
    // Per YAGCD's info on TX_SETMODE1_I0 (et al.), mirror "requires the texture size to be a power
    // of two. (wrapping is implemented by a logical AND (SIZE-1))".  So though this doesn't wrap
    // nicely for non-power-of-2 sizes, that's how hardware does it.
    coord = coord & (image_size - 1);
    break;
  case WrapMode::Mirror:
  {
    // YAGCD doesn't mention this, but this seems to be the check used to implement mirroring.
    // With power-of-2 sizes, this correctly checks if it's an even-numbered repeat or an
    // odd-numbered one, and thus can decide whether to reflect.  It fails in unusual ways
    // with non-power-of-2 sizes, but seems to match what happens on actual hardware.
    if ((coord & image_size) != 0)
      coord = ~coord;
    coord = coord & (image_size - 1);
    break;
  }
  default:
    // Hardware testing indicates that wrap_mode set to 3 behaves the same as clamp.
    PanicAlertFmt("Invalid wrap mode: {}", wrap_mode);
    coord = std::clamp(coord, 0, image_size - 1);
    break;
  }
  *coordp = coord;
}

static inline void SetTexel(const u8* inTexel, u32* outTexel, u32 fract)
{
  outTexel[0] = inTexel[0] * fract;
  outTexel[1] = inTexel[1] * fract;
  outTexel[2] = inTexel[2] * fract;
  outTexel[3] = inTexel[3] * fract;
}

static inline void AddTexel(const u8* inTexel, u32* outTexel, u32 fract)
{
  outTexel[0] += inTexel[0] * fract;
  outTexel[1] += inTexel[1] * fract;
  outTexel[2] += inTexel[2] * fract;
  outTexel[3] += inTexel[3] * fract;
}

void Sample(s32 s, s32 t, s32 lod, bool linear, u8 texmap, u8* sample)
{
  int base_mip = 0;
  bool mip_linear = false;

#if (ALLOW_MIPMAP)
  auto tex_unit = bpmem.tex.GetUnit(texmap);
  const TexMode0& tm0 = tex_unit.texMode0;

  const s32 lod_fract = lod & 0xf;

  if (lod > 0 && tm0.mipmap_filter != MipMode::None)
  {
    // use mipmap
    base_mip = lod >> 4;
    mip_linear = (lod_fract && tm0.mipmap_filter == MipMode::Linear);

    // if using nearest mip filter and lodFract >= 0.5 round up to next mip
    if (tm0.mipmap_filter == MipMode::Point && lod_fract >= 8)
      base_mip++;
  }

  if (mip_linear)
  {
    u8 sampled_tex[4];
    u32 texel[4];

    SampleMip(s, t, base_mip, linear, texmap, sampled_tex);
    SetTexel(sampled_tex, texel, (16 - lod_fract));

    SampleMip(s, t, base_mip + 1, linear, texmap, sampled_tex);
    AddTexel(sampled_tex, texel, lod_fract);

    sample[0] = (u8)(texel[0] >> 4);
    sample[1] = (u8)(texel[1] >> 4);
    sample[2] = (u8)(texel[2] >> 4);
    sample[3] = (u8)(texel[3] >> 4);
  }
  else
#endif
  {
    SampleMip(s, t, base_mip, linear, texmap, sample);
  }
}

void SampleMip(s32 s, s32 t, s32 mip, bool linear, u8 texmap, u8* sample)
{
  auto tex_unit = bpmem.tex.GetUnit(texmap);

  const TexMode0& tm0 = tex_unit.texMode0;
  const TexImage0& ti0 = tex_unit.texImage0;
  const TexTLUT& tex_tlut = tex_unit.texTlut;
  const TextureFormat texfmt = ti0.format;
  const TLUTFormat tlutfmt = tex_tlut.tlut_format;

  std::span<const u8> image_src;
  std::span<const u8> image_src_odd;
  if (tex_unit.texImage1.cache_manually_managed)
  {
    image_src = TexDecoder_GetTmemSpan(tex_unit.texImage1.tmem_even * TMEM_LINE_SIZE);
    if (texfmt == TextureFormat::RGBA8)
      image_src_odd = TexDecoder_GetTmemSpan(tex_unit.texImage2.tmem_odd * TMEM_LINE_SIZE);
  }
  else
  {
    auto& system = Core::System::GetInstance();
    auto& memory = system.GetMemory();

    const u32 image_base = tex_unit.texImage3.image_base << 5;
    image_src = memory.GetSpanForAddress(image_base);
  }

  int image_width_minus_1 = ti0.width;
  int image_height_minus_1 = ti0.height;

  const int tlut_address = tex_tlut.tmem_offset << 9;
  const std::span<const u8> tlut = TexDecoder_GetTmemSpan(tlut_address);

  // reduce sample location and texture size to mip level
  // move texture pointer to mip location
  if (mip)
  {
    int mip_width = image_width_minus_1 + 1;
    int mip_height = image_height_minus_1 + 1;

    const int fmt_width = TexDecoder_GetBlockWidthInTexels(texfmt);
    const int fmt_height = TexDecoder_GetBlockHeightInTexels(texfmt);
    const int fmt_depth = TexDecoder_GetTexelSizeInNibbles(texfmt);

    image_width_minus_1 >>= mip;
    image_height_minus_1 >>= mip;
    s >>= mip;
    t >>= mip;

    while (mip)
    {
      mip_width = std::max(mip_width, fmt_width);
      mip_height = std::max(mip_height, fmt_height);
      const u32 size = (mip_width * mip_height * fmt_depth) >> 1;

      image_src = Common::SafeSubspan(image_src, size);
      mip_width >>= 1;
      mip_height >>= 1;
      mip--;
    }
  }

  if (linear)
  {
    // offset linear sampling
    s -= 64;
    t -= 64;

    // integer part of sample location
    int image_s = s >> 7;
    int image_t = t >> 7;

    // linear sampling
    int image_s_plus1 = image_s + 1;
    const int fract_s = s & 0x7f;

    int image_t_plus1 = image_t + 1;
    const int fract_t = t & 0x7f;

    u8 sampled_tex[4];
    u32 texel[4];

    WrapCoord(&image_s, tm0.wrap_s, image_width_minus_1 + 1);
    WrapCoord(&image_t, tm0.wrap_t, image_height_minus_1 + 1);
    WrapCoord(&image_s_plus1, tm0.wrap_s, image_width_minus_1 + 1);
    WrapCoord(&image_t_plus1, tm0.wrap_t, image_height_minus_1 + 1);

    if (!(texfmt == TextureFormat::RGBA8 && tex_unit.texImage1.cache_manually_managed))
    {
      TexDecoder_DecodeTexel(sampled_tex, image_src, image_s, image_t, image_width_minus_1, texfmt,
                             tlut, tlutfmt);
      SetTexel(sampled_tex, texel, (128 - fract_s) * (128 - fract_t));

      TexDecoder_DecodeTexel(sampled_tex, image_src, image_s_plus1, image_t, image_width_minus_1,
                             texfmt, tlut, tlutfmt);
      AddTexel(sampled_tex, texel, (fract_s) * (128 - fract_t));

      TexDecoder_DecodeTexel(sampled_tex, image_src, image_s, image_t_plus1, image_width_minus_1,
                             texfmt, tlut, tlutfmt);
      AddTexel(sampled_tex, texel, (128 - fract_s) * (fract_t));

      TexDecoder_DecodeTexel(sampled_tex, image_src, image_s_plus1, image_t_plus1, image_width_minus_1,
                             texfmt, tlut, tlutfmt);
      AddTexel(sampled_tex, texel, (fract_s) * (fract_t));
    }
    else
    {
      TexDecoder_DecodeTexelRGBA8FromTmem(sampled_tex, image_src, image_src_odd, image_s, image_t,
                                          image_width_minus_1);
      SetTexel(sampled_tex, texel, (128 - fract_s) * (128 - fract_t));

      TexDecoder_DecodeTexelRGBA8FromTmem(sampled_tex, image_src, image_src_odd, image_s_plus1, image_t,
                                          image_width_minus_1);
      AddTexel(sampled_tex, texel, (fract_s) * (128 - fract_t));

      TexDecoder_DecodeTexelRGBA8FromTmem(sampled_tex, image_src, image_src_odd, image_s, image_t_plus1,
                                          image_width_minus_1);
      AddTexel(sampled_tex, texel, (128 - fract_s) * (fract_t));

      TexDecoder_DecodeTexelRGBA8FromTmem(sampled_tex, image_src, image_src_odd, image_s_plus1,
                                          image_t_plus1, image_width_minus_1);
      AddTexel(sampled_tex, texel, (fract_s) * (fract_t));
    }

    sample[0] = (u8)(texel[0] >> 14);
    sample[1] = (u8)(texel[1] >> 14);
    sample[2] = (u8)(texel[2] >> 14);
    sample[3] = (u8)(texel[3] >> 14);
  }
  else
  {
    // integer part of sample location
    int image_s = s >> 7;
    int image_t = t >> 7;

    // nearest neighbor sampling
    WrapCoord(&image_s, tm0.wrap_s, image_width_minus_1 + 1);
    WrapCoord(&image_t, tm0.wrap_t, image_height_minus_1 + 1);

    if (!(texfmt == TextureFormat::RGBA8 && tex_unit.texImage1.cache_manually_managed))
    {
      TexDecoder_DecodeTexel(sample, image_src, image_s, image_t, image_width_minus_1, texfmt, tlut,
                             tlutfmt);
    }
    else
    {
      TexDecoder_DecodeTexelRGBA8FromTmem(sample, image_src, image_src_odd, image_s, image_t,
                                          image_width_minus_1);
    }
  }
}
}  // namespace TextureSampler

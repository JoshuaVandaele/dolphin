// Copyright 2009 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "VideoBackends/Software/Rasterizer.h"

#include <algorithm>
#include <cstring>
#include <vector>

#include "Common/Assert.h"
#include "Common/CommonTypes.h"

#include "VideoBackends/Software/NativeVertexFormat.h"
#include "VideoBackends/Software/SWEfbInterface.h"
#include "VideoBackends/Software/Tev.h"
#include "VideoCommon/BPFunctions.h"
#include "VideoCommon/BPMemory.h"
#include "VideoCommon/PerfQueryBase.h"
#include "VideoCommon/Statistics.h"
#include "VideoCommon/VideoCommon.h"

namespace Rasterizer
{
static constexpr int BLOCK_SIZE = 2;

struct SlopeContext
{
  SlopeContext(const OutputVertexData* v0, const OutputVertexData* v1, const OutputVertexData* v2,
               s32 x0_, s32 y0_, s32 x_off, s32 y_off)
      : x0(x0_), y0(y0_)
  {
    // adjust a little less than 0.5
    const float adjust = 0.495f;

    xOff = ((float)x0_ - (v0->screenPosition.x - x_off)) + adjust;
    yOff = ((float)y0_ - (v0->screenPosition.y - y_off)) + adjust;

    dx10 = v1->screenPosition.x - v0->screenPosition.x;
    dx20 = v2->screenPosition.x - v0->screenPosition.x;
    dy10 = v1->screenPosition.y - v0->screenPosition.y;
    dy20 = v2->screenPosition.y - v0->screenPosition.y;
  }
  s32 x0;
  s32 y0;
  float xOff;
  float yOff;
  float dx10;
  float dx20;
  float dy10;
  float dy20;
};

struct Slope
{
  Slope() = default;
  Slope(float f0_, float f1, float f2, const SlopeContext& ctx) : f0(f0_)
  {
    float delta_20 = f2 - f0_;
    float delta_10 = f1 - f0_;

    //        x2 - x0    y1 - y0    x1 - x0    y2 - y0
    float a = delta_20 * ctx.dy10 - delta_10 * ctx.dy20;
    float b = ctx.dx20 * delta_10 - ctx.dx10 * delta_20;
    float c = ctx.dx20 * ctx.dy10 - ctx.dx10 * ctx.dy20;

    dfdx = a / c;
    dfdy = b / c;

    x0 = ctx.x0;
    y0 = ctx.y0;
    xOff = ctx.xOff;
    yOff = ctx.yOff;
  }

  // These default values are used in the unlikely case that zfreeze is enabled when drawing the
  // first primitive.
  // TODO: This is just a guess!
  float dfdx = 0.0f;
  float dfdy = 0.0f;
  float f0 = 1.0f;

  // Both an s32 value and a float value are used to minimize rounding error
  // TODO: is this really needed?
  s32 x0 = 0;
  s32 y0 = 0;
  float xOff = 0.0f;
  float yOff = 0.0f;

  float GetValue(s32 x, s32 y) const
  {
    float dx = xOff + (float)(x - x0);
    float dy = yOff + (float)(y - y0);
    return f0 + (dfdx * dx) + (dfdy * dy);
  }
};

static Slope z_slope;
static Slope w_slope;
static Slope color_slopes[2][4];
static Slope tex_slopes[8][3];

static Tev tev;
static RasterBlock raster_block;

static std::vector<BPFunctions::ScissorRect> scissors;

void Init()
{
  // The other slopes are set each for each primitive drawn, but zfreeze means that the z slope
  // needs to be set to an (untested) default value.
  z_slope = Slope();
}

void ScissorChanged()
{
  scissors = std::move(BPFunctions::ComputeScissorRects().m_result);
}

// Returns approximation of log2(f) in s28.4
// results are close enough to use for LOD
static s32 FixedLog2(float f)
{
  u32 x;
  std::memcpy(&x, &f, sizeof(u32));

  s32 log_int = ((x & 0x7F800000) >> 19) - 2032;  // integer part
  s32 log_fract = (x & 0x007fffff) >> 19;         // approximate fractional part

  return log_int + log_fract;
}

static inline int Iround(float x)
{
  int t = (int)x;
  if ((x - t) >= 0.5)
    return t + 1;

  return t;
}

void SetTevKonstColors()
{
  tev.SetKonstColors();
}

static void Draw(s32 x, s32 y, s32 xi, s32 yi)
{
  INCSTAT(g_stats.this_frame.rasterized_pixels);

  s32 z = (s32)std::clamp<float>(z_slope.GetValue(x, y), 0.0f, 16777215.0f);

  if (bpmem.GetEmulatedZ() == EmulatedZ::Early)
  {
    // TODO: Test if perf regs are incremented even if test is disabled
    EfbInterface::IncPerfCounterQuadCount(PQ_ZCOMP_INPUT_ZCOMPLOC);
    if (bpmem.zmode.testenable)
    {
      // early z
      if (!EfbInterface::ZCompare(x, y, z))
        return;
    }
    EfbInterface::IncPerfCounterQuadCount(PQ_ZCOMP_OUTPUT_ZCOMPLOC);
  }

  RasterBlockPixel& pixel = raster_block.Pixel[xi][yi];

  tev.Position[0] = x;
  tev.Position[1] = y;
  tev.Position[2] = z;

  //  colors
  for (unsigned int i = 0; i < bpmem.genMode.numcolchans; i++)
  {
    for (int comp = 0; comp < 4; comp++)
    {
      u16 color = (u16)color_slopes[i][comp].GetValue(x, y);

      // clamp color value to 0
      u16 mask = ~(color >> 8);

      tev.Color[i][comp] = color & mask;
    }
  }

  // tex coords
  for (unsigned int i = 0; i < bpmem.genMode.numtexgens; i++)
  {
    // multiply by 128 because TEV stores UVs as s17.7
    tev.Uv[i].s = (s32)(pixel.Uv[i][0] * 128);
    tev.Uv[i].t = (s32)(pixel.Uv[i][1] * 128);
  }

  for (unsigned int i = 0; i < bpmem.genMode.numindstages; i++)
  {
    tev.IndirectLod[i] = raster_block.IndirectLod[i];
    tev.IndirectLinear[i] = raster_block.IndirectLinear[i];
  }

  for (unsigned int i = 0; i <= bpmem.genMode.numtevstages; i++)
  {
    tev.TextureLod[i] = raster_block.TextureLod[i];
    tev.TextureLinear[i] = raster_block.TextureLinear[i];
  }

  tev.Draw();
}

static inline void CalculateLOD(s32* lodp, bool* linear, u32 texmap, u32 texcoord)
{
  auto tex_unit = bpmem.tex.GetUnit(texmap);

  // LOD calculation requires data from the texture mode for bias, etc.
  // it does not seem to use the actual texture size
  const TexMode0& tm0 = tex_unit.texMode0;
  const TexMode1& tm1 = tex_unit.texMode1;

  float s_delta, t_delta;

  float* uv00 = raster_block.Pixel[0][0].Uv[texcoord];
  float* uv10 = raster_block.Pixel[1][0].Uv[texcoord];
  float* uv01 = raster_block.Pixel[0][1].Uv[texcoord];

  float dudx = fabsf(uv00[0] - uv10[0]);
  float dvdx = fabsf(uv00[1] - uv10[1]);
  float dudy = fabsf(uv00[0] - uv01[0]);
  float dvdy = fabsf(uv00[1] - uv01[1]);

  if (tm0.diag_lod == LODType::Diagonal)
  {
    s_delta = dudx + dudy;
    t_delta = dvdx + dvdy;
  }
  else
  {
    s_delta = std::max(dudx, dudy);
    t_delta = std::max(dvdx, dvdy);
  }

  // get LOD in s28.4
  s32 lod = FixedLog2(std::max(s_delta, t_delta));

  // bias is s2.5
  int bias = tm0.lod_bias;
  bias >>= 1;
  lod += bias;

  *linear = ((lod > 0 && tm0.min_filter == FilterMode::Linear) ||
             (lod <= 0 && tm0.mag_filter == FilterMode::Linear));

  // NOTE: The order of comparisons for this clamp check matters.
  if (lod > static_cast<s32>(tm1.max_lod))
    lod = static_cast<s32>(tm1.max_lod);
  else if (lod < static_cast<s32>(tm1.min_lod))
    lod = static_cast<s32>(tm1.min_lod);

  *lodp = lod;
}

static void BuildBlock(s32 blockX, s32 blockY)
{
  for (s32 yi = 0; yi < BLOCK_SIZE; yi++)
  {
    for (s32 xi = 0; xi < BLOCK_SIZE; xi++)
    {
      RasterBlockPixel& pixel = raster_block.Pixel[xi][yi];

      s32 x = xi + blockX;
      s32 y = yi + blockY;

      float inv_w = 1.0f / w_slope.GetValue(x, y);
      pixel.InvW = inv_w;

      // tex coords
      for (unsigned int i = 0; i < bpmem.genMode.numtexgens; i++)
      {
        float projection = inv_w;
        float q = tex_slopes[i][2].GetValue(x, y) * inv_w;
        if (q != 0.0f)
          projection = inv_w / q;

        pixel.Uv[i][0] = tex_slopes[i][0].GetValue(x, y) * projection;
        pixel.Uv[i][1] = tex_slopes[i][1].GetValue(x, y) * projection;
      }
    }
  }

  for (unsigned int i = 0; i < bpmem.genMode.numindstages; i++)
  {
    u32 texmap = bpmem.tevindref.getTexMap(i);
    u32 texcoord = bpmem.tevindref.getTexCoord(i);

    CalculateLOD(&raster_block.IndirectLod[i], &raster_block.IndirectLinear[i], texmap, texcoord);
  }

  for (unsigned int i = 0; i <= bpmem.genMode.numtevstages; i++)
  {
    int stage_odd = i & 1;
    const TwoTevStageOrders& order = bpmem.tevorders[i >> 1];
    if (order.getEnable(stage_odd))
    {
      u32 texmap = order.getTexMap(stage_odd);
      u32 texcoord = order.getTexCoord(stage_odd);

      CalculateLOD(&raster_block.TextureLod[i], &raster_block.TextureLinear[i], texmap, texcoord);
    }
  }
}

void UpdateZSlope(const OutputVertexData* v0, const OutputVertexData* v1,
                  const OutputVertexData* v2, s32 x_off, s32 y_off)
{
  if (!bpmem.genMode.zfreeze)
  {
    const s32 x1 = Iround(16.0f * (v0->screenPosition.x - x_off)) - 9;
    const s32 y1 = Iround(16.0f * (v0->screenPosition.y - y_off)) - 9;
    const SlopeContext ctx(v0, v1, v2, (x1 + 0xF) >> 4, (y1 + 0xF) >> 4, x_off, y_off);
    z_slope = Slope(v0->screenPosition.z, v1->screenPosition.z, v2->screenPosition.z, ctx);
  }
}

static void DrawTriangleFrontFace(const OutputVertexData* v0, const OutputVertexData* v1,
                                  const OutputVertexData* v2,
                                  const BPFunctions::ScissorRect& scissor)
{
  // The zslope should be updated now, even if the triangle is rejected by the scissor test, as
  // zfreeze depends on it
  UpdateZSlope(v0, v1, v2, scissor.x_off, scissor.y_off);

  // adapted from http://devmaster.net/posts/6145/advanced-rasterization

  // 28.4 fixed-point coordinates. rounded to nearest and adjusted to match hardware output
  // could also take floor and adjust -8
  const s32 y1 = Iround(16.0f * (v0->screenPosition.y - scissor.y_off)) - 9;
  const s32 y2 = Iround(16.0f * (v1->screenPosition.y - scissor.y_off)) - 9;
  const s32 y3 = Iround(16.0f * (v2->screenPosition.y - scissor.y_off)) - 9;

  const s32 x1 = Iround(16.0f * (v0->screenPosition.x - scissor.x_off)) - 9;
  const s32 x2 = Iround(16.0f * (v1->screenPosition.x - scissor.x_off)) - 9;
  const s32 x3 = Iround(16.0f * (v2->screenPosition.x - scissor.x_off)) - 9;

  // Deltas
  const s32 d_x12 = x1 - x2;
  const s32 d_x23 = x2 - x3;
  const s32 d_x31 = x3 - x1;

  const s32 d_y12 = y1 - y2;
  const s32 d_y23 = y2 - y3;
  const s32 d_y31 = y3 - y1;

  // Fixed-point deltas
  const s32 fd_x12 = d_x12 * 16;
  const s32 fd_x23 = d_x23 * 16;
  const s32 fd_x31 = d_x31 * 16;

  const s32 fd_y12 = d_y12 * 16;
  const s32 fd_y23 = d_y23 * 16;
  const s32 fd_y31 = d_y31 * 16;

  // Bounding rectangle
  s32 minx = (std::min(std::min(x1, x2), x3) + 0xF) >> 4;
  s32 maxx = (std::max(std::max(x1, x2), x3) + 0xF) >> 4;
  s32 miny = (std::min(std::min(y1, y2), y3) + 0xF) >> 4;
  s32 maxy = (std::max(std::max(y1, y2), y3) + 0xF) >> 4;

  // scissor
  ASSERT(scissor.rect.left >= 0);
  ASSERT(scissor.rect.right <= static_cast<int>(EFB_WIDTH));
  ASSERT(scissor.rect.top >= 0);
  ASSERT(scissor.rect.bottom <= static_cast<int>(EFB_HEIGHT));

  minx = std::max(minx, scissor.rect.left);
  maxx = std::min(maxx, scissor.rect.right);
  miny = std::max(miny, scissor.rect.top);
  maxy = std::min(maxy, scissor.rect.bottom);

  if (minx >= maxx || miny >= maxy)
    return;

  // Set up the remaining slopes
  const SlopeContext ctx(v0, v1, v2, (x1 + 0xF) >> 4, (y1 + 0xF) >> 4, scissor.x_off,
                         scissor.y_off);

  float w[3] = {1.0f / v0->projectedPosition.w, 1.0f / v1->projectedPosition.w,
                1.0f / v2->projectedPosition.w};
  w_slope = Slope(w[0], w[1], w[2], ctx);

  for (unsigned int i = 0; i < bpmem.genMode.numcolchans; i++)
  {
    for (int comp = 0; comp < 4; comp++)
      color_slopes[i][comp] =
          Slope(v0->color[i][comp], v1->color[i][comp], v2->color[i][comp], ctx);
  }

  for (unsigned int i = 0; i < bpmem.genMode.numtexgens; i++)
  {
    for (int comp = 0; comp < 3; comp++)
    {
      tex_slopes[i][comp] = Slope(v0->texCoords[i][comp] * w[0], v1->texCoords[i][comp] * w[1],
                                  v2->texCoords[i][comp] * w[2], ctx);
    }
  }

  // Half-edge constants
  s32 c1 = d_y12 * x1 - d_x12 * y1;
  s32 c2 = d_y23 * x2 - d_x23 * y2;
  s32 c3 = d_y31 * x3 - d_x31 * y3;

  // Correct for fill convention
  if (d_y12 < 0 || (d_y12 == 0 && d_x12 > 0))
    c1++;
  if (d_y23 < 0 || (d_y23 == 0 && d_x23 > 0))
    c2++;
  if (d_y31 < 0 || (d_y31 == 0 && d_x31 > 0))
    c3++;

  // Start in corner of 2x2 block
  s32 block_minx = minx & ~(BLOCK_SIZE - 1);
  s32 block_miny = miny & ~(BLOCK_SIZE - 1);

  // Loop through blocks
  for (s32 y = block_miny & ~(BLOCK_SIZE - 1); y < maxy; y += BLOCK_SIZE)
  {
    for (s32 x = block_minx; x < maxx; x += BLOCK_SIZE)
    {
      s32 block_x1 = (x + BLOCK_SIZE - 1);
      s32 block_y1 = (y + BLOCK_SIZE - 1);

      // Corners of block
      s32 corner_x0 = x << 4;
      s32 corner_x1 = block_x1 << 4;
      s32 corner_y0 = y << 4;
      s32 corner_y1 = block_y1 << 4;

      // Evaluate half-space functions
      bool a00 = c1 + d_x12 * corner_y0 - d_y12 * corner_x0 > 0;
      bool a10 = c1 + d_x12 * corner_y0 - d_y12 * corner_x1 > 0;
      bool a01 = c1 + d_x12 * corner_y1 - d_y12 * corner_x0 > 0;
      bool a11 = c1 + d_x12 * corner_y1 - d_y12 * corner_x1 > 0;
      int a = (a00 << 0) | (a10 << 1) | (a01 << 2) | (a11 << 3);

      bool b00 = c2 + d_x23 * corner_y0 - d_y23 * corner_x0 > 0;
      bool b10 = c2 + d_x23 * corner_y0 - d_y23 * corner_x1 > 0;
      bool b01 = c2 + d_x23 * corner_y1 - d_y23 * corner_x0 > 0;
      bool b11 = c2 + d_x23 * corner_y1 - d_y23 * corner_x1 > 0;
      int b = (b00 << 0) | (b10 << 1) | (b01 << 2) | (b11 << 3);

      bool c00 = c3 + d_x31 * corner_y0 - d_y31 * corner_x0 > 0;
      bool c10 = c3 + d_x31 * corner_y0 - d_y31 * corner_x1 > 0;
      bool c01 = c3 + d_x31 * corner_y1 - d_y31 * corner_x0 > 0;
      bool c11 = c3 + d_x31 * corner_y1 - d_y31 * corner_x1 > 0;
      int c = (c00 << 0) | (c10 << 1) | (c01 << 2) | (c11 << 3);

      // Skip block when outside an edge
      if (a == 0x0 || b == 0x0 || c == 0x0)
        continue;

      BuildBlock(x, y);

      // Accept whole block when totally covered
      // We still need to check min/max x/y because of the scissor
      if (a == 0xF && b == 0xF && c == 0xF && x >= minx && corner_x1 < maxx && y >= miny &&
          corner_y1 < maxy)
      {
        for (s32 iy = 0; iy < BLOCK_SIZE; iy++)
        {
          for (s32 ix = 0; ix < BLOCK_SIZE; ix++)
          {
            Draw(x + ix, y + iy, ix, iy);
          }
        }
      }
      else  // Partially covered block
      {
        s32 c_y1 = c1 + d_x12 * corner_y0 - d_y12 * corner_x0;
        s32 c_y2 = c2 + d_x23 * corner_y0 - d_y23 * corner_x0;
        s32 c_y3 = c3 + d_x31 * corner_y0 - d_y31 * corner_x0;

        for (s32 iy = 0; iy < BLOCK_SIZE; iy++)
        {
          s32 c_x1 = c_y1;
          s32 c_x2 = c_y2;
          s32 c_x3 = c_y3;

          for (s32 ix = 0; ix < BLOCK_SIZE; ix++)
          {
            if (c_x1 > 0 && c_x2 > 0 && c_x3 > 0)
            {
              // This check enforces the scissor rectangle, since it might not be aligned with the
              // blocks
              if (x + ix >= minx && x + ix < maxx && y + iy >= miny && y + iy < maxy)
                Draw(x + ix, y + iy, ix, iy);
            }

            c_x1 -= fd_y12;
            c_x2 -= fd_y23;
            c_x3 -= fd_y31;
          }

          c_y1 += fd_x12;
          c_y2 += fd_x23;
          c_y3 += fd_x31;
        }
      }
    }
  }
}

void DrawTriangleFrontFace(const OutputVertexData* v0, const OutputVertexData* v1,
                           const OutputVertexData* v2)
{
  INCSTAT(g_stats.this_frame.num_triangles_drawn);

  for (const auto& scissor : scissors)
    DrawTriangleFrontFace(v0, v1, v2, scissor);
}
}  // namespace Rasterizer

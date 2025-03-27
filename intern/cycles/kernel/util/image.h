
/* SPDX-FileCopyrightText: 2011-2022 Blender Foundation
 *
 * SPDX-License-Identifier: Apache-2.0 */

#pragma once

#include "kernel/globals.h"

#include "util/math_fast.h"
#include "util/texture.h"

CCL_NAMESPACE_BEGIN

ccl_device_forceinline KernelTileDescriptor
kernel_image_tile_map(KernelGlobals kg,
                      const ccl_global KernelImageTexture &tex,
                      ccl_private float &x,
                      ccl_private float &y,
                      const differential2 dxy)
{
  /* Find mipmap level. */

  // TODO: make this faster
  // TODO: find good ratio, check how OIIO handles anisotropy
  const float dudxy = len(make_float2(dxy.dx.x, dxy.dy.x)) * float(tex.width);
  const float dvdxy = len(make_float2(dxy.dx.y, dxy.dy.y)) * float(tex.height);

  const float maxdxy = max(dudxy, dvdxy);
  const float mindxy = min(dudxy, dvdxy);
  const float max_aniso_ratio = 16.0f;
  const float sampledxy = max(mindxy, maxdxy * (1.0f / max_aniso_ratio));

  const float flevel = fast_log2f(sampledxy);
  const int level = clamp(int(flevel), 0, tex.tile_levels - 1);

  const int width = divide_up_by_shift(tex.width, level);
  const int height = divide_up_by_shift(tex.height, level);

  x *= width;
  y *= height;

  /* Tile mapping */
  const int ix = clamp((int)x, 0, width - 1);
  const int iy = clamp((int)y, 0, height - 1);
  const int tile_size_shift = tex.tile_size_shift;
  const int tile_size_padded = (1 << tile_size_shift) + KERNEL_IMAGE_TEX_PADDING * 2;

  const int tile_x = ix >> tile_size_shift;
  const int tile_y = iy >> tile_size_shift;
  const int tile_offset = kernel_data_fetch(image_texture_tile_descriptors,
                                            tex.tile_descriptor_offset + level) +
                          tile_x + tile_y * divide_up_by_shift(width, tile_size_shift);

  const KernelTileDescriptor tile_descriptor = kernel_data_fetch(
      image_texture_tile_descriptors, tex.tile_descriptor_offset + tile_offset);

  if (!kernel_tile_descriptor_loaded(tile_descriptor)) {
    if (tile_descriptor == KERNEL_TILE_LOAD_NONE) {
      kernel_data_assign(image_texture_tile_descriptors,
                         tex.tile_descriptor_offset + tile_offset,
                         KERNEL_TILE_LOAD_REQUEST);
    }

    return tile_descriptor;
  }

  /* Remap coordinates into tiled image space. */
  const int offset = kernel_tile_descriptor_offset(tile_descriptor);
  x += KERNEL_IMAGE_TEX_PADDING - (tile_x << tile_size_shift) + offset * tile_size_padded;
  y += KERNEL_IMAGE_TEX_PADDING - (tile_y << tile_size_shift);

  return tile_descriptor;
}

CCL_NAMESPACE_END

/* SPDX-FileCopyrightText: 2011-2022 Blender Foundation
 *
 * SPDX-License-Identifier: Apache-2.0 */

#pragma once

#include "scene/image.h"

#include "util/image.h"

CCL_NAMESPACE_BEGIN

class OIIOImageLoader : public ImageLoader {
 public:
  OIIOImageLoader(const string &filepath);
  ~OIIOImageLoader() override;

  bool load_metadata(const ImageDeviceFeatures &features, ImageMetaData &metadata) override;

  bool load_pixels_full(const ImageMetaData &metadata, uint8_t *pixels) override;

  bool load_pixels_tile(const ImageMetaData &metadata,
                        const int miplevel,
                        const int64_t x,
                        const int64_t y,
                        const int64_t w,
                        const int64_t h,
                        const int64_t x_stride,
                        const int64_t y_stride,
                        const int64_t padding,
                        uint8_t *pixels) override;

  void drop_file_handle() override;

  string name() const override;

  ustring osl_filepath() const override;

  bool equals(const ImageLoader &other) const override;

 protected:
  ustring filepath;
  unique_ptr<ImageInput> filehandle;
};

CCL_NAMESPACE_END

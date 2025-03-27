/* SPDX-FileCopyrightText: 2011-2025 Blender Foundation
 *
 * SPDX-License-Identifier: Apache-2.0 */

#pragma once

#include "device/device.h"
#include "device/memory.h"

#include "util/set.h"
#include "util/unique_ptr_vector.h"

CCL_NAMESPACE_BEGIN

class ImageCache {
  // TODO: speed up finding free tiles with some type of hash
#if 0
  struct DeviceImageKey {
    ImageDataType type;
    int tile_size;

    bool operator==(const DeviceImageKey &other) const
    {
      return type == other.type && tile_size == other.tile_size;
    }

    struct Hash {
      size_t operator()(const DeviceImageKey &key) const
      {
        return hash_uint2(key.type, key.tile_size);
      }
    };
  };
#endif

  thread_mutex device_mutex;

  struct DeviceImage {
    DeviceImage(Device *device,
                std::string &&name,
                const uint slot,
                const ImageDataType type,
                const InterpolationType interpolation,
                const ExtensionType extension);

    string mem_name;
    device_image mem;
    vector<bool> occupied;
    int num_free = 0;
  };

  unique_ptr_vector<DeviceImage> device_images;
  set<device_image *> updated_device_images;

 public:
  ImageCache();
  ~ImageCache();

  device_image &alloc_full(Device *device,
                           const ImageDataType type,
                           const InterpolationType interpolation,
                           const ExtensionType extension,
                           const int64_t width,
                           const int64_t height,
                           uint &slot);
  void free_full(const uint slot);

  device_image &alloc_tile(Device *device,
                           ImageDataType type,
                           InterpolationType interpolation,
                           ExtensionType extension,
                           const int tile_size_padded,
                           KernelTileDescriptor &r_tile_descriptor);
  void free_tile(const KernelTileDescriptor tile);

  void copy_to_device_if_modified();

  void device_free();
};

CCL_NAMESPACE_END

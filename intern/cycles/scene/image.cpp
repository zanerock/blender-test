/* SPDX-FileCopyrightText: 2011-2022 Blender Foundation
 *
 * SPDX-License-Identifier: Apache-2.0 */

#include "scene/image.h"
#include "scene/colorspace.h"
#include "scene/image_oiio.h"
#include "scene/image_vdb.h"
#include "scene/scene.h"
#include "scene/stats.h"

#include "util/image.h"
#include "util/image_impl.h"
#include "util/log.h"
#include "util/progress.h"
#include "util/task.h"
#include "util/texture.h"
#include "util/types_base.h"
#include <type_traits>

#ifdef WITH_OSL
#  include <OSL/oslexec.h>
#endif

CCL_NAMESPACE_BEGIN

/* Image Handle */

ImageHandle::ImageHandle() : manager(nullptr) {}

ImageHandle::ImageHandle(const ImageHandle &other)
    : slots(other.slots), is_tiled(other.is_tiled), manager(other.manager)
{
  /* Increase image user count. */
  for (const size_t slot : slots) {
    manager->add_image_user(slot);
  }
}

ImageHandle &ImageHandle::operator=(const ImageHandle &other)
{
  clear();
  manager = other.manager;
  is_tiled = other.is_tiled;
  slots = other.slots;

  for (const size_t slot : slots) {
    manager->add_image_user(slot);
  }

  return *this;
}

ImageHandle::~ImageHandle()
{
  clear();
}

void ImageHandle::clear()
{
  for (const size_t slot : slots) {
    manager->remove_image_user(slot);
  }

  slots.clear();
  manager = nullptr;
}

bool ImageHandle::empty() const
{
  return slots.empty();
}

int ImageHandle::num_tiles() const
{
  return (is_tiled) ? slots.size() : 0;
}

int ImageHandle::num_svm_slots() const
{
  return slots.size();
}

ImageMetaData ImageHandle::metadata()
{
  if (slots.empty()) {
    return ImageMetaData();
  }

  ImageManager::Image *img = manager->get_image_slot(slots.front());
  manager->load_image_metadata(img);
  return img->metadata;
}

int ImageHandle::svm_slot(const int slot_index) const
{
  if (slot_index >= slots.size()) {
    return -1;
  }

  if (manager->osl_texture_system) {
    ImageManager::Image *img = manager->get_image_slot(slots[slot_index]);
    if (!img->loader->osl_filepath().empty()) {
      return -1;
    }
  }

  return slots[slot_index];
}

vector<int4> ImageHandle::get_svm_slots() const
{
  const size_t num_nodes = divide_up(slots.size(), 2);

  vector<int4> svm_slots;
  svm_slots.reserve(num_nodes);
  for (size_t i = 0; i < num_nodes; i++) {
    int4 node;

    size_t slot = slots[2 * i];
    node.x = manager->get_image_slot(slot)->loader->get_tile_number();
    node.y = slot;

    if ((2 * i + 1) < slots.size()) {
      slot = slots[2 * i + 1];
      node.z = manager->get_image_slot(slot)->loader->get_tile_number();
      node.w = slot;
    }
    else {
      node.z = -1;
      node.w = -1;
    }

    svm_slots.push_back(node);
  }

  return svm_slots;
}

device_image *ImageHandle::vdb_image_memory() const
{
  if (slots.empty()) {
    return nullptr;
  }

  ImageManager::Image *img = manager->get_image_slot(slots[0]);
  return img ? img->vdb_memory : nullptr;
}

VDBImageLoader *ImageHandle::vdb_loader() const
{
  if (slots.empty()) {
    return nullptr;
  }

  ImageManager::Image *img = manager->get_image_slot(slots[0]);

  if (img == nullptr) {
    return nullptr;
  }

  ImageLoader *loader = img->loader.get();

  if (loader == nullptr) {
    return nullptr;
  }

  if (loader->is_vdb_loader()) {
    return dynamic_cast<VDBImageLoader *>(loader);
  }

  return nullptr;
}

ImageManager *ImageHandle::get_manager() const
{
  return manager;
}

bool ImageHandle::operator==(const ImageHandle &other) const
{
  return manager == other.manager && is_tiled == other.is_tiled && slots == other.slots;
}

/* Image MetaData */

ImageMetaData::ImageMetaData()
    : channels(0),
      width(0),
      height(0),
      depth(0),
      byte_size(0),
      type(IMAGE_DATA_NUM_TYPES),
      colorspace(u_colorspace_raw),
      colorspace_file_format(""),
      use_transform_3d(false),
      compress_as_srgb(false),
      associate_alpha(false),
      tile_size(0),
      average_color(zero_float4())
{
}

bool ImageMetaData::operator==(const ImageMetaData &other) const
{
  return channels == other.channels && width == other.width && height == other.height &&
         depth == other.depth && use_transform_3d == other.use_transform_3d &&
         (!use_transform_3d || transform_3d == other.transform_3d) && type == other.type &&
         colorspace == other.colorspace && compress_as_srgb == other.compress_as_srgb;
}

bool ImageMetaData::is_float() const
{
  return (type == IMAGE_DATA_TYPE_FLOAT || type == IMAGE_DATA_TYPE_FLOAT4 ||
          type == IMAGE_DATA_TYPE_HALF || type == IMAGE_DATA_TYPE_HALF4);
}

void ImageMetaData::detect_colorspace()
{
  /* Convert used specified color spaces to one we know how to handle. */
  colorspace = ColorSpaceManager::detect_known_colorspace(
      colorspace, colorspace_file_hint.c_str(), colorspace_file_format, is_float());

  if (colorspace == u_colorspace_raw) {
    /* Nothing to do. */
  }
  else if (colorspace == u_colorspace_srgb) {
    /* Keep sRGB colorspace stored as sRGB, to save memory and/or loading time
     * for the common case of 8bit sRGB images like PNG. */
    compress_as_srgb = true;
  }
  else {
    /* If colorspace conversion needed, use half instead of short so we can
     * represent HDR values that might result from conversion. */
    if (type == IMAGE_DATA_TYPE_BYTE || type == IMAGE_DATA_TYPE_USHORT) {
      type = IMAGE_DATA_TYPE_HALF;
    }
    else if (type == IMAGE_DATA_TYPE_BYTE4 || type == IMAGE_DATA_TYPE_USHORT4) {
      type = IMAGE_DATA_TYPE_HALF4;
    }
  }
}

/* Image Loader */

ImageLoader::ImageLoader() = default;

ustring ImageLoader::osl_filepath() const
{
  return ustring();
}

int ImageLoader::get_tile_number() const
{
  return 0;
}

bool ImageLoader::equals(const ImageLoader *a, const ImageLoader *b)
{
  if (a == nullptr && b == nullptr) {
    return true;
  }
  return (a && b && typeid(*a) == typeid(*b) && a->equals(*b));
}

bool ImageLoader::is_vdb_loader() const
{
  return false;
}

/* Image Manager */

ImageManager::ImageManager(const DeviceInfo &info)
{
  need_update_ = true;
  osl_texture_system = nullptr;
  animation_frame = 0;

  /* Set image limits */
  features.has_nanovdb = info.has_nanovdb;
}

ImageManager::~ImageManager()
{
  for (size_t slot = 0; slot < images.size(); slot++) {
    assert(!images[slot]);
  }
}

void ImageManager::set_osl_texture_system(void *texture_system)
{
  osl_texture_system = texture_system;
}

bool ImageManager::set_animation_frame_update(const int frame)
{
  if (frame != animation_frame) {
    const thread_scoped_lock device_lock(images_mutex);
    animation_frame = frame;

    for (size_t slot = 0; slot < images.size(); slot++) {
      if (images[slot] && images[slot]->params.animated) {
        return true;
      }
    }
  }

  return false;
}

void ImageManager::load_image_metadata(Image *img)
{
  if (!img->need_metadata) {
    return;
  }

  const thread_scoped_lock image_lock(img->mutex);
  if (!img->need_metadata) {
    return;
  }

  ImageMetaData &metadata = img->metadata;
  metadata = ImageMetaData();
  metadata.colorspace = img->params.colorspace;

  if (img->loader->load_metadata(features, metadata)) {
    assert(metadata.type != IMAGE_DATA_NUM_TYPES);
  }
  else {
    metadata.type = IMAGE_DATA_TYPE_BYTE4;
  }

  metadata.detect_colorspace();

  /* For typical RGBA images we let OIIO convert to associated alpha,
   * but some types we want to leave the RGB channels untouched. */
  metadata.associate_alpha = metadata.associate_alpha &&
                             !(ColorSpaceManager::colorspace_is_data(img->params.colorspace) ||
                               img->params.alpha_type == IMAGE_ALPHA_IGNORE ||
                               img->params.alpha_type == IMAGE_ALPHA_CHANNEL_PACKED);

  assert(features.has_nanovdb || (metadata.type != IMAGE_DATA_TYPE_NANOVDB_FLOAT ||
                                  metadata.type != IMAGE_DATA_TYPE_NANOVDB_FLOAT3 ||
                                  metadata.type != IMAGE_DATA_TYPE_NANOVDB_FPN ||
                                  metadata.type != IMAGE_DATA_TYPE_NANOVDB_FP16));

  img->need_metadata = false;
}

ImageHandle ImageManager::add_image(const string &filename, const ImageParams &params)
{
  const size_t slot = add_image_slot(make_unique<OIIOImageLoader>(filename), params, false);

  ImageHandle handle;
  handle.slots.push_back(slot);
  handle.manager = this;
  return handle;
}

ImageHandle ImageManager::add_image(const string &filename,
                                    const ImageParams &params,
                                    const array<int> &tiles)
{
  ImageHandle handle;
  handle.manager = this;
  handle.is_tiled = !tiles.empty();

  if (!handle.is_tiled) {
    const size_t slot = add_image_slot(make_unique<OIIOImageLoader>(filename), params, false);
    handle.slots.push_back(slot);
    return handle;
  }

  for (const int tile : tiles) {
    string tile_filename = filename;

    /* Since we don't have information about the exact tile format used in this code location,
     * just attempt all replacement patterns that Blender supports. */
    string_replace(tile_filename, "<UDIM>", string_printf("%04d", tile));

    const int u = ((tile - 1001) % 10);
    const int v = ((tile - 1001) / 10);
    string_replace(tile_filename, "<UVTILE>", string_printf("u%d_v%d", u + 1, v + 1));

    const size_t slot = add_image_slot(make_unique<OIIOImageLoader>(tile_filename), params, false);
    handle.slots.push_back(slot);
  }

  return handle;
}

ImageHandle ImageManager::add_image(unique_ptr<ImageLoader> &&loader,
                                    const ImageParams &params,
                                    const bool builtin)
{
  const size_t slot = add_image_slot(std::move(loader), params, builtin);

  ImageHandle handle;
  handle.slots.push_back(slot);
  handle.manager = this;
  return handle;
}

ImageHandle ImageManager::add_image(vector<unique_ptr<ImageLoader>> &&loaders,
                                    const ImageParams &params)
{
  ImageHandle handle;
  handle.is_tiled = true;

  for (unique_ptr<ImageLoader> &loader : loaders) {
    unique_ptr<ImageLoader> local_loader;
    std::swap(loader, local_loader);
    const size_t slot = add_image_slot(std::move(local_loader), params, true);
    handle.slots.push_back(slot);
  }

  handle.manager = this;
  return handle;
}

/* ImageManager */

size_t ImageManager::add_image_slot(unique_ptr<ImageLoader> &&loader,
                                    const ImageParams &params,
                                    const bool builtin)
{
  size_t slot;

  const thread_scoped_lock device_lock(images_mutex);

  /* Find existing image. */
  for (slot = 0; slot < images.size(); slot++) {
    Image *img = images[slot].get();
    if (img && ImageLoader::equals(img->loader.get(), loader.get()) && img->params == params) {
      img->users++;
      return slot;
    }
  }

  /* Find free slot. */
  for (slot = 0; slot < images.size(); slot++) {
    if (!images[slot]) {
      break;
    }
  }

  if (slot == images.size()) {
    images.resize(images.size() + 1);
  }

  /* Add new image. */
  unique_ptr<Image> img = make_unique<Image>();
  img->params = params;
  img->loader = std::move(loader);
  img->need_metadata = true;
  img->need_load = !(osl_texture_system && !img->loader->osl_filepath().empty());
  img->builtin = builtin;
  img->users = 1;
  img->texture_slot = KERNEL_IMAGE_TEX_NONE;
  img->tile_descriptor_offset = KERNEL_IMAGE_TEX_NONE;
  img->tile_descriptor_levels = 0;
  img->tile_descriptor_num = 0;

  images[slot] = std::move(img);

  need_update_ = true;

  return slot;
}

void ImageManager::add_image_user(const size_t slot)
{
  const thread_scoped_lock device_lock(images_mutex);
  Image *image = images[slot].get();
  assert(image && image->users >= 1);

  image->users++;
}

void ImageManager::remove_image_user(const size_t slot)
{
  const thread_scoped_lock device_lock(images_mutex);
  Image *image = images[slot].get();
  assert(image && image->users >= 1);

  /* decrement user count */
  image->users--;

  /* don't remove immediately, rather do it all together later on. one of
   * the reasons for this is that on shader changes we add and remove nodes
   * that use them, but we do not want to reload the image all the time. */
  if (image->users == 0) {
    need_update_ = true;
  }
}

ImageManager::Image *ImageManager::get_image_slot(const size_t slot)
{
  /* Need mutex lock, images vector might get resized by another thread. */
  const thread_scoped_lock device_lock(images_mutex);
  return images[slot].get();
}

template<typename StorageType>
static bool conform_pixels_to_metadata_type(const ImageManager::Image *img,
                                            StorageType *pixels,
                                            const int64_t num_pixels)
{
  /* The kernel can handle 1 and 4 channel images. Anything that is not a single
   * channel image is converted to RGBA format. */
  const ImageMetaData &metadata = img->metadata;
  const int channels = metadata.channels;
  const bool is_rgba = (metadata.type == IMAGE_DATA_TYPE_FLOAT4 ||
                        metadata.type == IMAGE_DATA_TYPE_HALF4 ||
                        metadata.type == IMAGE_DATA_TYPE_BYTE4 ||
                        metadata.type == IMAGE_DATA_TYPE_USHORT4);

  if (is_rgba) {
    const StorageType one = util_image_cast_from_float<StorageType>(1.0f);

    if (channels == 2) {
      /* Grayscale + alpha to RGBA. */
      for (int64_t i = num_pixels - 1, pixel = 0; pixel < num_pixels; pixel++, i--) {
        pixels[i * 4 + 3] = pixels[i * 2 + 1];
        pixels[i * 4 + 2] = pixels[i * 2 + 0];
        pixels[i * 4 + 1] = pixels[i * 2 + 0];
        pixels[i * 4 + 0] = pixels[i * 2 + 0];
      }
    }
    else if (channels == 3) {
      /* RGB to RGBA. */
      for (int64_t i = num_pixels - 1, pixel = 0; pixel < num_pixels; pixel++, i--) {
        pixels[i * 4 + 3] = one;
        pixels[i * 4 + 2] = pixels[i * 3 + 2];
        pixels[i * 4 + 1] = pixels[i * 3 + 1];
        pixels[i * 4 + 0] = pixels[i * 3 + 0];
      }
    }
    else if (channels == 1) {
      /* Grayscale to RGBA. */
      for (int64_t i = num_pixels - 1, pixel = 0; pixel < num_pixels; pixel++, i--) {
        pixels[i * 4 + 3] = one;
        pixels[i * 4 + 2] = pixels[i];
        pixels[i * 4 + 1] = pixels[i];
        pixels[i * 4 + 0] = pixels[i];
      }
    }

    /* Disable alpha if requested by the user. */
    if (img->params.alpha_type == IMAGE_ALPHA_IGNORE) {
      for (int64_t i = num_pixels - 1, pixel = 0; pixel < num_pixels; pixel++, i--) {
        pixels[i * 4 + 3] = one;
      }
    }
  }

  if (metadata.colorspace != u_colorspace_raw && metadata.colorspace != u_colorspace_srgb) {
    /* Convert to scene linear. */
    ColorSpaceManager::to_scene_linear(
        metadata.colorspace, pixels, num_pixels, is_rgba, metadata.compress_as_srgb);
  }

  /* Make sure we don't have buggy values. */
  if constexpr (std::is_same_v<float, StorageType>) {
    /* For RGBA buffers we put all channels to 0 if either of them is not
     * finite. This way we avoid possible artifacts caused by fully changed
     * hue. */
    if (is_rgba) {
      for (int64_t i = 0; i < num_pixels; i += 4) {
        StorageType *pixel = &pixels[i * 4];
        if (!isfinite(pixel[0]) || !isfinite(pixel[1]) || !isfinite(pixel[2]) ||
            !isfinite(pixel[3]))
        {
          pixel[0] = 0;
          pixel[1] = 0;
          pixel[2] = 0;
          pixel[3] = 0;
        }
      }
    }
    else {
      for (int64_t i = 0; i < num_pixels; ++i) {
        StorageType *pixel = &pixels[i];
        if (!isfinite(pixel[0])) {
          pixel[0] = 0;
        }
      }
    }
  }

  return is_rgba;
}

static bool conform_pixels_to_metadata(const ImageManager::Image *img,
                                       void *pixels,
                                       const int64_t num_pixels)
{
  switch (img->metadata.type) {
    case IMAGE_DATA_TYPE_FLOAT4:
    case IMAGE_DATA_TYPE_FLOAT:
      return conform_pixels_to_metadata_type<float>(img, static_cast<float *>(pixels), num_pixels);
    case IMAGE_DATA_TYPE_BYTE4:
    case IMAGE_DATA_TYPE_BYTE:
      return conform_pixels_to_metadata_type<uchar>(img, static_cast<uchar *>(pixels), num_pixels);
    case IMAGE_DATA_TYPE_HALF4:
    case IMAGE_DATA_TYPE_HALF:
      return conform_pixels_to_metadata_type<half>(img, static_cast<half *>(pixels), num_pixels);
    case IMAGE_DATA_TYPE_USHORT:
    case IMAGE_DATA_TYPE_USHORT4:
      return conform_pixels_to_metadata_type<uint16_t>(
          img, static_cast<uint16_t *>(pixels), num_pixels);
    case IMAGE_DATA_TYPE_NANOVDB_FLOAT:
    case IMAGE_DATA_TYPE_NANOVDB_FLOAT3:
    case IMAGE_DATA_TYPE_NANOVDB_FPN:
    case IMAGE_DATA_TYPE_NANOVDB_FP16:
    case IMAGE_DATA_NUM_TYPES:
      break;
  }

  return false;
}

template<TypeDesc::BASETYPE FileFormat, typename StorageType>
bool ImageManager::file_load_image(Device *device, Image *img, const int texture_limit)
{
  /* Ignore empty images. */
  if (!(img->metadata.channels > 0)) {
    return false;
  }

  /* Get metadata. */
  const int width = img->metadata.width;
  const int height = img->metadata.height;
  const int depth = img->metadata.depth;

  /* Read pixels. */
  vector<StorageType> pixels_storage;
  StorageType *pixels;
  const int64_t max_size = max(max(width, height), depth);
  if (max_size == 0) {
    /* Don't bother with empty images. */
    return false;
  }

  /* Allocate memory as needed, may be smaller to resize down. */
  if (texture_limit > 0 && max_size > texture_limit) {
    pixels_storage.resize(((int64_t)width) * height * depth * 4);
    pixels = &pixels_storage[0];
  }
  else {
    pixels = image_cache
                 .alloc_full(device,
                             img->metadata.type,
                             img->params.interpolation,
                             img->params.extension,
                             width,
                             height,
                             img->texture_slot)
                 .data<StorageType>();
  }

  if (pixels == nullptr) {
    /* Could be that we've run out of memory. */
    return false;
  }

  const int64_t num_pixels = ((int64_t)width) * height * depth;
  if (!img->loader->load_pixels_full(img->metadata, (uint8_t *)pixels)) {
    return false;
  }

  const bool is_rgba = conform_pixels_to_metadata(img, pixels, num_pixels);

  /* Scale image down if needed. */
  if (!pixels_storage.empty()) {
    float scale_factor = 1.0f;
    while (max_size * scale_factor > texture_limit) {
      scale_factor *= 0.5f;
    }
    VLOG_WORK << "Scaling image " << img->loader->name() << " by a factor of " << scale_factor
              << ".";
    vector<StorageType> scaled_pixels;
    int64_t scaled_width;
    int64_t scaled_height;
    int64_t scaled_depth;
    util_image_resize_pixels(pixels_storage,
                             width,
                             height,
                             depth,
                             is_rgba ? 4 : 1,
                             scale_factor,
                             &scaled_pixels,
                             &scaled_width,
                             &scaled_height,
                             &scaled_depth);

    StorageType *texture_pixels;

    texture_pixels = image_cache
                         .alloc_full(device,
                                     img->metadata.type,
                                     img->params.interpolation,
                                     img->params.extension,
                                     width,
                                     height,
                                     img->texture_slot)
                         .data<StorageType>();
    memcpy(texture_pixels, &scaled_pixels[0], scaled_pixels.size() * sizeof(StorageType));
  }

  return true;
}

void ImageManager::device_resize_image_textures(Scene *scene)
{
  const thread_scoped_lock device_lock(device_mutex);
  DeviceScene &dscene = scene->dscene;

  if (dscene.image_textures.size() < images.size()) {
    dscene.image_textures.resize(images.size());
  }
}

void ImageManager::device_copy_image_textures(Scene *scene)
{
  image_cache.copy_to_device_if_modified();

  const thread_scoped_lock device_lock(device_mutex);
  DeviceScene &dscene = scene->dscene;

  dscene.image_textures.copy_to_device_if_modified();
  dscene.image_texture_tile_descriptors.copy_to_device_if_modified();
}

void ImageManager::device_load_image_full(Device *device, Scene *scene, const size_t slot)
{
  Image *img = images[slot].get();

  const ImageDataType type = img->metadata.type;
  const int texture_limit = scene->params.texture_limit;

  /* Create new texture. */
  switch (type) {
    case IMAGE_DATA_TYPE_FLOAT4:
      file_load_image<TypeDesc::FLOAT, float>(device, img, texture_limit);
      break;
    case IMAGE_DATA_TYPE_FLOAT:
      file_load_image<TypeDesc::FLOAT, float>(device, img, texture_limit);
      break;
    case IMAGE_DATA_TYPE_BYTE4:
      file_load_image<TypeDesc::UINT8, uchar>(device, img, texture_limit);
      break;
    case IMAGE_DATA_TYPE_BYTE:
      file_load_image<TypeDesc::UINT8, uchar>(device, img, texture_limit);
      break;
    case IMAGE_DATA_TYPE_HALF4:
      file_load_image<TypeDesc::HALF, half>(device, img, texture_limit);
      break;
    case IMAGE_DATA_TYPE_HALF:
      file_load_image<TypeDesc::HALF, half>(device, img, texture_limit);
      break;
    case IMAGE_DATA_TYPE_USHORT:
      file_load_image<TypeDesc::USHORT, uint16_t>(device, img, texture_limit);
      break;
    case IMAGE_DATA_TYPE_USHORT4:
      file_load_image<TypeDesc::USHORT, uint16_t>(device, img, texture_limit);
      break;
    case IMAGE_DATA_TYPE_NANOVDB_FLOAT:
    case IMAGE_DATA_TYPE_NANOVDB_FLOAT3:
    case IMAGE_DATA_TYPE_NANOVDB_FPN:
    case IMAGE_DATA_TYPE_NANOVDB_FP16: {
#ifdef WITH_NANOVDB
      img->vdb_memory = &image_cache.alloc_full(device,
                                                type,
                                                img->params.interpolation,
                                                img->params.extension,
                                                img->metadata.byte_size,
                                                0,
                                                img->texture_slot);

      uint8_t *pixels = img->vdb_memory->data<uint8_t>();
      if (pixels) {
        img->loader->load_pixels_full(img->metadata, pixels);
      }
#endif
      break;
    }
    case IMAGE_DATA_NUM_TYPES:
      break;
  }
}

void ImageManager::device_load_image_tiled(Scene *scene, const size_t slot)
{
  Image *img = images[slot].get();

  vector<KernelTileDescriptor> levels;
  const int max_miplevels = img->params.interpolation != INTERPOLATION_CLOSEST ? 1 : INT_MAX;
  const int tile_size = img->metadata.tile_size;

  int num_tiles = 0;

  for (int miplevel = 0; max_miplevels; miplevel++) {
    const int width = divide_up(img->metadata.width, 1 << miplevel);
    const int height = divide_up(img->metadata.height, 1 << miplevel);

    levels.push_back(num_tiles);

    num_tiles += divide_up(width, tile_size) * divide_up(height, tile_size);

    if (width <= tile_size && height <= tile_size) {
      break;
    }
  }

  {
    // TODO: make this more efficient
    const thread_scoped_lock device_lock(device_mutex);
    const int tile_descriptor_offset = scene->dscene.image_texture_tile_descriptors.size();
    scene->dscene.image_texture_tile_descriptors.resize(tile_descriptor_offset + levels.size() +
                                                        num_tiles);

    KernelTileDescriptor *descr_data = scene->dscene.image_texture_tile_descriptors.data() +
                                       tile_descriptor_offset;

    for (int i = 0; i < levels.size(); i++) {
      descr_data[i] = levels.size() + levels[i];
    }
    std::fill_n(descr_data + levels.size(), num_tiles, KERNEL_TILE_LOAD_NONE);

    img->tile_descriptor_offset = tile_descriptor_offset;
    img->tile_descriptor_levels = levels.size();
    img->tile_descriptor_num = num_tiles;
  }
}

void ImageManager::device_update_image_requested(Device *device, Scene *scene, Image *img)
{
  const size_t tile_size = img->metadata.tile_size;

  KernelTileDescriptor *tile_descriptors = scene->dscene.image_texture_tile_descriptors.data() +
                                           img->tile_descriptor_offset +
                                           img->tile_descriptor_levels;

  size_t i = 0;
  for (int miplevel = 0; miplevel < img->tile_descriptor_levels; miplevel++) {
    const int width = divide_up(img->metadata.width, 1 << miplevel);
    const int height = divide_up(img->metadata.height, 1 << miplevel);

    for (size_t y = 0; y < height; y += tile_size) {
      for (size_t x = 0; x < width; x += tile_size, i++) {
        assert(i < img->tile_descriptor_num);

        if (tile_descriptors[i] != KERNEL_TILE_LOAD_REQUEST) {
          continue;
        }

        const size_t w = min(width - x, tile_size);
        const size_t h = min(height - y, tile_size);
        const size_t tile_size_padded = tile_size + KERNEL_IMAGE_TEX_PADDING * 2;

        KernelTileDescriptor tile_descriptor;

        device_image &mem = image_cache.alloc_tile(device,
                                                   img->metadata.type,
                                                   img->params.interpolation,
                                                   img->params.extension,
                                                   tile_size_padded,
                                                   tile_descriptor);

        const size_t pixel_bytes = mem.data_elements * datatype_size(mem.data_type);
        const size_t x_stride = pixel_bytes;
        const size_t y_stride = mem.data_width * pixel_bytes;
        const size_t x_offset = kernel_tile_descriptor_offset(tile_descriptor) * tile_size_padded *
                                pixel_bytes;

        uint8_t *pixels = mem.data<uint8_t>() + x_offset;

        const bool ok = img->loader->load_pixels_tile(img->metadata,
                                                      miplevel,
                                                      x,
                                                      y,
                                                      w,
                                                      h,
                                                      x_stride,
                                                      y_stride,
                                                      KERNEL_IMAGE_TEX_PADDING,
                                                      img->params.extension,
                                                      pixels);

        conform_pixels_to_metadata(img, pixels, w * h);

        tile_descriptors[i] = (ok) ? tile_descriptor : KERNEL_TILE_LOAD_FAILED;
        scene->dscene.image_texture_tile_descriptors.tag_modified();

        if (ok) {
          VLOG_DEBUG << "Load image tile: " << img->loader->name() << ", mip level " << miplevel
                     << " (" << x << " " << y << ")";
        }
        else {
          VLOG_WARNING << "Failed to load image tile: " << img->loader->name() << ", mip level "
                       << miplevel << " (" << x << " " << y << ")";
        }
      }
    }
  }

  img->loader->drop_file_handle();
}

void ImageManager::device_load_image(Device *device,
                                     Scene *scene,
                                     const size_t slot,
                                     Progress &progress)
{
  if (progress.get_cancel()) {
    return;
  }

  Image *img = images[slot].get();

  progress.set_status("Updating Images", "Loading " + img->loader->name());

  load_image_metadata(img);

  KernelImageTexture tex;
  tex.width = img->metadata.width;
  tex.height = img->metadata.height;
  tex.interpolation = img->params.interpolation;
  tex.extension = img->params.extension;
  tex.use_transform_3d = img->metadata.use_transform_3d;
  tex.transform_3d = img->metadata.transform_3d;
  tex.average_color = img->metadata.average_color;

  if (img->metadata.tile_size) {
    assert(is_power_of_two(img->metadata.tile_size));

    device_load_image_tiled(scene, slot);

    tex.tile_descriptor_offset = img->tile_descriptor_offset;
    tex.tile_size_shift = __bsr(img->metadata.tile_size);
    tex.tile_levels = img->tile_descriptor_levels;
  }
  else {
    device_load_image_full(device, scene, slot);
    tex.slot = img->texture_slot;
  }

  /* Update image texture device data. */
  scene->dscene.image_textures[slot] = tex;
  scene->dscene.image_textures.tag_modified();

  /* Cleanup memory in image loader. */
  img->loader->cleanup();
  img->need_load = false;
}

void ImageManager::device_free_image(Scene *scene, size_t slot)
{
  Image *img = images[slot].get();
  if (img == nullptr) {
    return;
  }

  if (osl_texture_system) {
#ifdef WITH_OSL
    const ustring filepath = img->loader->osl_filepath();
    if (!filepath.empty()) {
      ((OSL::TextureSystem *)osl_texture_system)->invalidate(filepath);
    }
#endif
  }

  if (img->texture_slot != KERNEL_IMAGE_TEX_NONE) {
    image_cache.free_full(img->texture_slot);
  }
  if (img->tile_descriptor_offset != KERNEL_IMAGE_TEX_NONE) {
    // TODO: shrink image_texture_tile_descriptors
    KernelTileDescriptor *tile_descriptors = scene->dscene.image_texture_tile_descriptors.data() +
                                             img->tile_descriptor_offset +
                                             img->tile_descriptor_levels;

    for (int i = 0; i < img->tile_descriptor_num; i++) {
      if (kernel_tile_descriptor_loaded(tile_descriptors[i])) {
        image_cache.free_tile(tile_descriptors[i]);
      }
    }
  }

  images[slot].reset();
}

void ImageManager::device_update_requested(Device *device, Scene *scene)
{
  // TODO: not supported for MEM_GLOBAL
  // TODO: only do if modified
  // scene->dscene.image_texture_tile_descriptors.copy_from_device();

  parallel_for(blocked_range<size_t>(0, images.size(), 1), [&](const blocked_range<size_t> &r) {
    for (size_t i = r.begin(); i != r.end(); i++) {
      unique_ptr<Image> &img = images[i];
      if (img && img->tile_descriptor_offset != KERNEL_IMAGE_TEX_NONE) {
        device_update_image_requested(device, scene, img.get());
      }
    }
  });

  device_copy_image_textures(scene);
}

void ImageManager::device_update(Device *device, Scene *scene, Progress &progress)
{
  if (!need_update()) {
    return;
  }

  const scoped_callback_timer timer([scene](double time) {
    if (scene->update_stats) {
      scene->update_stats->image.times.add_entry({"device_update", time});
    }
  });

  device_resize_image_textures(scene);

  TaskPool pool;
  for (size_t slot = 0; slot < images.size(); slot++) {
    Image *img = images[slot].get();
    if (img && img->users == 0) {
      device_free_image(scene, slot);
    }
    else if (img && img->need_load) {
      pool.push([this, device, scene, slot, &progress] {
        device_load_image(device, scene, slot, progress);
      });
    }
  }

  pool.wait_work();

  device_copy_image_textures(scene);

  need_update_ = false;
}

void ImageManager::device_load_slots(Device *device,
                                     Scene *scene,
                                     Progress &progress,
                                     const set<int> &slots)
{
  device_resize_image_textures(scene);

  TaskPool pool;
  for (const int slot : slots) {
    pool.push([this, device, scene, slot, &progress] {
      Image *img = images[slot].get();
      assert(img != nullptr);

      if (img->users == 0) {
        device_free_image(scene, slot);
      }
      else if (img->need_load) {
        device_load_image(device, scene, slot, progress);
      }
    });
  }
  pool.wait_work();

  device_copy_image_textures(scene);
}

void ImageManager::device_load_builtin(Device *device, Scene *scene, Progress &progress)
{
  /* Load only builtin images, Blender needs this to load evaluated
   * scene data from depsgraph before it is freed. */
  if (!need_update()) {
    return;
  }

  device_resize_image_textures(scene);

  TaskPool pool;
  for (size_t slot = 0; slot < images.size(); slot++) {
    Image *img = images[slot].get();
    if (img && img->need_load && img->builtin) {
      pool.push([this, device, scene, slot, &progress] {
        device_load_image(device, scene, slot, progress);
      });
    }
  }

  pool.wait_work();

  device_copy_image_textures(scene);
}

void ImageManager::device_free_builtin(Scene *scene)
{
  for (size_t slot = 0; slot < images.size(); slot++) {
    Image *img = images[slot].get();
    if (img && img->builtin) {
      device_free_image(scene, slot);
    }
  }
}

void ImageManager::device_free(Scene *scene)
{
  for (size_t slot = 0; slot < images.size(); slot++) {
    device_free_image(scene, slot);
  }
  images.clear();
  image_cache.device_free();
  scene->dscene.image_textures.free();
  scene->dscene.image_texture_tile_descriptors.free();
}

void ImageManager::collect_statistics(RenderStats *stats)
{
  for (const unique_ptr<Image> &image : images) {
    if (!image) {
      /* Image may have been freed due to lack of users. */
      continue;
    }

    // TODO: collection from image cache
    stats->image.textures.add_entry(NamedSizeEntry(image->loader->name(), 0));
  }
}

void ImageManager::tag_update()
{
  need_update_ = true;
}

bool ImageManager::need_update() const
{
  return need_update_;
}

CCL_NAMESPACE_END

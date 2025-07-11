/* SPDX-FileCopyrightText: 2006 Blender Authors
 *
 * SPDX-License-Identifier: GPL-2.0-or-later */
/** \file
 * \ingroup render
 *
 * This include is for non-render pipeline exports (still old cruft here).
 */

#pragma once

#include "BLI_compiler_attrs.h"

/* called by meshtools */
struct Depsgraph;
struct ImagePool;
struct MTex;
struct Tex;

/* `texture_procedural.cc` */

/**
 * \param pool: Thread pool, may be NULL.
 *
 * \return True if the texture has color, otherwise false.
 */
bool RE_texture_evaluate(const struct MTex *mtex,
                         const float vec[3],
                         int thread,
                         struct ImagePool *pool,
                         bool skip_load_image,
                         bool texnode_preview,
                         /* Return arguments. */
                         float *r_intensity,
                         float r_rgba[4]) ATTR_NONNULL(1, 2, 7, 8);

/**
 * \param in: Destination
 * \param tex: Texture.
 * \param out: Previous color.
 * \param fact: Texture strength.
 * \param facg: Button strength value.
 */
float texture_value_blend(float tex, float out, float fact, float facg, int blendtype);

void RE_texture_rng_init(void);
void RE_texture_rng_exit(void);

/* `texture_image.cc` */

void ibuf_sample(struct ImBuf *ibuf, float fx, float fy, float dx, float dy, float result[4]);

/* `texture_procedural.cc` */

/**
 * Texture evaluation result.
 */
struct TexResult {
  float tin;
  float trgba[4];
  /* Is actually a boolean: When true -> use alpha, false -> set alpha to 1.0. */
  int talpha;
};

/* This one uses nodes. */

/**
 * WARNING(@ideasman42): if the texres's values are not declared zero,
 * check the return value to be sure the color values are set before using the r/g/b values,
 * otherwise you may use uninitialized values.
 *
 * Use it for stuff which is out of render pipeline.
 */
int multitex_ext(struct Tex *tex,
                 const float texvec[3],
                 struct TexResult *texres,
                 short thread,
                 struct ImagePool *pool,
                 bool scene_color_manage,
                 bool skip_load_image);

/**
 * Nodes disabled.
 * extern-tex doesn't support nodes (#ntreeBeginExec() can't be called when rendering is going on).
 *
 * Use it for stuff which is out of render pipeline.
 */
int multitex_ext_safe(struct Tex *tex,
                      const float texvec[3],
                      struct TexResult *texres,
                      struct ImagePool *pool,
                      bool scene_color_manage,
                      bool skip_load_image);

/**
 * Only for internal node usage.
 *
 * this is called from the shader and texture nodes
 * Use it from render pipeline only!
 */
int multitex_nodes(struct Tex *tex,
                   const float texvec[3],
                   struct TexResult *texres,
                   short thread,
                   short which_output,
                   const struct MTex *mtex,
                   struct ImagePool *pool);

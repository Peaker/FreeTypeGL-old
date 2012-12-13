/* ============================================================================
 * Freetype GL - A C OpenGL Freetype engine
 * Platform:    Any
 * WWW:         http://code.google.com/p/freetype-gl/
 * ----------------------------------------------------------------------------
 * Copyright 2011,2012 Nicolas P. Rougier. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY NICOLAS P. ROUGIER ''AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
 * EVENT SHALL NICOLAS P. ROUGIER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * The views and conclusions contained in the software and documentation are
 * those of the authors and should not be interpreted as representing official
 * policies, either expressed or implied, of Nicolas P. Rougier.
 * ============================================================================
 *
 * This source is based on the article by Jukka Jylänki :
 * "A Thousand Ways to Pack the Bin - A Practical Approach to
 * Two-Dimensional Rectangle Bin Packing", February 27, 2010.
 *
 * More precisely, this is an implementation of the Skyline Bottom-Left
 * algorithm based on C++ sources provided by Jukka Jylänki at:
 * http://clb.demon.fi/files/RectangleBinPack/
 *
 *  ============================================================================
 */
#ifndef __TEXTURE_ATLAS_H__
#define __TEXTURE_ATLAS_H__

#ifdef __cplusplus
extern "C" {
#endif

#include "vector.h"
#include "vec234.h"
#include "opengl.h"
#include <stdbool.h>

/**
 * @file   texture-atlas.h
 * @author Nicolas Rougier (Nicolas.Rougier@inria.fr)
 *
 * @defgroup texture-atlas Texture atlas
 *
 * A texture atlas is used to pack several small regions into a single texture.
 *
 * The actual implementation is based on the article by Jukka Jylänki : "A
 * Thousand Ways to Pack the Bin - A Practical Approach to Two-Dimensional
 * Rectangle Bin Packing", February 27, 2010.
 * More precisely, this is an implementation of the Skyline Bottom-Left
 * algorithm based on C++ sources provided by Jukka Jylänki at:
 * http://clb.demon.fi/files/RectangleBinPack/
 *
 *
 * Example Usage:
 * @code
 * #include "texture-atlas.h"
 *
 * ...
 *
 * / Creates a new atlas of 512x512 with a depth of 1
 * texture_atlas_t * atlas = texture_atlas_new( 512, 512, 1 );
 *
 * // Allocates a region of 20x20
 * ivec4 region = texture_atlas_make_region( atlas, 20, 20, data, stride );
 *
 * ...
 *
 * @endcode
 *
 * @{
 */


/**
 * A texture atlas is used to pack several small regions into a single texture.
 */
typedef struct
{
    /**
     * Allocated nodes
     */
    vector_t * nodes;

    ivec2 size;

    /**
     * Depth (in bytes) of the underlying texture
     */
    size_t depth;

    /**
     * Allocated surface size
     */
    size_t used;

    /**
     * Texture identity (OpenGL)
     */
    unsigned int id;

    unsigned char * data;

    bool uploaded;

} texture_atlas_t;



/**
 * Creates a new empty texture atlas.
 *
 * @param   width   width of the atlas
 * @param   height  height of the atlas
 * @param   depth   bit depth of the atlas
 * @return          a new empty texture atlas.
 *
 */
  texture_atlas_t *
  texture_atlas_new( const ivec2 *size, const size_t depth );


/**
 *  Deletes a texture atlas.
 *
 *  @param self a texture atlas structure
 *
 */
  void
  texture_atlas_delete( texture_atlas_t * self );


/**
 *  Upload atlas to video memory.
 *
 *  @param self a texture atlas structure
 *
 */
  void
  texture_atlas_upload( texture_atlas_t * self );


/**
 *  Allocate a new region in the atlas.
 *
 *  @param self   a texture atlas structure
 *  @param width  width of the region to allocate
 *  @param height height of the region to allocate
 *  @param data   data to be uploaded into the specified region
 *  @param stride stride of the data
 *  @return       Coordinates of the allocated region
 *
 */
  ivec4
  texture_atlas_make_region(
      texture_atlas_t *, size_t width, size_t height,
      const unsigned char *data,
      size_t stride);

/**
 *  Remove all allocated regions from the atlas.
 *
 *  @param self   a texture atlas structure
 */
  void
  texture_atlas_clear( texture_atlas_t * self );


  void
  texture_atlas_render( texture_atlas_t * self,
                        float x, float y,
                        float width, float height );

/** @} */

#ifdef __cplusplus
}
#endif

#endif /* __TEXTURE_ATLAS_H__ */

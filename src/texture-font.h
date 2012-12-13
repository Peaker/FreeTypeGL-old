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
 */
#ifndef __TEXTURE_FONT_H__
#define __TEXTURE_FONT_H__

#include <ft2build.h>
#include FT_FREETYPE_H

#ifdef __cplusplus
extern "C" {
#endif

#include "vec234.h"
#include "vector.h"
#include "texture-atlas.h"

/**
 * @file   texture-font.h
 * @author Nicolas Rougier (Nicolas.Rougier@inria.fr)
 *
 * @defgroup texture-font Texture font
 *
 * Texture font.
 *
 * Example Usage:
 * @code
 * #include "texture-font.h"
 *
 * int main( int arrgc, char *argv[] )
 * {
 *   return 0;
 * }
 * @endcode
 *
 * @{
 */


/*
 * Glyph metrics:
 * --------------
 *
 *                       xmin                     xmax
 *                        |                         |
 *                        |<-------- width -------->|
 *                        |                         |
 *              |         +-------------------------+----------------- ymax
 *              |         |    ggggggggg   ggggg    |     ^        ^
 *              |         |   g:::::::::ggg::::g    |     |        |
 *              |         |  g:::::::::::::::::g    |     |        |
 *              |         | g::::::ggggg::::::gg    |     |        |
 *              |         | g:::::g     g:::::g     |     |        |
 *    offset_x -|-------->| g:::::g     g:::::g     |  offset_y    |
 *              |         | g:::::g     g:::::g     |     |        |
 *              |         | g::::::g    g:::::g     |     |        |
 *              |         | g:::::::ggggg:::::g     |     |        |
 *              |         |  g::::::::::::::::g     |     |      height
 *              |         |   gg::::::::::::::g     |     |        |
 *  baseline ---*---------|---- gggggggg::::::g-----*--------      |
 *            / |         |             g:::::g     |              |
 *     origin   |         | gggggg      g:::::g     |              |
 *              |         | g:::::gg   gg:::::g     |              |
 *              |         |  g::::::ggg:::::::g     |              |
 *              |         |   gg:::::::::::::g      |              |
 *              |         |     ggg::::::ggg        |              |
 *              |         |         gggggg          |              v
 *              |         +-------------------------+----------------- ymin
 *              |                                   |
 *              |------------- advance_x ---------->|
 */

enum texture_outline_type {
    TEXTURE_OUTLINE_NONE = 0,
    TEXTURE_OUTLINE_LINE = 1,
    TEXTURE_OUTLINE_INNER = 2,
    TEXTURE_OUTLINE_OUTER = 3,
};

typedef struct {
    FT_UInt glyph_index;
    wchar_t charcode;
    ivec2 size;
    /**
     * Remember that the vertical bearing is the distance from the
     * baseline to the top-most glyph scanline, upwards y coordinates
     * being positive:
     */
    ivec2 bearing;

    vec2 advance;

    vec2 texture_pos0;
    vec2 texture_pos1;

    enum texture_outline_type outline_type;
    float outline_thickness;

} texture_glyph_t;



/**
 *  Texture font structure.
 */
typedef struct
{
    /**
     * Vector of glyphs contained in this font.
     */
    vector_t * glyphs;

    /**
     * Atlas structure to store glyphs data.
     */
    texture_atlas_t * atlas;

    /* TODO: Share a copy of the library between different fonts? */
    FT_Library library;
    FT_Face face;

    /**
     * Whether to use autohint when rendering font
     */
    int hinting;

    enum texture_outline_type outline_type;

    /**
     * Outline thickness
     */
    float outline_thickness;

    /**
     * Whether to use our own lcd filter.
     */
    int filtering;

    /**
     * LCD filter weights
     */
    unsigned char lcd_weights[5];

    /**
     * This field is simply used to compute a default line spacing (i.e., the
     * baseline-to-baseline distance) when writing text with this font. Note
     * that it usually is larger than the sum of the ascender and descender
     * taken as absolute values. There is also no guarantee that no glyphs
     * extend above or below subsequent baselines when using this distance.
     */
    float height;

    /**
     * This field is the distance that must be placed between two lines of
     * text. The baseline-to-baseline distance should be computed as:
     * ascender - descender + linegap
     */
    float linegap;

    /**
     * The ascender is the vertical distance from the horizontal baseline to
     * the highest 'character' coordinate in a font face. Unfortunately, font
     * formats define the ascender differently. For some, it represents the
     * ascent of all capital latin characters (without accents), for others it
     * is the ascent of the highest accented character, and finally, other
     * formats define it as being equal to bbox.yMax.
     */
    float ascender;

    /**
     * The descender is the vertical distance from the horizontal baseline to
     * the lowest 'character' coordinate in a font face. Unfortunately, font
     * formats define the descender differently. For some, it represents the
     * descent of all capital latin characters (without accents), for others it
     * is the ascent of the lowest accented character, and finally, other
     * formats define it as being equal to bbox.yMin. This field is negative
     * for values below the baseline.
     */
    float descender;

    /**
     * The position of the underline line for this face. It is the center of
     * the underlining stem. Only relevant for scalable formats.
     */
    float underline_position;

    /**
     * The thickness of the underline for this face. Only relevant for scalable
     * formats.
     */
    float underline_thickness;

} texture_font_t;



/**
 * This function creates a new texture font from given filename and size.  The
 * texture atlas is used to store glyph on demand. Note the depth of the atlas
 * will determine if the font is rendered as alpha channel only (depth = 1) or
 * RGB (depth = 3) that correspond to subpixel rendering (if available on your
 * freetype implementation).
 *
 * @param atlas     A texture atlas
 * @param filename  A font filename
 * @param size      Size of font to be created (in points)
 *
 * @return A new empty font (no glyph inside yet)
 *
 */
  texture_font_t *
  texture_font_new( texture_atlas_t * atlas,
                    const char * filename,
                    const float size );


/**
 * Delete a texture font. Note that this does not delete the glyph from the
 * texture atlas.
 *
 * @param self a valid texture font
 */
  void
  texture_font_delete( texture_font_t * self );


/**
 * Request a new glyph from the font. If it has not been created yet, it will
 * be.
 *
 * @param self     A valid texture font
 * @param charcode Character codepoint to be loaded.
 *
 * @return A pointer on the new glyph or 0 if the texture atlas is not big
 *         enough
 *
 */
  texture_glyph_t *
  texture_font_get_glyph( texture_font_t * self,
                          wchar_t charcode );


/**
 * Request the loading of several glyphs at once.
 *
 * @param self      a valid texture font
 * @param charcodes character codepoints to be loaded.
 *
 * @return Number of missed glyph if the texture is not big enough to hold
 *         every glyphs.
 */
  texture_glyph_t **
  texture_font_load_glyphs( texture_font_t * self,
                            const wchar_t * charcodes );

/**
 * Get the size of a piece of text with this font
 *
 * @param self   a valid texture font
 * @param text   Text to get the size of
 * @param length Length of text to be sized (or 0 for null termination)
 * @param out_size Output parameter that will contain the size
 */
  void
  texture_font_get_text_size(
      texture_font_t *self, wchar_t *text, size_t length, vec2 *out_size );

/**
 * Get the kerning between two horizontal glyphs.
 *
 * @param self      a valid texture glyph
 * @param charcode  codepoint of the peceding glyph
 *
 * @return x kerning value
 */
float texture_font_get_kerning(
    texture_font_t *,
    FT_UInt glyph_index,
    FT_UInt prev_glyph_index);

float texture_font_glyph_get_kerning(
    texture_font_t *, texture_glyph_t *, wchar_t prev_char);

/** @} */

#ifdef __cplusplus
}
#endif

#endif /* __TEXTURE_FONT_H__ */

#ifndef __GLYPH_CACHE_H__
#define __GLYPH_CACHE_H__

#include "texture-atlas.h"
#include "texture-font.h"

typedef struct {
    unsigned glyph_index;
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

    texture_font_t *font;
} texture_glyph_t;

typedef struct {
    texture_atlas_t atlas;
    vector_t *glyphs;           /* of texture_glyph_t */
} glyph_cache_t;

void glyph_cache_init(glyph_cache_t *, const ivec2 *size, int depth);
void glyph_cache_fini(glyph_cache_t *);

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
texture_glyph_t *glyph_cache_get_glyph( glyph_cache_t *, texture_font_t *, wchar_t charcode );

float glyph_cache_get_kerning(
    glyph_cache_t *, texture_glyph_t *, wchar_t prev_char);

#endif

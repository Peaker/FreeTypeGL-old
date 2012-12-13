#include "glyph-cache.h"
#include <stdlib.h>
#include <assert.h>
#include <wchar.h>

static texture_glyph_t *texture_glyph_new(
    unsigned glyph_index,
    wchar_t charcode,
    ivec2 size,
    ivec2 bearing,
    vec2 advance,

    vec2 texture_pos0,
    vec2 texture_pos1,

    texture_font_t *font)
{
    texture_glyph_t *self = malloc( sizeof *self );
    *self = (texture_glyph_t){
        .glyph_index = glyph_index,
        .charcode = charcode,
        .size = size,
        .bearing = bearing,
        .advance = advance,
        .texture_pos0 = texture_pos0,
        .texture_pos1 = texture_pos1,
        .font = font,
    };
    return self;
}

static void texture_glyph_delete( texture_glyph_t *self )
{
    assert( self );
    free( self );
}

void glyph_cache_init(
    glyph_cache_t *self, const ivec2 *size, int depth)
{
    texture_atlas_init(&self->atlas, size, depth);
    self->glyphs = vector_new( sizeof(texture_glyph_t *) );
    /* -1 is a special glyph */
    glyph_cache_get_glyph(self, NULL, -1);
}

void glyph_cache_fini(glyph_cache_t *self)
{
    size_t i;
    texture_glyph_t *glyph;
    for(i=0; i<vector_size(self->glyphs); ++i) {
        glyph = *(texture_glyph_t **) vector_get( self->glyphs, i );
        texture_glyph_delete(glyph);
    }
    vector_delete(self->glyphs);
}

static texture_glyph_t *glyph_cache_load_glyph(
    glyph_cache_t *self, texture_font_t *font, const wchar_t charcode)
{
    size_t depth = self->atlas.depth;
    texture_font_loaded_glyph_t loaded;
    int rc = texture_font_load_glyph(font, charcode, depth == 3, &loaded);
    if(0 != rc) goto Error;

    size_t w = loaded.bitmap->width/depth;
    size_t h = loaded.bitmap->rows;

    ivec4 region = texture_atlas_make_region(
        &self->atlas, w, h, loaded.bitmap->buffer, loaded.bitmap->pitch);
    if(region.x < 0) goto Unload_Error;

    ivec2 size = self->atlas.size;
    texture_glyph_t *glyph =
        texture_glyph_new(
            loaded.glyph_index, charcode,
            (ivec2){{ region.width, region.height }},
            loaded.bearing,
            loaded.advance,
            (vec2){{ region.x/(float)size.x, region.y/(float)size.y }},
            (vec2){{ (region.x + region.width)/(float)size.x,
                     (region.y + region.height)/(float)size.y }},
            font);

    vector_push_back( self->glyphs, &glyph );
    texture_font_done_glyph(font, &loaded);

    return *(texture_glyph_t **)vector_back( self->glyphs );
Unload_Error:
    texture_font_done_glyph(font, &loaded);
Error:
    return NULL;
}

texture_glyph_t *glyph_cache_get_glyph(glyph_cache_t * self, texture_font_t *font, wchar_t charcode)
{
    size_t i;
    /* Check if charcode has been already loaded */
    for(i=0; i<self->glyphs->size; ++i) {
        texture_glyph_t *glyph =
            *(texture_glyph_t **) vector_get( self->glyphs, i );
        // If charcode is -1, we don't care about outline type or thickness
        if( (glyph->charcode == charcode) &&
            ((charcode == (wchar_t)(-1) ) || (glyph->font == font) ))
        {
            return glyph;
        }
    }

    /* charcode -1 is special : it is used for line drawing (overline,
     * underline, strikethrough) and background.
     */
    if( charcode == (wchar_t)(-1) )
    {
        static unsigned char data[4*4*3] = {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                                            -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                                            -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                                            -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};
        ivec4 region = texture_atlas_make_region(&self->atlas, 4, 4, data, 0);
        if (region.x < 0) return NULL;

        texture_glyph_t *glyph = malloc( sizeof *glyph );
        memset(glyph, 0, sizeof *glyph);
        glyph->glyph_index = -1;
        glyph->charcode = -1;
        ivec2 size  = self->atlas.size;
        glyph->texture_pos0 = (vec2){{ (region.x+2)/(float)size.x, (region.y+2)/(float)size.y }};
        glyph->texture_pos1 = (vec2){{ (region.x+3)/(float)size.x, (region.y+3)/(float)size.y }};
        vector_push_back( self->glyphs, &glyph );
        return glyph; //*(texture_glyph_t **) vector_back( self->glyphs );
    }

    /* Glyph has not been already loaded */
    texture_glyph_t *glyph = glyph_cache_load_glyph( self, font, charcode );
    if(!glyph) return NULL;
    return glyph;
}

void glyph_cache_get_text_size(
    glyph_cache_t *self, texture_font_t *font,
    wchar_t *text, size_t length,
    vec2 *out_size )
{
    if( 0 == length ) length = wcslen(text);

    float maxwidth = 0;
    float width = 0;
    unsigned lines = 1;
    size_t i;
    for( i=0; i<length; ++i ) {
        if (text[i] == L'\n') {
            if(width > maxwidth) maxwidth = width;
            width = 0;
            lines++;
            continue;
        }
        texture_glyph_t *glyph =
            glyph_cache_get_glyph( self, font, text[i] );
        width += glyph->advance.x;
    }
    if(width > maxwidth) maxwidth = width;

    *out_size = (vec2){{ maxwidth, lines * font->height }};
}

float glyph_cache_get_kerning(
    glyph_cache_t *self, texture_glyph_t *glyph, wchar_t prev_char)
{
    return texture_font_get_kerning(glyph->font, glyph->glyph_index, prev_char);
}

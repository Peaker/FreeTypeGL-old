/* ===========================================================================
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
#include "texture-font.h"
#include FT_STROKER_H
// #include FT_ADVANCES_H
#include FT_LCD_FILTER_H

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <math.h>
#include <wchar.h>

#undef __FTERRORS_H__
#define FT_ERRORDEF( e, v, s )  { e, s },
#define FT_ERROR_START_LIST     {
#define FT_ERROR_END_LIST       { 0, 0 } };
const struct {
    int          code;
    const char*  message;
} FT_Errors[] =
#include FT_ERRORS_H
#undef FT_ERROR_START_LIST
#undef FT_ERROR_END_LIST
#undef FT_ERRORDEF

#define FT_CHECK_CALL(name, params, err_label)                          \
    do {                                                                \
        FT_Error ft_err = name params;                                  \
        if(0 != ft_err) {                                               \
            fprintf(stderr, "FT_ERROR (at %s:%d: " #name "): %d (%s)\n", \
                    __FILE__, __LINE__,                                 \
                    FT_Errors[ft_err].code,                             \
                    FT_Errors[ft_err].message);                         \
            goto err_label;                                             \
        }                                                               \
    } while(0)


// ------------------------------------------------------ texture_glyph_new ---
texture_glyph_t *texture_glyph_new( void )
{
    texture_glyph_t *self = (texture_glyph_t *) malloc( sizeof(texture_glyph_t) );
    assert (self);
    self->size = (ivec2){{ 0, 0 }};
    self->outline_type = 0;
    self->outline_thickness = 0.0;
    self->bearing = (ivec2){{ 0, 0 }};
    self->advance = (vec2){{ 0, 0 }};
    self->texture_pos0 = (vec2){{ 0, 0 }};
    self->texture_pos1 = (vec2){{ 0, 0 }};
    self->kerning = vector_new( sizeof(kerning_t) );
    return self;
}


// --------------------------------------------------- texture_glyph_delete ---
void
texture_glyph_delete( texture_glyph_t *self )
{
    assert( self );
    vector_delete( self->kerning );
    free( self );
}

// ---------------------------------------------- texture_glyph_get_kerning ---
float
texture_glyph_get_kerning( const texture_glyph_t * self,
                           const wchar_t charcode )
{
    size_t i;

    assert( self );
    for( i=0; i<vector_size(self->kerning); ++i )
    {
        kerning_t * kerning = (kerning_t *) vector_get( self->kerning, i );
        if( kerning->charcode == charcode )
        {
            return kerning->kerning;
        }
    }
    return 0;
}


static void
generate_kerning( texture_font_t *self )
{
    size_t i;
    /* For each glyph couple combination, check if kerning is necessary */
    /* Starts at index 1 since 0 is for the special backgroudn glyph */
    for(i=1; i<self->glyphs->size; ++i) {
        texture_glyph_t *glyph = *(texture_glyph_t **) vector_get( self->glyphs, i );
        FT_UInt glyph_index = FT_Get_Char_Index( self->face, glyph->charcode );
        vector_clear( glyph->kerning );

        size_t j;
        for( j=1; j<self->glyphs->size; ++j ) {
            texture_glyph_t *prev_glyph =
                *(texture_glyph_t **) vector_get( self->glyphs, j );
            FT_UInt prev_index = FT_Get_Char_Index( self->face, prev_glyph->charcode );
            FT_Vector kerning;
            FT_CHECK_CALL(FT_Get_Kerning, ( self->face, prev_index, glyph_index, FT_KERNING_UNFITTED, &kerning ), Error);
            // printf("%c(%d)-%c(%d): %ld\n",
            //       prev_glyph->charcode, prev_glyph->charcode,
            //       glyph_index, glyph_index, kerning.x);
            if( kerning.x )
            {
                // 64 * 64 because of 26.6 encoding AND the transform matrix used
                // in new_face (hres = 64)
                kerning_t k = {prev_glyph->charcode, kerning.x / (float)(64.0f*64.0f)};
                vector_push_back( glyph->kerning, &k );
            }
        }
    }
    return;
Error:
    abort();
}


static float hres = 64.0f;

static int new_face( FT_Library library,
                      const char *filename,
                      FT_Face *face )
{
    FT_Matrix matrix = { (int)((1.0/hres) * 0x10000L),
                         (int)((0.0)      * 0x10000L),
                         (int)((0.0)      * 0x10000L),
                         (int)((1.0)      * 0x10000L) };

    /* Load face */
    FT_CHECK_CALL(FT_New_Face, ( library, filename, 0, face ), Error);

    /* Select charmap */
    FT_CHECK_CALL(FT_Select_Charmap, ( *face, FT_ENCODING_UNICODE ), DoneFace_Error);

    /* Set transform matrix */
    FT_Set_Transform( *face, &matrix, NULL );

    return 0;

DoneFace_Error:
    FT_Done_Face (*face);
Error:
    return -1;
}

// ------------------------------------------------------- texture_font_new ---
texture_font_t *texture_font_new(
    texture_atlas_t * atlas,
    const char * filename,
    const float size)
{
    assert(size > 0);

    texture_font_t *self = (texture_font_t *) malloc( sizeof(texture_font_t) );
    assert(self);

    /* Initialize library */
    FT_CHECK_CALL(FT_Init_FreeType, (&self->library), Free_Error);

#ifdef LCD_FEATURES_ENABLED
    if(atlas->depth == 3) {
        FT_CHECK_CALL(
            FT_Library_SetLcdFilter, ( self->library, FT_LCD_FILTER_LIGHT ), DoneLibrary_Error);
        if(self->filtering) {
            FT_CHECK_CALL(
                FT_Library_SetLcdFilterWeights, ( self->library, self->lcd_weights ),
                DoneLibrary_Error);
        }
    }
#endif

    self->glyphs = vector_new( sizeof(texture_glyph_t *) );
    self->atlas = atlas;
    self->height = 0;
    self->ascender = 0;
    self->descender = 0;
    self->outline_type = 0;
    self->outline_thickness = 0.0;
    self->hinting = 1;
    self->filtering = 1;
    // FT_LCD_FILTER_LIGHT   is (0x00, 0x55, 0x56, 0x55, 0x00)
    // FT_LCD_FILTER_DEFAULT is (0x10, 0x40, 0x70, 0x40, 0x10)
    self->lcd_weights[0] = 0x10;
    self->lcd_weights[1] = 0x40;
    self->lcd_weights[2] = 0x70;
    self->lcd_weights[3] = 0x40;
    self->lcd_weights[4] = 0x10;

    int res = new_face(self->library, filename, &self->face);
    if(0 != res) goto DoneLibrary_Error;

    /* Get font metrics at high resolution: */
    FT_CHECK_CALL(FT_Set_Char_Size, (self->face, (int)(size*64 * 100), 0, 72*hres, 72),
                  DoneFace_Error);

    // 64 * 64 because of 26.6 encoding AND the transform matrix used
    // in texture_font_new_face (hres = 64)
    self->underline_position = self->face->underline_position / (float)(64.0f*64.0f) * size;
    self->underline_position = floor( 0.5 + self->underline_position );
    if( self->underline_position > -2 ) {
        self->underline_position = -2.0;
    }

    self->underline_thickness = self->face->underline_thickness / (float)(64.0f*64.0f) * size;
    self->underline_thickness = floor( 0.5 + self->underline_thickness );
    if( self->underline_thickness < 1 ) {
        self->underline_thickness = 1.0;
    }

    FT_Size_Metrics metrics = self->face->size->metrics;
    self->ascender = (metrics.ascender >> 6) / 100.0;
    self->descender = (metrics.descender >> 6) / 100.0;
    self->height = (metrics.height >> 6) / 100.0;
    self->linegap = self->height - self->ascender + self->descender;

    FT_CHECK_CALL(FT_Set_Char_Size, (self->face, (int)(size*64), 0, 72*hres, 72),
                  DoneFace_Error);

    /* -1 is a special glyph */
    texture_font_get_glyph(self, -1);

    return self;
DoneFace_Error:
    FT_Done_Face(self->face);
DoneLibrary_Error:
    FT_Done_FreeType(self->library);
Free_Error:
    free(self);
    return NULL;
}


// ---------------------------------------------------- texture_font_delete ---
void
texture_font_delete( texture_font_t *self )
{
    size_t i;
    texture_glyph_t *glyph;
    for(i=0; i<vector_size(self->glyphs); ++i) {
        glyph = *(texture_glyph_t **) vector_get( self->glyphs, i );
        texture_glyph_delete( glyph);
    }

    vector_delete(self->glyphs);
    FT_Done_Face(self->face);
    FT_Done_FreeType(self->library);
    free(self);
}


texture_glyph_t **
texture_font_load_glyphs( texture_font_t * self,
                          const wchar_t * charcodes )
{
    assert( self );
    assert( charcodes );

    size_t missed = 0, pushed = 0;
    size_t width  = self->atlas->width;
    size_t height = self->atlas->height;
    size_t depth  = self->atlas->depth;

    /* Load each glyph */
    size_t i;
    for( i=0; charcodes[i] != 0; ++i )
    {
        FT_UInt glyph_index = FT_Get_Char_Index( self->face, charcodes[i] );
        // WARNING: We use texture-atlas depth to guess if user wants
        //          LCD subpixel rendering
        FT_Int32 flags = 0;

        if( self->outline_type > 0 ) {
            flags |= FT_LOAD_NO_BITMAP;
        } else {
            flags |= FT_LOAD_RENDER;
        }

        flags |=
            self->hinting
            ? FT_LOAD_FORCE_AUTOHINT
            : FT_LOAD_NO_HINTING | FT_LOAD_NO_AUTOHINT;

        if( depth == 3 ) {
            flags |= FT_LOAD_TARGET_LCD;
        }
        FT_CHECK_CALL(FT_Load_Glyph, ( self->face, glyph_index, flags ), Error);

        FT_Glyph ft_glyph;
        FT_Bitmap ft_bitmap;
        int ft_bitmap_width = 0;
        int ft_bitmap_rows = 0;
        int ft_glyph_top = 0;
        int ft_glyph_left = 0;
        if( 0 == self->outline_type ) {
            FT_GlyphSlot slot = self->face->glyph;
            ft_bitmap       = slot->bitmap;
            ft_bitmap_width = slot->bitmap.width;
            ft_bitmap_rows  = slot->bitmap.rows;
            ft_glyph_top    = slot->bitmap_top;
            ft_glyph_left   = slot->bitmap_left;
        } else {
            FT_Stroker stroker;
            FT_CHECK_CALL(FT_Stroker_New, ( self->library, &stroker ), Error);
            FT_Stroker_Set( stroker,
                            (int)(self->outline_thickness *64),
                            FT_STROKER_LINECAP_ROUND,
                            FT_STROKER_LINEJOIN_ROUND,
                            0);
            FT_CHECK_CALL(FT_Get_Glyph, ( self->face->glyph, &ft_glyph), Error);

            switch(self->outline_type) {
            case 1:
                FT_CHECK_CALL(FT_Glyph_Stroke, ( &ft_glyph, stroker, 1 ), Error);
                break;
            case 2:
                FT_CHECK_CALL(FT_Glyph_StrokeBorder, ( &ft_glyph, stroker, 0, 1 ), Error);
                break;
            case 3:
                FT_CHECK_CALL(FT_Glyph_StrokeBorder, ( &ft_glyph, stroker, 1, 1 ), Error);
                break;
            }

            FT_Render_Mode render_mode = (depth == 1) ? FT_RENDER_MODE_NORMAL : FT_RENDER_MODE_LCD;
            FT_CHECK_CALL(FT_Glyph_To_Bitmap, ( &ft_glyph, render_mode, 0, 1), Error);
            FT_BitmapGlyph ft_bitmap_glyph = (FT_BitmapGlyph) ft_glyph;
            ft_bitmap       = ft_bitmap_glyph->bitmap;
            ft_bitmap_width = ft_bitmap.width;
            ft_bitmap_rows  = ft_bitmap.rows;
            ft_glyph_top    = ft_bitmap_glyph->top;
            ft_glyph_left   = ft_bitmap_glyph->left;
            FT_Stroker_Done(stroker);
        }

        size_t w = ft_bitmap_width/depth;
        size_t h = ft_bitmap_rows;

        ivec4 region = texture_atlas_make_region(
            self->atlas, w, h, ft_bitmap.buffer, ft_bitmap.pitch);
        if(region.x < 0) {
            missed++;
            fprintf( stderr, "Texture atlas is full (line %d)\n",  __LINE__ );
            continue;
        }

        texture_glyph_t *glyph = texture_glyph_new();
        glyph->charcode = charcodes[i];
        glyph->size     = (ivec2){{ region.width, region.height }};
        glyph->outline_type = self->outline_type;
        glyph->outline_thickness = self->outline_thickness;
        glyph->bearing = (ivec2){{ ft_glyph_left, ft_glyph_top }};
        glyph->texture_pos0 = (vec2){{ region.x/(float)width, region.y/(float)height }};
        glyph->texture_pos1 = (vec2){{ (region.x + region.width)/(float)width,
                                       (region.y + region.height)/(float)height }};

        // Discard hinting to get advance
        FT_CHECK_CALL(FT_Load_Glyph, ( self->face, glyph_index, FT_LOAD_RENDER | FT_LOAD_NO_HINTING), Error);
        FT_GlyphSlot slot = self->face->glyph;
        glyph->advance = (vec2){{ slot->advance.x/64.0, slot->advance.y/64.0 }};

        vector_push_back( self->glyphs, &glyph );
        pushed++;

        if(self->outline_type > 0) {
            FT_Done_Glyph( ft_glyph );
        }
    }

    texture_atlas_upload( self->atlas );

    generate_kerning( self );
    if(missed) goto Error;

    return ((texture_glyph_t **) vector_back( self->glyphs )) + 1 - pushed;
Error:
    return NULL;
}

// ------------------------------------------------- texture_font_get_glyph ---
texture_glyph_t *
texture_font_get_glyph( texture_font_t * self,
                        wchar_t charcode )
{
    assert( self );

    size_t i;

    assert( self );
    assert( self->atlas );

    /* Check if charcode has been already loaded */
    for( i=0; i<self->glyphs->size; ++i )
    {
        texture_glyph_t *glyph =
            *(texture_glyph_t **) vector_get( self->glyphs, i );
        // If charcode is -1, we don't care about outline type or thickness
        if( (glyph->charcode == charcode) &&
            ((charcode == (wchar_t)(-1) ) ||
             ((glyph->outline_type == self->outline_type) &&
              (glyph->outline_thickness == self->outline_thickness)) ))
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
        ivec4 region = texture_atlas_make_region(self->atlas, 4, 4, data, 0);
        if (region.x < 0) {
            fprintf( stderr, "Texture atlas is full (line %d)\n",  __LINE__ );
            return NULL;
        }
        texture_glyph_t * glyph = texture_glyph_new();
        glyph->charcode = (wchar_t)(-1);
        size_t width  = self->atlas->width;
        size_t height = self->atlas->height;
        glyph->texture_pos0 = (vec2){{ (region.x+2)/(float)width, (region.y+2)/(float)height }};
        glyph->texture_pos1 = (vec2){{ (region.x+3)/(float)width, (region.y+3)/(float)height }};
        vector_push_back( self->glyphs, &glyph );
        return glyph; //*(texture_glyph_t **) vector_back( self->glyphs );
    }

    /* Glyph has not been already loaded */
    wchar_t buffer[2] = {charcode,0};
    texture_glyph_t **glyphs = texture_font_load_glyphs( self, buffer );
    if(!glyphs) return NULL;
    return *glyphs;
}

void
texture_font_get_text_size(
    texture_font_t *self, wchar_t *text, size_t length,
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
            texture_font_get_glyph( self, text[i] );
        width += glyph->advance.x;
    }
    if(width > maxwidth) maxwidth = width;

    *out_size = (vec2){{ maxwidth, lines * self->descender }};
}

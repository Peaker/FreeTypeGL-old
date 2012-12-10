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
#include "text-buffer.h"

#include "opengl.h"
#include "shader.h"
#include <assert.h>
#include <wchar.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

text_buffer_t *
text_buffer_new( texture_atlas_t *atlas, GLuint shader )
{

    text_buffer_t *self = (text_buffer_t *) malloc (sizeof(text_buffer_t));
    self->buffer = vertex_buffer_new( "v3f:t2f:c4f:1g1f:2g1f" );
    self->atlas = atlas;
    self->shader = shader;
    self->shader_texture = glGetUniformLocation(self->shader, "texture");
    self->shader_pixel = glGetUniformLocation(self->shader, "pixel");
    self->line_start = 0;
    self->line_ascender = 0;
    self->line_descender = 0;
    return self;
}

void
text_buffer_delete( text_buffer_t *self )
{
    vertex_buffer_delete(self->buffer);
    free (self);
}

// ----------------------------------------------------------------------------
void
text_buffer_render( text_buffer_t * self )
{
    glEnable( GL_BLEND );
    glEnable( GL_TEXTURE_2D );
    glColor4f( 1.0, 1.0, 1.0, 1.0);

    if( self->atlas->depth == 1 ) {
        glDisable( GL_COLOR_MATERIAL );
        glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
        vertex_buffer_render( self->buffer, GL_TRIANGLES, "vtc" );
    } else {
        glEnable( GL_COLOR_MATERIAL );
        glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE );
        glBlendFunc( GL_ONE, GL_ONE_MINUS_SRC_ALPHA );
        glBlendColor( 1.0, 1.0, 1.0, 1.0 );
        glUseProgram( self->shader );
        glUniform1i( self->shader_texture, 0 );
        glUniform3f( self->shader_pixel,
                     1.0/self->atlas->width,
                     1.0/self->atlas->height,
                     self->atlas->depth );
        vertex_buffer_render( self->buffer, GL_TRIANGLES, "vtc" );
        glUseProgram( 0 );
    }
}

struct coordinates {
    glyph_vertex_t *start_vertex;
    glyph_vertex_t *cur_vertex;
    glyph_vertex_t *end_vertex;
    GLuint *start_index;
    GLuint *cur_index;
    GLuint *end_index;
};

static GLuint
add_next_vertex( struct coordinates *coors, vec4 *color, float x, float y, float s, float t, float gamma )
{
    float r = color->r;
    float g = color->g;
    float b = color->b;
    float a = color->a;

    glyph_vertex_t *gv = coors->cur_vertex;
    coors->cur_vertex++;

    assert (gv < coors->end_vertex);
    gv->x=(int)x;
    gv->y=y;
    gv->z=0;
    gv->u=s;
    gv->v=t;
    gv->r=r; gv->g=g; gv->b=b; gv->a=a;
    gv->shift=x - (int)x;
    gv->gamma=gamma;

    return gv - coors->start_vertex;
}

static GLuint *
add_next_index( struct coordinates *coors )
{
    GLuint *i = coors->cur_index;
    coors->cur_index++;

    assert (i < coors->end_index);
    return i;
}

static void
add_glyph( struct coordinates *coors,
           vec2 *pen, vec4 *color,
           float x0_offset, float y0_offset,
           float x1_offset, float y1_offset,
           float gamma,
           texture_glyph_t *glyph )
{
    float x0 = ( pen->x + x0_offset );
    float y0 = (int)( pen->y + y0_offset );
    float x1 = ( x0 + x1_offset );
    float y1 = (int)( y0 + y1_offset );
    float s0 = glyph->s0;
    float s1 = glyph->s1;
    float t0 = glyph->t0;
    float t1 = glyph->t1;
    GLuint v0 = add_next_vertex (coors, color, x0, y0, s0, t0, gamma);
    GLuint v1 = add_next_vertex (coors, color, x0, y1, s0, t1, gamma);
    GLuint v2 = add_next_vertex (coors, color, x1, y1, s1, t1, gamma);
    GLuint v3 = add_next_vertex (coors, color, x1, y0, s1, t0, gamma);
    *add_next_index(coors) = v0;
    *add_next_index(coors) = v1;
    *add_next_index(coors) = v2;
    *add_next_index(coors) = v0;
    *add_next_index(coors) = v2;
    *add_next_index(coors) = v3;
}

static void
coors_push_to_vector( struct coordinates *coors, vertex_buffer_t * buffer )
{
    vertex_buffer_push_back(
        buffer,
        coors->start_vertex,
        coors->cur_vertex - coors->start_vertex,
        coors->start_index,
        coors->cur_index - coors->start_index );
}

static void
text_buffer_add_wchar( text_buffer_t * self,
                       vec2 * pen, markup_t * markup,
                       texture_font_t * font,
                       wchar_t current, wchar_t previous )
{
    float gamma = markup->gamma;

    // Maximum number of vertices is 20 (= 5x2 triangles) per glyph:
    //  - 2 triangles for background
    //  - 2 triangles for overline
    //  - 2 triangles for underline
    //  - 2 triangles for strikethrough
    //  - 2 triangles for glyph
    glyph_vertex_t vertices[4*5];
    GLuint indices[6*5];
    struct coordinates coors = {
        vertices, vertices,
        vertices + sizeof vertices / sizeof *vertices,
        indices, indices,
        indices + sizeof indices / sizeof *indices
    };

    if( current == L'\n' )
    {
        pen->x = self->origin.x;
        pen->y += (int)(self->line_descender);
        self->line_descender = 0;
        self->line_ascender = 0;
        self->line_start = vector_size( self->buffer->items );
        return;
    }

    texture_glyph_t *glyph = texture_font_get_glyph( font, current );
    texture_glyph_t *black = texture_font_get_glyph( font, -1 );

    if( glyph == NULL )
    {
        return;
    }

    float kerning = previous ? texture_glyph_get_kerning( glyph, previous ) : 0;
    pen->x += kerning;

    // Background
    if( markup->background_color.alpha > 0 ) {
        add_glyph(&coors, pen, &markup->background_color,
                  -kerning, font->descender,
                  glyph->advance_x, font->height + font->linegap,
                  gamma, black);
    }

    // Underline
    if( markup->underline ) {
        add_glyph(&coors, pen, &markup->underline_color,
                  -kerning, font->underline_position,
                  glyph->advance_x, font->underline_thickness,
                  gamma, black);
    }

    // Overline
    if( markup->overline ) {
        add_glyph(&coors, pen, &markup->overline_color,
                  -kerning, (int)font->ascender,
                  glyph->advance_x, (int)font->underline_thickness,
                  gamma, black);
    }

    // Actual glyph
    add_glyph(&coors, pen, &markup->foreground_color,
              glyph->offset_x, glyph->offset_y,
              glyph->width, -(float)glyph->height,
              gamma, glyph);

    /* Strikethrough */
    if( markup->strikethrough ) {
        add_glyph(&coors, pen, &markup->strikethrough_color,
                  -kerning, (int)font->ascender*.33,
                  glyph->advance_x, font->underline_thickness,
                  gamma, black);
    }

    coors_push_to_vector( &coors, self->buffer );
    pen->x += glyph->advance_x;
}

// ----------------------------------------------------------------------------
void
text_buffer_add_text( text_buffer_t * self,
                      vec2 * pen, markup_t * markup,
                      texture_font_t * font,
                      wchar_t * text, size_t length )
{
    vertex_buffer_t * buffer = self->buffer;

    if( 0 == length ) length = wcslen(text);
    if( 0 == vertex_buffer_size( self->buffer ) ) self->origin = *pen;

    if( font->ascender > self->line_ascender ) {
        size_t i, j;
        float dy = (int)(font->ascender - self->line_ascender);
        for( i=self->line_start; i < vector_size( buffer->items ); ++i )
        {
            ivec4 *item = (ivec4 *) vector_get( buffer->items, i);
            for( j=item->vstart; j<item->vstart+item->vcount; ++j)
            {
                glyph_vertex_t * vertex =
                    (glyph_vertex_t *)  vector_get( buffer->vertices, j );
                vertex->y -= dy;
            }
        }
        self->line_ascender = font->ascender;
        pen->y -= dy;
    }

    if( font->descender < self->line_descender ) {
        self->line_descender = font->descender;
    }

    text_buffer_add_wchar( self, pen, markup, font, text[0], 0 );

    size_t i;
    for( i=1; i<length; ++i ) {
        text_buffer_add_wchar( self, pen, markup, font, text[i], text[i-1] );
    }
}

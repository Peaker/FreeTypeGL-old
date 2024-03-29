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
#ifndef __TEXT_BUFFER_H__
#define __TEXT_BUFFER_H__

#ifdef __cplusplus
extern "C" {
#endif

#include "markup.h"
#include "texture-atlas.h"
#include "texture-font.h"
#include "glyph-cache.h"
#include "vertex-buffer.h"


/**
 * Use LCD filtering
 */
#define LCD_FILTERING_ON    3

/**
 * Do not use LCD filtering
 */
#define LCD_FILTERING_OFF 1

/**
 * @file   text-buffer.h
 * @author Nicolas Rougier (Nicolas.Rougier@inria.fr)
 *
 * @defgroup text-buffer Text buffer
 *
 *
 * <b>Example Usage</b>:
 * @code
 * #include "shader.h"
 *
 * int main( int arrgc, char *argv[] )
 * {
 *
 *     return 0;
 * }
 * @endcode
 *
 * @{
 */

/**
 * Text buffer structure
 */
typedef struct {
    /**
     * Vertex buffer
     */
    vertex_buffer_t *buffer;

    glyph_cache_t cache;

    /**
     * Pen origin
     */
    vec2 origin;

    /**
     * Index (in the vertex buffer) of the line start
     */
    size_t line_start;

    /**
     * Current line ascender
     */
    float line_ascender;

    /**
     * Current line decender
     */
    float line_descender;

    /**
     * Shader handler
     */
    GLuint shader;

    /**
     * Shader "texture" location
     */
    GLuint shader_texture;

    /**
     * Shader "pixel" location
     */
    GLuint shader_pixel;

} text_buffer_t;

/**
 * Creates a new empty text buffer.
 *
 * @return  a new empty text buffer.
 *
 */
text_buffer_t *text_buffer_new( GLuint shader, const ivec2 *size, int depth );

/**
 * Delete a text buffer.
 *
 * @param self a text buffer
 *
 */
void
text_buffer_delete( text_buffer_t *self );


/**
 * Render a text buffer.
 *
 * @param self a text buffer
 *
 */
void
text_buffer_render( text_buffer_t * self );

 /**
  * Add some text to the text buffer
  *
  * @param self   a text buffer
  * @param pen    position of text start
  * @param markup Markup to be used to add text
  * @param text   Text to be added
  * @param length Length of text to be added (or 0 for null termination)
  *
  * @return 0 for success.
  */
int
text_buffer_add_text( text_buffer_t * self,
                      vec2 * pen, markup_t * markup,
                      texture_font_t * font,
                      wchar_t * text );

/** @} */

#ifdef __cplusplus
}
#endif

#endif /* #define __TEXT_BUFFER_H__ */

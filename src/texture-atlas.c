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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include "texture-atlas.h"


// ------------------------------------------------------ texture_atlas_new ---
texture_atlas_t *
texture_atlas_new( const ivec2 *size, const size_t depth )
{
    assert( (depth == 1) || (depth == 3) || (depth == 4) );

    texture_atlas_t *self = (texture_atlas_t *) malloc( sizeof(texture_atlas_t) );
    assert (self);
    self->nodes = vector_new( sizeof(ivec3) );
    self->used = 0;
    self->size = *size;
    self->depth = depth;
    self->id = 0;
    self->uploaded = false;

    // We want a one pixel border around the whole atlas to avoid any artefact when
    // sampling texture
    ivec3 node = {{1,1,size->x-2}};

    vector_push_back( self->nodes, &node );
    self->data = (unsigned char *)
        calloc( size->x*size->y*depth, sizeof(unsigned char) );

    assert(self->data);

    return self;
}


// --------------------------------------------------- texture_atlas_delete ---
void
texture_atlas_delete( texture_atlas_t *self )
{
    assert( self );
    vector_delete( self->nodes );
    if( self->data )
    {
        free( self->data );
    }
    if( !self->id )
    {
        glDeleteTextures( 1, &self->id );
    }
    free( self );
}


static void set_region(
    texture_atlas_t * self,
    ivec4 region,
    const unsigned char * data,
    size_t stride )
{
    assert(self);
    assert(region.x > 0);
    assert(region.y > 0);
    assert(region.x + region.width < self->size.x);
    assert(region.y + region.height < self->size.y);

    if(!region.width) return;

    size_t i;
    size_t depth = self->depth;
    size_t charsize = sizeof(char);
    for(i=0; i<region.height; ++i) {
        memcpy( self->data+((region.y+i)*self->size.x + region.x ) * charsize * depth,
                data + (i*stride) * charsize, region.width * charsize * depth  );
    }
}


// ------------------------------------------------------ texture_atlas_fit ---
int
texture_atlas_fit( texture_atlas_t * self,
                   const size_t index,
                   const size_t width,
                   const size_t height )
{
    assert( self );

    ivec3 *node = (ivec3 *) (vector_get( self->nodes, index ));
    int x = node->x, y, width_left = width;
    size_t i = index;

    if ( (x + width) > (self->size.x-1) )
    {
        return -1;
    }
    y = node->y;
    while( width_left > 0 )
    {
        node = (ivec3 *) (vector_get( self->nodes, i ));
        if( node->y > y )
        {
            y = node->y;
        }
        if( (y + height) > (self->size.y-1) )
        {
            return -1;
        }
        width_left -= node->z;
        ++i;
    }
    return y;
}


// ---------------------------------------------------- texture_atlas_merge ---
void
texture_atlas_merge( texture_atlas_t * self )
{
    assert( self );

    ivec3 *node, *next;
    size_t i;

    for( i=0; i< self->nodes->size-1; ++i )
    {
        node = (ivec3 *) (vector_get( self->nodes, i ));
        next = (ivec3 *) (vector_get( self->nodes, i+1 ));
        if( node->y == next->y )
        {
            node->z += next->z;
            vector_erase( self->nodes, i+1 );
            --i;
        }
    }
}


ivec4 texture_atlas_make_region(
    texture_atlas_t *self,
    size_t narrow_width,
    size_t narrow_height,
    const unsigned char *data,
    size_t stride)
{
    // We want each glyph to be separated by at least one black pixel:
    size_t width = narrow_width + 1;
    size_t height = narrow_height + 1;
    assert( self );

    ivec4 region = {{0,0,narrow_width,narrow_height}};

    int best_height = INT_MAX;
    int best_index  = -1;
    int best_width = INT_MAX;

    size_t i;
    for(i=0; i<self->nodes->size; ++i) {
        int y = texture_atlas_fit(self, i, width, height);
        if( y >= 0 ) {
            ivec3 *node = (ivec3 *) vector_get( self->nodes, i );
            if( ( (y + height) < best_height ) ||
                ( ((y + height) == best_height) && (node->z < best_width)) )
            {
                best_height = y + height;
                best_index = i;
                best_width = node->z;
                region.x = node->x;
                region.y = y;
            }
        }
    }

    if( best_index == -1 ) {
        region.x = -1;
        region.y = -1;
        region.width = 0;
        region.height = 0;
        return region;
    }

    {
        ivec3 node =
            { { region.x
              , region.y + height
              , width } };
        vector_insert( self->nodes, best_index, &node );
    }

    for(i = best_index+1; i < self->nodes->size; ++i)
    {
        ivec3 *node = (ivec3 *) vector_get( self->nodes, i );
        ivec3 *prev = (ivec3 *) vector_get( self->nodes, i-1 );

        if (node->x >= (prev->x + prev->z) ) break;

        int shrink = prev->x + prev->z - node->x;
        node->x += shrink;
        node->z -= shrink;
        if(node->z > 0) break;

        vector_erase( self->nodes, i );
        --i;
    }
    texture_atlas_merge( self );
    self->used += width * height;

    set_region(self, region, data, stride);
    self->uploaded = false;
    return region;
}


// ---------------------------------------------------- texture_atlas_clear ---
void texture_atlas_clear( texture_atlas_t * self )
{
    assert( self );
    assert( self->data );

    vector_clear( self->nodes );
    self->used = 0;
    // We want a one pixel border around the whole atlas to avoid any artefact when
    // sampling texture
    ivec3 node = {{1,1,self->size.x-2}};
    vector_push_back( self->nodes, &node );
    memset( self->data, 0, self->size.x*self->size.y*self->depth );

    self->uploaded = false;
}


// --------------------------------------------------- texture_atlas_upload ---
void
texture_atlas_upload( texture_atlas_t * self )
{
    if(self->uploaded) return;
    if(!self->id) glGenTextures( 1, &self->id );

    glBindTexture( GL_TEXTURE_2D, self->id );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
    if( self->depth == 4 )
    {
#ifdef GL_UNSIGNED_INT_8_8_8_8_REV
        glTexImage2D( GL_TEXTURE_2D, 0, GL_RGBA, self->size.x, self->size.y,
                      0, GL_BGRA, GL_UNSIGNED_INT_8_8_8_8_REV, self->data );
#else
        glTexImage2D( GL_TEXTURE_2D, 0, GL_RGBA, self->size.x, self->size.y,
                      0, GL_RGBA, GL_UNSIGNED_BYTE, self->data );
#endif
    }
    else if( self->depth == 3 )
    {
        glTexImage2D( GL_TEXTURE_2D, 0, GL_RGB, self->size.x, self->size.y,
                      0, GL_RGB, GL_UNSIGNED_BYTE, self->data );
    }
    else
    {
        glTexImage2D( GL_TEXTURE_2D, 0, GL_ALPHA, self->size.x, self->size.y,
                      0, GL_ALPHA, GL_UNSIGNED_BYTE, self->data );
    }
    self->uploaded = true;
}

void
texture_atlas_render( texture_atlas_t * self,
                      float x, float y,
                      float width, float height )
{
    glEnable( GL_BLEND );
    glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );

    glEnable( GL_TEXTURE_2D );
    glColor4f(0,0,0,1);
    glBindTexture( GL_TEXTURE_2D, self->id );

    glBegin(GL_QUADS);
    glTexCoord2f( 0, 1 ); glVertex2i( 0, 0 );
    glTexCoord2f( 0, 0 ); glVertex2i( 0, height );
    glTexCoord2f( 1, 0 ); glVertex2i( width, height );
    glTexCoord2f( 1, 1 ); glVertex2i( width, 0 );
    glEnd();
}

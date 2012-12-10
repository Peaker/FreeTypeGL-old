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
 * Example showing texture atlas packing
 *
 * ============================================================================
 */
#include "freetype-gl.h"
#include <GL/glut.h>

texture_atlas_t * atlas = NULL;

// ---------------------------------------------------------------- display ---
static void display( void )
{
    int viewport[4];
    glGetIntegerv( GL_VIEWPORT, viewport );
    GLuint width  = viewport[2];
    GLuint height = viewport[3];

    glClearColor(1,1,1,1);
    glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );

    texture_atlas_render(atlas, 0, 0, width, height);

    glutSwapBuffers( );
}


// ---------------------------------------------------------------- reshape ---
void reshape( int width, int height )
{
    glViewport(0, 0, width, height);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho(0, width, 0, height, -1, 1);
    glMatrixMode(GL_MODELVIEW);
    glutPostRedisplay();
}


// --------------------------------------------------------------- keyboard ---
void keyboard( unsigned char key, int x, int y )
{
    if ( key == 27 )
    {
        exit( 1 );
    }
}

// ------------------------------------------------------------------- main ---
int main( int argc, char **argv )
{
    glutInit( &argc, argv );
    glutInitWindowSize( 512, 512 );
    glutInitDisplayMode( GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH );
    glutCreateWindow( argv[0] );
    glutReshapeFunc( reshape );
    glutDisplayFunc( display );
    glutKeyboardFunc( keyboard );

    atlas = texture_atlas_new( 512, 512, 1 );
    assert(atlas);
    const char *filename = "../fonts/Vera.ttf";
    const wchar_t *cache = L" !\"#$%&'()*+,-./0123456789:;<=>?"
                           L"@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_"
                           L"`abcdefghijklmnopqrstuvwxyz{|}~";
    size_t minsize = 8, maxsize = 27;
    size_t count = maxsize - minsize;
    size_t i, missed = 0;

    for( i=minsize; i < maxsize; ++i )
    {
        texture_font_t * font = texture_font_new( atlas, filename, i );
        if(!font) {
            fprintf(stderr, "Failed to load font: \"%s\"\n", filename);
            return -1;
        }
        missed += texture_font_load_glyphs( font, cache );
        texture_font_delete( font );
    }

    printf( "Matched font               : %s\n", filename );
    printf( "Number of fonts            : %zd\n", count );
    printf( "Number of glyphs per font  : %zd\n", wcslen(cache) );
    printf( "Number of missed glyphs    : %zd\n", missed );
    printf( "Total number of glyphs     : %zd/%zd\n",
            wcslen(cache)*count - missed, wcslen(cache)*count );
    printf( "Texture size               : %zdx%zd\n", atlas->width, atlas->height );
    printf( "Texture occupancy          : %.2f%%\n",
            100.0*atlas->used/(float)(atlas->width*atlas->height) );

    glutMainLoop( );

    return 0;
}

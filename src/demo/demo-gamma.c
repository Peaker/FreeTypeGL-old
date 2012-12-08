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
 * Example showing gamma influence
 *
 * ============================================================================
 */
#include "freetype-gl.h"

#include "font-manager.h"
#include "vertex-buffer.h"
#include "text-buffer.h"
#include "markup.h"
#include "shader.h"


// ------------------------------------------------------- typedef & struct ---
typedef struct {
    float x, y, z;
    float r, g, b;
} vertex_t;

// ------------------------------------------------------- global variables ---
text_buffer_t *text_buffer;
vertex_buffer_t *buffer;


// ---------------------------------------------------------------- display ---
void display( void )
{
    glClearColor( 1.0,1.0,1.0,1.0 );
    glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
    glDisable( GL_TEXTURE_2D );
    vertex_buffer_render( buffer, GL_TRIANGLES, "vc" );
    text_buffer_render( text_buffer );
    glutSwapBuffers( );
}


// ---------------------------------------------------------------- reshape ---
void reshape(int width, int height)
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

    buffer = vertex_buffer_new( "v3f:c3f" );
    vertex_t vertices[4*2] = { {  0,  0,0, 1,1,1}, {  0,256,0, 1,1,1},
                               {512,256,0, 1,1,1}, {512,  0,0, 1,1,1},
                               {0,  256,0, 0,0,0}, {  0,512,0, 0,0,0},
                               {512,512,0, 0,0,0}, {512,256,0, 0,0,0} };
    GLuint indices[4*3] = { 0,1,2, 0,2,3, 4,5,6, 4,6,7 };
    vertex_buffer_push_back( buffer, vertices, 8, indices, 12 );
    

    text_buffer = text_buffer_new( LCD_FILTERING_ON );
    vec4 white = {{1.0, 1.0, 1.0, 1.0}};
    vec4 black = {{0.0, 0.0, 0.0, 1.0}};
    vec4 none  = {{1.0, 1.0, 1.0, 0.0}};
    markup_t markup = {
        .family  = "fonts/Vera.ttf",
        .size    = 15.0, .bold    = 0,   .italic  = 0,
        .rise    = 0.0,  .spacing = 0.0, .gamma   = 1.0,
        .foreground_color    = white, .background_color    = none,
        .underline           = 0,     .underline_color     = none,
        .overline            = 0,     .overline_color      = none,
        .strikethrough       = 0,     .strikethrough_color = none,
        .font = 0,
    };

    size_t i;
    vec2 pen = {{32, 500}};
    wchar_t *text = L"A Quick Brown Fox Jumps Over The Lazy Dog 0123456789\n";
    for( i=0; i < 14; ++i )
    {
        markup.gamma = 0.75 + 1.5*i*(1.0/14);
        text_buffer_add_text( text_buffer, &pen, &markup, text, wcslen(text) );
    }
    pen = (vec2) {{32, 234}};
    markup.foreground_color = black;
    for( i=0; i < 14; ++i )
    {
        markup.gamma = 0.75 + 1.5*i*(1.0/14);
        text_buffer_add_text( text_buffer, &pen, &markup, text, wcslen(text) );
    }

    glutMainLoop( );
    return 0;
}

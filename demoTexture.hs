import Control.Monad
import Graphics.Rendering.OpenGL.GL (($=))
import Graphics.Rendering.OpenGL.GL (Color4(..))
import System.Environment (getArgs)
import qualified Graphics.Rendering.FreeTypeGL.Internal.Atlas as Atlas
import qualified Graphics.Rendering.FreeTypeGL.Internal.TextureFont as TextureFont
import qualified Graphics.Rendering.FreeTypeGL as FGL
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLFW as GLFW

resX, resY :: Num a => a
resX = 800
resY = 600

initScreen :: IO ()
initScreen = do
  True <- GLFW.initialize
  True <- GLFW.openWindow GLFW.defaultDisplayOptions {
    GLFW.displayOptions_width = resX,
    GLFW.displayOptions_height = resY
    }
  return ()

main = do
    -- glutInit( &argc, argv );
    -- glutInitWindowSize( 512, 512 );
    -- glutInitDisplayMode( GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH );
    -- glutCreateWindow( argv[0] );
  initScreen
    -- glutReshapeFunc( reshape );
    -- glutDisplayFunc( display );
    -- glutKeyboardFunc( keyboard );
  GLFW.setWindowCloseCallback $ fail "Quit"

    -- atlas = texture_atlas_new( 512, 512, 1 );
    -- assert(atlas);
  atlas <- Atlas.new (GL.Vector2 512 512) 1
    -- const char *filename = "../fonts/Vera.ttf";
  let filename = "src/fonts/Vera.ttf"
    -- const wchar_t *cache = L" !\"#$%&'()*+,-./0123456789:;<=>?"
    --                        L"@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_"
    --                        L"`abcdefghijklmnopqrstuvwxyz{|}~";
      cache =
        concat
        [ " !\"#$%&'()*+,-./0123456789:;<=>?"
        , "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_"
        , "`abcdefghijklmnopqrstuvwxyz{|}~"
        ]
    -- size_t minsize = 8, maxsize = 27;
  let minSize = 8
      maxSize = 27
      count = maxSize - minSize
    -- size_t count = maxsize - minsize;
    -- size_t i, missed = 0;

    -- for( i=minsize; i < maxsize; ++i )
    -- {
  forM_ [minSize .. maxSize-1] $ \i -> do
        -- texture_font_t * font = texture_font_new( atlas, filename, i );
        -- if(!font) {
        --     fprintf(stderr, "Failed to load font: \"%s\"\n", filename);
        --     return -1;
        -- }
    font <- TextureFont.new atlas filename i
        -- missed += texture_font_load_glyphs( font, cache );
    missed <- TextureFont.loadGlyphs font cache
    putStrLn $ "Missed: " ++ show missed
        -- texture_font_delete( font );
    -- }

    -- printf( "Matched font               : %s\n", filename );
    -- printf( "Number of fonts            : %zd\n", count );
    -- printf( "Number of glyphs per font  : %zd\n", wcslen(cache) );
    -- printf( "Number of missed glyphs    : %zd\n", missed );
    -- printf( "Total number of glyphs     : %zd/%zd\n",
    --         wcslen(cache)*count - missed, wcslen(cache)*count );
    -- printf( "Texture size               : %zdx%zd\n", atlas->width, atlas->height );
    -- printf( "Texture occupancy          : %.2f%%\n",
    --         100.0*atlas->used/(float)(atlas->width*atlas->height) );

    -- glViewport(0, 0, width, height);
    -- glMatrixMode(GL_PROJECTION);
    -- glLoadIdentity();
    -- glOrtho(0, width, 0, height, -1, 1);
    -- glMatrixMode(GL_MODELVIEW);
    -- glutPostRedisplay();
  GL.viewport GL.$= (GL.Position 0 0, GL.Size resX resY)
  GL.matrixMode GL.$= GL.Projection
  GL.loadIdentity
  GL.ortho 0 resX 0 resY (-1) 1
  GL.matrixMode GL.$= GL.Modelview 0

  forever $ do
    -- int viewport[4];
    -- glGetIntegerv( GL_VIEWPORT, viewport );
    -- GLuint width  = viewport[2];
    -- GLuint height = viewport[3];

    -- glClearColor(1,1,1,1);
    -- glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
    GL.clearColor GL.$= GL.Color4 1 1 1 1
    GL.clear [ GL.ColorBuffer, GL.DepthBuffer ]

    -- texture_atlas_render(atlas, 0, 0, width, height);
    Atlas.render atlas (GL.Vector2 0 0) (GL.Vector2 resX resY)

    -- glutSwapBuffers( );
    GLFW.swapBuffers

    -- return 0;
-- }

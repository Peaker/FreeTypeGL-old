import Control.Monad (forever)
import Foreign -- (peekArray, withForeignPtr)
import Graphics.Rendering.FreeTypeGL.Internal.Markup (Markup (..))
import Graphics.Rendering.FreeTypeGL.Internal.TextBuffer (TextBuffer)
import Graphics.Rendering.OpenGL.GL (($=))
import Graphics.Rendering.OpenGL.GL (Color4(..))
import System.Environment (getArgs)
import qualified Graphics.Rendering.FreeTypeGL.Internal.Atlas as Atlas
import qualified Graphics.Rendering.FreeTypeGL.Internal.Shader as Shader
import qualified Graphics.Rendering.FreeTypeGL.Internal.TextBuffer as TextBuffer
import qualified Graphics.Rendering.FreeTypeGL.Internal.TextureFont as TextureFont
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

reshape :: IO ()
reshape = do
  GL.viewport $= (GL.Position 0 0, GL.Size resX resY)
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.ortho 0 resX 0 resY (-1) 1
  GL.matrixMode $= GL.Modelview 0

display :: ForeignPtr TextBuffer -> IO ()
display textBuffer = do
  GL.clearColor $= GL.Color4 0.2 0 0 0
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  GL.texture GL.Texture2D $= GL.Disabled
  TextBuffer.render textBuffer
  GLFW.swapBuffers

main :: IO ()
main = do
  [ttfFilename] <- getArgs
  initScreen
  atlas <- Atlas.new (GL.Vector2 512 512) 3 -- depth
  shader <- Shader.load "src/shaders/text.vert" "src/shaders/text.frag"
  font <- TextureFont.new atlas ttfFilename 72.0
  pen <- TextBuffer.newPen 0 500
  textBuffer <- TextBuffer.new atlas shader
  reshape
  alloca $ \markup -> do
    poke markup Markup
      { rise = 0
      , spacing = 0
      , gamma = 1.0
      , foreground_color = Color4 1 0 0 1
      , background_color = Color4 0.3 0.3 0.3 0.3
      , outline = Just $ Color4 0.1 0.1 0.1 0.8
      , underline = Just $ Color4 0.8 0.3 0.2 0.3
      , overline = Just $ Color4 0.1 0.5 0.1 1
      , strikethrough = Just $ Color4 0 0 0.8 1
      }
    GLFW.setWindowCloseCallback $ error "Quit"
    TextBuffer.addText textBuffer markup font pen "Hello world"
    forever $ do
      display textBuffer
      GLFW.pollEvents

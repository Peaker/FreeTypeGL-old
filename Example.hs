import Control.Monad (forever)
import Graphics.Rendering.FreeTypeGL.Internal.TextBuffer (TextBuffer)
import Graphics.Rendering.OpenGL.GL (($=))
import System.Environment (getArgs)
import qualified Graphics.Rendering.FreeTypeGL.Internal.Atlas as Atlas
import qualified Graphics.Rendering.FreeTypeGL.Internal.TextBuffer as TextBuffer
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLFW as GLFW

import Foreign -- (peekArray, withForeignPtr)

resX, resY :: Num a => a
resX = 512
resY = 512

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

display :: TextBuffer -> IO ()
display textBuffer = do
  GL.clearColor $= GL.Color4 0.2 0 0 0
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  GL.texture GL.Texture2D $= GL.Disabled
  TextBuffer.render textBuffer
  GLFW.swapBuffers

main :: IO ()
main = do
  return ()
  -- [ttfFilename] <- getArgs
  -- initScreen
  -- atlas <- Atlas.new (512, 512) 3 -- depth
  -- textBuffer <- TextBuffer.new atlas "src/shaders/text.vert" "src/shaders/text.frag"
  -- font <- Atlas.loadFont atlas ttfFilename
  -- pen <- TextBuffer.newPen
  -- reshape
  -- GLFW.setWindowCloseCallback $ error "Quit"
  -- withForeignPtr pen $ \penPtr ->
  --   pokeArray penPtr [32, 500]
  -- TextBuffer.add textBuffer font pen "Hello world"
  -- forever $ do
  --   display textBuffer
  --   GLFW.pollEvents

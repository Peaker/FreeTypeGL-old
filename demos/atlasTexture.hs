import Control.Monad
import Graphics.Rendering.OpenGL.GL (($=))
import qualified Graphics.Rendering.FreeTypeGL.Internal.Atlas as Atlas
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

main :: IO ()
main = do
  initScreen
  GLFW.setWindowCloseCallback $ fail "Quit"

  atlas <- Atlas.new (GL.Vector2 2048 2048) 1
  let filename = "src/fonts/Vera.ttf"
      cache =
        concat
        [ " !\"#$%&'()*+,-./0123456789:;<=>?"
        , "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_"
        , "`abcdefghijklmnopqrstuvwxyz{|}~"
        ]
  let minSize = 8
      maxSize = 27
  forM_ [minSize .. maxSize-1] $ \i -> do
    font <- TextureFont.new atlas filename i
    missed <- TextureFont.loadGlyphs font cache
    putStrLn $ "Missed: " ++ show missed
  GL.viewport $= (GL.Position 0 0, GL.Size resX resY)
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.ortho 0 resX 0 resY (-1) 1
  GL.matrixMode $= GL.Modelview 0

  forever $ do
    GL.clearColor $= GL.Color4 1 1 1 1
    GL.clear [ GL.ColorBuffer, GL.DepthBuffer ]

    Atlas.render atlas (GL.Vector2 0 0) (GL.Vector2 resX resY)

    GLFW.swapBuffers

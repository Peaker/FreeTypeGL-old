import Control.Monad (forever)
import Graphics.Rendering.OpenGL.GL (($=))
import Graphics.Rendering.OpenGL.GL (Color4(..))
import System.Environment (getArgs)
import qualified Graphics.Rendering.FreeTypeGL as FGL
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLFW as GLFW

resX, resY :: Num a => a
resX = 800
resY = 600

initScreen :: IO ()
initScreen = do
  True <- GLFW.initialize
  True <- GLFW.openWindow GLFW.defaultDisplayOptions
    { GLFW.displayOptions_width = resX
    , GLFW.displayOptions_height = resY
    -- , GLFW.displayOptions_openGLProfile = GLFW.CompatibilityProfile
    }
  return ()

reshape :: IO ()
reshape = do
  GL.viewport $= (GL.Position 0 0, GL.Size resX resY)
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.ortho 0 resX 0 resY (-1) 1
  GL.matrixMode $= GL.Modelview 0

markup :: FGL.Markup
markup = FGL.Markup
  { FGL.rise = 0
  , FGL.spacing = 0
  , FGL.gamma = 1.0
  , FGL.foreground_color = Color4 1 0 0 1
  , FGL.background_color = Color4 0.3 0.3 0.3 0.3
  , FGL.outline = Just $ Color4 0.1 0.1 0.1 0.8
  , FGL.underline = Just $ Color4 0.8 0.3 0.2 0.3
  , FGL.overline = Just $ Color4 0.1 0.5 0.1 1
  , FGL.strikethrough = Just $ Color4 0 0 0.8 1
  }

main :: IO ()
main = do
  [ttfFilename] <- getArgs
  initScreen
  GLFW.setWindowCloseCallback $ fail "Quit"

  shader <- FGL.newShader
  font <- FGL.loadFont shader ttfFilename 72.0
  let hello = FGL.textRenderer (GL.Vector2 100 100) markup font "Hello world"
      bye = FGL.textRenderer (GL.Vector2 100 200) FGL.noMarkup font "Bye world"

  forever $ do
    reshape
    GL.clearColor $= GL.Color4 0.2 0 0 0
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    GL.texture GL.Texture2D $= GL.Disabled
    FGL.renderText hello
    FGL.renderText bye
    GLFW.swapBuffers
    GLFW.pollEvents

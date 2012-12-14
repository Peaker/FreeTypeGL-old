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

helloMarkup :: FGL.Markup
helloMarkup = FGL.Markup
  { FGL.gamma = 1.0
  , FGL.foreground_color = Color4 1 0 0 1
  , FGL.background_color = Color4 0.3 0.3 0.3 0.3
  , FGL.underline = Just $ Color4 0.8 0.3 0.2 0.3
  , FGL.overline = Just $ Color4 0.1 0.5 0.1 1
  , FGL.strikethrough = Nothing
  }

byeMarkup :: FGL.Markup
byeMarkup = FGL.noMarkup
  { FGL.strikethrough = Just $ Color4 0 0 0.8 1
  }

main :: IO ()
main = do
  [ttfFilename] <- getArgs
  initScreen
  GLFW.setWindowCloseCallback $ fail "Quit"

  let
    fontSize :: Fractional a => a
    fontSize = 72.0
  shader <- FGL.newShader
  font <- FGL.loadFont shader ttfFilename fontSize
  let hello = FGL.textRenderer helloMarkup font "Hello world"
      bye = FGL.textRenderer byeMarkup font "Bye world"

  forever $ do
    reshape
    GL.clearColor $= GL.Color4 0 0 0 0
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    GL.texture GL.Texture2D $= GL.Disabled
    let
      drawAt (GL.Vector2 x y) r = GL.preservingMatrix $ do
        GL.translate (GL.Vector3 x y (0 :: GL.GLfloat))
        FGL.renderText r
    drawAt (GL.Vector2 0 (fontSize + 2)) hello
    drawAt (GL.Vector2 0 resY) bye
    GLFW.swapBuffers
    GLFW.pollEvents

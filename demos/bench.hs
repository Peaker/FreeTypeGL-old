import Criterion.Main
import qualified Graphics.Rendering.FreeTypeGL as FGL
import qualified Graphics.UI.GLFW as GLFW

initScreen :: IO ()
initScreen = do
  True <- GLFW.initialize
  True <- GLFW.openWindow GLFW.defaultDisplayOptions
    { GLFW.displayOptions_width = 800
    , GLFW.displayOptions_height = 600
    -- , GLFW.displayOptions_openGLProfile = GLFW.CompatibilityProfile
    }
  return ()

main :: IO ()
main = do
  initScreen
  shader <- FGL.newShader

  defaultMain
    [ bench "Making/destroying atlases" $ FGL.newAtlas FGL.defaultAtlasNewParams
    ]

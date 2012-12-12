import Criterion.Main
import Foreign.ForeignPtr (ForeignPtr, finalizeForeignPtr)
import qualified Graphics.Rendering.FreeTypeGL as FGL
import qualified Graphics.Rendering.FreeTypeGL.Internal.Atlas as Atlas
import qualified Graphics.Rendering.FreeTypeGL.Internal.TextureFont as ITF
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

newDestroyFont :: FGL.Shader -> ForeignPtr Atlas.Atlas -> IO ()
newDestroyFont _shader iatlas = do
  ifont <- ITF.new iatlas "src/fonts/Vera.ttf" 72.0
  finalizeForeignPtr ifont

main :: IO ()
main = do
  initScreen
  shader <- FGL.newShader
  iatlas <- Atlas.new (Atlas.Vector2 512 512) 3
  defaultMain
    [ bench "Making/destroying atlases" $ FGL.newAtlas FGL.defaultAtlasNewParams
    , bench "Making/destroy fonts" $ newDestroyFont shader iatlas
    ]

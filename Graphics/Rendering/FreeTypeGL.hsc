{-# LANGUAGE ForeignFunctionInterface #-}
-- | A higher-level interface wrapping the low-level C API

module Graphics.Rendering.FreeTypeGL
  ( FontDesc(..), fontDescFindFileName
  , Shader, newShader
  , Atlas, renderAtlas
  , AtlasNewParams(..), defaultAtlasNewParams, newAtlas
  , Font, loadFont, textSize, Vector2(..)
  , Markup(..), noMarkup, Color4(..)
  , TextRenderer, textRenderer, renderText
  , initialize
  ) where

import Control.Applicative ((<$>))
import Foreign.C.String (withCString)
import Foreign.C.Types (CInt(..))
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Error (throwIf_)
import Foreign.Storable (poke)
import Graphics.Rendering.FreeTypeGL.Internal.Markup (Markup(..), noMarkup)
import Graphics.Rendering.FreeTypeGL.Internal.Shader (Shader)
import Graphics.Rendering.OpenGL.GL (Color4(..), Vector2(..))
import Paths_FreeTypeGL (getDataFileName)
import System.IO.Unsafe (unsafePerformIO)  -- for pure textWidth
import qualified Graphics.Rendering.FreeTypeGL.Internal.Atlas as IAtlas
import qualified Graphics.Rendering.FreeTypeGL.Internal.FontDesc as IFD
import qualified Graphics.Rendering.FreeTypeGL.Internal.Shader as Shader
import qualified Graphics.Rendering.FreeTypeGL.Internal.TextBuffer as ITB
import qualified Graphics.Rendering.FreeTypeGL.Internal.TextureFont as ITF

-- FontDesc:

data FontDesc = FontDesc
  { fdFamily :: String
  , fdSize :: Float
  , fdBold :: Bool
  , fdItalic :: Bool
  }

fontDescFindFileName :: FontDesc -> IO String
fontDescFindFileName (FontDesc family size bold italic) =
  withCString family $ \familyPtr ->
  IFD.fontDescFindFileName $ IFD.FontDesc familyPtr size bold italic

-- Shader:

#include "shader.h"

foreign import ccall "freetypegl_init"
  c_freetypegl_init :: IO CInt

initialize :: IO ()
initialize = throwIf_ (/= 0) (("freetypegl_init returned" ++) . show) c_freetypegl_init

-- TODO: Use Paths_module
newShader :: IO Shader
newShader = do
  initialize
  textVert <- getDataFileName "shaders/text.vert"
  textFrag <- getDataFileName "shaders/text.frag"
  Shader.load textVert textFrag

-- Atlas:

newtype Atlas = Atlas
  { getAtlas :: ForeignPtr IAtlas.Atlas
  }

data AtlasNewParams = AtlasNewParams
  { atlasNewParamSize :: Vector2 Int
  , atlasNewParamDepth :: Int
  }

defaultAtlasNewParams :: AtlasNewParams
defaultAtlasNewParams = AtlasNewParams
  { atlasNewParamSize = Vector2 512 512
  , atlasNewParamDepth = 3
  }

renderAtlas :: Atlas -> Vector2 Float -> Vector2 Float -> IO ()
renderAtlas (Atlas atlas) pos size = IAtlas.render atlas pos size

newAtlas :: AtlasNewParams -> IO Atlas
newAtlas (AtlasNewParams size depth) = Atlas <$> IAtlas.new size depth

-- Font:

data Font = Font
  { _fAtlas :: Atlas
  , _fShader :: Shader
  , fFont :: ForeignPtr ITF.TextureFont
  }

loadFont :: Atlas -> Shader -> FilePath -> Float -> IO Font
loadFont atlas shader fileName size = do
  textureFont <- ITF.new (getAtlas atlas) fileName size
  return $ Font atlas shader textureFont

textSize :: Font -> String -> IO (Vector2 Float)
textSize = ITF.getTextSize . fFont

newtype TextRenderer = TextRenderer
  { _tbBuffer :: ForeignPtr ITB.TextBuffer
  }

textRenderer :: Vector2 Float -> Markup -> Font -> String -> TextRenderer
textRenderer pos markup (Font atlas shader font) str = unsafePerformIO $
  alloca $ \pen ->
  alloca $ \markupPtr -> do
    poke pen pos
    poke markupPtr markup
    textBuffer <- ITB.new (getAtlas atlas) shader
    ITB.addText textBuffer markupPtr font pen str
    return $ TextRenderer textBuffer

renderText :: TextRenderer -> IO ()
renderText (TextRenderer buf) = ITB.render buf

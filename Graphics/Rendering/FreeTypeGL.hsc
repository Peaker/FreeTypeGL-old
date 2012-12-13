{-# LANGUAGE ForeignFunctionInterface #-}
-- | A higher-level interface wrapping the low-level C API

module Graphics.Rendering.FreeTypeGL
  ( FontDesc(..), fontDescFindFileName
  , Shader, newShader
  , Font, loadFont, textSize
  , Vector2(..)
  , Markup(..), noMarkup, Color4(..)
  , TextRenderer, textRenderer, textRendererSize, renderText
  , initialize
  ) where

import Control.Applicative
import Foreign.C.String (withCString)
import Foreign.C.Types (CInt(..))
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Error (throwIf_)
import Foreign.Storable (peek, poke)
import Graphics.Rendering.FreeTypeGL.Internal.Markup (Markup(..), noMarkup)
import Graphics.Rendering.FreeTypeGL.Internal.Shader (Shader)
import Graphics.Rendering.FreeTypeGL.Internal.TextureFont (IsLCD(..))
import Graphics.Rendering.OpenGL.GL (Color4(..), Vector2(..))
import Paths_FreeTypeGL (getDataFileName)
import System.IO.Unsafe (unsafePerformIO)  -- for pure textWidth
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

-- Font:

data Font = Font
  { _fShader :: Shader
  , fFont :: ForeignPtr ITF.TextureFont
  }

loadFont :: Shader -> FilePath -> Float -> IO Font
loadFont shader fileName size = do
  textureFont <- ITF.new NotLCD fileName size
  return $ Font shader textureFont

data TextRenderer = TextRenderer
  { trBuffer :: ForeignPtr ITB.TextBuffer
  , trSize :: Vector2 Float
  }

textRenderer :: Vector2 Float -> Markup -> Font -> String -> TextRenderer
textRenderer pos markup (Font shader font) str = unsafePerformIO $
  alloca $ \pen ->
  alloca $ \markupPtr -> do
    poke pen pos
    poke markupPtr markup
    textBuffer <- ITB.new shader (Vector2 512 512) 1
    ITB.addText textBuffer markupPtr font pen str
    newPos <- peek pen
    return $ TextRenderer textBuffer ((-) <$> newPos <*> pos)

renderText :: TextRenderer -> IO ()
renderText = ITB.render . trBuffer

textRendererSize :: TextRenderer -> Vector2 Float
textRendererSize = trSize

textSize :: Font -> String -> Vector2 Float
textSize font str =
  unsafePerformIO $ ITF.textSize (fFont font) str

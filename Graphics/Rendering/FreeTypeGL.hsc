{-# LANGUAGE ForeignFunctionInterface #-}
-- | A higher-level interface wrapping the low-level C API

module Graphics.Rendering.FreeTypeGL
  ( FreeTypeGL
  , FontDesc(..), fontDescFindFileName
  , Context, newContext, renderAtlas
  , Font, loadFont, textSize, Vector2(..)
  , Markup(..), noMarkup, Color4(..)
  , TextRenderer, textRenderer, renderText
  , initialize
  ) where

import Control.Applicative ((<$>), (<*>), (<$))
import Foreign.C.String (withCString)
import Foreign.C.Types (CInt(..))
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Marshal.Error (throwIf_)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (poke)
import Graphics.Rendering.FreeTypeGL.Internal.Atlas (Atlas)
import Graphics.Rendering.FreeTypeGL.Internal.Markup (Markup(..))
import Graphics.Rendering.FreeTypeGL.Internal.Shader (Shader)
import Graphics.Rendering.OpenGL.GL (Color4(..), Vector2(..))
import Paths_FreeTypeGL (getDataFileName)
import System.IO.Unsafe (unsafePerformIO)  -- for pure textWidth
import qualified Graphics.Rendering.FreeTypeGL.Internal.Atlas as Atlas
import qualified Graphics.Rendering.FreeTypeGL.Internal.FontDesc as IFD
import qualified Graphics.Rendering.FreeTypeGL.Internal.Shader as Shader
import qualified Graphics.Rendering.FreeTypeGL.Internal.TextBuffer as ITB
import qualified Graphics.Rendering.FreeTypeGL.Internal.TextureFont as ITF

-- Token that represents proof the library was initialized
data FreeTypeGL = FreeTypeGL

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

data Context = Context
  { _ctxAtlas :: ForeignPtr Atlas
  , _ctxShader :: Shader
  }

defaultAtlasSize :: Vector2 Int
defaultAtlasSize = Vector2 2048 4096

defaultAtlasDepth :: Int
defaultAtlasDepth = 3


-- TODO: Use Paths_module
newContext :: FreeTypeGL -> IO Context
newContext FreeTypeGL = do
  textVert <- getDataFileName "shaders/text.vert"
  textFrag <- getDataFileName "shaders/text.frag"
  Context
    <$> Atlas.new defaultAtlasSize defaultAtlasDepth
    <*> Shader.load textVert textFrag

renderAtlas :: Context -> Vector2 Float -> Vector2 Float -> IO ()
renderAtlas (Context atlas _) pos size = Atlas.render atlas pos size

data Font = Font
  { _fContext :: Context
  , _fFont :: ForeignPtr ITF.TextureFont
  }

loadFont :: Context -> FilePath -> Float -> IO Font
loadFont ctx@(Context atlas _) fileName size = do
  textureFont <- ITF.new atlas fileName size
  _ <- ITF.loadGlyphs textureFont $ ['A']
  return $ Font ctx textureFont

textSize :: Font -> String -> IO (Vector2 Float)
textSize (Font _ font) = ITF.getTextSize font

newtype TextRenderer = TextRenderer
  { _tbBuffer :: ForeignPtr ITB.TextBuffer
  }

noMarkup :: Markup
noMarkup = Markup
  { rise = 0.0
  , spacing = 0.0
  , gamma = 1.0
  , foreground_color = Color4 1 1 1 1
  , background_color = Color4 0 0 0 0
  , outline = Nothing
  , underline = Nothing
  , overline = Nothing
  , strikethrough = Nothing
  }

textRenderer :: Vector2 Float -> Markup -> Font -> String -> TextRenderer
textRenderer pos markup (Font (Context atlas shader) font) str = unsafePerformIO $
  alloca $ \pen ->
  alloca $ \markupPtr -> do
    poke pen pos
    poke markupPtr markup
    textBuffer <- ITB.new atlas shader
    ITB.addText textBuffer markupPtr font pen str
    return $ TextRenderer textBuffer

renderText :: TextRenderer -> IO ()
renderText (TextRenderer buf) = ITB.render buf

#include "shader.h"

foreign import ccall "freetypegl_init"
  c_freetypegl_init :: IO CInt

initialize :: IO FreeTypeGL
initialize =
  FreeTypeGL <$
  throwIf_ (/= 0) (("freetypegl_init returned" ++) . show) c_freetypegl_init

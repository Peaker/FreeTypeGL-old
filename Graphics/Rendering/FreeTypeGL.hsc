{-# LANGUAGE ForeignFunctionInterface #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.FreeTypeGL
-- Copyright   :  (C) 2012 Eyal Lotem
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Eyal Lotem <eyal.lotem@gmail.com>
-- Stability   :  experimental
-- Portability :  Haskell2010
--
-- FreeTypeGL lets one load and use texture fonts in an OpenGL
-- context.  You can use it with GLFW-b
-- (<http://hackage.haskell.org/package/GLFW-b>), GLUT
-- (<http://hackage.haskell.org/package/GLUT>), or any other GL context
-- you wish.  The main benefit of this library is that it can be
-- installed with a pure "cabal install", no need to manually install
-- any C bits.
-------------------------------------------------------------------------------

module Graphics.Rendering.FreeTypeGL
  ( FontDesc(..), fontDescFindFileName
  , Shader, newShader
  , Font, loadFont, textSize
  , Markup(..), noMarkup
  , TextRenderer, textRenderer, textRendererSize, renderText
  , Vector2(..), Color4(..)
  ) where

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

-- | A 'FontDesc' describes the desired properties of a system font.
-- It's only purpose is querying the font_config library for the
-- desired font.
--
-- NOTE: This functionality is only available if the
-- package was configured with the use_font_config flag (e.g: cabal
-- install -f use_font_config).
data FontDesc = FontDesc
  { fdFamily :: String
  , fdSize :: Float
  , fdBold :: Bool
  , fdItalic :: Bool
  }

-- | Look up a system font file.
--
-- NOTE: This functionality is only available if the package was
-- configured with the use_font_config flag
-- (e.g: @cabal install -f use_font_config@).
fontDescFindFileName :: FontDesc -> IO FilePath
fontDescFindFileName (FontDesc family size bold italic) =
  withCString family $ \familyPtr ->
  IFD.fontDescFindFileName $ IFD.FontDesc familyPtr size bold italic

-- Shader:

#include "shader.h"

foreign import ccall "freetypegl_init"
  c_freetypegl_init :: IO CInt


initialize :: IO ()
initialize = throwIf_ (/= 0) (("freetypegl_init returned" ++) . show) c_freetypegl_init

-- | Make a 'Shader' needed for 'loadFont'.
newShader :: IO Shader
newShader = do
  initialize
  textVert <- getDataFileName "shaders/text.vert"
  textFrag <- getDataFileName "shaders/text.frag"
  Shader.load textVert textFrag

-- Font:

-- | Represents a loaded font file with a configured face-size.
--
-- For different face sizes, you must load different fonts.
data Font = Font
  { _fShader :: Shader
  , fFont :: ForeignPtr ITF.TextureFont
  }

-- | Load a 'Font' with a given size.
loadFont
  :: Shader   -- ^ A shader created with 'newShader'
  -> FilePath -- ^ The font filename (e.g: \"foo.ttf\")
  -> Float    -- ^ The desired face-size
  -> IO Font  -- ^ The result loaded font
loadFont shader fileName size = do
  textureFont <- ITF.new NotLCD fileName size
  return $ Font shader textureFont

-- | A 'TextRenderer' is an intermediate representation of all font
-- renderings. It represents the GL program to render the font, and
-- may be re-used multiple times.  Re-using a TextRenderer is faster
-- than computing the same renderer multiple times.
data TextRenderer = TextRenderer
  { trBuffer :: ForeignPtr ITB.TextBuffer
  , trSize :: Vector2 Float
  }

-- | Make a 'TextRenderer' for a given font.
textRenderer :: Markup -> Font -> String -> TextRenderer
textRenderer markup (Font shader font) str = unsafePerformIO $
  alloca $ \pen ->
  alloca $ \markupPtr -> do
    poke pen (Vector2 0 0)
    poke markupPtr markup
    textBuffer <- ITB.new shader (Vector2 512 512) 1
    ITB.addText textBuffer markupPtr font pen str
    newPos <- peek pen
    return $ TextRenderer textBuffer newPos

-- | Render a 'TextRenderer' to the GL context
--
-- NOTE: This will have the following side effects:
--
-- * GL's blend will be enabled
--
-- * GL's Texture2D will be enabled
--
-- * GL's color will be changed
--
-- * GL's color_material will be disabled
--
-- * GL's blend_func will be set to (SRC_ALPHA, ONE_MINUS_SRC_ALPHA)
--
-- * GL's blend_color will be modified
renderText :: TextRenderer -> IO ()
renderText = ITB.render . trBuffer

-- | Get the size of a 'TextRenderer'
--
-- NOTE: If you don't need to also render the text, it is better to
-- use the faster 'textSize' function.
textRendererSize :: TextRenderer -> Vector2 Float
textRendererSize = trSize

-- | Compute the text size of a given text in a given font
--
-- NOTE: If you also need to render the text, it is better to make a
-- 'TextRenderer'.
textSize :: Font -> String -> Vector2 Float
textSize font str =
  unsafePerformIO $ ITF.textSize (fFont font) str

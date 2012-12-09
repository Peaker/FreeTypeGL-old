{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Graphics.Rendering.FreeTypeGL.FontManager(FontManager, loadFont) where

import Foreign (Ptr, nullPtr)
import Foreign.C.Types (CSize(..), CInt(..))
import Foreign.Marshal.Alloc (malloc)
import Foreign.Marshal.Error (throwIf_)
import Foreign.Storable (Storable(..))
import Graphics.Rendering.FreeTypeGL.Markup (Markup, Markup_S(..), Color(..))

newtype FontCache = FontCache
  { fontCache :: Map FontDesc TextureFont
  }
type FontManager = Ptr FontManager_S

type Size = (Int, Int)
type Depth = Int

foreign import ccall "font_manager_new"
  c_font_manager_new :: CSize -> CSize -> CSize -> IO FontManager

foreign import ccall "font_manager_load_markup_font"
  c_font_manager_load_markup_font :: FontManager -> Markup -> IO CInt

new :: Size -> Depth -> IO FontManager
new (width, height) depth =
  c_font_manager_new (fi width) (fi height) (fi depth)
  where
    fi = fromIntegral

mkMarkup :: FilePath -> IO Markup
mkMarkup ttfFileName = do
  markup <- malloc
  let
    white = Color 1 1 1 1
    transparent = Color 0 0 0 0
  family' <- newCString ttfFileName
  poke markup $
    Markup_S
    { family = family'
    , size = 72.0
    , bold = 0
    , italic = 0
    , rise = 0
    , spacing = 0
    , gamma = 0.3
    , foreground_color = white
    , background_color = transparent
    , outline = 0
    , outline_color = transparent
    , underline = 1
    , underline_color = white
    , overline = 0
    , overline_color = transparent
    , strikethrough = 0
    , strikethrough_color = transparent
    , font = nullPtr
    }
  return markup

loadFont :: FontManager -> FilePath -> IO Markup
loadFont manager ttfFileName = do
  markup <- mkMarkup ttfFileName
  throwIf_ (/= 0) (("Load font error: "++) . show) $
    c_font_manager_load_markup_font manager markup
  return markup

{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Graphics.Rendering.FreeTypeGL.Internal.FontManager(FontManager, new, loadFont) where

import Foreign (Ptr)
import Foreign.C.Types (CSize(..), CInt(..))
import Foreign.Marshal.Alloc (malloc)
import Foreign.Marshal.Error (throwIf_)
import Foreign.Storable (Storable(..))
import Graphics.Rendering.FreeTypeGL.Internal.Markup (Markup(..))

data FontManager_S
type FontManager = Ptr FontManager_S

type Size = (Int, Int)
type Depth = Int

foreign import ccall "font_manager_new"
  c_font_manager_new :: CSize -> CSize -> CSize -> IO FontManager

foreign import ccall "font_manager_load_markup_font"
  c_font_manager_load_markup_font :: FontManager -> Ptr Markup -> IO CInt

new :: Size -> Depth -> IO FontManager
new (width, height) depth =
  c_font_manager_new (fi width) (fi height) (fi depth)
  where
    fi = fromIntegral

loadFont :: FontManager -> Markup -> IO (Ptr Markup)
loadFont manager val = do
  markup <- malloc
  poke markup val
  throwIf_ (/= 0) (("Load font error: "++) . show) $
    c_font_manager_load_markup_font manager markup
  return markup

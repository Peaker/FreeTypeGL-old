{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.Rendering.FreeTypeGL.Internal.FontDesc (FontDesc(..), fontDescFindFileName) where

import Control.Applicative ((<*))
import Foreign (Ptr)
import Foreign.C.String (CString, peekCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Error (throwIfNull)
import Foreign.Storable (Storable (..))

data FontDesc = FontDesc
  { fdFamily :: CString
  , fdSize :: Float
  , fdBold :: Bool
  , fdItalic :: Bool
  }

#include "font-desc.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

instance Storable FontDesc where
  sizeOf _    = #size font_desc_t
  alignment _ = #alignment font_desc_t
  peek ptr = do
    family <- (#peek font_desc_t, family) ptr
    size <- (#peek font_desc_t, size) ptr
    bold <- (#peek font_desc_t, bold) ptr
    italic <- (#peek font_desc_t, italic) ptr
    return FontDesc
      { fdFamily = family
      , fdSize = size
      , fdBold = bold
      , fdItalic = italic
      }
  poke ptr (FontDesc family size bold italic) = do
    (#poke font_desc_t, family) ptr family
    (#poke font_desc_t, size) ptr size
    (#poke font_desc_t, bold) ptr bold
    (#poke font_desc_t, italic) ptr italic

foreign import ccall "font_desc_find_filename"
  c_font_desc_find_filename :: Ptr FontDesc -> IO CString

foreign import ccall "free"
  c_free :: Ptr a -> IO ()

fontDescFindFileName :: FontDesc -> IO String
fontDescFindFileName fontDesc =
  alloca $ \ptr -> do
    poke ptr fontDesc
    cStr <-
      throwIfNull "Failed to find font filename!" $ c_font_desc_find_filename ptr
    peekCString cStr <* c_free cStr

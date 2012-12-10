{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.Rendering.FreeTypeGL.Internal.FontDesc (FontDesc(..), fontDescFindFileName) where

import Control.Applicative ((<$>))
import Foreign (Ptr)
import Foreign.C.String (CString, peekCString, newCString)
import Foreign.C.Types (CFloat(..), CInt(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (Storable (..))

data FontDesc
  = FontDesc
    { fdFamily :: String
    , fdSize :: Float
    , fdBold :: Bool
    , fdItalic :: Bool
    }

fromCFloat :: CFloat -> Float
fromCFloat = realToFrac

boolFromCInt :: CInt -> Bool
boolFromCInt = toEnum . fromIntegral

toCFloat :: Float -> CFloat
toCFloat = realToFrac

cintFromBool :: Bool -> CInt
cintFromBool = fromIntegral . fromEnum

#include "font-desc.h"

instance Storable FontDesc where
  sizeOf _    = #size font_desc_t
  alignment _ = 4 -- #alignment font_desc_t
  peek ptr = do
    family <- peekCString =<< (#peek font_desc_t, family) ptr
    size <- fromCFloat <$> (#peek font_desc_t, size) ptr
    bold <- boolFromCInt <$> (#peek font_desc_t, bold) ptr
    italic <- boolFromCInt <$> (#peek font_desc_t, italic) ptr
    return FontDesc
      { fdFamily = family
      , fdSize = size
      , fdBold = bold
      , fdItalic = italic
      }
  poke ptr (FontDesc family size bold italic) = do
    (#poke font_desc_t, family) ptr =<< newCString family
    (#poke font_desc_t, size) ptr $ toCFloat size
    (#poke font_desc_t, bold) ptr $ cintFromBool bold
    (#poke font_desc_t, italic) ptr $ cintFromBool italic

foreign import ccall "font_desc_find_filename"
  c_font_desc_find_filename :: Ptr FontDesc -> IO CString

fontDescFindFileName :: FontDesc -> IO String
fontDescFindFileName fontDesc =
  alloca $ \ptr -> do
    poke ptr fontDesc
    peekCString =<< c_font_desc_find_filename ptr

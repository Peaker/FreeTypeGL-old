{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Graphics.Rendering.FreeTypeGL.Internal.TextureFont
  ( TextureFont
  , IsLCD(..), new
  , Vector2(..), textSize
  ) where

import Foreign (Ptr, FunPtr)
import Foreign.C.String (CString, withCString, CWString, withCWString)
import Foreign.C.Types (CFloat(..), CInt(..))
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Error (throwIfNull, throwIf_)
import Foreign.Storable (peek)
import Graphics.Rendering.OpenGL.GL (Vector2(..))

data TextureFont

foreign import ccall "texture_font_new"
  c_texture_font_new :: Bool -> CString -> CFloat -> IO (Ptr TextureFont)

foreign import ccall "texture_font_get_text_size"
  c_texture_font_get_text_size :: Ptr TextureFont -> CWString -> Ptr (Vector2 Float) -> IO CInt

foreign import ccall "&texture_font_delete"
  c_texture_font_delete :: FunPtr (Ptr TextureFont -> IO ())

foreign import ccall "strdup"
  c_strdup :: CString -> IO CString

data IsLCD = IsLCD | NotLCD
  deriving (Read, Show, Eq, Ord)

new :: IsLCD -> FilePath -> Float -> IO (ForeignPtr TextureFont)
new isLcd filename size =
  withCString filename $ \filenamePtr -> do
    newFilenamePtr <- c_strdup filenamePtr
    newForeignPtr c_texture_font_delete =<<
      throwIfNull ("Failed to make texture font for " ++ show filename)
      (c_texture_font_new (toBool isLcd) newFilenamePtr (realToFrac size))
  where
    toBool IsLCD = True
    toBool NotLCD = False

textSize :: ForeignPtr TextureFont -> String -> IO (Vector2 Float)
textSize font str =
  withForeignPtr font $ \fontPtr ->
  withCWString str $ \strPtr ->
  alloca $ \sizePtr -> do
    throwIf_ (/= 0) (("returned: " ++) . show)
      (c_texture_font_get_text_size fontPtr strPtr sizePtr)
    peek sizePtr

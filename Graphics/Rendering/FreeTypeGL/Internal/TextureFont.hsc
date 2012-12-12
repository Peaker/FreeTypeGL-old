{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Graphics.Rendering.FreeTypeGL.Internal.TextureFont
  ( TextureFont, new
  , Vector2(..), getTextSize
  , loadGlyphs
  ) where

import Foreign (Ptr, nullPtr, FunPtr)
import Foreign.C.String (CWString, withCWString, withCWStringLen, CString, withCString)
import Foreign.C.Types (CFloat(..), CSize(..))
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Marshal.Error (throwIfNull, throwIf_)
import Graphics.Rendering.FreeTypeGL.Internal.Atlas (Atlas)
import Graphics.Rendering.OpenGL.GL (Vector2(..))

data TextureFont

foreign import ccall "texture_font_new"
  c_texture_font_new :: Ptr Atlas -> CString -> CFloat -> IO (Ptr TextureFont)

foreign import ccall "&texture_font_delete"
  c_texture_font_delete :: FunPtr (Ptr TextureFont -> IO ())

foreign import ccall "texture_font_get_text_size"
  c_texture_font_get_text_size :: Ptr TextureFont -> CWString -> CSize -> Ptr Float -> IO ()

foreign import ccall "texture_font_load_glyphs"
  c_texture_font_load_glyphs :: Ptr TextureFont -> CWString -> IO (Ptr ())

foreign import ccall "strdup"
  c_strdup :: CString -> IO CString

new :: ForeignPtr Atlas -> FilePath -> Float -> IO (ForeignPtr TextureFont)
new atlas filename size =
  withForeignPtr atlas $ \atlasPtr ->
  withCString filename $ \filenamePtr -> do
    newFilenamePtr <- c_strdup filenamePtr
    newForeignPtr c_texture_font_delete =<<
      throwIfNull ("Failed to make texture font for " ++ show filename)
      (c_texture_font_new atlasPtr newFilenamePtr (realToFrac size))

getTextSize :: ForeignPtr TextureFont -> String -> IO (Vector2 Float)
getTextSize textureFont str =
  withCWStringLen str $ \(strPtr, len) ->
  withForeignPtr textureFont $ \fontPtr ->
  allocaArray 2 $ \sizePtr -> do
    c_texture_font_get_text_size fontPtr strPtr (fromIntegral len) sizePtr
    [width, height] <- peekArray 2 sizePtr
    return $ Vector2 width height

-- | Returns how many glyphs failed to load
loadGlyphs :: ForeignPtr TextureFont -> String -> IO ()
loadGlyphs textureFont str =
  withCWString str $ \strPtr ->
  withForeignPtr textureFont $ \fontPtr ->
  throwIf_ (== nullPtr) (const ("Failed to load glyphs: " ++ show str)) $
  c_texture_font_load_glyphs fontPtr strPtr

{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Graphics.Rendering.FreeTypeGL.Internal.TextBuffer
  ( TextBuffer, new, render, addText
  , Pen, Vector2(..)
  ) where

import Foreign (FunPtr, Ptr)
import Foreign.C.String (CWString, withCWStringLen)
import Foreign.C.Types (CSize(..), CUInt(..), CInt(..))
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, newForeignPtr)
import Foreign.Marshal.Error (throwIf_)
import Graphics.Rendering.FreeTypeGL.Internal.Atlas (Atlas)
import Graphics.Rendering.FreeTypeGL.Internal.Markup (Markup)
import Graphics.Rendering.FreeTypeGL.Internal.Shader (Shader(..))
import Graphics.Rendering.FreeTypeGL.Internal.TextureFont (TextureFont)
import Graphics.Rendering.OpenGL.GL (Vector2(..))

#include "text-buffer.h"

data TextBuffer

foreign import ccall "text_buffer_new"
  c_text_buffer_new :: Ptr Atlas -> Shader -> IO (Ptr TextBuffer)

foreign import ccall "text_buffer_render"
  c_text_buffer_render :: Ptr TextBuffer -> IO ()

type Pen = Vector2 Float

foreign import ccall "text_buffer_add_text"
  c_text_buffer_add_text :: Ptr TextBuffer -> Ptr Pen -> Ptr Markup -> Ptr TextureFont -> CWString -> CSize -> IO CInt

foreign import ccall "&text_buffer_delete"
  c_text_buffer_delete :: FunPtr (Ptr TextBuffer -> IO ())

new :: ForeignPtr Atlas -> Shader -> IO (ForeignPtr TextBuffer)
new atlas shader =
  withForeignPtr atlas $ \atlasPtr -> do
    ptr <- c_text_buffer_new atlasPtr shader
    newForeignPtr c_text_buffer_delete ptr

render :: ForeignPtr TextBuffer -> IO ()
render = flip withForeignPtr c_text_buffer_render

addText :: ForeignPtr TextBuffer -> Ptr Markup -> ForeignPtr TextureFont -> Ptr Pen -> String -> IO ()
addText textBuffer markup font pen str =
  throwIf_ (/= 0)
  ((++ "Most likely cause: Out of atlas memory. Try to enlarge the atlas.") .
   ("text_buffer_add_text returned: " ++) . show) .
  withCWStringLen str $ \(strPtr, len) ->
  withForeignPtr textBuffer $ \textBufferPtr ->
  withForeignPtr font $ \fontPtr ->
  c_text_buffer_add_text textBufferPtr pen markup fontPtr strPtr $
  fromIntegral len

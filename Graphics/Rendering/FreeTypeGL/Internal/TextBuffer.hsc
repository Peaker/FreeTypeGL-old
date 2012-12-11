{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Graphics.Rendering.FreeTypeGL.Internal.TextBuffer
  ( TextBuffer, new, render, addText
  , Pen, newPen, getPen, Vector2(..)
  ) where

import Foreign (FunPtr, Ptr)
import Foreign.C.String (CWString, withCWStringLen)
import Foreign.C.Types (CSize(..), CUInt(..))
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrArray, withForeignPtr, newForeignPtr)
import Foreign.Marshal.Array (peekArray, pokeArray)
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

foreign import ccall "text_buffer_add_text"
  c_text_buffer_add_text :: Ptr TextBuffer -> Ptr Float -> Ptr Markup -> Ptr TextureFont -> CWString -> CSize -> IO ()

foreign import ccall "&text_buffer_delete"
  c_text_buffer_delete :: FunPtr (Ptr TextBuffer -> IO ())

newtype Pen = Pen (ForeignPtr Float)

newPen :: Vector2 Float -> IO Pen
newPen (Vector2 x y) = do
  pen <- mallocForeignPtrArray 2
  withForeignPtr pen $ \penPtr ->
    pokeArray penPtr [x, y]
  return $ Pen pen

getPen :: Pen -> IO (Vector2 Float)
getPen (Pen pen) =
  withForeignPtr pen $ \penPtr -> do
    [x, y] <- peekArray 2 penPtr
    return $ Vector2 x y

new :: ForeignPtr Atlas -> Shader -> IO (ForeignPtr TextBuffer)
new atlas shader =
  withForeignPtr atlas $ \atlasPtr -> do
    ptr <- c_text_buffer_new atlasPtr shader
    newForeignPtr c_text_buffer_delete ptr

render :: ForeignPtr TextBuffer -> IO ()
render = flip withForeignPtr c_text_buffer_render

addText :: ForeignPtr TextBuffer -> Ptr Markup -> ForeignPtr TextureFont -> Pen -> String -> IO ()
addText textBuffer markup font (Pen pen) str = do
  withCWStringLen str $ \(strPtr, len) ->
    withForeignPtr pen $ \penPtr ->
    withForeignPtr textBuffer $ \textBufferPtr ->
    withForeignPtr font $ \fontPtr ->
    c_text_buffer_add_text textBufferPtr penPtr markup fontPtr strPtr $
    fromIntegral len

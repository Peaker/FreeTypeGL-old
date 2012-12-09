{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Graphics.Rendering.FreeTypeGL.Internal.TextBuffer
  ( TextBuffer, new, render, add
  , Pen, newPen, getPen
  ) where

import Foreign (Ptr)
import Foreign.C.Types (CSize(..))
import Foreign.C.String (CWString, withCWStringLen, CString, withCString)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrArray, withForeignPtr, newForeignPtr)
import Foreign.Marshal.Alloc (finalizerFree)
import Foreign.Marshal.Array (peekArray, pokeArray)
import Foreign.Marshal.Error (throwIfNull)
import Graphics.Rendering.FreeTypeGL.Internal.FontManager (FontManager)
import Graphics.Rendering.FreeTypeGL.Internal.Markup (Markup)

#include "text-buffer.h"

data TextBuffer_S
type TextBuffer = ForeignPtr TextBuffer_S

foreign import ccall "text_buffer_new_with"
  c_text_buffer_new_with :: FontManager -> CString -> CString -> IO (Ptr TextBuffer_S)

foreign import ccall "text_buffer_render"
  c_text_buffer_render :: Ptr TextBuffer_S -> IO ()

foreign import ccall "text_buffer_add_text"
  c_text_buffer_add_text :: Ptr TextBuffer_S -> Ptr Float -> Ptr Markup -> CWString -> CSize -> IO ()

type Pen = ForeignPtr Float

newPen :: Float -> Float -> IO Pen
newPen x y = do
  pen <- mallocForeignPtrArray 2
  withForeignPtr pen $ \penPtr ->
    pokeArray penPtr [x, y]
  return pen

getPen :: Pen -> IO (Float, Float)
getPen pen =
  withForeignPtr pen $ \penPtr -> do
    [x, y] <- peekArray 2 penPtr
    return (x, y)

new :: FontManager -> FilePath -> FilePath -> IO TextBuffer
new fontManager vertFilename fragFilename = do
  ptr <-
    throwIfNull "Failed to load shader files" .
    withCString vertFilename $ \vertFilenamePtr ->
    withCString fragFilename $ \fragFilenamePtr ->
    c_text_buffer_new_with fontManager vertFilenamePtr fragFilenamePtr
  -- There's no destructor for TextBuffer, so we
  newForeignPtr finalizerFree ptr

render :: TextBuffer -> IO ()
render = flip withForeignPtr c_text_buffer_render

add :: TextBuffer -> Ptr Markup -> Pen -> String -> IO ()
add textBuffer markup pen str = do
  withCWStringLen str $ \(strPtr, len) ->
    withForeignPtr pen $ \penPtr ->
    withForeignPtr textBuffer $ \textBufferPtr ->
    c_text_buffer_add_text textBufferPtr penPtr markup strPtr $
    fromIntegral len

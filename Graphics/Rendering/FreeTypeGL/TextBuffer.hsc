{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Graphics.Rendering.FreeTypeGL.TextBuffer
  ( TextBuffer, new, render, add
  , Pen, newPen
  ) where

import Foreign (Ptr)
import Foreign.C.Types (CSize(..))
import Foreign.C.String (CWString, withCWStringLen, CString, withCString)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrArray, withForeignPtr)
import Foreign.Marshal.Array (pokeArray)
import Foreign.Marshal.Error (throwIfNull)
import Graphics.Rendering.FreeTypeGL.Atlas (Atlas)
import Graphics.Rendering.FreeTypeGL.Markup (Markup)

#include "text-buffer.h"

data TextBuffer_S
type TextBuffer = Ptr TextBuffer_S

foreign import ccall "text_buffer_new"
  c_text_buffer_new :: Atlas -> CString -> CString -> IO TextBuffer

foreign import ccall "text_buffer_render"
  c_text_buffer_render :: TextBuffer -> IO ()

foreign import ccall "text_buffer_add_text"
  c_text_buffer_add_text :: TextBuffer -> Ptr Float -> Markup -> CWString -> CSize -> IO ()

type Pen = ForeignPtr Float

newPen :: IO Pen
newPen = do
  pen <- mallocForeignPtrArray 2
  withForeignPtr pen $ \penPtr ->
    pokeArray penPtr [0, 0]
  return pen

new :: Atlas -> FilePath -> FilePath -> IO TextBuffer
new atlas vertFilename fragFilename =
  throwIfNull "Failed to load shader files" .
  withCString vertFilename $ \vertFilenamePtr ->
  withCString fragFilename $ \fragFilenamePtr ->
  c_text_buffer_new atlas vertFilenamePtr fragFilenamePtr

render :: TextBuffer -> IO ()
render = c_text_buffer_render

add :: TextBuffer -> Markup -> Pen -> String -> IO ()
add textBuffer markup pen str = do
  withCWStringLen str $ \(strPtr, len) ->
    withForeignPtr pen $ \penPtr ->
      c_text_buffer_add_text textBuffer penPtr markup strPtr $ fromIntegral len

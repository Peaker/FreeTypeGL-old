{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Graphics.Rendering.FreeTypeGL
  ( textBufferNew
  , textBufferRender
  , penNew
  , mkColor, Color
  , textBufferAddWChar
  ) where

import Foreign (Ptr, pokeArray, nullPtr, mallocArray)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrArray, withForeignPtr)
import Foreign.C.Types (CSize(..), CWchar(..))
import Foreign.C.String (newCString)
import Foreign.Marshal.Alloc (malloc)
import Foreign.Storable (Storable(..))
import Graphics.Rendering.FreeTypeGL.Markup (Markup, Markup_S(..))

data TextBuffer_S
type TextBuffer = Ptr TextBuffer_S

type Pen = ForeignPtr Float

type Color = Ptr Float

penNew :: IO Pen
penNew = do
  pen <- mallocForeignPtrArray 2
  withForeignPtr pen $ \penPtr ->
    pokeArray penPtr [0, 0]
  return pen

foreign import ccall "text_buffer_new"
  c_text_buffer_new :: CSize -> IO TextBuffer

foreign import ccall "text_buffer_render"
  c_text_buffer_render :: TextBuffer -> IO ()

-- foreign import ccall "text_buffer_add_text"
--   c_text_buffer_add_text :: TextBuffer -> Pen -> Markup -> Ptr CWchar -> IO ()

foreign import ccall "text_buffer_add_wchar"
  c_text_buffer_add_wchar :: TextBuffer -> Ptr Float -> Markup -> CWchar -> CWchar -> IO ()

textBufferNew :: IO TextBuffer
textBufferNew = c_text_buffer_new 4 -- depth=4, RGBA

textBufferRender :: TextBuffer -> IO ()
textBufferRender = c_text_buffer_render

fromChar :: Char -> CWchar
fromChar = toEnum . fromEnum

mkColor :: Float -> Float -> Float -> Float -> IO Color
mkColor r g b a = do
  color <- mallocArray 4
  pokeArray color [r, g, b, a]
  return color

mkMarkup :: String -> IO Markup
mkMarkup ttf = do
  markup <- malloc
  white <- mkColor 1 1 1 1
  transparent <- mkColor 0 0 0 0
  family' <- newCString ttf
  poke markup $
    Markup_S
    { family = family'
    , size = 72.0
    , bold = 0
    , italic = 0
    , rise = 0
    , spacing = 0
    , gamma = 1.0
    , foreground_color = white
    , background_color = transparent
    , outline = 0
    , outline_color = transparent
    , underline = 0
    , underline_color = transparent
    , overline = 0
    , overline_color = transparent
    , strikethrough = 0
    , strikethrough_color = transparent
    , font = nullPtr
    }
  return markup

textBufferAddWChar :: TextBuffer -> Pen -> Char -> Char -> IO ()
textBufferAddWChar textBuffer pen prev new = do
  markup <- mkMarkup "ttf"
  withForeignPtr pen $ \penPtr ->
    c_text_buffer_add_wchar textBuffer penPtr markup (fromChar prev) (fromChar new)

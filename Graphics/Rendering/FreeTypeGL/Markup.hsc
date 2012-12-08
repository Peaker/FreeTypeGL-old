{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Graphics.Rendering.FreeTypeGL.Markup(Markup, Markup_S(..), Color(..)) where

import Control.Applicative
import Foreign (Ptr, plusPtr)
import Foreign.Storable (Storable(..))
import Foreign.C.String (CString)
import Foreign.C.Types (CFloat, CInt)

type Markup = Ptr Markup_S

data TextureFont_S
type TextureFont = Ptr TextureFont_S

data Color = Color Float Float Float Float

peekColor :: Ptr Float -> IO Color
peekColor ptr = Color <$> p 0 <*> p 1 <*> p 2 <*> p 3
  where
    p = peekElemOff ptr

pokeColor :: Ptr Float -> Color -> IO ()
pokeColor ptr (Color r g b a) = mapM_ p $ zip [0..] [r, g, b, a]
  where
    p (off, val) = pokeElemOff ptr off val

data Markup_S = Markup_S
  { family :: CString
  , size :: CFloat
  , bold :: CInt
  , italic :: CInt
  , rise :: CFloat
  , spacing :: CFloat
  , gamma :: CFloat
  , foreground_color :: Color -- [4]
  , background_color :: Color -- [4]
  , outline :: CInt
  , outline_color :: Color -- [4]
  , underline :: CInt
  , underline_color :: Color -- [4]
  , overline :: CInt
  , overline_color :: Color -- [4]
  , strikethrough :: CInt
  , strikethrough_color :: Color -- [4]
  , font :: TextureFont
  }

#include "markup.h"

instance Storable Markup_S where
  sizeOf _    = #size markup_t
  alignment _ = 4 -- #alignment markup_t
  peek ptr = do
    family' <- (#peek markup_t, family) ptr
    size' <- (#peek markup_t, size) ptr
    bold' <- (#peek markup_t, bold) ptr
    italic' <- (#peek markup_t, italic) ptr
    rise' <- (#peek markup_t, rise) ptr
    spacing' <- (#peek markup_t, spacing) ptr
    gamma' <- (#peek markup_t, gamma) ptr
    foreground_color' <- peekColor $ (#ptr markup_t, foreground_color) ptr
    background_color' <- peekColor $ (#ptr markup_t, background_color) ptr
    outline' <- (#peek markup_t, outline) ptr
    outline_color' <- peekColor $ (#ptr markup_t, outline_color) ptr
    underline' <- (#peek markup_t, underline) ptr
    underline_color' <- peekColor $ (#ptr markup_t, underline_color) ptr
    overline' <- (#peek markup_t, overline) ptr
    overline_color' <- peekColor $ (#ptr markup_t, overline_color) ptr
    strikethrough' <- (#peek markup_t, strikethrough) ptr
    strikethrough_color' <- peekColor $ (#ptr markup_t, strikethrough_color) ptr
    font' <- (#peek markup_t, font) ptr
    return $ Markup_S
      family'
      size'
      bold'
      italic'
      rise'
      spacing'
      gamma'
      foreground_color'
      background_color'
      outline'
      outline_color'
      underline'
      underline_color'
      overline'
      overline_color'
      strikethrough'
      strikethrough_color'
      font'
  poke ptr
    (Markup_S
      family'
      size'
      bold'
      italic'
      rise'
      spacing'
      gamma'
      foreground_color'
      background_color'
      outline'
      outline_color'
      underline'
      underline_color'
      overline'
      overline_color'
      strikethrough'
      strikethrough_color'
      font'
    ) = do
      (#poke markup_t, family) ptr family'
      (#poke markup_t, size) ptr size'
      (#poke markup_t, bold) ptr bold'
      (#poke markup_t, italic) ptr italic'
      (#poke markup_t, rise) ptr rise'
      (#poke markup_t, spacing) ptr spacing'
      (#poke markup_t, gamma) ptr gamma'
      pokeColor ((#ptr markup_t, foreground_color) ptr) foreground_color'
      pokeColor ((#ptr markup_t, background_color) ptr) background_color'
      (#poke markup_t, outline) ptr outline'
      pokeColor ((#ptr markup_t, outline_color) ptr) outline_color'
      (#poke markup_t, underline) ptr underline'
      pokeColor ((#ptr markup_t, underline_color) ptr) underline_color'
      (#poke markup_t, overline) ptr overline'
      pokeColor ((#ptr markup_t, overline_color) ptr) overline_color'
      (#poke markup_t, strikethrough) ptr strikethrough'
      pokeColor ((#ptr markup_t, strikethrough_color) ptr) strikethrough_color'
      (#poke markup_t, font) ptr font'

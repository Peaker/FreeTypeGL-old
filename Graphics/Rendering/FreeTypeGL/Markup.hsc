{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Graphics.Rendering.FreeTypeGL.Markup(Markup, Markup_S(..)) where

import Foreign (Ptr)
import Foreign.Storable (Storable(..))
import Foreign.C.String (CString)
import Foreign.C.Types (CFloat, CInt)

type Markup = Ptr Markup_S

data TextureFont_S
type TextureFont = Ptr TextureFont_S

data Markup_S = Markup_S
  { family :: CString
  , size :: CFloat
  , bold :: CInt
  , italic :: CInt
  , rise :: CFloat
  , spacing :: CFloat
  , gamma :: CFloat
  , foreground_color :: Ptr Float -- [4]
  , background_color :: Ptr Float -- [4]
  , outline :: CInt
  , outline_color :: Ptr Float -- [4]
  , underline :: CInt
  , underline_color :: Ptr Float -- [4]
  , overline :: CInt
  , overline_color :: Ptr Float -- [4]
  , strikethrough :: CInt
  , strikethrough_color :: Ptr Float -- [4]
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
    foreground_color' <- (#peek markup_t, foreground_color) ptr
    background_color' <- (#peek markup_t, background_color) ptr
    outline' <- (#peek markup_t, outline) ptr
    outline_color' <- (#peek markup_t, outline_color) ptr
    underline' <- (#peek markup_t, underline) ptr
    underline_color' <- (#peek markup_t, underline_color) ptr
    overline' <- (#peek markup_t, overline) ptr
    overline_color' <- (#peek markup_t, overline_color) ptr
    strikethrough' <- (#peek markup_t, strikethrough) ptr
    strikethrough_color' <- (#peek markup_t, strikethrough_color) ptr
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
      (#poke markup_t, foreground_color) ptr foreground_color'
      (#poke markup_t, background_color) ptr background_color'
      (#poke markup_t, outline) ptr outline'
      (#poke markup_t, outline_color) ptr outline_color'
      (#poke markup_t, underline) ptr underline'
      (#poke markup_t, underline_color) ptr underline_color'
      (#poke markup_t, overline) ptr overline'
      (#poke markup_t, overline_color) ptr overline_color'
      (#poke markup_t, strikethrough) ptr strikethrough'
      (#poke markup_t, strikethrough_color) ptr strikethrough_color'
      (#poke markup_t, font) ptr font'

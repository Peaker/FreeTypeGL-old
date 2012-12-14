{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Graphics.Rendering.FreeTypeGL.Internal.Markup(Markup(..), noMarkup) where

import Control.Applicative ((<$>), (<*>))
import Foreign (Ptr, plusPtr)
import Foreign.C.Types (CInt)
import Foreign.Storable (Storable(..))
import Graphics.Rendering.OpenGL.GL (Color4(..))

peekColor :: Ptr Float -> IO (Color4 Float)
peekColor ptr = Color4 <$> p 0 <*> p 1 <*> p 2 <*> p 3
  where
    p = peekElemOff ptr

pokeColor :: Ptr Float -> Color4 Float -> IO ()
pokeColor ptr (Color4 r g b a) = mapM_ p $ zip [0..] [r, g, b, a]
  where
    p (off, val) = pokeElemOff ptr off val


data Markup = Markup
  { gamma :: Float
  , foreground_color :: Color4 Float
  , background_color :: Color4 Float
  , underline :: Maybe (Color4 Float)
  , overline :: Maybe (Color4 Float)
  , strikethrough :: Maybe (Color4 Float)
  }

noMarkup :: Markup
noMarkup = Markup
  { gamma = 1.0
  , foreground_color = Color4 1 1 1 1
  , background_color = Color4 0 0 0 0
  , underline = Nothing
  , overline = Nothing
  , strikethrough = Nothing
  }

#include "markup.h"

applyTuple :: (a -> b, a -> c) -> a -> (b, c)
applyTuple (fx, fy) arg = (fx arg, fy arg)

underlinePtrs, overlinePtrs, strikethroughPtrs :: Ptr Markup -> (Ptr CInt, Ptr Float)
underlinePtrs     = applyTuple ((#ptr markup_t, underline    ), (#ptr markup_t, underline_color))
overlinePtrs      = applyTuple ((#ptr markup_t, overline     ), (#ptr markup_t, overline_color))
strikethroughPtrs = applyTuple ((#ptr markup_t, strikethrough), (#ptr markup_t, strikethrough_color))

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

instance Storable Markup where
  sizeOf _    = #size markup_t
  alignment _ = #alignment markup_t
  peek ptr = do
    gamma' <- (#peek markup_t, gamma) ptr
    foreground_color' <- peekColor $ (#ptr markup_t, foreground_color) ptr
    background_color' <- peekColor $ (#ptr markup_t, background_color) ptr
    let
      peekAnnotation ptrs = do
        let (boolPtr, colorPtr) = ptrs ptr
        b <- peek boolPtr
        case b of
          0 -> return Nothing
          _ -> Just <$> peekColor colorPtr
    underline' <- peekAnnotation underlinePtrs
    overline' <- peekAnnotation overlinePtrs
    strikethrough' <- peekAnnotation strikethroughPtrs
    return $
      Markup gamma'
      foreground_color' background_color'
      underline' overline' strikethrough'
  poke ptr
    (Markup gamma'
      foreground_color' background_color'
      underline' overline' strikethrough') = do
    (#poke markup_t, gamma) ptr gamma'
    pokeColor ((#ptr markup_t, foreground_color) ptr) foreground_color'
    pokeColor ((#ptr markup_t, background_color) ptr) background_color'
    let
      pokeAnnotation ptrs annotation =
        case annotation of
          Nothing -> poke boolPtr 0
          Just color -> do
            poke boolPtr 1
            pokeColor colorPtr color
        where
          (boolPtr, colorPtr) = ptrs ptr
    pokeAnnotation underlinePtrs underline'
    pokeAnnotation overlinePtrs overline'
    pokeAnnotation strikethroughPtrs strikethrough'

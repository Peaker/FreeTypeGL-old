{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Graphics.Rendering.FreeTypeGL.Atlas(Atlas, new, Vector2(..)) where

import Data.Tensor (Vector2(..))
import Foreign (Ptr)
import Foreign.C.Types (CSize(..))

data Atlas_S
type Atlas = Ptr Atlas_S

foreign import ccall "font_manager_new"
  c_texture_atlas_new :: CSize -> CSize -> CSize -> IO Atlas

new :: Vector2 Int -> Int -> IO Atlas
new (Vector2 width height) depth =
  c_texture_atlas_new (fi width) (fi height) (fi depth)
  where
    fi = fromIntegral

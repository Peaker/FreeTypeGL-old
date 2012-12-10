{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Graphics.Rendering.FreeTypeGL.Internal.Atlas(Atlas, new, Vector2(..)) where

import Data.Tensor (Vector2(..))
import Foreign (Ptr, FunPtr)
import Foreign.C.Types (CSize(..))
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr)

data Atlas

foreign import ccall "texture_atlas_new"
  c_texture_atlas_new :: CSize -> CSize -> CSize -> IO (Ptr Atlas)

foreign import ccall "&texture_atlas_delete"
  c_texture_font_delete :: FunPtr (Ptr Atlas -> IO ())

new :: Vector2 Int -> Int -> IO (ForeignPtr Atlas)
new (Vector2 width height) depth =
  newForeignPtr c_texture_font_delete =<<
  c_texture_atlas_new (fi width) (fi height) (fi depth)
  where
    fi = fromIntegral

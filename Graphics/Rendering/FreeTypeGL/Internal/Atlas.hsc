{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Graphics.Rendering.FreeTypeGL.Internal.Atlas
  ( Atlas, new
  , Vector2(..)
  , render
  ) where

import Foreign (Ptr, FunPtr)
import Foreign.C.Types (CSize(..))
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Graphics.Rendering.OpenGL.GL (Vector2(..))

data Atlas

foreign import ccall "texture_atlas_new"
  c_texture_atlas_new :: CSize -> CSize -> CSize -> IO (Ptr Atlas)

foreign import ccall "&texture_atlas_delete"
  c_texture_font_delete :: FunPtr (Ptr Atlas -> IO ())

foreign import ccall "texture_atlas_render"
  c_texture_atlas_render :: Ptr Atlas -> Float -> Float -> Float -> Float -> IO ()

-- Allow rendering the atlas on screen
render :: ForeignPtr Atlas -> Vector2 Float -> Vector2 Float -> IO ()
render atlas (Vector2 x y) (Vector2 width height) =
  withForeignPtr atlas $ \atlasPtr -> c_texture_atlas_render atlasPtr x y width height

new :: Vector2 Int -> Int -> IO (ForeignPtr Atlas)
new (Vector2 width height) depth =
  newForeignPtr c_texture_font_delete =<<
  c_texture_atlas_new (fi width) (fi height) (fi depth)
  where
    fi = fromIntegral

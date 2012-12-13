{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Graphics.Rendering.FreeTypeGL.Internal.Atlas
  ( Atlas, new
  , Vector2(..)
  , render
  ) where

import Control.Applicative ((<$>))
import Foreign (Ptr, FunPtr)
import Foreign.C.Types (CSize(..), CInt(..))
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (Storable(..))
import Graphics.Rendering.OpenGL.GL (Vector2(..))

data Atlas

foreign import ccall "texture_atlas_new"
  c_texture_atlas_new :: Ptr (Vector2 CInt) -> CSize -> IO (Ptr Atlas)

foreign import ccall "&texture_atlas_delete"
  c_texture_atlas_delete :: FunPtr (Ptr Atlas -> IO ())

foreign import ccall "texture_atlas_render"
  c_texture_atlas_render :: Ptr Atlas -> Float -> Float -> Float -> Float -> IO ()

-- Allow rendering the atlas on screen
render :: ForeignPtr Atlas -> Vector2 Float -> Vector2 Float -> IO ()
render atlas (Vector2 x y) (Vector2 width height) =
  withForeignPtr atlas $ \atlasPtr -> c_texture_atlas_render atlasPtr x y width height

new :: Vector2 Int -> Int -> IO (ForeignPtr Atlas)
new vec depth =
  alloca $ \sizePtr -> do
    poke sizePtr $ fromIntegral <$> vec
    newForeignPtr c_texture_atlas_delete =<<
      c_texture_atlas_new sizePtr (fromIntegral depth)

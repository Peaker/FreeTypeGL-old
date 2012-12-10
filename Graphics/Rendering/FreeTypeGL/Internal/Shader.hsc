{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.Rendering.FreeTypeGL.Internal.Shader(Shader(..), load) where

import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CUInt(..))

newtype Shader = Shader CUInt

foreign import ccall "shader_load"
  c_shader_load :: CString -> CString -> IO CUInt

load :: FilePath -> FilePath -> IO Shader
load vertFilename fragFilename = do
  shader <-
    withCString vertFilename $ \vertPtr ->
    withCString fragFilename $ \fragPtr ->
    c_shader_load vertPtr fragPtr
  case shader of
    -1 -> fail $ "Failed to load shader: " ++ show (vertFilename, fragFilename)
    _ -> return $ Shader shader

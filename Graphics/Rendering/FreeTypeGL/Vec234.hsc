module Graphics.Rendering.FreeTypeGL.Vec234
  ( Vec2
  , Vec2_S(..)
  , Vec4
  , Vec4_S(..)
  ) where

import Foreign (Ptr)
import Foreign.C.Types (CFloat)
import Foreign.Storable (Storable(..))

#include "vec234.h"

type Vec2 = Ptr Vec2_S

data Vec2_S = Vec2_S
  { v2x :: CFloat
  , v2y :: CFloat
  }

instance Storable Vec2_S where
  sizeOf _    = #size vec2
  alignment _ = 4 -- #alignment vec2
  peek ptr = do
    x' <- (#peek vec2, x) ptr
    y' <- (#peek vec2, y) ptr
    return $ Vec2_S { v2x = x', v2y = y' }
  poke ptr val = do
    (#poke vec2, x) ptr (v2x val)
    (#poke vec2, y) ptr (v2y val)


type Vec4 = Ptr Vec4_S

data Vec4_S = Vec4_S
  { v4x :: CFloat
  , v4y :: CFloat
  , v4z :: CFloat
  , v4w :: CFloat
  }

instance Storable Vec4_S where
  sizeOf _    = #size vec4
  alignment _ = 4 -- #alignment vec4
  peek ptr = do
    x' <- (#peek vec4, x) ptr
    y' <- (#peek vec4, y) ptr
    z' <- (#peek vec4, z) ptr
    w' <- (#peek vec4, w) ptr
    return $ Vec4_S { v4x = x', v4y = y', v4z = z', v4w = w' }
  poke ptr val = do
    (#poke vec4, x) ptr (v4x val)
    (#poke vec4, y) ptr (v4y val)
    (#poke vec4, z) ptr (v4z val)
    (#poke vec4, w) ptr (v4w val)

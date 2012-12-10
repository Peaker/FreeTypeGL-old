-- {-# LANGUAGE ForeignFunctionInterface #-}
-- -- | A higher-level interface wrapping the low-level C API

-- module Graphics.Rendering.FreeTypeGL
--   ( FontManager
--   , Font
--   , Markup(..)
--   , Vector2 (..)
--   , TextBuffer, textBufferSize, textBufferRender
--   ) where

-- import Data.Tensor (Vector2(..))
-- import Foreign (Ptr)
-- import Graphics.Rendering.FreeTypeGL.Internal.FontManager (FontManager)
-- import Graphics.Rendering.FreeTypeGL.Internal.Markup (Markup(..))
-- import qualified Graphics.Rendering.FreeTypeGL.Internal.FontManager as IFM
-- import qualified Graphics.Rendering.FreeTypeGL.Internal.TextBuffer as ITB

-- -- | The FontManager serves as a cache to get fonts from with caching
-- -- data FontManager = FontManager { In

-- -- We use the Markup record as POD description of the font we want,
-- -- and a (Ptr Markup) as an actualized font to use.

-- newtype Font = Font (Ptr Markup)

-- -- These defaults come from the C lib. Not sure about the depth=3...
-- newFontManager :: IO FontManager
-- newFontManager = IFM.new (512, 512) 3

-- -- loadFont :: FontManager -> Markup -> Font
-- -- loadFont = (fmap . fmap . fmap) Font IFM.loadFont

-- type Size = Vector2 Float
-- type Pos = Vector2 Float

-- data TextBuffer = TextBuffer
--   { _tbSize :: Size
--   , _tbBuf :: ITB.TextBuffer
--   }

-- textBufferSize :: TextBuffer -> Size
-- textBufferSize = _tbSize

-- textBufferRender :: TextBuffer -> IO ()
-- textBufferRender = ITB.render . _tbBuf

-- -- makeTextBuffer :: Font -> String -> IO TextBuffer
-- -- makeTextBuffer (Font markupPtr) str = do
-- --   textBuffer <- ITB.new
-- --   ITB.newPen
-- --   ITB.add textBuffer markupPtr undefined

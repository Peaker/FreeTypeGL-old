module Graphics.Rendering.FreeTypeGL.FontDesc (FontDesc(..)) where

data FontDesc
  = FontDesc
    { fdFamily :: String
    , fdSize :: Float
    , fdBold :: Bool
    , fdItalic :: Bool
    }

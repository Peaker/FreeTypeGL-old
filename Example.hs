import qualified Graphics.Rendering.FreeTypeGL as FGL

main = do
  pen <- FGL.penNew
  textBuffer <- FGL.textBufferNew
  FGL.textBufferAddWChar textBuffer pen 'a' 'b'

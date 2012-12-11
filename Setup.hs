import System.Directory (copyFile)
import Distribution.Simple (defaultMain)
import Distribution.System (buildOS, OS(..))
main = do
  case buildOS of
    Windows -> do
      mapM_ (uncurry copyFile) []
    _ -> return ()
  defaultMain

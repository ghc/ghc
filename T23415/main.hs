import Control.Monad
import System.FilePath
import System.Directory
import GHCi.ObjLink

hsLoadObjs = do
  cwd <- getCurrentDirectory
  forM_ [0..499] $ \i ->
    loadDLL (cwd </> "lib" ++ show i ++ ".out")

hsLoadSymbols = do
  forM_ [0..99] $ \j ->
    forM_ [0..499] $ \i ->
      lookupSymbol ("lib" ++ show i ++ "_" ++ show j)

main = do
  initObjLinker RetainCAFs
  hsLoadObjs
  hsLoadSymbols


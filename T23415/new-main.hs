import Data.Either
import Data.Foldable
import Data.Map as M
import Control.Monad
import System.FilePath
import System.Directory
import GHCi.ObjLink

libname i = "lib" ++ show i

hsLoadObjs = do
  cwd <- getCurrentDirectory
  foldrM (\i acc -> do
    Right handle <- loadDLL (cwd </> libname i ++ ".out")
    return $ M.insert (libname i) handle acc
         )
         M.empty [0..499]

hsLoadSymbols handles = do
  forM_ [0..499] $ \i ->
    forM_ [0..99] $ \j -> do
      let symbolname = libname i ++ "_" ++ show j
      lookupSymbolInDLL (handles M.! libname i) symbolname

main = do
  initObjLinker RetainCAFs
  handles <- hsLoadObjs
  hsLoadSymbols handles
  print "hi"

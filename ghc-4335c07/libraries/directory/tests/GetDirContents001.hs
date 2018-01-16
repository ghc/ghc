{-# LANGUAGE CPP #-}
module GetDirContents001 where
#include "util.inl"
import System.FilePath ((</>))
import qualified Data.List as List

main :: TestEnv -> IO ()
main _t = do
  createDirectory dir
  T(expectEq) () specials . List.sort =<<
    getDirectoryContents dir
  T(expectEq) () [] . List.sort =<<
    listDirectory dir
  names <- for [1 .. 100 :: Int] $ \ i -> do
    let name = 'f' : show i
    writeFile (dir </> name) ""
    return name
  T(expectEq) () (List.sort (specials <> names)) . List.sort =<<
    getDirectoryContents dir
  T(expectEq) () (List.sort names) . List.sort =<<
    listDirectory dir
  where dir      = "dir"
        specials = [".", ".."]

{-# LANGUAGE CPP #-}
module CopyFile001 where
#include "util.inl"
import System.FilePath ((</>))
import qualified Data.List as List

main :: TestEnv -> IO ()
main _t = do
  createDirectory dir
  writeFile (dir </> from) contents
  T(expectEq) () [from] . List.sort =<< listDirectory dir
  copyFile (dir </> from) (dir </> to)
  T(expectEq) () [from, to] . List.sort =<< listDirectory dir
  T(expectEq) () contents =<< readFile (dir </> to)
  where
    contents = "This is the data\n"
    from     = "source"
    to       = "target"
    dir      = "dir"

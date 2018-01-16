{-# LANGUAGE CPP #-}
module CopyFile002 where
#include "util.inl"
import qualified Data.List as List

main :: TestEnv -> IO ()
main _t = do
  -- Similar to CopyFile001 but moves a file in the current directory
  -- (Bug #1652 on GHC Trac)
  writeFile from contents
  T(expectEq) () [from] . List.sort =<< listDirectory "."
  copyFile from to
  T(expectEq) () [from, to] . List.sort =<< listDirectory "."
  T(expectEq) () contents =<< readFile to
  where
    contents = "This is the data\n"
    from     = "source"
    to       = "target"

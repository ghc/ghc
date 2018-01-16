{-# LANGUAGE CPP #-}
module Directory001 where
#include "util.inl"

main :: TestEnv -> IO ()
main _t = do

  createDirectory "foo"
  writeFile "foo/bar" str
  renameFile "foo/bar" "foo/baz"
  renameDirectory "foo" "bar"
  str' <- readFile "bar/baz"
  T(expectEq) () str' str
  removeFile "bar/baz"
  removeDirectory "bar"

  where
    str = "Okay\n"

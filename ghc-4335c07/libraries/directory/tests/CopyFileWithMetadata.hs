{-# LANGUAGE CPP #-}
module CopyFileWithMetadata where
#include "util.inl"
import qualified Data.List as List

main :: TestEnv -> IO ()
main _t = (`finally` cleanup) $ do

  -- prepare source file
  writeFile "a" contents
  writeFile "b" "To be replaced\n"
  setModificationTime "a" mtime
  modifyWritable False "a"
  perm <- getPermissions "a"

  -- sanity check
  T(expectEq) () ["a", "b"] . List.sort =<< listDirectory "."

  -- copy file
  copyFileWithMetadata "a" "b"
  copyFileWithMetadata "a" "c"

  -- make sure we got the right results
  T(expectEq) () ["a", "b", "c"] . List.sort =<< listDirectory "."
  for_ ["b", "c"] $ \ f -> do
    T(expectEq) f perm =<< getPermissions f
    T(expectEq) f mtime =<< getModificationTime f
    T(expectEq) f contents =<< readFile f

  where
    contents = "This is the data\n"
    mtime = read "2000-01-01 00:00:00"

    cleanup = do
      -- needed to ensure the test runner can clean up our mess
      modifyWritable True "a" `catchIOError` \ _ -> return ()
      modifyWritable True "b" `catchIOError` \ _ -> return ()
      modifyWritable True "c" `catchIOError` \ _ -> return ()

    modifyWritable b f = do
      perm <- getPermissions f
      setPermissions f (setOwnerWritable b perm)

{-# LANGUAGE CPP #-}
module RemoveDirectoryRecursive001 where
#include "util.inl"
import System.FilePath ((</>), normalise)
import qualified Data.List as List
import TestUtils

main :: TestEnv -> IO ()
main _t = do

  ------------------------------------------------------------
  -- clean up junk from previous invocations

  modifyPermissions (tmp "c") (\ p -> p { writable = True })
    `catchIOError` \ _ -> return ()
  removeDirectoryRecursive tmpD
    `catchIOError` \ _ -> return ()

  ------------------------------------------------------------
  -- set up

  createDirectoryIfMissing True (tmp "a/x/w")
  createDirectoryIfMissing True (tmp "a/y")
  createDirectoryIfMissing True (tmp "a/z")
  createDirectoryIfMissing True (tmp "b")
  createDirectoryIfMissing True (tmp "c")
  writeFile (tmp "a/x/w/u") "foo"
  writeFile (tmp "a/t")     "bar"
  symlinkOrCopy (normalise "../a") (tmp "b/g")
  symlinkOrCopy (normalise "../b") (tmp "c/h")
  symlinkOrCopy (normalise "a")    (tmp "d")
  modifyPermissions (tmp "c") (\ p -> p { writable = False })

  ------------------------------------------------------------
  -- tests

  T(expectEq) () [".", "..", "a", "b", "c", "d"] . List.sort =<<
    getDirectoryContents  tmpD
  T(expectEq) () [".", "..", "t", "x", "y", "z"] . List.sort =<<
    getDirectoryContents (tmp "a")
  T(expectEq) () [".", "..", "g"] . List.sort =<<
    getDirectoryContents (tmp "b")
  T(expectEq) () [".", "..", "h"] . List.sort =<<
    getDirectoryContents (tmp "c")
  T(expectEq) () [".", "..", "t", "x", "y", "z"] . List.sort =<<
    getDirectoryContents (tmp "d")

  removeDirectoryRecursive (tmp "d")
    `catchIOError` \ _ -> removeFile      (tmp "d")
#ifdef mingw32_HOST_OS
    `catchIOError` \ _ -> removeDirectory (tmp "d")
#endif

  T(expectEq) () [".", "..", "a", "b", "c"] . List.sort =<<
    getDirectoryContents  tmpD
  T(expectEq) () [".", "..", "t", "x", "y", "z"] . List.sort =<<
    getDirectoryContents (tmp "a")
  T(expectEq) () [".", "..", "g"] . List.sort =<<
    getDirectoryContents (tmp "b")
  T(expectEq) () [".", "..", "h"] . List.sort =<<
    getDirectoryContents (tmp "c")

  removeDirectoryRecursive (tmp "c")
    `catchIOError` \ _ -> do
      modifyPermissions (tmp "c") (\ p -> p { writable = True })
      removeDirectoryRecursive (tmp "c")

  T(expectEq) () [".", "..", "a", "b"] . List.sort =<<
    getDirectoryContents  tmpD
  T(expectEq) () [".", "..", "t", "x", "y", "z"] . List.sort =<<
   getDirectoryContents (tmp "a")
  T(expectEq) () [".", "..", "g"] . List.sort =<<
    getDirectoryContents (tmp "b")

  removeDirectoryRecursive (tmp "b")

  T(expectEq) () [".", "..", "a"] . List.sort =<<
    getDirectoryContents  tmpD
  T(expectEq) () [".", "..", "t", "x", "y", "z"] . List.sort =<<
    getDirectoryContents (tmp "a")

  removeDirectoryRecursive (tmp "a")

  T(expectEq) () [".", ".."] . List.sort =<<
    getDirectoryContents  tmpD

  where testName = "removeDirectoryRecursive001"
        tmpD  = testName ++ ".tmp"
        tmp s = tmpD </> normalise s

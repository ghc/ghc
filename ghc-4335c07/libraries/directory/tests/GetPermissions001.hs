{-# LANGUAGE CPP #-}
module GetPermissions001 where
#include "util.inl"
import TestUtils

main :: TestEnv -> IO ()
main _t = do

  checkCurrentDir
  checkExecutable
  checkOrdinary
  checkTrailingSlash

  -- 'writable' is the only permission that can be changed on Windows
  writeFile "foo.txt" ""
  foo <- makeAbsolute "foo.txt"
  modifyPermissions "foo.txt" (\ p -> p { writable = False })
  T(expect) () =<< not . writable <$> getPermissions "foo.txt"
  modifyPermissions "foo.txt" (\ p -> p { writable = True })
  T(expect) () =<< writable <$> getPermissions "foo.txt"
  modifyPermissions "foo.txt" (\ p -> p { writable = False })
  T(expect) () =<< not . writable <$> getPermissions "foo.txt"
  modifyPermissions foo (\ p -> p { writable = True })
  T(expect) () =<< writable <$> getPermissions foo
  modifyPermissions foo (\ p -> p { writable = False })
  T(expect) () =<< not . writable <$> getPermissions foo

  where

    checkCurrentDir = do
      -- since the current directory is created by the test runner,
      -- it should be readable, writable, and searchable
      p <- getPermissions "."
      T(expect) () (readable p)
      T(expect) () (writable p)
      T(expect) () (not (executable p))
      T(expect) () (searchable p)

    checkExecutable = do
      -- 'find' expected to exist on both Windows and POSIX,
      -- though we have no idea if it's writable
      Just f <- findExecutable "find"
      p <- getPermissions f
      T(expect) () (readable p)
      T(expect) () (executable p)
      T(expect) () (not (searchable p))

    checkOrdinary = do
      writeFile "foo" ""
      p <- getPermissions "foo"
      T(expect) () (readable p)
      T(expect) () (writable p)
      T(expect) () (not (executable p))
      T(expect) () (not (searchable p))

    -- [regression test] (issue #9)
    -- Windows doesn't like trailing path separators
    checkTrailingSlash = do
      createDirectory "bar"
      _ <- getPermissions "bar/"
      return ()

{-# LANGUAGE CPP #-}
module DoesDirectoryExist001 where
#include "util.inl"

main :: TestEnv -> IO ()
main _t = do

  -- [regression test] "/" was not recognised as a directory prior to GHC 6.1
  T(expect) () =<< doesDirectoryExist rootDir

  createDirectory "somedir"

  T(expect) () . not =<< doesDirectoryExist "nonexistent"
  T(expect) () =<< doesDirectoryExist "somedir"
#ifdef mingw32_HOST_OS
  T(expect) () =<< doesDirectoryExist "SoMeDiR"
#endif

  where
#ifdef mingw32_HOST_OS
    rootDir = "C:\\"
#else
    rootDir = "/"
#endif

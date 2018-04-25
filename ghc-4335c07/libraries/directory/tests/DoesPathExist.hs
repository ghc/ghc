{-# LANGUAGE CPP #-}
module DoesPathExist where
#include "util.inl"

main :: TestEnv -> IO ()
main _t = do

  T(expect) () =<< doesPathExist rootDir

  createDirectory "somedir"
  writeFile "somefile" "somedata"
  writeFile "\x3c0\x42f\x97f3\xe6\x221e" "somedata"

  T(expect) () . not =<< doesPathExist "nonexistent"
  T(expect) () =<< doesPathExist "somedir"
  T(expect) () =<< doesPathExist "somefile"
  T(expect) () =<< doesPathExist "./somefile"
#ifdef mingw32_HOST_OS
  T(expect) () =<< doesPathExist "SoMeDiR"
  T(expect) () =<< doesPathExist "sOmEfIlE"
#endif
  T(expect) () =<< doesPathExist "\x3c0\x42f\x97f3\xe6\x221e"

  where
#ifdef mingw32_HOST_OS
    rootDir = "C:\\"
#else
    rootDir = "/"
#endif

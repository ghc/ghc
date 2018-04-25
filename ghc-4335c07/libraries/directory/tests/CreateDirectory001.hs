{-# LANGUAGE CPP #-}
module CreateDirectory001 where
#include "util.inl"

main :: TestEnv -> IO ()
main _t = do
  createDirectory testdir
  T(expectIOErrorType) () isAlreadyExistsError (createDirectory testdir)
  where testdir = "dir"

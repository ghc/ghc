{-# LANGUAGE CPP #-}
module GetDirContents002 where
#include "util.inl"

main :: TestEnv -> IO ()
main _t = do
  T(expectIOErrorType) () isDoesNotExistError $
    getDirectoryContents "nonexistent"

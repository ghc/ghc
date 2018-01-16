{-# LANGUAGE CPP #-}
module GetFileSize where
#include "util.inl"

main :: TestEnv -> IO ()
main _t = do

  withBinaryFile "emptyfile" WriteMode $ \ _ -> do
    return ()
  withBinaryFile "testfile" WriteMode $ \ h -> do
    hPutStr h string

  T(expectEq) () 0 =<< getFileSize "emptyfile"
  T(expectEq) () (fromIntegral (length string)) =<< getFileSize "testfile"

  where
    string = "The quick brown fox jumps over the lazy dog."

module Main where
import Foreign.Marshal.Alloc
import System.IO

main = do
  h <- openBinaryFile "4895.hs" ReadMode
  allocaBytes 10 $ \ptr -> hGetBuf h ptr 10
  some <- allocaBytes 10 $ \ptr -> hGetBufSome h ptr 10
  print some

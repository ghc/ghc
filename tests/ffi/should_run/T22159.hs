{-# LANGUAGE CPP #-}
module Main (main) where

import Foreign.C.String (peekCWString)
import Foreign.C.Types (CWchar)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr)

foreign import ccall "hello" c_hello :: Ptr CWchar -> IO ()

main :: IO ()
main = allocaBytes 12 $ \buf -> do
  c_hello buf
  str <- peekCWString buf
  putStrLn str

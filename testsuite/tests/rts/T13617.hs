module Main where

import Foreign
import Foreign.Ptr
import Foreign.C

import Foreign.Marshal.Array

foreign import ccall unsafe "mult" mult :: Ptr CInt -> Ptr CInt
                                        -> CInt -> IO CInt

main = do res <- withArray [1..10] $ \a ->
                 withArray [5..15] $ \b ->
                   mult a b 10
          print res

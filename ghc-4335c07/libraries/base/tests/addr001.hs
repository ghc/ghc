-- !!! Testing that Show for Addr is OK..
module Main(main) where

import Foreign.Ptr

main :: IO ()
main = do
  print (nullPtr `plusPtr` maxBound)
  print (nullPtr `plusPtr` minBound)


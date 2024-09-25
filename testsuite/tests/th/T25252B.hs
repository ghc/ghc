module T25252B where

import Data.Word
import Foreign.StablePtr

foreign import ccall "hs_custom_closureSize" closureSize :: StablePtr a -> Word64

foo :: IO ()
foo = do
  let
    x :: [Int]
    x = cycle [10,20] -- segfaults without "cycle"...
  sp <- newStablePtr x
  print (closureSize sp /= 0)


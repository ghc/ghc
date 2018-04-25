module Main where

import Foreign
import Foreign.C.Types
foreign import ccall "foo" dle :: IO CInt

main = dle >>= print

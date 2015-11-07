module Main where

import Foreign
import Foreign.C.Types
foreign import ccall "bar" dle :: IO CInt

main = dle >>= print

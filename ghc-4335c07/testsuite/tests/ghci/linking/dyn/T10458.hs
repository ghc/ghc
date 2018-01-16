module T10458 (callSO) where

import Foreign
import Foreign.C.Types
foreign import ccall "foo" dle :: IO CInt

callSO :: IO CInt
callSO = dle

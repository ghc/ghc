module WeakTest where

import Foreign.C.Types

foreign import ccall weak_test :: CInt -> IO CInt

module T7243 where

import Foreign.Ptr
foreign import ccall "wrapper" foo :: IO (FunPtr ())

-- !!! cc008 -- foreign export dynamic returning newtype of Addr
module ShouldCompile where

import Addr

newtype Ptr a = Ptr Addr

foreign import ccall "wrapper" mkFoo :: IO () -> IO (Ptr Int)

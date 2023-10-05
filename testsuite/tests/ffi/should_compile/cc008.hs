-- !!! cc008 -- foreign export dynamic returning newtype of Addr
module ShouldCompile where

import Foreign
type Addr = FunPtr (IO ())
newtype NPtr a = NPtr Addr

foreign import ccall "wrapper" mkFoo :: IO () -> IO (NPtr Int)

-- !!! cc009 -- foreign label returning newtype of Addr
module ShouldCompile where

import Foreign
type Addr = Ptr ()
newtype NPtr a = NPtr Addr

foreign import ccall "&" foo :: NPtr Int

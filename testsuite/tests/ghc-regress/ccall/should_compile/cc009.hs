-- !!! cc009 -- foreign label returning newtype of Addr
module ShouldCompile where

import Addr

newtype Ptr a = Ptr Addr

foreign import ccall "&" foo :: Ptr Int

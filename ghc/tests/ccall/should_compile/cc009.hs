-- !!! cc009 -- foreign label returning newtype of Addr
module Test where

import Addr

newtype Ptr a = Ptr Addr

foreign label foo :: Ptr Int

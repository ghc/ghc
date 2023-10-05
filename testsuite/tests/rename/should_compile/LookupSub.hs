{-# Language NoImplicitPrelude #-}
module LookupSub where
import qualified LookupSubA
import qualified LookupSubB

data FD = FD

getEcho = FD

instance LookupSubA.IODevice FD where
  getEcho = getEcho

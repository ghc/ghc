-- !!! a bad _CCallable thing (from a bug from Satnam)
--
module ShouldFail where

import Foreign
import Addr
import CCall

data Socket = Socket# Addr
instance CCallable Socket

f :: Socket -> IO ()
f x = _ccall_ foo x

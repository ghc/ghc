--!!! a bad _CCallable thing (from a bug from Satnam)
--
module ShouldSucceed where
import PreludeGlaST

data Socket = Socket# Addr
instance CCallable Socket

f :: Socket -> PrimIO ()
f x = _ccall_ foo x

--!!! a bad _CCallable thing (from a bug from Satnam)
--
data Socket = Socket# _Addr
instance _CCallable Socket

f :: Socket -> PrimIO ()
f x = _ccall_ foo x

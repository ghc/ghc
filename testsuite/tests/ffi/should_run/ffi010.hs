import Foreign

-- !!! GHC 5.04.2 was missing rts_mkFunPtr, which meant that this example
-- !!! didn't link.

foreign import ccall "wrapper" 
  makeHaskellFun :: (FunPtr a -> IO ()) -> IO (FunPtr (FunPtr a -> IO ()))

main = makeHaskellFun (const (return ()))

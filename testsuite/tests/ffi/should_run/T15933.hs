module Main(main) where

import Foreign
import Foreign.C

type HsCallback = CInt -> IO ()

foreign import ccall "T15933.h function_in_c"
  functionInC :: FunPtr HsCallback -> IO ()

foreign import ccall "wrapper"
  wrap :: HsCallback -> IO (FunPtr HsCallback)

main = do
  f <- wrap $ \x -> print x
  functionInC f
  freeHaskellFunPtr f

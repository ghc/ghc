-- A reduced version of the original test case

{-# LANGUAGE ForeignFunctionInterface #-}


import Control.Concurrent
import Control.Monad
import Foreign.C.Types
import Foreign.Ptr


main :: IO ()
main = do
    mv <- newEmptyMVar
    -- Fork a thread to continually dereference a stable pointer...
    void $ forkIO $ f 1 1000000 >> putMVar mv ()
    -- ...while we keep enlarging the stable pointer table
    f 65536 1
    void $ takeMVar mv
  where
    f nWraps nApplies = replicateM_ nWraps $ do
                            -- Each call to wrap creates a stable pointer
                            wrappedPlus <- wrap (+)
                            c_applyFun nApplies wrappedPlus 1 2


type CIntFun = CInt -> CInt -> CInt

foreign import ccall "wrapper"
    wrap :: CIntFun -> IO (FunPtr CIntFun)

foreign import ccall "apply_fun"
    c_applyFun :: CInt -> FunPtr CIntFun -> CInt -> CInt -> IO CInt

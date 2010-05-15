
{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign
import Foreign.C

d f x = unsafePerformIO $ do
    g <- mkfun f
    r <- deriv g x 1
    return r

main = do
    print $ d (\x -> x * 2) 3
    print $ d (\x -> x * d (\y -> x + y) 5) 7


foreign import ccall safe "deriv"
    deriv :: FunPtr (CDouble -> CDouble) -> CDouble -> CDouble -> IO CDouble

foreign import ccall safe "wrapper"
    mkfun :: (CDouble -> CDouble) -> IO (FunPtr (CDouble -> CDouble))

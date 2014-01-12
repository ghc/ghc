module T5664 where

import Foreign
import Foreign.C

data D = D
newtype I = I CInt

foreign import ccall "dynamic"
  mkFun1 :: FunPtr (CInt -> IO ()) -> CInt -> IO ()

foreign import ccall "dynamic"
  mkFun2 :: FunPtr (I -> IO ()) -> CInt -> IO ()

foreign import ccall "dynamic"
  mkFun3 :: FunPtr (D -> IO ()) -> CInt -> IO ()

foreign import ccall "wrapper"
  mkCallBack1 :: IO CInt -> IO (FunPtr (IO CInt))

foreign import ccall "wrapper"
  mkCallBack2 :: IO CInt -> IO (FunPtr (IO I))

foreign import ccall "wrapper"
  mkCallBack3 :: IO CInt -> IO (FunPtr (IO D))

{-# LANGUAGE LinearTypes #-}
module LinearFFI where -- #18472

import Foreign.Ptr

foreign import ccall "exp" c_exp :: Double %1 -> Double
foreign import stdcall "dynamic" d8  :: FunPtr (IO Int) %1 -> IO Int
foreign import ccall "wrapper" mkF :: IO () %1 -> IO (FunPtr (IO ()))

{-# LANGUAGE LinearTypes #-}
module LinearFFI where -- #18472

import Foreign.Ptr

foreign import ccall "exp" c_exp :: Double #-> Double
foreign import stdcall "dynamic" d8  :: FunPtr (IO Int) #-> IO Int
foreign import ccall "wrapper" mkF :: IO () #-> IO (FunPtr (IO ()))

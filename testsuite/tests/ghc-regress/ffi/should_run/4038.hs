{-# LANGUAGE ForeignFunctionInterface #-}
import Foreign
import Foreign.C

type IOF = Int -> IO Int

foreign import ccall "wrapper" wrap_f_io :: IOF -> IO (FunPtr IOF)
foreign import ccall "dynamic" f_io :: FunPtr IOF -> IOF

f :: Int -> IO Int
f 0 = return 42
f n = do 
  f' <- wrap_f_io f
  f_io f' (n-1)

main = f 1000 >>= print

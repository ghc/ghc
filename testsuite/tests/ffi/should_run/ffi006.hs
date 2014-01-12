-- !!! Test that we can call a foreign import "wrapper" using foreign
-- import "dynamic", in both IO and non-IO flavours.

import Foreign
import Foreign.C

type IOF = Int -> IO Int
type F   = Int ->    Int

foreign import ccall "wrapper" wrap_f    :: F -> IO (FunPtr F)
foreign import ccall "wrapper" wrap_f_io :: IOF -> IO (FunPtr IOF)

foreign import ccall "dynamic" f    :: FunPtr F   -> F
foreign import ccall "dynamic" f_io :: FunPtr IOF -> IOF

double :: Int -> Int
double x = x * 2

double_io :: Int -> IO Int
double_io x = return (x * 2)

main = do
  double1 <- wrap_f double
  print (f double1 42)
  double2 <- wrap_f_io double_io
  x <- f_io double2 42
  print x

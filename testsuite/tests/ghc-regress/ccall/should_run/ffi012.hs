-- !!! Same as ffi006, but using the stdcall calling convention.

import Foreign
import Foreign.C

type IOF = Int -> IO Int
type F   = Int ->    Int

foreign import stdcall "wrapper" wrap_f    :: F -> IO (FunPtr F)
foreign import stdcall "wrapper" wrap_f_io :: IOF -> IO (FunPtr IOF)

foreign import stdcall "dynamic" f    :: FunPtr F   -> F
foreign import stdcall "dynamic" f_io :: FunPtr IOF -> IOF

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

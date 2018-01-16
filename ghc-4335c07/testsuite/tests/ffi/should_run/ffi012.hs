-- !!! Same as ffi006, but using the stdcall calling convention.

import Foreign
import Foreign.C

-- With Ints: ---------------------------------

type IOF = Int -> IO Int
type F   = Int ->    Int

foreign import stdcall "wrapper" wrap_f    :: F -> IO (FunPtr F)
foreign import stdcall "wrapper" wrap_f_io :: IOF -> IO (FunPtr IOF)

foreign import stdcall "dynamic" f    :: FunPtr F   -> F
foreign import stdcall "dynamic" f_io :: FunPtr IOF -> IOF

fn_int :: Int -> Int
fn_int x = x * 2

fn_int_io :: Int -> IO Int
fn_int_io x = return (x * 2)

-- With Doubles: ---------------------------------

type IOD = Double -> IO Double
type D   = Double ->    Double

foreign import stdcall "wrapper" wrap_d    :: D -> IO (FunPtr D)
foreign import stdcall "wrapper" wrap_d_io :: IOD -> IO (FunPtr IOD)

foreign import stdcall "dynamic" d    :: FunPtr D   -> D
foreign import stdcall "dynamic" d_io :: FunPtr IOD -> IOD

fn_double :: Double -> Double
fn_double x = x * 2

fn_double_io :: Double -> IO Double
fn_double_io x = return (x * 2)

--------------------------------------------------

main = do
  wrapped_fn_int <- wrap_f fn_int
  print (f wrapped_fn_int 42)
  wrapped_fn_int_io <- wrap_f_io fn_int_io
  x <- f_io wrapped_fn_int_io 42
  print x

  wrapped_fn_double <- wrap_d fn_double
  print (d wrapped_fn_double 42)
  wrapped_fn_double_io <- wrap_d_io fn_double_io
  x <- d_io wrapped_fn_double_io 42
  print x

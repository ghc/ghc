import Foreign
import Foreign.C

type IOF = Int -> IO Int

foreign import ccall "wrapper" wrap_f_io :: IOF -> IO (FunPtr IOF)
foreign import ccall "dynamic" f_io :: FunPtr IOF -> IOF

double_io :: Int -> IO Int
double_io x = return (x * 2)

main = do
  double2 <- wrap_f_io double_io
  x <- f_io double2 42
  return ()

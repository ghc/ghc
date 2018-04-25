-- !!! Test passing doubles to a ccall

import Foreign.C

foreign import ccall unsafe "math.h sin"
  c_sin :: CDouble -> IO CDouble

main = c_sin 1.0 >>= print

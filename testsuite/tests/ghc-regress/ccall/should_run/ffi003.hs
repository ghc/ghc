-- !!! Test passing doubles to a ccall

import Foreign
import Foreign.C

foreign import ccall unsafe 
  printf :: CString -> Double -> IO CInt

main = withCString "%f" $ \s -> printf s 1.2345

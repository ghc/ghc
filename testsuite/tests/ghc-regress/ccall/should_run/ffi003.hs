{-# OPTIONS -fasm #-}

-- !!! Test passing floats to a ccall, was broken in the NCG in 5.02.2

import Foreign
import Foreign.C

foreign import ccall unsafe 
  printf :: CString -> Float -> IO CInt

main = withCString "%f" $ \s -> printf s 1.2345

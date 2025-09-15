module TestLink where

import Foreign.C

foreign import ccall "f" f :: CInt -> IO CInt

test :: IO ()
test = f 42 >>= print

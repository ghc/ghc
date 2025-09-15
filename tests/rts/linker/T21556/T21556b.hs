{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.C.Types

foreign import ccall unsafe "hello" hello :: CInt -> IO ()

main = hello 42

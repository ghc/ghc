{-# LANGUAGE ForeignFunctionInterface #-}

foreign import ccall "ffi.h some_function" someFunction :: Int -> Int

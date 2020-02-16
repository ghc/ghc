{-# LANGUAGE ForeignFunctionInterface #-}

foreign import ccall "T11531.h some_function" someFunction :: Int -> Int

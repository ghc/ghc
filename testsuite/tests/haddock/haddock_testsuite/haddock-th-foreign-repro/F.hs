{-# LANGUAGE ForeignFunctionInterface #-}
module F where

foreign import ccall "some_c_function" c_some_c_function :: IO ()

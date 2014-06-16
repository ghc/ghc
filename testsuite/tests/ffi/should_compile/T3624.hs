{-# LANGUAGE ForeignFunctionInterface #-}
module Test where

foreign import ccall "path/to/foo.h foo" foo :: Int -> IO Float

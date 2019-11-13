{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Foreign.C.Types

foreign import ccall "foo" foo :: IO CInt

main :: IO ()
main = foo >>= print

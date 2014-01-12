
{-# LANGUAGE ForeignFunctionInterface #-}
module T3807Export where

import Foreign.C

foo :: IO CInt
foo = return (3 + read "123")

foreign export ccall foo :: IO CInt


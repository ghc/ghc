{-# LANGUAGE ForeignFunctionInterface #-}

module T18072 where

import Foreign.C

foo :: IO CInt
foo = return 3

foreign export ccall foo :: IO CInt

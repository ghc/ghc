{-# LANGUAGE ForeignFunctionInterface #-}
-- !!! test that exporting the same thing multiple times works.
module ShouldCompile where

import Data.Int

foreign export ccall "addByte" (+) :: Int8  -> Int8  -> Int8
foreign export ccall "addInt"  (+) :: Int16 -> Int16 -> Int16
foreign export ccall "addLong" (+) :: Int32 -> Int32 -> Int32

foreign export ccall "divByte" div :: Int8  -> Int8  -> Int8
foreign export ccall "divInt"  div :: Int16 -> Int16 -> Int16
foreign export ccall "divLong" div :: Int32 -> Int32 -> Int32


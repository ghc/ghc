{-# LANGUAGE ForeignFunctionInterface #-}
module Shared001 where

-- Test for building DLLs with ghc -shared, see #2745

f :: Int -> Int
f x = x+1

foreign export ccall f :: Int -> Int


{-# LANGUAGE TypeFamilies #-}

module Ccfail005 where

type family F a
type instance F Bool = D -> IO Int
type instance F Char = Int -> IO D
data D = D

-- These should be rejected as D isn't a type we can use with the FFI.
-- Note that, in the signature the user writes, there aren't an
-- "argument type" and "result type" to complain about, though.
foreign import ccall f1 :: F Bool
foreign import ccall f2 :: F Char


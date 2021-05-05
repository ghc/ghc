{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -ffun-to-thunk #-}   -- This is essential for the test

module Foo where
import GHC.Exts

f :: Int -> Int#
f x = f (x+1)

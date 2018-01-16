module Foo where
{-# NOINLINE foo #-}
foo :: Int -> Int
foo x = x + 23

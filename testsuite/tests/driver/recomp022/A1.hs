module A (foo) where

{-# OPAQUE foo #-}
foo :: Int -> Int
foo x = x + 1

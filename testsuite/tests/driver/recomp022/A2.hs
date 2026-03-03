module A (foo) where

{-# OPAQUE foo #-}
foo :: Int -> Int
foo x = x + n
  where n = 1

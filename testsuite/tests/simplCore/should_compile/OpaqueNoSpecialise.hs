module OpaqueNoSpecialise where

f x = x : f (x-1)
{-# OPAQUE f #-}

-- This would normally induce a specialisation of f on Int
g (x :: Int) = f x

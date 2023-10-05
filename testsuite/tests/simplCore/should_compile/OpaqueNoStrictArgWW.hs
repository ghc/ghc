module OpaqueNoStrictArgWW where

f :: Int -> Int
f x = x + 1
{-# OPAQUE f #-}

module OpaqueNoAbsentArgWW where

f :: Int -> Int -> Bool
f _ i = i == 0
{-# OPAQUE f #-}

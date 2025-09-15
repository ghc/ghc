module OpaqueNoWW where

-- Would normally result in a worker of type Int# -> Int#
f :: Int -> Int
f 0 = 0
f x = f (x + 1)
{-# OPAQUE f #-}

g :: Bool -> Bool -> Int -> Int
g True  True  p = f p
g False True  p = p + 1
g b     False p = g b True p

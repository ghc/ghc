module Main (main, f, g, h, i) where

main = print (f 42)

f, g, h, i :: Int -> Int
f x = g x + h x + i x

g x = x + 1

{-# INLINE h #-}
h x = x + 2

{-# INLINABLE i #-}
i x = x + 3

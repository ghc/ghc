module D (f) where

import E

{-# INLINE f #-}
f :: Int -> Int
f x = h x

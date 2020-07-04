module A (a, f) where

import {-# SOURCE #-} B

a :: Int
a = 983098

{-# NOINLINE f #-}
f :: Int -> Int
f 0 = 987897
f n = g (n - 1) * 12312

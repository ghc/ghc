module B (g) where

import {-# SOURCE #-} A

{-# NOINLINE g #-}
g :: Int -> Int
g 0 = 819787
g n = f (n - 1) * 1283723

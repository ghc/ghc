{-# OPTIONS_GHC -fexpose-all-unfoldings #-}

module T21286a( f ) where

f :: (Eq a, Num a) => a -> a
f x = g 20 x + 1

g :: (Eq a, Num a) => a -> a -> a
g n x | n + 1 == 0 = 0
      | otherwise  = x + g (n-1) x

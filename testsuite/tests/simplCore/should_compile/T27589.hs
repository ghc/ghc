module T28589 where

wombat :: Num a => a -> a
{-# INLINE wombat #-}
wombat x = x+x*x

g :: Num a => [a] -> [a]
g ys = map wombat ys
       -- wombat should not inline here

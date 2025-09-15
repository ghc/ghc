module T23413 where

f :: (Int ~ Bool) => Int -> Bool
f x = f x

g1 :: (Int ~ Bool) => Int -> Bool
g1 x = f x

g2 :: (Bool ~ Int) => Int -> Bool
g2 x = f x

h :: (Int ~ Bool) => Int -> Bool
h x = x

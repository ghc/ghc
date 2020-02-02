{-# LANGUAGE RankNTypes #-}
module T9569a where

g :: (Int -> Int) -> Int
g f = f 4

f1 :: (forall a. a -> a) -> Int
-- Fails; needs eta-expansion
-- cf T9569b
f1 = g


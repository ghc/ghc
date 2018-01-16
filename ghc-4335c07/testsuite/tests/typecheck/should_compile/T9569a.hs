{-# LANGUAGE RankNTypes #-}
module T9569a where

g :: (Int -> Int) -> Int
g f = f 4

f1 :: (forall a. a -> a) -> Int
f1 = g

f2 :: (forall a. a -> a) -> Int
f2 x = g x

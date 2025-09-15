{-# LANGUAGE RankNTypes #-}
module T9569a where

g :: (Int -> Int) -> Int
g f = f 4

f2 :: (forall a. a -> a) -> Int
f2 f = g f

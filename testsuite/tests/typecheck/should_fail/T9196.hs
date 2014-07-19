{-# LANGUAGE RankNTypes #-}
module T9196 where

f :: (forall a. Eq a) => a -> a
f x = x

g :: (Eq a => Ord a) => a -> a
g x = x

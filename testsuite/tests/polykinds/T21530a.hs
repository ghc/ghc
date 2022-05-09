{-# LANGUAGE RankNTypes #-}

module T21530 where

f :: (forall a. (Show a, Eq a) => a -> String) -> String
f h = h True

g :: (forall a. Show a => a -> String) -> String
g = f

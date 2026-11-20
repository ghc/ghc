{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
module Filter (Hidden, filterVec, toList) where
import Vector

type Hidden a = exists (m :: Nat). Vec m a

filterVec :: (a -> Bool) -> Vec n a -> Hidden a
filterVec _ Nil = Nil
filterVec p (Cons x xs)
    | p x = Cons x (filterVec p xs)
    | otherwise = filterVec p xs

toList :: Hidden a -> [a]
toList xs = go xs
  where
    go :: Vec n a -> [a]
    go Nil = []
    go (Cons x rest) = x : go rest

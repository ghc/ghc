{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
module Filter (Hidden, filterVec, partitionVec, toList, fromList) where
import Vector

type Hidden a = exists (m :: Nat). Vec m a

filterVec :: (a -> Bool) -> Vec n a -> Hidden a
filterVec _ Nil = Nil
filterVec p (Cons x xs)
    | p x = Cons x (filterVec p xs)
    | otherwise = filterVec p xs

partitionVec :: (a -> Bool) -> Vec n a -> (Hidden a, Hidden a)
partitionVec _ Nil = (Nil, Nil)
partitionVec p (Cons x xs) =
    let (yes, no) = partitionVec p xs
    in if p x then (Cons x yes, no) else (yes, Cons x no)

toList :: Hidden a -> [a]
toList xs = go xs
  where
    go :: Vec n a -> [a]
    go Nil = []
    go (Cons x rest) = x : go rest

-- | Build a vector that hides the length of the input list.
fromList :: [a] -> Hidden a
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

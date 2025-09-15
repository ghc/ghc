module T21315 where

data T a = MkT a deriving (Eq, Ord)

class Ord a => C a
instance (Eq a, Ord a) => C (T a)

{-# LANGUAGE QuantifiedConstraints #-}

module T22216d where

class (forall a. Eq a => Eq (f a)) => Eq1 f where
  liftEq :: (a -> b -> Bool) -> f a -> f b -> Bool

eq1 :: (Eq1 f, Eq a) => f a -> f a -> Bool
eq1 = liftEq (==)

class (Eq1 f, forall a. Ord a => Ord (f a)) => Ord1 f where
  liftCompare :: (a -> b -> Ordering) -> f a -> f b -> Ordering

compare1 :: (Ord1 f, Ord a) => f a -> f a -> Ordering
compare1 = liftCompare compare

data Cofree f a = a :< f (Cofree f a)

instance (Eq1 f, Eq a) => Eq (Cofree f a) where
  (==) = eq1

instance (Eq1 f) => Eq1 (Cofree f) where
  liftEq eq = go
    where
      go (a :< as) (b :< bs) = eq a b && liftEq go as bs

instance (Ord1 f, Ord a) => Ord (Cofree f a) where
  compare = compare1

instance (Ord1 f) => Ord1 (Cofree f) where
  liftCompare cmp = go
    where
      go (a :< as) (b :< bs) = cmp a b `mappend` liftCompare go as bs

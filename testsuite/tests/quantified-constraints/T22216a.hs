{-# LANGUAGE QuantifiedConstraints, UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module T22216a where

class Eq a
class Eq a => Ord a

class (forall b. Eq b => Eq (f b)) => Eq1 f
class (Eq1 f, forall b. Ord b => Ord (f b)) => Ord1 f

foo :: (Ord a, Ord1 f) => (Eq (f a) => r) -> r
foo r = r

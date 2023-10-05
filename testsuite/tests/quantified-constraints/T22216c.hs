{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}

module T22216c where

import Prelude (Bool, Ordering)

class Eq a where
class Eq a => Ord a where
class (forall a. Eq a => Eq (f a)) => Eq1 f where

class (Eq1 f, forall a. Ord a => Ord (f a)) => Ord1 f where
  liftCompare :: (a -> b -> Ordering) -> f a -> f b -> Ordering

instance Eq  (T f a)
instance Ord (T f a)
instance Eq1 (T f)

newtype T f a = MkT (f a)
  deriving Ord1

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module T15290b where

import Data.Coerce
import Data.Kind

type Representational1 m = (forall a b. Coercible a b => Coercible (m a) (m b) :: Constraint)

class Representational1 f => Functor' f where
  fmap' :: (a -> b) -> f a -> f b

class Functor' f => Applicative' f where
  pure'  :: a -> f a
  (<*>@) :: f (a -> b) -> f a -> f b

class Functor' t => Traversable' t where
  traverse' :: Applicative' f => (a -> f b) -> t a -> f (t b)

-- Typechecks
newtype T1 m a = MkT1 (m a) deriving (Functor', Traversable')

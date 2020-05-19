{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Monad.Graded where

import Data.Kind (Constraint, Type)
import Prelude (const, id)

class GradedMonad (m :: k -> Type -> Type) where
  type Unit m :: k
  type Plus m (i :: k) (j :: k) :: k
  type Inv  m (i :: k) (j :: k) :: Constraint
  (>>=) :: Inv m i j => m i a -> (a -> m j b) -> m (Plus m i j) b
  return :: a -> m (Unit m) a

(>>) :: (GradedMonad m, Inv m i j) => m i a -> m j b -> m (Plus m i j) b
m >> n = m >>= const n

join :: (GradedMonad m, Inv m i j) => m i (m j a) -> m (Plus m i j) a
join m = m >>= id

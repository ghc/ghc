{-# LANGUAGE QuantifiedConstraints, UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module T22216e where

import Data.Kind

type C :: Type -> Type -> Constraint
type D :: Type -> Type -> Constraint

class C a b
instance C () Int
class D a b

foo :: ( forall a b. ( Eq a, Num b, C a b  ) => D a b
       , forall a  .                C a Int  => D a Int
       )
    => ( D () Int => r ) -> r
foo r = r

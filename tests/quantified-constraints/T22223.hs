{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, QuantifiedConstraints #-}

module T22223 where

import Data.Kind

type C :: Type -> Type -> Constraint
class C a b

-- Change the order of quantification
foo :: ( forall a b. (Eq a, Ord b) => C a b
       , forall b a. (Eq a, Ord b) => C a b
       , Eq x
       , Ord y )
    => ( C x y => r ) -> r
foo r = r

-- Permute the constraints in the context
bar :: ( forall a b. (Eq a, Ord b) => C a b
       , forall a b. (Ord b, Eq a) => C a b
       , Eq x
       , Ord y )
    => ( C x y => r ) -> r
bar r = r

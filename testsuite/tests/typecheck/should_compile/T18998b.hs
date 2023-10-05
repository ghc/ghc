{-# LANGUAGE ScopedTypeVariables, LinearTypes, DataKinds, TypeOperators, GADTs,
             PolyKinds, ConstraintKinds, TypeApplications #-}

module T18998b where

import GHC.TypeLits
import Data.Kind
import Unsafe.Coerce

data Dict :: Constraint -> Type where
  Dict :: c => Dict c
knowPred :: Dict (KnownNat (n+1)) -> Dict (KnownNat n)
knowPred Dict = unsafeCoerce (Dict :: Dict ())
data NList :: Nat -> Type -> Type where
  Nil :: NList 0 a
  Cons :: a %1-> NList n a %1-> NList (n+1) a
-- Alright, this breaks linearity for some unknown reason

snoc :: forall n a. KnownNat n => a %1-> NList n a %1-> NList (n+1) a
snoc a Nil = Cons a Nil
snoc a (Cons x (xs :: NList n' a)) = case knowPred (Dict :: Dict (KnownNat n)) of
   Dict -> Cons x (snoc a xs)
-- This works fine

snoc' :: forall n a. a %1-> NList n a %1-> NList (n+1) a
snoc' a Nil = Cons a Nil
snoc' a (Cons x xs) = Cons x (snoc' a xs)

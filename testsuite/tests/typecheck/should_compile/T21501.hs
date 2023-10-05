{-# LANGUAGE MonoLocalBinds, PatternSynonyms, ViewPatterns, TypeAbstractions #-}

module T21501 where

import Data.Kind
import Type.Reflection

pattern TypeApp ::
  forall {k1} {k2} (f :: k1 -> k2) (result :: k2).
  Typeable f =>
  forall (arg :: k1).
  result ~ f arg =>
  TypeRep arg ->
  TypeRep result
pattern TypeApp arg_rep <- App (eqTypeRep (typeRep @f) -> Just HRefl) arg_rep

f :: TypeRep (a :: Type) -> String
f (TypeApp @[] rep) = show rep

{- Expected type: TypeRep k (a::k)
   Instantiate at k10 k20 (f0 :: k10 -> k20) (result0 :: k20)
   Unify (TypeRep k (a::k) ~ TypeRep k20 (result :: k20)
   Unify f0 ~ []
-}

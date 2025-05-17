{-# Language PatternSynonyms, ViewPatterns, GADTs, ConstraintKinds, RankNTypes,
             PolyKinds, ScopedTypeVariables, DataKinds, TypeOperators,
             TypeApplications, TypeFamilies, TypeFamilyDependencies,
             TypeAbstractions #-}

module T14507 where

import Type.Reflection
import Data.Kind

foo :: TypeRep a -> (Bool :~~: k, TypeRep a)
foo rep = error "urk"

type SING :: k -> Type
type family SING @k where
  SING @Bool = TypeRep

pattern RepN :: forall kk (a::kk). () => Bool~kk => SING a -> TypeRep (a::kk)
pattern RepN tr <- (foo -> ( HRefl :: Bool:~~:kk
                           , tr :: TypeRep (a::Bool)))

pattern SO x <- RepN (id -> x)


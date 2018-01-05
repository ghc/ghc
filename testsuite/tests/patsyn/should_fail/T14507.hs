{-# Language PatternSynonyms, ViewPatterns, GADTs, ConstraintKinds, RankNTypes, KindSignatures, PolyKinds, ScopedTypeVariables, DataKinds, TypeInType, TypeOperators, TypeApplications, TypeFamilies, TypeFamilyDependencies #-}

module T14507 where

import Type.Reflection
import Data.Kind

foo :: TypeRep a -> (Bool :~~: k, TypeRep a)
foo rep = error "urk"

type family SING :: k -> Type where
  SING = (TypeRep :: Bool -> Type)

pattern RepN :: forall (a::kk). () => Bool~kk => SING a -> TypeRep (a::kk)
pattern RepN tr <- (foo -> ( HRefl :: Bool:~~:kk
                           , tr :: TypeRep (a::Bool)))

pattern SO x <- RepN (id -> x)


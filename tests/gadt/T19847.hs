{-# LANGUAGE GADTs, PatternSynonyms, ViewPatterns, ScopedTypeVariables, TypeAbstractions #-}

module T19847 where

import Data.Kind
import Type.Reflection

pattern Is :: forall (b :: Type) (a :: Type). Typeable b => (a ~ b) => TypeRep a
pattern Is <- (eqTypeRep (typeRep @b) -> Just HRefl)
  where Is = typeRep

def :: TypeRep a -> a
def x = case x of
  Is @Int  -> 10
  Is @Bool -> False

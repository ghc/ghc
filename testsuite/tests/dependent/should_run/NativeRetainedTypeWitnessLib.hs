{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module NativeRetainedTypeWitnessLib (preserve, duplicate, onlyBool) where

import Data.Kind (Type)
import Type.Reflection

preserve :: foreach (a :: Type) -> a -> a
preserve _ x = x

type Lifted = Type

duplicate :: foreach (a :: Lifted) -> TypeRep a -> (TypeRep a, TypeRep a)
duplicate (first :: TypeRep witness) second = (same first, same second)
  where
    same :: TypeRep witness -> TypeRep witness
    same x = x

onlyBool :: foreach (a :: Type) -> a -> Maybe a
onlyBool rep value =
  case eqTypeRep rep (typeRep @Bool) of
    Just HRefl -> Just (not value)
    Nothing -> Nothing

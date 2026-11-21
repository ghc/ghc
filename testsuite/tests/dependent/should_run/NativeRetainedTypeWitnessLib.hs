{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module NativeRetainedTypeWitnessLib
  (preserve, duplicate, onlyBool, selected, preserveImplicit, reflected,
   Label, labelWith, readLabel) where

import Data.Kind (Type)
import Numeric.Natural (Natural)
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

selected :: foreach (a :: Type). a -> String
selected _ = show a

preserveImplicit :: foreach (a :: Type). a -> a
preserveImplicit x = x

reflected :: foreach (a :: Type). TypeRep a
reflected = a

type role Label nominal
data Label (n :: Natural) = Label Natural String

labelWith :: foreach (a :: Type). foreach (n :: Natural) -> a -> Label n
labelWith n _ = Label n (show a)

readLabel :: Label n -> (Natural, String)
readLabel (Label n name) = (n, name)

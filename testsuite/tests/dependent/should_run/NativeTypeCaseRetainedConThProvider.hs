{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, LinearTypes #-}
module NativeTypeCaseRetainedConThProvider where

import GHC.Types (Multiplicity(Many))

type Select (b :: Bool) = case b of { True -> Int; False -> Char }
data Stored a where
  Stored :: forall a. ~Int %1 -> foreach (b :: Bool) -> Select b %Many ->
            foreach (b :: Bool) -> Select b %1 -> a %1 -> Stored a
  Vacuous :: forall a. foreach (b :: Bool) -> a %1 -> Stored a
  PlainStored :: a %1 -> Stored a

{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, TypeOperators #-}
module NativeRetainedCastNominal where

import Data.Coerce (Coercible)
import PackedVecLibrary (Nat, Vec, transportElement)

bad :: forall a b. Coercible a b => (foreach (n :: Nat) -> Vec a n) -> (foreach (n :: Nat) -> Vec b n)
bad f = transportElement @a @b f

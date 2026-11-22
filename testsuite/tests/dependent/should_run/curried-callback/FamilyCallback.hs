{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, TypeFamilies, TypeOperators #-}
module FamilyCallback (Result, law) where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))

type family Result (tag :: Bool) :: Type where
  Result 'True = Bool -> Ordering
  Result 'False = Bool

-- | Keep the callback and its argument unevaluated at a family result.
#ifdef AMBIGUOUS
law :: forall tag. foreach (f :: Bool -> Result tag) -> foreach (x :: Bool) -> f x :~: f x
law _ _ = Refl
#else
law :: forall tag. Proxy tag -> foreach (f :: Bool -> Result tag) -> foreach (x :: Bool) -> f x :~: f x
law _ _ _ = Refl
#endif

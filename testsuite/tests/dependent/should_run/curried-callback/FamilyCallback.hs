{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, TypeFamilies, TypeOperators #-}
module FamilyCallback (Result, law) where

import CompareAt (Result)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))

-- | Keep the callback and its argument unevaluated at a family result.
#ifdef AMBIGUOUS
law :: forall tag. foreach (f :: Bool -> Result tag) -> foreach (x :: Bool) -> f x :~: f x
law _ _ = Refl
#else
law :: forall tag. Proxy tag -> foreach (f :: Bool -> Result tag) -> foreach (x :: Bool) -> f x :~: f x
law _ _ _ = Refl
#endif

#if defined(NONFUNCTION) || defined(STUCK)
#ifdef NONFUNCTION
applyTwice :: foreach (f :: Bool -> Result 'False)
#else
applyTwice :: forall tag. foreach (f :: Bool -> Result tag)
#endif
           -> foreach (x :: Bool) -> foreach (y :: Bool) -> f x y :~: f x y
applyTwice _ _ _ = Refl
#endif

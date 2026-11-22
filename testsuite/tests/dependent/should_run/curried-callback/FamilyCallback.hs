{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, TypeFamilies, TypeOperators, UndecidableInstances #-}
module FamilyCallback (Result, Via, decideVia, law) where

import CompareAt (Result, Witness(..))
import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))

-- | Expose the callback arrow through two family equations.
type family Via (tag :: Bool) :: Type where
    Via tag = Result tag

{-# INLINE decideVia #-}
decideVia :: foreach (cmp :: Bool -> Via 'True)
          -> foreach (x :: Bool) -> foreach (y :: Bool) -> Witness (cmp x y)
#ifdef SWAPPED
decideVia cmp x y = case cmp y x of
#else
decideVia cmp x y = case cmp x y of
#endif
    LT -> Less
    EQ -> Equal
    GT -> Greater

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

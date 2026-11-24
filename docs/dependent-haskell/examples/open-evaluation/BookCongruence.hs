{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, Modifiers #-}
{-# LANGUAGE PolyKinds, RequiredTypeArguments, TypeOperators #-}
-- | Congruence with a required type argument from the PLFH Induction chapter.
module BookCongruence (plusAssoc, addUnder) where

import Data.Type.Equality ((:~:)(Refl))
import GHC.Types (Matchability)
import HaskLib.Nat (Nat(..), plus, plusZero)

-- | Apply the same function to both sides of an equality.
congVis :: forall {a} {b} {m :: Matchability}.
           forall (f :: a %m -> b) -> forall (x :: a) (y :: a).
           x :~: y -> f x :~: f y
congVis _ Refl = Refl

-- | Lift the induction hypothesis under the successor constructor.
plusAssoc :: foreach (m :: Nat) -> foreach (n :: Nat) -> foreach (p :: Nat)
          -> plus (plus m n) p :~: plus m (plus n p)
plusAssoc Zero _ _ = Refl
plusAssoc (Succ m) n p = congVis Succ (plusAssoc m n p)

-- | Lift right identity under addition by the first natural.
addUnder :: foreach (m :: Nat) -> foreach (n :: Nat)
         -> plus m (plus n Zero) :~: plus m n
addUnder m n = congVis (plus m) (plusZero n)

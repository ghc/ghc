{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, Modifiers #-}
{-# LANGUAGE PolyKinds, RequiredTypeArguments, TypeOperators #-}
-- | Congruence with a required type argument from the PLFH Induction chapter.
module BookCongruence (plusAssoc, plusComm, addUnder) where

import Data.Type.Equality ((:~:)(Refl), trans)
import GHC.Types (Matchability)
import HaskLib.Nat (Nat(..), plus, plusSucc, plusZero)

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

-- | Prove commutativity by induction on the second argument.
plusComm :: foreach (m :: Nat) -> foreach (n :: Nat) -> plus m n :~: plus n m
plusComm m Zero = plusZero m
plusComm m (Succ k) = plusSucc m k `trans` congVis Succ (plusComm m k)

-- | Lift right identity under addition by the first natural.
addUnder :: foreach (m :: Nat) -> foreach (n :: Nat)
         -> plus m (plus n Zero) :~: plus m n
addUnder m n = congVis (plus m) (plusZero n)

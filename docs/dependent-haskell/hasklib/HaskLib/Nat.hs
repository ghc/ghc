{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, Modifiers, TypeOperators #-}

-- | Peano naturals and their arithmetic laws.
module HaskLib.Nat (Nat(..), plus, plusZero, plusSucc, congSucc, congFirst) where

import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import PreludeMapLengthSize (Nat(..), plus)

-- | Adding zero on the right preserves the left operand.
plusZero :: foreach (m :: Nat) -> plus m 'Zero :~: m
plusZero Zero = Refl
plusZero (Succ m) = congSucc (plusZero m)

-- | Move a successor from the right operand to the result.
plusSucc :: foreach (m :: Nat) -> foreach (n :: Nat)
         -> plus m ('Succ n) :~: 'Succ (plus m n)
plusSucc Zero _ = Refl
plusSucc (Succ m) n = congSucc (plusSucc m n)

-- | Equal naturals have equal successors.
congSucc :: m :~: n -> 'Succ m :~: 'Succ n
congSucc Refl = Refl

-- | Lift an equality through the first argument of a binary natural number function.
-- Supply the function and its fixed second argument by type application.
congFirst :: forall (f :: Nat %U -> Nat %U -> Nat) r m n.
             m :~: n -> f m r :~: f n r
congFirst Refl = Refl

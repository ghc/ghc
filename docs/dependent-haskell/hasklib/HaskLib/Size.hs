{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, Modifiers, TypeOperators #-}

-- | Arithmetic laws for finite sizes.
module HaskLib.Size (Size(..), eqSize, eqSizeRefl, plus, plusZero, plusSucc, congSucc, congFirst) where

import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import PreludeAppendLengthSource (plus)
import PreludeMapLengthSize (Size(..), eqSize)

-- | Adding zero on the right preserves the left operand.
plusZero :: foreach (m :: Size) -> plus m 'Zero :~: m
plusZero Zero = Refl
plusZero (Succ m) = congSucc (plusZero m)

-- | Move a successor from the right operand to the result.
plusSucc :: foreach (m :: Size) -> foreach (n :: Size)
         -> plus m ('Succ n) :~: 'Succ (plus m n)
plusSucc Zero _ = Refl
plusSucc (Succ m) n = congSucc (plusSucc m n)

-- | Equal sizes have equal successors.
congSucc :: m :~: n -> 'Succ m :~: 'Succ n
congSucc Refl = Refl

-- | Lift an equality through the first argument of a binary size function.
-- Supply the function and its fixed second argument by type application.
congFirst :: forall (f :: Size %U -> Size %U -> Size) r m n.
             m :~: n -> f m r :~: f n r
congFirst Refl = Refl

-- | Comparing a finite size to itself yields 'True'.
eqSizeRefl :: foreach (n :: Size) -> eqSize n n :~: 'True
eqSizeRefl Zero     = Refl
eqSizeRefl (Succ n) = eqSizeRefl n

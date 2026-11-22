{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}

-- | Addition laws for finite sizes.
module HaskLib.Size (Size(..), plus, plusZero, plusSucc) where

import Data.Type.Equality ((:~:)(Refl))
import PreludeAppendLengthSource (plus)
import PreludeMapLengthSize (Size(..))

-- | Adding zero on the right preserves the left operand.
plusZero :: foreach (m :: Size) -> plus m 'Zero :~: m
plusZero Zero = Refl
plusZero (Succ m) = case plusZero m of Refl -> Refl

-- | Move a successor from the right operand to the result.
plusSucc :: foreach (m :: Size) -> foreach (n :: Size)
         -> plus m ('Succ n) :~: 'Succ (plus m n)
plusSucc Zero _ = Refl
plusSucc (Succ m) n = case plusSucc m n of Refl -> Refl

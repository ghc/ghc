{-# LANGUAGE DataKinds, GADTs, Modifiers, NoExperimentalDependentTypes, PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module PreludeFilterLengthProof
    ( AtMost(..), FilterSteps(..), filterLengthBound, keepIfTrue, skipIfFalse
    , showPreludeFilterLength ) where

import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import PreludeFilterSource (FilterWith, Negation)
import qualified PreludeMapLengthSize as S
import PreludeMapLengthSource (Count)

-- | Witness a bound between two Peano sizes.
data AtMost (m :: S.Size) (n :: S.Size) where
    ZeroBound :: AtMost 'S.Zero 'S.Zero
    BothSucc :: AtMost m n -> AtMost ('S.Succ m) ('S.Succ n)
    RightSucc :: AtMost m n -> AtMost m ('S.Succ n)

deriving instance Show (AtMost m n)

-- | Witness a finite list spine and give one checked filter step per head.
data FilterSteps p xs where
    End :: FilterSteps p '[]
    Keep :: FilterWith p (x ': xs) :~: (x ': FilterWith p xs)
         -> FilterSteps p xs -> FilterSteps p (x ': xs)
    Skip :: FilterWith p (x ': xs) :~: FilterWith p xs
         -> FilterSteps p xs -> FilterSteps p (x ': xs)

-- | Bound the size of the installed filter result by the original size.
filterLengthBound :: FilterSteps p xs -> AtMost (Count (FilterWith p xs)) (Count xs)
filterLengthBound End = ZeroBound
filterLengthBound (Keep Refl xs) = BothSucc (filterLengthBound xs)
filterLengthBound (Skip Refl xs) = RightSucc (filterLengthBound xs)

-- | Add a step for a head whose predicate returns true.
keepIfTrue :: forall a (p :: a %U -> Bool) (x :: a) (xs :: [a]).
              p x ~ 'True => FilterSteps p xs -> FilterSteps p (x ': xs)
keepIfTrue = Keep Refl

-- | Add a step for a head whose predicate returns false.
skipIfFalse :: forall a (p :: a %U -> Bool) (x :: a) (xs :: [a]).
               'False ~ p x => FilterSteps p xs -> FilterSteps p (x ': xs)
skipIfFalse = Skip Refl

-- | Observe the proof and count the filtered and original lists.
showPreludeFilterLength :: String
showPreludeFilterLength = show (filterLengthBound steps, S.size (filter not xs), S.size xs)
  where
    xs = [False, True]
    steps :: FilterSteps Negation '[ 'False, 'True]
    steps = keepIfTrue (skipIfFalse End)

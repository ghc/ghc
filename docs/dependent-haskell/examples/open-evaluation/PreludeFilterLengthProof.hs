{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, PolyKinds, StandaloneDeriving, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module PreludeFilterLengthProof
    ( AtMost(..), FilterSteps(..), filterLengthBound, showPreludeFilterLength ) where

import Data.Type.Equality ((:~:)(Refl))
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

-- | Keep a head when the given negation result is true.
keepNegated :: (Negation x ~ 'True) => FilterSteps Negation xs -> FilterSteps Negation (x ': xs)
keepNegated = Keep Refl

-- | Skip a head when the given negation result is false.
skipNegated :: (Negation x ~ 'False) => FilterSteps Negation xs -> FilterSteps Negation (x ': xs)
skipNegated = Skip Refl

-- | Observe the proof and count the filtered and original lists.
showPreludeFilterLength :: String
showPreludeFilterLength = show (filterLengthBound steps, S.size (filter not xs), S.size xs)
  where
    xs = [False, True]
    steps :: FilterSteps Negation '[ 'False, 'True]
    steps = keepNegated (skipNegated End)

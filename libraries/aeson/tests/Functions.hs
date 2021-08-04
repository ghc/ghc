{-# LANGUAGE NoImplicitPrelude #-}

module Functions
    (
      approxEq
    , approxEqWith
    ) where

import Prelude.Compat

approxEq :: (Fractional a, Ord a) => a -> a -> Bool
approxEq = approxEqWith 1e-15 1e-15

approxEqWith :: (Fractional a, Ord a) => a -> a -> a -> a -> Bool
approxEqWith maxAbsoluteError maxRelativeError a b =
    a == b || d < maxAbsoluteError ||
    d / max (abs b) (abs a) <= maxRelativeError
  where d = abs (a - b)

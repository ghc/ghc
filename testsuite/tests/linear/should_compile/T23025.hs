{-# LANGUAGE LinearTypes, BangPatterns #-}
module T23025 where

import Data.Void
import GHC.Types (Multiplicity)

f :: a %1 -> a
f !x = x

g :: Void %(m :: Multiplicity) -> Maybe ()
g a = Just (case a of {})

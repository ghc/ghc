module NativeClosedSumDependenciesInstance (Pick (pick)) where

import Numeric.Natural (Natural)
import NativeClosedSumDependenciesHelper (first)

class Pick a where
    pick :: [a] -> a
    again :: [a] -> a

instance Pick Natural where
    pick = first
    again = first

{-# LANGUAGE TypeInType, TypeOperators, TypeFamilies,
             UndecidableInstances, ConstraintKinds #-}
module DCo_Phantom where

import GHC.TypeLits as L
import Data.Type.Bool


-- We define a very simplistic O notation, with sufficient expressiveness
-- to capture the complexity of a few simple sorting algorithms
data AsympPoly = NLogN Nat Nat

-- Synonyms for common terms
type N     = NLogN 1 0
type LogN  = NLogN 0 1
type One   = NLogN 0 0

-- Just to be able to write it nicely
type O (a :: AsympPoly) = a

type family (^.) (n :: AsympPoly) (m :: Nat) :: AsympPoly where
  (NLogN a b) ^. n = (NLogN (a L.* n) (b L.* n))

infixl 7 ^.

newtype Sorted (cpu :: AsympPoly) -- The minimum operational complexity
                                       -- this algorithm satisfies.
               (mem :: AsympPoly) -- The minimum space complexity this
                                       -- algorithm satisfies.
               (stable :: Bool)        -- Whether the sort is stable or not.
               a                       -- What was being sorted.
               = Sorted {sortedBy :: [a]}

mySortA :: Sorted (O(N^.2)) (O(N)) True Integer
mySortA = _a [3,1,2]

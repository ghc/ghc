{-# LANGUAGE TypeInType, TypeOperators, TypeFamilies,
             UndecidableInstances, ConstraintKinds #-}
module TypeInTypeSubstitutions where

import GHC.TypeLits as L
import Data.Type.Bool
import Data.Type.Equality
import Data.List (sort)


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

type family (*.) (n :: AsympPoly) (m :: AsympPoly) :: AsympPoly where
  (NLogN a b) *. (NLogN c d) = NLogN (a+c) (b+d)

type family OCmp (n :: AsympPoly) (m :: AsympPoly) :: Ordering where
  OCmp (NLogN a b) (NLogN c d) = If (CmpNat a c == EQ)
                                    (CmpNat b d)
                                    (CmpNat a c)

type family OGEq (n :: AsympPoly) (m :: AsympPoly) :: Bool where
  OGEq n m = Not (OCmp n m == 'LT)

type (>=.) n m = OGEq n m ~ True

infix 4 >=.
infixl 7 *., ^.



-- Stable sorts must be stable, but unstable can be, but don't need to.
type IsStable s = (s || True) ~ True
-- We encode in the return type of the sorting function its average complexity,
-- memory use and stability.
newtype Sorted (cpu :: AsympPoly) -- The minimum operational complexity
                                       -- this algorithm satisfies.
               (mem :: AsympPoly) -- The minimum space complexity this
                                       -- algorithm satisfies.
               (stable :: Bool)        -- Whether the sort is stable or not.
               a                       -- What was being sorted.
               = Sorted {sortedBy :: [a]}

-- Merge sort is O(N*Log(N)) on average in complexity, so that's the
-- minimum complexity we promise to satisfy. Same goes with memory, which is
-- O(N), and as we all know, mergesort is a stable sorting algorithm.
mergeSort :: (Ord a, n >=. O(N*.LogN), m >=. O(N), IsStable s) =>
             [a] -> Sorted n m s a
mergeSort = Sorted . sort

insertionSort :: (Ord a, n >=. O(N^.2), m >=. O(One), IsStable s) =>
                 [a] -> Sorted n m s a
insertionSort = Sorted . sort

-- Note that we don't actually check the complexity (as evidenced by them all
-- being implemented with sort, a smooth applicative merge sort). With more
-- dependent types however, some of these properties might be verifiable.
quickSort :: (Ord a, n >=. O(N*.LogN), m >=. O(N)) => [a] -> Sorted n m False a
quickSort = Sorted . sort

heapSort :: (Ord a, n >=. O(N*.LogN), m >=. O(One)) => [a] -> Sorted n m False a
heapSort = Sorted . sort

-- Here we say that sorted can use at most operational complexity O(N^2), space
-- complexity of at most (O(N)) and that it should be stable.
mySortA :: Sorted (O(N^.2)) (O(N)) True Integer
mySortA = _a [3,1,2]

mySortB :: Sorted (O(N*.LogN)) (O(N)) False Integer
mySortB = _b [3,1,2]

mySortC :: Sorted (O(N*.LogN)) (O(One)) False Integer
mySortC = _c [3,1,2]

main :: IO ()
main = print (sortedBy mySortA)

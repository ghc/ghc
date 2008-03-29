module Interval
  ( Interval
  , mkInterval, intervalToInfinityFrom
  , integersInInterval

  , DisjointIntervalSet
  , emptyIntervalSet, extendIntervalSet, deleteFromIntervalSet
  , subIntervals
  ) 
where

import Panic

#include "HsVersions.h"

------------------------------------------------------------------ 
-- Intervals and Sets of Intervals
------------------------------------------------------------------ 

-- This module implements intervals over the integer line and sets of
-- disjoint intervals.  

{-
An interval $[x,y)$ over ordered points represents a half-open
interval of points:  $\{ p \mid x \leq p < y \}$.  Half-open intervals
have the nice property $[x,y) \cup [y,z) = [x,z)$.  Non-empty
intervals can precede or overlap each other; an empty interval never
overlaps or precedes any other.  The set of ordered elements contains
a unique element $\mathit{zero}$; using it in any interval is an
\emph{unchecked} run-time error.
-}


data Interval = Interval { i_min :: Int, i_lim :: Int }
  -- width == i_lim - i_min >= 0

type Width = Int

mkInterval :: Int -> Width -> Interval
mkInterval min w = ASSERT (w>=0) Interval min (min+w)
intervalToInfinityFrom :: Int -> Interval
intervalToInfinityFrom min = Interval min maxBound
integersInInterval :: Interval -> [Int]
integersInInterval (Interval min lim) = gen min lim
    where gen min lim | min >= lim = []
                      | otherwise = min : gen (min+1) lim

precedes, overlaps, adjoins, contains :: Interval -> Interval -> Bool
precedes (Interval m l) (Interval m' l') = l <= m' || l' <= m
overlaps i i' = not (i `precedes` i' || i' `precedes` i)
adjoins (Interval _ l) (Interval m _) = l == m
contains (Interval m l) (Interval m' l') = m <= m' && l >= l'

merge :: Interval -> Interval -> Interval
merge _i@(Interval m _) _i'@(Interval _ l) = {- ASSERT (adjoins i i') -} (Interval m l)


----------


newtype DisjointIntervalSet = Intervals [Interval]
 -- invariants: * No two intervals overlap
 --             * Adjacent intervals have a gap between
 --             * Intervals are sorted by min element

emptyIntervalSet :: DisjointIntervalSet
emptyIntervalSet = Intervals []
extendIntervalSet :: DisjointIntervalSet -> Interval -> DisjointIntervalSet
extendIntervalSet (Intervals l) i = Intervals (insert [] i l)
    where insert :: [Interval] -> Interval -> [Interval] -> [Interval]
          -- precondition: in 'insert prev' i l', every element of prev'
          -- precedes and does not adjoin i
          insert prev' i [] = rev_app prev' [i]
          insert prev' i (i':is) =
                 if i `precedes` i' then
                     if i `adjoins` i' then
                         insert prev' (merge i i') is
                     else
                         rev_app prev' (i : i' : is)
                 else if i' `precedes` i then
                          if i' `adjoins` i then
                              insert prev' (merge i' i) is
                          else
                              insert (i' : prev') i is
                      else
                          panic "overlapping intervals"

deleteFromIntervalSet :: DisjointIntervalSet -> Interval -> DisjointIntervalSet
deleteFromIntervalSet (Intervals l) i = Intervals (rm [] i l)
    where rm :: [Interval] -> Interval -> [Interval] -> [Interval]
          -- precondition: in 'rm prev' i l', every element of prev'
          -- precedes and does not adjoin i
          rm _ _ [] = panic "removed interval not present in set"
          rm prev' i (i':is) =
                 if i `precedes` i' then
                     panic "removed interval not present in set"
                 else if i' `precedes` i then
                          rm (i' : prev') i is
                      else
                          -- remove i from i', leaving 0, 1, or 2 leftovers
                          undefined {-
                          ASSERTX (i' `contains` i)
                          let (Interval m l, Interval m' l'
                          panic "overlapping intervals"
                                     -}

subIntervals :: DisjointIntervalSet -> Width -> [Interval]
subIntervals = undefined

rev_app :: [a] -> [a] -> [a]
rev_app [] xs = xs
rev_app (y:ys) xs = rev_app ys (y:xs)

_unused :: ()
_unused = undefined i_min i_lim overlaps contains


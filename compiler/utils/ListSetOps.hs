{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[ListSetOps]{Set-like operations on lists}
-}

{-# LANGUAGE CPP #-}

module ListSetOps (
        unionLists, minusList, deleteBys,

        -- Association lists
        Assoc, assoc, assocMaybe, assocUsing, assocDefault, assocDefaultUsing,

        -- Duplicate handling
        hasNoDups, removeDups, findDupsEq,
        equivClasses,

        -- Indexing
        getNth
   ) where

#include "HsVersions.h"

import GhcPrelude

import Outputable
import Util

import Data.List
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Set as S

getNth :: Outputable a => [a] -> Int -> a
getNth xs n = ASSERT2( xs `lengthExceeds` n, ppr n $$ ppr xs )
             xs !! n

deleteBys :: (a -> a -> Bool) -> [a] -> [a] -> [a]
-- (deleteBys eq xs ys) returns xs-ys, using the given equality function
-- Just like 'Data.List.delete' but with an equality function
deleteBys eq xs ys = foldl' (flip (deleteBy eq)) xs ys

{-
************************************************************************
*                                                                      *
        Treating lists as sets
        Assumes the lists contain no duplicates, but are unordered
*                                                                      *
************************************************************************
-}


unionLists :: (Outputable a, Eq a) => [a] -> [a] -> [a]
-- Assumes that the arguments contain no duplicates
unionLists xs ys
  = WARN(lengthExceeds xs 100 || lengthExceeds ys 100, ppr xs $$ ppr ys)
    [x | x <- xs, isn'tIn "unionLists" x ys] ++ ys

-- | Calculate the set difference of two lists. This is
-- /O((m + n) log n)/, where we subtract a list of /n/ elements
-- from a list of /m/ elements.
--
-- Extremely short cases are handled specially:
-- When /m/ or /n/ is 0, this takes /O(1)/ time. When /m/ is 1,
-- it takes /O(n)/ time.
minusList :: Ord a => [a] -> [a] -> [a]
-- There's no point building a set to perform just one lookup, so we handle
-- extremely short lists specially. It might actually be better to use
-- an O(m*n) algorithm when m is a little longer (perhaps up to 4 or even 5).
-- The tipping point will be somewhere in the area of where /m/ and /log n/
-- become comparable, but we probably don't want to work too hard on this.
minusList [] _ = []
minusList xs@[x] ys
  | x `elem` ys = []
  | otherwise = xs
-- Using an empty set or a singleton would also be silly, so let's not.
minusList xs [] = xs
minusList xs [y] = filter (/= y) xs
-- When each list has at least two elements, we build a set from the
-- second argument, allowing us to filter the first argument fairly
-- efficiently.
minusList xs ys = filter (`S.notMember` yss) xs
  where
    yss = S.fromList ys

{-
************************************************************************
*                                                                      *
\subsection[Utils-assoc]{Association lists}
*                                                                      *
************************************************************************

Inefficient finite maps based on association lists and equality.
-}

-- A finite mapping based on equality and association lists
type Assoc a b = [(a,b)]

assoc             :: (Eq a) => String -> Assoc a b -> a -> b
assocDefault      :: (Eq a) => b -> Assoc a b -> a -> b
assocUsing        :: (a -> a -> Bool) -> String -> Assoc a b -> a -> b
assocMaybe        :: (Eq a) => Assoc a b -> a -> Maybe b
assocDefaultUsing :: (a -> a -> Bool) -> b -> Assoc a b -> a -> b

assocDefaultUsing _  deflt []             _   = deflt
assocDefaultUsing eq deflt ((k,v) : rest) key
  | k `eq` key = v
  | otherwise  = assocDefaultUsing eq deflt rest key

assoc crash_msg         list key = assocDefaultUsing (==) (panic ("Failed in assoc: " ++ crash_msg)) list key
assocDefault deflt      list key = assocDefaultUsing (==) deflt list key
assocUsing eq crash_msg list key = assocDefaultUsing eq (panic ("Failed in assoc: " ++ crash_msg)) list key

assocMaybe alist key
  = lookup alist
  where
    lookup []             = Nothing
    lookup ((tv,ty):rest) = if key == tv then Just ty else lookup rest

{-
************************************************************************
*                                                                      *
\subsection[Utils-dups]{Duplicate-handling}
*                                                                      *
************************************************************************
-}

hasNoDups :: (Eq a) => [a] -> Bool

hasNoDups xs = f [] xs
  where
    f _           []     = True
    f seen_so_far (x:xs) = if x `is_elem` seen_so_far
                           then False
                           else f (x:seen_so_far) xs

    is_elem = isIn "hasNoDups"

equivClasses :: (a -> a -> Ordering) -- Comparison
             -> [a]
             -> [NonEmpty a]

equivClasses _   []      = []
equivClasses _   [stuff] = [stuff :| []]
equivClasses cmp items   = NE.groupBy eq (sortBy cmp items)
  where
    eq a b = case cmp a b of { EQ -> True; _ -> False }

removeDups :: (a -> a -> Ordering) -- Comparison function
           -> [a]
           -> ([a],          -- List with no duplicates
               [NonEmpty a]) -- List of duplicate groups.  One representative
                             -- from each group appears in the first result

removeDups _   []  = ([], [])
removeDups _   [x] = ([x],[])
removeDups cmp xs
  = case (mapAccumR collect_dups [] (equivClasses cmp xs)) of { (dups, xs') ->
    (xs', dups) }
  where
    collect_dups :: [NonEmpty a] -> NonEmpty a -> ([NonEmpty a], a)
    collect_dups dups_so_far (x :| [])     = (dups_so_far,      x)
    collect_dups dups_so_far dups@(x :| _) = (dups:dups_so_far, x)

findDupsEq :: (a->a->Bool) -> [a] -> [NonEmpty a]
findDupsEq _  [] = []
findDupsEq eq (x:xs) | null eq_xs  = findDupsEq eq xs
                     | otherwise   = (x :| eq_xs) : findDupsEq eq neq_xs
    where (eq_xs, neq_xs) = partition (eq x) xs

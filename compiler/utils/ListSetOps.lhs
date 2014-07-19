%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[ListSetOps]{Set-like operations on lists}

\begin{code}
{-# LANGUAGE CPP #-}

module ListSetOps (
        unionLists, minusList, insertList,

        -- Association lists
        Assoc, assoc, assocMaybe, assocUsing, assocDefault, assocDefaultUsing,

        -- Duplicate handling
        hasNoDups, runs, removeDups, findDupsEq,
        equivClasses, equivClassesByUniq,

        -- Indexing
        getNth
   ) where

#include "HsVersions.h"

import Outputable
import Unique
import UniqFM
import Util

import Data.List
\end{code}

---------
#ifndef DEBUG
getNth :: [a] -> Int -> a
getNth xs n = xs !! n
#else
getNth :: Outputable a => [a] -> Int -> a
getNth xs n = ASSERT2( xs `lengthAtLeast` n, ppr n $$ ppr xs )
              xs !! n
#endif
----------
\begin{code}
getNth :: Outputable a => [a] -> Int -> a
getNth xs n = ASSERT2( xs `lengthExceeds` n, ppr n $$ ppr xs )
              xs !! n
\end{code}

%************************************************************************
%*                                                                      *
        Treating lists as sets
        Assumes the lists contain no duplicates, but are unordered
%*                                                                      *
%************************************************************************

\begin{code}
insertList :: Eq a => a -> [a] -> [a]
-- Assumes the arg list contains no dups; guarantees the result has no dups
insertList x xs | isIn "insert" x xs = xs
                | otherwise          = x : xs

unionLists :: (Outputable a, Eq a) => [a] -> [a] -> [a]
-- Assumes that the arguments contain no duplicates
unionLists xs ys 
  = WARN(length xs > 100 || length ys > 100, ppr xs $$ ppr ys)
    [x | x <- xs, isn'tIn "unionLists" x ys] ++ ys

minusList :: (Eq a) => [a] -> [a] -> [a]
-- Everything in the first list that is not in the second list:
minusList xs ys = [ x | x <- xs, isn'tIn "minusList" x ys]
\end{code}


%************************************************************************
%*                                                                      *
\subsection[Utils-assoc]{Association lists}
%*                                                                      *
%************************************************************************

Inefficient finite maps based on association lists and equality.

\begin{code}
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
\end{code}

%************************************************************************
%*                                                                      *
\subsection[Utils-dups]{Duplicate-handling}
%*                                                                      *
%************************************************************************

\begin{code}
hasNoDups :: (Eq a) => [a] -> Bool

hasNoDups xs = f [] xs
  where
    f _           []     = True
    f seen_so_far (x:xs) = if x `is_elem` seen_so_far
                           then False
                           else f (x:seen_so_far) xs

    is_elem = isIn "hasNoDups"
\end{code}

\begin{code}
equivClasses :: (a -> a -> Ordering) -- Comparison
             -> [a]
             -> [[a]]

equivClasses _         []  = []
equivClasses _   stuff@[_] = [stuff]
equivClasses cmp items     = runs eq (sortBy cmp items)
  where
    eq a b = case cmp a b of { EQ -> True; _ -> False }
\end{code}

The first cases in @equivClasses@ above are just to cut to the point
more quickly...

@runs@ groups a list into a list of lists, each sublist being a run of
identical elements of the input list. It is passed a predicate @p@ which
tells when two elements are equal.

\begin{code}
runs :: (a -> a -> Bool) -- Equality
     -> [a]
     -> [[a]]

runs _ []     = []
runs p (x:xs) = case (span (p x) xs) of
                (first, rest) -> (x:first) : (runs p rest)
\end{code}

\begin{code}
removeDups :: (a -> a -> Ordering) -- Comparison function
           -> [a]
           -> ([a],     -- List with no duplicates
               [[a]])   -- List of duplicate groups.  One representative from
                        -- each group appears in the first result

removeDups _   []  = ([], [])
removeDups _   [x] = ([x],[])
removeDups cmp xs
  = case (mapAccumR collect_dups [] (equivClasses cmp xs)) of { (dups, xs') ->
    (xs', dups) }
  where
    collect_dups _           []         = panic "ListSetOps: removeDups"
    collect_dups dups_so_far [x]        = (dups_so_far,      x)
    collect_dups dups_so_far dups@(x:_) = (dups:dups_so_far, x)

findDupsEq :: (a->a->Bool) -> [a] -> [[a]]
findDupsEq _  [] = []
findDupsEq eq (x:xs) | null eq_xs  = findDupsEq eq xs
                     | otherwise   = (x:eq_xs) : findDupsEq eq neq_xs
    where (eq_xs, neq_xs) = partition (eq x) xs
\end{code}


\begin{code}
equivClassesByUniq :: (a -> Unique) -> [a] -> [[a]]
        -- NB: it's *very* important that if we have the input list [a,b,c],
        -- where a,b,c all have the same unique, then we get back the list
        --      [a,b,c]
        -- not
        --      [c,b,a]
        -- Hence the use of foldr, plus the reversed-args tack_on below
equivClassesByUniq get_uniq xs
  = eltsUFM (foldr add emptyUFM xs)
  where
    add a ufm = addToUFM_C tack_on ufm (get_uniq a) [a]
    tack_on old new = new++old
\end{code}


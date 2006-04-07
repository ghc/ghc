%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[ListSetOps]{Set-like operations on lists}

\begin{code}
module ListSetOps (
	unionLists, minusList, insertList,

	-- Association lists
	Assoc, assoc, assocMaybe, assocUsing, assocDefault, assocDefaultUsing,
	emptyAssoc, unitAssoc, mapAssoc, plusAssoc_C, extendAssoc_C,
	mkLookupFun, findInList, assocElts,

	-- Duplicate handling
	hasNoDups, runs, removeDups, findDupsEq, 
	equivClasses, equivClassesByUniq

   ) where

#include "HsVersions.h"

import Outputable
import Unique	( Unique )
import UniqFM	( eltsUFM, emptyUFM, addToUFM_C )
import Util	( isn'tIn, isIn, mapAccumR, sortLe )
import List	( partition )
\end{code}


%************************************************************************
%*									*
	Treating lists as sets
	Assumes the lists contain no duplicates, but are unordered
%*									*
%************************************************************************

\begin{code}
insertList :: Eq a => a -> [a] -> [a]
-- Assumes the arg list contains no dups; guarantees the result has no dups
insertList x xs | isIn "insert" x xs = xs
	    | otherwise		 = x : xs

unionLists :: (Eq a) => [a] -> [a] -> [a]
-- Assumes that the arguments contain no duplicates
unionLists xs ys = [x | x <- xs, isn'tIn "unionLists" x ys] ++ ys

minusList :: (Eq a) => [a] -> [a] -> [a]
-- Everything in the first list that is not in the second list:
minusList xs ys = [ x | x <- xs, isn'tIn "minusList" x ys]
\end{code}


%************************************************************************
%*									*
\subsection[Utils-assoc]{Association lists}
%*									*
%************************************************************************

Inefficient finite maps based on association lists and equality.

\begin{code}
type Assoc a b = [(a,b)]	-- A finite mapping based on equality and association lists

emptyAssoc	  :: Assoc a b
unitAssoc	  :: a -> b -> Assoc a b
assocElts	  :: Assoc a b -> [(a,b)]
assoc		  :: (Eq a) => String -> Assoc a b -> a -> b
assocDefault	  :: (Eq a) => b -> Assoc a b -> a -> b
assocUsing	  :: (a -> a -> Bool) -> String -> Assoc a b -> a -> b
assocMaybe	  :: (Eq a) => Assoc a b -> a -> Maybe b
assocDefaultUsing :: (a -> a -> Bool) -> b -> Assoc a b -> a -> b
mapAssoc	  :: (b -> c) -> Assoc a b -> Assoc a c
extendAssoc_C	  :: (Eq a) => (b -> b -> b) -> Assoc a b -> (a,b)     -> Assoc a b
plusAssoc_C	  :: (Eq a) => (b -> b -> b) -> Assoc a b -> Assoc a b -> Assoc a b
	-- combining fn takes (old->new->result)

emptyAssoc    = []
unitAssoc a b = [(a,b)]
assocElts xs  = xs

assocDefaultUsing eq deflt ((k,v) : rest) key
  | k `eq` key = v
  | otherwise  = assocDefaultUsing eq deflt rest key

assocDefaultUsing eq deflt [] key = deflt

assoc crash_msg         list key = assocDefaultUsing (==) (panic ("Failed in assoc: " ++ crash_msg)) list key
assocDefault deflt      list key = assocDefaultUsing (==) deflt list key
assocUsing eq crash_msg list key = assocDefaultUsing eq (panic ("Failed in assoc: " ++ crash_msg)) list key

assocMaybe alist key
  = lookup alist
  where
    lookup []		  = Nothing
    lookup ((tv,ty):rest) = if key == tv then Just ty else lookup rest

mapAssoc f alist = [(key, f val) | (key,val) <- alist]

plusAssoc_C combine []  new = new	-- Shortcut for common case
plusAssoc_C combine old new = foldl (extendAssoc_C combine) old new

extendAssoc_C combine old_list (new_key, new_val)
  = go old_list
  where
    go [] = [(new_key, new_val)]
    go ((old_key, old_val) : old_list) 
	| new_key == old_key = ((old_key, old_val `combine` new_val) : old_list)
	| otherwise	     = (old_key, old_val) : go old_list
\end{code}


@mkLookupFun eq alist@ is a function which looks up
its argument in the association list @alist@, returning a Maybe type.
@mkLookupFunDef@ is similar except that it is given a value to return
on failure.

\begin{code}
mkLookupFun :: (key -> key -> Bool)	-- Equality predicate
	    -> [(key,val)] 		-- The assoc list
	    -> key 			-- The key
	    -> Maybe val		-- The corresponding value

mkLookupFun eq alist s
  = case [a | (s',a) <- alist, s' `eq` s] of
      []    -> Nothing
      (a:_) -> Just a

findInList :: (a -> Bool) -> [a] -> Maybe a
findInList p [] = Nothing
findInList p (x:xs) | p x	= Just x
		    | otherwise = findInList p xs
\end{code}


%************************************************************************
%*									*
\subsection[Utils-dups]{Duplicate-handling}
%*									*
%************************************************************************

\begin{code}
hasNoDups :: (Eq a) => [a] -> Bool

hasNoDups xs = f [] xs
  where
    f seen_so_far []     = True
    f seen_so_far (x:xs) = if x `is_elem` seen_so_far then
				False
			   else
				f (x:seen_so_far) xs

    is_elem = isIn "hasNoDups"
\end{code}

\begin{code}
equivClasses :: (a -> a -> Ordering) 	-- Comparison
	     -> [a]
	     -> [[a]]

equivClasses cmp stuff@[]     = []
equivClasses cmp stuff@[item] = [stuff]
equivClasses cmp items
  = runs eq (sortLe le items)
  where
    eq a b = case cmp a b of { EQ -> True; _ -> False }
    le a b = case cmp a b of { LT -> True; EQ -> True; GT -> False }
\end{code}

The first cases in @equivClasses@ above are just to cut to the point
more quickly...

@runs@ groups a list into a list of lists, each sublist being a run of
identical elements of the input list. It is passed a predicate @p@ which
tells when two elements are equal.

\begin{code}
runs :: (a -> a -> Bool) 	-- Equality
     -> [a]
     -> [[a]]

runs p []     = []
runs p (x:xs) = case (span (p x) xs) of
		  (first, rest) -> (x:first) : (runs p rest)
\end{code}

\begin{code}
removeDups :: (a -> a -> Ordering) 	-- Comparison function
	   -> [a]
	   -> ([a], 	-- List with no duplicates
	       [[a]])	-- List of duplicate groups.  One representative from
			-- each group appears in the first result

removeDups cmp []  = ([], [])
removeDups cmp [x] = ([x],[])
removeDups cmp xs
  = case (mapAccumR collect_dups [] (equivClasses cmp xs)) of { (dups, xs') ->
    (xs', dups) }
  where
    collect_dups dups_so_far [x]         = (dups_so_far,      x)
    collect_dups dups_so_far dups@(x:xs) = (dups:dups_so_far, x)

findDupsEq :: (a->a->Bool) -> [a] -> [[a]]
findDupsEq eq [] = []
findDupsEq eq (x:xs) | null eq_xs  = findDupsEq eq xs
		     | otherwise   = (x:eq_xs) : findDupsEq eq neq_xs
		     where
		       (eq_xs, neq_xs) = partition (eq x) xs
\end{code}


\begin{code}
equivClassesByUniq :: (a -> Unique) -> [a] -> [[a]]
	-- NB: it's *very* important that if we have the input list [a,b,c],
	-- where a,b,c all have the same unique, then we get back the list
	-- 	[a,b,c]
	-- not
	--	[c,b,a]
	-- Hence the use of foldr, plus the reversed-args tack_on below
equivClassesByUniq get_uniq xs
  = eltsUFM (foldr add emptyUFM xs)
  where
    add a ufm = addToUFM_C tack_on ufm (get_uniq a) [a]
    tack_on old new = new++old
\end{code}



{-# LANGUAGE CPP #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}



-- | Set-like operations on lists
--
-- Avoid using them as much as possible
module GHC.Data.List.SetOps (
        unionLists, minusList,

        -- Association lists
        Assoc, assoc, assocMaybe, assocUsing, assocDefault, assocDefaultUsing,

        -- Duplicate handling
        hasNoDups, removeDups, nubOrdBy, findDupsEq,
        equivClasses,

        -- Indexing
        getNth,

        -- Membership
        isIn, isn'tIn,
   ) where

import GHC.Prelude

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc
import GHC.Utils.Trace

import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Set as S

getNth :: Outputable a => [a] -> Int -> a
getNth xs n = assertPpr (xs `lengthExceeds` n) (ppr n $$ ppr xs) $
             xs !! n

{-
************************************************************************
*                                                                      *
        Treating lists as sets
        Assumes the lists contain no duplicates, but are unordered
*                                                                      *
************************************************************************
-}


-- | Assumes that the arguments contain no duplicates
unionLists :: (HasDebugCallStack, Outputable a, Eq a) => [a] -> [a] -> [a]
-- We special case some reasonable common patterns.
unionLists xs [] = xs
unionLists [] ys = ys
unionLists [x] ys
  | isIn "unionLists" x ys = ys
  | otherwise = x:ys
unionLists xs [y]
  | isIn "unionLists" y xs = xs
  | otherwise = y:xs
unionLists xs ys
  = warnPprTrace (lengthExceeds xs 100 || lengthExceeds ys 100) "unionLists" (ppr xs $$ ppr ys) $
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

-- | A finite mapping based on equality and association lists.
type Assoc a b = [(a,b)]

assoc             :: (Eq a) => String -> Assoc a b -> a -> b
assocDefault      :: (Eq a) => b -> Assoc a b -> a -> b
assocUsing        :: (a -> a -> Bool) -> String -> Assoc a b -> a -> b
-- | Lookup key, fail gracefully using Nothing if not found.
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
equivClasses cmp items   = NE.groupBy eq (L.sortBy cmp items)
  where
    eq a b = case cmp a b of { EQ -> True; _ -> False }

-- | Remove the duplicates from a list using the provided
-- comparison function.
--
-- Returns the list without duplicates, and accumulates
-- all the duplicates in the second component of its result.
removeDups :: (a -> a -> Ordering) -- Comparison function
           -> [a]
           -> ([a],          -- List with no duplicates
               [NonEmpty a]) -- List of duplicate groups.  One representative
                             -- from each group appears in the first result

removeDups _   []  = ([], [])
removeDups _   [x] = ([x],[])
removeDups cmp xs
  = case L.mapAccumR collect_dups [] (equivClasses cmp xs) of { (dups, xs') ->
    (xs', dups) }
  where
    collect_dups :: [NonEmpty a] -> NonEmpty a -> ([NonEmpty a], a)
    collect_dups dups_so_far (x :| [])     = (dups_so_far,      x)
    collect_dups dups_so_far dups@(x :| _) = (dups:dups_so_far, x)

-- | Remove the duplicates from a list using the provided
-- comparison function.
nubOrdBy :: (a -> a -> Ordering) -> [a] -> [a]
nubOrdBy cmp xs = fst (removeDups cmp xs)

findDupsEq :: (a->a->Bool) -> [a] -> [NonEmpty a]
findDupsEq _  [] = []
findDupsEq eq (x:xs) | L.null eq_xs  = findDupsEq eq xs
                     | otherwise     = (x :| eq_xs) : findDupsEq eq neq_xs
    where (eq_xs, neq_xs) = L.partition (eq x) xs

-- Debugging/specialising versions of \tr{elem} and \tr{notElem}

# if !defined(DEBUG)
isIn, isn'tIn :: Eq a => String -> a -> [a] -> Bool
isIn    _msg x ys = x `elem` ys
isn'tIn _msg x ys = x `notElem` ys

# else /* DEBUG */
isIn, isn'tIn :: (HasDebugCallStack, Eq a) => String -> a -> [a] -> Bool
isIn msg x ys
  = elem100 0 x ys
  where
    elem100 :: Eq a => Int -> a -> [a] -> Bool
    elem100 _ _ [] = False
    elem100 i x (y:ys)
      | i > 100 = warnPprTrace True ("Over-long elem in " ++ msg) empty (x `elem` (y:ys))
      | otherwise = x == y || elem100 (i + 1) x ys

isn'tIn msg x ys
  = notElem100 0 x ys
  where
    notElem100 :: Eq a => Int -> a -> [a] -> Bool
    notElem100 _ _ [] =  True
    notElem100 i x (y:ys)
      | i > 100 = warnPprTrace True ("Over-long notElem in " ++ msg) empty (x `notElem` (y:ys))
      | otherwise = x /= y && notElem100 (i + 1) x ys
# endif /* DEBUG */

%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[ListSetOps]{Set-like operations on lists}

\begin{code}
module ListSetOps (
	unionLists,
	intersectLists,
	minusList
#if ! defined(COMPILING_GHC)
	, disjointLists, intersectingLists
#endif
   ) where

#if defined(COMPILING_GHC)
import Ubiq{-uitous-}

import Util	( isIn, isn'tIn )
#endif
\end{code}

\begin{code}
unionLists :: (Eq a) => [a] -> [a] -> [a]
unionLists []     []		= []
unionLists []     b		= b
unionLists a	   []		= a
unionLists (a:as) b
  | a `is_elem` b = unionLists as b
  | otherwise     = a : unionLists as b
  where
#if defined(COMPILING_GHC)
    is_elem = isIn "unionLists"
#else
    is_elem = elem
#endif

intersectLists :: (Eq a) => [a] -> [a] -> [a]
intersectLists []     []		= []
intersectLists []     b			= []
intersectLists a      []		= []
intersectLists (a:as) b
  | a `is_elem` b = a : intersectLists as b
  | otherwise	  = intersectLists as b
  where
#if defined(COMPILING_GHC)
    is_elem = isIn "intersectLists"
#else
    is_elem = elem
#endif
\end{code}

Everything in the first list that is not in the second list:
\begin{code}
minusList :: (Eq a) => [a] -> [a] -> [a]
minusList xs ys = [ x | x <- xs, x `not_elem` ys]
  where
#if defined(COMPILING_GHC)
    not_elem = isn'tIn "minusList"
#else
    not_elem = notElem
#endif
\end{code}

\begin{code}
#if ! defined(COMPILING_GHC)

disjointLists, intersectingLists :: Eq a => [a] -> [a] -> Bool

disjointLists []     bs = True
disjointLists (a:as) bs
  | a `elem` bs = False
  | otherwise   = disjointLists as bs

intersectingLists xs ys = not (disjointLists xs ys)
#endif
\end{code}

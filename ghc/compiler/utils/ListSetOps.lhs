%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[ListSetOps]{Set-like operations on lists}

\begin{code}
module ListSetOps (
	unionLists,
	--UNUSED: intersectLists,
	minusList

   ) where

#include "HsVersions.h"

import Util	( isIn, isn'tIn )
import List	( union )
\end{code}

\begin{code}
unionLists :: (Eq a) => [a] -> [a] -> [a]
#ifdef REALLY_HASKELL_1_3
unionLists = union
#else
unionLists []     []		= []
unionLists []     b		= b
unionLists a	   []		= a
unionLists (a:as) b
  | a `is_elem` b = unionLists as b
  | otherwise     = a : unionLists as b
  where
    is_elem = isIn "unionLists"
#endif

{- UNUSED
intersectLists :: (Eq a) => [a] -> [a] -> [a]
intersectLists []     []		= []
intersectLists []     b			= []
intersectLists a      []		= []
intersectLists (a:as) b
  | a `is_elem` b = a : intersectLists as b
  | otherwise	  = intersectLists as b
  where
    is_elem = isIn "intersectLists"
-}
\end{code}

Everything in the first list that is not in the second list:
\begin{code}
minusList :: (Eq a) => [a] -> [a] -> [a]
minusList xs ys = [ x | x <- xs, x `not_elem` ys]
  where
    not_elem = isn'tIn "minusList"

\end{code}

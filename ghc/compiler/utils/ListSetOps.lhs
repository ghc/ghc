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

import Util	( isn'tIn )
import List	( union )
\end{code}

\begin{code}
unionLists :: (Eq a) => [a] -> [a] -> [a]
unionLists = union
\end{code}

Everything in the first list that is not in the second list:
\begin{code}
minusList :: (Eq a) => [a] -> [a] -> [a]
minusList xs ys = [ x | x <- xs, x `not_elem` ys]
  where
    not_elem = isn'tIn "minusList"

\end{code}

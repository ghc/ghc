%
% (c) The GRASP Project, Glasgow University, 1992-1998
%
\section{Panic error messages}

Defines basic funtions for printing error messages.

It's hard to put these functions anywhere else without causing
some unnecessary loops in the module dependency graph.

\begin{code}
module Panic ( panic, panic#, assertPanic, trace ) where

import GlaExts( trace )

#include "HsVersions.h"
\end{code}

\begin{code}
panic :: String -> a
panic x = error ("panic! (the `impossible' happened):\n\t"
	      ++ x ++ "\n\n"
	      ++ "Please report it as a compiler bug "
	      ++ "to glasgow-haskell-bugs@dcs.gla.ac.uk.\n\n" )

-- #-versions because panic can't return an unboxed int, and that's
-- what TAG_ is with GHC at the moment.  Ugh. (Simon)
-- No, man -- Too Beautiful! (Will)

panic# :: String -> FAST_INT
panic# s = case (panic s) of () -> ILIT(0)

assertPanic :: String -> Int -> a
assertPanic file line = panic ("ASSERT failed! file " ++ file ++ ", line " ++ show line)
\end{code}

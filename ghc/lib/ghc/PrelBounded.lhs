%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[PrelBounded]{Module @PrelBounded@}

Instances of Bounded for various datatypes.

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelBounded where

import PrelBase

instance Bounded () where
    minBound = ()
    maxBound = ()

instance  Bounded Char  where
    minBound =  '\0'
    maxBound =  '\255'

instance  Bounded Int where
    minBound =  -2147483648		-- GHC <= 2.09 had this at -2147483647
    maxBound =   2147483647
\end{code}

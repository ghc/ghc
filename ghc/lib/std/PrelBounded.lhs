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

\end{code}

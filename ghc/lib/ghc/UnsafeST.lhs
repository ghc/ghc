%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[UnsafeST]{Unsafe ST operations}

VERY IMPORTANT!  This module must be compiled without "-O".  If you
compile it with "-O" then the inlinings of the unsafe ST operators are exposed.
It turns out that exposing these inlininings can lead to unsound transformations,
such as generating a MutVar only once rather than once each call to unsafePerformIO.

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}
\end{code}


\begin{code}
module UnsafeST(
	unsafeInterleaveST,
	unsafePerformPrimIO,
	unsafeInterleavePrimIO
  )  where

import STBase
import PrelBase
import GHC


unsafeInterleaveST :: ST s a -> ST s a
unsafeInterleaveST (ST m) = ST $ \ s ->
    let
	(r, new_s) = m s
    in
    (r, s)

unsafePerformPrimIO	:: PrimIO a -> a
	-- We give a fresh definition here.  There are no
	-- magical universal types kicking around.
unsafePerformPrimIO (ST m)
  = case m (S# realWorld#) of
      (r,_) -> r

unsafeInterleavePrimIO	:: PrimIO a -> PrimIO a
unsafeInterleavePrimIO	= unsafeInterleaveST
\end{code}


% ------------------------------------------------------------------------------
% $Id: Weak.lhs,v 1.2 2001/07/03 14:13:32 simonmar Exp $
%
% (c) The University of Glasgow, 1998-2000
%

\section[GHC.Weak]{Module @GHC.Weak@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module GHC.Weak where

import GHC.Prim
import GHC.Base
import Data.Maybe
import GHC.IOBase	( IO(..), unIO )

data Weak v = Weak (Weak# v)

mkWeak  :: k				-- key
	-> v				-- value
	-> Maybe (IO ())		-- finalizer
	-> IO (Weak v)			-- weak pointer

mkWeak key val (Just finalizer) = IO $ \s ->
   case mkWeak# key val finalizer s of { (# s1, w #) -> (# s1, Weak w #) }
mkWeak key val Nothing = IO $ \s ->
   case mkWeak# key val (unsafeCoerce# 0#) s of { (# s1, w #) -> (# s1, Weak w #) }

mkWeakPtr :: k -> Maybe (IO ()) -> IO (Weak k)
mkWeakPtr key finalizer = mkWeak key key finalizer

addFinalizer :: key -> IO () -> IO ()
addFinalizer key finalizer = do
   mkWeakPtr key (Just finalizer)	-- throw it away
   return ()

{-
Instance Eq (Weak v) where
  (Weak w1) == (Weak w2) = w1 `sameWeak#` w2
-}


-- run a batch of finalizers from the garbage collector.  We're given 
-- an array of finalizers and the length of the array, and we just
-- call each one in turn.
--
-- the IO primitives are inlined by hand here to get the optimal
-- code (sigh) --SDM.

runFinalizerBatch :: Int -> Array# (IO ()) -> IO ()
runFinalizerBatch (I# n) arr = 
   let  go m  = IO $ \s ->
		  case m of 
		  0# -> (# s, () #)
		  _  -> let m' = m -# 1# in
			case indexArray# arr m' of { (# io #) -> 
			case unIO io s of	   { (# s, _ #) -> 
			unIO (go m') s
			}}
   in
        go n

\end{code}

%
% (c) The AQUA Project, Glasgow University, 1998
%

\section[PrelWeak]{Module @PrelWeak@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelWeak where

import PrelGHC
import PrelBase
import PrelMaybe
-- NOTE: To break a cycle, ForeignObj is not in PrelForeign, but PrelIOBase!
import PrelIOBase	( IO(..), unIO, ForeignObj(..) )

#ifndef __PARALLEL_HASKELL__

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

addForeignFinalizer :: ForeignObj -> IO () -> IO ()
addForeignFinalizer (ForeignObj fo) finalizer = addFinalizer fo finalizer

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

#endif

\end{code}

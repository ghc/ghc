%
% (c) The AQUA Project, Glasgow University, 1998
%

\section[PrelWeak]{Module @PrelWeak@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

#ifndef __PARALLEL_HASKELL__

module PrelWeak where

import PrelGHC
import PrelBase
import PrelMaybe
import PrelIOBase
import PrelForeign

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
instance Eq (Weak v) where
  (Weak w1) == (Weak w2) = w1 `sameWeak#` w2
-}

#endif

\end{code}

%
% (c) The AQUA Project, Glasgow University, 1998
%

\section[PrelWeak]{Module @PrelWeak@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelWeak where

import PrelGHC
import PrelMaybe
import PrelBase
import PrelIOBase
import PrelForeign

data Weak v = Weak (Weak# v)

mkWeak  :: k				-- key
	-> v				-- value
	-> IO ()			-- finaliser
	-> IO (Weak v)			-- weak pointer

mkWeak key val finaliser = IO $ \s ->
   case mkWeak# key val finaliser s of { (# s1, w #) ->
   (# s1, Weak w #) }

deRefWeak :: Weak v -> IO (Maybe v)
deRefWeak (Weak w) = IO $ \s ->
   case deRefWeak# w s of
	(# s1, flag, p #) -> case flag of
				0# -> (# s1, Nothing #)
				_  -> (# s1, Just p #)

mkWeakPtr :: k -> IO () -> IO (Weak k)
mkWeakPtr key finaliser = mkWeak key key finaliser

mkWeakPair :: k -> v -> IO () -> IO (Weak (k,v))
mkWeakPair key val finaliser = mkWeak key (key,val) finaliser

addFinaliser :: key -> IO () -> IO ()
addFinaliser key finaliser = do
   mkWeakPtr key finaliser		-- throw it away
   return ()

addForeignFinaliser :: ForeignObj -> IO () -> IO ()
addForeignFinaliser (ForeignObj fo) finaliser = addFinaliser fo finaliser

{-
finalise :: Weak v -> IO ()
finalise (Weak w) = finaliseWeak# w

instance Eq (Weak v) where
  (Weak w1) == (Weak w2) = w1 `sameWeak#` w2
-}

\end{code}

%
% (c) The AQUA Project, Glasgow University, 1998
%

\section[Weak]{Module @Weak@}

\begin{code}
module Weak (
	Weak,	    		-- abstract
	-- instance Eq (Weak v)  

	mkWeak,      		-- :: k -> v -> IO () -> IO (Weak v)
	deRefWeak, 		-- :: Weak v -> IO (Maybe v)
	finalise,		-- :: Weak v -> IO ()
	-- replaceFinaliser	-- :: Weak v -> IO () -> IO ()
	mkWeakNoFinaliser,	-- :: k -> v -> IO (Weak v)

	mkWeakPtr, 		-- :: k -> IO () -> IO (Weak k)
	mkWeakPair, 		-- :: k -> v -> IO () -> IO (Weak (k,v))
	addFinaliser, 		-- :: key -> IO () -> IO ()
	addForeignFinaliser 	-- :: ForeignObj -> IO () -> IO ()
   ) where

import PrelBase
import PrelIOBase
import PrelWeak
import Foreign

deRefWeak :: Weak v -> IO (Maybe v)
deRefWeak (Weak w) = IO $ \s ->
   case deRefWeak# w s of
	(# s1, flag, p #) -> case flag of
				0# -> (# s1, Nothing #)
				_  -> (# s1, Just p #)

mkWeakNoFinaliser key val = IO $ \s ->
   -- zero is a valid finaliser argument to mkWeak#, and means "no finaliser"
   case mkWeak# key val (unsafeCoerce# 0#) s of { (# s1, w #) ->
   (# s1, Weak w #) }

mkWeakPair :: k -> v -> IO () -> IO (Weak (k,v))
mkWeakPair key val finaliser = mkWeak key (key,val) finaliser

finalise :: Weak v -> IO ()
finalise (Weak w) = IO $ \s ->
   case finaliseWeak# w s of s1 -> (# s1, () #)

\end{code}

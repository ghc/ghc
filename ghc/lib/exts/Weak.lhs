%
% (c) The AQUA Project, Glasgow University, 1998
%

\section[Weak]{Module @Weak@}

\begin{code}
module Weak (
	Weak,	    		-- abstract
	-- instance Eq (Weak v)  

	mkWeak,      		-- :: k -> v -> Maybe (IO ()) -> IO (Weak v)
	deRefWeak, 		-- :: Weak v -> IO (Maybe v)
	finalize,		-- :: Weak v -> IO ()
	-- replaceFinaliser	-- :: Weak v -> IO () -> IO ()

	mkWeakPtr, 		-- :: k -> Maybe (IO ()) -> IO (Weak k)
	mkWeakPair, 		-- :: k -> v -> Maybe (IO ()) -> IO (Weak (k,v))
	addFinalizer, 		-- :: key -> IO () -> IO ()
	addForeignFinalizer 	-- :: ForeignObj -> IO () -> IO ()
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

mkWeakPair :: k -> v -> Maybe (IO ()) -> IO (Weak (k,v))
mkWeakPair key val finalizer = mkWeak key (key,val) finalizer

finalize :: Weak v -> IO ()
finalize (Weak w) = IO $ \s ->
   case finalizeWeak# w s of 
	(# s1, 0#, _ #) -> (# s1, () #)	-- already dead, or no finaliser
	(# s1, _,  f #) -> f s1
\end{code}

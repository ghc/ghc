%
% (c) The AQUA Project, Glasgow University, 1998
%

\section[Weak]{Module @PrelWeak@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module Weak (
	Weak,	    		-- abstract
	-- instance Eq (Weak v)  

	mkWeak,      		-- :: k -> v -> IO () -> IO (Weak v)
	deRefWeak, 		-- :: Weak v -> IO (Maybe v)
	-- finalise		-- :: Weak v -> IO ()
	-- replaceFinaliser	-- :: Weak v -> IO () -> IO ()

	mkWeakPtr, 		-- :: k -> IO () -> IO (Weak k)
	mkWeakPair, 		-- :: k -> v -> IO () -> IO (Weak (k,v))
	addFinaliser, 		-- :: key -> IO () -> IO ()
	addForeignFinaliser 	-- :: ForeignObj -> IO () -> IO ()
   ) where

import PrelWeak
import Foreign
\end{code}

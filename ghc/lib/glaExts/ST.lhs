%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[module_ST]{The State Transformer Monad, @ST@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module ST (

	ST,

	runST,				-- :: (All s => ST s a) -> a
	fixST,				-- :: (a -> ST s a) -> ST s a

	unsafeInterleaveST,

        -- ST is one, so you'll likely need some Monad bits
        module Monad,

	STRef,
	newSTRef, readSTRef, writeSTRef,

	STArray,
	newSTArray, readSTArray, writeSTArray, boundsSTArray, 
	thawSTArray, freezeSTArray, unsafeFreezeSTArray, 
	Ix

    ) where

import ArrBase
import UnsafeST
import STBase
import PrelBase	( Eq(..), Int, Bool, ($), ()(..) )
import Monad
import Ix

\end{code}

%*********************************************************
%*							*
\subsection{Variables}
%*							*
%*********************************************************

\begin{code}
newtype STRef s a = STRef (MutableVar s a) 
    deriving Eq

newSTRef :: a -> ST s (STRef s a)
newSTRef v = newVar v >>= \ var -> return (STRef var)

readSTRef :: STRef s a -> ST s a
readSTRef (STRef var) = readVar var

writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef (STRef var) v = writeVar var v
\end{code}

%*********************************************************
%*							*
\subsection{Arrays}
%*							*
%*********************************************************

\begin{code}
newtype STArray s ix elt = STArray (MutableArray s ix elt)
    deriving Eq

newSTArray 		:: Ix ix => (ix,ix) -> elt -> ST s (STArray s ix elt)
writeSTArray	  	:: Ix ix => STArray s ix elt -> ix -> elt -> ST s () 
readSTArray   		:: Ix ix => STArray s ix elt -> ix -> ST s elt 
boundsSTArray     	:: Ix ix => STArray s ix elt -> (ix, ix)  
thawSTArray 		:: Ix ix => Array ix elt -> ST s (STArray s ix elt)
freezeSTArray	  	:: Ix ix => STArray s ix elt -> ST s (Array ix elt)
unsafeFreezeSTArray 	:: Ix ix => STArray s ix elt -> ST s (Array ix elt)

newSTArray ixs elt = 
    newArray ixs elt >>= \arr -> 
    return (STArray arr)

boundsSTArray (STArray arr) = boundsOfArray arr

readSTArray (STArray arr) ix = readArray arr ix

writeSTArray (STArray arr) ix elt = writeArray arr ix elt

thawSTArray arr = thawArray arr >>= \starr -> return (STArray starr)

freezeSTArray (STArray arr) = freezeArray arr

unsafeFreezeSTArray (STArray arr) = unsafeFreezeArray arr
\end{code}


%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1997
%

\section[LazyST]{The Lazy State Transformer Monad, @LazyST@}

This module presents an identical interface to ST, but the underlying
implementation of the state thread is lazy.

\begin{code}
module LazyST (

	STBase.ST,

	unsafeInterleaveST,

        -- ST is one, so you'll likely need some Monad bits
        module Monad,

	ST.STRef,
	newSTRef, readSTRef, writeSTRef,

	ST.STArray,
	newSTArray, readSTArray, writeSTArray, Ix,

	strictToLazyST, lazyToStrictST
    ) where

import qualified ST
import qualified STBase
import ArrBase
import Unsafe   ( unsafeInterleaveST )
import PrelBase	( Eq(..), Int, Bool, ($), ()(..) )
import Monad
import Ix

newtype ST s a = ST (STBase.State s -> (a,STBase.State s))

instance Monad (ST s) where

        return a = ST $ \ s -> (a,s)
        m >> k   =  m >>= \ _ -> k

        (ST m) >>= k
         = ST $ \ s ->
           let
             (r,new_s) = m s
             ST k_a = k r
           in
           k_a new_s
\end{code}

%*********************************************************
%*							*
\subsection{Variables}
%*							*
%*********************************************************

\begin{code}
newSTRef   :: a -> ST s (ST.STRef s a)
readSTRef  :: ST.STRef s a -> ST s a
writeSTRef :: ST.STRef s a -> a -> ST s ()

newSTRef   = strictToLazyST . ST.newSTRef
readSTRef  = strictToLazyST . ST.readSTRef
writeSTRef r a = strictToLazyST (ST.writeSTRef r a)
\end{code}

%*********************************************************
%*							*
\subsection{Arrays}
%*							*
%*********************************************************

\begin{code}
type STArray s ix elt = MutableArray s ix elt

newSTArray 	    :: Ix ix => (ix,ix) -> elt -> ST s (STArray s ix elt)
readSTArray   	    :: Ix ix => STArray s ix elt -> ix -> ST s elt 
writeSTArray	    :: Ix ix => STArray s ix elt -> ix -> elt -> ST s () 
boundsSTArray       :: Ix ix => STArray s ix elt -> (ix, ix)  
thawSTArray 	    :: Ix ix => Array ix elt -> ST s (STArray s ix elt)
freezeSTArray	    :: Ix ix => STArray s ix elt -> ST s (Array ix elt)
unsafeFreezeSTArray :: Ix ix => STArray s ix elt -> ST s (Array ix elt)

newSTArray ixs init   	= strictToLazyST (newArray ixs init)
readSTArray arr ix    	= strictToLazyST (readArray arr ix)
writeSTArray arr ix v 	= strictToLazyST (writeArray arr ix v)
boundsSTArray     	= boundsOfArray
thawSTArray 		= strictToLazyST . thawArray
freezeSTArray 		= strictToLazyST . freezeArray
unsafeFreezeSTArray 	= strictToLazyST . unsafeFreezeArray

strictToLazyST :: STBase.ST s a -> ST s a
strictToLazyST (STBase.ST m) = ST $ \s ->
        let STBase.S# s# = s in
	case m s# of { STBase.STret s2# r -> (r, STBase.S# s2#) }

lazyToStrictST :: ST s a -> STBase.ST s a
lazyToStrictST (ST m) = STBase.ST $ \s ->
        case (m (STBase.S# s)) of (a, STBase.S# s') -> STBase.STret s' a


\end{code}

%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1997
%

\section[LazyST]{The Lazy State Transformer Monad, @LazyST@}

This module presents an identical interface to ST, but the underlying
implementation of the state thread is lazy.

\begin{code}
module LazyST (

	ST,

	runST,
	unsafeInterleaveST,

        -- ST is one, so you'll likely need some Monad bits
        module Monad,

	ST.STRef,
	newSTRef, readSTRef, writeSTRef,

	STArray,
	newSTArray, readSTArray, writeSTArray, boundsSTArray, 
	thawSTArray, freezeSTArray, unsafeFreezeSTArray, 
	Ix,

	ST.unsafeIOToST, ST.stToIO,

	strictToLazyST, lazyToStrictST
    ) where

import qualified ST
import qualified PrelST
import PrelArr
import PrelBase	( Eq(..), Int, Bool, ($), ()(..) )
import Monad
import Ix
import PrelGHC

newtype ST s a = ST (State s -> (a, State s))

data State s = S# (State# s)

instance Functor (ST s) where
    fmap f m = ST $ \ s ->
      let 
       ST m_a = m
       (r,new_s) = m_a s
      in
      (f r,new_s)

instance Monad (ST s) where

        return a = ST $ \ s -> (a,s)
        m >> k   =  m >>= \ _ -> k
	fail s   = error s

        (ST m) >>= k
         = ST $ \ s ->
           let
             (r,new_s) = m s
             ST k_a = k r
           in
           k_a new_s

{-# NOINLINE runST #-}
runST :: (forall s. ST s a) -> a
runST st = case st of ST the_st -> let (r,_) = the_st (S# realWorld#) in r
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
newtype STArray s ix elt = STArray (MutableArray s ix elt)

newSTArray 	    :: Ix ix => (ix,ix) -> elt -> ST s (STArray s ix elt)
readSTArray   	    :: Ix ix => STArray s ix elt -> ix -> ST s elt 
writeSTArray	    :: Ix ix => STArray s ix elt -> ix -> elt -> ST s () 
boundsSTArray       :: Ix ix => STArray s ix elt -> (ix, ix)  
thawSTArray 	    :: Ix ix => Array ix elt -> ST s (STArray s ix elt)
freezeSTArray	    :: Ix ix => STArray s ix elt -> ST s (Array ix elt)
unsafeFreezeSTArray :: Ix ix => STArray s ix elt -> ST s (Array ix elt)

newSTArray ixs init   	= 
	   strictToLazyST (newArray ixs init) >>= \arr ->
	   return (STArray arr)

readSTArray (STArray arr) ix = strictToLazyST (readArray arr ix)
writeSTArray (STArray arr) ix v = strictToLazyST (writeArray arr ix v)
boundsSTArray (STArray arr) = boundsOfArray arr
thawSTArray arr	= 
	    strictToLazyST (thawArray arr) >>= \ marr -> 
	    return (STArray marr)

freezeSTArray (STArray arr) = strictToLazyST (freezeArray arr)
unsafeFreezeSTArray (STArray arr) = strictToLazyST (unsafeFreezeArray arr)

strictToLazyST :: PrelST.ST s a -> ST s a
strictToLazyST m = ST $ \s ->
        let 
  	   pr = case s of { S# s# -> PrelST.liftST m s# }
	   r  = case pr of { PrelST.STret _ v -> v }
	   s' = case pr of { PrelST.STret s2# _ -> S# s2# }
	in
	(r, s')

lazyToStrictST :: ST s a -> PrelST.ST s a
lazyToStrictST (ST m) = PrelST.ST $ \s ->
        case (m (S# s)) of (a, S# s') -> (# s', a #)

unsafeInterleaveST :: ST s a -> ST s a
unsafeInterleaveST = strictToLazyST . ST.unsafeInterleaveST . lazyToStrictST

\end{code}

%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[PrelConc]{Module @PrelConc@}

Basic concurrency stuff

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}
module PrelConc(
		-- Forking and suchlike
	ST,	forkST,
	IO,	forkIO,	
	par, fork,
	threadDelay, threadWaitRead, threadWaitWrite,

		-- MVars
	MVar, newMVar, newEmptyMVar, takeMVar, putMVar, readMVar, swapMVar
    ) where

import PrelBase
import PrelST	  	( ST(..), STret(..), StateAndPtr#(..) )
import PrelIOBase	( IO(..), IOResult(..), MVar(..) )
import PrelErr		( parError )
import PrelBase		( Int(..) )
import PrelGHC		( fork#, delay#, waitRead#, waitWrite#,
		  	  SynchVar#, newSynchVar#, takeMVar#, putMVar#,
		  	  State#, RealWorld, par#
			)

infixr 0 `par`, `fork`
\end{code}



%************************************************************************
%*									*
\subsection{@par@, and @fork@}
%*									*
%************************************************************************

\begin{code}
forkST :: ST s a -> ST s a

forkST (ST action) = ST $ \ s -> 
	let d@(STret _ r) = action s in
	d `fork` STret s r

forkIO :: IO () -> IO ()
forkIO (IO action) = IO $ \ s -> (action s) `fork` IOok s ()

par, fork :: Eval a => a -> b -> b

{-# INLINE par  #-}
{-# INLINE fork #-}

#if defined(__PARALLEL_HASKELL__) || defined (__GRANSIM__)
par  x y = case (par#  x) of { 0# -> parError; _ -> y }
#else
par  x y = y
#endif

#if defined(__CONCURRENT_HASKELL__) || defined (__GRANSIM__)
fork x y = case (fork# x) of { 0# -> parError; _ -> y }
#else
fork x y = y
#endif

runOrBlockIO m = m			-- ?????

\end{code}

%************************************************************************
%*									*
\subsection[mvars]{M-Structures}
%*									*
%************************************************************************

M-Vars are rendezvous points for concurrent threads.  They begin
empty, and any attempt to read an empty M-Var blocks.  When an M-Var
is written, a single blocked thread may be freed.  Reading an M-Var
toggles its state from full back to empty.  Therefore, any value
written to an M-Var may only be read once.  Multiple reads and writes
are allowed, but there must be at least one read between any two
writes.

\begin{code}
--Defined in IOBase to avoid cycle: data MVar a = MVar (SynchVar# RealWorld a)

newEmptyMVar  :: IO (MVar a)

newEmptyMVar = IO $ \ s# ->
    case newSynchVar# s# of
        StateAndSynchVar# s2# svar# -> IOok s2# (MVar svar#)

takeMVar :: MVar a -> IO a

takeMVar (MVar mvar#) = IO $ \ s# ->
    case takeMVar# mvar# s# of
        StateAndPtr# s2# r -> IOok s2# r

putMVar  :: MVar a -> a -> IO ()

putMVar (MVar mvar#) x = IO $ \ s# ->
    case putMVar# mvar# x s# of
        s2# -> IOok s2# ()

newMVar :: a -> IO (MVar a)

newMVar value =
    newEmptyMVar	>>= \ mvar ->
    putMVar mvar value	>>
    return mvar

readMVar :: MVar a -> IO a

readMVar mvar =
    takeMVar mvar	>>= \ value ->
    putMVar mvar value	>>
    return value

swapMVar :: MVar a -> a -> IO a

swapMVar mvar new =
    takeMVar mvar	>>= \ old ->
    putMVar mvar new	>>
    return old
\end{code}


%************************************************************************
%*									*
\subsection{Thread waiting}
%*									*
%************************************************************************

@threadDelay@ delays rescheduling of a thread until the indicated
number of microseconds have elapsed.  Generally, the microseconds are
counted by the context switch timer, which ticks in virtual time;
however, when there are no runnable threads, we don't accumulate any
virtual time, so we start ticking in real time.  (The granularity is
the effective resolution of the context switch timer, so it is
affected by the RTS -C option.)

@threadWaitRead@ delays rescheduling of a thread until input on the
specified file descriptor is available for reading (just like select).
@threadWaitWrite@ is similar, but for writing on a file descriptor.

\begin{code}
threadDelay, threadWaitRead, threadWaitWrite :: Int -> IO ()

threadDelay (I# x#) = IO $ \ s# ->
    case delay# x# s# of
      s2# -> IOok s2# ()

threadWaitRead (I# x#) = IO $ \ s# -> 
    case waitRead# x# s# of
      s2# -> IOok s2# ()

threadWaitWrite (I# x#) = IO $ \ s# ->
    case waitWrite# x# s# of
      s2# -> IOok s2# ()
\end{code}

%*********************************************************
%*							*
\subsection{Ghastly return types}
%*							*
%*********************************************************

\begin{code}
data StateAndSynchVar# s elt = StateAndSynchVar# (State# s) (SynchVar# s elt)
\end{code}

%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[PrelConc]{Module @PrelConc@}

Basic concurrency stuff

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelConc (

		-- Thread Ids
	ThreadId,

		-- Forking and suchlike
	forkIO,	
	killThread,
	seq, par, fork,
	{-threadDelay, threadWaitRead, threadWaitWrite, -}

  		-- MVars
	MVar, newMVar, newEmptyMVar, takeMVar, putMVar, readMVar, swapMVar
    ) where

import PrelBase
import {-# SOURCE #-} PrelErr ( parError )
import PrelST	  	( ST(..), STret(..), liftST )
import PrelIOBase	( IO(..), MVar(..), liftIO, unsafePerformIO )
import PrelErr		( parError )
import PrelBase		( Int(..) )
import PrelErr		( seqError )

infixr 0 `par`, `fork`
\end{code}

%************************************************************************
%*									*
\subsection{@ThreadId@, @par@, and @fork@}
%*									*
%************************************************************************

\begin{code}
data ThreadId = ThreadId ThreadId#
-- ToDo: data ThreadId = ThreadId (WeakPair ThreadId# ())
-- But since ThreadId# is unlifted, the WeakPair type must use open
-- type variables.

forkIO :: IO () -> IO ThreadId
forkIO action = IO $ \ s -> 
   case (fork# action s) of (# s, id #) -> (# s, ThreadId id #)

killThread :: ThreadId -> IO ()
killThread (ThreadId id) = IO $ \ s ->
   case (killThread# id s) of s -> (# s, () #)

-- "seq" is defined a bit wierdly (see below)
--
-- The reason for the strange "0# -> parError" case is that
-- it fools the compiler into thinking that seq is non-strict in
-- its second argument (even if it inlines seq at the call site).
-- If it thinks seq is strict in "y", then it often evaluates
-- "y" before "x", which is totally wrong.  
--
-- Just before converting from Core to STG there's a bit of magic
-- that recognises the seq# and eliminates the duff case.

{-# INLINE seq  #-}
seq :: a -> b -> b
seq  x y = case (seq#  x) of { 0# -> seqError; _ -> y }

par, fork :: a -> b -> b

{-# INLINE par  #-}
{-# INLINE fork #-}
#if defined(__PARALLEL_HASKELL__) || defined (__GRANSIM__)
par  x y = case (par# x) of { 0# -> parError; _ -> y }
#else
par  x y = y
#endif

fork x y = unsafePerformIO (forkIO (x `seq` return ())) `seq` y

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

instance Eq (MVar a) where
	(MVar mvar1#) == (MVar mvar2#) = sameMVar# mvar1# mvar2#

newEmptyMVar  :: IO (MVar a)

newEmptyMVar = IO $ \ s# ->
    case newMVar# s# of
         (# s2#, svar# #) -> (# s2#, MVar svar# #)

takeMVar :: MVar a -> IO a

takeMVar (MVar mvar#) = IO $ \ s# -> takeMVar# mvar# s#

putMVar  :: MVar a -> a -> IO ()

putMVar (MVar mvar#) x = IO $ \ s# ->
    case putMVar# mvar# x s# of
        s2# -> (# s2#, () #)

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
{- Not yet -- SDM
threadDelay, threadWaitRead, threadWaitWrite :: Int -> IO ()

threadDelay (I# x#) = IO $ \ s# ->
    case delay# x# s# of
      s2# -> (# s2#, () #)

threadWaitRead (I# x#) = IO $ \ s# -> 
    case waitRead# x# s# of
      s2# -> (# s2#, () #)

threadWaitWrite (I# x#) = IO $ \ s# ->
    case waitWrite# x# s# of
      s2# -> (# s2#, () #)
-}
\end{code}

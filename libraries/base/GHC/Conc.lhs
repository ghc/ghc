% -----------------------------------------------------------------------------
% $Id: Conc.lhs,v 1.1 2001/06/28 14:15:03 simonmar Exp $
%
% (c) The University of Glasgow, 1994-2000
%

\section[GHC.Conc]{Module @GHC.Conc@}

Basic concurrency stuff

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module GHC.Conc
	( ThreadId(..)

	-- Forking and suchlike
	, myThreadId 	-- :: IO ThreadId
	, killThread	-- :: ThreadId -> IO ()
	, throwTo       -- :: ThreadId -> Exception -> IO ()
	, par  		-- :: a -> b -> b
	, seq  		-- :: a -> b -> b
	, yield         -- :: IO ()

	-- Waiting
	, threadDelay	  	-- :: Int -> IO ()
	, threadWaitRead	-- :: Int -> IO ()
	, threadWaitWrite	-- :: Int -> IO ()

	-- MVars
	, MVar		-- abstract
	, newMVar 	-- :: a -> IO (MVar a)
	, newEmptyMVar  -- :: IO (MVar a)
	, takeMVar 	-- :: MVar a -> IO a
	, putMVar  	-- :: MVar a -> a -> IO ()
	, tryTakeMVar   -- :: MVar a -> IO (Maybe a)
	, tryPutMVar  	-- :: MVar a -> a -> IO Bool
	, isEmptyMVar	-- :: MVar a -> IO Bool
	, addMVarFinalizer -- :: MVar a -> IO () -> IO ()

    ) where

import GHC.Base
import GHC.Maybe
import GHC.Err		( parError, seqError )
import GHC.IOBase	( IO(..), MVar(..) )
import GHC.Base		( Int(..) )
import GHC.Exception    ( Exception(..), AsyncException(..) )

infixr 0 `par`, `seq`
\end{code}

%************************************************************************
%*									*
\subsection{@ThreadId@, @par@, and @fork@}
%*									*
%************************************************************************

\begin{code}
data ThreadId = ThreadId ThreadId#
-- ToDo: data ThreadId = ThreadId (Weak ThreadId#)
-- But since ThreadId# is unlifted, the Weak type must use open
-- type variables.

--forkIO has now been hoisted out into the Concurrent library.

killThread :: ThreadId -> IO ()
killThread (ThreadId id) = IO $ \ s ->
   case (killThread# id (AsyncException ThreadKilled) s) of s1 -> (# s1, () #)

throwTo :: ThreadId -> Exception -> IO ()
throwTo (ThreadId id) ex = IO $ \ s ->
   case (killThread# id ex s) of s1 -> (# s1, () #)

myThreadId :: IO ThreadId
myThreadId = IO $ \s ->
   case (myThreadId# s) of (# s1, id #) -> (# s1, ThreadId id #)

yield :: IO ()
yield = IO $ \s -> 
   case (yield# s) of s1 -> (# s1, () #)

-- "seq" is defined a bit weirdly (see below)
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

{-# INLINE par  #-}
par :: a -> b -> b
par  x y = case (par# x) of { 0# -> parError; _ -> y }
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
    case newMVar# s# of
         (# s2#, svar# #) -> (# s2#, MVar svar# #)

takeMVar :: MVar a -> IO a
takeMVar (MVar mvar#) = IO $ \ s# -> takeMVar# mvar# s#

putMVar  :: MVar a -> a -> IO ()
putMVar (MVar mvar#) x = IO $ \ s# ->
    case putMVar# mvar# x s# of
        s2# -> (# s2#, () #)

tryPutMVar  :: MVar a -> a -> IO Bool
tryPutMVar (MVar mvar#) x = IO $ \ s# ->
    case tryPutMVar# mvar# x s# of
        (# s, 0# #) -> (# s, False #)
        (# s, _  #) -> (# s, True #)

newMVar :: a -> IO (MVar a)
newMVar value =
    newEmptyMVar	>>= \ mvar ->
    putMVar mvar value	>>
    return mvar

-- tryTakeMVar is a non-blocking takeMVar
tryTakeMVar :: MVar a -> IO (Maybe a)
tryTakeMVar (MVar m) = IO $ \ s ->
    case tryTakeMVar# m s of
	(# s, 0#, _ #) -> (# s, Nothing #)	-- MVar is empty
	(# s, _,  a #) -> (# s, Just a  #)	-- MVar is full

{- 
 Low-level op. for checking whether an MVar is filled-in or not.
 Notice that the boolean value returned  is just a snapshot of
 the state of the MVar. By the time you get to react on its result,
 the MVar may have been filled (or emptied) - so be extremely
 careful when using this operation.  

 Use tryTakeMVar instead if possible.

 If you can re-work your abstractions to avoid having to
 depend on isEmptyMVar, then you're encouraged to do so,
 i.e., consider yourself warned about the imprecision in
 general of isEmptyMVar :-)
-}
isEmptyMVar :: MVar a -> IO Bool
isEmptyMVar (MVar mv#) = IO $ \ s# -> 
    case isEmptyMVar# mv# s# of
        (# s2#, flg #) -> (# s2#, not (flg ==# 0#) #)

-- Like addForeignPtrFinalizer, but for MVars
addMVarFinalizer :: MVar a -> IO () -> IO ()
addMVarFinalizer (MVar m) finalizer = 
  IO $ \s -> case mkWeak# m () finalizer s of { (# s1, w #) -> (# s1, () #) }
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

threadDelay     (I# ms) = IO $ \s -> case delay# ms s     of s -> (# s, () #)
threadWaitRead  (I# fd) = IO $ \s -> case waitRead# fd s  of s -> (# s, () #)
threadWaitWrite (I# fd) = IO $ \s -> case waitWrite# fd s of s -> (# s, () #)
\end{code}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Conc
-- Copyright   :  (c) The University of Glasgow, 1994-2002
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Basic concurrency stuff.
-- 
-----------------------------------------------------------------------------

#include "config.h"
module GHC.Conc
	( ThreadId(..)

	-- Forking and suchlike
	, myThreadId 	-- :: IO ThreadId
	, killThread	-- :: ThreadId -> IO ()
	, throwTo       -- :: ThreadId -> Exception -> IO ()
	, par  		-- :: a -> b -> b
	, pseq 		-- :: a -> b -> b
	, yield         -- :: IO ()
	, labelThread	-- :: ThreadId -> String -> IO ()

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

#ifdef mingw32_TARGET_OS
	, asyncRead     -- :: Int -> Int -> Int -> Ptr a -> IO (Int, Int)
	, asyncWrite    -- :: Int -> Int -> Int -> Ptr a -> IO (Int, Int)
	, asyncDoProc   -- :: FunPtr (Ptr a -> IO Int) -> Ptr a -> IO Int

	, asyncReadBA   -- :: Int -> Int -> Int -> Int -> MutableByteArray# RealWorld -> IO (Int, Int)
	, asyncWriteBA  -- :: Int -> Int -> Int -> Int -> MutableByteArray# RealWorld -> IO (Int, Int)
#endif
        ) where

import Data.Maybe

import GHC.Base
import GHC.IOBase	( IO(..), MVar(..), ioException, IOException(..), IOErrorType(..) )
import GHC.Num		( fromInteger, negate )
import GHC.Real		( fromIntegral )
import GHC.Base		( Int(..) )
import GHC.Exception    ( Exception(..), AsyncException(..) )
import GHC.Pack		( packCString# )
import GHC.Ptr          ( Ptr(..), plusPtr, FunPtr(..) )

infixr 0 `par`, `pseq`
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
{- ^
A 'ThreadId' is an abstract type representing a handle to a thread.
'ThreadId' is an instance of 'Eq', 'Ord' and 'Show', where
the 'Ord' instance implements an arbitrary total ordering over
'ThreadId's. The 'Show' instance lets you convert an arbitrary-valued
'ThreadId' to string form; showing a 'ThreadId' value is occasionally
useful when debugging or diagnosing the behaviour of a concurrent
program.

/Note/: in GHC, if you have a 'ThreadId', you essentially have
a pointer to the thread itself.  This means the thread itself can\'t be
garbage collected until you drop the 'ThreadId'.
This misfeature will hopefully be corrected at a later date.

/Note/: Hugs does not provide any operations on other threads;
it defines 'ThreadId' as a synonym for ().
-}

--forkIO has now been hoisted out into the Concurrent library.

{- | 'killThread' terminates the given thread (GHC only).
Any work already done by the thread isn\'t
lost: the computation is suspended until required by another thread.
The memory used by the thread will be garbage collected if it isn\'t
referenced from anywhere.  The 'killThread' function is defined in
terms of 'throwTo':

> killThread tid = throwTo tid (AsyncException ThreadKilled)

-}
killThread :: ThreadId -> IO ()
killThread tid = throwTo tid (AsyncException ThreadKilled)

{- | 'throwTo' raises an arbitrary exception in the target thread (GHC only).

'throwTo' does not return until the exception has been raised in the
target thread.  The calling thread can thus be certain that the target
thread has received the exception.  This is a useful property to know
when dealing with race conditions: eg. if there are two threads that
can kill each other, it is guaranteed that only one of the threads
will get to kill the other. -}
throwTo :: ThreadId -> Exception -> IO ()
throwTo (ThreadId id) ex = IO $ \ s ->
   case (killThread# id ex s) of s1 -> (# s1, () #)

-- | Returns the 'ThreadId' of the calling thread (GHC only).
myThreadId :: IO ThreadId
myThreadId = IO $ \s ->
   case (myThreadId# s) of (# s1, id #) -> (# s1, ThreadId id #)


-- |The 'yield' action allows (forces, in a co-operative multitasking
-- implementation) a context-switch to any other currently runnable
-- threads (if any), and is occasionally useful when implementing
-- concurrency abstractions.
yield :: IO ()
yield = IO $ \s -> 
   case (yield# s) of s1 -> (# s1, () #)

{- | 'labelThread' stores a string as identifier for this thread if
you built a RTS with debugging support. This identifier will be used in
the debugging output to make distinction of different threads easier
(otherwise you only have the thread state object\'s address in the heap).

Other applications like the graphical Concurrent Haskell Debugger
(<http://www.informatik.uni-kiel.de/~fhu/chd/>) may choose to overload
'labelThread' for their purposes as well.
-}

labelThread :: ThreadId -> String -> IO ()
labelThread (ThreadId t) str = IO $ \ s ->
   let ps  = packCString# str
       adr = byteArrayContents# ps in
     case (labelThread# t adr s) of s1 -> (# s1, () #)

-- 	Nota Bene: 'pseq' used to be 'seq'
--		   but 'seq' is now defined in PrelGHC
--
-- "pseq" is defined a bit weirdly (see below)
--
-- The reason for the strange "lazy" call is that
-- it fools the compiler into thinking that pseq  and par are non-strict in
-- their second argument (even if it inlines pseq at the call site).
-- If it thinks pseq is strict in "y", then it often evaluates
-- "y" before "x", which is totally wrong.  

{-# INLINE pseq  #-}
pseq :: a -> b -> b
pseq  x y = x `seq` lazy y

{-# INLINE par  #-}
par :: a -> b -> b
par  x y = case (par# x) of { _ -> lazy y }
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

-- |Create an 'MVar' which is initially empty.
newEmptyMVar  :: IO (MVar a)
newEmptyMVar = IO $ \ s# ->
    case newMVar# s# of
         (# s2#, svar# #) -> (# s2#, MVar svar# #)

-- |Create an 'MVar' which contains the supplied value.
newMVar :: a -> IO (MVar a)
newMVar value =
    newEmptyMVar	>>= \ mvar ->
    putMVar mvar value	>>
    return mvar

-- |Return the contents of the 'MVar'.  If the 'MVar' is currently
-- empty, 'takeMVar' will wait until it is full.  After a 'takeMVar', 
-- the 'MVar' is left empty.
-- 
-- If several threads are competing to take the same 'MVar', one is chosen
-- to continue at random when the 'MVar' becomes full.
takeMVar :: MVar a -> IO a
takeMVar (MVar mvar#) = IO $ \ s# -> takeMVar# mvar# s#

-- |Put a value into an 'MVar'.  If the 'MVar' is currently full,
-- 'putMVar' will wait until it becomes empty.
--
-- If several threads are competing to fill the same 'MVar', one is
-- chosen to continue at random with the 'MVar' becomes empty.
putMVar  :: MVar a -> a -> IO ()
putMVar (MVar mvar#) x = IO $ \ s# ->
    case putMVar# mvar# x s# of
        s2# -> (# s2#, () #)

-- |A non-blocking version of 'takeMVar'.  The 'tryTakeMVar' function
-- returns immediately, with 'Nothing' if the 'MVar' was empty, or
-- @'Just' a@ if the 'MVar' was full with contents @a@.  After 'tryTakeMVar',
-- the 'MVar' is left empty.
tryTakeMVar :: MVar a -> IO (Maybe a)
tryTakeMVar (MVar m) = IO $ \ s ->
    case tryTakeMVar# m s of
	(# s, 0#, _ #) -> (# s, Nothing #)	-- MVar is empty
	(# s, _,  a #) -> (# s, Just a  #)	-- MVar is full

-- |A non-blocking version of 'putMVar'.  The 'tryPutMVar' function
-- attempts to put the value @a@ into the 'MVar', returning 'True' if
-- it was successful, or 'False' otherwise.
tryPutMVar  :: MVar a -> a -> IO Bool
tryPutMVar (MVar mvar#) x = IO $ \ s# ->
    case tryPutMVar# mvar# x s# of
        (# s, 0# #) -> (# s, False #)
        (# s, _  #) -> (# s, True #)

-- |Check whether a given 'MVar' is empty.
--
-- Notice that the boolean value returned  is just a snapshot of
-- the state of the MVar. By the time you get to react on its result,
-- the MVar may have been filled (or emptied) - so be extremely
-- careful when using this operation.   Use 'tryTakeMVar' instead if possible.
isEmptyMVar :: MVar a -> IO Bool
isEmptyMVar (MVar mv#) = IO $ \ s# -> 
    case isEmptyMVar# mv# s# of
        (# s2#, flg #) -> (# s2#, not (flg ==# 0#) #)

-- |Add a finalizer to an 'MVar' (GHC only).  See "Foreign.ForeignPtr" and
-- "System.Mem.Weak" for more about finalizers.
addMVarFinalizer :: MVar a -> IO () -> IO ()
addMVarFinalizer (MVar m) finalizer = 
  IO $ \s -> case mkWeak# m () finalizer s of { (# s1, w #) -> (# s1, () #) }
\end{code}


%************************************************************************
%*									*
\subsection{Thread waiting}
%*									*
%************************************************************************

@threadWaitRead@ delays rescheduling of a thread until input on the
specified file descriptor is available for reading (just like select).
@threadWaitWrite@ is similar, but for writing on a file descriptor.

\begin{code}
-- |The 'threadDelay' operation will cause the current thread to
-- suspend for a given number of microseconds (GHC only).
--
-- Note that the resolution
-- used by the Haskell runtime system\'s internal timer together with the
-- fact that the thread may take some time to be rescheduled after the
-- time has expired, means that the accuracy is more like 1\/50 second.
threadDelay :: Int -> IO ()

-- | Block the current thread until data is available to read on the
-- given file descriptor (GHC only).
threadWaitRead :: Int -> IO ()

-- | Block the current thread until data can be written to the
-- given file descriptor (GHC only).
threadWaitWrite :: Int -> IO ()

threadDelay     (I# ms) = IO $ \s -> case delay# ms s     of s -> (# s, () #)
threadWaitRead  (I# fd) = IO $ \s -> case waitRead# fd s  of s -> (# s, () #)
threadWaitWrite (I# fd) = IO $ \s -> case waitWrite# fd s of s -> (# s, () #)

#ifdef mingw32_TARGET_OS

-- Note: threadDelay, threadWaitRead and threadWaitWrite aren't really functional
-- on Win32, but left in there because lib code (still) uses them (the manner
-- in which they're used doesn't cause problems on a Win32 platform though.)

asyncRead :: Int -> Int -> Int -> Ptr a -> IO (Int, Int)
asyncRead  (I# fd) (I# isSock) (I# len) (Ptr buf) = 
  IO $ \s -> case asyncRead# fd isSock len buf s  of 
  	       (# s, len#, err# #) -> (# s, (I# len#, I# err#) #)

asyncWrite :: Int -> Int -> Int -> Ptr a -> IO (Int, Int)
asyncWrite  (I# fd) (I# isSock) (I# len) (Ptr buf) = 
  IO $ \s -> case asyncWrite# fd isSock len buf s  of 
  	       (# s, len#, err# #) -> (# s, (I# len#, I# err#) #)

asyncDoProc :: FunPtr (Ptr a -> IO Int) -> Ptr a -> IO Int
asyncDoProc (FunPtr proc) (Ptr param) = 
    -- the 'length' value is ignored; simplifies implementation of
    -- the async*# primops to have them all return the same result.
  IO $ \s -> case asyncDoProc# proc param s  of 
  	       (# s, len#, err# #) -> (# s, I# err# #)

-- to aid the use of these primops by the IO Handle implementation,
-- provide the following convenience funs:

-- this better be a pinned byte array!
asyncReadBA :: Int -> Int -> Int -> Int -> MutableByteArray# RealWorld -> IO (Int,Int)
asyncReadBA fd isSock len off bufB = 
  asyncRead fd isSock len ((Ptr (byteArrayContents# (unsafeCoerce# bufB))) `plusPtr` off)
  
asyncWriteBA :: Int -> Int -> Int -> Int -> MutableByteArray# RealWorld -> IO (Int,Int)
asyncWriteBA fd isSock len off bufB = 
  asyncWrite fd isSock len ((Ptr (byteArrayContents# (unsafeCoerce# bufB))) `plusPtr` off)

#endif
\end{code}

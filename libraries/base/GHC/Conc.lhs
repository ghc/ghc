\begin{code}
{-# OPTIONS_GHC -fno-implicit-prelude #-}
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

-- No: #hide, because bits of this module are exposed by the stm package.
-- However, we don't want this module to be the home location for the
-- bits it exports, we'd rather have Control.Concurrent and the other
-- higher level modules be the home.  Hence:

-- #not-home
module GHC.Conc
	( ThreadId(..)

	-- Forking and suchlike
	, forkIO	-- :: IO a -> IO ThreadId
	, forkOnIO	-- :: Int -> IO a -> IO ThreadId
	, childHandler  -- :: Exception -> IO ()
	, myThreadId 	-- :: IO ThreadId
	, killThread	-- :: ThreadId -> IO ()
	, throwTo       -- :: ThreadId -> Exception -> IO ()
	, par  		-- :: a -> b -> b
	, pseq 		-- :: a -> b -> b
	, yield         -- :: IO ()
	, labelThread	-- :: ThreadId -> String -> IO ()

	-- Waiting
	, threadDelay	  	-- :: Int -> IO ()
	, registerDelay		-- :: Int -> IO (TVar Bool)
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

   	-- TVars
	, STM           -- abstract
	, atomically    -- :: STM a -> IO a
	, retry         -- :: STM a
	, orElse        -- :: STM a -> STM a -> STM a
        , catchSTM      -- :: STM a -> (Exception -> STM a) -> STM a
	, TVar          -- abstract
	, newTVar 	-- :: a -> STM (TVar a)
	, newTVarIO 	-- :: a -> STM (TVar a)
	, readTVar	-- :: TVar a -> STM a
	, writeTVar	-- :: a -> TVar a -> STM ()
	, unsafeIOToSTM	-- :: IO a -> STM a

#ifdef mingw32_HOST_OS
	, asyncRead     -- :: Int -> Int -> Int -> Ptr a -> IO (Int, Int)
	, asyncWrite    -- :: Int -> Int -> Int -> Ptr a -> IO (Int, Int)
	, asyncDoProc   -- :: FunPtr (Ptr a -> IO Int) -> Ptr a -> IO Int

	, asyncReadBA   -- :: Int -> Int -> Int -> Int -> MutableByteArray# RealWorld -> IO (Int, Int)
	, asyncWriteBA  -- :: Int -> Int -> Int -> Int -> MutableByteArray# RealWorld -> IO (Int, Int)
#endif

#ifndef mingw32_HOST_OS
	, ensureIOManagerIsRunning
#endif
        ) where

import System.Posix.Types
import System.Posix.Internals
import Foreign
import Foreign.C

#ifndef __HADDOCK__
import {-# SOURCE #-} GHC.TopHandler ( reportError, reportStackOverflow )
#endif

import Data.Maybe

import GHC.Base
import GHC.IOBase
import GHC.Num		( Num(..) )
import GHC.Real		( fromIntegral, quot )
import GHC.Base		( Int(..) )
import GHC.Exception    ( catchException, Exception(..), AsyncException(..) )
import GHC.Pack		( packCString# )
import GHC.Ptr          ( Ptr(..), plusPtr, FunPtr(..) )
import GHC.STRef
import Data.Typeable

infixr 0 `par`, `pseq`
\end{code}

%************************************************************************
%*									*
\subsection{@ThreadId@, @par@, and @fork@}
%*									*
%************************************************************************

\begin{code}
data ThreadId = ThreadId ThreadId# deriving( Typeable )
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

{- |
This sparks off a new thread to run the 'IO' computation passed as the
first argument, and returns the 'ThreadId' of the newly created
thread.

The new thread will be a lightweight thread; if you want to use a foreign
library that uses thread-local storage, use 'forkOS' instead.
-}
forkIO :: IO () -> IO ThreadId
forkIO action = IO $ \ s -> 
   case (fork# action_plus s) of (# s1, id #) -> (# s1, ThreadId id #)
 where
  action_plus = catchException action childHandler

forkOnIO :: Int -> IO () -> IO ThreadId
forkOnIO (I# cpu) action = IO $ \ s -> 
   case (forkOn# cpu action_plus s) of (# s1, id #) -> (# s1, ThreadId id #)
 where
  action_plus = catchException action childHandler

childHandler :: Exception -> IO ()
childHandler err = catchException (real_handler err) childHandler

real_handler :: Exception -> IO ()
real_handler ex =
  case ex of
	-- ignore thread GC and killThread exceptions:
	BlockedOnDeadMVar            -> return ()
	BlockedIndefinitely          -> return ()
	AsyncException ThreadKilled  -> return ()

	-- report all others:
	AsyncException StackOverflow -> reportStackOverflow
	other       -> reportError other

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
will get to kill the other.

If the target thread is currently making a foreign call, then the
exception will not be raised (and hence 'throwTo' will not return)
until the call has completed.  This is the case regardless of whether
the call is inside a 'block' or not.
 -}
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
\subsection[stm]{Transactional heap operations}
%*									*
%************************************************************************

TVars are shared memory locations which support atomic memory
transactions.

\begin{code}
newtype STM a = STM (State# RealWorld -> (# State# RealWorld, a #)) deriving( Typeable )

unSTM :: STM a -> (State# RealWorld -> (# State# RealWorld, a #))
unSTM (STM a) = a

instance  Functor STM where
   fmap f x = x >>= (return . f)

instance  Monad STM  where
    {-# INLINE return #-}
    {-# INLINE (>>)   #-}
    {-# INLINE (>>=)  #-}
    m >> k      = thenSTM m k
    return x	= returnSTM x
    m >>= k     = bindSTM m k

bindSTM :: STM a -> (a -> STM b) -> STM b
bindSTM (STM m) k = STM ( \s ->
  case m s of 
    (# new_s, a #) -> unSTM (k a) new_s
  )

thenSTM :: STM a -> STM b -> STM b
thenSTM (STM m) k = STM ( \s ->
  case m s of 
    (# new_s, a #) -> unSTM k new_s
  )

returnSTM :: a -> STM a
returnSTM x = STM (\s -> (# s, x #))

-- | Unsafely performs IO in the STM monad.
unsafeIOToSTM :: IO a -> STM a
unsafeIOToSTM (IO m) = STM m

-- |Perform a series of STM actions atomically.
atomically :: STM a -> IO a
atomically (STM m) = IO (\s -> (atomically# m) s )

-- |Retry execution of the current memory transaction because it has seen
-- values in TVars which mean that it should not continue (e.g. the TVars
-- represent a shared buffer that is now empty).  The implementation may
-- block the thread until one of the TVars that it has read from has been
-- udpated.
retry :: STM a
retry = STM $ \s# -> retry# s#

-- |Compose two alternative STM actions.  If the first action completes without
-- retrying then it forms the result of the orElse.  Otherwise, if the first
-- action retries, then the second action is tried in its place.  If both actions
-- retry then the orElse as a whole retries.
orElse :: STM a -> STM a -> STM a
orElse (STM m) e = STM $ \s -> catchRetry# m (unSTM e) s

-- |Exception handling within STM actions.
catchSTM :: STM a -> (Exception -> STM a) -> STM a
catchSTM (STM m) k = STM $ \s -> catchSTM# m (\ex -> unSTM (k ex)) s

data TVar a = TVar (TVar# RealWorld a) deriving( Typeable )

instance Eq (TVar a) where
	(TVar tvar1#) == (TVar tvar2#) = sameTVar# tvar1# tvar2#

-- |Create a new TVar holding a value supplied
newTVar :: a -> STM (TVar a)
newTVar val = STM $ \s1# ->
    case newTVar# val s1# of
	 (# s2#, tvar# #) -> (# s2#, TVar tvar# #)

-- |@IO@ version of 'newTVar'.  This is useful for creating top-level
-- 'TVar's using 'System.IO.Unsafe.unsafePerformIO', because using
-- 'atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
-- possible.
newTVarIO :: a -> IO (TVar a)
newTVarIO val = IO $ \s1# ->
    case newTVar# val s1# of
	 (# s2#, tvar# #) -> (# s2#, TVar tvar# #)

-- |Return the current value stored in a TVar
readTVar :: TVar a -> STM a
readTVar (TVar tvar#) = STM $ \s# -> readTVar# tvar# s#

-- |Write the supplied value into a TVar
writeTVar :: TVar a -> a -> STM ()
writeTVar (TVar tvar#) val = STM $ \s1# ->
    case writeTVar# tvar# val s1# of
    	 s2# -> (# s2#, () #)
  
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
-- There are two further important properties of 'takeMVar':
--
--   * 'takeMVar' is single-wakeup.  That is, if there are multiple
--     threads blocked in 'takeMVar', and the 'MVar' becomes full,
--     only one thread will be woken up.  The runtime guarantees that
--     the woken thread completes its 'takeMVar' operation.
--
--   * When multiple threads are blocked on an 'MVar', they are
--     woken up in FIFO order.  This is useful for providing
--     fairness properties of abstractions built using 'MVar's.
--
takeMVar :: MVar a -> IO a
takeMVar (MVar mvar#) = IO $ \ s# -> takeMVar# mvar# s#

-- |Put a value into an 'MVar'.  If the 'MVar' is currently full,
-- 'putMVar' will wait until it becomes empty.
--
-- There are two further important properties of 'putMVar':
--
--   * 'putMVar' is single-wakeup.  That is, if there are multiple
--     threads blocked in 'putMVar', and the 'MVar' becomes empty,
--     only one thread will be woken up.  The runtime guarantees that
--     the woken thread completes its 'putMVar' operation.
--
--   * When multiple threads are blocked on an 'MVar', they are
--     woken up in FIFO order.  This is useful for providing
--     fairness properties of abstractions built using 'MVar's.
--
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

\begin{code}
#ifdef mingw32_HOST_OS

-- Note: threadDelay, threadWaitRead and threadWaitWrite aren't really functional
-- on Win32, but left in there because lib code (still) uses them (the manner
-- in which they're used doesn't cause problems on a Win32 platform though.)

asyncRead :: Int -> Int -> Int -> Ptr a -> IO (Int, Int)
asyncRead  (I# fd) (I# isSock) (I# len) (Ptr buf) =
  IO $ \s -> case asyncRead# fd isSock len buf s of 
  	       (# s, len#, err# #) -> (# s, (I# len#, I# err#) #)

asyncWrite :: Int -> Int -> Int -> Ptr a -> IO (Int, Int)
asyncWrite  (I# fd) (I# isSock) (I# len) (Ptr buf) =
  IO $ \s -> case asyncWrite# fd isSock len buf s of 
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

-- -----------------------------------------------------------------------------
-- Thread IO API

-- | Block the current thread until data is available to read on the
-- given file descriptor (GHC only).
threadWaitRead :: Fd -> IO ()
threadWaitRead fd
#ifndef mingw32_HOST_OS
  | threaded  = waitForReadEvent fd
#endif
  | otherwise = IO $ \s -> 
	case fromIntegral fd of { I# fd# ->
	case waitRead# fd# s of { s -> (# s, () #)
	}}

-- | Block the current thread until data can be written to the
-- given file descriptor (GHC only).
threadWaitWrite :: Fd -> IO ()
threadWaitWrite fd
#ifndef mingw32_HOST_OS
  | threaded  = waitForWriteEvent fd
#endif
  | otherwise = IO $ \s -> 
	case fromIntegral fd of { I# fd# ->
	case waitWrite# fd# s of { s -> (# s, () #)
	}}

-- | Suspends the current thread for a given number of microseconds
-- (GHC only).
--
-- Note that the resolution used by the Haskell runtime system's
-- internal timer is 1\/50 second, and 'threadDelay' will round its
-- argument up to the nearest multiple of this resolution.
--
-- There is no guarantee that the thread will be rescheduled promptly
-- when the delay has expired, but the thread will never continue to
-- run /earlier/ than specified.
--
threadDelay :: Int -> IO ()
threadDelay time
#ifndef mingw32_HOST_OS
  | threaded  = waitForDelayEvent time
#else
  | threaded  = c_Sleep (fromIntegral (time `quot` 1000))
#endif
  | otherwise = IO $ \s -> 
	case fromIntegral time of { I# time# ->
	case delay# time# s of { s -> (# s, () #)
	}}

registerDelay usecs 
#ifndef mingw32_HOST_OS
  | threaded = waitForDelayEventSTM usecs
  | otherwise = error "registerDelay: requires -threaded"
#else
  = error "registerDelay: not currently supported on Windows"
#endif

-- On Windows, we just make a safe call to 'Sleep' to implement threadDelay.
#ifdef mingw32_HOST_OS
foreign import stdcall safe "Sleep" c_Sleep :: CInt -> IO ()
#endif

foreign import ccall unsafe "rtsSupportsBoundThreads" threaded :: Bool

-- ----------------------------------------------------------------------------
-- Threaded RTS implementation of threadWaitRead, threadWaitWrite, threadDelay

-- In the threaded RTS, we employ a single IO Manager thread to wait
-- for all outstanding IO requests (threadWaitRead,threadWaitWrite)
-- and delays (threadDelay).  
--
-- We can do this because in the threaded RTS the IO Manager can make
-- a non-blocking call to select(), so we don't have to do select() in
-- the scheduler as we have to in the non-threaded RTS.  We get performance
-- benefits from doing it this way, because we only have to restart the select()
-- when a new request arrives, rather than doing one select() each time
-- around the scheduler loop.  Furthermore, the scheduler can be simplified
-- by not having to check for completed IO requests.

-- Issues, possible problems:
--
--	- we might want bound threads to just do the blocking
--	  operation rather than communicating with the IO manager
--	  thread.  This would prevent simgle-threaded programs which do
--	  IO from requiring multiple OS threads.  However, it would also
--	  prevent bound threads waiting on IO from being killed or sent
--	  exceptions.
--
--	- Apprently exec() doesn't work on Linux in a multithreaded program.
--	  I couldn't repeat this.
--
-- 	- How do we handle signal delivery in the multithreaded RTS?
--
--	- forkProcess will kill the IO manager thread.  Let's just
--	  hope we don't need to do any blocking IO between fork & exec.

#ifndef mingw32_HOST_OS

data IOReq
  = Read   {-# UNPACK #-} !Fd {-# UNPACK #-} !(MVar ())
  | Write  {-# UNPACK #-} !Fd {-# UNPACK #-} !(MVar ())

data DelayReq
  = Delay    {-# UNPACK #-} !Int {-# UNPACK #-} !(MVar ())
  | DelaySTM {-# UNPACK #-} !Int {-# UNPACK #-} !(TVar Bool)

pendingEvents :: IORef [IOReq]
pendingDelays :: IORef [DelayReq]
	-- could use a strict list or array here
{-# NOINLINE pendingEvents #-}
{-# NOINLINE pendingDelays #-}
(pendingEvents,pendingDelays) = unsafePerformIO $ do
  startIOManagerThread
  reqs <- newIORef []
  dels <- newIORef []
  return (reqs, dels)
	-- the first time we schedule an IO request, the service thread
	-- will be created (cool, huh?)

ensureIOManagerIsRunning :: IO ()
ensureIOManagerIsRunning 
  | threaded  = seq pendingEvents $ return ()
  | otherwise = return ()

startIOManagerThread :: IO ()
startIOManagerThread = do
        allocaArray 2 $ \fds -> do
	throwErrnoIfMinus1 "startIOManagerThread" (c_pipe fds)
	rd_end <- peekElemOff fds 0
	wr_end <- peekElemOff fds 1
	writeIORef stick (fromIntegral wr_end)
	c_setIOManagerPipe wr_end
	forkIO $ do
	    allocaBytes sizeofFdSet   $ \readfds -> do
	    allocaBytes sizeofFdSet   $ \writefds -> do 
	    allocaBytes sizeofTimeVal $ \timeval -> do
	    service_loop (fromIntegral rd_end) readfds writefds timeval [] []
	return ()

service_loop
   :: Fd		-- listen to this for wakeup calls
   -> Ptr CFdSet
   -> Ptr CFdSet
   -> Ptr CTimeVal
   -> [IOReq]
   -> [DelayReq]
   -> IO ()
service_loop wakeup readfds writefds ptimeval old_reqs old_delays = do

  -- pick up new IO requests
  new_reqs <- atomicModifyIORef pendingEvents (\a -> ([],a))
  let reqs = new_reqs ++ old_reqs

  -- pick up new delay requests
  new_delays <- atomicModifyIORef pendingDelays (\a -> ([],a))
  let  delays = foldr insertDelay old_delays new_delays

  -- build the FDSets for select()
  fdZero readfds
  fdZero writefds
  fdSet wakeup readfds
  maxfd <- buildFdSets 0 readfds writefds reqs

  -- perform the select()
  let do_select delays = do
	  -- check the current time and wake up any thread in
	  -- threadDelay whose timeout has expired.  Also find the
	  -- timeout value for the select() call.
	  now <- getTicksOfDay
	  (delays', timeout) <- getDelay now ptimeval delays

	  res <- c_select ((max wakeup maxfd)+1) readfds writefds 
			nullPtr timeout
	  if (res == -1)
	     then do
		err <- getErrno
		if err == eINTR
			then do_select delays'
			else return (res,delays')
	     else
		return (res,delays')

  (res,delays') <- do_select delays
  -- ToDo: check result

  b <- fdIsSet wakeup readfds
  if b == 0 
    then return ()
    else alloca $ \p -> do 
	    c_read (fromIntegral wakeup) p 1; return ()
	    s <- peek p		
	    if (s == 0xff) 
	      then return ()
	      else do handler_tbl <- peek handlers
		      sp <- peekElemOff handler_tbl (fromIntegral s)
		      forkIO (do io <- deRefStablePtr sp; io)
		      return ()

  takeMVar prodding
  putMVar prodding False

  reqs' <- completeRequests reqs readfds writefds []
  service_loop wakeup readfds writefds ptimeval reqs' delays'

stick :: IORef Fd
{-# NOINLINE stick #-}
stick = unsafePerformIO (newIORef 0)

prodding :: MVar Bool
{-# NOINLINE prodding #-}
prodding = unsafePerformIO (newMVar False)

prodServiceThread :: IO ()
prodServiceThread = do
  b <- takeMVar prodding
  if (not b) 
    then do fd <- readIORef stick
	    with 0xff $ \pbuf -> do c_write (fromIntegral fd) pbuf 1; return ()
    else return ()
  putMVar prodding True

foreign import ccall "&signal_handlers" handlers :: Ptr (Ptr (StablePtr (IO ())))

foreign import ccall "setIOManagerPipe"
  c_setIOManagerPipe :: CInt -> IO ()

-- -----------------------------------------------------------------------------
-- IO requests

buildFdSets maxfd readfds writefds [] = return maxfd
buildFdSets maxfd readfds writefds (Read fd m : reqs)
  | fd >= fD_SETSIZE =  error "buildFdSets: file descriptor out of range"
  | otherwise        =  do
	fdSet fd readfds
        buildFdSets (max maxfd fd) readfds writefds reqs
buildFdSets maxfd readfds writefds (Write fd m : reqs)
  | fd >= fD_SETSIZE =  error "buildFdSets: file descriptor out of range"
  | otherwise        =  do
	fdSet fd writefds
	buildFdSets (max maxfd fd) readfds writefds reqs

completeRequests [] _ _ reqs' = return reqs'
completeRequests (Read fd m : reqs) readfds writefds reqs' = do
  b <- fdIsSet fd readfds
  if b /= 0
    then do putMVar m (); completeRequests reqs readfds writefds reqs'
    else completeRequests reqs readfds writefds (Read fd m : reqs')
completeRequests (Write fd m : reqs) readfds writefds reqs' = do
  b <- fdIsSet fd writefds
  if b /= 0
    then do putMVar m (); completeRequests reqs readfds writefds reqs'
    else completeRequests reqs readfds writefds (Write fd m : reqs')

waitForReadEvent :: Fd -> IO ()
waitForReadEvent fd = do
  m <- newEmptyMVar
  atomicModifyIORef pendingEvents (\xs -> (Read fd m : xs, ()))
  prodServiceThread
  takeMVar m

waitForWriteEvent :: Fd -> IO ()
waitForWriteEvent fd = do
  m <- newEmptyMVar
  atomicModifyIORef pendingEvents (\xs -> (Write fd m : xs, ()))
  prodServiceThread
  takeMVar m

-- XXX: move into GHC.IOBase from Data.IORef?
atomicModifyIORef :: IORef a -> (a -> (a,b)) -> IO b
atomicModifyIORef (IORef (STRef r#)) f = IO $ \s -> atomicModifyMutVar# r# f s

-- -----------------------------------------------------------------------------
-- Delays

waitForDelayEvent :: Int -> IO ()
waitForDelayEvent usecs = do
  m <- newEmptyMVar
  now <- getTicksOfDay
  let target = now + usecs `quot` tick_usecs
  atomicModifyIORef pendingDelays (\xs -> (Delay target m : xs, ()))
  prodServiceThread
  takeMVar m

-- Delays for use in STM
waitForDelayEventSTM :: Int -> IO (TVar Bool)
waitForDelayEventSTM usecs = do
   t <- atomically $ newTVar False
   now <- getTicksOfDay
   let target = now + usecs `quot` tick_usecs
   atomicModifyIORef pendingDelays (\xs -> (DelaySTM target t : xs, ()))
   prodServiceThread
   return t  
    
-- Walk the queue of pending delays, waking up any that have passed
-- and return the smallest delay to wait for.  The queue of pending
-- delays is kept ordered.
getDelay :: Ticks -> Ptr CTimeVal -> [DelayReq] -> IO ([DelayReq], Ptr CTimeVal)
getDelay now ptimeval [] = return ([],nullPtr)
getDelay now ptimeval all@(d : rest) 
  = case d of
     Delay time m | now >= time -> do
	putMVar m ()
	getDelay now ptimeval rest
     DelaySTM time t | now >= time -> do
	atomically $ writeTVar t True
	getDelay now ptimeval rest
     _otherwise -> do
	setTimevalTicks ptimeval (delayTime d - now)
    	return (all,ptimeval)

insertDelay :: DelayReq -> [DelayReq] -> [DelayReq]
insertDelay d [] = [d]
insertDelay d1 ds@(d2 : rest)
  | delayTime d1 <= delayTime d2 = d1 : ds
  | otherwise                    = d2 : insertDelay d1 rest

delayTime (Delay t _) = t
delayTime (DelaySTM t _) = t

type Ticks = Int
tick_freq  = 50 :: Ticks  -- accuracy of threadDelay (ticks per sec)
tick_usecs = 1000000 `quot` tick_freq :: Int

newtype CTimeVal = CTimeVal ()

foreign import ccall unsafe "sizeofTimeVal"
  sizeofTimeVal :: Int

foreign import ccall unsafe "getTicksOfDay" 
  getTicksOfDay :: IO Ticks

foreign import ccall unsafe "setTimevalTicks" 
  setTimevalTicks :: Ptr CTimeVal -> Ticks -> IO ()

-- ----------------------------------------------------------------------------
-- select() interface

-- ToDo: move to System.Posix.Internals?

newtype CFdSet = CFdSet ()

foreign import ccall safe "select"
  c_select :: Fd -> Ptr CFdSet -> Ptr CFdSet -> Ptr CFdSet -> Ptr CTimeVal
           -> IO CInt

foreign import ccall unsafe "hsFD_SETSIZE"
  fD_SETSIZE :: Fd

foreign import ccall unsafe "hsFD_CLR"
  fdClr :: Fd -> Ptr CFdSet -> IO ()

foreign import ccall unsafe "hsFD_ISSET"
  fdIsSet :: Fd -> Ptr CFdSet -> IO CInt

foreign import ccall unsafe "hsFD_SET"
  fdSet :: Fd -> Ptr CFdSet -> IO ()

foreign import ccall unsafe "hsFD_ZERO"
  fdZero :: Ptr CFdSet -> IO ()

foreign import ccall unsafe "sizeof_fd_set"
  sizeofFdSet :: Int

#endif
\end{code}

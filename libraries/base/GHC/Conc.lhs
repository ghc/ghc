\begin{code}
{-# OPTIONS_GHC -XNoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_HADDOCK not-home #-}
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

#include "Typeable.h"

-- #not-home
module GHC.Conc
        ( ThreadId(..)

        -- * Forking and suchlike
        , forkIO        -- :: IO a -> IO ThreadId
        , forkIOUnmasked
        , forkOnIO      -- :: Int -> IO a -> IO ThreadId
        , forkOnIOUnmasked
        , numCapabilities -- :: Int
        , childHandler  -- :: Exception -> IO ()
        , myThreadId    -- :: IO ThreadId
        , killThread    -- :: ThreadId -> IO ()
        , throwTo       -- :: ThreadId -> Exception -> IO ()
        , par           -- :: a -> b -> b
        , pseq          -- :: a -> b -> b
        , runSparks
        , yield         -- :: IO ()
        , labelThread   -- :: ThreadId -> String -> IO ()

        , ThreadStatus(..), BlockReason(..)
        , threadStatus  -- :: ThreadId -> IO ThreadStatus

        -- * Waiting
        , threadDelay           -- :: Int -> IO ()
        , registerDelay         -- :: Int -> IO (TVar Bool)
        , threadWaitRead        -- :: Int -> IO ()
        , threadWaitWrite       -- :: Int -> IO ()

        -- * TVars
        , STM(..)
        , atomically    -- :: STM a -> IO a
        , retry         -- :: STM a
        , orElse        -- :: STM a -> STM a -> STM a
        , catchSTM      -- :: STM a -> (Exception -> STM a) -> STM a
        , alwaysSucceeds -- :: STM a -> STM ()
        , always        -- :: STM Bool -> STM ()
        , TVar(..)
        , newTVar       -- :: a -> STM (TVar a)
        , newTVarIO     -- :: a -> STM (TVar a)
        , readTVar      -- :: TVar a -> STM a
        , readTVarIO    -- :: TVar a -> IO a
        , writeTVar     -- :: a -> TVar a -> STM ()
        , unsafeIOToSTM -- :: IO a -> STM a

        -- * Miscellaneous
        , withMVar
#ifdef mingw32_HOST_OS
        , asyncRead     -- :: Int -> Int -> Int -> Ptr a -> IO (Int, Int)
        , asyncWrite    -- :: Int -> Int -> Int -> Ptr a -> IO (Int, Int)
        , asyncDoProc   -- :: FunPtr (Ptr a -> IO Int) -> Ptr a -> IO Int

        , asyncReadBA   -- :: Int -> Int -> Int -> Int -> MutableByteArray# RealWorld -> IO (Int, Int)
        , asyncWriteBA  -- :: Int -> Int -> Int -> Int -> MutableByteArray# RealWorld -> IO (Int, Int)
#endif

#ifndef mingw32_HOST_OS
        , Signal, HandlerFun, setHandler, runHandlers
#endif

        , ensureIOManagerIsRunning
#ifndef mingw32_HOST_OS
        , syncIOManager
#endif

#ifdef mingw32_HOST_OS
        , ConsoleEvent(..)
        , win32ConsoleHandler
        , toWin32ConsoleEvent
#endif
        , setUncaughtExceptionHandler      -- :: (Exception -> IO ()) -> IO ()
        , getUncaughtExceptionHandler      -- :: IO (Exception -> IO ())

        , reportError, reportStackOverflow
        ) where

import System.Posix.Types
#ifndef mingw32_HOST_OS
import System.Posix.Internals
#endif
import Foreign
import Foreign.C

#ifdef mingw32_HOST_OS
import Data.Typeable
#endif

#ifndef mingw32_HOST_OS
import Data.Dynamic
#endif
import Control.Monad
import Data.Maybe

import GHC.Base
#ifndef mingw32_HOST_OS
import GHC.Debug
#endif
import {-# SOURCE #-} GHC.IO.Handle ( hFlush )
import {-# SOURCE #-} GHC.IO.Handle.FD ( stdout )
import GHC.IO
import GHC.IO.Exception
import GHC.Exception
import GHC.IORef
import GHC.MVar
import GHC.Num          ( Num(..) )
import GHC.Real         ( fromIntegral )
#ifndef mingw32_HOST_OS
import GHC.IOArray
import GHC.Arr          ( inRange )
#endif
#ifdef mingw32_HOST_OS
import GHC.Real         ( div )
import GHC.Ptr
#endif
#ifdef mingw32_HOST_OS
import GHC.Read         ( Read )
import GHC.Enum         ( Enum )
#endif
import GHC.Pack         ( packCString# )
import GHC.Show         ( Show(..), showString )

infixr 0 `par`, `pseq`
\end{code}

%************************************************************************
%*                                                                      *
\subsection{@ThreadId@, @par@, and @fork@}
%*                                                                      *
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

instance Show ThreadId where
   showsPrec d t = 
        showString "ThreadId " . 
        showsPrec d (getThreadId (id2TSO t))

foreign import ccall unsafe "rts_getThreadId" getThreadId :: ThreadId# -> CInt

id2TSO :: ThreadId -> ThreadId#
id2TSO (ThreadId t) = t

foreign import ccall unsafe "cmp_thread" cmp_thread :: ThreadId# -> ThreadId# -> CInt
-- Returns -1, 0, 1

cmpThread :: ThreadId -> ThreadId -> Ordering
cmpThread t1 t2 = 
   case cmp_thread (id2TSO t1) (id2TSO t2) of
      -1 -> LT
      0  -> EQ
      _  -> GT -- must be 1

instance Eq ThreadId where
   t1 == t2 = 
      case t1 `cmpThread` t2 of
         EQ -> True
         _  -> False

instance Ord ThreadId where
   compare = cmpThread

{- |
Sparks off a new thread to run the 'IO' computation passed as the
first argument, and returns the 'ThreadId' of the newly created
thread.

The new thread will be a lightweight thread; if you want to use a foreign
library that uses thread-local storage, use 'Control.Concurrent.forkOS' instead.

GHC note: the new thread inherits the /masked/ state of the parent 
(see 'Control.Exception.mask').

The newly created thread has an exception handler that discards the
exceptions 'BlockedIndefinitelyOnMVar', 'BlockedIndefinitelyOnSTM', and
'ThreadKilled', and passes all other exceptions to the uncaught
exception handler (see 'setUncaughtExceptionHandler').
-}
forkIO :: IO () -> IO ThreadId
forkIO action = IO $ \ s -> 
   case (fork# action_plus s) of (# s1, tid #) -> (# s1, ThreadId tid #)
 where
  action_plus = catchException action childHandler

-- | Like 'forkIO', but the child thread is created with asynchronous exceptions
-- unmasked (see 'Control.Exception.mask').
forkIOUnmasked :: IO () -> IO ThreadId
forkIOUnmasked io = forkIO (unsafeUnmask io)

{- |
Like 'forkIO', but lets you specify on which CPU the thread is
created.  Unlike a `forkIO` thread, a thread created by `forkOnIO`
will stay on the same CPU for its entire lifetime (`forkIO` threads
can migrate between CPUs according to the scheduling policy).
`forkOnIO` is useful for overriding the scheduling policy when you
know in advance how best to distribute the threads.

The `Int` argument specifies the CPU number; it is interpreted modulo
'numCapabilities' (note that it actually specifies a capability number
rather than a CPU number, but to a first approximation the two are
equivalent).
-}
forkOnIO :: Int -> IO () -> IO ThreadId
forkOnIO (I# cpu) action = IO $ \ s -> 
   case (forkOn# cpu action_plus s) of (# s1, tid #) -> (# s1, ThreadId tid #)
 where
  action_plus = catchException action childHandler

-- | Like 'forkOnIO', but the child thread is created with
-- asynchronous exceptions unmasked (see 'Control.Exception.mask').
forkOnIOUnmasked :: Int -> IO () -> IO ThreadId
forkOnIOUnmasked cpu io = forkOnIO cpu (unsafeUnmask io)

-- | the value passed to the @+RTS -N@ flag.  This is the number of
-- Haskell threads that can run truly simultaneously at any given
-- time, and is typically set to the number of physical CPU cores on
-- the machine.
numCapabilities :: Int
numCapabilities = unsafePerformIO $  do 
                    n <- peek n_capabilities
                    return (fromIntegral n)

#if defined(mingw32_HOST_OS) && defined(__PIC__)
foreign import ccall "_imp__n_capabilities" n_capabilities :: Ptr CInt
#else
foreign import ccall "&n_capabilities" n_capabilities :: Ptr CInt
#endif
childHandler :: SomeException -> IO ()
childHandler err = catchException (real_handler err) childHandler

real_handler :: SomeException -> IO ()
real_handler se@(SomeException ex) =
  -- ignore thread GC and killThread exceptions:
  case cast ex of
  Just BlockedIndefinitelyOnMVar        -> return ()
  _ -> case cast ex of
       Just BlockedIndefinitelyOnSTM    -> return ()
       _ -> case cast ex of
            Just ThreadKilled           -> return ()
            _ -> case cast ex of
                 -- report all others:
                 Just StackOverflow     -> reportStackOverflow
                 _                      -> reportError se

{- | 'killThread' raises the 'ThreadKilled' exception in the given
thread (GHC only). 

> killThread tid = throwTo tid ThreadKilled

-}
killThread :: ThreadId -> IO ()
killThread tid = throwTo tid ThreadKilled

{- | 'throwTo' raises an arbitrary exception in the target thread (GHC only).

'throwTo' does not return until the exception has been raised in the
target thread. 
The calling thread can thus be certain that the target
thread has received the exception.  This is a useful property to know
when dealing with race conditions: eg. if there are two threads that
can kill each other, it is guaranteed that only one of the threads
will get to kill the other.

Whatever work the target thread was doing when the exception was
raised is not lost: the computation is suspended until required by
another thread.

If the target thread is currently making a foreign call, then the
exception will not be raised (and hence 'throwTo' will not return)
until the call has completed.  This is the case regardless of whether
the call is inside a 'mask' or not.

Important note: the behaviour of 'throwTo' differs from that described in
the paper \"Asynchronous exceptions in Haskell\"
(<http://research.microsoft.com/~simonpj/Papers/asynch-exns.htm>).
In the paper, 'throwTo' is non-blocking; but the library implementation adopts
a more synchronous design in which 'throwTo' does not return until the exception
is received by the target thread.  The trade-off is discussed in Section 9 of the paper.
Like any blocking operation, 'throwTo' is therefore interruptible (see Section 5.3 of
the paper).  Unlike other interruptible operations, however, 'throwTo'
is /always/ interruptible, even if it does not actually block.

There is no guarantee that the exception will be delivered promptly,
although the runtime will endeavour to ensure that arbitrary
delays don't occur.  In GHC, an exception can only be raised when a
thread reaches a /safe point/, where a safe point is where memory
allocation occurs.  Some loops do not perform any memory allocation
inside the loop and therefore cannot be interrupted by a 'throwTo'.

Blocked 'throwTo' is fair: if multiple threads are trying to throw an
exception to the same target thread, they will succeed in FIFO order.

  -}
throwTo :: Exception e => ThreadId -> e -> IO ()
throwTo (ThreadId tid) ex = IO $ \ s ->
   case (killThread# tid (toException ex) s) of s1 -> (# s1, () #)

-- | Returns the 'ThreadId' of the calling thread (GHC only).
myThreadId :: IO ThreadId
myThreadId = IO $ \s ->
   case (myThreadId# s) of (# s1, tid #) -> (# s1, ThreadId tid #)


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
   let !ps  = packCString# str
       !adr = byteArrayContents# ps in
     case (labelThread# t adr s) of s1 -> (# s1, () #)

--      Nota Bene: 'pseq' used to be 'seq'
--                 but 'seq' is now defined in PrelGHC
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

-- | Internal function used by the RTS to run sparks.
runSparks :: IO ()
runSparks = IO loop
  where loop s = case getSpark# s of
                   (# s', n, p #) ->
                      if n ==# 0# then (# s', () #)
                                  else p `seq` loop s'

data BlockReason
  = BlockedOnMVar
        -- ^blocked on on 'MVar'
  | BlockedOnBlackHole
        -- ^blocked on a computation in progress by another thread
  | BlockedOnException
        -- ^blocked in 'throwTo'
  | BlockedOnSTM
        -- ^blocked in 'retry' in an STM transaction
  | BlockedOnForeignCall
        -- ^currently in a foreign call
  | BlockedOnOther
        -- ^blocked on some other resource.  Without @-threaded@,
        -- I\/O and 'threadDelay' show up as 'BlockedOnOther', with @-threaded@
        -- they show up as 'BlockedOnMVar'.
  deriving (Eq,Ord,Show)

-- | The current status of a thread
data ThreadStatus
  = ThreadRunning
        -- ^the thread is currently runnable or running
  | ThreadFinished
        -- ^the thread has finished
  | ThreadBlocked  BlockReason
        -- ^the thread is blocked on some resource
  | ThreadDied
        -- ^the thread received an uncaught exception
  deriving (Eq,Ord,Show)

threadStatus :: ThreadId -> IO ThreadStatus
threadStatus (ThreadId t) = IO $ \s ->
   case threadStatus# t s of
     (# s', stat #) -> (# s', mk_stat (I# stat) #)
   where
        -- NB. keep these in sync with includes/Constants.h
     mk_stat 0  = ThreadRunning
     mk_stat 1  = ThreadBlocked BlockedOnMVar
     mk_stat 2  = ThreadBlocked BlockedOnBlackHole
     mk_stat 3  = ThreadBlocked BlockedOnException
     mk_stat 7  = ThreadBlocked BlockedOnSTM
     mk_stat 11 = ThreadBlocked BlockedOnForeignCall
     mk_stat 12 = ThreadBlocked BlockedOnForeignCall
     mk_stat 16 = ThreadFinished
     mk_stat 17 = ThreadDied
     mk_stat _  = ThreadBlocked BlockedOnOther
\end{code}


%************************************************************************
%*                                                                      *
\subsection[stm]{Transactional heap operations}
%*                                                                      *
%************************************************************************

TVars are shared memory locations which support atomic memory
transactions.

\begin{code}
-- |A monad supporting atomic memory transactions.
newtype STM a = STM (State# RealWorld -> (# State# RealWorld, a #))

unSTM :: STM a -> (State# RealWorld -> (# State# RealWorld, a #))
unSTM (STM a) = a

INSTANCE_TYPEABLE1(STM,stmTc,"STM")

instance  Functor STM where
   fmap f x = x >>= (return . f)

instance  Monad STM  where
    {-# INLINE return #-}
    {-# INLINE (>>)   #-}
    {-# INLINE (>>=)  #-}
    m >> k      = thenSTM m k
    return x    = returnSTM x
    m >>= k     = bindSTM m k

bindSTM :: STM a -> (a -> STM b) -> STM b
bindSTM (STM m) k = STM ( \s ->
  case m s of 
    (# new_s, a #) -> unSTM (k a) new_s
  )

thenSTM :: STM a -> STM b -> STM b
thenSTM (STM m) k = STM ( \s ->
  case m s of 
    (# new_s, _ #) -> unSTM k new_s
  )

returnSTM :: a -> STM a
returnSTM x = STM (\s -> (# s, x #))

instance MonadPlus STM where
  mzero = retry
  mplus = orElse

-- | Unsafely performs IO in the STM monad.  Beware: this is a highly
-- dangerous thing to do.  
--
--   * The STM implementation will often run transactions multiple
--     times, so you need to be prepared for this if your IO has any
--     side effects.
--
--   * The STM implementation will abort transactions that are known to
--     be invalid and need to be restarted.  This may happen in the middle
--     of `unsafeIOToSTM`, so make sure you don't acquire any resources
--     that need releasing (exception handlers are ignored when aborting
--     the transaction).  That includes doing any IO using Handles, for
--     example.  Getting this wrong will probably lead to random deadlocks.
--
--   * The transaction may have seen an inconsistent view of memory when
--     the IO runs.  Invariants that you expect to be true throughout
--     your program may not be true inside a transaction, due to the
--     way transactions are implemented.  Normally this wouldn't be visible
--     to the programmer, but using `unsafeIOToSTM` can expose it.
--
unsafeIOToSTM :: IO a -> STM a
unsafeIOToSTM (IO m) = STM m

-- |Perform a series of STM actions atomically.
--
-- You cannot use 'atomically' inside an 'unsafePerformIO' or 'unsafeInterleaveIO'. 
-- Any attempt to do so will result in a runtime error.  (Reason: allowing
-- this would effectively allow a transaction inside a transaction, depending
-- on exactly when the thunk is evaluated.)
--
-- However, see 'newTVarIO', which can be called inside 'unsafePerformIO',
-- and which allows top-level TVars to be allocated.

atomically :: STM a -> IO a
atomically (STM m) = IO (\s -> (atomically# m) s )

-- |Retry execution of the current memory transaction because it has seen
-- values in TVars which mean that it should not continue (e.g. the TVars
-- represent a shared buffer that is now empty).  The implementation may
-- block the thread until one of the TVars that it has read from has been
-- udpated. (GHC only)
retry :: STM a
retry = STM $ \s# -> retry# s#

-- |Compose two alternative STM actions (GHC only).  If the first action
-- completes without retrying then it forms the result of the orElse.
-- Otherwise, if the first action retries, then the second action is
-- tried in its place.  If both actions retry then the orElse as a
-- whole retries.
orElse :: STM a -> STM a -> STM a
orElse (STM m) e = STM $ \s -> catchRetry# m (unSTM e) s

-- |Exception handling within STM actions.
catchSTM :: STM a -> (SomeException -> STM a) -> STM a
catchSTM (STM m) k = STM $ \s -> catchSTM# m (\ex -> unSTM (k ex)) s

-- | Low-level primitive on which always and alwaysSucceeds are built.
-- checkInv differs form these in that (i) the invariant is not 
-- checked when checkInv is called, only at the end of this and
-- subsequent transcations, (ii) the invariant failure is indicated
-- by raising an exception.
checkInv :: STM a -> STM ()
checkInv (STM m) = STM (\s -> (check# m) s)

-- | alwaysSucceeds adds a new invariant that must be true when passed
-- to alwaysSucceeds, at the end of the current transaction, and at
-- the end of every subsequent transaction.  If it fails at any
-- of those points then the transaction violating it is aborted
-- and the exception raised by the invariant is propagated.
alwaysSucceeds :: STM a -> STM ()
alwaysSucceeds i = do ( i >> retry ) `orElse` ( return () ) 
                      checkInv i

-- | always is a variant of alwaysSucceeds in which the invariant is
-- expressed as an STM Bool action that must return True.  Returning
-- False or raising an exception are both treated as invariant failures.
always :: STM Bool -> STM ()
always i = alwaysSucceeds ( do v <- i
                               if (v) then return () else ( error "Transacional invariant violation" ) )

-- |Shared memory locations that support atomic memory transactions.
data TVar a = TVar (TVar# RealWorld a)

INSTANCE_TYPEABLE1(TVar,tvarTc,"TVar")

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

-- |Return the current value stored in a TVar.
-- This is equivalent to
--
-- >  readTVarIO = atomically . readTVar
--
-- but works much faster, because it doesn't perform a complete
-- transaction, it just reads the current value of the 'TVar'.
readTVarIO :: TVar a -> IO a
readTVarIO (TVar tvar#) = IO $ \s# -> readTVarIO# tvar# s#

-- |Return the current value stored in a TVar
readTVar :: TVar a -> STM a
readTVar (TVar tvar#) = STM $ \s# -> readTVar# tvar# s#

-- |Write the supplied value into a TVar
writeTVar :: TVar a -> a -> STM ()
writeTVar (TVar tvar#) val = STM $ \s1# ->
    case writeTVar# tvar# val s1# of
         s2# -> (# s2#, () #)
  
\end{code}

MVar utilities

\begin{code}
withMVar :: MVar a -> (a -> IO b) -> IO b
withMVar m io = 
  mask $ \restore -> do
    a <- takeMVar m
    b <- catchAny (restore (io a))
            (\e -> do putMVar m a; throw e)
    putMVar m a
    return b

modifyMVar_ :: MVar a -> (a -> IO a) -> IO ()
modifyMVar_ m io =
  mask $ \restore -> do
    a <- takeMVar m
    a' <- catchAny (restore (io a))
            (\e -> do putMVar m a; throw e)
    putMVar m a'
    return ()
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Thread waiting}
%*                                                                      *
%************************************************************************

\begin{code}
#ifdef mingw32_HOST_OS

-- Note: threadWaitRead and threadWaitWrite aren't really functional
-- on Win32, but left in there because lib code (still) uses them (the manner
-- in which they're used doesn't cause problems on a Win32 platform though.)

asyncRead :: Int -> Int -> Int -> Ptr a -> IO (Int, Int)
asyncRead  (I# fd) (I# isSock) (I# len) (Ptr buf) =
  IO $ \s -> case asyncRead# fd isSock len buf s of 
               (# s', len#, err# #) -> (# s', (I# len#, I# err#) #)

asyncWrite :: Int -> Int -> Int -> Ptr a -> IO (Int, Int)
asyncWrite  (I# fd) (I# isSock) (I# len) (Ptr buf) =
  IO $ \s -> case asyncWrite# fd isSock len buf s of 
               (# s', len#, err# #) -> (# s', (I# len#, I# err#) #)

asyncDoProc :: FunPtr (Ptr a -> IO Int) -> Ptr a -> IO Int
asyncDoProc (FunPtr proc) (Ptr param) = 
    -- the 'length' value is ignored; simplifies implementation of
    -- the async*# primops to have them all return the same result.
  IO $ \s -> case asyncDoProc# proc param s  of 
               (# s', _len#, err# #) -> (# s', I# err# #)

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
        case waitRead# fd# s of { s' -> (# s', () #)
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
        case waitWrite# fd# s of { s' -> (# s', () #)
        }}

-- | Suspends the current thread for a given number of microseconds
-- (GHC only).
--
-- There is no guarantee that the thread will be rescheduled promptly
-- when the delay has expired, but the thread will never continue to
-- run /earlier/ than specified.
--
threadDelay :: Int -> IO ()
threadDelay time
  | threaded  = waitForDelayEvent time
  | otherwise = IO $ \s -> 
        case fromIntegral time of { I# time# ->
        case delay# time# s of { s' -> (# s', () #)
        }}


-- | Set the value of returned TVar to True after a given number of
-- microseconds. The caveats associated with threadDelay also apply.
--
registerDelay :: Int -> IO (TVar Bool)
registerDelay usecs 
  | threaded = waitForDelayEventSTM usecs
  | otherwise = error "registerDelay: requires -threaded"

foreign import ccall unsafe "rtsSupportsBoundThreads" threaded :: Bool

waitForDelayEvent :: Int -> IO ()
waitForDelayEvent usecs = do
  m <- newEmptyMVar
  target <- calculateTarget usecs
  atomicModifyIORef pendingDelays (\xs -> (Delay target m : xs, ()))
  prodServiceThread
  takeMVar m

-- Delays for use in STM
waitForDelayEventSTM :: Int -> IO (TVar Bool)
waitForDelayEventSTM usecs = do
   t <- atomically $ newTVar False
   target <- calculateTarget usecs
   atomicModifyIORef pendingDelays (\xs -> (DelaySTM target t : xs, ()))
   prodServiceThread
   return t  
    
calculateTarget :: Int -> IO USecs
calculateTarget usecs = do
    now <- getUSecOfDay
    return $ now + (fromIntegral usecs)


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

#ifndef mingw32_HOST_OS
data IOReq
  = Read   {-# UNPACK #-} !Fd {-# UNPACK #-} !(MVar ())
  | Write  {-# UNPACK #-} !Fd {-# UNPACK #-} !(MVar ())
#endif

data DelayReq
  = Delay    {-# UNPACK #-} !USecs {-# UNPACK #-} !(MVar ())
  | DelaySTM {-# UNPACK #-} !USecs {-# UNPACK #-} !(TVar Bool)

#ifndef mingw32_HOST_OS
{-# NOINLINE pendingEvents #-}
pendingEvents :: IORef [IOReq]
pendingEvents = unsafePerformIO $ do
   m <- newIORef []
   sharedCAF m getOrSetGHCConcPendingEventsStore

foreign import ccall unsafe "getOrSetGHCConcPendingEventsStore"
    getOrSetGHCConcPendingEventsStore :: Ptr a -> IO (Ptr a)
#endif

{-# NOINLINE pendingDelays #-}
pendingDelays :: IORef [DelayReq]
pendingDelays = unsafePerformIO $ do
   m <- newIORef []
   sharedCAF m getOrSetGHCConcPendingDelaysStore

foreign import ccall unsafe "getOrSetGHCConcPendingDelaysStore"
    getOrSetGHCConcPendingDelaysStore :: Ptr a -> IO (Ptr a)

{-# NOINLINE ioManagerThread #-}
ioManagerThread :: MVar (Maybe ThreadId)
ioManagerThread = unsafePerformIO $ do
   m <- newMVar Nothing
   sharedCAF m getOrSetGHCConcIOManagerThreadStore

foreign import ccall unsafe "getOrSetGHCConcIOManagerThreadStore"
    getOrSetGHCConcIOManagerThreadStore :: Ptr a -> IO (Ptr a)

ensureIOManagerIsRunning :: IO ()
ensureIOManagerIsRunning 
  | threaded  = startIOManagerThread
  | otherwise = return ()

startIOManagerThread :: IO ()
startIOManagerThread = do
  modifyMVar_ ioManagerThread $ \old -> do
    let create = do t <- forkIO ioManager; return (Just t)
    case old of
      Nothing -> create
      Just t  -> do
        s <- threadStatus t
        case s of
          ThreadFinished -> create
          ThreadDied     -> create
          _other         -> return (Just t)

insertDelay :: DelayReq -> [DelayReq] -> [DelayReq]
insertDelay d [] = [d]
insertDelay d1 ds@(d2 : rest)
  | delayTime d1 <= delayTime d2 = d1 : ds
  | otherwise                    = d2 : insertDelay d1 rest

delayTime :: DelayReq -> USecs
delayTime (Delay t _) = t
delayTime (DelaySTM t _) = t

type USecs = Word64

foreign import ccall unsafe "getUSecOfDay" 
  getUSecOfDay :: IO USecs

{-# NOINLINE prodding #-}
prodding :: IORef Bool
prodding = unsafePerformIO $ do
   r <- newIORef False
   sharedCAF r getOrSetGHCConcProddingStore

foreign import ccall unsafe "getOrSetGHCConcProddingStore"
    getOrSetGHCConcProddingStore :: Ptr a -> IO (Ptr a)

prodServiceThread :: IO ()
prodServiceThread = do
  -- NB. use atomicModifyIORef here, otherwise there are race
  -- conditions in which prodding is left at True but the server is
  -- blocked in select().
  was_set <- atomicModifyIORef prodding $ \b -> (True,b)
  unless was_set wakeupIOManager

-- Machinery needed to ensure that we only have one copy of certain
-- CAFs in this module even when the base package is present twice, as
-- it is when base is dynamically loaded into GHCi.  The RTS keeps
-- track of the single true value of the CAF, so even when the CAFs in
-- the dynamically-loaded base package are reverted, nothing bad
-- happens.
--
sharedCAF :: a -> (Ptr a -> IO (Ptr a)) -> IO a
sharedCAF a get_or_set =
   mask_ $ do
     stable_ref <- newStablePtr a
     let ref = castPtr (castStablePtrToPtr stable_ref)
     ref2 <- get_or_set ref
     if ref==ref2
        then return a
        else do freeStablePtr stable_ref
                deRefStablePtr (castPtrToStablePtr (castPtr ref2))

#ifdef mingw32_HOST_OS
-- ----------------------------------------------------------------------------
-- Windows IO manager thread

ioManager :: IO ()
ioManager = do
  wakeup <- c_getIOManagerEvent
  service_loop wakeup []

service_loop :: HANDLE          -- read end of pipe
             -> [DelayReq]      -- current delay requests
             -> IO ()

service_loop wakeup old_delays = do
  -- pick up new delay requests
  new_delays <- atomicModifyIORef pendingDelays (\a -> ([],a))
  let  delays = foldr insertDelay old_delays new_delays

  now <- getUSecOfDay
  (delays', timeout) <- getDelay now delays

  r <- c_WaitForSingleObject wakeup timeout
  case r of
    0xffffffff -> do c_maperrno; throwErrno "service_loop"
    0 -> do
        r2 <- c_readIOManagerEvent
        exit <- 
              case r2 of
                _ | r2 == io_MANAGER_WAKEUP -> return False
                _ | r2 == io_MANAGER_DIE    -> return True
                0 -> return False -- spurious wakeup
                _ -> do start_console_handler (r2 `shiftR` 1); return False
        unless exit $ service_cont wakeup delays'

    _other -> service_cont wakeup delays' -- probably timeout        

service_cont :: HANDLE -> [DelayReq] -> IO ()
service_cont wakeup delays = do
  r <- atomicModifyIORef prodding (\_ -> (False,False))
  r `seq` return () -- avoid space leak
  service_loop wakeup delays

-- must agree with rts/win32/ThrIOManager.c
io_MANAGER_WAKEUP, io_MANAGER_DIE :: Word32
io_MANAGER_WAKEUP = 0xffffffff
io_MANAGER_DIE    = 0xfffffffe

data ConsoleEvent
 = ControlC
 | Break
 | Close
    -- these are sent to Services only.
 | Logoff
 | Shutdown
 deriving (Eq, Ord, Enum, Show, Read, Typeable)

start_console_handler :: Word32 -> IO ()
start_console_handler r =
  case toWin32ConsoleEvent r of
     Just x  -> withMVar win32ConsoleHandler $ \handler -> do
                    _ <- forkIO (handler x)
                    return ()
     Nothing -> return ()

toWin32ConsoleEvent :: Num a => a -> Maybe ConsoleEvent
toWin32ConsoleEvent ev = 
   case ev of
       0 {- CTRL_C_EVENT-}        -> Just ControlC
       1 {- CTRL_BREAK_EVENT-}    -> Just Break
       2 {- CTRL_CLOSE_EVENT-}    -> Just Close
       5 {- CTRL_LOGOFF_EVENT-}   -> Just Logoff
       6 {- CTRL_SHUTDOWN_EVENT-} -> Just Shutdown
       _ -> Nothing

win32ConsoleHandler :: MVar (ConsoleEvent -> IO ())
win32ConsoleHandler = unsafePerformIO (newMVar (error "win32ConsoleHandler"))

wakeupIOManager :: IO ()
wakeupIOManager = c_sendIOManagerEvent io_MANAGER_WAKEUP

-- Walk the queue of pending delays, waking up any that have passed
-- and return the smallest delay to wait for.  The queue of pending
-- delays is kept ordered.
getDelay :: USecs -> [DelayReq] -> IO ([DelayReq], DWORD)
getDelay _   [] = return ([], iNFINITE)
getDelay now all@(d : rest) 
  = case d of
     Delay time m | now >= time -> do
        putMVar m ()
        getDelay now rest
     DelaySTM time t | now >= time -> do
        atomically $ writeTVar t True
        getDelay now rest
     _otherwise ->
        -- delay is in millisecs for WaitForSingleObject
        let micro_seconds = delayTime d - now
            milli_seconds = (micro_seconds + 999) `div` 1000
        in return (all, fromIntegral milli_seconds)

-- ToDo: this just duplicates part of System.Win32.Types, which isn't
-- available yet.  We should move some Win32 functionality down here,
-- maybe as part of the grand reorganisation of the base package...
type HANDLE       = Ptr ()
type DWORD        = Word32

iNFINITE :: DWORD
iNFINITE = 0xFFFFFFFF -- urgh

foreign import ccall unsafe "getIOManagerEvent" -- in the RTS (ThrIOManager.c)
  c_getIOManagerEvent :: IO HANDLE

foreign import ccall unsafe "readIOManagerEvent" -- in the RTS (ThrIOManager.c)
  c_readIOManagerEvent :: IO Word32

foreign import ccall unsafe "sendIOManagerEvent" -- in the RTS (ThrIOManager.c)
  c_sendIOManagerEvent :: Word32 -> IO ()

foreign import ccall unsafe "maperrno"             -- in Win32Utils.c
   c_maperrno :: IO ()

foreign import stdcall "WaitForSingleObject"
   c_WaitForSingleObject :: HANDLE -> DWORD -> IO DWORD

#else
-- ----------------------------------------------------------------------------
-- Unix IO manager thread, using select()

ioManager :: IO ()
ioManager = do
        allocaArray 2 $ \fds -> do
        throwErrnoIfMinus1_ "startIOManagerThread" (c_pipe fds)
        rd_end <- peekElemOff fds 0
        wr_end <- peekElemOff fds 1
        setNonBlockingFD wr_end True -- writes happen in a signal handler, we
                                     -- don't want them to block.
        setCloseOnExec rd_end
        setCloseOnExec wr_end
        c_setIOManagerPipe wr_end
        allocaBytes sizeofFdSet   $ \readfds -> do
        allocaBytes sizeofFdSet   $ \writefds -> do 
        allocaBytes sizeofTimeVal $ \timeval -> do
        service_loop (fromIntegral rd_end) readfds writefds timeval [] []
        return ()

service_loop
   :: Fd                -- listen to this for wakeup calls
   -> Ptr CFdSet
   -> Ptr CFdSet
   -> Ptr CTimeVal
   -> [IOReq]
   -> [DelayReq]
   -> IO ()
service_loop wakeup readfds writefds ptimeval old_reqs old_delays = do

  -- reset prodding before we look at the new requests.  If a new
  -- client arrives after this point they will send a wakup which will
  -- cause the server to loop around again, so we can be sure to not
  -- miss any requests.
  --
  -- NB. it's important to do this in the *first* iteration of
  -- service_loop, rather than after calling select(), since a client
  -- may have set prodding to True without sending a wakeup byte down
  -- the pipe, because the pipe wasn't set up.
  atomicModifyIORef prodding (\_ -> (False, ()))

  -- pick up new IO requests
  new_reqs <- atomicModifyIORef pendingEvents (\a -> ([],a))
  let reqs = new_reqs ++ old_reqs

  -- pick up new delay requests
  new_delays <- atomicModifyIORef pendingDelays (\a -> ([],a))
  let  delays0 = foldr insertDelay old_delays new_delays

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
          now <- getUSecOfDay
          (delays', timeout) <- getDelay now ptimeval delays

          res <- c_select (fromIntegral ((max wakeup maxfd)+1)) readfds writefds 
                        nullPtr timeout
          if (res == -1)
             then do
                err <- getErrno
                case err of
                  _ | err == eINTR ->  do_select delays'
                        -- EINTR: just redo the select()
                  _ | err == eBADF ->  return (True, delays)
                        -- EBADF: one of the file descriptors is closed or bad,
                        -- we don't know which one, so wake everyone up.
                  _ | otherwise    ->  throwErrno "select"
                        -- otherwise (ENOMEM or EINVAL) something has gone
                        -- wrong; report the error.
             else
                return (False,delays')

  (wakeup_all,delays') <- do_select delays0

  exit <-
    if wakeup_all then return False
      else do
        b <- fdIsSet wakeup readfds
        if b == 0 
          then return False
          else alloca $ \p -> do 
                 warnErrnoIfMinus1_ "service_loop" $
                     c_read (fromIntegral wakeup) p 1
                 s <- peek p            
                 case s of
                  _ | s == io_MANAGER_WAKEUP -> return False
                  _ | s == io_MANAGER_DIE    -> return True
                  _ | s == io_MANAGER_SYNC   -> do
                       mvars <- readIORef sync
                       mapM_ (flip putMVar ()) mvars
                       return False
                  _ -> do
                       fp <- mallocForeignPtrBytes (fromIntegral sizeof_siginfo_t)
                       withForeignPtr fp $ \p_siginfo -> do
                         r <- c_read (fromIntegral wakeup) (castPtr p_siginfo)
                                 sizeof_siginfo_t
                         when (r /= fromIntegral sizeof_siginfo_t) $
                            error "failed to read siginfo_t"
                       runHandlers' fp (fromIntegral s)
                       return False

  unless exit $ do

  reqs' <- if wakeup_all then do wakeupAll reqs; return []
                         else completeRequests reqs readfds writefds []

  service_loop wakeup readfds writefds ptimeval reqs' delays'

io_MANAGER_WAKEUP, io_MANAGER_DIE, io_MANAGER_SYNC :: Word8
io_MANAGER_WAKEUP = 0xff
io_MANAGER_DIE    = 0xfe
io_MANAGER_SYNC   = 0xfd

{-# NOINLINE sync #-}
sync :: IORef [MVar ()]
sync = unsafePerformIO (newIORef [])

-- waits for the IO manager to drain the pipe
syncIOManager :: IO ()
syncIOManager = do
  m <- newEmptyMVar
  atomicModifyIORef sync (\old -> (m:old,()))
  c_ioManagerSync
  takeMVar m

foreign import ccall unsafe "ioManagerSync"   c_ioManagerSync :: IO ()
foreign import ccall unsafe "ioManagerWakeup" wakeupIOManager :: IO ()

-- For the non-threaded RTS
runHandlers :: Ptr Word8 -> Int -> IO ()
runHandlers p_info sig = do
  fp <- mallocForeignPtrBytes (fromIntegral sizeof_siginfo_t)
  withForeignPtr fp $ \p -> do
    copyBytes p p_info (fromIntegral sizeof_siginfo_t)
    free p_info
  runHandlers' fp (fromIntegral sig)

runHandlers' :: ForeignPtr Word8 -> Signal -> IO ()
runHandlers' p_info sig = do
  let int = fromIntegral sig
  withMVar signal_handlers $ \arr ->
      if not (inRange (boundsIOArray arr) int)
         then return ()
         else do handler <- unsafeReadIOArray arr int
                 case handler of
                    Nothing -> return ()
                    Just (f,_)  -> do _ <- forkIO (f p_info)
                                      return ()

warnErrnoIfMinus1_ :: Num a => String -> IO a -> IO ()
warnErrnoIfMinus1_ what io
    = do r <- io
         when (r == -1) $ do
             errno <- getErrno
             str <- strerror errno >>= peekCString
             when (r == -1) $
                 debugErrLn ("Warning: " ++ what ++ " failed: " ++ str)

foreign import ccall unsafe "string.h" strerror :: Errno -> IO (Ptr CChar)

foreign import ccall "setIOManagerPipe"
  c_setIOManagerPipe :: CInt -> IO ()

foreign import ccall "__hscore_sizeof_siginfo_t"
  sizeof_siginfo_t :: CSize

type Signal = CInt

maxSig = 64 :: Int

type HandlerFun = ForeignPtr Word8 -> IO ()

-- Lock used to protect concurrent access to signal_handlers.  Symptom of
-- this race condition is #1922, although that bug was on Windows a similar
-- bug also exists on Unix.
{-# NOINLINE signal_handlers #-}
signal_handlers :: MVar (IOArray Int (Maybe (HandlerFun,Dynamic)))
signal_handlers = unsafePerformIO $ do
   arr <- newIOArray (0,maxSig) Nothing
   m <- newMVar arr
   sharedCAF m getOrSetGHCConcSignalHandlerStore

foreign import ccall unsafe "getOrSetGHCConcSignalHandlerStore"
    getOrSetGHCConcSignalHandlerStore :: Ptr a -> IO (Ptr a)

setHandler :: Signal -> Maybe (HandlerFun,Dynamic) -> IO (Maybe (HandlerFun,Dynamic))
setHandler sig handler = do
  let int = fromIntegral sig
  withMVar signal_handlers $ \arr -> 
     if not (inRange (boundsIOArray arr) int)
        then error "GHC.Conc.setHandler: signal out of range"
        else do old <- unsafeReadIOArray arr int
                unsafeWriteIOArray arr int handler
                return old

-- -----------------------------------------------------------------------------
-- IO requests

buildFdSets :: Fd -> Ptr CFdSet -> Ptr CFdSet -> [IOReq] -> IO Fd
buildFdSets maxfd _       _        [] = return maxfd
buildFdSets maxfd readfds writefds (Read fd _ : reqs)
  | fd >= fD_SETSIZE =  error "buildFdSets: file descriptor out of range"
  | otherwise        =  do
        fdSet fd readfds
        buildFdSets (max maxfd fd) readfds writefds reqs
buildFdSets maxfd readfds writefds (Write fd _ : reqs)
  | fd >= fD_SETSIZE =  error "buildFdSets: file descriptor out of range"
  | otherwise        =  do
        fdSet fd writefds
        buildFdSets (max maxfd fd) readfds writefds reqs

completeRequests :: [IOReq] -> Ptr CFdSet -> Ptr CFdSet -> [IOReq]
                 -> IO [IOReq]
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

wakeupAll :: [IOReq] -> IO ()
wakeupAll [] = return ()
wakeupAll (Read  _ m : reqs) = do putMVar m (); wakeupAll reqs
wakeupAll (Write _ m : reqs) = do putMVar m (); wakeupAll reqs

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

-- -----------------------------------------------------------------------------
-- Delays

-- Walk the queue of pending delays, waking up any that have passed
-- and return the smallest delay to wait for.  The queue of pending
-- delays is kept ordered.
getDelay :: USecs -> Ptr CTimeVal -> [DelayReq] -> IO ([DelayReq], Ptr CTimeVal)
getDelay _   _        [] = return ([],nullPtr)
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

data CTimeVal

foreign import ccall unsafe "sizeofTimeVal"
  sizeofTimeVal :: Int

foreign import ccall unsafe "setTimevalTicks" 
  setTimevalTicks :: Ptr CTimeVal -> USecs -> IO ()

{- 
  On Win32 we're going to have a single Pipe, and a
  waitForSingleObject with the delay time.  For signals, we send a
  byte down the pipe just like on Unix.
-}

-- ----------------------------------------------------------------------------
-- select() interface

-- ToDo: move to System.Posix.Internals?

data CFdSet

foreign import ccall safe "__hscore_select"
  c_select :: CInt -> Ptr CFdSet -> Ptr CFdSet -> Ptr CFdSet -> Ptr CTimeVal
           -> IO CInt

foreign import ccall unsafe "hsFD_SETSIZE"
  c_fD_SETSIZE :: CInt

fD_SETSIZE :: Fd
fD_SETSIZE = fromIntegral c_fD_SETSIZE

foreign import ccall unsafe "hsFD_ISSET"
  c_fdIsSet :: CInt -> Ptr CFdSet -> IO CInt

fdIsSet :: Fd -> Ptr CFdSet -> IO CInt
fdIsSet (Fd fd) fdset = c_fdIsSet fd fdset

foreign import ccall unsafe "hsFD_SET"
  c_fdSet :: CInt -> Ptr CFdSet -> IO ()

fdSet :: Fd -> Ptr CFdSet -> IO ()
fdSet (Fd fd) fdset = c_fdSet fd fdset

foreign import ccall unsafe "hsFD_ZERO"
  fdZero :: Ptr CFdSet -> IO ()

foreign import ccall unsafe "sizeof_fd_set"
  sizeofFdSet :: Int

#endif

reportStackOverflow :: IO ()
reportStackOverflow = callStackOverflowHook

reportError :: SomeException -> IO ()
reportError ex = do
   handler <- getUncaughtExceptionHandler
   handler ex

-- SUP: Are the hooks allowed to re-enter Haskell land?  If so, remove
-- the unsafe below.
foreign import ccall unsafe "stackOverflow"
        callStackOverflowHook :: IO ()

{-# NOINLINE uncaughtExceptionHandler #-}
uncaughtExceptionHandler :: IORef (SomeException -> IO ())
uncaughtExceptionHandler = unsafePerformIO (newIORef defaultHandler)
   where
      defaultHandler :: SomeException -> IO ()
      defaultHandler se@(SomeException ex) = do
         (hFlush stdout) `catchAny` (\ _ -> return ())
         let msg = case cast ex of
               Just Deadlock -> "no threads to run:  infinite loop or deadlock?"
               _ -> case cast ex of
                    Just (ErrorCall s) -> s
                    _                  -> showsPrec 0 se ""
         withCString "%s" $ \cfmt ->
          withCString msg $ \cmsg ->
            errorBelch cfmt cmsg

-- don't use errorBelch() directly, because we cannot call varargs functions
-- using the FFI.
foreign import ccall unsafe "HsBase.h errorBelch2"
   errorBelch :: CString -> CString -> IO ()

setUncaughtExceptionHandler :: (SomeException -> IO ()) -> IO ()
setUncaughtExceptionHandler = writeIORef uncaughtExceptionHandler

getUncaughtExceptionHandler :: IO (SomeException -> IO ())
getUncaughtExceptionHandler = readIORef uncaughtExceptionHandler

\end{code}

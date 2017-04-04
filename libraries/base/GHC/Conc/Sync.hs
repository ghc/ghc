{-# LANGUAGE Unsafe #-}
{-# LANGUAGE CPP
           , NoImplicitPrelude
           , BangPatterns
           , MagicHash
           , UnboxedTuples
           , UnliftedFFITypes
           , StandaloneDeriving
           , RankNTypes
  #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Conc.Sync
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
module GHC.Conc.Sync
        ( ThreadId(..)

        -- * Forking and suchlike
        , forkIO
        , forkIOWithUnmask
        , forkOn
        , forkOnWithUnmask
        , numCapabilities
        , getNumCapabilities
        , setNumCapabilities
        , getNumProcessors
        , numSparks
        , childHandler
        , myThreadId
        , killThread
        , throwTo
        , par
        , pseq
        , runSparks
        , yield
        , labelThread
        , mkWeakThreadId

        , ThreadStatus(..), BlockReason(..)
        , threadStatus
        , threadCapability

        , newStablePtrPrimMVar, PrimMVar

        -- * Allocation counter and quota
        , setAllocationCounter
        , getAllocationCounter
        , enableAllocationLimit
        , disableAllocationLimit

        -- * TVars
        , STM(..)
        , atomically
        , retry
        , orElse
        , throwSTM
        , catchSTM
        , alwaysSucceeds
        , always
        , TVar(..)
        , newTVar
        , newTVarIO
        , readTVar
        , readTVarIO
        , writeTVar
        , unsafeIOToSTM

        -- * Miscellaneous
        , withMVar
        , modifyMVar_

        , setUncaughtExceptionHandler
        , getUncaughtExceptionHandler

        , reportError, reportStackOverflow, reportHeapOverflow

        , sharedCAF
        ) where

import Foreign
import Foreign.C

import Data.Typeable
import Data.Maybe

import GHC.Base
import {-# SOURCE #-} GHC.IO.Handle ( hFlush )
import {-# SOURCE #-} GHC.IO.Handle.FD ( stdout )
import GHC.IO
import GHC.IO.Encoding.UTF8
import GHC.IO.Exception
import GHC.Exception
import qualified GHC.Foreign
import GHC.IORef
import GHC.MVar
import GHC.Ptr
import GHC.Real         ( fromIntegral )
import GHC.Show         ( Show(..), showString )
import GHC.Stable       ( StablePtr(..) )
import GHC.Weak

infixr 0 `par`, `pseq`

-----------------------------------------------------------------------------
-- 'ThreadId', 'par', and 'fork'
-----------------------------------------------------------------------------

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

-}

-- | @since 4.2.0.0
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

-- | @since 4.2.0.0
instance Eq ThreadId where
   t1 == t2 =
      case t1 `cmpThread` t2 of
         EQ -> True
         _  -> False

-- | @since 4.2.0.0
instance Ord ThreadId where
   compare = cmpThread

-- | Every thread has an allocation counter that tracks how much
-- memory has been allocated by the thread.  The counter is
-- initialized to zero, and 'setAllocationCounter' sets the current
-- value.  The allocation counter counts *down*, so in the absence of
-- a call to 'setAllocationCounter' its value is the negation of the
-- number of bytes of memory allocated by the thread.
--
-- There are two things that you can do with this counter:
--
-- * Use it as a simple profiling mechanism, with
--   'getAllocationCounter'.
--
-- * Use it as a resource limit.  See 'enableAllocationLimit'.
--
-- Allocation accounting is accurate only to about 4Kbytes.
--
-- @since 4.8.0.0
setAllocationCounter :: Int64 -> IO ()
setAllocationCounter i = do
  ThreadId t <- myThreadId
  rts_setThreadAllocationCounter t i

-- | Return the current value of the allocation counter for the
-- current thread.
--
-- @since 4.8.0.0
getAllocationCounter :: IO Int64
getAllocationCounter = do
  ThreadId t <- myThreadId
  rts_getThreadAllocationCounter t

-- | Enables the allocation counter to be treated as a limit for the
-- current thread.  When the allocation limit is enabled, if the
-- allocation counter counts down below zero, the thread will be sent
-- the 'AllocationLimitExceeded' asynchronous exception.  When this
-- happens, the counter is reinitialised (by default
-- to 100K, but tunable with the @+RTS -xq@ option) so that it can handle
-- the exception and perform any necessary clean up.  If it exhausts
-- this additional allowance, another 'AllocationLimitExceeded' exception
-- is sent, and so forth.  Like other asynchronous exceptions, the
-- 'AllocationLimitExceeded' exception is deferred while the thread is inside
-- 'mask' or an exception handler in 'catch'.
--
-- Note that memory allocation is unrelated to /live memory/, also
-- known as /heap residency/.  A thread can allocate a large amount of
-- memory and retain anything between none and all of it.  It is
-- better to think of the allocation limit as a limit on
-- /CPU time/, rather than a limit on memory.
--
-- Compared to using timeouts, allocation limits don't count time
-- spent blocked or in foreign calls.
--
-- @since 4.8.0.0
enableAllocationLimit :: IO ()
enableAllocationLimit = do
  ThreadId t <- myThreadId
  rts_enableThreadAllocationLimit t

-- | Disable allocation limit processing for the current thread.
--
-- @since 4.8.0.0
disableAllocationLimit :: IO ()
disableAllocationLimit = do
  ThreadId t <- myThreadId
  rts_disableThreadAllocationLimit t

-- We cannot do these operations safely on another thread, because on
-- a 32-bit machine we cannot do atomic operations on a 64-bit value.
-- Therefore, we only expose APIs that allow getting and setting the
-- limit of the current thread.
foreign import ccall unsafe "rts_setThreadAllocationCounter"
  rts_setThreadAllocationCounter :: ThreadId# -> Int64 -> IO ()

foreign import ccall unsafe "rts_getThreadAllocationCounter"
  rts_getThreadAllocationCounter :: ThreadId# -> IO Int64

foreign import ccall unsafe "rts_enableThreadAllocationLimit"
  rts_enableThreadAllocationLimit :: ThreadId# -> IO ()

foreign import ccall unsafe "rts_disableThreadAllocationLimit"
  rts_disableThreadAllocationLimit :: ThreadId# -> IO ()

{- |
Creates a new thread to run the 'IO' computation passed as the
first argument, and returns the 'ThreadId' of the newly created
thread.

The new thread will be a lightweight, /unbound/ thread.  Foreign calls
made by this thread are not guaranteed to be made by any particular OS
thread; if you need foreign calls to be made by a particular OS
thread, then use 'Control.Concurrent.forkOS' instead.

The new thread inherits the /masked/ state of the parent (see
'Control.Exception.mask').

The newly created thread has an exception handler that discards the
exceptions 'BlockedIndefinitelyOnMVar', 'BlockedIndefinitelyOnSTM', and
'ThreadKilled', and passes all other exceptions to the uncaught
exception handler.
-}
forkIO :: IO () -> IO ThreadId
forkIO action = IO $ \ s ->
   case (fork# action_plus s) of (# s1, tid #) -> (# s1, ThreadId tid #)
 where
  -- We must use 'catch' rather than 'catchException' because the action
  -- could be bottom. #13330
  action_plus = catch action childHandler

-- | Like 'forkIO', but the child thread is passed a function that can
-- be used to unmask asynchronous exceptions.  This function is
-- typically used in the following way
--
-- >  ... mask_ $ forkIOWithUnmask $ \unmask ->
-- >                 catch (unmask ...) handler
--
-- so that the exception handler in the child thread is established
-- with asynchronous exceptions masked, meanwhile the main body of
-- the child thread is executed in the unmasked state.
--
-- Note that the unmask function passed to the child thread should
-- only be used in that thread; the behaviour is undefined if it is
-- invoked in a different thread.
--
-- @since 4.4.0.0
forkIOWithUnmask :: ((forall a . IO a -> IO a) -> IO ()) -> IO ThreadId
forkIOWithUnmask io = forkIO (io unsafeUnmask)

{- |
Like 'forkIO', but lets you specify on which capability the thread
should run.  Unlike a `forkIO` thread, a thread created by `forkOn`
will stay on the same capability for its entire lifetime (`forkIO`
threads can migrate between capabilities according to the scheduling
policy).  `forkOn` is useful for overriding the scheduling policy when
you know in advance how best to distribute the threads.

The `Int` argument specifies a /capability number/ (see
'getNumCapabilities').  Typically capabilities correspond to physical
processors, but the exact behaviour is implementation-dependent.  The
value passed to 'forkOn' is interpreted modulo the total number of
capabilities as returned by 'getNumCapabilities'.

GHC note: the number of capabilities is specified by the @+RTS -N@
option when the program is started.  Capabilities can be fixed to
actual processor cores with @+RTS -qa@ if the underlying operating
system supports that, although in practice this is usually unnecessary
(and may actually degrade performance in some cases - experimentation
is recommended).

@since 4.4.0.0
-}
forkOn :: Int -> IO () -> IO ThreadId
forkOn (I# cpu) action = IO $ \ s ->
   case (forkOn# cpu action_plus s) of (# s1, tid #) -> (# s1, ThreadId tid #)
 where
  -- We must use 'catch' rather than 'catchException' because the action
  -- could be bottom. #13330
  action_plus = catch action childHandler

-- | Like 'forkIOWithUnmask', but the child thread is pinned to the
-- given CPU, as with 'forkOn'.
--
-- @since 4.4.0.0
forkOnWithUnmask :: Int -> ((forall a . IO a -> IO a) -> IO ()) -> IO ThreadId
forkOnWithUnmask cpu io = forkOn cpu (io unsafeUnmask)

-- | the value passed to the @+RTS -N@ flag.  This is the number of
-- Haskell threads that can run truly simultaneously at any given
-- time, and is typically set to the number of physical processor cores on
-- the machine.
--
-- Strictly speaking it is better to use 'getNumCapabilities', because
-- the number of capabilities might vary at runtime.
--
numCapabilities :: Int
numCapabilities = unsafePerformIO $ getNumCapabilities

{- |
Returns the number of Haskell threads that can run truly
simultaneously (on separate physical processors) at any given time.  To change
this value, use 'setNumCapabilities'.

@since 4.4.0.0
-}
getNumCapabilities :: IO Int
getNumCapabilities = do
   n <- peek enabled_capabilities
   return (fromIntegral n)

{- |
Set the number of Haskell threads that can run truly simultaneously
(on separate physical processors) at any given time.  The number
passed to `forkOn` is interpreted modulo this value.  The initial
value is given by the @+RTS -N@ runtime flag.

This is also the number of threads that will participate in parallel
garbage collection.  It is strongly recommended that the number of
capabilities is not set larger than the number of physical processor
cores, and it may often be beneficial to leave one or more cores free
to avoid contention with other processes in the machine.

@since 4.5.0.0
-}
setNumCapabilities :: Int -> IO ()
setNumCapabilities i = c_setNumCapabilities (fromIntegral i)

foreign import ccall safe "setNumCapabilities"
  c_setNumCapabilities :: CUInt -> IO ()

-- | Returns the number of CPUs that the machine has
--
-- @since 4.5.0.0
getNumProcessors :: IO Int
getNumProcessors = fmap fromIntegral c_getNumberOfProcessors

foreign import ccall unsafe "getNumberOfProcessors"
  c_getNumberOfProcessors :: IO CUInt

-- | Returns the number of sparks currently in the local spark pool
numSparks :: IO Int
numSparks = IO $ \s -> case numSparks# s of (# s', n #) -> (# s', I# n #)

foreign import ccall "&enabled_capabilities" enabled_capabilities :: Ptr CInt

childHandler :: SomeException -> IO ()
childHandler err = catch (real_handler err) childHandler
  -- We must use catch here rather than catchException. If the
  -- raised exception throws an (imprecise) exception, then real_handler err
  -- will do so as well. If we use catchException here, then we could miss
  -- that exception.

real_handler :: SomeException -> IO ()
real_handler se
  | Just BlockedIndefinitelyOnMVar <- fromException se  =  return ()
  | Just BlockedIndefinitelyOnSTM  <- fromException se  =  return ()
  | Just ThreadKilled              <- fromException se  =  return ()
  | Just StackOverflow             <- fromException se  =  reportStackOverflow
  | otherwise                                           =  reportError se

{- | 'killThread' raises the 'ThreadKilled' exception in the given
thread (GHC only).

> killThread tid = throwTo tid ThreadKilled

-}
killThread :: ThreadId -> IO ()
killThread tid = throwTo tid ThreadKilled

{- | 'throwTo' raises an arbitrary exception in the target thread (GHC only).

Exception delivery synchronizes between the source and target thread:
'throwTo' does not return until the exception has been raised in the
target thread. The calling thread can thus be certain that the target
thread has received the exception.  Exception delivery is also atomic
with respect to other exceptions. Atomicity is a useful property to have
when dealing with race conditions: e.g. if there are two threads that
can kill each other, it is guaranteed that only one of the threads
will get to kill the other.

Whatever work the target thread was doing when the exception was
raised is not lost: the computation is suspended until required by
another thread.

If the target thread is currently making a foreign call, then the
exception will not be raised (and hence 'throwTo' will not return)
until the call has completed.  This is the case regardless of whether
the call is inside a 'mask' or not.  However, in GHC a foreign call
can be annotated as @interruptible@, in which case a 'throwTo' will
cause the RTS to attempt to cause the call to return; see the GHC
documentation for more details.

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

If the target of 'throwTo' is the calling thread, then the behaviour
is the same as 'Control.Exception.throwIO', except that the exception
is thrown as an asynchronous exception.  This means that if there is
an enclosing pure computation, which would be the case if the current
IO operation is inside 'unsafePerformIO' or 'unsafeInterleaveIO', that
computation is not permanently replaced by the exception, but is
suspended as if it had received an asynchronous exception.

Note that if 'throwTo' is called with the current thread as the
target, the exception will be thrown even if the thread is currently
inside 'mask' or 'uninterruptibleMask'.
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
labelThread (ThreadId t) str =
    GHC.Foreign.withCString utf8 str $ \(Ptr p) ->
    IO $ \ s ->
     case labelThread# t p s of s1 -> (# s1, () #)

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
                      if isTrue# (n ==# 0#)
                      then (# s', () #)
                      else p `seq` loop s'

data BlockReason
  = BlockedOnMVar
        -- ^blocked on 'MVar'
  {- possibly (see 'threadstatus' below):
  | BlockedOnMVarRead
        -- ^blocked on reading an empty 'MVar'
  -}
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
    (# s', stat, _cap, _locked #) -> (# s', mk_stat (I# stat) #)
   where
        -- NB. keep these in sync with includes/rts/Constants.h
     mk_stat 0  = ThreadRunning
     mk_stat 1  = ThreadBlocked BlockedOnMVar
     mk_stat 2  = ThreadBlocked BlockedOnBlackHole
     mk_stat 6  = ThreadBlocked BlockedOnSTM
     mk_stat 10 = ThreadBlocked BlockedOnForeignCall
     mk_stat 11 = ThreadBlocked BlockedOnForeignCall
     mk_stat 12 = ThreadBlocked BlockedOnException
     mk_stat 14 = ThreadBlocked BlockedOnMVar -- possibly: BlockedOnMVarRead
     -- NB. these are hardcoded in rts/PrimOps.cmm
     mk_stat 16 = ThreadFinished
     mk_stat 17 = ThreadDied
     mk_stat _  = ThreadBlocked BlockedOnOther

-- | returns the number of the capability on which the thread is currently
-- running, and a boolean indicating whether the thread is locked to
-- that capability or not.  A thread is locked to a capability if it
-- was created with @forkOn@.
--
-- @since 4.4.0.0
threadCapability :: ThreadId -> IO (Int, Bool)
threadCapability (ThreadId t) = IO $ \s ->
   case threadStatus# t s of
     (# s', _, cap#, locked# #) -> (# s', (I# cap#, isTrue# (locked# /=# 0#)) #)

-- | make a weak pointer to a 'ThreadId'.  It can be important to do
-- this if you want to hold a reference to a 'ThreadId' while still
-- allowing the thread to receive the @BlockedIndefinitely@ family of
-- exceptions (e.g. 'BlockedIndefinitelyOnMVar').  Holding a normal
-- 'ThreadId' reference will prevent the delivery of
-- @BlockedIndefinitely@ exceptions because the reference could be
-- used as the target of 'throwTo' at any time, which would unblock
-- the thread.
--
-- Holding a @Weak ThreadId@, on the other hand, will not prevent the
-- thread from receiving @BlockedIndefinitely@ exceptions.  It is
-- still possible to throw an exception to a @Weak ThreadId@, but the
-- caller must use @deRefWeak@ first to determine whether the thread
-- still exists.
--
-- @since 4.6.0.0
mkWeakThreadId :: ThreadId -> IO (Weak ThreadId)
mkWeakThreadId t@(ThreadId t#) = IO $ \s ->
   case mkWeakNoFinalizer# t# t s of
      (# s1, w #) -> (# s1, Weak w #)


data PrimMVar

-- | Make a StablePtr that can be passed to the C function
-- @hs_try_putmvar()@.  The RTS wants a 'StablePtr' to the underlying
-- 'MVar#', but a 'StablePtr#' can only refer to lifted types, so we
-- have to cheat by coercing.
newStablePtrPrimMVar :: MVar () -> IO (StablePtr PrimMVar)
newStablePtrPrimMVar (MVar m) = IO $ \s0 ->
  case makeStablePtr# (unsafeCoerce# m :: PrimMVar) s0 of
    (# s1, sp #) -> (# s1, StablePtr sp #)

-----------------------------------------------------------------------------
-- Transactional heap operations
-----------------------------------------------------------------------------

-- TVars are shared memory locations which support atomic memory
-- transactions.

-- |A monad supporting atomic memory transactions.
newtype STM a = STM (State# RealWorld -> (# State# RealWorld, a #))

unSTM :: STM a -> (State# RealWorld -> (# State# RealWorld, a #))
unSTM (STM a) = a

-- | @since 4.3.0.0
instance  Functor STM where
   fmap f x = x >>= (pure . f)

-- | @since 4.8.0.0
instance Applicative STM where
  {-# INLINE pure #-}
  {-# INLINE (*>) #-}
  {-# INLINE liftA2 #-}
  pure x = returnSTM x
  (<*>) = ap
  liftA2 = liftM2
  m *> k = thenSTM m k

-- | @since 4.3.0.0
instance  Monad STM  where
    {-# INLINE (>>=)  #-}
    m >>= k     = bindSTM m k
    (>>) = (*>)

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

-- | @since 4.8.0.0
instance Alternative STM where
  empty = retry
  (<|>) = orElse

-- | @since 4.3.0.0
instance MonadPlus STM

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

-- | A variant of 'throw' that can only be used within the 'STM' monad.
--
-- Throwing an exception in @STM@ aborts the transaction and propagates the
-- exception.
--
-- Although 'throwSTM' has a type that is an instance of the type of 'throw', the
-- two functions are subtly different:
--
-- > throw e    `seq` x  ===> throw e
-- > throwSTM e `seq` x  ===> x
--
-- The first example will cause the exception @e@ to be raised,
-- whereas the second one won\'t.  In fact, 'throwSTM' will only cause
-- an exception to be raised when it is used within the 'STM' monad.
-- The 'throwSTM' variant should be used in preference to 'throw' to
-- raise an exception within the 'STM' monad because it guarantees
-- ordering with respect to other 'STM' operations, whereas 'throw'
-- does not.
throwSTM :: Exception e => e -> STM a
throwSTM e = STM $ raiseIO# (toException e)

-- |Exception handling within STM actions.
catchSTM :: Exception e => STM a -> (e -> STM a) -> STM a
catchSTM (STM m) handler = STM $ catchSTM# m handler'
    where
      handler' e = case fromException e of
                     Just e' -> unSTM (handler e')
                     Nothing -> raiseIO# e

-- | Low-level primitive on which always and alwaysSucceeds are built.
-- checkInv differs form these in that (i) the invariant is not
-- checked when checkInv is called, only at the end of this and
-- subsequent transcations, (ii) the invariant failure is indicated
-- by raising an exception.
checkInv :: STM a -> STM ()
checkInv (STM m) = STM (\s -> case (check# m) s of s' -> (# s', () #))

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
                               if (v) then return () else ( errorWithoutStackTrace "Transactional invariant violation" ) )

-- |Shared memory locations that support atomic memory transactions.
data TVar a = TVar (TVar# RealWorld a)

-- | @since 4.8.0.0
instance Eq (TVar a) where
        (TVar tvar1#) == (TVar tvar2#) = isTrue# (sameTVar# tvar1# tvar2#)

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

-----------------------------------------------------------------------------
-- MVar utilities
-----------------------------------------------------------------------------

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

-----------------------------------------------------------------------------
-- Thread waiting
-----------------------------------------------------------------------------

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

reportStackOverflow :: IO ()
reportStackOverflow = do
     ThreadId tid <- myThreadId
     c_reportStackOverflow tid

reportError :: SomeException -> IO ()
reportError ex = do
   handler <- getUncaughtExceptionHandler
   handler ex

-- SUP: Are the hooks allowed to re-enter Haskell land?  If so, remove
-- the unsafe below.
foreign import ccall unsafe "reportStackOverflow"
        c_reportStackOverflow :: ThreadId# -> IO ()

foreign import ccall unsafe "reportHeapOverflow"
        reportHeapOverflow :: IO ()

{-# NOINLINE uncaughtExceptionHandler #-}
uncaughtExceptionHandler :: IORef (SomeException -> IO ())
uncaughtExceptionHandler = unsafePerformIO (newIORef defaultHandler)
   where
      defaultHandler :: SomeException -> IO ()
      defaultHandler se@(SomeException ex) = do
         (hFlush stdout) `catchAny` (\ _ -> return ())
         let msg = case cast ex of
               Just Deadlock -> "no threads to run:  infinite loop or deadlock?"
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

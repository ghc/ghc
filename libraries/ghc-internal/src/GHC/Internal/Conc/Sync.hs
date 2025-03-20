{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Unsafe #-}

{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Conc.Sync
-- Copyright   :  (c) The University of Glasgow, 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
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
module GHC.Internal.Conc.Sync
        (
        -- * Threads
          ThreadId(..)
        , fromThreadId
        , showThreadId
        , myThreadId
        , killThread
        , throwTo
        , yield
        , labelThread
        , labelThreadByteArray#
        , mkWeakThreadId
        -- ** Queries
        , listThreads
        , threadLabel
        , ThreadStatus(..), BlockReason(..)
        , threadStatus
        , threadCapability

        -- * Forking and suchlike
        , forkIO
        , forkIOWithUnmask
        , forkOn
        , forkOnWithUnmask

        -- * Capabilities
        , numCapabilities
        , getNumCapabilities
        , setNumCapabilities
        , getNumProcessors

        -- * Sparks
        , numSparks
        , childHandler
        , par
        , pseq
        , runSparks

        -- * PrimMVar
        , newStablePtrPrimMVar, PrimMVar

        -- * Allocation counter and quota
        , setAllocationCounter
        , getAllocationCounter
        , enableAllocationLimit
        , disableAllocationLimit

        -- * Miscellaneous
        , withMVar
        , modifyMVar_

        , setUncaughtExceptionHandler
        , getUncaughtExceptionHandler

        , reportError, reportStackOverflow, reportHeapOverflow

        , sharedCAF
        ) where

import GHC.Internal.Foreign.C.Types
import GHC.Internal.Foreign.C.String
import GHC.Internal.Foreign.Storable
import GHC.Internal.Foreign.StablePtr

import GHC.Internal.Base
import {-# SOURCE #-} GHC.Internal.IO.Handle ( hFlush )
import {-# SOURCE #-} GHC.Internal.IO.StdHandles ( stdout )
import GHC.Internal.Encoding.UTF8
import GHC.Internal.Int
import GHC.Internal.IO
import GHC.Internal.IO.Exception
import GHC.Internal.Exception
import GHC.Internal.IORef
import GHC.Internal.MVar
import GHC.Internal.Ptr
import GHC.Internal.Real         ( fromIntegral )
import GHC.Internal.Show         ( Show(..), showParen, showString )
import GHC.Internal.Weak
import GHC.Internal.Word

infixr 0 `par`, `pseq`

-----------------------------------------------------------------------------
-- 'ThreadId', 'par', and 'fork'
-----------------------------------------------------------------------------

data ThreadId = ThreadId ThreadId#
{- ^
A 'ThreadId' is an abstract type representing a handle to a thread.
'ThreadId' is an instance of 'Eq', 'Ord' and 'Show', where
the 'Ord' instance implements an arbitrary total ordering over
'ThreadId's. The 'Show' instance lets you convert an arbitrary-valued
'ThreadId' to string form; showing a 'ThreadId' value is occasionally
useful when debugging or diagnosing the behaviour of a concurrent
program.

/Note/: in GHC, if you have a 'ThreadId', you essentially have
a pointer to the thread itself. This means the thread itself can\'t be
garbage collected until you drop the 'ThreadId'. This misfeature would
be difficult to correct while continuing to support 'threadStatus'.
-}

-- | Map a thread to an integer identifier which is unique within the
-- current process.
--
-- @since base-4.19.0.0
fromThreadId :: ThreadId -> Word64
fromThreadId tid = fromIntegral $ getThreadId (id2TSO tid)

-- | @since base-4.2.0.0
instance Show ThreadId where
   showsPrec d t = showParen (d >= 11) $
        showString "ThreadId " .
        showsPrec d (fromThreadId t)

showThreadId :: ThreadId -> String
showThreadId = show

foreign import ccall unsafe "rts_getThreadId" getThreadId :: ThreadId# -> CULLong

id2TSO :: ThreadId -> ThreadId#
id2TSO (ThreadId t) = t

foreign import ccall unsafe "eq_thread" eq_thread :: ThreadId# -> ThreadId# -> CBool

foreign import ccall unsafe "cmp_thread" cmp_thread :: ThreadId# -> ThreadId# -> CInt
-- Returns -1, 0, 1

-- | @since base-4.2.0.0
instance Eq ThreadId where
  ThreadId t1 == ThreadId t2 = eq_thread t1 t2 /= 0

-- | @since base-4.2.0.0
instance Ord ThreadId where
  compare (ThreadId t1) (ThreadId t2) = case cmp_thread t1 t2 of
    -1 -> LT
    0  -> EQ
    _  -> GT

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
-- @since base-4.8.0.0
setAllocationCounter :: Int64 -> IO ()
setAllocationCounter (I64# i) = IO $ \s ->
  case setThreadAllocationCounter# i s of s' -> (# s', () #)

-- | Return the current value of the allocation counter for the
-- current thread.
--
-- @since base-4.8.0.0
getAllocationCounter :: IO Int64
getAllocationCounter = IO $ \s ->
  case getThreadAllocationCounter# s of (# s', ctr #) -> (# s', I64# ctr #)

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
-- @since base-4.8.0.0
enableAllocationLimit :: IO ()
enableAllocationLimit = do
  ThreadId t <- myThreadId
  rts_enableThreadAllocationLimit t

-- | Disable allocation limit processing for the current thread.
--
-- @since base-4.8.0.0
disableAllocationLimit :: IO ()
disableAllocationLimit = do
  ThreadId t <- myThreadId
  rts_disableThreadAllocationLimit t

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
'GHC.Control.Exception.mask').

The newly created thread has an exception handler that discards the
exceptions 'BlockedIndefinitelyOnMVar', 'BlockedIndefinitelyOnSTM', and
'ThreadKilled', and passes all other exceptions to the uncaught
exception handler.

WARNING: Exceptions in the new thread will not be rethrown in the thread that
created it. This means that you might be completely unaware of the problem
if/when this happens.  You may want to use the
<https://hackage.haskell.org/package/async async> library instead.
-}
forkIO :: IO () -> IO ThreadId
forkIO action = IO $ \ s ->
   case (fork# (unIO action_plus) s) of (# s1, tid #) -> (# s1, ThreadId tid #)
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
-- @since base-4.4.0.0
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

@since base-4.4.0.0
-}
forkOn :: Int -> IO () -> IO ThreadId
forkOn (I# cpu) action = IO $ \ s ->
   case (forkOn# cpu (unIO action_plus) s) of (# s1, tid #) -> (# s1, ThreadId tid #)
 where
  -- We must use 'catch' rather than 'catchException' because the action
  -- could be bottom. #13330
  action_plus = catch action childHandler

-- | Like 'forkIOWithUnmask', but the child thread is pinned to the
-- given CPU, as with 'forkOn'.
--
-- @since base-4.4.0.0
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

@since base-4.4.0.0
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

@since base-4.5.0.0
-}
setNumCapabilities :: Int -> IO ()
setNumCapabilities i
  | i <= 0    = failIO $ "setNumCapabilities: Capability count ("++show i++") must be positive"
  | otherwise = c_setNumCapabilities (fromIntegral i)

foreign import ccall safe "setNumCapabilities"
  c_setNumCapabilities :: CUInt -> IO ()

-- | Returns the number of CPUs that the machine has
--
-- @since base-4.5.0.0
getNumProcessors :: IO Int
getNumProcessors = fmap fromIntegral c_getNumberOfProcessors

foreign import ccall unsafe "getNumberOfProcessors"
  c_getNumberOfProcessors :: IO Word32

-- | Returns the number of sparks currently in the local spark pool
numSparks :: IO Int
numSparks = IO $ \s -> case numSparks# s of (# s', n #) -> (# s', I# n #)

foreign import ccall "&enabled_capabilities"
  enabled_capabilities :: Ptr Word32

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
is the same as 'GHC.Internal.Control.Exception.throwIO', except that the exception
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


-- | The 'yield' action allows (forces, in a co-operative multitasking
-- implementation) a context-switch to any other currently runnable
-- threads (if any), and is occasionally useful when implementing
-- concurrency abstractions.
yield :: IO ()
yield = IO $ \s ->
   case (yield# s) of s1 -> (# s1, () #)

{- | 'labelThread' stores a string as identifier for this thread. This
identifier will be used in the debugging output to make distinction of
different threads easier (otherwise you only have the thread state object\'s
address in the heap). It also emits an event to the RTS eventlog.
-}
labelThread :: ThreadId -> String -> IO ()
labelThread t str =
    labelThreadByteArray# t (utf8EncodeByteArray# str)

-- | 'labelThreadByteArray#' sets the label of a thread to the given UTF-8
--  encoded string contained in a `ByteArray#`.
--
--  @since base-4.18
labelThreadByteArray# :: ThreadId -> ByteArray# -> IO ()
labelThreadByteArray# (ThreadId t) str =
    IO $ \s -> case labelThread# t str s of s1 -> (# s1, () #)

--      Nota Bene: 'pseq' used to be 'seq'
--                 but 'seq' is now defined in GHC.Internal.Prim
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

-- | List the Haskell threads of the current process.
--
-- @since base-4.18
listThreads :: IO [ThreadId]
listThreads = IO $ \s ->
    case listThreads# s of
      (# s', arr #) ->
        (# s', mapListArrayUnlifted ThreadId arr #)

mapListArrayUnlifted :: forall (a :: TYPE UnliftedRep) b. (a -> b) -> Array# a -> [b]
mapListArrayUnlifted f arr = go 0#
  where
    sz = sizeofArray# arr
    go i#
      | isTrue# (i# ==# sz) = []
      | otherwise = case indexArray# arr i# of
                      (# x #) -> f x : go (i# +# 1#)
{-# NOINLINE mapListArrayUnlifted #-}

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
        -- I\/O and 'Control.Concurrent.threadDelay' show up as
        -- 'BlockedOnOther', with @-threaded@ they show up as 'BlockedOnMVar'.
  deriving ( Eq   -- ^ @since base-4.3.0.0
           , Ord  -- ^ @since base-4.3.0.0
           , Show -- ^ @since base-4.3.0.0
           )

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
  deriving ( Eq   -- ^ @since base-4.3.0.0
           , Ord  -- ^ @since base-4.3.0.0
           , Show -- ^ @since base-4.3.0.0
           )

-- | Query the current execution status of a thread.
threadStatus :: ThreadId -> IO ThreadStatus
threadStatus (ThreadId t) = IO $ \s ->
   case threadStatus# t s of
    (# s', stat, _cap, _locked #) -> (# s', mk_stat (I# stat) #)
   where
        -- NB. keep these in sync with rts/include/rts/Constants.h
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

-- | Returns the number of the capability on which the thread is currently
-- running, and a boolean indicating whether the thread is locked to
-- that capability or not.  A thread is locked to a capability if it
-- was created with @forkOn@.
--
-- @since base-4.4.0.0
threadCapability :: ThreadId -> IO (Int, Bool)
threadCapability (ThreadId t) = IO $ \s ->
   case threadStatus# t s of
     (# s', _, cap#, locked# #) -> (# s', (I# cap#, isTrue# (locked# /=# 0#)) #)

-- | Query the label of thread, returning 'Nothing' if the
-- thread's label has not been set.
--
-- @since base-4.18
threadLabel :: ThreadId -> IO (Maybe String)
threadLabel (ThreadId t) = IO $ \s ->
    case threadLabel# t s of
      (# s', 1#, lbl #) ->
          let lbl' = utf8DecodeByteArray# lbl
          in (# s', Just lbl' #)
      (# s', 0#, _ #) -> (# s', Nothing #)
      _ -> error "threadLabel: impossible"

-- | Make a weak pointer to a 'ThreadId'.  It can be important to do
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
-- @since base-4.6.0.0
mkWeakThreadId :: ThreadId -> IO (Weak ThreadId)
mkWeakThreadId t@(ThreadId t#) = IO $ \s ->
   case mkWeakNoFinalizer# t# t s of
      (# s1, w #) -> (# s1, Weak w #)


-----------------------------------------------------------------------------
-- MVar utilities
-----------------------------------------------------------------------------

-- | Provide an 'IO' action with the current value of an 'MVar'. The 'MVar'
-- will be empty for the duration that the action is running.
withMVar :: MVar a -> (a -> IO b) -> IO b
withMVar m io =
  mask $ \restore -> do
    a <- takeMVar m
    b <- catchAny (restore (io a))
            (\e -> do putMVar m a; throw e)
    putMVar m a
    return b

-- | Modify the value of an 'MVar'.
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
      defaultHandler se = do
         (hFlush stdout) `catchAny` (\ _ -> return ())

         let exMsg = displayExceptionWithInfo se
             msg = "Uncaught exception " ++ exMsg
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

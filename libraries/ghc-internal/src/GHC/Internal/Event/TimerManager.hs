{-# LANGUAGE CPP #-}
#if defined(javascript_HOST_ARCH)

module GHC.Internal.Event.TimerManager () where

-- See W1 of Note [Tracking dependencies on primitives] in GHC.Internal.Base
import GHC.Internal.Types ()


#else

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UnboxedTuples #-}

-- TODO: use the new Windows IO manager
module GHC.Internal.Event.TimerManager
    ( -- * Types
      TimerManager

      -- * Creation
    , new
    , newWith
    , newDefaultBackend
    , emControl

      -- * Running
    , finished
    , loop
    , step
    , shutdown
    , cleanup
    , wakeManager

      -- * Registering interest in timeout events
    , TimeoutCallback
    , TimeoutKey
    , registerTimeout
    , updateTimeout
    , unregisterTimeout
    ) where

#include "EventConfig.h"

------------------------------------------------------------------------
-- Imports

import GHC.Internal.Base
import GHC.Internal.Control.Exception (finally)
import GHC.Internal.Data.Foldable (sequence_)
import GHC.Internal.Data.IORef (IORef, atomicModifyIORef', mkWeakIORef, newIORef, readIORef,
                   writeIORef)
import GHC.Internal.IORef (IORef(..))
import GHC.Internal.Prim (casMutVar#, readMutVar#)
import GHC.Internal.STRef (STRef(..))
import GHC.Internal.Err (errorWithoutStackTrace)
import GHC.Internal.Clock (getMonotonicTimeNSec)
import GHC.Internal.Conc.Signal (runHandlers)
import GHC.Internal.Enum (maxBound)
import GHC.Internal.Maybe (Maybe(..))
import GHC.Internal.Num (Num(..))
import GHC.Internal.Real (quot, fromIntegral)
import GHC.Internal.Prim (seq)
import GHC.Internal.Show
import GHC.Internal.Event.Control
import GHC.Internal.Event.Internal (Backend, Event, evtRead, Timeout(..))
import GHC.Internal.Event.Unique (UniqueSource, newSource, newUnique)
import GHC.Internal.Event.TimeOut
import GHC.Internal.System.Posix.Types (Fd)

import qualified GHC.Internal.Event.Internal as I
import qualified GHC.Internal.Event.PSQ as Q

#if defined(HAVE_POLL)
import qualified GHC.Internal.Event.Poll   as Poll
#else
# error not implemented for this operating system
#endif

------------------------------------------------------------------------
-- Types

data State = Created
           | Running
           | Dying
           | Finished
             deriving ( Eq   -- ^ @since base-4.7.0.0
                      , Show -- ^ @since base-4.7.0.0
                      )

-- | The event manager state.
data TimerManager = TimerManager
    { emBackend      :: !Backend
    , emTimeouts     :: {-# UNPACK #-} !(IORef TimeoutQueue)
    , emState        :: {-# UNPACK #-} !(IORef State)
    , emUniqueSource :: {-# UNPACK #-} !UniqueSource
    , emControl      :: {-# UNPACK #-} !Control
    }

------------------------------------------------------------------------
-- Creation

handleControlEvent :: TimerManager -> Fd -> Event -> IO ()
handleControlEvent mgr fd _evt = do
  msg <- readControlMessage (emControl mgr) fd
  case msg of
    CMsgWakeup      -> return ()
    CMsgDie         -> writeIORef (emState mgr) Finished
    CMsgSignal fp s -> runHandlers fp s

newDefaultBackend :: IO Backend
#if defined(HAVE_POLL)
newDefaultBackend = Poll.new
#else
newDefaultBackend = errorWithoutStackTrace "no back end for this platform"
#endif

-- | Create a new event manager.
new :: IO TimerManager
new = newWith =<< newDefaultBackend

newWith :: Backend -> IO TimerManager
newWith be = do
  timeouts <- newIORef Q.empty
  ctrl <- newControl
  state <- newIORef Created
  us <- newSource
  _ <- mkWeakIORef state $ do
               st <- atomicModifyIORef' state $ \s -> (Finished, s)
               when (st /= Finished) $ do
                 I.delete be
                 closeControl ctrl
  let mgr = TimerManager { emBackend = be
                         , emTimeouts = timeouts
                         , emState = state
                         , emUniqueSource = us
                         , emControl = ctrl
                         }
  _ <- I.modifyFd be (controlReadFd ctrl) mempty evtRead
  _ <- I.modifyFd be (wakeupReadFd ctrl) mempty evtRead
  return mgr

-- | Asynchronously shuts down the event manager, if running.
shutdown :: TimerManager -> IO ()
shutdown mgr = do
  state <- atomicModifyIORef' (emState mgr) $ \s -> (Dying, s)
  when (state == Running) $ sendDie (emControl mgr)

finished :: TimerManager -> IO Bool
finished mgr = (== Finished) `liftM` readIORef (emState mgr)

cleanup :: TimerManager -> IO ()
cleanup mgr = do
  writeIORef (emState mgr) Finished
  I.delete (emBackend mgr)
  closeControl (emControl mgr)

------------------------------------------------------------------------
-- Event loop

-- | Start handling events.  This function loops until told to stop,
-- using 'shutdown'.
--
-- /Note/: This loop can only be run once per 'TimerManager', as it
-- closes all of its control resources when it finishes.
loop :: TimerManager -> IO ()
loop mgr = do
  state <- atomicModifyIORef' (emState mgr) $ \s -> case s of
    Created -> (Running, s)
    _       -> (s, s)
  case state of
    Created -> go `finally` cleanup mgr
    Dying   -> cleanup mgr
    _       -> do cleanup mgr
                  errorWithoutStackTrace $ "GHC.Internal.Event.Manager.loop: state is already " ++
                      show state
 where
  go = do running <- step mgr
          when running go

step :: TimerManager -> IO Bool
step mgr = do
  timeout <- mkTimeout
  _ <- I.poll (emBackend mgr) (Just timeout) (handleControlEvent mgr)
  state <- readIORef (emState mgr)
  state `seq` return (state == Running)
 where

  -- Call all expired timer callbacks and return the time to the
  -- next timeout.
  mkTimeout :: IO Timeout
  mkTimeout = do
      now <- getMonotonicTimeNSec
      (expired, timeout) <- atomicModifyIORef' (emTimeouts mgr) $ \tq ->
           let (expired, tq') = Q.atMost now tq
               timeout = case Q.minView tq' of
                 Nothing             -> Forever
                 Just (Q.E _ t _, _) ->
                     -- This value will always be positive since the call
                     -- to 'atMost' above removed any timeouts <= 'now'
                     let t' = t - now in t' `seq` Timeout t'
           in (tq', (expired, timeout))
      sequence_ $ map Q.value expired
      return timeout

-- | Wake up the event manager.
wakeManager :: TimerManager -> IO ()
wakeManager mgr = sendWakeup (emControl mgr)

------------------------------------------------------------------------
-- Registering interest in timeout events

expirationTime :: Int -> IO Q.Prio
expirationTime us = do
    now <- getMonotonicTimeNSec
    let expTime
          -- Currently we treat overflows by clamping to maxBound. If humanity
          -- still exists in 2500 CE we will ned to be a bit more careful here.
          -- See #15158.
          | (maxBound - now) `quot` 1000 < fromIntegral us  = maxBound
          | otherwise                                       = now + ns
          where ns = 1000 * fromIntegral us
    return expTime

-- | Register a timeout in the given number of microseconds.  The
-- returned 'TimeoutKey' can be used to later unregister or update the
-- timeout.  The timeout is automatically unregistered after the given
-- time has passed.
--
-- Be careful not to exceed @maxBound :: Int@, which on 32-bit machines is only
-- 2147483647 μs, less than 36 minutes.
--
registerTimeout :: TimerManager -> Int -> TimeoutCallback -> IO TimeoutKey
registerTimeout mgr us cb = do
  !key <- newUnique (emUniqueSource mgr)
  if us <= 0 then cb
    else do
      expTime <- expirationTime us

      -- "unsafeInsertNew" is safe - the key must not exist in the PSQ. It
      -- doesn't because we just generated it from a unique supply.
      editTimeouts mgr (Q.unsafeInsertNew key expTime cb)
  return $ TK key

-- | Unregister an active timeout.
unregisterTimeout :: TimerManager -> TimeoutKey -> IO ()
unregisterTimeout mgr (TK key) =
  editTimeouts mgr (Q.delete key)

-- | Update an active timeout to fire in the given number of
-- microseconds.
--
-- Be careful not to exceed @maxBound :: Int@, which on 32-bit machines is only
-- 2147483647 μs, less than 36 minutes.
--
updateTimeout :: TimerManager -> TimeoutKey -> Int -> IO ()
updateTimeout mgr (TK key) us = do
  expTime <- expirationTime us
  editTimeouts mgr (Q.adjust (const expTime) key)

editTimeouts :: TimerManager -> TimeoutEdit -> IO ()
editTimeouts mgr g = do
  wake <- modifyTimeouts (emTimeouts mgr) f
  when wake (wakeManager mgr)
  where
    f q = (q', wake)
      where
        q' = g q
        wake = case Q.minView q of
                Nothing -> True
                Just (Q.E _ t0 _, _) ->
                  case Q.minView q' of
                    Just (Q.E _ t1 _, _) ->
                      -- don't wake the manager if the
                      -- minimum element didn't change.
                      t0 /= t1
                    _ -> True

-- | Atomically replace the timeout queue with the first component of
-- @f old@ and return the second component.
--
-- Unlike 'atomicModifyIORef'', the new queue is evaluated /before/ it is
-- published, using a compare-and-swap loop. 'atomicModifyIORef'' installs
-- the new contents as a thunk and only evaluates it afterwards, so when many
-- threads register timeouts at once (#9809) each one builds its insert on
-- top of the previous thread's unevaluated insert, giving long thunk chains,
-- threads blocking on each other's half evaluated updates and lots of extra
-- GC work. Since 'Q.IntPSQ' is spine strict, WHNF means fully evaluated, and
-- other threads only ever see a fully evaluated queue.
--
-- The cost is that a thread whose compare-and-swap loses a race has to redo
-- its O(log n) update. The update runs in constant stack (see #7653).
--
-- Only the short edits made by registering threads use this. The timer
-- manager's own update in 'mkTimeout' (removing every expired timeout) can
-- take much longer, and under a stream of registrations from other
-- capabilities its compare-and-swap would almost never succeed, starving it
-- so that expired timeouts don't fire. It therefore keeps using
-- 'atomicModifyIORef'', which never retries: it publishes a single thunk and
-- evaluates it straight away. A registering thread that reads that thunk
-- evaluates it (or waits for the manager to) before publishing its own fully
-- evaluated queue, so thunks still cannot pile up.
--
-- 'casMutVar#' compares pointers, so the expected value must be exactly the
-- pointer read from the 'MutVar#'. See Note [Pointer equality in
-- modifyTimeouts] for why this function is OPAQUE.
{-# OPAQUE modifyTimeouts #-}
modifyTimeouts :: IORef TimeoutQueue -> (TimeoutQueue -> (TimeoutQueue, b)) -> IO b
modifyTimeouts (IORef (STRef ref)) f = IO modLoop
  where
    modLoop s0 = case readMutVar# ref s0 of
      (# s1, oldVar #) -> case f oldVar of
        (newVar, r) -> newVar `seq` case casMutVar# ref oldVar newVar s1 of
          (# s2, 0#, _ #) -> (# s2, r #)
          (# s2, _, _ #) -> modLoop s2

{- Note [Pointer equality in modifyTimeouts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'casMutVar#' succeeds only if the expected value is the very pointer stored
in the 'MutVar#', so all the difficulties of 'reallyUnsafePtrEquality#'
apply to it. In 'modifyTimeouts' the expected value is @oldVar@, exactly as
'readMutVar#' returned it, and that must survive optimisation.

The danger is the optimiser seeing a @case@ on @oldVar@ in the same function
as the 'casMutVar#'. That would happen if 'modifyTimeouts' were inlined or
specialised at a call site, bringing in the body of @f@ (in 'editTimeouts',
@f@ inspects the queue to decide whether to wake the manager). The binder
swap (Note [Binder swap] in GHC.Core.Opt.OccurAnal) may then replace @oldVar@
by the case binder, which is the /evaluated/ queue. That is a different
pointer whenever the 'MutVar#' holds a thunk or an indirection, for example
the selector thunk that 'atomicModifyIORef'' in 'mkTimeout' leaves behind.
Case-of-known-constructor can likewise replace it by a freshly built
constructor. Either way the compare-and-swap would keep failing and the
registering thread would spin.

The OPAQUE pragma keeps @f@ an unknown function inside 'modifyTimeouts', so
there is never a @case@ on @oldVar@ to rewrite. OPAQUE rather than NOINLINE:
both stop inlining and specialisation, but NOINLINE still allows the
worker/wrapper transformation. Today that is harmless (the worker only
unboxes the 'IORef' and still takes @f@ as an argument), but OPAQUE states
the requirement directly: the body must be compiled as written and every call
must go to it. It also costs nothing, since 'editTimeouts' passes a closure
anyway.

An alternative is to hide the expected pointer in a box built by a NOINLINE
function before @f@ runs (like the @Ticket@ type of the atomic-primops
package), but that allocates on every attempt.
-}

#endif

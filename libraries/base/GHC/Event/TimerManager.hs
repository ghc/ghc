{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns
           , CPP
           , ExistentialQuantification
           , NoImplicitPrelude
           , RecordWildCards
           , TypeSynonymInstances
           , FlexibleInstances
  #-}

module GHC.Event.TimerManager
    ( -- * Types
      TimerManager

      -- * Creation
    , new
    , newWith
    , newDefaultBackend

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

import Control.Exception (finally)
import Control.Monad ((=<<), liftM, sequence_, when)
import Data.IORef (IORef, atomicModifyIORef, mkWeakIORef, newIORef, readIORef,
                   writeIORef)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import GHC.Base
import GHC.Conc.Signal (runHandlers)
import GHC.Num (Num(..))
import GHC.Real ((/), fromIntegral )
import GHC.Show (Show(..))
import GHC.Event.Clock (getMonotonicTime)
import GHC.Event.Control
import GHC.Event.Internal (Backend, Event, evtRead, Timeout(..))
import GHC.Event.Unique (Unique, UniqueSource, newSource, newUnique)
import System.Posix.Types (Fd)

import qualified GHC.Event.Internal as I
import qualified GHC.Event.PSQ as Q

#if defined(HAVE_POLL)
import qualified GHC.Event.Poll   as Poll
#else
# error not implemented for this operating system
#endif

------------------------------------------------------------------------
-- Types

-- | A timeout registration cookie.
newtype TimeoutKey   = TK Unique
    deriving (Eq)

-- | Callback invoked on timeout events.
type TimeoutCallback = IO ()

data State = Created
           | Running
           | Dying
           | Finished
             deriving (Eq, Show)

-- | A priority search queue, with timeouts as priorities.
type TimeoutQueue = Q.PSQ TimeoutCallback

{-
Instead of directly modifying the 'TimeoutQueue' in
e.g. 'registerTimeout' we keep a list of edits to perform, in the form
of a chain of function closures, and have the I/O manager thread
perform the edits later.  This exist to address the following GC
problem:

Since e.g. 'registerTimeout' doesn't force the evaluation of the
thunks inside the 'emTimeouts' IORef a number of thunks build up
inside the IORef.  If the I/O manager thread doesn't evaluate these
thunks soon enough they'll get promoted to the old generation and
become roots for all subsequent minor GCs.

When the thunks eventually get evaluated they will each create a new
intermediate 'TimeoutQueue' that immediately becomes garbage.  Since
the thunks serve as roots until the next major GC these intermediate
'TimeoutQueue's will get copied unnecesarily in the next minor GC,
increasing GC time.  This problem is known as "floating garbage".

Keeping a list of edits doesn't stop this from happening but makes the
amount of data that gets copied smaller.

TODO: Evaluate the content of the IORef to WHNF on each insert once
this bug is resolved: http://hackage.haskell.org/trac/ghc/ticket/3838
-}

-- | An edit to apply to a 'TimeoutQueue'.
type TimeoutEdit = TimeoutQueue -> TimeoutQueue

-- | The event manager state.
data TimerManager = TimerManager
    { emBackend      :: !Backend
    , emTimeouts     :: {-# UNPACK #-} !(IORef TimeoutEdit)
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
newDefaultBackend = error "no back end for this platform"
#endif

-- | Create a new event manager.
new :: IO TimerManager
new = newWith =<< newDefaultBackend

newWith :: Backend -> IO TimerManager
newWith be = do
  timeouts <- newIORef id
  ctrl <- newControl True
  state <- newIORef Created
  us <- newSource
  _ <- mkWeakIORef state $ do
               st <- atomicModifyIORef state $ \s -> (Finished, s)
               when (st /= Finished) $ do
                 I.delete be
                 closeControl ctrl
  let mgr = TimerManager { emBackend = be
                         , emTimeouts = timeouts
                         , emState = state
                         , emUniqueSource = us
                         , emControl = ctrl
                         }
  I.modifyFd be (controlReadFd ctrl) mempty evtRead
  I.modifyFd be (wakeupReadFd ctrl) mempty evtRead
  return mgr

-- | Asynchronously shuts down the event manager, if running.
shutdown :: TimerManager -> IO ()
shutdown mgr = do
  state <- atomicModifyIORef (emState mgr) $ \s -> (Dying, s)
  when (state == Running) $ sendDie (emControl mgr)

finished :: TimerManager -> IO Bool
finished mgr = (== Finished) `liftM` readIORef (emState mgr)

cleanup :: TimerManager -> IO ()
cleanup TimerManager{..} = do
  writeIORef emState Finished
  I.delete emBackend
  closeControl emControl

------------------------------------------------------------------------
-- Event loop

-- | Start handling events.  This function loops until told to stop,
-- using 'shutdown'.
--
-- /Note/: This loop can only be run once per 'TimerManager', as it
-- closes all of its control resources when it finishes.
loop :: TimerManager -> IO ()
loop mgr@TimerManager{..} = do
  state <- atomicModifyIORef emState $ \s -> case s of
    Created -> (Running, s)
    _       -> (s, s)
  case state of
    Created -> go Q.empty `finally` cleanup mgr
    Dying   -> cleanup mgr
    _       -> do cleanup mgr
                  error $ "GHC.Event.Manager.loop: state is already " ++
                      show state
 where
  go q = do (running, q') <- step mgr q
            when running $ go q'

step :: TimerManager -> TimeoutQueue -> IO (Bool, TimeoutQueue)
step mgr@TimerManager{..} tq = do
  (timeout, q') <- mkTimeout tq
  _ <- I.poll emBackend (Just timeout) (handleControlEvent mgr)
  state <- readIORef emState
  state `seq` return (state == Running, q')
 where

  -- | Call all expired timer callbacks and return the time to the
  -- next timeout.
  mkTimeout :: TimeoutQueue -> IO (Timeout, TimeoutQueue)
  mkTimeout q = do
      now <- getMonotonicTime
      applyEdits <- atomicModifyIORef emTimeouts $ \f -> (id, f)
      let (expired, q'') = let q' = applyEdits q in q' `seq` Q.atMost now q'
      sequence_ $ map Q.value expired
      let timeout = case Q.minView q'' of
            Nothing             -> Forever
            Just (Q.E _ t _, _) ->
                -- This value will always be positive since the call
                -- to 'atMost' above removed any timeouts <= 'now'
                let t' = t - now in t' `seq` Timeout t'
      return (timeout, q'')

-- | Wake up the event manager.
wakeManager :: TimerManager -> IO ()
wakeManager mgr = sendWakeup (emControl mgr)

------------------------------------------------------------------------
-- Registering interest in timeout events

-- | Register a timeout in the given number of microseconds.  The
-- returned 'TimeoutKey' can be used to later unregister or update the
-- timeout.  The timeout is automatically unregistered after the given
-- time has passed.
registerTimeout :: TimerManager -> Int -> TimeoutCallback -> IO TimeoutKey
registerTimeout mgr us cb = do
  !key <- newUnique (emUniqueSource mgr)
  if us <= 0 then cb
    else do
      now <- getMonotonicTime
      let expTime = fromIntegral us / 1000000.0 + now

      -- We intentionally do not evaluate the modified map to WHNF here.
      -- Instead, we leave a thunk inside the IORef and defer its
      -- evaluation until mkTimeout in the event loop.  This is a
      -- workaround for a nasty IORef contention problem that causes the
      -- thread-delay benchmark to take 20 seconds instead of 0.2.
      atomicModifyIORef (emTimeouts mgr) $ \f ->
          let f' = (Q.insert key expTime cb) . f in (f', ())
      wakeManager mgr
  return $ TK key

-- | Unregister an active timeout.
unregisterTimeout :: TimerManager -> TimeoutKey -> IO ()
unregisterTimeout mgr (TK key) = do
  atomicModifyIORef (emTimeouts mgr) $ \f ->
      let f' = (Q.delete key) . f in (f', ())
  wakeManager mgr

-- | Update an active timeout to fire in the given number of
-- microseconds.
updateTimeout :: TimerManager -> TimeoutKey -> Int -> IO ()
updateTimeout mgr (TK key) us = do
  now <- getMonotonicTime
  let expTime = fromIntegral us / 1000000.0 + now

  atomicModifyIORef (emTimeouts mgr) $ \f ->
      let f' = (Q.adjust (const expTime) key) . f in (f', ())
  wakeManager mgr

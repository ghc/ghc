{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns
           , CPP
           , ExistentialQuantification
           , NoImplicitPrelude
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

import Control.Exception (finally)
import Data.Foldable (sequence_)
import Data.IORef (IORef, atomicModifyIORef', mkWeakIORef, newIORef, readIORef,
                   writeIORef)
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

-- | An edit to apply to a 'TimeoutQueue'.
type TimeoutEdit = TimeoutQueue -> TimeoutQueue

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
  ctrl <- newControl True
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
                  errorWithoutStackTrace $ "GHC.Event.Manager.loop: state is already " ++
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

  -- | Call all expired timer callbacks and return the time to the
  -- next timeout.
  mkTimeout :: IO Timeout
  mkTimeout = do
      now <- getMonotonicTime
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

      editTimeouts mgr (Q.insert key expTime cb)
      wakeManager mgr
  return $ TK key

-- | Unregister an active timeout.
unregisterTimeout :: TimerManager -> TimeoutKey -> IO ()
unregisterTimeout mgr (TK key) = do
  editTimeouts mgr (Q.delete key)
  wakeManager mgr

-- | Update an active timeout to fire in the given number of
-- microseconds.
updateTimeout :: TimerManager -> TimeoutKey -> Int -> IO ()
updateTimeout mgr (TK key) us = do
  now <- getMonotonicTime
  let expTime = fromIntegral us / 1000000.0 + now

  editTimeouts mgr (Q.adjust (const expTime) key)
  wakeManager mgr

editTimeouts :: TimerManager -> TimeoutEdit -> IO ()
editTimeouts mgr g = atomicModifyIORef' (emTimeouts mgr) $ \tq -> (g tq, ())


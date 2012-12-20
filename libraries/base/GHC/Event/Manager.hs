{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns
           , CPP
           , ExistentialQuantification
           , NoImplicitPrelude
           , RecordWildCards
           , TypeSynonymInstances
           , FlexibleInstances
  #-}

module GHC.Event.Manager
    ( -- * Types
      EventManager

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

      -- * Registering interest in I/O events
    , Event
    , evtRead
    , evtWrite
    , IOCallback
    , FdKey(keyFd)
    , registerFd_
    , registerFd
    , unregisterFd_
    , unregisterFd
    , closeFd

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

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar)
import Control.Exception (finally)
import Control.Monad ((=<<), forM_, liftM, sequence_, when)
import Data.IORef (IORef, atomicModifyIORef, mkWeakIORef, newIORef, readIORef,
                   writeIORef)
import Data.Maybe (Maybe(..))
import Data.Monoid (mappend, mconcat, mempty)
import GHC.Base
import GHC.Conc.Signal (runHandlers)
import GHC.List (filter)
import GHC.Num (Num(..))
import GHC.Real ((/), fromIntegral )
import GHC.Show (Show(..))
import GHC.Event.Clock (getMonotonicTime)
import GHC.Event.Control
import GHC.Event.Internal (Backend, Event, evtClose, evtRead, evtWrite,
                           Timeout(..))
import GHC.Event.Unique (Unique, UniqueSource, newSource, newUnique)
import System.Posix.Types (Fd)

import qualified GHC.Event.IntMap as IM
import qualified GHC.Event.Internal as I
import qualified GHC.Event.PSQ as Q

#if defined(HAVE_KQUEUE)
import qualified GHC.Event.KQueue as KQueue
#elif defined(HAVE_EPOLL)
import qualified GHC.Event.EPoll  as EPoll
#elif defined(HAVE_POLL)
import qualified GHC.Event.Poll   as Poll
#else
# error not implemented for this operating system
#endif

------------------------------------------------------------------------
-- Types

data FdData = FdData {
      fdKey       :: {-# UNPACK #-} !FdKey
    , fdEvents    :: {-# UNPACK #-} !Event
    , _fdCallback :: !IOCallback
    }

-- | A file descriptor registration cookie.
data FdKey = FdKey {
      keyFd     :: {-# UNPACK #-} !Fd
    , keyUnique :: {-# UNPACK #-} !Unique
    } deriving (Eq, Show)

-- | Callback invoked on I/O events.
type IOCallback = FdKey -> Event -> IO ()

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
data EventManager = EventManager
    { emBackend      :: !Backend
    , emFds          :: {-# UNPACK #-} !(MVar (IM.IntMap [FdData]))
    , emTimeouts     :: {-# UNPACK #-} !(IORef TimeoutEdit)
    , emState        :: {-# UNPACK #-} !(IORef State)
    , emUniqueSource :: {-# UNPACK #-} !UniqueSource
    , emControl      :: {-# UNPACK #-} !Control
    }

------------------------------------------------------------------------
-- Creation

handleControlEvent :: EventManager -> FdKey -> Event -> IO ()
handleControlEvent mgr reg _evt = do
  msg <- readControlMessage (emControl mgr) (keyFd reg)
  case msg of
    CMsgWakeup      -> return ()
    CMsgDie         -> writeIORef (emState mgr) Finished
    CMsgSignal fp s -> runHandlers fp s

newDefaultBackend :: IO Backend
#if defined(HAVE_KQUEUE)
newDefaultBackend = KQueue.new
#elif defined(HAVE_EPOLL)
newDefaultBackend = EPoll.new
#elif defined(HAVE_POLL)
newDefaultBackend = Poll.new
#else
newDefaultBackend = error "no back end for this platform"
#endif

-- | Create a new event manager.
new :: Bool -> IO EventManager
new shouldRegister = newWith shouldRegister =<< newDefaultBackend

newWith :: Bool -> Backend -> IO EventManager
newWith shouldRegister be = do
  iofds <- newMVar IM.empty
  timeouts <- newIORef id
  ctrl <- newControl shouldRegister
  state <- newIORef Created
  us <- newSource
  _ <- mkWeakIORef state $ do
               st <- atomicModifyIORef state $ \s -> (Finished, s)
               when (st /= Finished) $ do
                 I.delete be
                 closeControl ctrl
  let mgr = EventManager { emBackend = be
                         , emFds = iofds
                         , emTimeouts = timeouts
                         , emState = state
                         , emUniqueSource = us
                         , emControl = ctrl
                         }
  _ <- registerFd_ mgr (handleControlEvent mgr) (controlReadFd ctrl) evtRead
  _ <- registerFd_ mgr (handleControlEvent mgr) (wakeupReadFd ctrl) evtRead
  return mgr

-- | Asynchronously shuts down the event manager, if running.
shutdown :: EventManager -> IO ()
shutdown mgr = do
  state <- atomicModifyIORef (emState mgr) $ \s -> (Dying, s)
  when (state == Running) $ sendDie (emControl mgr)

finished :: EventManager -> IO Bool
finished mgr = (== Finished) `liftM` readIORef (emState mgr)

cleanup :: EventManager -> IO ()
cleanup EventManager{..} = do
  writeIORef emState Finished
  I.delete emBackend
  closeControl emControl

------------------------------------------------------------------------
-- Event loop

-- | Start handling events.  This function loops until told to stop,
-- using 'shutdown'.
--
-- /Note/: This loop can only be run once per 'EventManager', as it
-- closes all of its control resources when it finishes.
loop :: EventManager -> IO ()
loop mgr@EventManager{..} = do
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

step :: EventManager -> TimeoutQueue -> IO (Bool, TimeoutQueue)
step mgr@EventManager{..} tq = do
  (timeout, q') <- mkTimeout tq
  I.poll emBackend timeout (onFdEvent mgr)
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

------------------------------------------------------------------------
-- Registering interest in I/O events

-- | Register interest in the given events, without waking the event
-- manager thread.  The 'Bool' return value indicates whether the
-- event manager ought to be woken.
registerFd_ :: EventManager -> IOCallback -> Fd -> Event
            -> IO (FdKey, Bool)
registerFd_ EventManager{..} cb fd evs = do
  u <- newUnique emUniqueSource
  modifyMVar emFds $ \oldMap -> do
    let fd'  = fromIntegral fd
        reg  = FdKey fd u
        !fdd = FdData reg evs cb
        (!newMap, (oldEvs, newEvs)) =
            case IM.insertWith (++) fd' [fdd] oldMap of
              (Nothing,   n) -> (n, (mempty, evs))
              (Just prev, n) -> (n, pairEvents prev newMap fd')
        modify = oldEvs /= newEvs
    when modify $ I.modifyFd emBackend fd oldEvs newEvs
    return (newMap, (reg, modify))
{-# INLINE registerFd_ #-}

-- | @registerFd mgr cb fd evs@ registers interest in the events @evs@
-- on the file descriptor @fd@.  @cb@ is called for each event that
-- occurs.  Returns a cookie that can be handed to 'unregisterFd'.
registerFd :: EventManager -> IOCallback -> Fd -> Event -> IO FdKey
registerFd mgr cb fd evs = do
  (r, wake) <- registerFd_ mgr cb fd evs
  when wake $ wakeManager mgr
  return r
{-# INLINE registerFd #-}

-- | Wake up the event manager.
wakeManager :: EventManager -> IO ()
wakeManager mgr = sendWakeup (emControl mgr)

eventsOf :: [FdData] -> Event
eventsOf = mconcat . map fdEvents

pairEvents :: [FdData] -> IM.IntMap [FdData] -> Int -> (Event, Event)
pairEvents prev m fd = let l = eventsOf prev
                           r = case IM.lookup fd m of
                                 Nothing  -> mempty
                                 Just fds -> eventsOf fds
                       in (l, r)

-- | Drop a previous file descriptor registration, without waking the
-- event manager thread.  The return value indicates whether the event
-- manager ought to be woken.
unregisterFd_ :: EventManager -> FdKey -> IO Bool
unregisterFd_ EventManager{..} (FdKey fd u) =
  modifyMVar emFds $ \oldMap -> do
    let dropReg cbs = case filter ((/= u) . keyUnique . fdKey) cbs of
                          []   -> Nothing
                          cbs' -> Just cbs'
        fd' = fromIntegral fd
        (!newMap, (oldEvs, newEvs)) =
            case IM.updateWith dropReg fd' oldMap of
              (Nothing,   _)    -> (oldMap, (mempty, mempty))
              (Just prev, newm) -> (newm, pairEvents prev newm fd')
        modify = oldEvs /= newEvs
    when modify $ I.modifyFd emBackend fd oldEvs newEvs
    return (newMap, modify)

-- | Drop a previous file descriptor registration.
unregisterFd :: EventManager -> FdKey -> IO ()
unregisterFd mgr reg = do
  wake <- unregisterFd_ mgr reg
  when wake $ wakeManager mgr

-- | Close a file descriptor in a race-safe way.
closeFd :: EventManager -> (Fd -> IO ()) -> Fd -> IO ()
closeFd mgr close fd = do
  fds <- modifyMVar (emFds mgr) $ \oldMap -> do
    close fd
    case IM.delete (fromIntegral fd) oldMap of
      (Nothing,  _)       -> return (oldMap, [])
      (Just fds, !newMap) -> do
        when (eventsOf fds /= mempty) $ wakeManager mgr
        return (newMap, fds)
  forM_ fds $ \(FdData reg ev cb) -> cb reg (ev `mappend` evtClose)

------------------------------------------------------------------------
-- Registering interest in timeout events

-- | Register a timeout in the given number of microseconds.  The
-- returned 'TimeoutKey' can be used to later unregister or update the
-- timeout.  The timeout is automatically unregistered after the given
-- time has passed.
registerTimeout :: EventManager -> Int -> TimeoutCallback -> IO TimeoutKey
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
unregisterTimeout :: EventManager -> TimeoutKey -> IO ()
unregisterTimeout mgr (TK key) = do
  atomicModifyIORef (emTimeouts mgr) $ \f ->
      let f' = (Q.delete key) . f in (f', ())
  wakeManager mgr

-- | Update an active timeout to fire in the given number of
-- microseconds.
updateTimeout :: EventManager -> TimeoutKey -> Int -> IO ()
updateTimeout mgr (TK key) us = do
  now <- getMonotonicTime
  let expTime = fromIntegral us / 1000000.0 + now

  atomicModifyIORef (emTimeouts mgr) $ \f ->
      let f' = (Q.adjust (const expTime) key) . f in (f', ())
  wakeManager mgr

------------------------------------------------------------------------
-- Utilities

-- | Call the callbacks corresponding to the given file descriptor.
onFdEvent :: EventManager -> Fd -> Event -> IO ()
onFdEvent mgr fd evs = do
  fds <- readMVar (emFds mgr)
  case IM.lookup (fromIntegral fd) fds of
      Just cbs -> forM_ cbs $ \(FdData reg ev cb) ->
                    when (evs `I.eventIs` ev) $ cb reg evs
      Nothing  -> return ()

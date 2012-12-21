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

      -- * State
    , callbackTableVar

      -- * Registering interest in I/O events
    , Event
    , evtRead
    , evtWrite
    , IOCallback
    , FdKey(keyFd)
    , FdData
    , registerFd_
    , registerFd
    , unregisterFd_
    , unregisterFd
    , closeFd
    , closeFd_
    ) where

#include "EventConfig.h"

------------------------------------------------------------------------
-- Imports

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar)
import Control.Exception (finally)
import Control.Monad ((=<<), forM_, liftM, sequence_, when, replicateM)
import Data.IORef (IORef, atomicModifyIORef, mkWeakIORef, newIORef, readIORef,
                   writeIORef)
import Data.Maybe (Maybe(..))
import Data.Monoid (mappend, mconcat, mempty)
import GHC.Arr (Array, (!), listArray)
import GHC.Base
import GHC.Conc.Signal (runHandlers)
import GHC.Conc.Sync (yield)
import GHC.List (filter)
import GHC.Num (Num(..))
import GHC.Real ((/), fromIntegral, mod)
import GHC.Show (Show(..))
import GHC.Event.Control
import GHC.Event.Internal (Backend, Event, evtClose, evtRead, evtWrite,
                           Timeout(..))
import GHC.Event.Unique (Unique, UniqueSource, newSource, newUnique)
import System.Posix.Types (Fd)

import qualified GHC.Event.IntMap as IM
import qualified GHC.Event.Internal as I

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

data State = Created
           | Running
           | Dying
           | Finished
             deriving (Eq, Show)

-- | The event manager state.
data EventManager = EventManager
    { emBackend      :: !Backend
    , emFds          :: {-# UNPACK #-} !(Array Int (MVar (IM.IntMap [FdData])))
    , emState        :: {-# UNPACK #-} !(IORef State)
    , emUniqueSource :: {-# UNPACK #-} !UniqueSource
    , emControl      :: {-# UNPACK #-} !Control
    }

callbackArraySize :: Int
callbackArraySize = 32

hashFd :: Fd -> Int
hashFd fd = fromIntegral fd `mod` callbackArraySize
{-# INLINE hashFd #-}

callbackTableVar :: EventManager -> Fd -> MVar (IM.IntMap [FdData])
callbackTableVar mgr fd = emFds mgr ! hashFd fd
{-# INLINE callbackTableVar #-}
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
new :: IO EventManager
new = newWith =<< newDefaultBackend

newWith :: Backend -> IO EventManager
newWith be = do
  iofds <- fmap (listArray (0, callbackArraySize-1)) $
           replicateM callbackArraySize (newMVar IM.empty)
  ctrl <- newControl False
  state <- newIORef Created
  us <- newSource
  _ <- mkWeakIORef state $ do
               st <- atomicModifyIORef state $ \s -> (Finished, s)
               when (st /= Finished) $ do
                 I.delete be
                 closeControl ctrl
  let mgr = EventManager { emBackend = be
                         , emFds = iofds
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
    Created -> go `finally` cleanup mgr
    Dying   -> cleanup mgr
    _       -> do cleanup mgr
                  error $ "GHC.Event.Manager.loop: state is already " ++
                      show state
 where
  go = do running <- step mgr
          when running (yield >> go)

step :: EventManager -> IO Bool
step mgr@EventManager{..} = do
  waitForIO
  state <- readIORef emState
  state `seq` return (state == Running)
  where
    waitForIO = do
      n1 <- I.poll emBackend Nothing (onFdEvent mgr)
      when (n1 <= 0) $ do
        yield
        n2 <- I.poll emBackend Nothing (onFdEvent mgr)
        when (n2 <= 0) $ do
          _ <- I.poll emBackend (Just Forever) (onFdEvent mgr)
          return ()

------------------------------------------------------------------------
-- Registering interest in I/O events

-- | Register interest in the given events, without waking the event
-- manager thread.  The 'Bool' return value indicates whether the
-- event manager ought to be woken.
registerFd_ :: EventManager -> IOCallback -> Fd -> Event
            -> IO (FdKey, Bool)
registerFd_ mgr@(EventManager{..}) cb fd evs = do
  u <- newUnique emUniqueSource
  modifyMVar (callbackTableVar mgr fd) $ \oldMap -> do
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
unregisterFd_ mgr@(EventManager{..}) (FdKey fd u) =
  modifyMVar (callbackTableVar mgr fd) $ \oldMap -> do
    let dropReg = nullToNothing . filter ((/= u) . keyUnique . fdKey) 
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
  fds <- modifyMVar (callbackTableVar mgr fd) $ \oldMap -> do
    close fd
    case IM.delete (fromIntegral fd) oldMap of
      (Nothing,  _)       -> return (oldMap, [])
      (Just fds, !newMap) -> do
        when (eventsOf fds /= mempty) $ wakeManager mgr
        return (newMap, fds)
  forM_ fds $ \(FdData reg ev cb) -> cb reg (ev `mappend` evtClose)

-- | Does everything that closeFd does, except for updating the callback tables.
-- It assumes the caller will update the callback tables and that the caller
-- holds the callback table lock for the fd.
closeFd_ :: EventManager -> IM.IntMap [FdData] -> Fd -> IO (IM.IntMap [FdData])
closeFd_ mgr oldMap fd = do
  case IM.delete (fromIntegral fd) oldMap of
    (Nothing,  _)       -> return oldMap
    (Just fds, !newMap) -> do
      let oldEvs = eventsOf fds
      I.modifyFd (emBackend mgr) fd oldEvs mempty
      forM_ fds $ \(FdData reg ev cb) -> cb reg (ev `mappend` evtClose)
      return newMap
------------------------------------------------------------------------
-- Utilities

-- | Call the callbacks corresponding to the given file descriptor.
onFdEvent :: EventManager -> Fd -> Event -> IO ()
onFdEvent mgr fd evs = do
  fds <- readMVar (callbackTableVar mgr fd)
  case IM.lookup (fromIntegral fd) fds of
      Just cbs -> forM_ cbs $ \(FdData reg ev cb) ->
                    when (evs `I.eventIs` ev) $ cb reg evs
      Nothing  -> return ()

nullToNothing :: [a] -> Maybe [a]
nullToNothing []       = Nothing
nullToNothing xs@(_:_) = Just xs

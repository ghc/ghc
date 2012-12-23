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
    , release
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

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar, putMVar,
                                tryPutMVar, takeMVar)
import Control.Exception (onException)
import Control.Monad ((=<<), forM_, liftM, sequence_, when, replicateM, void)
import Data.IORef (IORef, atomicModifyIORef, mkWeakIORef, newIORef, readIORef,
                   writeIORef)
import Data.Maybe (Maybe(..))
import Data.Monoid (mappend, mconcat, mempty)
import Data.Tuple (snd)
import GHC.Arr (Array, (!), listArray)
import GHC.Base
import GHC.Conc.Signal (runHandlers)
import GHC.Conc.Sync (yield)
import GHC.List (filter)
import GHC.Num (Num(..))
import GHC.Real (fromIntegral, mod)
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
           | Releasing
           | Finished
             deriving (Eq, Show)

-- | The event manager state.
data EventManager = EventManager
    { emBackend      :: !Backend
    , emFds          :: {-# UNPACK #-} !(Array Int (MVar (IM.IntMap [FdData])))
    , emState        :: {-# UNPACK #-} !(IORef State)
    , emUniqueSource :: {-# UNPACK #-} !UniqueSource
    , emControl      :: {-# UNPACK #-} !Control
    , emOneShot      :: !Bool
    , emLock         :: MVar ()
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

handleControlEvent :: EventManager -> Fd -> Event -> IO ()
handleControlEvent mgr fd _evt = do
  msg <- readControlMessage (emControl mgr) fd
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
new oneShot = newWith oneShot =<< newDefaultBackend

newWith :: Bool -> Backend -> IO EventManager
newWith oneShot be = do
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
  lockVar <- newMVar ()
  let mgr = EventManager { emBackend = be
                         , emFds = iofds
                         , emState = state
                         , emUniqueSource = us
                         , emControl = ctrl
                         , emOneShot = oneShot
                         , emLock = lockVar
                         }
  registerControlFd mgr (controlReadFd ctrl) evtRead
  registerControlFd mgr (wakeupReadFd ctrl) evtRead
  return mgr

registerControlFd :: EventManager -> Fd -> Event -> IO ()
registerControlFd mgr fd evs = I.modifyFd (emBackend mgr) fd mempty evs

-- | Asynchronously shuts down the event manager, if running.
shutdown :: EventManager -> IO ()
shutdown mgr = do
  state <- atomicModifyIORef (emState mgr) $ \s -> (Dying, s)
  when (state == Running) $ sendDie (emControl mgr)

-- | Asynchronously tell the thread executing the event
-- manager loop to exit.
release :: EventManager -> IO ()
release EventManager{..} = do
  state <- atomicModifyIORef emState $ \s -> (Releasing, s)
  when (state == Running) $ sendWakeup emControl

finished :: EventManager -> IO Bool
finished mgr = (== Finished) `liftM` readIORef (emState mgr)

cleanup :: EventManager -> IO ()
cleanup EventManager{..} = do
  writeIORef emState Finished
  void $ tryPutMVar emLock ()
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
  void $ takeMVar emLock
  state <- atomicModifyIORef emState $ \s -> case s of
    Created -> (Running, s)
    Releasing -> (Running, s)
    _       -> (s, s)
  case state of
    Created   -> go `onException` cleanup mgr
    Releasing -> go `onException` cleanup mgr
    Dying     -> cleanup mgr
    _         -> do cleanup mgr
                    error $ "GHC.Event.Manager.loop: state is already " ++
                            show state
 where
  go = do state <- step mgr
          case state of
            Running   -> yield >> go
            Releasing -> putMVar emLock ()
            _         -> cleanup mgr

step :: EventManager -> IO State
step mgr@EventManager{..} = do
  waitForIO
  state <- readIORef emState
  state `seq` return state
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
  let fd'  = fromIntegral fd
      reg  = FdKey fd u
      !fdd = FdData reg evs cb
  modifyMVar (callbackTableVar mgr fd) $ \oldMap -> 
#if defined(HAVE_EPOLL) || defined(HAVE_KQUEUE)
    if emOneShot
    then case IM.insertWith (++) fd' [fdd] oldMap of
      (Nothing,   n) -> do I.modifyFdOnce emBackend fd evs
                           return (n, (reg, False))
      (Just prev, n) -> do I.modifyFdOnce emBackend fd (combineEvents evs prev)
                           return (n, (reg, False))
    else
#endif      
      let (!newMap, (oldEvs, newEvs)) =
            case IM.insertWith (++) fd' [fdd] oldMap of
              (Nothing,   n) -> (n, (mempty, evs))
              (Just prev, n) -> (n, (eventsOf prev, combineEvents evs prev))
          modify = oldEvs /= newEvs
      in do when modify $ I.modifyFd emBackend fd oldEvs newEvs
            return (newMap, (reg, modify))
{-# INLINE registerFd_ #-}

combineEvents :: Event -> [FdData] -> Event
combineEvents ev [fdd] = mappend ev (fdEvents fdd)
combineEvents ev fdds  = mappend ev (eventsOf fdds)
{-# INLINE combineEvents #-}

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
wakeManager mgr =
#if defined(HAVE_EPOLL) || defined(HAVE_KQUEUE)
  return ()
#else    
  sendWakeup (emControl mgr)
#endif

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
    when modify $
      if emOneShot && newEvs /= mempty
      then I.modifyFdOnce emBackend fd newEvs
      else I.modifyFd emBackend fd oldEvs newEvs
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
onFdEvent mgr fd evs =
  if fd == controlReadFd (emControl mgr) || fd == wakeupReadFd (emControl mgr)
  then handleControlEvent mgr fd evs
  else
    if emOneShot mgr
    then
      do fdds <- modifyMVar (callbackTableVar mgr fd) $ \oldMap ->
            case IM.delete fd' oldMap of
              (Nothing, _)       -> return (oldMap, [])
              (Just cbs, newmap) -> selectCallbacks newmap cbs
         forM_ fdds $ \(FdData reg _ cb) -> cb reg evs
    else 
      do fds <- readMVar (callbackTableVar mgr fd)
         case IM.lookup fd' fds of
           Just cbs -> forM_ cbs $ \(FdData reg ev cb) -> do
             when (evs `I.eventIs` ev) $ cb reg evs
           Nothing  -> return ()
  where
    fd' :: Int
    fd' = fromIntegral fd

    selectCallbacks ::
      IM.IntMap [FdData] -> [FdData] -> IO (IM.IntMap [FdData], [FdData])
    selectCallbacks curmap cbs = aux cbs [] []
      where
        -- nothing to rearm.
        aux [] _    []          =
#if defined(HAVE_EPOLL) || defined(HAVE_KQUEUE)
           return (curmap, cbs)
#else
          do I.modifyFd (emBackend mgr) fd (eventsOf cbs) mempty
             return (curmap, cbs)
#endif
        -- reinsert and rearm; note that we already have the lock on the
        -- callback table for this fd, and we deleted above, so we know there
        -- is no entry in the table for this fd.
        aux [] fdds saved@(_:_) = do
#if defined(HAVE_EPOLL) || defined(HAVE_KQUEUE)          
          I.modifyFdOnce (emBackend mgr) fd $ eventsOf saved
#else
          I.modifyFd (emBackend mgr) fd (eventsOf cbs) $ eventsOf saved
#endif    
          return (snd $ IM.insertWith (\_ _ -> saved) fd' saved curmap, fdds)

        -- continue, saving those callbacks that don't match the event
        aux (fdd@(FdData _ evs' _) : cbs') fdds saved
          | evs `I.eventIs` evs' = aux cbs' (fdd:fdds) saved
          | otherwise            = aux cbs' fdds (fdd:saved)

nullToNothing :: [a] -> Maybe [a]
nullToNothing []       = Nothing
nullToNothing xs@(_:_) = Just xs

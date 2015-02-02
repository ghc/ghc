{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns
           , CPP
           , ExistentialQuantification
           , NoImplicitPrelude
           , RecordWildCards
           , TypeSynonymInstances
           , FlexibleInstances
  #-}

-- |
-- The event manager supports event notification on fds. Each fd may
-- have multiple callbacks registered, each listening for a different
-- set of events. Registrations may be automatically deactivated after
-- the occurrence of an event ("one-shot mode") or active until
-- explicitly unregistered.
--
-- If an fd has only one-shot registrations then we use one-shot
-- polling if available. Otherwise we use multi-shot polling.

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
    , emControl

      -- * Registering interest in I/O events
    , Lifetime (..)
    , Event
    , evtRead
    , evtWrite
    , IOCallback
    , FdKey(keyFd)
    , FdData
    , registerFd
    , unregisterFd_
    , unregisterFd
    , closeFd
    , closeFd_
    ) where

#include "EventConfig.h"

------------------------------------------------------------------------
-- Imports

import Control.Concurrent.MVar (MVar, newMVar, putMVar,
                                tryPutMVar, takeMVar, withMVar)
import Control.Exception (onException)
import Data.Bits ((.&.))
import Data.Foldable (forM_)
import Data.Functor (void)
import Data.IORef (IORef, atomicModifyIORef', mkWeakIORef, newIORef, readIORef,
                   writeIORef)
import Data.Maybe (maybe)
import Data.OldList (partition)
import GHC.Arr (Array, (!), listArray)
import GHC.Base
import GHC.Conc.Sync (yield)
import GHC.List (filter, replicate)
import GHC.Num (Num(..))
import GHC.Real (fromIntegral)
import GHC.Show (Show(..))
import GHC.Event.Control
import GHC.Event.IntTable (IntTable)
import GHC.Event.Internal (Backend, Event, evtClose, evtRead, evtWrite,
                           Lifetime(..), EventLifetime, Timeout(..))
import GHC.Event.Unique (Unique, UniqueSource, newSource, newUnique)
import System.Posix.Types (Fd)

import qualified GHC.Event.IntTable as IT
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
    , fdEvents    :: {-# UNPACK #-} !EventLifetime
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
    , emFds          :: {-# UNPACK #-} !(Array Int (MVar (IntTable [FdData])))
    , emState        :: {-# UNPACK #-} !(IORef State)
    , emUniqueSource :: {-# UNPACK #-} !UniqueSource
    , emControl      :: {-# UNPACK #-} !Control
    , emLock         :: {-# UNPACK #-} !(MVar ())
    }

-- must be power of 2
callbackArraySize :: Int
callbackArraySize = 32

hashFd :: Fd -> Int
hashFd fd = fromIntegral fd .&. (callbackArraySize - 1)
{-# INLINE hashFd #-}

callbackTableVar :: EventManager -> Fd -> MVar (IntTable [FdData])
callbackTableVar mgr fd = emFds mgr ! hashFd fd
{-# INLINE callbackTableVar #-}

haveOneShot :: Bool
{-# INLINE haveOneShot #-}
#if defined(darwin_HOST_OS) || defined(ios_HOST_OS)
haveOneShot = False
#elif defined(HAVE_EPOLL) || defined(HAVE_KQUEUE)
haveOneShot = True
#else
haveOneShot = False
#endif
------------------------------------------------------------------------
-- Creation

handleControlEvent :: EventManager -> Fd -> Event -> IO ()
handleControlEvent mgr fd _evt = do
  msg <- readControlMessage (emControl mgr) fd
  case msg of
    CMsgWakeup      -> return ()
    CMsgDie         -> writeIORef (emState mgr) Finished
    _               -> return ()

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

-- | Create a new 'EventManager' with the given polling backend.
newWith :: Backend -> IO EventManager
newWith be = do
  iofds <- fmap (listArray (0, callbackArraySize-1)) $
           replicateM callbackArraySize (newMVar =<< IT.new 8)
  ctrl <- newControl False
  state <- newIORef Created
  us <- newSource
  _ <- mkWeakIORef state $ do
               st <- atomicModifyIORef' state $ \s -> (Finished, s)
               when (st /= Finished) $ do
                 I.delete be
                 closeControl ctrl
  lockVar <- newMVar ()
  let mgr = EventManager { emBackend = be
                         , emFds = iofds
                         , emState = state
                         , emUniqueSource = us
                         , emControl = ctrl
                         , emLock = lockVar
                         }
  registerControlFd mgr (controlReadFd ctrl) evtRead
  registerControlFd mgr (wakeupReadFd ctrl) evtRead
  return mgr
  where
    replicateM n x = sequence (replicate n x)

failOnInvalidFile :: String -> Fd -> IO Bool -> IO ()
failOnInvalidFile loc fd m = do
  ok <- m
  when (not ok) $
    let msg = "Failed while attempting to modify registration of file " ++
              show fd ++ " at location " ++ loc
    in error msg

registerControlFd :: EventManager -> Fd -> Event -> IO ()
registerControlFd mgr fd evs =
  failOnInvalidFile "registerControlFd" fd $
  I.modifyFd (emBackend mgr) fd mempty evs

-- | Asynchronously shuts down the event manager, if running.
shutdown :: EventManager -> IO ()
shutdown mgr = do
  state <- atomicModifyIORef' (emState mgr) $ \s -> (Dying, s)
  when (state == Running) $ sendDie (emControl mgr)

-- | Asynchronously tell the thread executing the event
-- manager loop to exit.
release :: EventManager -> IO ()
release EventManager{..} = do
  state <- atomicModifyIORef' emState $ \s -> (Releasing, s)
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
  state <- atomicModifyIORef' emState $ \s -> case s of
    Created -> (Running, s)
    Releasing -> (Running, s)
    _       -> (s, s)
  case state of
    Created   -> go `onException` cleanup mgr
    Releasing -> go `onException` cleanup mgr
    Dying     -> cleanup mgr
    -- While a poll loop is never forked when the event manager is in the
    -- 'Finished' state, its state could read 'Finished' once the new thread
    -- actually runs.  This is not an error, just an unfortunate race condition
    -- in Thread.restartPollLoop.  See #8235
    Finished  -> return ()
    _         -> do cleanup mgr
                    error $ "GHC.Event.Manager.loop: state is already " ++
                            show state
 where
  go = do state <- step mgr
          case state of
            Running   -> yield >> go
            Releasing -> putMVar emLock ()
            _         -> cleanup mgr

-- | To make a step, we first do a non-blocking poll, in case
-- there are already events ready to handle. This improves performance
-- because we can make an unsafe foreign C call, thereby avoiding
-- forcing the current Task to release the Capability and forcing a context switch.
-- If the poll fails to find events, we yield, putting the poll loop thread at
-- end of the Haskell run queue. When it comes back around, we do one more
-- non-blocking poll, in case we get lucky and have ready events.
-- If that also returns no events, then we do a blocking poll.
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
registerFd_ :: EventManager -> IOCallback -> Fd -> Event -> Lifetime
            -> IO (FdKey, Bool)
registerFd_ mgr@(EventManager{..}) cb fd evs lt = do
  u <- newUnique emUniqueSource
  let fd'  = fromIntegral fd
      reg  = FdKey fd u
      el = I.eventLifetime evs lt
      !fdd = FdData reg el cb
  (modify,ok) <- withMVar (callbackTableVar mgr fd) $ \tbl -> do
    oldFdd <- IT.insertWith (++) fd' [fdd] tbl
    let prevEvs :: EventLifetime
        prevEvs = maybe mempty eventsOf oldFdd

        el' :: EventLifetime
        el' = prevEvs `mappend` el
    case I.elLifetime el' of
      -- All registrations want one-shot semantics and this is supported
      OneShot | haveOneShot -> do
        ok <- I.modifyFdOnce emBackend fd (I.elEvent el')
        if ok
          then return (False, True)
          else IT.reset fd' oldFdd tbl >> return (False, False)

      -- We don't want or don't support one-shot semantics
      _ -> do
        let modify = prevEvs /= el'
        ok <- if modify
              then let newEvs = I.elEvent el'
                       oldEvs = I.elEvent prevEvs
                   in I.modifyFd emBackend fd oldEvs newEvs
              else return True
        if ok
          then return (modify, True)
          else IT.reset fd' oldFdd tbl >> return (False, False)
  -- this simulates behavior of old IO manager:
  -- i.e. just call the callback if the registration fails.
  when (not ok) (cb reg evs)
  return (reg,modify)
{-# INLINE registerFd_ #-}

-- | @registerFd mgr cb fd evs lt@ registers interest in the events @evs@
-- on the file descriptor @fd@ for lifetime @lt@. @cb@ is called for
-- each event that occurs.  Returns a cookie that can be handed to
-- 'unregisterFd'.
registerFd :: EventManager -> IOCallback -> Fd -> Event -> Lifetime -> IO FdKey
registerFd mgr cb fd evs lt = do
  (r, wake) <- registerFd_ mgr cb fd evs lt
  when wake $ wakeManager mgr
  return r
{-# INLINE registerFd #-}

{-
    Building GHC with parallel IO manager on Mac freezes when
    compiling the dph libraries in the phase 2. As workaround, we
    don't use oneshot and we wake up an IO manager on Mac every time
    when we register an event.

    For more information, please read:
        http://ghc.haskell.org/trac/ghc/ticket/7651
-}
-- | Wake up the event manager.
wakeManager :: EventManager -> IO ()
#if defined(darwin_HOST_OS) || defined(ios_HOST_OS)
wakeManager mgr = sendWakeup (emControl mgr)
#elif defined(HAVE_EPOLL) || defined(HAVE_KQUEUE)
wakeManager _ = return ()
#else
wakeManager mgr = sendWakeup (emControl mgr)
#endif

eventsOf :: [FdData] -> EventLifetime
eventsOf [fdd] = fdEvents fdd
eventsOf fdds  = mconcat $ map fdEvents fdds

-- | Drop a previous file descriptor registration, without waking the
-- event manager thread.  The return value indicates whether the event
-- manager ought to be woken.
unregisterFd_ :: EventManager -> FdKey -> IO Bool
unregisterFd_ mgr@(EventManager{..}) (FdKey fd u) =
  withMVar (callbackTableVar mgr fd) $ \tbl -> do
    let dropReg = nullToNothing . filter ((/= u) . keyUnique . fdKey)
        fd' = fromIntegral fd
        pairEvents :: [FdData] -> IO (EventLifetime, EventLifetime)
        pairEvents prev = do
          r <- maybe mempty eventsOf `fmap` IT.lookup fd' tbl
          return (eventsOf prev, r)
    (oldEls, newEls) <- IT.updateWith dropReg fd' tbl >>=
                        maybe (return (mempty, mempty)) pairEvents
    let modify = oldEls /= newEls
    when modify $ failOnInvalidFile "unregisterFd_" fd $
      case I.elLifetime newEls of
        OneShot | I.elEvent newEls /= mempty, haveOneShot ->
          I.modifyFdOnce emBackend fd (I.elEvent newEls)
        _ ->
          I.modifyFd emBackend fd (I.elEvent oldEls) (I.elEvent newEls)
    return modify

-- | Drop a previous file descriptor registration.
unregisterFd :: EventManager -> FdKey -> IO ()
unregisterFd mgr reg = do
  wake <- unregisterFd_ mgr reg
  when wake $ wakeManager mgr

-- | Close a file descriptor in a race-safe way.
closeFd :: EventManager -> (Fd -> IO ()) -> Fd -> IO ()
closeFd mgr close fd = do
  fds <- withMVar (callbackTableVar mgr fd) $ \tbl -> do
    prev <- IT.delete (fromIntegral fd) tbl
    case prev of
      Nothing  -> close fd >> return []
      Just fds -> do
        let oldEls = eventsOf fds
        when (I.elEvent oldEls /= mempty) $ do
          _ <- I.modifyFd (emBackend mgr) fd (I.elEvent oldEls) mempty
          wakeManager mgr
        close fd
        return fds
  forM_ fds $ \(FdData reg el cb) -> cb reg (I.elEvent el `mappend` evtClose)

-- | Close a file descriptor in a race-safe way.
-- It assumes the caller will update the callback tables and that the caller
-- holds the callback table lock for the fd. It must hold this lock because
-- this command executes a backend command on the fd.
closeFd_ :: EventManager
            -> IntTable [FdData]
            -> Fd
            -> IO (IO ())
closeFd_ mgr tbl fd = do
  prev <- IT.delete (fromIntegral fd) tbl
  case prev of
    Nothing  -> return (return ())
    Just fds -> do
      let oldEls = eventsOf fds
      when (oldEls /= mempty) $ do
        _ <- I.modifyFd (emBackend mgr) fd (I.elEvent oldEls) mempty
        wakeManager mgr
      return $
        forM_ fds $ \(FdData reg el cb) ->
          cb reg (I.elEvent el `mappend` evtClose)

------------------------------------------------------------------------
-- Utilities

-- | Call the callbacks corresponding to the given file descriptor.
onFdEvent :: EventManager -> Fd -> Event -> IO ()
onFdEvent mgr fd evs
  | fd == controlReadFd (emControl mgr) || fd == wakeupReadFd (emControl mgr) =
    handleControlEvent mgr fd evs

  | otherwise = do
    fdds <- withMVar (callbackTableVar mgr fd) $ \tbl ->
        IT.delete (fromIntegral fd) tbl >>= maybe (return []) selectCallbacks
    forM_ fdds $ \(FdData reg _ cb) -> cb reg evs
  where
    -- | Here we look through the list of registrations for the fd of interest
    -- and sort out which match the events that were triggered. We re-arm
    -- the fd as appropriate and return this subset.
    selectCallbacks :: [FdData] -> IO [FdData]
    selectCallbacks fdds = do
        let matches :: FdData -> Bool
            matches fd' = evs `I.eventIs` I.elEvent (fdEvents fd')
            (triggered, saved) = partition matches fdds
            savedEls = eventsOf saved
            allEls = eventsOf fdds

        case I.elLifetime allEls of
          -- we previously armed the fd for multiple shots, no need to rearm
          MultiShot | allEls == savedEls ->
            return ()

          -- either we previously registered for one shot or the
          -- events of interest have changed, we must re-arm
          _ -> do
            case I.elLifetime savedEls of
              OneShot | haveOneShot ->
                -- if there are no saved events there is no need to re-arm
                unless (OneShot == I.elLifetime (eventsOf triggered)
                        && mempty == savedEls) $
                  void $ I.modifyFdOnce (emBackend mgr) fd (I.elEvent savedEls)
              _ ->
                void $ I.modifyFd (emBackend mgr) fd
                                  (I.elEvent allEls) (I.elEvent savedEls)
            return ()

        return triggered

nullToNothing :: [a] -> Maybe [a]
nullToNothing []       = Nothing
nullToNothing xs@(_:_) = Just xs

unless :: Monad m => Bool -> m () -> m ()
unless p = when (not p)

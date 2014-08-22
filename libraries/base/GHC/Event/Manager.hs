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

import Control.Concurrent.MVar (MVar, newMVar, readMVar, putMVar,
                                tryPutMVar, takeMVar, withMVar)
import Control.Exception (onException)
import Control.Monad ((=<<), forM_, liftM, when, replicateM, void)
import Data.Bits ((.&.))
import Data.IORef (IORef, atomicModifyIORef', mkWeakIORef, newIORef, readIORef,
                   writeIORef)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mappend, mconcat, mempty)
import GHC.Arr (Array, (!), listArray)
import GHC.Base
import GHC.Conc.Signal (runHandlers)
import GHC.Conc.Sync (yield)
import GHC.List (filter)
import GHC.Num (Num(..))
import GHC.Real (fromIntegral)
import GHC.Show (Show(..))
import GHC.Event.Control
import GHC.Event.IntTable (IntTable)
import GHC.Event.Internal (Backend, Event, evtClose, evtRead, evtWrite,
                           Timeout(..))
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
    , emFds          :: {-# UNPACK #-} !(Array Int (MVar (IntTable [FdData])))
    , emState        :: {-# UNPACK #-} !(IORef State)
    , emUniqueSource :: {-# UNPACK #-} !UniqueSource
    , emControl      :: {-# UNPACK #-} !Control
    , emOneShot      :: !Bool
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
                         , emOneShot = oneShot
                         , emLock = lockVar
                         }
  registerControlFd mgr (controlReadFd ctrl) evtRead
  registerControlFd mgr (wakeupReadFd ctrl) evtRead
  return mgr

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
registerFd_ :: EventManager -> IOCallback -> Fd -> Event
            -> IO (FdKey, Bool)
registerFd_ mgr@(EventManager{..}) cb fd evs = do
  u <- newUnique emUniqueSource
  let fd'  = fromIntegral fd
      reg  = FdKey fd u
      !fdd = FdData reg evs cb
  (modify,ok) <- withMVar (callbackTableVar mgr fd) $ \tbl ->
    if haveOneShot && emOneShot
    then do
      oldFdd <- IT.insertWith (++) fd' [fdd] tbl
      let evs' = maybe evs (combineEvents evs) oldFdd
      ok <- I.modifyFdOnce emBackend fd evs'
      if ok
        then return (False, True)
        else IT.reset fd' oldFdd tbl >> return (False, False)
    else do
      oldFdd <- IT.insertWith (++) fd' [fdd] tbl
      let (oldEvs, newEvs) =
            case oldFdd of
              Nothing   -> (mempty, evs)
              Just prev -> (eventsOf prev, combineEvents evs prev)
          modify = oldEvs /= newEvs
      ok <- if modify
            then I.modifyFd emBackend fd oldEvs newEvs
            else return True
      if ok
        then return (modify, True)
        else IT.reset fd' oldFdd tbl >> return (False, False)
  -- this simulates behavior of old IO manager:
  -- i.e. just call the callback if the registration fails.
  when (not ok) (cb reg evs)
  return (reg,modify)
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

eventsOf :: [FdData] -> Event
eventsOf = mconcat . map fdEvents

-- | Drop a previous file descriptor registration, without waking the
-- event manager thread.  The return value indicates whether the event
-- manager ought to be woken.
unregisterFd_ :: EventManager -> FdKey -> IO Bool
unregisterFd_ mgr@(EventManager{..}) (FdKey fd u) =
  withMVar (callbackTableVar mgr fd) $ \tbl -> do
    let dropReg = nullToNothing . filter ((/= u) . keyUnique . fdKey)
        fd' = fromIntegral fd
        pairEvents prev = do
          r <- maybe mempty eventsOf `fmap` IT.lookup fd' tbl
          return (eventsOf prev, r)
    (oldEvs, newEvs) <- IT.updateWith dropReg fd' tbl >>=
                        maybe (return (mempty, mempty)) pairEvents
    let modify = oldEvs /= newEvs
    when modify $ failOnInvalidFile "unregisterFd_" fd $
      if haveOneShot && emOneShot && newEvs /= mempty
      then I.modifyFdOnce emBackend fd newEvs
      else I.modifyFd emBackend fd oldEvs newEvs
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
        let oldEvs = eventsOf fds
        when (oldEvs /= mempty) $ do
          _ <- I.modifyFd (emBackend mgr) fd oldEvs mempty
          wakeManager mgr
        close fd
        return fds
  forM_ fds $ \(FdData reg ev cb) -> cb reg (ev `mappend` evtClose)

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
      let oldEvs = eventsOf fds
      when (oldEvs /= mempty) $ do
        _ <- I.modifyFd (emBackend mgr) fd oldEvs mempty
        wakeManager mgr
      return $
        forM_ fds $ \(FdData reg ev cb) -> cb reg (ev `mappend` evtClose)

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
      do fdds <- withMVar (callbackTableVar mgr fd) $ \tbl ->
           IT.delete fd' tbl >>=
           maybe (return []) (selectCallbacks tbl)
         forM_ fdds $ \(FdData reg _ cb) -> cb reg evs
    else
      do found <- IT.lookup fd' =<< readMVar (callbackTableVar mgr fd)
         case found of
           Just cbs -> forM_ cbs $ \(FdData reg ev cb) -> do
             when (evs `I.eventIs` ev) $ cb reg evs
           Nothing  -> return ()
  where
    fd' :: Int
    fd' = fromIntegral fd

    selectCallbacks :: IntTable [FdData] -> [FdData] -> IO [FdData]
    selectCallbacks tbl cbs = aux cbs [] []
      where
        -- nothing to rearm.
        aux [] _    []          =
          if haveOneShot
          then return cbs
          else do _ <- I.modifyFd (emBackend mgr) fd (eventsOf cbs) mempty
                  return cbs

        -- reinsert and rearm; note that we already have the lock on the
        -- callback table for this fd, and we deleted above, so we know there
        -- is no entry in the table for this fd.
        aux [] fdds saved@(_:_) = do
          _ <- if haveOneShot
               then I.modifyFdOnce (emBackend mgr) fd $ eventsOf saved
               else I.modifyFd (emBackend mgr) fd (eventsOf cbs) $ eventsOf saved
          _ <- IT.insertWith (\_ _ -> saved) fd' saved tbl
          return fdds

        -- continue, saving those callbacks that don't match the event
        aux (fdd@(FdData _ evs' _) : cbs') fdds saved
          | evs `I.eventIs` evs' = aux cbs' (fdd:fdds) saved
          | otherwise            = aux cbs' fdds (fdd:saved)

nullToNothing :: [a] -> Maybe [a]
nullToNothing []       = Nothing
nullToNothing xs@(_:_) = Just xs

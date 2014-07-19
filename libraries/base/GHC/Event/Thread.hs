{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns, NoImplicitPrelude #-}
module GHC.Event.Thread
    ( getSystemEventManager
    , getSystemTimerManager
    , ensureIOManagerIsRunning
    , ioManagerCapabilitiesChanged
    , threadWaitRead
    , threadWaitWrite
    , threadWaitReadSTM
    , threadWaitWriteSTM
    , closeFdWith
    , threadDelay
    , registerDelay
    , blockedOnBadFD -- used by RTS
    ) where

import Control.Exception (finally, SomeException, toException)
import Control.Monad (forM, forM_, sequence_, zipWithM, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (zipWith3)
import Data.Maybe (Maybe(..))
import Data.Tuple (snd)
import Foreign.C.Error (eBADF, errnoToIOError)
import Foreign.Ptr (Ptr)
import GHC.Base
import GHC.Conc.Sync (TVar, ThreadId, ThreadStatus(..), atomically, forkIO,
                      labelThread, modifyMVar_, withMVar, newTVar, sharedCAF,
                      getNumCapabilities, threadCapability, myThreadId, forkOn,
                      threadStatus, writeTVar, newTVarIO, readTVar, retry,throwSTM,STM)
import GHC.IO (mask_, onException)
import GHC.IO.Exception (ioError)
import GHC.IOArray (IOArray, newIOArray, readIOArray, writeIOArray,
                    boundsIOArray)
import GHC.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar)
import GHC.Event.Internal (eventIs, evtClose)
import GHC.Event.Manager (Event, EventManager, evtRead, evtWrite, loop,
                             new, registerFd, unregisterFd_)
import qualified GHC.Event.Manager as M
import qualified GHC.Event.TimerManager as TM
import GHC.Num ((-), (+))
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Types (Fd)

-- | Suspends the current thread for a given number of microseconds
-- (GHC only).
--
-- There is no guarantee that the thread will be rescheduled promptly
-- when the delay has expired, but the thread will never continue to
-- run /earlier/ than specified.
threadDelay :: Int -> IO ()
threadDelay usecs = mask_ $ do
  mgr <- getSystemTimerManager
  m <- newEmptyMVar
  reg <- TM.registerTimeout mgr usecs (putMVar m ())
  takeMVar m `onException` TM.unregisterTimeout mgr reg

-- | Set the value of returned TVar to True after a given number of
-- microseconds. The caveats associated with threadDelay also apply.
--
registerDelay :: Int -> IO (TVar Bool)
registerDelay usecs = do
  t <- atomically $ newTVar False
  mgr <- getSystemTimerManager
  _ <- TM.registerTimeout mgr usecs . atomically $ writeTVar t True
  return t

-- | Block the current thread until data is available to read from the
-- given file descriptor.
--
-- This will throw an 'IOError' if the file descriptor was closed
-- while this thread was blocked.  To safely close a file descriptor
-- that has been used with 'threadWaitRead', use 'closeFdWith'.
threadWaitRead :: Fd -> IO ()
threadWaitRead = threadWait evtRead
{-# INLINE threadWaitRead #-}

-- | Block the current thread until the given file descriptor can
-- accept data to write.
--
-- This will throw an 'IOError' if the file descriptor was closed
-- while this thread was blocked.  To safely close a file descriptor
-- that has been used with 'threadWaitWrite', use 'closeFdWith'.
threadWaitWrite :: Fd -> IO ()
threadWaitWrite = threadWait evtWrite
{-# INLINE threadWaitWrite #-}

-- | Close a file descriptor in a concurrency-safe way.
--
-- Any threads that are blocked on the file descriptor via
-- 'threadWaitRead' or 'threadWaitWrite' will be unblocked by having
-- IO exceptions thrown.
closeFdWith :: (Fd -> IO ())        -- ^ Action that performs the close.
            -> Fd                   -- ^ File descriptor to close.
            -> IO ()
closeFdWith close fd = do
  eventManagerArray <- readIORef eventManager
  let (low, high) = boundsIOArray eventManagerArray
  mgrs <- forM [low..high] $ \i -> do
    Just (_,!mgr) <- readIOArray eventManagerArray i
    return mgr
  mask_ $ do
    tables <- forM mgrs $ \mgr -> takeMVar $ M.callbackTableVar mgr fd
    cbApps <- zipWithM (\mgr table -> M.closeFd_ mgr table fd) mgrs tables
    close fd `finally` sequence_ (zipWith3 finish mgrs tables cbApps)
  where
    finish mgr table cbApp = putMVar (M.callbackTableVar mgr fd) table >> cbApp

threadWait :: Event -> Fd -> IO ()
threadWait evt fd = mask_ $ do
  m <- newEmptyMVar
  mgr <- getSystemEventManager_
  reg <- registerFd mgr (\_ e -> putMVar m e) fd evt
  evt' <- takeMVar m `onException` unregisterFd_ mgr reg
  if evt' `eventIs` evtClose
    then ioError $ errnoToIOError "threadWait" eBADF Nothing Nothing
    else return ()

-- used at least by RTS in 'select()' IO manager backend
blockedOnBadFD :: SomeException
blockedOnBadFD = toException $ errnoToIOError "awaitEvent" eBADF Nothing Nothing

threadWaitSTM :: Event -> Fd -> IO (STM (), IO ())
threadWaitSTM evt fd = mask_ $ do
  m <- newTVarIO Nothing
  mgr <- getSystemEventManager_
  reg <- registerFd mgr (\_ e -> atomically (writeTVar m (Just e))) fd evt
  let waitAction =
        do mevt <- readTVar m
           case mevt of
             Nothing -> retry
             Just evt' ->
               if evt' `eventIs` evtClose
               then throwSTM $ errnoToIOError "threadWaitSTM" eBADF Nothing Nothing
               else return ()
  return (waitAction, unregisterFd_ mgr reg >> return ())

-- | Allows a thread to use an STM action to wait for a file descriptor to be readable.
-- The STM action will retry until the file descriptor has data ready.
-- The second element of the return value pair is an IO action that can be used
-- to deregister interest in the file descriptor.
--
-- The STM action will throw an 'IOError' if the file descriptor was closed
-- while the STM action is being executed.  To safely close a file descriptor
-- that has been used with 'threadWaitReadSTM', use 'closeFdWith'.
threadWaitReadSTM :: Fd -> IO (STM (), IO ())
threadWaitReadSTM = threadWaitSTM evtRead
{-# INLINE threadWaitReadSTM #-}

-- | Allows a thread to use an STM action to wait until a file descriptor can accept a write.
-- The STM action will retry while the file until the given file descriptor can accept a write.
-- The second element of the return value pair is an IO action that can be used to deregister
-- interest in the file descriptor.
--
-- The STM action will throw an 'IOError' if the file descriptor was closed
-- while the STM action is being executed.  To safely close a file descriptor
-- that has been used with 'threadWaitWriteSTM', use 'closeFdWith'.
threadWaitWriteSTM :: Fd -> IO (STM (), IO ())
threadWaitWriteSTM = threadWaitSTM evtWrite
{-# INLINE threadWaitWriteSTM #-}


-- | Retrieve the system event manager for the capability on which the
-- calling thread is running.
--
-- This function always returns 'Just' the current thread's event manager
-- when using the threaded RTS and 'Nothing' otherwise.
getSystemEventManager :: IO (Maybe EventManager)
getSystemEventManager = do
  t <- myThreadId
  (cap, _) <- threadCapability t
  eventManagerArray <- readIORef eventManager
  mmgr <- readIOArray eventManagerArray cap
  return $ fmap snd mmgr

getSystemEventManager_ :: IO EventManager
getSystemEventManager_ = do
  Just mgr <- getSystemEventManager
  return mgr
{-# INLINE getSystemEventManager_ #-}

foreign import ccall unsafe "getOrSetSystemEventThreadEventManagerStore"
    getOrSetSystemEventThreadEventManagerStore :: Ptr a -> IO (Ptr a)

eventManager :: IORef (IOArray Int (Maybe (ThreadId, EventManager)))
eventManager = unsafePerformIO $ do
    numCaps <- getNumCapabilities
    eventManagerArray <- newIOArray (0, numCaps - 1) Nothing
    em <- newIORef eventManagerArray
    sharedCAF em getOrSetSystemEventThreadEventManagerStore
{-# NOINLINE eventManager #-}

numEnabledEventManagers :: IORef Int
numEnabledEventManagers = unsafePerformIO $ do
  newIORef 0
{-# NOINLINE numEnabledEventManagers #-}

foreign import ccall unsafe "getOrSetSystemEventThreadIOManagerThreadStore"
    getOrSetSystemEventThreadIOManagerThreadStore :: Ptr a -> IO (Ptr a)

-- | The ioManagerLock protects the 'eventManager' value:
-- Only one thread at a time can start or shutdown event managers.
{-# NOINLINE ioManagerLock #-}
ioManagerLock :: MVar ()
ioManagerLock = unsafePerformIO $ do
   m <- newMVar ()
   sharedCAF m getOrSetSystemEventThreadIOManagerThreadStore

getSystemTimerManager :: IO TM.TimerManager
getSystemTimerManager = do
  Just mgr <- readIORef timerManager
  return mgr

foreign import ccall unsafe "getOrSetSystemTimerThreadEventManagerStore"
    getOrSetSystemTimerThreadEventManagerStore :: Ptr a -> IO (Ptr a)

timerManager :: IORef (Maybe TM.TimerManager)
timerManager = unsafePerformIO $ do
    em <- newIORef Nothing
    sharedCAF em getOrSetSystemTimerThreadEventManagerStore
{-# NOINLINE timerManager #-}

foreign import ccall unsafe "getOrSetSystemTimerThreadIOManagerThreadStore"
    getOrSetSystemTimerThreadIOManagerThreadStore :: Ptr a -> IO (Ptr a)

{-# NOINLINE timerManagerThreadVar #-}
timerManagerThreadVar :: MVar (Maybe ThreadId)
timerManagerThreadVar = unsafePerformIO $ do
   m <- newMVar Nothing
   sharedCAF m getOrSetSystemTimerThreadIOManagerThreadStore

ensureIOManagerIsRunning :: IO ()
ensureIOManagerIsRunning
  | not threaded = return ()
  | otherwise = do
      startIOManagerThreads
      startTimerManagerThread

startIOManagerThreads :: IO ()
startIOManagerThreads =
  withMVar ioManagerLock $ \_ -> do
    eventManagerArray <- readIORef eventManager
    let (_, high) = boundsIOArray eventManagerArray
    forM_ [0..high] (startIOManagerThread eventManagerArray)
    writeIORef numEnabledEventManagers (high+1)

restartPollLoop :: EventManager -> Int -> IO ThreadId
restartPollLoop mgr i = do
  M.release mgr
  !t <- forkOn i $ loop mgr
  labelThread t "IOManager"
  return t

startIOManagerThread :: IOArray Int (Maybe (ThreadId, EventManager))
                        -> Int
                        -> IO ()
startIOManagerThread eventManagerArray i = do
  let create = do
        !mgr <- new True
        !t <- forkOn i $ loop mgr
        labelThread t "IOManager"
        writeIOArray eventManagerArray i (Just (t,mgr))
  old <- readIOArray eventManagerArray i
  case old of
    Nothing     -> create
    Just (t,em) -> do
      s <- threadStatus t
      case s of
        ThreadFinished -> create
        ThreadDied     -> do
          -- Sanity check: if the thread has died, there is a chance
          -- that event manager is still alive. This could happend during
          -- the fork, for example. In this case we should clean up
          -- open pipes and everything else related to the event manager.
          -- See #4449
          M.cleanup em
          create
        _other         -> return ()

startTimerManagerThread :: IO ()
startTimerManagerThread = modifyMVar_ timerManagerThreadVar $ \old -> do
  let create = do
        !mgr <- TM.new
        writeIORef timerManager $ Just mgr
        !t <- forkIO $ TM.loop mgr `finally` shutdownManagers
        labelThread t "TimerManager"
        return $ Just t
  case old of
    Nothing            -> create
    st@(Just t) -> do
      s <- threadStatus t
      case s of
        ThreadFinished -> create
        ThreadDied     -> do
          -- Sanity check: if the thread has died, there is a chance
          -- that event manager is still alive. This could happend during
          -- the fork, for example. In this case we should clean up
          -- open pipes and everything else related to the event manager.
          -- See #4449
          mem <- readIORef timerManager
          _ <- case mem of
                 Nothing -> return ()
                 Just em -> TM.cleanup em
          create
        _other         -> return st

shutdownManagers :: IO ()
shutdownManagers =
  withMVar ioManagerLock $ \_ -> do
    eventManagerArray <- readIORef eventManager
    let (_, high) = boundsIOArray eventManagerArray
    forM_ [0..high] $ \i -> do
      mmgr <- readIOArray eventManagerArray i
      case mmgr of
        Nothing -> return ()
        Just (_,mgr) -> M.shutdown mgr

foreign import ccall unsafe "rtsSupportsBoundThreads" threaded :: Bool

ioManagerCapabilitiesChanged :: IO ()
ioManagerCapabilitiesChanged = do
  withMVar ioManagerLock $ \_ -> do
    new_n_caps <- getNumCapabilities
    numEnabled <- readIORef numEnabledEventManagers
    writeIORef numEnabledEventManagers new_n_caps
    eventManagerArray <- readIORef eventManager
    let (_, high) = boundsIOArray eventManagerArray
    let old_n_caps = high + 1
    if new_n_caps > old_n_caps
      then do new_eventManagerArray <- newIOArray (0, new_n_caps - 1) Nothing

              -- copy the existing values into the new array:
              forM_ [0..high] $ \i -> do
                Just (tid,mgr) <- readIOArray eventManagerArray i
                if i < numEnabled
                  then writeIOArray new_eventManagerArray i (Just (tid,mgr))
                  else do tid' <- restartPollLoop mgr i
                          writeIOArray new_eventManagerArray i (Just (tid',mgr))

              -- create new IO managers for the new caps:
              forM_ [old_n_caps..new_n_caps-1] $
                startIOManagerThread new_eventManagerArray

              -- update the event manager array reference:
              writeIORef eventManager new_eventManagerArray
      else when (new_n_caps > numEnabled) $
            forM_ [numEnabled..new_n_caps-1] $ \i -> do
              Just (_,mgr) <- readIOArray eventManagerArray i
              tid <- restartPollLoop mgr i
              writeIOArray eventManagerArray i (Just (tid,mgr))

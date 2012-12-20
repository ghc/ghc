{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns, ForeignFunctionInterface, NoImplicitPrelude #-}

module GHC.Event.Thread
    ( getSystemEventManager
    , getSystemTimerManager
    , ensureIOManagerIsRunning
    , threadWaitRead
    , threadWaitWrite
    , threadWaitReadSTM
    , threadWaitWriteSTM
    , closeFdWith
    , threadDelay
    , registerDelay
    ) where

import Control.Exception (finally)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (Maybe(..))
import Foreign.C.Error (eBADF, errnoToIOError)
import Foreign.Ptr (Ptr)
import GHC.Base
import GHC.Conc.Sync (TVar, ThreadId, ThreadStatus(..), atomically, forkIO,
                      labelThread, modifyMVar_, newTVar, sharedCAF,
                      threadStatus, writeTVar, newTVarIO, readTVar, retry,throwSTM,STM)
import GHC.IO (mask_, onException)
import GHC.IO.Exception (ioError)
import GHC.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar)
import GHC.Event.Internal (eventIs, evtClose)
import GHC.Event.Manager (Event, EventManager, evtRead, evtWrite, loop,
                             new, registerFd, unregisterFd_, registerTimeout)
import qualified GHC.Event.Manager as M
import qualified GHC.Event.TimerManager as TM
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
  mgr <- getSystemEventManager
  M.closeFd mgr close fd

threadWait :: Event -> Fd -> IO ()
threadWait evt fd = mask_ $ do
  m <- newEmptyMVar
  mgr <- getSystemEventManager
  reg <- registerFd mgr (\reg e -> unregisterFd_ mgr reg >> putMVar m e) fd evt
  evt' <- takeMVar m `onException` unregisterFd_ mgr reg
  if evt' `eventIs` evtClose
    then ioError $ errnoToIOError "threadWait" eBADF Nothing Nothing
    else return ()


threadWaitSTM :: Event -> Fd -> IO (STM (), IO ())
threadWaitSTM evt fd = mask_ $ do
  m <- newTVarIO Nothing
  mgr <- getSystemEventManager 
  reg <- registerFd mgr (\reg e -> unregisterFd_ mgr reg >> atomically (writeTVar m (Just e))) fd evt
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


-- | Retrieve the system event manager.
--
-- This function always returns 'Just' the system event manager when using the
-- threaded RTS and 'Nothing' otherwise.
getSystemEventManager :: IO EventManager
getSystemEventManager = do 
  Just mgr <- readIORef eventManager
  return mgr

foreign import ccall unsafe "getOrSetSystemEventThreadEventManagerStore"
    getOrSetSystemEventThreadEventManagerStore :: Ptr a -> IO (Ptr a)

eventManager :: IORef (Maybe EventManager)
eventManager = unsafePerformIO $ do
    em <- newIORef Nothing
    sharedCAF em getOrSetSystemEventThreadEventManagerStore
{-# NOINLINE eventManager #-}

foreign import ccall unsafe "getOrSetSystemEventThreadIOManagerThreadStore"
    getOrSetSystemEventThreadIOManagerThreadStore :: Ptr a -> IO (Ptr a)

{-# NOINLINE ioManager #-}
ioManager :: MVar (Maybe ThreadId)
ioManager = unsafePerformIO $ do
   m <- newMVar Nothing
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
      startIOManagerThread
      startTimerManagerThread

startIOManagerThread :: IO ()
startIOManagerThread = modifyMVar_ ioManager $ \old -> do
  let create = do
        !mgr <- new
        writeIORef eventManager $ Just mgr
        !t <- forkIO $ loop mgr
        labelThread t "IOManager"
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
          mem <- readIORef eventManager
          _ <- case mem of
                 Nothing -> return ()
                 Just em -> M.cleanup em
          create
        _other         -> return st

startTimerManagerThread :: IO ()
startTimerManagerThread = modifyMVar_ timerManagerThreadVar $ \old -> do
  let shutdownEM = do
        mem <- readIORef eventManager
        case mem of
          Nothing -> return ()
          Just em -> M.shutdown em
  let create = do
        !mgr <- TM.new
        writeIORef timerManager $ Just mgr
        !t <- forkIO $ TM.loop mgr `finally` shutdownEM
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

foreign import ccall unsafe "rtsSupportsBoundThreads" threaded :: Bool

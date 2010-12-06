{-# LANGUAGE BangPatterns, ForeignFunctionInterface, NoImplicitPrelude #-}

module System.Event.Thread
    (
      ensureIOManagerIsRunning
    , threadWaitRead
    , threadWaitWrite
    , closeFdWith
    , threadDelay
    , registerDelay
    ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (Maybe(..))
import Foreign.C.Error (eBADF, errnoToIOError)
import Foreign.Ptr (Ptr)
import GHC.Base
import GHC.Conc.Sync (TVar, ThreadId, ThreadStatus(..), atomically, forkIO,
                      labelThread, modifyMVar_, newTVar, sharedCAF,
                      threadStatus, writeTVar)
import GHC.IO (mask_, onException)
import GHC.IO.Exception (ioError)
import GHC.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar)
import GHC.Real (fromIntegral)
import System.Event.Internal (eventIs, evtClose)
import System.Event.Manager (Event, EventManager, evtRead, evtWrite, loop,
                             new, registerFd, unregisterFd_, registerTimeout)
import qualified System.Event.Manager as M
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
  Just mgr <- readIORef eventManager
  m <- newEmptyMVar
  reg <- registerTimeout mgr usecs (putMVar m ())
  takeMVar m `onException` M.unregisterTimeout mgr reg

-- | Set the value of returned TVar to True after a given number of
-- microseconds. The caveats associated with threadDelay also apply.
--
registerDelay :: Int -> IO (TVar Bool)
registerDelay usecs = do
  t <- atomically $ newTVar False
  Just mgr <- readIORef eventManager
  _ <- registerTimeout mgr usecs . atomically $ writeTVar t True
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
  Just mgr <- readIORef eventManager
  M.closeFd mgr close fd

threadWait :: Event -> Fd -> IO ()
threadWait evt fd = mask_ $ do
  m <- newEmptyMVar
  Just mgr <- readIORef eventManager
  reg <- registerFd mgr (\reg e -> unregisterFd_ mgr reg >> putMVar m e) fd evt
  evt' <- takeMVar m `onException` unregisterFd_ mgr reg
  if evt' `eventIs` evtClose
    then ioError $ errnoToIOError "threadWait" eBADF Nothing Nothing
    else return ()

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

ensureIOManagerIsRunning :: IO ()
ensureIOManagerIsRunning
  | not threaded = return ()
  | otherwise = modifyMVar_ ioManager $ \old -> do
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
        ThreadDied     -> create
        _other         -> return st

foreign import ccall unsafe "rtsSupportsBoundThreads" threaded :: Bool

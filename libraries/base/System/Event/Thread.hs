{-# LANGUAGE BangPatterns, ForeignFunctionInterface, NoImplicitPrelude #-}

module System.Event.Thread
    (
      ensureIOManagerIsRunning
    , threadWaitRead
    , threadWaitWrite
    , threadDelay
    , registerDelay
    ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (Maybe(..))
import Foreign.Ptr (Ptr)
import GHC.Base
import GHC.Conc.Sync (TVar, ThreadId, ThreadStatus(..), atomically, forkIO,
                      labelThread, modifyMVar_, newTVar, sharedCAF,
                      threadStatus, writeTVar)
import GHC.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar)
import GHC.Num (fromInteger)
import GHC.Real (div)
import System.Event.Manager (Event, EventManager, evtRead, evtWrite, loop,
                             new, registerFd, unregisterFd_, registerTimeout)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Types (Fd)

-- | Suspends the current thread for a given number of microseconds
-- (GHC only).
--
-- There is no guarantee that the thread will be rescheduled promptly
-- when the delay has expired, but the thread will never continue to
-- run /earlier/ than specified.
threadDelay :: Int -> IO ()
threadDelay usecs = do
  Just mgr <- readIORef eventManager
  m <- newEmptyMVar
  _ <- registerTimeout mgr (usecs `div` 1000) (putMVar m ())
  takeMVar m

-- | Set the value of returned TVar to True after a given number of
-- microseconds. The caveats associated with threadDelay also apply.
--
registerDelay :: Int -> IO (TVar Bool)
registerDelay usecs = do
  t <- atomically $ newTVar False
  Just mgr <- readIORef eventManager
  _ <- registerTimeout mgr (usecs `div` 1000) . atomically $ writeTVar t True
  return t

-- | Block the current thread until data is available to read from the
-- given file descriptor.
threadWaitRead :: Fd -> IO ()
threadWaitRead = threadWait evtRead
{-# INLINE threadWaitRead #-}

-- | Block the current thread until the given file descriptor can
-- accept data to write.
threadWaitWrite :: Fd -> IO ()
threadWaitWrite = threadWait evtWrite
{-# INLINE threadWaitWrite #-}

threadWait :: Event -> Fd -> IO ()
threadWait evt fd = do
  m <- newEmptyMVar
  Just mgr <- readIORef eventManager
  _ <- registerFd mgr (\reg _ -> unregisterFd_ mgr reg >> putMVar m ()) fd evt
  takeMVar m

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

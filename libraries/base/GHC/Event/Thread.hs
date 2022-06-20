{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns, NoImplicitPrelude #-}

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
    , blockedOnBadFD -- used by RTS
    ) where
-- TODO: Use new Windows I/O manager
import Control.Exception (finally, SomeException, toException)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Foreign.C.Error (eBADF, errnoToIOError)
import Foreign.C.Types (CInt(..), CUInt(..))
import Foreign.Ptr (Ptr)
import GHC.Base
import GHC.Conc.Sync (TVar, ThreadId, ThreadStatus(..), atomically, forkIO,
                      labelThread, modifyMVar_, newTVar, sharedCAF, forkOn,
                      threadStatus, writeTVar, newTVarIO, readTVar, retry,throwSTM,STM,
                      numCapabilitiesLock)
import GHC.IO (mask_, uninterruptibleMask_, onException)
import GHC.IO.Exception (ioError)
import GHC.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar)
import GHC.Event.Control (controlWriteFd)
import GHC.Event.Internal (eventIs, evtClose)
import GHC.Event.Manager (Event, EventManager, evtRead, evtWrite, loop,
                             new, registerFd, unregisterFd_)
import qualified GHC.Event.Manager as M
import qualified GHC.Event.TimerManager as TM
import GHC.PerCapability
import GHC.Real (fromIntegral)
import GHC.Show (showSignedInt)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Types (Fd)
import GHC.RWLock

-- | Suspends the current thread for a given number of microseconds
-- (GHC only).
--
-- There is no guarantee that the thread will be rescheduled promptly
-- when the delay has expired, but the thread will never continue to
-- run /earlier/ than specified.
--
-- Be careful not to exceed @maxBound :: Int@, which on 32-bit machines is only
-- 2147483647 μs, less than 36 minutes.
--
threadDelay :: Int -> IO ()
threadDelay usecs = mask_ $ do
  mgr <- getSystemTimerManager
  m <- newEmptyMVar
  reg <- TM.registerTimeout mgr usecs (putMVar m ())
  takeMVar m `onException` TM.unregisterTimeout mgr reg

-- | Set the value of returned TVar to True after a given number of
-- microseconds. The caveats associated with threadDelay also apply.
--
-- Be careful not to exceed @maxBound :: Int@, which on 32-bit machines is only
-- 2147483647 μs, less than 36 minutes.
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
-- This will throw an 'Prelude.IOError' if the file descriptor was closed
-- while this thread was blocked.  To safely close a file descriptor
-- that has been used with 'threadWaitRead', use 'closeFdWith'.
threadWaitRead :: Fd -> IO ()
threadWaitRead = threadWait evtRead
{-# INLINE threadWaitRead #-}

-- | Block the current thread until the given file descriptor can
-- accept data to write.
--
-- This will throw an 'Prelude.IOError' if the file descriptor was closed
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
closeFdWith close fd = withReader numCapabilitiesLock $ \() -> do
  -- 'takeMVar', and 'M.closeFd_' might block, although for a very short time.
  -- To make 'closeFdWith' safe in presence of asynchronous exceptions we have
  -- to use uninterruptible mask.
  uninterruptibleMask_ $ do
    tables <- forEachCapability eventManager $ \_cap (_, mgr) ->
        takeMVar $ M.callbackTableVar mgr fd
    cbApps <- forEachCapability eventManager $ \cap (_, mgr) -> do
      case lookupPerCapMap tables cap of
        Just table -> M.closeFd_ mgr table fd
        Nothing  -> error "closeFdWith: this shouldn't happen"
          -- Is this true? Couldn't the user concurrently increase the
          -- capability count?
    close fd `finally` finish tables cbApps
  where
    finish tables cbApps = forEachCapability eventManager $ \cap (_, mgr) -> do
      case lookupPerCapMap tables cap of
        Just table -> putMVar (M.callbackTableVar mgr fd) table
        Nothing -> return ()
      case lookupPerCapMap cbApps cap of
        Just cbApp -> cbApp
        Nothing -> return ()

threadWait :: Event -> Fd -> IO ()
threadWait evt fd = mask_ $ do
  m <- newEmptyMVar
  mgr <- getSystemEventManager_
  reg <- registerFd mgr (\_ e -> putMVar m e) fd evt M.OneShot
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
  reg <- registerFd mgr (\_ e -> atomically (writeTVar m (Just e))) fd evt M.OneShot
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
-- The STM action will throw an 'Prelude.IOError' if the file descriptor was closed
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
-- The STM action will throw an 'Prelude.IOError' if the file descriptor was closed
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
getSystemEventManager :: IO (ThreadId, EventManager)
getSystemEventManager = do
  getPerCapability eventManager

getSystemEventManager_ :: IO EventManager
getSystemEventManager_ = do
  (_, mgr) <- getSystemEventManager
  return mgr
{-# INLINE getSystemEventManager_ #-}

foreign import ccall unsafe "getOrSetSystemEventThreadEventManagerStore"
    getOrSetSystemEventThreadEventManagerStore :: Ptr a -> IO (Ptr a)

eventManager :: PerCapability (ThreadId, EventManager)
eventManager = unsafePerformIO $ do
    let new_cap i = do
          !mgr <- new
          !t <- forkOn i $ do
                  c_setIOManagerControlFd
                    (fromIntegral i)
                    (fromIntegral $ controlWriteFd $ M.emControl mgr)
                  loop mgr
          labelThread t ("IOManager on cap " ++ show_int i)
          return (t, mgr)
        free_cap (_t, mgr) = M.cleanup mgr
    pc <- newPerCapability new_cap free_cap
    sharedCAF pc getOrSetSystemEventThreadEventManagerStore
{-# NOINLINE eventManager #-}

getSystemTimerManager :: IO TM.TimerManager
getSystemTimerManager =
  fromMaybe err `fmap` readIORef timerManager
    where
      err = error "GHC.Event.Thread.getSystemTimerManager: the TimerManager requires linking against the threaded runtime"

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
  | otherwise = startTimerManagerThread

show_int :: Int -> String
show_int i = showSignedInt 0 i ""

startTimerManagerThread :: IO ()
startTimerManagerThread = modifyMVar_ timerManagerThreadVar $ \old -> do
  let create = do
        !mgr <- TM.new
        c_setTimerManagerControlFd
          (fromIntegral $ controlWriteFd $ TM.emControl mgr)
        writeIORef timerManager $ Just mgr
        !t <- forkIO $ TM.loop mgr
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
          -- that event manager is still alive. This could happened during
          -- the fork, for example. In this case we should clean up
          -- open pipes and everything else related to the event manager.
          -- See #4449
          mem <- readIORef timerManager
          _ <- case mem of
                 Nothing -> return ()
                 Just em -> do c_setTimerManagerControlFd (-1)
                               TM.cleanup em
          create
        _other         -> return st

foreign import ccall unsafe "rtsSupportsBoundThreads" threaded :: Bool

-- Used to tell the RTS how it can send messages to the I/O manager.
foreign import ccall unsafe "setIOManagerControlFd"
   c_setIOManagerControlFd :: CUInt -> CInt -> IO ()

foreign import ccall unsafe "setTimerManagerControlFd"
   c_setTimerManagerControlFd :: CInt -> IO ()

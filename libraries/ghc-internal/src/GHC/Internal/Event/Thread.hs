{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns, NoImplicitPrelude #-}

#include <ghcplatform.h>

module GHC.Internal.Event.Thread
#if defined(javascript_HOST_ARCH)
    ( ) where
#else
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


-- TODO: Use new Windows I/O manager
import GHC.Internal.Control.Exception (finally, SomeException, toException)
import GHC.Internal.Data.Foldable (forM_, mapM_, sequence_)
import GHC.Internal.Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicWriteIORef)
import GHC.Internal.Data.Maybe (fromMaybe)
import GHC.Internal.Data.Tuple (snd)
import GHC.Internal.Foreign.C.Error (eBADF, errnoToIOError)
import GHC.Internal.Foreign.C.Types (CInt(..), CUInt(..))
import GHC.Internal.Foreign.Ptr (Ptr)
import GHC.Internal.Base
import GHC.Internal.List (zipWith, zipWith3)
import GHC.Internal.Conc.Sync (TVar, ThreadId, ThreadStatus(..), atomically, forkIO,
                      labelThread, modifyMVar_, withMVar, newTVar, sharedCAF,
                      getNumCapabilities, threadCapability, myThreadId, forkOn,
                      threadStatus, writeTVar, newTVarIO, readTVar, retry,
                      throwSTM, STM, yield)
import GHC.Internal.IO (mask_, uninterruptibleMask_, onException)
import GHC.Internal.IO.Exception (ioError)
import GHC.Internal.IOArray (IOArray, newIOArray, readIOArray, writeIOArray,
                    boundsIOArray)
import GHC.Internal.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar)
import GHC.Internal.Event.Control (controlWriteFd)
import GHC.Internal.Event.Internal (eventIs, evtClose)
import GHC.Internal.Event.Manager (Event, EventManager, evtRead, evtWrite, loop,
                             new, registerFd, unregisterFd_)
import qualified GHC.Internal.Event.Manager as M
import qualified GHC.Internal.Event.TimerManager as TM
import GHC.Internal.Ix (inRange)
import GHC.Internal.Num ((-), (+))
import GHC.Internal.Real (fromIntegral)
import GHC.Internal.Show (showSignedInt)
import GHC.Internal.IO.Unsafe (unsafePerformIO)
import GHC.Internal.System.Posix.Types (Fd)

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
--
-- Closing file descriptors on one thread while they are still being
-- used by other threads is always prone to race conditions (since e.g.
-- on Linux file descriptors can be immediately reused after closing).
--
-- It is recommended to only call @'closeFdWith'@ when the file descriptor
-- can no longer be used by other threads.
closeFdWith :: (Fd -> IO ())        -- ^ Action that performs the close.
            -> Fd                   -- ^ File descriptor to close.
            -> IO ()
closeFdWith close fd = close_loop
  where
    finish mgr table cbApp = putMVar (M.callbackTableVar mgr fd) table >> cbApp
    zipWithM f xs ys = sequence (zipWith f xs ys)
      -- The array inside 'eventManager' can be swapped out at any time, see
      -- 'ioManagerCapabilitiesChanged'. See #21651. We detect this case by
      -- checking the array bounds before and after. When such a swap has
      -- happened we cleanup and try again
    close_loop = do
      eventManagerArray <- readIORef eventManager
      let ema_bounds@(low, high) = boundsIOArray eventManagerArray
      mgrs <- flip mapM [low..high] $ \i -> do
        Just (_,!mgr) <- readIOArray eventManagerArray i
        return mgr

      -- 'takeMVar', and 'M.closeFd_' might block, although for a very short time.
      -- To make 'closeFdWith' safe in presence of asynchronous exceptions we have
      -- to use uninterruptible mask.
      join $ uninterruptibleMask_ $ do
        tables <- flip mapM mgrs $ \mgr -> takeMVar $ M.callbackTableVar mgr fd
        new_ema_bounds <- boundsIOArray `fmap` readIORef eventManager
        -- Here we exploit Note [The eventManager Array]
        if new_ema_bounds /= ema_bounds
          then do
            -- the array has been modified.
            -- mgrs still holds the right EventManagers, by the Note.
            -- new_ema_bounds must be larger than ema_bounds, by the note.
            -- return the MVars we took and try again
            sequence_ $ zipWith (\mgr table -> finish mgr table (pure ())) mgrs tables
            pure close_loop
          else do
            -- We surely have taken all the appropriate MVars. Even if the array
            -- has been swapped, our mgrs is still correct.
            -- Remove the Fd from all callback tables, close the Fd, and run all
            -- callbacks.
            cbApps <- zipWithM (\mgr table -> M.closeFd_ mgr table fd) mgrs tables
            close fd `finally` sequence_ (zipWith3 finish mgrs tables cbApps)
            pure (pure ())

-- | Wait for an event on a file descriptor.
--
-- The given @'Event'@ may only be (a combination of) @'evtRead'@ or
-- @'evtWrite'@, but not @'evtClose'@. See @'evtClose'@ for more details.
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
getSystemEventManager :: IO (Maybe EventManager)
getSystemEventManager = do
  t <- myThreadId
  eventManagerArray <- readIORef eventManager
  let r = boundsIOArray eventManagerArray
  (cap, _) <- threadCapability t
  -- It is possible that we've just increased the number of capabilities and the
  -- new EventManager has not yet been constructed by
  -- 'ioManagerCapabilitiesChanged'. We expect this to happen very rarely.
  -- T21561 exercises this.
  -- Two options to proceed:
  --  1) return the EventManager for capability 0. This is guaranteed to exist,
  --     and "shouldn't" cause any correctness issues.
  --  2) Busy wait, with or without a call to 'yield'. This can't deadlock,
  --     because we must be on a brand capability and there must be a call to
  --     'ioManagerCapabilitiesChanged' pending.
  --
  -- We take the second option, with the yield, judging it the most robust.
  if not (inRange r cap)
    then yield >> getSystemEventManager
    else fmap snd `fmap` readIOArray eventManagerArray cap

getSystemEventManager_ :: IO EventManager
getSystemEventManager_ = do
  Just mgr <- getSystemEventManager
  return mgr
{-# INLINE getSystemEventManager_ #-}

foreign import ccall unsafe "getOrSetSystemEventThreadEventManagerStore"
    getOrSetSystemEventThreadEventManagerStore :: Ptr a -> IO (Ptr a)

-- Note [The eventManager Array]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A mutable array holding the current EventManager for each capability
-- An entry is Nothing only while the eventmanagers are initialised, see
-- 'startIOManagerThread' and 'ioManagerCapabilitiesChanged'.
-- The 'ThreadId' at array position 'cap'  will have been 'forkOn'ed capability
-- 'cap'.
-- The array will be swapped with newer arrays when the number of capabilities
-- changes(via 'setNumCapabilities'). However:
--   * the size of the arrays will never decrease; and
--   * The 'EventManager's in the array are not replaced with other
--     'EventManager' constructors.
--
-- This is a similar strategy as the rts uses for it's
-- capabilities array (n_capabilities is the size of the array,
-- enabled_capabilities' is the number of active capabilities).
eventManager :: IORef (IOArray Int (Maybe (ThreadId, EventManager)))
eventManager = unsafePerformIO $ do
    numCaps <- getNumCapabilities
    eventManagerArray <- newIOArray (0, numCaps - 1) Nothing
    em <- newIORef eventManagerArray
    sharedCAF em getOrSetSystemEventThreadEventManagerStore
{-# NOINLINE eventManager #-}

numEnabledEventManagers :: IORef Int
numEnabledEventManagers = unsafePerformIO $ newIORef 0
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
getSystemTimerManager =
  fromMaybe err `fmap` readIORef timerManager
    where
      err = error "GHC.Internal.Event.Thread.getSystemTimerManager: the TimerManager requires linking against the threaded runtime"

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
    mapM_ (startIOManagerThread eventManagerArray) [0..high]
    writeIORef numEnabledEventManagers (high+1)

show_int :: Int -> String
show_int i = showSignedInt 0 i ""

restartPollLoop :: EventManager -> Int -> IO ThreadId
restartPollLoop mgr i = do
  M.release mgr
  !t <- forkOn i $ loop mgr
  labelThread t ("IOManager on cap " ++ show_int i)
  return t

startIOManagerThread :: IOArray Int (Maybe (ThreadId, EventManager))
                        -> Int
                        -> IO ()
startIOManagerThread eventManagerArray i = do
  let create = do
        !mgr <- new
        !t <- forkOn i $ do
                c_setIOManagerControlFd
                  (fromIntegral i)
                  (fromIntegral $ controlWriteFd $ M.emControl mgr)
                loop mgr
        labelThread t ("IOManager on cap " ++ show_int i)
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
          -- that event manager is still alive. This could happened during
          -- the fork, for example. In this case we should clean up
          -- open pipes and everything else related to the event manager.
          -- See #4449
          c_setIOManagerControlFd (fromIntegral i) (-1)
          M.cleanup em
          create
        _other         -> return ()

startTimerManagerThread :: IO ()
startTimerManagerThread = modifyMVar_ timerManagerThreadVar $ \old -> do
  let create = do
        !mgr <- TM.new
        c_setTimerManagerControlFd
          (fromIntegral $ controlWriteFd $ TM.emControl mgr)
        atomicWriteIORef timerManager $ Just mgr
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

ioManagerCapabilitiesChanged :: IO ()
ioManagerCapabilitiesChanged =
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
              atomicWriteIORef eventManager new_eventManagerArray
              -- We need an atomic write here because 'eventManager' is accessed
              -- unsynchronized in 'getSystemEventManager' and 'closeFdWith'
      else when (new_n_caps > numEnabled) $
            forM_ [numEnabled..new_n_caps-1] $ \i -> do
              Just (_,mgr) <- readIOArray eventManagerArray i
              tid <- restartPollLoop mgr i
              writeIOArray eventManagerArray i (Just (tid,mgr))

#if defined(wasm32_HOST_ARCH)
c_setIOManagerControlFd :: CUInt -> CInt -> IO ()
c_setIOManagerControlFd _ _ = pure ()

c_setTimerManagerControlFd :: CInt -> IO ()
c_setTimerManagerControlFd _ = pure ()
#else
-- Used to tell the RTS how it can send messages to the I/O manager.
foreign import ccall unsafe "setIOManagerControlFd"
   c_setIOManagerControlFd :: CUInt -> CInt -> IO ()

foreign import ccall unsafe "setTimerManagerControlFd"
   c_setTimerManagerControlFd :: CInt -> IO ()
#endif

#endif

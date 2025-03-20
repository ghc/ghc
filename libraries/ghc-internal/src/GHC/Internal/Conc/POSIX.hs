{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, MagicHash, UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Conc.POSIX
-- Copyright   :  (c) The University of Glasgow, 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Windows I/O manager
--
-- This is the I/O manager based on posix FDs for windows.
-- When using the winio manager these functions may not
-- be used as they will behave in unexpected ways.
--
-- TODO: This manager is currently the default. But we will eventually
-- switch to use winio instead.
--
-----------------------------------------------------------------------------

-- #not-home
module GHC.Internal.Conc.POSIX
       ( ensureIOManagerIsRunning
       , interruptIOManager

       -- * Waiting
       , threadDelay
       , registerDelay

       -- * Miscellaneous
       , asyncRead
       , asyncWrite
       , asyncDoProc

       , asyncReadBA
       , asyncWriteBA

       , module GHC.Internal.Event.Windows.ConsoleEvent
       ) where

import GHC.Internal.Data.Bits (shiftR)
import GHC.Internal.Base
import GHC.Internal.Clock
import GHC.Internal.Conc.Sync
import GHC.Internal.Conc.POSIX.Const
import GHC.Internal.Event.Windows.ConsoleEvent
import GHC.Internal.IO (unsafePerformIO)
import GHC.Internal.IORef
import GHC.Internal.MVar
import GHC.Internal.Num (Num(..))
import GHC.Internal.Ptr
import GHC.Internal.Real (div, fromIntegral)
import GHC.Internal.STM (TVar, atomically, newTVar, writeTVar)
import GHC.Internal.Word (Word32, Word64)
import GHC.Internal.Windows

-- ----------------------------------------------------------------------------
-- Thread waiting

-- Note: threadWaitRead and threadWaitWrite aren't really functional
-- on Win32, but left in there because lib code (still) uses them (the manner
-- in which they're used doesn't cause problems on a Win32 platform though.)

asyncRead :: Int -> Int -> Int -> Ptr a -> IO (Int, Int)
asyncRead  (I# fd) (I# isSock) (I# len) (Ptr buf) =
  IO $ \s -> case asyncRead# fd isSock len buf s of
               (# s', len#, err# #) -> (# s', (I# len#, I# err#) #)

asyncWrite :: Int -> Int -> Int -> Ptr a -> IO (Int, Int)
asyncWrite  (I# fd) (I# isSock) (I# len) (Ptr buf) =
  IO $ \s -> case asyncWrite# fd isSock len buf s of
               (# s', len#, err# #) -> (# s', (I# len#, I# err#) #)

asyncDoProc :: FunPtr (Ptr a -> IO Int) -> Ptr a -> IO Int
asyncDoProc (FunPtr proc) (Ptr param) =
    -- the 'length' value is ignored; simplifies implementation of
    -- the async*# primops to have them all return the same result.
  IO $ \s -> case asyncDoProc# proc param s  of
               (# s', _len#, err# #) -> (# s', I# err# #)

-- to aid the use of these primops by the IO Handle implementation,
-- provide the following convenience funs:

-- this better be a pinned byte array!
asyncReadBA :: Int -> Int -> Int -> Int -> MutableByteArray# RealWorld -> IO (Int,Int)
asyncReadBA fd isSock len off bufB =
  asyncRead fd isSock len ((Ptr (mutableByteArrayContents# bufB)) `plusPtr` off)

asyncWriteBA :: Int -> Int -> Int -> Int -> MutableByteArray# RealWorld -> IO (Int,Int)
asyncWriteBA fd isSock len off bufB =
  asyncWrite fd isSock len ((Ptr (mutableByteArrayContents# bufB)) `plusPtr` off)

-- ----------------------------------------------------------------------------
-- Threaded RTS implementation of threadDelay

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
threadDelay time
  | threaded  = waitForDelayEvent time
  | otherwise = IO $ \s ->
        case time of { I# time# ->
        case delay# time# s of { s' -> (# s', () #)
        }}

-- | Set the value of returned TVar to True after a given number of
-- microseconds. The caveats associated with threadDelay also apply.
--
-- Be careful not to exceed @maxBound :: Int@, which on 32-bit machines is only
-- 2147483647 μs, less than 36 minutes.
--
registerDelay :: Int -> IO (TVar Bool)
registerDelay usecs
  | threaded = waitForDelayEventSTM usecs
  | otherwise = errorWithoutStackTrace "registerDelay: requires -threaded"

foreign import ccall unsafe "rtsSupportsBoundThreads" threaded :: Bool

waitForDelayEvent :: Int -> IO ()
waitForDelayEvent usecs = do
  m <- newEmptyMVar
  target <- calculateTarget usecs
  _ <- atomicModifyIORef'_ pendingDelays (\xs -> Delay target m : xs)
  prodServiceThread
  takeMVar m

-- Delays for use in STM
waitForDelayEventSTM :: Int -> IO (TVar Bool)
waitForDelayEventSTM usecs = do
   t <- atomically $ newTVar False
   target <- calculateTarget usecs
   _ <- atomicModifyIORef'_ pendingDelays (\xs -> DelaySTM target t : xs)
   prodServiceThread
   return t

calculateTarget :: Int -> IO USecs
calculateTarget usecs = do
    now <- getMonotonicUSec
    return $ now + (fromIntegral usecs)

data DelayReq
  = Delay    {-# UNPACK #-} !USecs {-# UNPACK #-} !(MVar ())
  | DelaySTM {-# UNPACK #-} !USecs {-# UNPACK #-} !(TVar Bool)

{-# NOINLINE pendingDelays #-}
pendingDelays :: IORef [DelayReq]
pendingDelays = unsafePerformIO $ do
   m <- newIORef []
   sharedCAF m getOrSetGHCConcWindowsPendingDelaysStore

foreign import ccall unsafe "getOrSetGHCConcWindowsPendingDelaysStore"
    getOrSetGHCConcWindowsPendingDelaysStore :: Ptr a -> IO (Ptr a)

{-# NOINLINE ioManagerThread #-}
ioManagerThread :: MVar (Maybe ThreadId)
ioManagerThread = unsafePerformIO $ do
   m <- newMVar Nothing
   sharedCAF m getOrSetGHCConcWindowsIOManagerThreadStore

foreign import ccall unsafe "getOrSetGHCConcWindowsIOManagerThreadStore"
    getOrSetGHCConcWindowsIOManagerThreadStore :: Ptr a -> IO (Ptr a)

ensureIOManagerIsRunning :: IO ()
ensureIOManagerIsRunning
  | threaded  = startIOManagerThread
  | otherwise = return ()

interruptIOManager :: IO ()
interruptIOManager = return ()

startIOManagerThread :: IO ()
startIOManagerThread =
  modifyMVar_ ioManagerThread $ \old -> do
    let create = do t <- forkIO ioManager;
                    labelThread t "IOManagerThread";
                    return (Just t)
    case old of
      Nothing -> create
      Just t  -> do
        s <- threadStatus t
        case s of
          ThreadFinished -> create
          ThreadDied     -> create
          _other         -> return (Just t)

insertDelay :: DelayReq -> [DelayReq] -> [DelayReq]
insertDelay d [] = [d]
insertDelay d1 ds@(d2 : rest)
  | delayTime d1 <= delayTime d2 = d1 : ds
  | otherwise                    = d2 : insertDelay d1 rest

delayTime :: DelayReq -> USecs
delayTime (Delay t _) = t
delayTime (DelaySTM t _) = t

type USecs = Word64

getMonotonicUSec :: IO USecs
getMonotonicUSec = fmap (`div` 1000) getMonotonicTimeNSec

{-# NOINLINE prodding #-}
prodding :: IORef Bool
prodding = unsafePerformIO $ do
   r <- newIORef False
   sharedCAF r getOrSetGHCConcWindowsProddingStore

foreign import ccall unsafe "getOrSetGHCConcWindowsProddingStore"
    getOrSetGHCConcWindowsProddingStore :: Ptr a -> IO (Ptr a)

prodServiceThread :: IO ()
prodServiceThread = do
  -- NB. use atomicSwapIORef here, otherwise there are race
  -- conditions in which prodding is left at True but the server is
  -- blocked in select().
  was_set <- atomicSwapIORef prodding True
  when (not was_set) wakeupIOManager

-- ----------------------------------------------------------------------------
-- Windows IO manager thread

ioManager :: IO ()
ioManager = do
  wakeup <- c_getIOManagerEvent
  service_loop wakeup []

service_loop :: HANDLE          -- read end of pipe
             -> [DelayReq]      -- current delay requests
             -> IO ()

service_loop wakeup old_delays = do
  -- pick up new delay requests
  new_delays <- atomicSwapIORef pendingDelays []
  let  delays = foldr insertDelay old_delays new_delays

  now <- getMonotonicUSec
  (delays', timeout) <- getDelay now delays

  r <- c_WaitForSingleObject wakeup timeout
  case r of
    0xffffffff -> throwGetLastError "service_loop"
    0 -> do
        r2 <- c_readIOManagerEvent
        exit <-
              case r2 of
                _ | r2 == io_MANAGER_WAKEUP -> return False
                _ | r2 == io_MANAGER_DIE    -> return True
                0 -> return False -- spurious wakeup
                _ -> do start_console_handler (r2 `shiftR` 1); return False
        when (not exit) $ service_cont wakeup delays'

    _other -> service_cont wakeup delays' -- probably timeout

service_cont :: HANDLE -> [DelayReq] -> IO ()
service_cont wakeup delays = do
  _ <- atomicSwapIORef prodding False
  service_loop wakeup delays

wakeupIOManager :: IO ()
wakeupIOManager = c_sendIOManagerEvent io_MANAGER_WAKEUP

-- Walk the queue of pending delays, waking up any that have passed
-- and return the smallest delay to wait for.  The queue of pending
-- delays is kept ordered.
getDelay :: USecs -> [DelayReq] -> IO ([DelayReq], DWORD)
getDelay _   [] = return ([], iNFINITE)
getDelay now all@(d : rest)
  = case d of
     Delay time m | now >= time -> do
        putMVar m ()
        getDelay now rest
     DelaySTM time t | now >= time -> do
        atomically $ writeTVar t True
        getDelay now rest
     _otherwise ->
        -- delay is in millisecs for WaitForSingleObject
        let micro_seconds = delayTime d - now
            milli_seconds = (micro_seconds + 999) `div` 1000
        in return (all, fromIntegral milli_seconds)

foreign import ccall unsafe "getIOManagerEvent" -- in the RTS (ThrIOManager.c)
  c_getIOManagerEvent :: IO HANDLE

foreign import ccall unsafe "readIOManagerEvent" -- in the RTS (ThrIOManager.c)
  c_readIOManagerEvent :: IO Word32

foreign import ccall unsafe "sendIOManagerEvent" -- in the RTS (ThrIOManager.c)
  c_sendIOManagerEvent :: Word32 -> IO ()

foreign import ccall "WaitForSingleObject"
   c_WaitForSingleObject :: HANDLE -> DWORD -> IO DWORD

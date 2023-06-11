{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP
           , NoImplicitPrelude
           , MagicHash
           , UnboxedTuples
  #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Conc.IO
-- Copyright   :  (c) The University of Glasgow, 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Basic concurrency stuff.
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--
-----------------------------------------------------------------------------

-- No: #hide, because bits of this module are exposed by the stm package.
-- However, we don't want this module to be the home location for the
-- bits it exports, we'd rather have Control.Concurrent and the other
-- higher level modules be the home.  Hence: #not-home

module GHC.Internal.Conc.IO
        ( ensureIOManagerIsRunning
        , ioManagerCapabilitiesChanged
        , interruptIOManager

        -- * Waiting
        , threadDelay
        , registerDelay
        , threadWaitRead
        , threadWaitWrite
        , threadWaitReadSTM
        , threadWaitWriteSTM
        , closeFdWith

#if defined(mingw32_HOST_OS)
        , asyncRead
        , asyncWrite
        , asyncDoProc

        , asyncReadBA
        , asyncWriteBA

        , ConsoleEvent(..)
        , win32ConsoleHandler
        , toWin32ConsoleEvent
#endif
        ) where

import GHC.Internal.Base
import GHC.Internal.Conc.Sync as Sync
import GHC.Internal.Real ( fromIntegral )
import GHC.Internal.System.Posix.Types

#if defined(mingw32_HOST_OS)
import qualified GHC.Internal.Conc.Windows as Windows
import GHC.Internal.IO.SubSystem
import GHC.Internal.Conc.Windows (asyncRead, asyncWrite, asyncDoProc, asyncReadBA,
                         asyncWriteBA, ConsoleEvent(..), win32ConsoleHandler,
                         toWin32ConsoleEvent)
#elif !defined(javascript_HOST_ARCH)
import qualified GHC.Internal.Event.Thread as Event
#endif

#if defined(wasm32_HOST_ARCH)
import qualified GHC.Internal.Wasm.Prim.Conc as Wasm
import qualified GHC.Internal.Wasm.Prim.Flag as Wasm
#endif

ensureIOManagerIsRunning :: IO ()
#if defined(javascript_HOST_ARCH)
ensureIOManagerIsRunning = pure ()
#elif !defined(mingw32_HOST_OS)
ensureIOManagerIsRunning = Event.ensureIOManagerIsRunning
#else
ensureIOManagerIsRunning = Windows.ensureIOManagerIsRunning
#endif

-- | Interrupts the current wait of the I/O manager if it is currently blocked.
-- This instructs it to re-read how much it should wait and to process any
-- pending events.
--
-- @since base-4.15
interruptIOManager :: IO ()
#if !defined(mingw32_HOST_OS)
interruptIOManager = return ()
#else
interruptIOManager = Windows.interruptIOManager
#endif

ioManagerCapabilitiesChanged :: IO ()
#if !defined(mingw32_HOST_OS) && !defined(javascript_HOST_ARCH)
ioManagerCapabilitiesChanged = Event.ioManagerCapabilitiesChanged
#else
ioManagerCapabilitiesChanged = return ()
#endif

-- | Block the current thread until data is available to read on the
-- given file descriptor (GHC only).
--
-- This will throw an 'Prelude.IOError' if the file descriptor was closed
-- while this thread was blocked.  To safely close a file descriptor
-- that has been used with 'threadWaitRead', use 'closeFdWith'.
threadWaitRead :: Fd -> IO ()
threadWaitRead fd
#if !defined(mingw32_HOST_OS) && !defined(javascript_HOST_ARCH)
  | threaded  = Event.threadWaitRead fd
#endif
  | otherwise = IO $ \s ->
        case fromIntegral fd of { I# fd# ->
        case waitRead# fd# s of { s' -> (# s', () #)
        }}

-- | Block the current thread until data can be written to the
-- given file descriptor (GHC only).
--
-- This will throw an 'Prelude.IOError' if the file descriptor was closed
-- while this thread was blocked.  To safely close a file descriptor
-- that has been used with 'threadWaitWrite', use 'closeFdWith'.
threadWaitWrite :: Fd -> IO ()
threadWaitWrite fd
#if !defined(mingw32_HOST_OS) && !defined(javascript_HOST_ARCH)
  | threaded  = Event.threadWaitWrite fd
#endif
  | otherwise = IO $ \s ->
        case fromIntegral fd of { I# fd# ->
        case waitWrite# fd# s of { s' -> (# s', () #)
        }}

-- | Returns an STM action that can be used to wait for data
-- to read from a file descriptor. The second returned value
-- is an IO action that can be used to deregister interest
-- in the file descriptor.
threadWaitReadSTM :: Fd -> IO (Sync.STM (), IO ())
threadWaitReadSTM fd
#if !defined(mingw32_HOST_OS) && !defined(javascript_HOST_ARCH)
  | threaded  = Event.threadWaitReadSTM fd
#endif
  | otherwise = do
      m <- Sync.newTVarIO False
      t <- Sync.forkIO $ do
        threadWaitRead fd
        Sync.atomically $ Sync.writeTVar m True
      let waitAction = do b <- Sync.readTVar m
                          if b then return () else retry
      let killAction = Sync.killThread t
      return (waitAction, killAction)

-- | Returns an STM action that can be used to wait until data
-- can be written to a file descriptor. The second returned value
-- is an IO action that can be used to deregister interest
-- in the file descriptor.
threadWaitWriteSTM :: Fd -> IO (Sync.STM (), IO ())
threadWaitWriteSTM fd
#if !defined(mingw32_HOST_OS) && !defined(javascript_HOST_ARCH)
  | threaded  = Event.threadWaitWriteSTM fd
#endif
  | otherwise = do
      m <- Sync.newTVarIO False
      t <- Sync.forkIO $ do
        threadWaitWrite fd
        Sync.atomically $ Sync.writeTVar m True
      let waitAction = do b <- Sync.readTVar m
                          if b then return () else retry
      let killAction = Sync.killThread t
      return (waitAction, killAction)

-- | Close a file descriptor in a concurrency-safe way as far as the runtime
-- system is concerned (GHC only).  If you are using 'threadWaitRead' or
-- 'threadWaitWrite' to perform blocking I\/O, you /must/ use this function
-- to close file descriptors, or blocked threads may not be woken.
--
-- Any threads that are blocked on the file descriptor via
-- 'threadWaitRead' or 'threadWaitWrite' will be unblocked by having
-- IO exceptions thrown.
--
-- Note that on systems that reuse file descriptors (such as Linux),
-- using this function on a file descriptor while other threads can still
-- potentially use it is always prone to race conditions without further
-- synchronization.
--
-- It is recommended to only call @'closeFdWith'@ once no other threads can
-- use the given file descriptor anymore.
closeFdWith :: (Fd -> IO ()) -- ^ Low-level action that performs the real close.
            -> Fd            -- ^ File descriptor to close.
            -> IO ()
closeFdWith close fd
#if !defined(mingw32_HOST_OS) && !defined(javascript_HOST_ARCH)
  | threaded  = Event.closeFdWith close fd
#endif
  | otherwise = close fd

-- | Suspends the current thread for a given number of microseconds
-- (GHC only).
--
-- There is no guarantee that the thread will be rescheduled promptly
-- when the delay has expired, but the thread will never continue to
-- run /earlier/ than specified.
--
-- Be careful not to exceed @maxBound :: Int@, which on 32-bit machines is only
-- 2147483647 μs, less than 36 minutes.
-- Consider using @Control.Concurrent.Thread.Delay.delay@ from @unbounded-delays@ package.
threadDelay :: Int -> IO ()
threadDelay time
#if defined(mingw32_HOST_OS)
  | isWindowsNativeIO = Windows.threadDelay time
  | threaded          = Windows.threadDelay time
#elif defined(wasm32_HOST_ARCH)
  | Wasm.isJSFFIUsed  = Wasm.threadDelay time
#elif !defined(javascript_HOST_ARCH)
  | threaded  = Event.threadDelay time
#endif
  | otherwise         = IO $ \s ->
        case time of { I# time# ->
        case delay# time# s of { s' -> (# s', () #)
        }}

-- | Switch the value of returned 'TVar' from initial value 'False' to 'True'
-- after a given number of microseconds. The caveats associated with
-- 'threadDelay' also apply.
--
-- Be careful not to exceed @maxBound :: Int@, which on 32-bit machines is only
-- 2147483647 μs, less than 36 minutes.
--
registerDelay :: Int -> IO (TVar Bool)
registerDelay _usecs
#if defined(mingw32_HOST_OS)
  | isWindowsNativeIO = Windows.registerDelay _usecs
  | threaded          = Windows.registerDelay _usecs
#elif !defined(javascript_HOST_ARCH)
  | threaded          = Event.registerDelay _usecs
#endif
  | otherwise         = errorWithoutStackTrace "registerDelay: requires -threaded"

#if !defined(javascript_HOST_ARCH)
foreign import ccall unsafe "rtsSupportsBoundThreads" threaded :: Bool
#endif

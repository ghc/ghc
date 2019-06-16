{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP
           , NoImplicitPrelude
           , MagicHash
           , UnboxedTuples
  #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Conc.IO
-- Copyright   :  (c) The University of Glasgow, 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Basic concurrency stuff.
--
-----------------------------------------------------------------------------

-- No: #hide, because bits of this module are exposed by the stm package.
-- However, we don't want this module to be the home location for the
-- bits it exports, we'd rather have Control.Concurrent and the other
-- higher level modules be the home.  Hence: #not-home

module GHC.Conc.IO
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
        , closeWith

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

import Foreign
import GHC.Base
import GHC.Conc.Sync as Sync
import GHC.Real ( fromIntegral )

#if defined(mingw32_HOST_OS)
import qualified GHC.Conc.Windows as Windows
import GHC.IO.SubSystem
import GHC.Conc.Windows (asyncRead, asyncWrite, asyncDoProc, asyncReadBA,
                         asyncWriteBA, ConsoleEvent(..), win32ConsoleHandler,
                         toWin32ConsoleEvent)
#else
import qualified GHC.Event.Thread as Event
#endif
import GHC.IO.Types (BHandle)
import qualified GHC.IO.Types as Types

ensureIOManagerIsRunning :: IO ()
#if !defined(mingw32_HOST_OS)
ensureIOManagerIsRunning = Event.ensureIOManagerIsRunning
#else
ensureIOManagerIsRunning = Windows.ensureIOManagerIsRunning
#endif

-- | Interrupts the current wait of the I/O manager if it is currently blocked.
-- This instructs it to re-read how much it should wait and to process any
-- pending events.
-- @since <basever>
interruptIOManager :: IO ()
#if !defined(mingw32_HOST_OS)
interruptIOManager = return ()
#else
interruptIOManager = Windows.interruptIOManager
#endif

ioManagerCapabilitiesChanged :: IO ()
#if !defined(mingw32_HOST_OS)
ioManagerCapabilitiesChanged = Event.ioManagerCapabilitiesChanged
#else
ioManagerCapabilitiesChanged = return ()
#endif

-- | Block the current thread until data is available to read on the
-- given handle or file descriptor (GHC only).
--
-- This will throw an 'Prelude.IOError' if the handle or file descriptor was closed
-- while this thread was blocked.  To safely close a handle or file descriptor
-- that has been used with 'threadWaitRead', use 'closeFdWith'.
{-# SPECIALIZE threadWaitRead :: Types.IntPtr -> IO () #-}
{-# SPECIALIZE threadWaitRead :: Types.Fd -> IO () #-}
threadWaitRead :: BHandle a => a -> IO ()
threadWaitRead bh
#if !defined(mingw32_HOST_OS)
  | threaded  = Event.threadWaitRead (toFD bh)
#endif
  | otherwise = IO $ \s ->
        case fromIntegral bh of { I# bh# ->
        case waitRead# bh# s of { s' -> (# s', () #)
        }}

-- | Block the current thread until data can be written to the
-- given handle or file descriptor (GHC only).
--
-- This will throw an 'Prelude.IOError' if the handle or file descriptor was closed
-- while this thread was blocked.  To safely close a handle or file descriptor
-- that has been used with 'threadWaitWrite', use 'closeFdWith'.
{-# SPECIALIZE threadWaitWrite :: Types.IntPtr -> IO () #-}
{-# SPECIALIZE threadWaitWrite :: Types.Fd -> IO () #-}
threadWaitWrite :: BHandle a => a -> IO ()
threadWaitWrite bh
#if !defined(mingw32_HOST_OS)
  | threaded  = Event.threadWaitWrite (toFD bh)
#endif
  | otherwise = IO $ \s ->
        case fromIntegral bh of { I# bh# ->
        case waitWrite# bh# s of { s' -> (# s', () #)
        }}

-- | Returns an STM action that can be used to wait for data
-- to read from a handle or file descriptor. The second returned value
-- is an IO action that can be used to deregister interest
-- in the handle or file descriptor.
{-# SPECIALIZE threadWaitReadSTM :: Types.IntPtr -> IO (Sync.STM (), IO ()) #-}
{-# SPECIALIZE threadWaitReadSTM :: Types.Fd -> IO (Sync.STM (), IO ()) #-}
threadWaitReadSTM :: BHandle a => a -> IO (Sync.STM (), IO ())
threadWaitReadSTM bh
#if !defined(mingw32_HOST_OS)
  | threaded  = Event.threadWaitReadSTM (toFD bh)
#endif
  | otherwise = do
      m <- Sync.newTVarIO False
      t <- Sync.forkIO $ do
        threadWaitRead bh
        Sync.atomically $ Sync.writeTVar m True
      let waitAction = do b <- Sync.readTVar m
                          if b then return () else retry
      let killAction = Sync.killThread t
      return (waitAction, killAction)

-- | Returns an STM action that can be used to wait until data
-- can be written to a handle or file descriptor. The second returned value
-- is an IO action that can be used to deregister interest
-- in the handle or file descriptor.
{-# SPECIALIZE threadWaitWriteSTM :: Types.IntPtr -> IO (Sync.STM (), IO ()) #-}
{-# SPECIALIZE threadWaitWriteSTM :: Types.Fd -> IO (Sync.STM (), IO ()) #-}
threadWaitWriteSTM :: BHandle a => a -> IO (Sync.STM (), IO ())
threadWaitWriteSTM bh
#if !defined(mingw32_HOST_OS)
  | threaded  = Event.threadWaitWriteSTM (toFD bh)
#endif
  | otherwise = do
      m <- Sync.newTVarIO False
      t <- Sync.forkIO $ do
        threadWaitWrite bh
        Sync.atomically $ Sync.writeTVar m True
      let waitAction = do b <- Sync.readTVar m
                          if b then return () else retry
      let killAction = Sync.killThread t
      return (waitAction, killAction)

-- | Close a handle or file descriptor in a concurrency-safe way (GHC only).  If
-- you are using 'threadWaitRead' or 'threadWaitWrite' to perform
-- blocking I\/O, you /must/ use this function to close file
-- descriptors, or blocked threads may not be woken.
--
-- Any threads that are blocked on the handle or file descriptor via
-- 'threadWaitRead' or 'threadWaitWrite' will be unblocked by having
-- IO exceptions thrown.
{-# SPECIALIZE closeWith :: (Types.IntPtr ->IO ()) -> Types.IntPtr -> IO () #-}
{-# SPECIALIZE closeWith :: (Types.Fd ->IO ()) -> Types.Fd -> IO () #-}
closeWith :: BHandle a => (a -> IO ()) -- ^ Low-level action that performs the real close.
          -> a            -- ^ handle or file descriptor to close.
          -> IO ()
closeWith close bh
#if !defined(mingw32_HOST_OS)
  | threaded  = Event.closeFdWith close (toFD bh)
#endif
  | otherwise = close bh

-- | Suspends the current thread for a given number of microseconds
-- (GHC only).
--
-- There is no guarantee that the thread will be rescheduled promptly
-- when the delay has expired, but the thread will never continue to
-- run /earlier/ than specified.
--
threadDelay :: Int -> IO ()
threadDelay time
#if defined(mingw32_HOST_OS)
  | isWindowsNativeIO = Windows.threadDelay time
  | threaded          = Windows.threadDelay time
#else
  | threaded          = Event.threadDelay time
#endif
  | otherwise         = IO $ \s ->
        case time of { I# time# ->
        case delay# time# s of { s' -> (# s', () #)
        }}

-- | Switch the value of returned 'TVar' from initial value 'False' to 'True'
-- after a given number of microseconds. The caveats associated with
-- 'threadDelay' also apply.
--
registerDelay :: Int -> IO (TVar Bool)
registerDelay usecs
#if defined(mingw32_HOST_OS)
  | isWindowsNativeIO = Windows.registerDelay usecs
  | threaded          = Windows.registerDelay usecs
#else
  | threaded          = Event.registerDelay usecs
#endif
  | otherwise         = errorWithoutStackTrace "registerDelay: requires -threaded"

foreign import ccall unsafe "rtsSupportsBoundThreads" threaded :: Bool

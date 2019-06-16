{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  GHC.Event.Windows.ManagedThreadPool
-- Copyright   :  (c) Tamar Christina 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- WinIO Windows Managed Thread pool API.  This thread pool scales dynamically
-- based on demand.
--
-------------------------------------------------------------------------------

module GHC.Event.Windows.ManagedThreadPool
  ( ThreadPool(..)
  , startThreadPool
  , notifyRunning
  , notifyWaiting
  ) where

import Control.Concurrent.MVar
import Data.Maybe
import Foreign
import GHC.Base
import GHC.Num ((-), (+))
import GHC.Real (fromIntegral)
import GHC.Show
import GHC.Windows
import qualified GHC.Event.Array as A
import qualified GHC.Windows     as Win32
import GHC.IO.Handle.Internals (debugIO)
import GHC.Conc.Sync (forkIO, showThreadId,
                      ThreadId(..), ThreadStatus(..),
                      threadStatus, sharedCAF)
import System.IO.Unsafe     (unsafePerformIO)
import GHC.RTS.Flags

------------------------------------------------------------------------
-- Thread spool manager

type WorkerJob = IO ()

-- | Thread pool manager state
data ThreadPool = ThreadPool
  { thrMainThread    :: Maybe ThreadId
  , thrMaxThreads    :: {-# UNPACK #-} !Int
  , thrMinThreads    :: {-# UNPACK #-} !Int
  , thrCurThreads    :: {-# UNPACK #-} !Int
  , thrCallBack      :: WorkerJob
  , thrActiveThreads :: MVar Int
  , thrMonitor       :: MVar ()
  , thrThreadIds     :: {-#UNPACK #-} !(A.Array ThreadId)
  }

startThreadPool :: WorkerJob -> IO ThreadPool
startThreadPool job = do
  debugIO "Starting I/O manager threadpool..."
  let thrMinThreads = 2
  let thrCurThreads = 0
  let thrCallBack   = job
  thrMaxThreads     <- (fromIntegral . numIoWorkerThreads) `fmap` getMiscFlags
  thrActiveThreads  <- newMVar 0
  thrMonitor        <- newEmptyMVar
  thrThreadIds      <- undefined -- A.new thrMaxThreads
  let thrMainThread = Nothing

  let !pool = ThreadPool{..}
  return pool

monitorThreadPool :: MVar () -> IO ()
monitorThreadPool monitor = do
  active <- takeMVar monitor

  return ()

notifyRunning :: Maybe ThreadPool -> IO ()
notifyRunning Nothing     = return ()
notifyRunning (Just pool) = do
  modifyMVar_ (thrActiveThreads pool) (\x -> return $ x + 1)
  _ <- tryPutMVar (thrMonitor pool) ()
  return ()

notifyWaiting :: Maybe ThreadPool -> IO ()
notifyWaiting Nothing     = return ()
notifyWaiting (Just pool) = do
  modifyMVar_ (thrActiveThreads pool) (\x -> return $ x - 1)
  _ <- tryPutMVar (thrMonitor pool) ()
  return ()

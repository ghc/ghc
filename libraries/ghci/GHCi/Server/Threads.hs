{-# LANGUAGE LambdaCase, CPP, RankNTypes, RecordWildCards, GADTs, ScopedTypeVariables, LinearTypes, BlockArguments #-}
module GHCi.Server.Threads
  ( ThreadManager     -- abstract for linearity
  , withThreadManager -- scoped, so that we're guaranteed to wait for threads
  , spawnFinally
  ) where

import Prelude
import Control.Exception
import Control.Monad

import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Set as Set
import qualified Unsafe.Coerce as Unsafe

data ThreadManager = ThreadManager
  { managedThreads :: !(TVar (Set.Set ThreadId))
  , managerClosed  :: !(TVar Bool)
  }

-- | Cancels and Waits for all threads spawned by 'ThreadManager' to terminate
-- before returning.
--
-- This thread manager which cancels then waits for all pending threads allows
-- us to provide a resource-safe linearly typed 'spawnFinally', because
-- 'withThreadManager' guarantees neither the threads nor their exceptions
-- leak.
withThreadManager :: (ThreadManager -> IO a) -> IO a
withThreadManager k = do
  managedThreads <- newTVarIO Set.empty
  managerClosed  <- newTVarIO False
  let tm = ThreadManager{..}
  k tm `finally` shutdown tm
  where
    shutdown ThreadManager{..} = do
      ts <- atomically $ do
        writeTVar managerClosed True
        Set.elems <$> readTVar managedThreads
      mapM_ killThread ts

-- | Linear and resource-safe because 'ThreadManager' is guaranteed to wait or
-- cancel all threads.
spawnFinally :: ThreadManager -> (Either SomeException a -> IO ()) %1 -> IO a -> IO ()
spawnFinally ThreadManager{..} = unsafe_linear \fin -> \action -> void $
  forkFinally
    ( mask $ \unmask -> do
        tid <- myThreadId
        registered <- atomically $ do
          s <- readTVar managerClosed
          unless s $ modifyTVar' managedThreads (Set.insert tid)
          pure (not s)
        if registered
          then (unmask action)
                `finally`
                  atomically (modifyTVar' managedThreads (Set.delete tid))
          else throwIO ThreadKilled
    )
    ( \ea -> fin ea )

unsafe_linear :: (a -> b) %1 -> (a %1 -> b)
unsafe_linear = Unsafe.unsafeCoerce (Unsafe.unsafeCoerce :: (a -> b) -> (a %1 -> b))

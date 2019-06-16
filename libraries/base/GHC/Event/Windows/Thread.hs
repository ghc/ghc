{-# LANGUAGE NoImplicitPrelude #-}
module GHC.Event.Windows.Thread (
    ensureIOManagerIsRunning,
    interruptIOManager,
    threadDelay,
    registerDelay,
) where

import GHC.Conc.Sync
import GHC.Base
import GHC.IO
import GHC.IOPort
import GHC.Real

import GHC.Event.Windows.Clock
import GHC.Event.Windows

ensureIOManagerIsRunning :: IO ()
ensureIOManagerIsRunning = wakeupIOManager

interruptIOManager :: IO ()
interruptIOManager = interruptSystemManager

threadDelay :: Int -> IO ()
threadDelay usecs = mask_ $ do
    m <- newEmptyIOPort
    mgr <- getSystemManager
    reg <- registerTimeout mgr secs $ writeIOPort m () >> return ()
    readIOPort m `onException` unregisterTimeout mgr reg
  where
    secs = microsecondsToSeconds usecs

registerDelay :: Int -> IO (TVar Bool)
registerDelay usecs = do
    t <- newTVarIO False
    mgr <- getSystemManager
    _ <- registerTimeout mgr secs $ atomically $ writeTVar t True
    return t
  where
    secs = microsecondsToSeconds usecs

microsecondsToSeconds :: Int -> Seconds
microsecondsToSeconds us = fromIntegral us / 1000000.0

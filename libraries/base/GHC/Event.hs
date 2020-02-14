{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- ----------------------------------------------------------------------------
-- | This module provides scalable event notification for file
-- descriptors and timeouts.
--
-- This module should be considered GHC internal.
--
-- ----------------------------------------------------------------------------

module GHC.Event
    (
      Poll
    , PSQ

      -- * Registering interest in I/O events
    , EventManager
    , Event
    , evtRead
    , evtWrite
    , IOCallback
    , FdKey(keyFd)
    , Lifetime(..)
    , registerFd
    , unregisterFd
    , unregisterFd_
    , closeFd
    , new
    , loop
    , emState

      -- * Registering interest in timeout events
    , newWith
    , newDefaultBackend
    , registerTimeout
    , TimeoutCallback
    , TimeoutKey
    , updateTimeout
    , unregisterTimeout
    , TimerManager
    , timeLoop

    -- * Threads
    , getSystemEventManager
    , getSystemTimerManager
    , ensureIOManagerIsRunning
    , threadDelay
    , threadWaitRead
    , threadWaitWrite
    ) where

import GHC.Event.Poll hiding (new)
import GHC.Event.PSQ
import GHC.Event.Manager (EventManager, Event, evtRead, evtWrite, IOCallback,
                          FdKey(keyFd), Lifetime(..), registerFd, unregisterFd,
                          unregisterFd_, closeFd, new, loop, emState)
import GHC.Event.TimerManager (newWith, newDefaultBackend, TimeoutCallback,
                               TimeoutKey, registerTimeout, updateTimeout,
                               unregisterTimeout, TimerManager, timeLoop)
import GHC.Event.Thread (ensureIOManagerIsRunning, getSystemEventManager,
                         getSystemTimerManager, threadDelay,
                         threadWaitRead, threadWaitWrite)

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
    ( -- * Types
      Event
    , EventManager
    , TimerManager
    , Poll
    , PSQ

      -- * Creation
    , getSystemEventManager
    , getSystemTimerManager

      -- * Registering interest in I/O events
    , evtRead
    , evtWrite
    , IOCallback
    , FdKey(keyFd)
    , Lifetime(..)
    , registerFd
    , unregisterFd
    , unregisterFd_
    , closeFd
    , newWith
    , loop

      -- * Registering interest in timeout events
    , new
    , newDefaultBackend
    , registerTimeout
    , TimeoutCallback
    , TimeoutKey
    , updateTimeout
    , unregisterTimeout

    -- * Threads
    , ensureIOManagerIsRunning
    , threadDelay
    , threadWaitRead
    ) where

import GHC.Event.Poll hiding (new)
import GHC.Event.PSQ
import GHC.Event.Manager (EventManager, Event, evtRead, evtWrite, IOCallback,
                          FdKey(keyFd), Lifetime(..), registerFd, unregisterFd,
                          unregisterFd_, closeFd, newWith, loop)
import GHC.Event.TimerManager (new, newDefaultBackend, TimeoutCallback,
                               TimeoutKey, registerTimeout, updateTimeout,
                               unregisterTimeout, TimerManager)
import GHC.Event.Thread (ensureIOManagerIsRunning, getSystemEventManager,
                         getSystemTimerManager, threadDelay, threadWaitRead)

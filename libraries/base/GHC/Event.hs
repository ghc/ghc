{-# LANGUAGE Trustworthy #-}

-- ----------------------------------------------------------------------------
-- | This module provides scalable event notification for file
-- descriptors and timeouts.
--
-- This module should be considered GHC internal.
--
-- ----------------------------------------------------------------------------

module GHC.Event
    ( -- * Types
      EventManager

      -- * Creation
    , getSystemEventManager

      -- * Running
    , loop

    -- ** Stepwise running
    , step
    , shutdown

      -- * Registering interest in I/O events
    , Event
    , evtRead
    , evtWrite
    , IOCallback
    , FdKey(keyFd)
    , registerFd
    , registerFd_
    , unregisterFd
    , unregisterFd_
    , closeFd

      -- * Registering interest in timeout events
    , TimeoutCallback
    , TimeoutKey
    , registerTimeout
    , updateTimeout
    , unregisterTimeout
    ) where

import GHC.Event.Manager
import GHC.Event.Thread (getSystemEventManager)


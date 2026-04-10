{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}

-- ----------------------------------------------------------------------------
-- | This module provides scalable event notification for file
-- descriptors and timeouts.
--
-- This module should be considered GHC internal.
--
-- ----------------------------------------------------------------------------

module GHC.Internal.Event
#if defined(javascript_HOST_ARCH)
    ( ) where

-- See W1 of Note [Tracking dependencies on primitives] in GHC.Internal.Base
import GHC.Internal.Types ()

#else
    ( -- * Types
      EventManager
    , TimerManager

      -- * Creation
    , getSystemEventManager
    , new
    , getSystemTimerManager

      -- * Registering interest in I/O events
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

      -- * Registering interest in timeout events
    , TimeoutCallback
    , TimeoutKey
    , registerTimeout
    , updateTimeout
    , unregisterTimeout
    ) where

import GHC.Internal.Event.Manager
import GHC.Internal.Event.TimerManager (TimeoutCallback, TimeoutKey, registerTimeout,
                               updateTimeout, unregisterTimeout, TimerManager)
import GHC.Internal.Event.Thread (getSystemEventManager, getSystemTimerManager)

#endif

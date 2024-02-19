{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

-- |
-- This module provides scalable event notification for file
-- descriptors and timeouts.
--
-- This module should be considered GHC internal.
--
-- ----------------------------------------------------------------------------

#if defined(javascript_HOST_ARCH)

module GHC.Event ( ) where

#else

module GHC.Event
    (-- *  Types
     EventManager,
     TimerManager,
     -- *  Creation
     getSystemEventManager,
     new,
     getSystemTimerManager,
     -- *  Registering interest in I/O events
     Event,
     evtRead,
     evtWrite,
     IOCallback,
     FdKey(keyFd),
     Lifetime(..),
     registerFd,
     unregisterFd,
     unregisterFd_,
     closeFd,
     -- *  Registering interest in timeout events
     TimeoutCallback,
     TimeoutKey,
     registerTimeout,
     updateTimeout,
     unregisterTimeout
     ) where

import GHC.Internal.Event

#endif

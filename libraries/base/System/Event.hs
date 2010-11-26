module System.Event
    ( -- * Types
      EventManager

      -- * Creation
    , new

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

import System.Event.Manager

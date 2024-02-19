-- |
-- Module      :  GHC.Event.Windows
-- Copyright   :  (c) Tamar Christina 2018
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  non-portable
--
-- WinIO Windows event manager.
--

module GHC.Event.Windows (
    -- * Manager
    Manager,
    getSystemManager,
    interruptSystemManager,
    wakeupIOManager,
    processRemoteCompletion,

    -- * Overlapped I/O
    associateHandle,
    associateHandle',
    withOverlapped,
    withOverlappedEx,
    StartCallback,
    StartIOCallback,
    CbResult(..),
    CompletionCallback,
    LPOVERLAPPED,

    -- * Timeouts
    TimeoutCallback,
    TimeoutKey,
    Seconds,
    registerTimeout,
    updateTimeout,
    unregisterTimeout,

    -- * Utilities
    withException,
    ioSuccess,
    ioFailed,
    ioFailedAny,
    getLastError,

    -- * I/O Result type
    IOResult(..),

    -- * I/O Event notifications
    HandleData (..), -- seal for release
    HandleKey (handleValue),
    registerHandle,
    unregisterHandle,

    -- * Console events
    module GHC.Internal.Event.Windows.ConsoleEvent
) where

import GHC.Internal.Event.Windows
import GHC.Internal.Event.Windows.ConsoleEvent

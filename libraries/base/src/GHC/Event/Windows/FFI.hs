-- |
-- Module      :  GHC.Event.Windows.FFI
-- Copyright   :  (c) Tamar Christina 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  non-portable
--
-- WinIO Windows API Foreign Function imports
--

module GHC.Event.Windows.FFI (
    -- * IOCP
    IOCP(..),
    CompletionKey,
    newIOCP,
    associateHandleWithIOCP,
    getQueuedCompletionStatusEx,
    postQueuedCompletionStatus,
    getOverlappedResult,

    -- * Completion Data
    CompletionData(..),
    CompletionCallback,
    withRequest,

    -- * Overlapped
    OVERLAPPED,
    LPOVERLAPPED,
    OVERLAPPED_ENTRY(..),
    LPOVERLAPPED_ENTRY,
    HASKELL_OVERLAPPED,
    LPHASKELL_OVERLAPPED,
    allocOverlapped,
    zeroOverlapped,
    pokeOffsetOverlapped,
    overlappedIOStatus,
    overlappedIONumBytes,

    -- * Cancel pending I/O
    cancelIoEx,
    cancelIoEx',

    -- * Monotonic time

    -- ** GetTickCount
    getTickCount64,

    -- ** QueryPerformanceCounter
    queryPerformanceCounter,
    queryPerformanceFrequency,

    -- ** Miscellaneous
    throwWinErr,
    setLastError
  ) where

import GHC.Internal.Event.Windows.FFI

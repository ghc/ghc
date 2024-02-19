{-# LANGUAGE Unsafe #-}
{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Conc
-- Copyright   :  (c) The University of Glasgow, 1994-2023
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Basic concurrency stuff.
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--
-----------------------------------------------------------------------------

-- No: #hide, because bits of this module are exposed by the stm package.
-- However, we don't want this module to be the home location for the
-- bits it exports, we'd rather have Control.Concurrent and the other
-- higher level modules be the home.  Hence: #not-home

module GHC.Conc
        ( ThreadId(..)

        -- * Forking and suchlike
        , forkIO
        , forkIOWithUnmask
        , forkOn
        , forkOnWithUnmask
        , numCapabilities
        , getNumCapabilities
        , setNumCapabilities
        , getNumProcessors
        , numSparks
        , childHandler
        , myThreadId
        , killThread
        , throwTo
        , par
        , pseq
        , runSparks
        , yield
        , labelThread
        , mkWeakThreadId
        , listThreads

        , ThreadStatus(..), BlockReason(..)
        , threadStatus
        , threadCapability

        , newStablePtrPrimMVar, PrimMVar

        -- * Waiting
        , threadDelay
        , registerDelay
        , threadWaitRead
        , threadWaitWrite
        , threadWaitReadSTM
        , threadWaitWriteSTM
        , closeFdWith

        -- * Allocation counter and limit
        , setAllocationCounter
        , getAllocationCounter
        , enableAllocationLimit
        , disableAllocationLimit

        -- * TVars
        , STM(..)
        , atomically
        , retry
        , orElse
        , throwSTM
        , catchSTM
        , TVar(..)
        , newTVar
        , newTVarIO
        , readTVar
        , readTVarIO
        , writeTVar
        , unsafeIOToSTM

        -- * Miscellaneous
        , withMVar
#if defined(mingw32_HOST_OS)
        , asyncRead
        , asyncWrite
        , asyncDoProc

        , asyncReadBA
        , asyncWriteBA
#endif

#if !defined(mingw32_HOST_OS)
        , Signal, HandlerFun, setHandler, runHandlers
#endif

        , ensureIOManagerIsRunning
        , ioManagerCapabilitiesChanged

#if defined(mingw32_HOST_OS)
        , ConsoleEvent(..)
        , win32ConsoleHandler
        , toWin32ConsoleEvent
#endif
        , setUncaughtExceptionHandler
        , getUncaughtExceptionHandler

        , reportError, reportStackOverflow, reportHeapOverflow
        ) where

import GHC.Internal.Conc.IO
import GHC.Internal.Conc.Sync

#if !defined(mingw32_HOST_OS)
import GHC.Internal.Conc.Signal
#endif

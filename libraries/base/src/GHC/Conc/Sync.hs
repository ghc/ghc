{-# LANGUAGE MagicHash #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      :  GHC.Conc.Sync
-- Copyright   :  (c) The University of Glasgow, 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Basic concurrency stuff.
--

module GHC.Conc.Sync
        (
        -- * Threads
          ThreadId(..)
        , fromThreadId
        , showThreadId
        , myThreadId
        , killThread
        , throwTo
        , yield
        , labelThread
        , labelThreadByteArray#
        , mkWeakThreadId
        -- ** Queries
        , listThreads
        , threadLabel
        , ThreadStatus(..), BlockReason(..)
        , threadStatus
        , threadCapability

        -- * Forking and suchlike
        , forkIO
        , forkIOWithUnmask
        , forkOn
        , forkOnWithUnmask

        -- * Capabilities
        , numCapabilities
        , getNumCapabilities
        , setNumCapabilities
        , getNumProcessors

        -- * Sparks
        , numSparks
        , childHandler
        , par
        , pseq
        , runSparks

        -- * PrimMVar
        , newStablePtrPrimMVar, PrimMVar

        -- * Allocation counter and quota
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
        , modifyMVar_

        , setUncaughtExceptionHandler
        , getUncaughtExceptionHandler

        , reportError, reportStackOverflow, reportHeapOverflow

        , sharedCAF
        ) where

import GHC.Internal.Conc.Sync

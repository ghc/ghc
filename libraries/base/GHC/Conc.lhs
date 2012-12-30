\begin{code}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Conc
-- Copyright   :  (c) The University of Glasgow, 1994-2002
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Basic concurrency stuff.
-- 
-----------------------------------------------------------------------------

-- No: #hide, because bits of this module are exposed by the stm package.
-- However, we don't want this module to be the home location for the
-- bits it exports, we'd rather have Control.Concurrent and the other
-- higher level modules be the home.  Hence:

#include "Typeable.h"

-- #not-home
module GHC.Conc
        ( ThreadId(..)

        -- * Forking and suchlike
        , forkIO
        , forkIOUnmasked
        , forkIOWithUnmask
        , forkOn
        , forkOnIO
        , forkOnIOUnmasked
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

        , ThreadStatus(..), BlockReason(..)
        , threadStatus
        , threadCapability

        -- * Waiting
        , threadDelay
        , registerDelay
        , threadWaitRead
        , threadWaitWrite
        , threadWaitReadSTM
        , threadWaitWriteSTM
        , closeFdWith

        -- * TVars
        , STM(..)
        , atomically
        , retry
        , orElse
        , throwSTM
        , catchSTM
        , alwaysSucceeds
        , always
        , TVar(..)
        , newTVar
        , newTVarIO
        , readTVar
        , readTVarIO
        , writeTVar
        , unsafeIOToSTM

        -- * Miscellaneous
        , withMVar
#ifdef mingw32_HOST_OS
        , asyncRead
        , asyncWrite
        , asyncDoProc

        , asyncReadBA
        , asyncWriteBA
#endif

#ifndef mingw32_HOST_OS
        , Signal, HandlerFun, setHandler, runHandlers
#endif

        , ensureIOManagerIsRunning

#ifdef mingw32_HOST_OS
        , ConsoleEvent(..)
        , win32ConsoleHandler
        , toWin32ConsoleEvent
#endif
        , setUncaughtExceptionHandler
        , getUncaughtExceptionHandler

        , reportError, reportStackOverflow
        ) where

import GHC.Conc.IO
import GHC.Conc.Sync

#ifndef mingw32_HOST_OS
import GHC.Conc.Signal
#endif

\end{code}

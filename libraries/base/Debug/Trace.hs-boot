{-# LANGUAGE Unsafe #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnboxedTuples #-}

-- This boot file is necessary to allow GHC developers to
-- use trace facilities in those (relatively few) modules that Debug.Trace
-- itself depends on. It is also necessary to make DsMonad.pprRuntimeTrace
-- trace injections work in those modules.

-----------------------------------------------------------------------------
-- |
-- Module      :  Debug.Trace
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions for tracing and monitoring execution.
--
-- These can be useful for investigating bugs or performance problems.
-- They should /not/ be used in production code.
--
-----------------------------------------------------------------------------

module Debug.Trace (
        -- * Tracing
        -- $tracing
        trace,
        traceId,
        traceShow,
        traceShowId,
        traceStack,
        traceIO,
        traceM,
        traceShowM,

        -- * Eventlog tracing
        -- $eventlog_tracing
        traceEvent,
        traceEventIO,

        -- * Execution phase markers
        -- $markers
        traceMarker,
        traceMarkerIO,
  ) where

import GHC.Base
import GHC.Show

traceIO :: String -> IO ()

trace :: String -> a -> a

traceId :: String -> String

traceShow :: Show a => a -> b -> b

traceShowId :: Show a => a -> a

traceM :: Applicative f => String -> f ()

traceShowM :: (Show a, Applicative f) => a -> f ()

traceStack :: String -> a -> a

traceEvent :: String -> a -> a

traceEventIO :: String -> IO ()

traceMarker :: String -> a -> a

traceMarkerIO :: String -> IO ()

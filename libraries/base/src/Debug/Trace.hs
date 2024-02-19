-- |
--
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

module Debug.Trace
    (-- * Tracing
     -- $tracing
     trace,
     traceId,
     traceShow,
     traceShowId,
     traceWith,
     traceShowWith,
     traceStack,
     traceIO,
     traceM,
     traceShowM,
     putTraceMsg,

     -- * Eventlog tracing
     -- $eventlog_tracing
     traceEvent,
     traceEventWith,
     traceEventIO,
     flushEventLog,

     -- * Execution phase markers
     -- $markers
     traceMarker,
     traceMarkerIO,
     ) where

import GHC.Internal.Debug.Trace

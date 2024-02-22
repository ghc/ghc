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

-- $setup
-- >>> import Prelude

-- $tracing
--
-- The 'trace', 'traceShow' and 'traceIO' functions print messages to an output
-- stream. They are intended for \"printf debugging\", that is: tracing the flow
-- of execution and printing interesting values.
--
-- All these functions evaluate the message completely before printing
-- it; so if the message is not fully defined, none of it will be
-- printed.
--
-- The usual output stream is 'GHC.Internal.System.IO.stderr'. For Windows GUI applications
-- (that have no stderr) the output is directed to the Windows debug console.
-- Some implementations of these functions may decorate the string that\'s
-- output to indicate that you\'re tracing.

-- $eventlog_tracing
--
-- Eventlog tracing is a performance profiling system. These functions emit
-- extra events into the eventlog. In combination with eventlog profiling
-- tools these functions can be used for monitoring execution and
-- investigating performance problems.
--
-- Currently only GHC provides eventlog profiling, see the GHC user guide for
-- details on how to use it. These function exists for other Haskell
-- implementations but no events are emitted. Note that the string message is
-- always evaluated, whether or not profiling is available or enabled.

-- $markers
--
-- When looking at a profile for the execution of a program we often want to
-- be able to mark certain points or phases in the execution and see that
-- visually in the profile.
--
-- For example, a program might have several distinct phases with different
-- performance or resource behaviour in each phase. To properly interpret the
-- profile graph we really want to see when each phase starts and ends.
--
-- Markers let us do this: we can annotate the program to emit a marker at
-- an appropriate point during execution and then see that in a profile.
--
-- Currently this feature is only supported in GHC by the eventlog tracing
-- system, but in future it may also be supported by the heap profiling or
-- other profiling tools. These function exists for other Haskell
-- implementations but they have no effect. Note that the string message is
-- always evaluated, whether or not profiling is available or enabled.


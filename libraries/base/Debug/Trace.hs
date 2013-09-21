{-# LANGUAGE Unsafe #-}
{-# LANGUAGE ForeignFunctionInterface, MagicHash, UnboxedTuples #-}

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
        putTraceMsg,

        -- * Eventlog tracing
        -- $eventlog_tracing
        traceEvent,
        traceEventIO,
        
        -- * Execution phase markers
        -- $markers
        traceMarker,
        traceMarkerIO,
  ) where

import Prelude
import System.IO.Unsafe
import Control.Monad

import Foreign.C.String
import GHC.Base
import qualified GHC.Foreign
import GHC.IO.Encoding
import GHC.Ptr
import GHC.Stack

-- $tracing
--
-- The 'trace', 'traceShow' and 'traceIO' functions print messages to an output
-- stream. They are intended for \"printf debugging\", that is: tracing the flow
-- of execution and printing interesting values.

-- The usual output stream is 'System.IO.stderr'. For Windows GUI applications
-- (that have no stderr) the output is directed to the Windows debug console.
-- Some implementations of these functions may decorate the string that\'s
-- output to indicate that you\'re tracing.

-- | The 'traceIO' function outputs the trace message from the IO monad.
-- This sequences the output with respect to other IO actions.
--
traceIO :: String -> IO ()
traceIO msg = do
    withCString "%s\n" $ \cfmt ->
     withCString msg  $ \cmsg ->
      debugBelch cfmt cmsg

-- don't use debugBelch() directly, because we cannot call varargs functions
-- using the FFI.
foreign import ccall unsafe "HsBase.h debugBelch2"
   debugBelch :: CString -> CString -> IO ()

-- | Deprecated. Use 'traceIO'.
putTraceMsg :: String -> IO ()
putTraceMsg = traceIO
{-# DEPRECATED putTraceMsg "Use Debug.Trace.traceIO" #-} -- deprecated in 7.4


{-# NOINLINE trace #-}
{-|
The 'trace' function outputs the trace message given as its first argument,
before returning the second argument as its result.

For example, this returns the value of @f x@ but first outputs the message.

> trace ("calling f with x = " ++ show x) (f x)

The 'trace' function should /only/ be used for debugging, or for monitoring
execution. The function is not referentially transparent: its type indicates
that it is a pure function but it has the side effect of outputting the
trace message.
-}
trace :: String -> a -> a
trace string expr = unsafePerformIO $ do
    traceIO string
    return expr

{-|
Like 'trace' but returns the message instead of a third value.

/Since: 4.7.0.0/
-}
traceId :: String -> String
traceId a = trace a a

{-|
Like 'trace', but uses 'show' on the argument to convert it to a 'String'.

This makes it convenient for printing the values of interesting variables or
expressions inside a function. For example here we print the value of the
variables @x@ and @z@:

> f x y =
>     traceShow (x, z) $ result
>   where
>     z = ...
>     ...
-}
traceShow :: (Show a) => a -> b -> b
traceShow = trace . show

{-|
Like 'traceShow' but returns the shown value instead of a third value.

/Since: 4.7.0.0/
-}
traceShowId :: (Show a) => a -> a
traceShowId a = trace (show a) a

{-|
Like 'trace' but returning unit in an arbitrary monad. Allows for convenient
use in do-notation. Note that the application of 'trace' is not an action in the
monad, as 'traceIO' is in the 'IO' monad.

> ... = do
>   x <- ...
>   traceM $ "x: " ++ show x
>   y <- ...
>   traceM $ "y: " ++ show y

/Since: 4.7.0.0/
-}
traceM :: (Monad m) => String -> m ()
traceM string = trace string $ return ()

{-|
Like 'traceM', but uses 'show' on the argument to convert it to a 'String'.

> ... = do
>   x <- ...
>   traceMShow $ x
>   y <- ...
>   traceMShow $ x + y

/Since: 4.7.0.0/
-}
traceShowM :: (Show a, Monad m) => a -> m ()
traceShowM = traceM . show

-- | like 'trace', but additionally prints a call stack if one is
-- available.
--
-- In the current GHC implementation, the call stack is only
-- availble if the program was compiled with @-prof@; otherwise
-- 'traceStack' behaves exactly like 'trace'.  Entries in the call
-- stack correspond to @SCC@ annotations, so it is a good idea to use
-- @-fprof-auto@ or @-fprof-auto-calls@ to add SCC annotations automatically.
--
traceStack :: String -> a -> a
traceStack str expr = unsafePerformIO $ do
   traceIO str
   stack <- currentCallStack
   when (not (null stack)) $ traceIO (renderStack stack)
   return expr


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

{-# NOINLINE traceEvent #-}
-- | The 'traceEvent' function behaves like 'trace' with the difference that
-- the message is emitted to the eventlog, if eventlog profiling is available
-- and enabled at runtime.
--
-- It is suitable for use in pure code. In an IO context use 'traceEventIO'
-- instead.
--
-- Note that when using GHC's SMP runtime, it is possible (but rare) to get
-- duplicate events emitted if two CPUs simultaneously evaluate the same thunk
-- that uses 'traceEvent'.
--
traceEvent :: String -> a -> a
traceEvent msg expr = unsafeDupablePerformIO $ do
    traceEventIO msg
    return expr

-- | The 'traceEventIO' function emits a message to the eventlog, if eventlog
-- profiling is available and enabled at runtime.
--
-- Compared to 'traceEvent', 'traceEventIO' sequences the event with respect to
-- other IO actions.
--
traceEventIO :: String -> IO ()
traceEventIO msg =
  GHC.Foreign.withCString utf8 msg $ \(Ptr p) -> IO $ \s ->
    case traceEvent# p s of s' -> (# s', () #)

-- $markers
--
-- When looking at a profile for the execution of a program we often want to
-- be able to mark certain points or phases in the execution and see that
-- visually in the profile.

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

{-# NOINLINE traceMarker #-}
-- | The 'traceMarker' function emits a marker to the eventlog, if eventlog
-- profiling is available and enabled at runtime. The @String@ is the name of
-- the marker. The name is just used in the profiling tools to help you keep
-- clear which marker is which.
--
-- This function is suitable for use in pure code. In an IO context use
-- 'traceMarkerIO' instead.
--
-- Note that when using GHC's SMP runtime, it is possible (but rare) to get
-- duplicate events emitted if two CPUs simultaneously evaluate the same thunk
-- that uses 'traceMarker'.
--
-- /Since: 4.7.0.0/
traceMarker :: String -> a -> a
traceMarker msg expr = unsafeDupablePerformIO $ do
    traceMarkerIO msg
    return expr

-- | The 'traceMarkerIO' function emits a marker to the eventlog, if eventlog
-- profiling is available and enabled at runtime.
--
-- Compared to 'traceMarker', 'traceMarkerIO' sequences the event with respect to
-- other IO actions.
--
-- /Since: 4.7.0.0/
traceMarkerIO :: String -> IO ()
traceMarkerIO msg =
  GHC.Foreign.withCString utf8 msg $ \(Ptr p) -> IO $ \s ->
    case traceMarker# p s of s' -> (# s', () #)

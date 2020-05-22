{-# LANGUAGE Unsafe #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnboxedTuples #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Debug.Trace
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

module GHC.Internal.Debug.Trace (
        -- * Tracing
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
        traceEvent,
        traceEventWith,
        traceEventIO,
        flushEventLog,

        -- * Execution phase markers
        traceMarker,
        traceMarkerIO,
  ) where

import GHC.Internal.Foreign.C.String
import qualified GHC.Internal.Foreign.C.String.Encoding as Enc
import GHC.Internal.Base
import GHC.Internal.IO.Encoding
import GHC.Internal.IO.Unsafe
import GHC.Internal.Ptr
import GHC.Internal.Show
import GHC.Internal.Stack
import GHC.Internal.Data.List (null, partition)
import GHC.Internal.RTS.Flags.Test

-- | 'userEventTracingEnabled' is True if event logging for user events (@+RTS -l@) is enabled.
userEventTracingEnabled :: IO Bool
userEventTracingEnabled = getUserEventTracingEnabled

-- | The 'traceIO' function outputs the trace message from the IO monad.
-- This sequences the output with respect to other IO actions.
--
-- @since base-4.5.0.0
traceIO :: String -> IO ()
traceIO msg =
    withCString "%s\n" $ \cfmt -> do
     -- NB: debugBelch can't deal with null bytes, so filter them
     -- out so we don't accidentally truncate the message.  See #9395
     let (nulls, msg') = partition (=='\0') msg
     withCString msg' $ \cmsg ->
      debugBelch cfmt cmsg
     when (not (null nulls)) $
       withCString "WARNING: previous trace message had null bytes" $ \cmsg ->
         debugBelch cfmt cmsg

-- don't use debugBelch() directly, because we cannot call varargs functions
-- using the FFI.
foreign import ccall unsafe "HsBase.h debugBelch2"
   debugBelch :: CString -> CString -> IO ()

-- |
putTraceMsg :: String -> IO ()
putTraceMsg = traceIO
{-# DEPRECATED putTraceMsg "Use 'GHC.Internal.Debug.Trace.traceIO'" #-} -- deprecated in 7.4


{-# NOINLINE trace #-}
{-|
The 'trace' function outputs the trace message given as its first argument,
before returning the second argument as its result.

For example, this returns the value of @f x@ and outputs the message to stderr.
Depending on your terminal (settings), they may or may not be mixed.

>>> let x = 123; f = show
>>> trace ("calling f with x = " ++ show x) (f x)
calling f with x = 123
"123"

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

>>> traceId "hello"
hello
"hello"

@since base-4.7.0.0
-}
traceId :: String -> String
traceId a = trace a a

{-|
Like 'trace', but uses 'show' on the argument to convert it to a 'String'.

This makes it convenient for printing the values of interesting variables or
expressions inside a function. For example, here we print the values of the
variables @x@ and @y@:

>>> let f x y = traceShow ("x", x, "y", y) (x + y) in f (1+2) 5
("x",3,"y",5)
8

Note in this example we also create simple labels just by including some strings.

-}
traceShow :: Show a => a -> b -> b
traceShow = trace . show

{-|
Like 'traceShow' but returns the shown value instead of a third value.

>>> traceShowId (1+2+3, "hello" ++ "world")
(6,"helloworld")
(6,"helloworld")

@since base-4.7.0.0
-}
traceShowId :: Show a => a -> a
traceShowId a = trace (show a) a

{-|
Like 'trace', but outputs the result of calling a function on the argument.

>>> traceWith fst ("hello","world")
hello
("hello","world")

@since base-4.18.0.0
-}
traceWith :: (a -> String) -> a -> a
traceWith f a = trace (f a) a

{-|
Like 'traceWith', but uses 'show' on the result of the function to convert it to
a 'String'.

>>> traceShowWith length [1,2,3]
3
[1,2,3]

@since base-4.18.0.0
-}
traceShowWith :: Show b => (a -> b) -> a -> a
traceShowWith f = traceWith (show . f)

{-|
Like 'trace' but returning unit in an arbitrary 'Applicative' context. Allows
for convenient use in do-notation.

Note that the application of 'traceM' is not an action in the 'Applicative'
context, as 'traceIO' is in the 'IO' type. While the fresh bindings in the
following example will force the 'traceM' expressions to be reduced every time
the @do@-block is executed, @traceM "not crashed"@ would only be reduced once,
and the message would only be printed once.  If your monad is in
'Control.Monad.IO.Class.MonadIO', @'Control.Monad.IO.Class.liftIO' . 'traceIO'@
may be a better option.

>>> :{
do
    x <- Just 3
    traceM ("x: " ++ show x)
    y <- pure 12
    traceM ("y: " ++ show y)
    pure (x*2 + y)
:}
x: 3
y: 12
Just 18

@since base-4.7.0.0
-}
traceM :: Applicative f => String -> f ()
traceM string = trace string $ pure ()

{-|
Like 'traceM', but uses 'show' on the argument to convert it to a 'String'.

>>> :{
do
    x <- Just 3
    traceShowM x
    y <- pure 12
    traceShowM y
    pure (x*2 + y)
:}
3
12
Just 18

@since base-4.7.0.0
-}
traceShowM :: (Show a, Applicative f) => a -> f ()
traceShowM = traceM . show

-- | like 'trace', but additionally prints a call stack if one is
-- available.
--
-- In the current GHC implementation, the call stack is only
-- available if the program was compiled with @-prof@; otherwise
-- 'traceStack' behaves exactly like 'trace'.  Entries in the call
-- stack correspond to @SCC@ annotations, so it is a good idea to use
-- @-fprof-auto@ or @-fprof-auto-calls@ to add SCC annotations automatically.
--
-- @since base-4.5.0.0
traceStack :: String -> a -> a
traceStack str expr = unsafePerformIO $ do
   traceIO str
   stack <- currentCallStack
   when (not (null stack)) $ traceIO (renderStack stack)
   return expr

{-# NOINLINE traceEvent #-}
-- | The 'traceEvent' function behaves like 'trace' with the difference that
-- the message is emitted to the eventlog, if eventlog tracing is available
-- and user event tracing is enabled at runtime.
--
-- It is suitable for use in pure code. In an IO context use 'traceEventIO'
-- instead.
--
-- Note that when using GHC's SMP runtime, it is possible (but rare) to get
-- duplicate events emitted if two CPUs simultaneously evaluate the same thunk
-- that uses 'traceEvent'.
--
-- @since base-4.5.0.0
traceEvent :: String -> a -> a
traceEvent msg expr = unsafeDupablePerformIO $ do
    traceEventIO msg
    return expr

-- | The 'traceEventIO' function emits a message to the eventlog, if eventlog
-- tracing is available and user event tracing is enabled at runtime.
--
-- Compared to 'traceEvent', 'traceEventIO' sequences the event with respect to
-- other IO actions.
--
-- @since base-4.5.0.0
traceEventIO :: String -> IO ()
{-# INLINE traceEventIO #-}
traceEventIO msg = do
  enabled <- userEventTracingEnabled
  when enabled $
    Enc.withCString utf8 msg $ \(Ptr p) -> IO $ \s ->
      case traceEvent# p s of s' -> (# s', () #)

-- | Like 'traceEvent', but emits the result of calling a function on its
-- argument.
--
-- @since base-4.18.0.0
traceEventWith :: (a -> String) -> a -> a
traceEventWith f a = traceEvent (f a) a

{-# NOINLINE traceMarker #-}
-- | The 'traceMarker' function emits a marker to the eventlog, if eventlog
-- tracing is available and enabled at runtime. The @String@ is the name of
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
-- @since base-4.7.0.0
traceMarker :: String -> a -> a
traceMarker msg expr = unsafeDupablePerformIO $ do
    traceMarkerIO msg
    return expr

-- | The 'traceMarkerIO' function emits a marker to the eventlog, if eventlog
-- tracing is available and user event tracing is enabled at runtime.
--
-- Compared to 'traceMarker', 'traceMarkerIO' sequences the event with respect to
-- other IO actions.
--
-- @since base-4.7.0.0
traceMarkerIO :: String -> IO ()
{-# INLINE traceMarkerIO #-}
traceMarkerIO msg = do
  enabled <- userEventTracingEnabled
  when enabled $
    Enc.withCString utf8 msg $ \(Ptr p) -> IO $ \s ->
      case traceMarker# p s of s' -> (# s', () #)

-- | Immediately flush the event log, if enabled.
--
-- @since base-4.15.0.0
flushEventLog :: IO ()
flushEventLog = c_flushEventLog nullPtr

foreign import ccall "flushEventLog" c_flushEventLog :: Ptr () -> IO ()

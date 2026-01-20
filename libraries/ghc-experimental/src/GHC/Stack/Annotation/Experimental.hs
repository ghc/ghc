{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ImplicitParams #-}
-- |
-- Module      : GHC.Stack.Annotation.Experimental
-- Description : Push annotation stack frames onto the Haskell call stack
-- Copyright   : (c) The GHC Team
-- License     : see libraries/ghc-experimental/LICENSE
-- Maintainer  : ghc-devs@haskell.org
-- Stability   : experimental
-- Portability : non-portable (GHC Extensions)
--
-- Push user-defined annotation stack frames into the Haskell call stack.
-- Annotation stack frames may be decoded when unwinding the call stack, allowing
-- the user to gain more control over what an IPE stack trace looks like.
--
-- The main advantages of stack frame annotations over other 'Backtraces':
--
-- * Function signatures don't need to be modified to improve stack traces (e.g. via 'HasCallStack').
-- * Annotation are arbitrary user-defined datatypes, not just source locations.
-- * Stack frame annotations are always present and do not require recompilation (e.g. @-prof@ or @-g3@).
module GHC.Stack.Annotation.Experimental (
  -- * The root of Stack Annotation Types
  SomeStackAnnotation(..),
  -- * Displaying Stack Annotations
  StackAnnotation(..),
  -- * Annotation helpers
  ShowAnnotation(..),
  StringAnnotation(..),
  -- * 'CallStack' annotations
  CallStackAnnotation(..),
  -- * Push stack frame annotations in 'IO' code.
  --
  --
  annotateStackIO,
  annotateStackStringIO,
  annotateStackShowIO,
  annotateCallStackIO,
  -- * Push stack frame annotations in non-'IO' code.
  --
  -- | These variants all evaluate the code to be annotated to WHNF.
  -- Otherwise, the stack annotations will not be shown in stack traces,
  -- as the computation is immediately "evaluated" to a thunk, popping the
  -- annotation frames from the stack.
  -- If the pure computation throws an exception later, the annotation frame
  -- will not be present, thus missing in the stack trace.
  --
  -- Note, you will encounter similar issues if the exception is thrown
  -- during evaluation of a nested value, for example @Just (error "Oh, no!")@.
  annotateStack,
  annotateStackString,
  annotateStackShow,
  annotateCallStack,
  ) where

import Data.Typeable
import GHC.Exts
import GHC.IO
import GHC.Internal.Stack
import GHC.Internal.Stack.Annotation

-- Note [User-defined stack annotations for better stack traces]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The primop 'annotateStack#' allows users to push arbitrary data onto
-- the Haskell-native call stack.
-- These annotations can later be discovered when unwinding and decoding the stack, for
-- example when an exception is thrown.
-- The annotations can add information to the call stack, such as source locations,
-- without needing 'HasCallStack' constraints in the calling function.
--
-- The feature is implemented via the stack frame 'AnnFrame', which consists of
-- nothing but an info table and a generic payload.
-- The 'AnnFrame' is semantically a no-op, and serves no further purpose than to
-- push user-defined annotations onto the Haskell-native call stack.
--
-- We provide a wrapper API for the primop 'annotateStack#' which allows users to annotate their
-- call stack in programs.
-- There are wrappers using 'IO', as well as wrappers that are pure.
-- Annotation stack frames are most reliable in the 'IO' monad, while
-- the pure variations can behave in ways that are hard to predict.
--
-- See Note [Stack annotations in pure code] for more details.

-- Note [Stack annotations in pure code]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- In lazy, non-IO code, the execution stack is quite confusing due to laziness
-- and doesn't follow any obvious intuition.
-- To make the pure API slightly more predictable, we evaluate the annotated value to WHNF.
-- This makes sure that stack annotations are present when we would intuitively expect them.
--
-- For example:
--
-- @
--  annotateStackShow (5 @Int) (fib 20 + error "Oh no!")
-- @
--
-- Without forcing the result of @(fib 20 + error "Oh no!")@, the computation
-- will simply return a thunk, and the stack annotation would be popped off the stack.
-- Once the thunk is evaluated, the exception is raised, but no stack annotation will be found!
-- If we force the result of @(fib 20 + error "Oh no!")@, then the stack
-- annotations remain on the stack, and are displayed in the stack trace.
--
-- Naturally, this only holds if no imprecise exceptions are thrown during evaluation of any
-- nested value, for example in 'annotateStackShow 5 (Just $ throw (ErrorCall "Oh no!"))', the
-- stack trace will not include the value @5@.
--

-- ----------------------------------------------------------------------------
-- Annotations
-- ----------------------------------------------------------------------------


-- | A 'String' only annotation with an optional source location.
data StringAnnotation where
  StringAnnotation :: !(Maybe SrcLoc) -> String -> StringAnnotation

instance StackAnnotation StringAnnotation where
  displayStackAnnotationShort (StringAnnotation _srcLoc str) =
    str

  stackAnnotationSourceLocation (StringAnnotation srcLoc _str) =
    srcLoc

-- | Use the 'Show' instance of a type to display as the 'StackAnnotation'.
data ShowAnnotation where
  ShowAnnotation :: forall a . Show a => !(Maybe SrcLoc) -> a -> ShowAnnotation

instance StackAnnotation ShowAnnotation where
  displayStackAnnotationShort (ShowAnnotation _srcLoc showAnno) =
    show showAnno

  stackAnnotationSourceLocation (ShowAnnotation srcLoc _showAnno) =
    srcLoc

-- | A 'CallStack' stack annotation.
--
-- Captures the whole 'CallStack'.
newtype CallStackAnnotation = CallStackAnnotation CallStack

instance Show CallStackAnnotation where
  show (CallStackAnnotation cs) = prettyCallStack cs

-- | Displays the first entry of the 'CallStack'
instance StackAnnotation CallStackAnnotation where
  stackAnnotationSourceLocation (CallStackAnnotation cs) =
    callStackHeadSrcLoc cs

  displayStackAnnotationShort (CallStackAnnotation cs) =
    callStackHeadFunctionName cs

callStackHeadSrcLoc :: CallStack -> Maybe SrcLoc
callStackHeadSrcLoc cs =
  case getCallStack cs of
    [] -> Nothing
    (_, srcLoc):_ -> Just srcLoc

callStackHeadFunctionName :: CallStack -> String
callStackHeadFunctionName cs =
  case getCallStack cs of
    [] -> "<unknown source location>"
    (fnName, _):_ -> fnName

-- ----------------------------------------------------------------------------
-- Annotate the CallStack with custom data
-- ----------------------------------------------------------------------------

-- See Note [User-defined stack annotations for better stack traces]

-- | @'annotateStack' anno b@ annotates the evaluation stack of @b@
-- with the value of @anno@.
--
-- When decoding the call stack, the annotation frames can be used to add more
-- information to stack traces.
--
-- WARNING: forces the evaluation of @b@ to WHNF.
{-# NOINLINE annotateStack #-}
annotateStack :: forall a b. (HasCallStack, Typeable a, StackAnnotation a) => a -> b -> b
annotateStack ann b = unsafePerformIO $
  annotateStackIO ann (evaluate b)

-- | @'annotateCallStack' b@ annotates the evaluation stack of @b@
-- with the current 'callstack'.
--
-- When decoding the call stack, the annotation frames can be used to add more
-- information to stack traces.
--
-- WARNING: forces the evaluation of @b@ to WHNF.
{-# NOINLINE annotateCallStack #-}
annotateCallStack :: HasCallStack => b -> b
annotateCallStack b = unsafePerformIO $ withFrozenCallStack $
  annotateCallStackIO (evaluate b)


-- | @'annotateStackString' msg b@ annotates the evaluation stack of @b@
-- with the value @msg@.
--
-- When decoding the call stack, the annotation frames can be used to add more
-- information to stack traces.
--
-- WARNING: forces the evaluation of @b@ to WHNF.
annotateStackString :: forall b . HasCallStack => String -> b -> b
annotateStackString ann =
  annotateStack (StringAnnotation (callStackHeadSrcLoc ?callStack) ann)

-- | @'annotateStackShow' showable b@ annotates the evaluation stack of @b@
-- with the value @showable@.
--
-- When decoding the call stack, the annotation frames can be used to add more
-- information to stack traces.
--
-- WARNING: forces the evaluation of @b@ to WHNF.
annotateStackShow :: forall a b . (HasCallStack, Typeable a, Show a) => a -> b -> b
annotateStackShow ann =
  annotateStack (ShowAnnotation (callStackHeadSrcLoc ?callStack) ann)

-- | @'annotateStackIO' showable b@ annotates the evaluation stack of @b@
-- with the value @showable@.
--
-- When decoding the call stack, the annotation frames can be used to add more
-- information to stack traces.
annotateStackIO :: forall a b . (HasCallStack, Typeable a, StackAnnotation a) => a -> IO b -> IO b
annotateStackIO ann (IO act) =
  IO $ \s -> annotateStack# (SomeStackAnnotation ann) act s

-- | @'annotateStackStringIO' msg b@ annotates the evaluation stack of @b@
-- with the value @msg@.
--
-- When decoding the call stack, the annotation frames can be used to add more
-- information to stack traces.
annotateStackStringIO :: forall b . HasCallStack => String -> IO b -> IO b
annotateStackStringIO ann =
  annotateStackIO (StringAnnotation (callStackHeadSrcLoc ?callStack) ann)

-- | @'annotateStackShowIO' msg b@ annotates the evaluation stack of @b@
-- with the value @msg@.
--
-- When decoding the call stack, the annotation frames can be used to add more
-- information to stack traces.
annotateStackShowIO :: forall a b . (HasCallStack, Show a) => a -> IO b -> IO b
annotateStackShowIO ann =
  annotateStackIO (ShowAnnotation (callStackHeadSrcLoc ?callStack) ann)

-- | @'annotateCallStackIO' b@ annotates the evaluation stack of @b@ with the
-- current 'callstack'.
--
-- When decoding the call stack, the annotation frames can be used to add more
-- information to stack traces.
annotateCallStackIO :: HasCallStack => IO a -> IO a
annotateCallStackIO =
  annotateStackIO (CallStackAnnotation ?callStack)

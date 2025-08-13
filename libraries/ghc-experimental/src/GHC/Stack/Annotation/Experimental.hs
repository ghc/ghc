{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ImplicitParams #-}
module GHC.Stack.Annotation.Experimental (
  StackAnnotation(..),
  SomeStackAnnotation(..),
  -- * Annotation helpers
  ShowAnnotation(..),
  StringAnnotation(..),
  -- * Source Location annotations
  SrcLocAnnotation,
  UnknownSrcLocAnnotation,
  -- * Stack annotations
  annotateStack,
  annotateStackString,
  annotateStackShow,
  annotateCallStack,
  annotateStackIO,
  annotateStackStringIO,
  annotateStackShowIO,
  annotateCallStackIO,
  ) where

import Data.Typeable
import GHC.Exts
import GHC.IO
import GHC.Internal.Stack

-- ----------------------------------------------------------------------------
-- StackAnnotation
-- ----------------------------------------------------------------------------

-- | 'StackAnnotation's are types which can be pushed onto the call stack
-- as the payload of 'AnnFrame' stack frames.
--
class StackAnnotation a where
  displayStackAnnotation :: a -> String

-- ----------------------------------------------------------------------------
-- Annotations
-- ----------------------------------------------------------------------------

-- |
-- The @SomeStackAnnotation@ type is the root of the stack annotation type hierarchy.
-- When the call stack is annotated with a value of type @a@, behind the scenes it is
-- encapsulated in a @SomeStackAnnotation@.
--
data SomeStackAnnotation where
  SomeStackAnnotation :: forall a. (Typeable a, StackAnnotation a) => a -> SomeStackAnnotation

instance StackAnnotation SomeStackAnnotation where
  displayStackAnnotation (SomeStackAnnotation a) = displayStackAnnotation a

data StringAnnotation where
  StringAnnotation :: String -> StringAnnotation

instance StackAnnotation StringAnnotation where
  displayStackAnnotation (StringAnnotation str) = str

data ShowAnnotation where
  ShowAnnotation :: forall a . Show a => a -> ShowAnnotation

instance StackAnnotation ShowAnnotation where
  displayStackAnnotation (ShowAnnotation showAnno) = show showAnno

-- | Wrapper around 'SrcLoc', to push them as the payload for annotation frames.
newtype SrcLocAnnotation = SrcLocAnnotation SrcLoc

data UnknownSrcLocAnnotation = UnknownSrcLocAnnotation
  deriving Show

instance Show SrcLocAnnotation where
  show (SrcLocAnnotation l) = prettySrcLoc l

instance StackAnnotation SrcLocAnnotation where
  displayStackAnnotation (SrcLocAnnotation l) = prettySrcLoc l

instance StackAnnotation UnknownSrcLocAnnotation where
  displayStackAnnotation UnknownSrcLocAnnotation = "<no location info>"

-- ----------------------------------------------------------------------------
-- Annotate the CallStack with custom data
-- ----------------------------------------------------------------------------

-- | @'annotateStack' anno b@ annotates the evaluation stack of @b@
-- with the value of @anno@.
--
-- When decoding the call stack, the annotation frames can be used to add more
-- information to stack traces.
--
-- WARNING: forces the evaluation of @b@.
{-# NOINLINE annotateStack #-}
annotateStack :: forall a b. (Typeable a, StackAnnotation a) => a -> b -> b
annotateStack ann b = unsafePerformIO $
  annotateStackIO ann (evaluate b)

-- | @'annotateCallStack' b@ annotates the evaluation stack of @b@
-- with the current 'callstack'.
--
-- When decoding the call stack, the annotation frames can be used to add more
-- information to stack traces.
--
-- WARNING: forces the evaluation of @b@.
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
-- WARNING: forces the evaluation of @b@.
annotateStackString :: forall b . String -> b -> b
annotateStackString ann =
  annotateStack (StringAnnotation ann)

-- | @'annotateStackShow' showable b@ annotates the evaluation stack of @b@
-- with the value @showable@.
--
-- When decoding the call stack, the annotation frames can be used to add more
-- information to stack traces.
--
-- WARNING: forces the evaluation of @b@.
annotateStackShow :: forall a b . (Typeable a, Show a) => a -> b -> b
annotateStackShow ann =
  annotateStack (ShowAnnotation ann)

-- | @'annotateStackIO' showable b@ annotates the evaluation stack of @b@
-- with the value @showable@.
--
-- When decoding the call stack, the annotation frames can be used to add more
-- information to stack traces.
annotateStackIO :: forall a b . (Typeable a, StackAnnotation a) => a -> IO b -> IO b
annotateStackIO ann (IO act) =
  IO $ \s -> annotateStack# (SomeStackAnnotation ann) act s

-- | @'annotateStackStringIO' msg b@ annotates the evaluation stack of @b@
-- with the value @msg@.
--
-- When decoding the call stack, the annotation frames can be used to add more
-- information to stack traces.
annotateStackStringIO :: forall b . String -> IO b -> IO b
annotateStackStringIO ann =
  annotateStackIO (StringAnnotation ann)

-- | @'annotateStackShowIO' msg b@ annotates the evaluation stack of @b@
-- with the value @msg@.
--
-- When decoding the call stack, the annotation frames can be used to add more
-- information to stack traces.
annotateStackShowIO :: forall a b . (Show a) => a -> IO b -> IO b
annotateStackShowIO ann =
  annotateStackIO (ShowAnnotation ann)

-- | @'annotateCallStackIO' b@ annotates the evaluation stack of @b@ with the
-- current 'callstack'.
--
-- When decoding the call stack, the annotation frames can be used to add more
-- information to stack traces.
annotateCallStackIO :: HasCallStack => IO a -> IO a
annotateCallStackIO act =
  let
    cs = getCallStack ?callStack
  in case cs of
    [] -> annotateStackIO UnknownSrcLocAnnotation act
    (_, srcLoc):_ -> annotateStackIO (SrcLocAnnotation srcLoc) act

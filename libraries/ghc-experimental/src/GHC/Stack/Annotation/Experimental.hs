{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ImplicitParams #-}
module GHC.Stack.Annotation.Experimental (
  StackAnnotation(..),
  SomeStackAnnotation(..),
  -- * Source Location annotations
  SrcLocAnnotation,
  UnknownSrcLocAnnotation,
  -- * Stack annotations
  annotateStack,
  annotateShow,
  annotateCallStack,
  annotateStackM,
  annotateStringM,
  annotateStackShowM,
  annotateCallStackM,
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

-- ----------------------------------------------------------------------------
-- Source location annotations
-- ----------------------------------------------------------------------------

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
-- Annotate the CallStack!
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
  annotateStackM ann (evaluate b)

-- | @'annotateCallStack' b@ annotates the evaluation stack of @b@
-- with the 'callstack'.
--
-- When decoding the call stack, the annotation frames can be used to add more
-- information to stack traces.
--
-- WARNING: forces the evaluation of @b@.
{-# NOINLINE annotateCallStack #-}
annotateCallStack :: HasCallStack => b -> b
annotateCallStack b = unsafePerformIO $
  annotateCallStackM (evaluate b)

-- | @'annotateShow' showable b@ annotates the evaluation stack of @b@
-- with the value @showable@.
--
-- When decoding the call stack, the annotation frames can be used to add more
-- information to stack traces.
--
-- WARNING: forces the evaluation of @b@.
annotateShow :: forall a b . (Typeable a, Show a) => a -> b -> b
annotateShow ann =
  annotateStack (StringAnnotation $ show ann)

-- | @'annotateStackM' showable b@ annotates the evaluation stack of @b@
-- with the value @showable@.
--
-- When decoding the call stack, the annotation frames can be used to add more
-- information to stack traces.
annotateStackM :: forall a b . (Typeable a, StackAnnotation a) => a -> IO b -> IO b
annotateStackM ann (IO act) =
  IO $ \s -> annotateStack# (SomeStackAnnotation ann) act s

-- | @'annotateStringM' msg b@ annotates the evaluation stack of @b@
-- with the value @msg@.
--
-- When decoding the call stack, the annotation frames can be used to add more
-- information to stack traces.
annotateStringM :: forall b . String -> IO b -> IO b
annotateStringM ann =
  annotateStackM (StringAnnotation ann)

-- | @'annotateStackShowM' msg b@ annotates the evaluation stack of @b@
-- with the value @msg@.
--
-- When decoding the call stack, the annotation frames can be used to add more
-- information to stack traces.
annotateStackShowM :: forall a b . (Typeable a, Show a) => a -> IO b -> IO b
annotateStackShowM ann =
  annotateStringM (show ann)

-- | @'annotateStackShowM' msg b@ annotates the evaluation stack of @b@
-- with the value @msg@.
--
-- When the call-site of 'annotateCallStackM' has a 'HasCallStack' constraint,
-- the source location of the calling function is used.
-- Otherwise, the call-site source location of 'annotateCallStackM' is used
-- as the annotation.
--
-- When decoding the call stack, the annotation frames can be used to add more
-- information to stack traces.
annotateCallStackM :: HasCallStack => IO a -> IO a
annotateCallStackM act =
  let
    cs = getCallStack ?callStack
  in case cs of
    [] -> annotateStackM UnknownSrcLocAnnotation act
    [(_, srcLoc)] -> annotateStackM (SrcLocAnnotation srcLoc) act
    (_:(_, srcLoc):_) -> annotateStackM (SrcLocAnnotation srcLoc) act

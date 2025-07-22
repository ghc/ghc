{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ImplicitParams #-}
module GHC.Stack.Annotation.Experimental (
  IsStackAnnotation(..),
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
import GHC.Internal.Stack.Annotation

data StringAnnotation where
  StringAnnotation :: String -> StringAnnotation

instance IsStackAnnotation StringAnnotation where
  displayStackAnnotation (StringAnnotation str) = str

-- ----------------------------------------------------------------------------
-- Source location annotations
-- ----------------------------------------------------------------------------

newtype SrcLocAnnotation = SrcLocAnnotation SrcLoc

data UnknownSrcLocAnnotation = UnknownSrcLocAnnotation
  deriving Show

instance Show SrcLocAnnotation where
  show (SrcLocAnnotation l) = prettySrcLoc l

instance IsStackAnnotation SrcLocAnnotation where
  displayStackAnnotation = show

instance IsStackAnnotation UnknownSrcLocAnnotation where
  displayStackAnnotation UnknownSrcLocAnnotation = "<no location info>"

-- ----------------------------------------------------------------------------
-- Annotate the CallStack!
-- ----------------------------------------------------------------------------

{-# NOINLINE annotateStack #-}
-- TODO @fendor: it seems the pure interface doesnt work,
-- investigate more and then decide what to do
annotateStack :: forall a b. (Typeable a, IsStackAnnotation a) => a -> b -> b
annotateStack ann b = unsafePerformIO $
  annotateStackM ann (pure b)

{-# NOINLINE annotateCallStack #-}
-- TODO @fendor: it seems the pure interface doesnt work,
-- investigate more and then decide what to do
annotateCallStack :: HasCallStack => b -> b
annotateCallStack b = unsafePerformIO $
  annotateCallStackM (pure b)

-- TODO @fendor: it seems the pure interface doesnt work,
-- investigate more and then decide what to do
annotateShow :: forall a b . (Typeable a, Show a) => a -> b -> b
annotateShow ann =
  annotateStack (StringAnnotation $ show ann)

annotateStackM :: forall a b . (Typeable a, IsStackAnnotation a) => a -> IO b -> IO b
annotateStackM ann (IO act) =
  IO $ \s -> annotateStack# (SomeStackAnnotation ann) act s

annotateStringM :: forall b . String -> IO b -> IO b
annotateStringM ann =
  annotateStackM (StringAnnotation ann)

annotateStackShowM :: forall a b . (Typeable a, Show a) => a -> IO b -> IO b
annotateStackShowM ann =
  annotateStringM (show ann)

annotateCallStackM :: HasCallStack => IO a -> IO a
annotateCallStackM act =
  let
    cs = getCallStack ?callStack
  in case cs of
    [] -> annotateStackM UnknownSrcLocAnnotation act
    [(_, srcLoc)] -> annotateStackM (SrcLocAnnotation srcLoc) act
    (_:(_, srcLoc):_) -> annotateStackM (SrcLocAnnotation srcLoc) act

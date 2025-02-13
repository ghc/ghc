{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ImplicitParams #-}
module GHC.Stack.Annotation.Experimental where

import Data.Typeable
import GHC.Exts
import GHC.IO
import GHC.Internal.Stack.Types

data StackAnnotation where
  StackAnnotation :: forall a. (Typeable a, Show a) => a -> StackAnnotation

class IsStackAnnotation a where
  display :: a -> String

instance IsStackAnnotation StackAnnotation where
  display (StackAnnotation a) = show a

newtype SrcLocAnno = MkSrcLocAnno SrcLoc

data UnknownSrcLocAnno = UnknownSrcLocAnno
  deriving Show

instance Show SrcLocAnno where
  show (MkSrcLocAnno l) =
    concat
      [ srcLocPackage l
      , ":"
      , srcLocModule l
      , " "
      , srcLocFile l
      , ":"
      , show $ srcLocStartLine l
      , "-"
      , show $ srcLocStartCol l
      , ":"
      , show $ srcLocEndLine l
      , "-"
      , show $ srcLocEndCol l
      ]

instance IsStackAnnotation SrcLocAnno where
  display = show

instance IsStackAnnotation UnknownSrcLocAnno where
  display UnknownSrcLocAnno = "UnknownSrcLocAnno"

{-# NOINLINE annotateStack #-}
annotateStack :: forall a b. (Typeable a, Show a) => a -> b -> b
annotateStack ann b = unsafePerformIO $
  annotateStackM ann (pure b)

annotateStackM :: forall a b . (Typeable a, Show a) => a -> IO b -> IO b
annotateStackM ann (IO act) =
  IO $ \s -> annotateStack# (StackAnnotation ann) act s

annotateCallStackM :: HasCallStack => IO a -> IO a
annotateCallStackM act =
  let
    cs = getCallStack ?callStack
  in case cs of
    [] -> annotateStackM UnknownSrcLocAnno act
    [(_, srcLoc)] -> annotateStackM (MkSrcLocAnno srcLoc) act
    (_:(_, srcLoc):_) -> annotateStackM (MkSrcLocAnno srcLoc) act

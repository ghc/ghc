{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ImplicitParams #-}
module GHC.Stack.Annotation.Experimental where

import Data.Typeable
import GHC.Exts
import GHC.Internal.Stack.Types

data StackAnnotation where
  StackAnnotation :: forall a. (Typeable a, Show a) => a -> StackAnnotation

class IsStackAnnotation a where
  display :: a -> String

newtype SrcLocAnno = MkSrcLocAnno SrcLoc

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

annotateStackWith :: forall a b. (Typeable a, Show a) => a -> b -> b
annotateStackWith ann =
  annotateStack# (StackAnnotation ann)

annotateCallStack :: HasCallStack => a -> a
annotateCallStack =
  let
    cs = getCallStack ?callStack
  in case cs of
    [] -> id
    ((_, srcLoc):_) -> annotateStackWith (MkSrcLocAnno srcLoc)

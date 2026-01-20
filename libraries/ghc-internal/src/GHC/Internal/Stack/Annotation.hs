{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GHC.Internal.Stack.Annotation where

import GHC.Internal.Base
import GHC.Internal.Data.Typeable
import GHC.Internal.Stack (SrcLoc, prettySrcLoc)

-- ----------------------------------------------------------------------------
-- StackAnnotation
-- ----------------------------------------------------------------------------

-- | 'StackAnnotation's are types which can be pushed onto the call stack
-- as the payload of 'AnnFrame' stack frames.
--
class StackAnnotation a where
  -- | Display a human readable string for the 'StackAnnotation'.
  --
  -- This is supposed to be the long version of 'displayStackAnnotationShort'
  -- and may contain a source location.
  --
  -- If not provided, 'displayStackAnnotation' is derived from 'stackAnnotationSourceLocation'
  -- and 'displayStackAnnotationShort'.
  displayStackAnnotation :: a -> String

  -- | Get the 'SrcLoc' of the given 'StackAnnotation'.
  --
  -- This is optional, 'SrcLoc' are not strictly required for 'StackAnnotation', but
  -- it is still heavily encouarged to provide a 'SrcLoc' for better IPE backtraces.
  stackAnnotationSourceLocation :: a -> Maybe SrcLoc

  -- | The description of the StackAnnotation without any metadata such as source locations.
  --
  -- Pefer implementing 'displayStackAnnotationShort' over 'displayStackAnnotation'.
  displayStackAnnotationShort :: a -> String

  {-# MINIMAL displayStackAnnotation | displayStackAnnotationShort #-}

  displayStackAnnotation ann =
    displayStackAnnotationShort ann
      ++ case stackAnnotationSourceLocation ann of
          Nothing -> ""
          Just srcLoc -> ", called at " ++ prettySrcLoc srcLoc

  stackAnnotationSourceLocation _ann = Nothing

  displayStackAnnotationShort = displayStackAnnotation

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
  displayStackAnnotation (SomeStackAnnotation a) =
    displayStackAnnotation a

  stackAnnotationSourceLocation (SomeStackAnnotation a) =
    stackAnnotationSourceLocation a

  displayStackAnnotationShort (SomeStackAnnotation a) =
    displayStackAnnotationShort a

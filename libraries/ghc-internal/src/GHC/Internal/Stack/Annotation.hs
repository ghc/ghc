{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GHC.Internal.Stack.Annotation where

import GHC.Internal.Base
import GHC.Internal.Data.Typeable

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

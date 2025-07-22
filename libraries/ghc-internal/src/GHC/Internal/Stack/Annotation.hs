{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GHC.Internal.Stack.Annotation (
  IsStackAnnotation(..),
  SomeStackAnnotation(..),
  )
  where

import GHC.Internal.Base
import GHC.Internal.Data.Typeable

-- ----------------------------------------------------------------------------
-- IsStackAnnotation
-- ----------------------------------------------------------------------------

class IsStackAnnotation a where
  displayStackAnnotation :: a -> String

-- ----------------------------------------------------------------------------
-- Annotations
-- ----------------------------------------------------------------------------

{- |
The @SomeStackAnnotation@ type is the root of the stack annotation type hierarchy.
When the call stack is annotated with a value of type @a@, behind the scenes it is
encapsulated in a @SomeStackAnnotation@.
-}
data SomeStackAnnotation where
  SomeStackAnnotation :: forall a. (Typeable a, IsStackAnnotation a) => a -> SomeStackAnnotation

instance IsStackAnnotation SomeStackAnnotation where
  displayStackAnnotation (SomeStackAnnotation a) = displayStackAnnotation a

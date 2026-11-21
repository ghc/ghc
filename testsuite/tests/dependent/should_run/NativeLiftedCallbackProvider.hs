{-# LANGUAGE ExperimentalDependentTypes, GADTs, Modifiers #-}
module NativeLiftedCallbackProvider (Decision(..), decide) where

import Data.Kind (Type)

data Decision (b :: Bool) where
  IsTrue :: Decision True
  IsFalse :: Decision False

-- | Return a witness for the result of this callback on this argument.
decide :: forall (a :: Type). foreach (f :: a -> Bool)
       -> foreach (x :: a) -> Decision (f x)
decide f x = case f x of
  True -> IsTrue
  False -> IsFalse

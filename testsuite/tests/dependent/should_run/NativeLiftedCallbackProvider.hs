{-# LANGUAGE ExperimentalDependentTypes, GADTs, Modifiers #-}
module NativeLiftedCallbackProvider (Decision(..), decide) where

data Decision (b :: Bool) where
  IsTrue :: Decision True
  IsFalse :: Decision False
deriving instance Show (Decision b)

-- | Return a witness for the result of this callback on this argument.
decide :: forall a. foreach (f :: a -> Bool)
       -> foreach (x :: a) -> Decision (f x)
decide f x = case f x of
  True -> IsTrue
  False -> IsFalse

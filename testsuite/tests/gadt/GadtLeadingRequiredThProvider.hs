{-# LANGUAGE GADTs, RequiredTypeArguments #-}
module GadtLeadingRequiredThProvider where

data T where
  C :: forall a -> Eq a => forall {b} c. Show c => a -> !(b, c) -> T

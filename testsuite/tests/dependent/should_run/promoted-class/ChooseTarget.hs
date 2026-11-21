{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, ScopedTypeVariables #-}
module ChooseTarget where

class Choose a where
  choose :: a -> Bool

data Tagged (b :: Bool) where
  TaggedTrue :: Tagged 'True
  TaggedFalse :: Tagged 'False

tagBool :: foreach (b :: Bool) -> Tagged b
tagBool True = TaggedTrue
tagBool False = TaggedFalse

chooseTagged :: forall a. Choose a => foreach (x :: a) -> Tagged (choose x)
chooseTagged x = tagBool (choose x)

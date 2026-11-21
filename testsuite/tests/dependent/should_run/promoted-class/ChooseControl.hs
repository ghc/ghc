{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, ScopedTypeVariables, TypeFamilies #-}
module ChooseControl where

data Tagged (b :: Bool) where
  TaggedTrue :: Tagged 'True
  TaggedFalse :: Tagged 'False

tagBool :: foreach (b :: Bool) -> Tagged b
tagBool True = TaggedTrue
tagBool False = TaggedFalse

class Choose a where
  type Chosen a (x :: a) :: Bool
  chooseIndexed :: foreach (x :: a) -> Tagged (Chosen a x)

instance Choose Bool where
  type Chosen Bool x = x
  chooseIndexed x = tagBool x

chooseFamily :: forall a. Choose a => foreach (x :: a) -> Tagged (Chosen a x)
chooseFamily x = chooseIndexed x

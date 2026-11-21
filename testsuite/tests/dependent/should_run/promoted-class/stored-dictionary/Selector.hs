{-# LANGUAGE DataKinds, ExperimentalDependentTypes, RoleAnnotations #-}
module Selector (Tagged(..), tag, observe) where
import StoredEvidence (Pick(..))

type role Tagged nominal
data Tagged (b :: Bool) = Tagged Bool

tag :: foreach (b :: Bool) -> Tagged b
tag b = Tagged @b b

observe :: forall a. Pick a => foreach (x :: a) -> Tagged (pick x)
observe x = tag (pick x)

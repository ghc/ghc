{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeTypeCaseInvalid where

type Test = Bool
type F a = case a of
  Test -> Int

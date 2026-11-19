{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeTypeCaseScrutinee where

type F = case Missing of
  True -> Int
  False -> Bool

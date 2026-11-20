{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeTypeCaseBadPattern where

type Bad a = case a of
  Just 0 -> Bool

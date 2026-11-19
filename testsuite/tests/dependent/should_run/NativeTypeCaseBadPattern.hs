{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
module NativeTypeCaseBadPattern where

type Bad a = case a of
  Just Int -> Bool

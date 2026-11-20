{-# LANGUAGE DataKinds, ExperimentalDependentTypes, RankNTypes #-}
module NativeTypeCaseForallResult where

type ForallResult = () -> (forall (b :: Bool). case b of
  True -> Int
  False -> Char)

{-# LANGUAGE DataKinds, ExperimentalDependentTypes, RankNTypes #-}
module NativeTypeCaseForallResult where

type ForallResult (b :: Bool) = () -> (forall (b :: Bool). case b of
  True -> Int
  False -> Char)

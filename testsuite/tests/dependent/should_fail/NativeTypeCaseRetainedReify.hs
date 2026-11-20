{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TemplateHaskell #-}
module NativeTypeCaseRetainedReify where

import Language.Haskell.TH (reify)

type Select (b :: Bool) = case b of { True -> Int; False -> Char }

data Stored where
  Stored :: foreach (b :: Bool) -> Select b -> Stored

$([] <$ reify ''Stored)

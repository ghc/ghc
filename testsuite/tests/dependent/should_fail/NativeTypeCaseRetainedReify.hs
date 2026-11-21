{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, RequiredTypeArguments, TemplateHaskell #-}
module NativeTypeCaseRetainedReify where

import Language.Haskell.TH (reify)

data Stored a where
  Stored :: forall a -> foreach (b :: Bool) -> Int -> Stored a

$([] <$ reify ''Stored)

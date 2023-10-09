{-# LANGUAGE LambdaCase #-}

module GHC.Types.Stability where

import GHC.Utils.Binary

import GHC.Prelude

data StabilityMode
   = StabilityDefault
   | StabilityRestricted ExperimentalLevel
   | StabilityNonstable  ExperimentalLevel
   deriving (Eq, Ord)

data ExperimentalLevel
   = StabilityExperimental
  deriving (Enum, Eq, Ord)

instance Binary StabilityMode where
    put_ bh = \case
      StabilityDefault                          -> putByte bh 0
      StabilityRestricted StabilityExperimental -> putByte bh 1
      StabilityNonstable  StabilityExperimental -> putByte bh 2
    get bh = getByte bh >>= return . \case
      0 -> StabilityDefault
      1 -> StabilityRestricted StabilityExperimental
      2 -> StabilityNonstable  StabilityExperimental
module GHC.HsToCore.PmCheck.Types where

import GHC.Data.Bag

data Delta

newtype Deltas = MkDeltas (Bag Delta)

initDeltas :: Deltas

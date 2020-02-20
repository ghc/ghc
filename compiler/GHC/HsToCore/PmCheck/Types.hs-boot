module GHC.HsToCore.PmCheck.Types where

import Bag

data Delta

newtype Deltas = MkDeltas (Bag Delta)

initDeltas :: Deltas

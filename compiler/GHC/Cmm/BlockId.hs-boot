module GHC.Cmm.BlockId (BlockId, mkBlockId) where

import GHC.Cmm.Dataflow.Label (Label)
import Unique (Unique)

type BlockId = Label

mkBlockId :: Unique -> BlockId

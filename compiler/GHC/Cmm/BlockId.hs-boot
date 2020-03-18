module GHC.Cmm.BlockId (BlockId, mkBlockId) where

import GHC.Cmm.Dataflow.Label (Label)
import GHC.Types.Unique (Unique)

type BlockId = Label

mkBlockId :: Unique -> BlockId

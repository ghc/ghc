module BlockId (BlockId, mkBlockId) where

import Hoopl.Label (Label)
import Unique (Unique)

type BlockId = Label

mkBlockId :: Unique -> BlockId

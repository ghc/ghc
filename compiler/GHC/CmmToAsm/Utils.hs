module GHC.CmmToAsm.Utils
   ( topInfoTable
   , entryBlocks
   )
where

import GHC.Prelude

import GHC.Cmm.BlockId
import qualified GHC.Cmm.Dataflow.Label.NonDet as NonDet
import GHC.Cmm hiding (topInfoTable)

-- | Returns the info table associated with the CmmDecl's entry point,
-- if any.
topInfoTable :: GenCmmDecl a (NonDet.LabelMap i) (ListGraph b) -> Maybe i
topInfoTable (CmmProc infos _ _ (ListGraph (b:_)))
  = NonDet.mapLookup (blockId b) infos
topInfoTable _
  = Nothing

-- | Return the list of BlockIds in a CmmDecl that are entry points
-- for this proc (i.e. they may be jumped to from outside this proc).
entryBlocks :: GenCmmDecl a (NonDet.LabelMap i) (ListGraph b) -> [BlockId]
entryBlocks (CmmProc info _ _ (ListGraph code)) = entries
  where
        infos = NonDet.nonDetMapKeys info
        entries = case code of
                    [] -> infos
                    BasicBlock entry _ : _ -- first block is the entry point
                       | entry `elem` infos -> infos
                       | otherwise          -> entry : infos
entryBlocks _ = []

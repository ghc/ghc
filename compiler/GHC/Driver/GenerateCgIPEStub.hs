{-# LANGUAGE GADTs #-}

module GHC.Driver.GenerateCgIPEStub (generateCgIPEStub) where

import Data.Maybe (catMaybes, listToMaybe)
import GHC.Cmm (CmmGraph, CmmGroupSRTs, CmmInfoTable (cit_rep), CmmNode (CmmCall, CmmTick), CmmTickish, CmmTopInfo (info_tbls), GenCmmDecl (CmmProc), RawCmmStatics)
import GHC.Cmm.Dataflow (Block, C, O)
import GHC.Cmm.Dataflow.Block (blockSplit, blockToList)
import GHC.Cmm.Dataflow.Collections (mapToList)
import GHC.Cmm.Dataflow.Label (Label)
import GHC.Cmm.Info.Build (emptySRT)
import GHC.Cmm.Pipeline (cmmPipeline)
import GHC.Cmm.Utils (toBlockList)
import GHC.Data.Maybe (firstJusts)
import GHC.Data.Stream (Stream, liftIO)
import qualified GHC.Data.Stream as Stream
import GHC.Driver.Env (hsc_dflags)
import GHC.Driver.Flags (GeneralFlag (Opt_InfoTableMap))
import GHC.Driver.Session (gopt)
import GHC.Plugins (HscEnv, NonCaffySet)
import GHC.Prelude
import GHC.Runtime.Heap.Layout (isStackRep)
import GHC.StgToCmm.Monad (getCmm, initC, runC)
import GHC.StgToCmm.Prof (initInfoTableProv)
import GHC.StgToCmm.Types (CgInfos (..), ModuleLFInfos)
import GHC.Types.IPE (InfoTableProvMap (labeledInfoTablesWithTickishes))
import GHC.Unit.Types (Module)

-- TODO: Does this check flag to see if IPE is wanted?
generateCgIPEStub :: HscEnv -> Module -> InfoTableProvMap -> Stream IO CmmGroupSRTs (NonCaffySet, ModuleLFInfos) -> Stream IO CmmGroupSRTs CgInfos
generateCgIPEStub hsc_env this_mod denv s = do
  let dflags = hsc_dflags hsc_env
  cgState <- liftIO initC

  -- Collect info tables, but only if -finfo-table-map is enabled, otherwise labeledInfoTablesWithTickishes is empty.
  let collectFun = if gopt Opt_InfoTableMap dflags then collect else collectNothing
  (labeledInfoTablesWithTickishes, (nonCaffySet, moduleLFInfos)) <- Stream.mapAccumL_ collectFun [] s

  let denv' = denv {labeledInfoTablesWithTickishes = labeledInfoTablesWithTickishes}
      ((ipeStub, cmmGroup), _) = runC dflags this_mod cgState $ getCmm (initInfoTableProv (map sndOfTriple labeledInfoTablesWithTickishes) denv' this_mod)
  (_, cmmGroupSRTs) <- liftIO $ cmmPipeline hsc_env (emptySRT this_mod) cmmGroup
  Stream.yield cmmGroupSRTs
  return CgInfos {cgNonCafs = nonCaffySet, cgLFInfos = moduleLFInfos, cgIPEStub = ipeStub}
  where
    collect :: [(Label, CmmInfoTable, Maybe CmmTickish)] -> CmmGroupSRTs -> IO ([(Label, CmmInfoTable, Maybe CmmTickish)], CmmGroupSRTs)
    collect acc cmmGroupSRTs = do
      let labelsToInfoTables = collectInfoTables cmmGroupSRTs
          labelsToInfoTablesToTickishes = map (\(l, i) -> (l, i, lookupEstimatedTick cmmGroupSRTs l i)) labelsToInfoTables
      return (acc ++ labelsToInfoTablesToTickishes, cmmGroupSRTs)

    collectNothing :: [(Label, CmmInfoTable, Maybe CmmTickish)] -> CmmGroupSRTs -> IO ([(Label, CmmInfoTable, Maybe CmmTickish)], CmmGroupSRTs)
    collectNothing _ cmmGroupSRTs = pure ([], cmmGroupSRTs)

    sndOfTriple :: (a, b, c) -> b
    sndOfTriple (_, b, _) = b

    collectInfoTables :: CmmGroupSRTs -> [(Label, CmmInfoTable)]
    collectInfoTables cmmGroup = concat $ catMaybes $ map extractInfoTables cmmGroup

    -- All return frame info tables are stack represented, though not all stack represented info tables
    -- have to be return frames.
    extractInfoTables :: GenCmmDecl RawCmmStatics CmmTopInfo CmmGraph -> Maybe [(Label, CmmInfoTable)]
    extractInfoTables (CmmProc h _ _ _) = Just $ mapToList (info_tbls h)
    extractInfoTables _ = Nothing

    -- TODO: Write a note: [Looking up CmmTickish for return frame info tables]
    lookupEstimatedTick :: CmmGroupSRTs -> Label -> CmmInfoTable -> Maybe CmmTickish
    lookupEstimatedTick cmmGroup infoTableLabel infoTable = do
      if (isStackRep . cit_rep) infoTable
        then do
          let blocks = concatMap toBlockList (graphs cmmGroup)
          firstJusts $ map (findCmmTickishForLabelInBlock infoTableLabel) blocks
        else Nothing

    graphs :: CmmGroupSRTs -> [CmmGraph]
    graphs = foldl' go []
      where
        go :: [CmmGraph] -> GenCmmDecl d h CmmGraph -> [CmmGraph]
        go acc (CmmProc _ _ _ g) = g : acc
        go acc _ = acc

    findCmmTickishForLabelInBlock :: Label -> Block CmmNode C C -> Maybe CmmTickish
    findCmmTickishForLabelInBlock label block = do
      let (_, middleBlock, endBlock) = blockSplit block

      isCallWithReturnFrameLabel endBlock label
      lastTickInBlock middleBlock
      where
        isCallWithReturnFrameLabel :: CmmNode O C -> Label -> Maybe ()
        isCallWithReturnFrameLabel (CmmCall _ (Just l) _ _ _ _) clabel | l == clabel = Just ()
        isCallWithReturnFrameLabel _ _ = Nothing

        lastTickInBlock block =
          listToMaybe $
            catMaybes $
              map maybeTick $ (reverse . blockToList) block

        maybeTick :: CmmNode O O -> Maybe CmmTickish
        maybeTick (CmmTick t) = Just t
        maybeTick _ = Nothing

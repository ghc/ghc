{-# LANGUAGE BangPatterns #-}

--
-- (c) 2021 Benjamin Maurer
--

module GHC.CmmToAsm.SSA.Stats (
    SsaStats (..),

    countUniqueNames,

    pprSsaStats
) where

import GHC.Prelude

import GHC.Cmm (GenCmmDecl(..), GenBasicBlock(..))
import GHC.CmmToAsm.Reg.Liveness
import GHC.Platform.Reg (isVirtualReg)
import GHC.Types.Unique.Set
import GHC.Utils.Outputable

import Data.Graph (flattenSCCs)


data SsaStats
    = SsaStats
    {
        ssasUniqueNamesBefore :: !Int,
        ssasUniqueNamesAfter  :: !Int
    }


emptySsaStats :: SsaStats
emptySsaStats = SsaStats 0 0


addStats :: SsaStats -> SsaStats -> SsaStats
addStats (SsaStats x1 y1) (SsaStats x2 y2) = SsaStats (x1 + x2) (y1 + y2)


pprSsaStats :: [SsaStats] -> SDoc
pprSsaStats stats
 = let totals = foldl' addStats emptySsaStats stats
   in (  text "-- Unique vreg names"
      $$ text "--    (before-SSA, after-SSA)"
      $$ parens (hsep $ punctuate comma
            [ppr $ ssasUniqueNamesBefore totals, ppr $ ssasUniqueNamesAfter totals])
      $$ text "")


countUniqueNames :: LiveCmmDecl statics instr -> Int

countUniqueNames CmmData{} = 0

countUniqueNames (CmmProc _ _ _ sccs)
 = let
     fromInstr names (LiveInstr _ mLiveness)
        = maybe names (\l -> names `unionUniqSets`
            (filterUniqSet isVirtualReg $ liveBorn l)) mLiveness

     fromBlk names (BasicBlock _ ins)
        = foldl' fromInstr names ins

     uniqueNames
        = foldl' fromBlk emptyUniqSet $ flattenSCCs sccs
   in sizeUniqSet uniqueNames

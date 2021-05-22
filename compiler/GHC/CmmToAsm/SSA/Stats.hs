
--
-- (c) 2021 Benjamin Maurer
--

module GHC.CmmToAsm.SSA.Stats (
    SsaStats (..),

    mkPreSsaStats,
    mkSsaPhiNodeStats,
    mkPostSsaStats,

    pprSsaStats
) where

import GHC.Prelude

import GHC.Cmm (GenCmmDecl(..))
import GHC.CmmToAsm.Instr
import GHC.CmmToAsm.SSA
import GHC.CmmToAsm.Types
import GHC.Platform (Platform)
import GHC.Platform.Reg (isVirtualReg)
import GHC.Types.Unique.Set
import GHC.Utils.Outputable


data SsaStats
    = SsaStats
    {
        ssasUniqueNamesBefore :: !Int,  -- Unique vregs before SSA transformation
        ssasUniqueNamesAfter  :: !Int,  -- Unique vregs after out-of-SSA transformation
        ssasPhiCount          :: !Int,  -- Number of Phi nodes
        ssasPhiArgsMin        :: !Int,  -- Least phi arguments
        ssasPhiArgsMax        :: !Int,  -- Maximum number of phi arguments
        ssasPhiArgsAvg        :: !Int   -- Average number of phi arguments
    }


emptySsaStats :: SsaStats
emptySsaStats = SsaStats 0 0 0 0 0 0


addStats :: SsaStats -> SsaStats -> SsaStats
addStats (SsaStats bef1 aft1 cnt1 min1 max1 avg1)
         (SsaStats bef2 aft2 cnt2 min2 max2 avg2)
 = let cnt      = cnt1 + cnt2
       argSum   = (avg1 * cnt1) + (avg2 * cnt2)
       avg      = if cnt /= 0
                    then argSum `div` cnt
                    else 0
       min3     = case (min1, min2) of
                    (0, y)  -> y
                    (x, 0)  -> x
                    _       -> min min1 min2
   in   SsaStats (bef1 + bef2) (aft1 + aft2) cnt min3 (max max1 max2) avg


pprSsaStats :: [SsaStats] -> SDoc
pprSsaStats stats
 = let totals = foldl' addStats emptySsaStats stats
   in    text "-- Unique vreg names"
      $$ text "--    (before-SSA, after-SSA)"
      $$ parens (hsep $ punctuate comma
            [ppr $ ssasUniqueNamesBefore totals, ppr $ ssasUniqueNamesAfter totals])
      $$ text ""
      $$ text "-- SSA Phi nodes"
      $$ text "--    (count, min args, max args, avg args)"
      $$ parens (hsep $ punctuate comma
            [ppr $ ssasPhiCount totals,
             ppr $ ssasPhiArgsMin totals,
             ppr $ ssasPhiArgsMax totals,
             ppr $ ssasPhiArgsAvg totals])
      $$ text ""


mkPreSsaStats
    :: Instruction instr
    => Platform
    -> NatCmmDecl statics instr
    -> SsaStats

mkPreSsaStats platform cmmDecl
 = let cntBefore = countUniqueNames platform cmmDecl
   in  emptySsaStats { ssasUniqueNamesBefore = cntBefore }


mkSsaPhiNodeStats
    :: SsaCmmDecl statics instr
    -> SsaStats
    -> SsaStats

mkSsaPhiNodeStats ssaCmmDecl stats
 = let (min, max, avg, cnt) = gatherPhiStats ssaCmmDecl
   in  stats { ssasPhiCount = cnt,
               ssasPhiArgsMin = min,
               ssasPhiArgsMax = max,
               ssasPhiArgsAvg = avg }


mkPostSsaStats
    :: Instruction instr
    => Platform
    -> NatCmmDecl statics instr
    -> SsaStats
    -> SsaStats

mkPostSsaStats platform cmmDecl stats
 = let cntAfter = countUniqueNames platform cmmDecl
   in  stats { ssasUniqueNamesAfter = cntAfter }

countUniqueNames
    :: Instruction instr
    => Platform
    -> NatCmmDecl statics instr
    -> Int

countUniqueNames _ CmmData{} = 0

countUniqueNames _ (CmmProc _ _ _ (ListGraph [])) = 0

countUniqueNames platform (CmmProc _ _ _ (ListGraph blocks))
 = let
     fromInstr names instr
        = let RU rlRead rlWritten = regUsageOfInstr platform instr
              rsRead              = mkUniqSet $ filter isVirtualReg rlRead
              rsWritten           = mkUniqSet $ filter isVirtualReg rlWritten
          in  unionManyUniqSets [names, rsRead, rsWritten]

     fromBlk names (BasicBlock _ ins)
        = foldl' fromInstr names ins

     uniqueNames
        = foldl' fromBlk emptyUniqSet blocks

   in sizeUniqSet uniqueNames


gatherPhiStats
    :: SsaCmmDecl statics instr
    -> (Int, Int, Int, Int)

gatherPhiStats CmmData{} = (0, 0, 0, 0)

gatherPhiStats (CmmProc _ _ _ []) = (0, 0, 0, 0)

gatherPhiStats (CmmProc _ _ _ blks)
 = let
     handlePhis acc (SSABB phis _) = foldl' handlePhi acc phis

     handlePhi (min, max, total, cnt) (PhiF _ _ args)
        = let numArgs = length args
          in  ( if min < numArgs then min else numArgs,
                if max > numArgs then max else numArgs,
                total + numArgs,
                succ cnt )

     (min, max, total, cnt)
        = foldl' handlePhis (maxBound, minBound, 0, 0) blks

   in  if cnt /= 0
        then (min, max, total `div` cnt, cnt)
        else (0, 0, 0, 0)

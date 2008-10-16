
module CmmLiveZ
    ( CmmLive
    , cmmLivenessZ
    , liveLattice
    , middleLiveness, lastLiveness, noLiveOnEntry
    ) 
where

import BlockId
import CmmExpr
import CmmTx
import DFMonad
import Monad
import PprCmm()
import PprCmmZ()
import ZipCfg
import ZipDataflow
import ZipCfgCmmRep

import Maybes
import Outputable
import UniqSet

-----------------------------------------------------------------------------
-- Calculating what variables are live on entry to a basic block
-----------------------------------------------------------------------------

-- | The variables live on entry to a block
type CmmLive = RegSet

-- | The dataflow lattice
liveLattice :: DataflowLattice CmmLive
liveLattice = DataflowLattice "live LocalReg's" emptyUniqSet add False
    where add new old =
            let join = unionUniqSets new old in
            (if sizeUniqSet join > sizeUniqSet old then aTx else noTx) join

-- | A mapping from block labels to the variables live on entry
type BlockEntryLiveness = BlockEnv CmmLive

-----------------------------------------------------------------------------
-- | Calculated liveness info for a CmmGraph
-----------------------------------------------------------------------------
cmmLivenessZ :: CmmGraph -> FuelMonad BlockEntryLiveness
cmmLivenessZ g@(LGraph entry _ _) =
  liftM (check . zdfFpFacts) (res :: FuelMonad (CmmBackwardFixedPoint CmmLive))
  where res = zdfSolveFrom emptyBlockEnv "liveness analysis" liveLattice transfers
                           emptyUniqSet (graphOfLGraph g)
        transfers = BackwardTransfers first middle last
        first live _ = live
        middle       = flip middleLiveness
        last         = flip lastLiveness
        check facts  =
          noLiveOnEntry entry (expectJust "check" $ lookupBlockEnv facts entry) facts

-- | On entry to the procedure, there had better not be any LocalReg's live-in.
noLiveOnEntry :: BlockId -> CmmLive -> a -> a
noLiveOnEntry bid in_fact x =
  if isEmptyUniqSet in_fact then x
  else pprPanic "LocalReg's live-in to graph" (ppr bid <+> ppr in_fact)

-- | The transfer equations use the traditional 'gen' and 'kill'
-- notations, which should be familiar from the dragon book.
gen, kill :: UserOfLocalRegs a => a -> RegSet -> RegSet
gen  a live = foldRegsUsed extendRegSet      live a
kill a live = foldRegsUsed delOneFromUniqSet live a

-- Why aren't these function using the typeclasses on Middle and Last?
middleLiveness :: Middle -> CmmLive -> CmmLive
middleLiveness (MidComment {})            live = live
middleLiveness (MidAssign lhs expr)       live = gen expr $ kill lhs live
middleLiveness (MidStore addr rval)       live = gen addr $ gen rval live
middleLiveness (MidForeignCall _ tgt _ args) _ = gen tgt $ gen args emptyUniqSet

lastLiveness :: Last -> (BlockId -> CmmLive) -> CmmLive
lastLiveness l env = last l
  where last (LastBranch id)             = env id
        last (LastCall tgt Nothing  _ _) = gen tgt $ emptyUniqSet
        last (LastCall tgt (Just k) _ _) = gen tgt $ env k
        last (LastCondBranch e t f)      = gen e $ unionUniqSets (env t) (env f)
        last (LastSwitch e tbl)          =
          gen e $ unionManyUniqSets $ map env (catMaybes tbl)

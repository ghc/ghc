
module CmmLiveZ
    ( CmmLive
    , cmmLivenessZ
    , liveLattice
    , middleLiveness, noLiveOnEntry
    ) 
where

import BlockId
import CmmExpr
import CmmTx
import DFMonad
import Control.Monad
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
cmmLivenessZ g@(LGraph entry _) =
  liftM (check . zdfFpFacts) (res :: FuelMonad (CmmBackwardFixedPoint CmmLive))
  where res = zdfSolveFrom emptyBlockEnv "liveness analysis" liveLattice transfers
                           emptyUniqSet (graphOfLGraph g)
        transfers = BackwardTransfers (flip const) mid last
        mid  m = gen_kill m . midLive  m
        last l = gen_kill l . lastLive l 
        check facts   =
          noLiveOnEntry entry (expectJust "check" $ lookupBlockEnv facts entry) facts

gen_kill :: (DefinerOfLocalRegs a, UserOfLocalRegs a) => a -> CmmLive -> CmmLive
gen_kill a = gen a . kill a

middleLiveness :: Middle -> CmmLive -> CmmLive
middleLiveness = gen_kill

-- | On entry to the procedure, there had better not be any LocalReg's live-in.
noLiveOnEntry :: BlockId -> CmmLive -> a -> a
noLiveOnEntry bid in_fact x =
  if isEmptyUniqSet in_fact then x
  else pprPanic "LocalReg's live-in to graph" (ppr bid <+> ppr in_fact)

-- | The transfer equations use the traditional 'gen' and 'kill'
-- notations, which should be familiar from the dragon book.
gen  :: UserOfLocalRegs    a => a -> RegSet -> RegSet
gen  a live = foldRegsUsed    extendRegSet      live a
kill :: DefinerOfLocalRegs a => a -> RegSet -> RegSet
kill a live = foldRegsDefd delOneFromUniqSet live a

midLive :: Middle -> CmmLive -> CmmLive
midLive (MidForeignCall {}) _ = emptyUniqSet
midLive _                live = live

lastLive :: Last -> (BlockId -> CmmLive) -> CmmLive
lastLive l env = last l
  where last (LastBranch id)        = env id
        last (LastCall _ _  _ _ _)  = emptyUniqSet
        last (LastCondBranch _ t f) = unionUniqSets (env t) (env f)
        last (LastSwitch _ tbl)     = unionManyUniqSets $ map env (catMaybes tbl)


module CmmLiveZ
    ( CmmLive
    , cmmLivenessZ
    , liveLattice
    , middleLiveness, lastLiveness
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
cmmLivenessZ g = liftM zdfFpFacts $ (res :: FuelMonad (CmmBackwardFixedPoint CmmLive))
  where res = zdfSolveFrom emptyBlockEnv "liveness analysis" liveLattice transfers
                           emptyUniqSet (graphOfLGraph g)
        transfers = BackwardTransfers first middle last
        first live _ = live
        middle       = flip middleLiveness
        last         = flip lastLiveness

-- | The transfer equations use the traditional 'gen' and 'kill'
-- notations, which should be familiar from the dragon book.
gen, kill :: UserOfLocalRegs a => a -> RegSet -> RegSet
gen  a live = foldRegsUsed extendRegSet      live a
kill a live = foldRegsUsed delOneFromUniqSet live a

middleLiveness :: Middle -> CmmLive -> CmmLive
middleLiveness m = middle m
  where middle (MidComment {})               = id
        middle (MidAssign lhs expr)          = gen expr . kill lhs
        middle (MidStore addr rval)          = gen addr . gen rval
        middle (MidUnsafeCall tgt ress args) = gen tgt . gen args . kill ress
        middle (MidAddToContext ra args)     = gen ra . gen args
        middle (CopyIn _ formals _)          = kill formals
        middle (CopyOut _ actuals)           = gen actuals

lastLiveness :: Last -> (BlockId -> CmmLive) -> CmmLive
lastLiveness l env = last l
  where last (LastReturn)            = emptyUniqSet
        last (LastJump e)            = gen e $ emptyUniqSet
        last (LastBranch id)         = env id
        last (LastCall tgt (Just k)) = gen tgt $ env k
        last (LastCall tgt Nothing)  = gen tgt $ emptyUniqSet
        last (LastCondBranch e t f)  = gen e $ unionUniqSets (env t) (env f)
        last (LastSwitch e tbl) = gen e $ unionManyUniqSets $ map env (catMaybes tbl)

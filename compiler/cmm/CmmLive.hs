{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module CmmLive
    ( CmmLive
    , cmmLiveness
    , liveLattice
    , noLiveOnEntry, xferLive
    )
where

import BlockId
import Cmm
import CmmExpr
import Control.Monad
import OptimizationFuel
import PprCmmExpr ()

import Compiler.Hoopl
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
liveLattice = DataflowLattice "live LocalReg's" emptyRegSet add
    where add _ (OldFact old) (NewFact new) = case unionUniqSets old new of
            join -> (changeIf $ sizeUniqSet join > sizeUniqSet old, join)

-- | A mapping from block labels to the variables live on entry
type BlockEntryLiveness = BlockEnv CmmLive

-----------------------------------------------------------------------------
-- | Calculated liveness info for a CmmGraph
-----------------------------------------------------------------------------

cmmLiveness :: CmmGraph -> FuelUniqSM BlockEntryLiveness
cmmLiveness graph =
  liftM check $ liftM snd $ dataflowPassBwd graph [] $ analBwd liveLattice xferLive
  where entry = g_entry graph
        check facts = noLiveOnEntry entry (expectJust "check" $ mapLookup entry facts) facts

gen_kill :: (DefinerOfLocalRegs a, UserOfLocalRegs a) => a -> CmmLive -> CmmLive
gen_kill a = gen a . kill a

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

xferLive :: BwdTransfer CmmNode CmmLive
xferLive = mkBTransfer3 fst mid lst
  where fst _ f = f
        mid :: CmmNode O O -> CmmLive -> CmmLive
        mid n f = gen_kill n $ case n of CmmUnsafeForeignCall {} -> emptyRegSet
                                         _                       -> f
        lst :: CmmNode O C -> FactBase CmmLive -> CmmLive
        lst n f = gen_kill n $ case n of CmmCall {}            -> emptyRegSet
                                         CmmForeignCall {}     -> emptyRegSet
                                         _                     -> joinOutFacts liveLattice n f

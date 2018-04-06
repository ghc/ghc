{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CmmLive
    ( CmmLocalLive
    , cmmLocalLiveness
    , cmmGlobalLiveness
    , liveLattice
    , gen_kill
    )
where

import GhcPrelude

import DynFlags
import BlockId
import Cmm
import PprCmmExpr ()
import Hoopl.Block
import Hoopl.Collections
import Hoopl.Dataflow
import Hoopl.Label

import Maybes
import Outputable

-----------------------------------------------------------------------------
-- Calculating what variables are live on entry to a basic block
-----------------------------------------------------------------------------

-- | The variables live on entry to a block
type CmmLive r = RegSet r
type CmmLocalLive = CmmLive LocalReg

-- | The dataflow lattice
liveLattice :: Ord r => DataflowLattice (CmmLive r)
{-# SPECIALIZE liveLattice :: DataflowLattice (CmmLive LocalReg) #-}
{-# SPECIALIZE liveLattice :: DataflowLattice (CmmLive GlobalReg) #-}
liveLattice = DataflowLattice emptyRegSet add
  where
    add (OldFact old) (NewFact new) =
        let !join = plusRegSet old new
        in changedIf (sizeRegSet join > sizeRegSet old) join

-- | A mapping from block labels to the variables live on entry
type BlockEntryLiveness r = LabelMap (CmmLive r)

-----------------------------------------------------------------------------
-- | Calculated liveness info for a CmmGraph
-----------------------------------------------------------------------------

cmmLocalLiveness :: DynFlags -> CmmGraph -> BlockEntryLiveness LocalReg
cmmLocalLiveness dflags graph =
    check $ analyzeCmmBwd liveLattice (xferLive dflags) graph mapEmpty
  where
    entry = g_entry graph
    check facts =
        noLiveOnEntry entry (expectJust "check" $ mapLookup entry facts) facts

cmmGlobalLiveness :: DynFlags -> CmmGraph -> BlockEntryLiveness GlobalReg
cmmGlobalLiveness dflags graph =
    analyzeCmmBwd liveLattice (xferLive dflags) graph mapEmpty

-- | On entry to the procedure, there had better not be any LocalReg's live-in.
noLiveOnEntry :: BlockId -> CmmLive LocalReg -> a -> a
noLiveOnEntry bid in_fact x =
  if nullRegSet in_fact then x
  else pprPanic "LocalReg's live-in to graph" (ppr bid <+> ppr in_fact)

gen_kill
    :: (DefinerOfRegs r n, UserOfRegs r n)
    => DynFlags -> n -> CmmLive r -> CmmLive r
gen_kill dflags node set =
    let !afterKill = foldRegsDefd dflags deleteFromRegSet set node
    in foldRegsUsed dflags extendRegSet afterKill node
{-# INLINE gen_kill #-}

xferLive
    :: forall r.
       ( UserOfRegs r (CmmNode O O)
       , DefinerOfRegs r (CmmNode O O)
       , UserOfRegs r (CmmNode O C)
       , DefinerOfRegs r (CmmNode O C)
       )
    => DynFlags -> TransferFun (CmmLive r)
xferLive dflags (BlockCC eNode middle xNode) fBase =
    let joined = gen_kill dflags xNode $! joinOutFacts liveLattice xNode fBase
        !result = foldNodesBwdOO (gen_kill dflags) middle joined
    in mapSingleton (entryLabel eNode) result
{-# SPECIALIZE xferLive :: DynFlags -> TransferFun (CmmLive LocalReg) #-}
{-# SPECIALIZE xferLive :: DynFlags -> TransferFun (CmmLive GlobalReg) #-}

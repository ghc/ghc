{-# LANGUAGE GADTs #-}

module GHC.Cmm.Liveness
    ( CmmLocalLive
    , cmmLocalLiveness
    , cmmLocalLivenessL
    , cmmGlobalLiveness
    , liveLattice
    , liveLatticeL
    , gen_kill
    , gen_killL
    )
where

import GHC.Prelude

import GHC.Platform
import GHC.Cmm.BlockId
import GHC.Cmm
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.LRegSet

import GHC.Data.Maybe
import GHC.Utils.Outputable
import GHC.Utils.Panic

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

cmmLocalLiveness :: Platform -> CmmGraph -> BlockEntryLiveness LocalReg
cmmLocalLiveness platform graph =
    check $ analyzeCmmBwd liveLattice (xferLive platform) graph mapEmpty
  where
    entry = g_entry graph
    check facts =
        noLiveOnEntry entry (expectJust $ mapLookup entry facts) facts

cmmGlobalLiveness :: Platform -> CmmGraph -> BlockEntryLiveness GlobalRegUse
cmmGlobalLiveness platform graph =
    analyzeCmmBwd liveLattice (xferLive platform) graph mapEmpty

-- | On entry to the procedure, there had better not be any LocalReg's live-in.
-- If you see this error it most likely means you are trying to use a variable
-- without it being defined in the given scope.
noLiveOnEntry :: BlockId -> CmmLive LocalReg -> a -> a
noLiveOnEntry bid in_fact x =
  if nullRegSet in_fact then x
  else pprPanic "LocalReg's live-in to graph" (ppr bid <+> ppr in_fact)

gen_kill
    :: (DefinerOfRegs r n, UserOfRegs r n)
    => Platform -> n -> CmmLive r -> CmmLive r
gen_kill platform node set =
    let !afterKill = foldRegsDefd platform deleteFromRegSet set node
    in foldRegsUsed platform extendRegSet afterKill node
{-# INLINE gen_kill #-}

xferLive
    :: forall r.
       ( UserOfRegs r (CmmNode O O)
       , DefinerOfRegs r (CmmNode O O)
       , UserOfRegs r (CmmNode O C)
       , DefinerOfRegs r (CmmNode O C)
       )
    => Platform -> TransferFun (CmmLive r)
xferLive platform (BlockCC eNode middle xNode) fBase =
    let joined = gen_kill platform xNode $! joinOutFacts liveLattice xNode fBase
        !result = foldNodesBwdOO (gen_kill platform) middle joined
    in mapSingleton (entryLabel eNode) result
{-# SPECIALIZE xferLive :: Platform -> TransferFun (CmmLive LocalReg) #-}
{-# SPECIALIZE xferLive :: Platform -> TransferFun (CmmLive GlobalRegUse) #-}

-----------------------------------------------------------------------------
-- | Specialization that only retains the keys for local variables.
--
-- Local variables are mostly glorified Ints, and some parts of the compiler
-- really don't care about anything but the Int part. So we can avoid some
-- overhead by computing a IntSet instead of a Set LocalReg which (unsurprisingly)
-- is quite a bit faster.
-----------------------------------------------------------------------------

type BlockEntryLivenessL  = LabelMap LRegSet

-- | The dataflow lattice
liveLatticeL :: DataflowLattice LRegSet
liveLatticeL = DataflowLattice emptyLRegSet add
  where
    add (OldFact old) (NewFact new) =
        let !join = unionLRegSet old new
        in changedIf (sizeLRegSet join > sizeLRegSet old) join


cmmLocalLivenessL :: Platform -> CmmGraph -> BlockEntryLivenessL
cmmLocalLivenessL platform graph =
    check $ analyzeCmmBwd liveLatticeL (xferLiveL platform) graph mapEmpty
  where
    entry = g_entry graph
    check facts =
        noLiveOnEntryL entry (expectJust $ mapLookup entry facts) facts

-- | On entry to the procedure, there had better not be any LocalReg's live-in.
noLiveOnEntryL :: BlockId -> LRegSet -> a -> a
noLiveOnEntryL bid in_fact x =
  if nullLRegSet in_fact then x
  else pprPanic "LocalReg's live-in to graph" (ppr bid <+> ppr reg_uniques)
    where
        -- We convert the int's to uniques so that the printing matches that
        -- of registers.
        reg_uniques = elemsLRegSet in_fact




gen_killL
    :: (DefinerOfRegs LocalReg n, UserOfRegs LocalReg n)
    => Platform -> n -> LRegSet -> LRegSet
gen_killL platform node set =
    let !afterKill = foldRegsDefd platform deleteFromLRegSet set node
    in foldRegsUsed platform (flip insertLRegSet) afterKill node
{-# INLINE gen_killL #-}

xferLiveL
    :: ( UserOfRegs LocalReg (CmmNode O O)
       , DefinerOfRegs LocalReg (CmmNode O O)
       , UserOfRegs LocalReg (CmmNode O C)
       , DefinerOfRegs LocalReg (CmmNode O C)
       )
    => Platform -> TransferFun LRegSet
xferLiveL platform (BlockCC eNode middle xNode) fBase =
    let joined = gen_killL platform xNode $! joinOutFacts liveLatticeL xNode fBase
        !result = foldNodesBwdOO (gen_killL platform) middle joined
    in mapSingleton (entryLabel eNode) result



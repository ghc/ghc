{-# LANGUAGE GADTs, NoMonoLocalBinds, FlexibleContexts #-}
-- Norman likes local bindings
-- If this module lives on I'd like to get rid of this flag in due course

{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

-- TODO: Get rid of this flag:
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module CmmSpillReload
  ( dualLivenessWithInsertion
  )
where

import BlockId
import Cmm
import CmmUtils
import CmmLive
import OptimizationFuel

import Control.Monad
import Outputable hiding (empty)
import qualified Outputable as PP
import UniqSet

import Compiler.Hoopl hiding (Unique)
import Data.Maybe
import Prelude hiding (succ, zip)

{- Note [Overview of spill/reload]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The point of this module is to insert spills and reloads to establish
the invariant that at a call or any proc point with an established
protocol all live variables not expected in registers are sitting on the
stack.  We use a backward dual liveness analysis (both traditional
register liveness as well as register slot liveness on the stack) to
insert spills and reloads.  It should be followed by a forward
transformation to sink reloads as deeply as possible, so as to reduce
register pressure: this transformation is performed by
CmmRewriteAssignments.

A variable can be expected to be live in a register, live on the
stack, or both.  This analysis ensures that spills and reloads are
inserted as needed to make sure that every live variable needed
after a call is available on the stack.  Spills are placed immediately
after their reaching definitions, but reloads are placed immediately
after a return from a call (the entry point.)

Note that we offer no guarantees about the consistency of the value
in memory and the value in the register, except that they are
equal across calls/procpoints.  If the variable is changed, this
mapping breaks: but as the original value of the register may still
be useful in a different context, the memory location is not updated.
-}

data DualLive = DualLive { on_stack :: RegSet, in_regs :: RegSet }

changeStack, changeRegs :: (RegSet -> RegSet) -> DualLive -> DualLive
changeStack f live = live { on_stack = f (on_stack live) }
changeRegs  f live = live { in_regs  = f (in_regs  live) }

dualLiveLattice :: DataflowLattice DualLive
dualLiveLattice = DataflowLattice "variables live in registers and on stack" empty add
    where empty = DualLive emptyRegSet emptyRegSet
          add _ (OldFact old) (NewFact new) = (changeIf $ change1 || change2, DualLive stack regs)
            where (change1, stack) = add1 (on_stack old) (on_stack new)
                  (change2, regs)  = add1 (in_regs old)  (in_regs new)
          add1 old new = if sizeUniqSet join > sizeUniqSet old then (True, join) else (False, old)
            where join = unionUniqSets old new

dualLivenessWithInsertion :: BlockSet -> CmmGraph -> FuelUniqSM CmmGraph
dualLivenessWithInsertion procPoints g =
  liftM fst $ dataflowPassBwd g [] $ analRewBwd dualLiveLattice
                                                (dualLiveTransfers (g_entry g) procPoints)
                                                (insertSpillsAndReloads g procPoints)

-- Note [Live registers on entry to procpoints]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Remember that the transfer function is only ever run on the rewritten
-- version of a graph, and the rewrite function for spills and reloads
-- enforces the invariant that no local registers are live on entry to
-- a procpoint.  Accordingly, we check for this invariant here.  An old
-- version of this code incorrectly claimed that any live registers were
-- live on the stack before entering the function: this is wrong, but
-- didn't cause bugs because it never actually was invoked.

dualLiveTransfers :: BlockId -> BlockSet -> (BwdTransfer CmmNode DualLive)
dualLiveTransfers entry procPoints = mkBTransfer3 first middle last
    where first :: CmmNode C O -> DualLive -> DualLive
          first (CmmEntry id) live -- See Note [Live registers on entry to procpoints]
            | id == entry || setMember id procPoints = noLiveOnEntry id (in_regs live) live
            | otherwise                              = live

          middle :: CmmNode O O -> DualLive -> DualLive
          middle m = changeStack updSlots
                   . changeRegs  updRegs
            where -- Reuse middle of liveness analysis from CmmLive
                  updRegs = case getBTransfer3 xferLive of (_, middle, _) -> middle m

                  updSlots live = foldSlotsUsed reload (foldSlotsDefd spill live m) m
                  spill  live s@(RegSlot r, _, _) = check s $ deleteFromRegSet live r
                  spill  live _ = live
                  reload live s@(RegSlot r, _, _) = check s $ extendRegSet live r
                  reload live _ = live
                  -- Ensure the assignment refers to the entirety of the
                  -- register slot (and not just a slice).
                  check (RegSlot (LocalReg _ ty), o, w) x
                     | o == w && w == widthInBytes (typeWidth ty) = x
                  check _ _ = panic "dualLiveTransfers: slices unsupported"

          -- Register analysis is identical to liveness analysis from CmmLive.
          last :: CmmNode O C -> FactBase DualLive -> DualLive
          last l fb = changeRegs (gen_kill l) $ case l of
            CmmCall {cml_cont=Nothing} -> empty
            CmmCall {cml_cont=Just k}  -> keep_stack_only k
            CmmForeignCall {succ=k}    -> keep_stack_only k
            _                          -> joinOutFacts dualLiveLattice l fb
            where empty = fact_bot dualLiveLattice
                  lkp k = fromMaybe empty (lookupFact k fb)
                  keep_stack_only k = DualLive (on_stack (lkp k)) emptyRegSet

insertSpillsAndReloads :: CmmGraph -> BlockSet -> CmmBwdRewrite DualLive
insertSpillsAndReloads graph procPoints = deepBwdRw3 first middle nothing
  -- Beware: deepBwdRw with one polymorphic function seems more reasonable here,
  -- but GHC miscompiles it, see bug #4044.
    where first :: CmmNode C O -> Fact O DualLive -> CmmReplGraph C O
          first e@(CmmEntry id) live = return $
            if id /= (g_entry graph) && setMember id procPoints then
              case map reload (uniqSetToList (in_regs live)) of
                [] -> Nothing
                is -> Just $ mkFirst e <*> mkMiddles is
            else Nothing
          -- EZY: There was some dead code for handling the case where
          -- we were not splitting procedures.  Check Git history if
          -- you're interested (circa e26ea0f41).

          middle :: CmmNode O O -> Fact O DualLive -> CmmReplGraph O O
          -- Don't add spills next to reloads.
          middle (CmmAssign (CmmLocal reg) (CmmLoad (CmmStackSlot (RegSlot reg') _) _)) _ | reg == reg' = return Nothing
          -- Spill if register is live on stack.
          middle m@(CmmAssign (CmmLocal reg) _) live
            | reg `elemRegSet` on_stack live = return (Just (mkMiddles [m, spill reg]))
          middle _ _ = return Nothing

          nothing _ _ = return Nothing

spill, reload :: LocalReg -> CmmNode O O
spill  r = CmmStore  (regSlot r) (CmmReg $ CmmLocal r)
reload r = CmmAssign (CmmLocal r) (CmmLoad (regSlot r) $ localRegType r)

---------------------
-- prettyprinting

ppr_regs :: String -> RegSet -> SDoc
ppr_regs s regs = text s <+> commafy (map ppr $ uniqSetToList regs)
  where commafy xs = hsep $ punctuate comma xs

instance Outputable DualLive where
  ppr (DualLive {in_regs = regs, on_stack = stack}) =
      if isEmptyUniqSet regs && isEmptyUniqSet stack then
          text "<nothing-live>"
      else
          nest 2 $ fsep [if isEmptyUniqSet regs then PP.empty
                         else (ppr_regs "live in regs =" regs),
                         if isEmptyUniqSet stack then PP.empty
                         else (ppr_regs "live on stack =" stack)]

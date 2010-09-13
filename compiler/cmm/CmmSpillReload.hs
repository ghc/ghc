#if __GLASGOW_HASKELL__ >= 611
{-# OPTIONS_GHC -XNoMonoLocalBinds #-}
#endif
-- Norman likes local bindings
-- If this module lives on I'd like to get rid of this flag in due course

module CmmSpillReload
  ( DualLive(..)
  , dualLiveLattice, dualLiveTransfers, dualLiveness
  --, insertSpillsAndReloads  --- XXX todo check live-in at entry against formals
  , dualLivenessWithInsertion

  , availRegsLattice
  , cmmAvailableReloads
  , insertLateReloads
  , removeDeadAssignmentsAndReloads
  )
where

import BlockId
import CmmExpr
import CmmTx
import CmmLiveZ
import DFMonad
import MkZipCfg
import PprCmm()
import ZipCfg
import ZipCfgCmmRep
import ZipDataflow

import Control.Monad
import Outputable hiding (empty)
import qualified Outputable as PP
import UniqSet

import Data.Maybe
import Prelude hiding (zip)

{- Note [Overview of spill/reload]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The point of this module is to insert spills and reloads to
establish the invariant that at a call (or at any proc point with
an established protocol) all live variables not expected in
registers are sitting on the stack.  We use a backward analysis to
insert spills and reloads.  It should be followed by a
forward transformation to sink reloads as deeply as possible, so as
to reduce register pressure.

A variable can be expected to be live in a register, live on the
stack, or both.  This analysis ensures that spills and reloads are
inserted as needed to make sure that every live variable needed
after a call is available on the stack.  Spills are pushed back to
their reaching definitions, but reloads are dropped wherever needed
and will have to be sunk by a later forward transformation.
-}

data DualLive = DualLive { on_stack :: RegSet, in_regs :: RegSet }

dualUnion :: DualLive -> DualLive -> DualLive
dualUnion (DualLive s r) (DualLive s' r') =
    DualLive (s `unionUniqSets` s') (r `unionUniqSets` r') 

dualUnionList :: [DualLive] -> DualLive
dualUnionList ls = DualLive ss rs
    where ss = unionManyUniqSets $ map on_stack ls
          rs = unionManyUniqSets $ map in_regs  ls

changeStack, changeRegs :: (RegSet -> RegSet) -> DualLive -> DualLive
changeStack f live = live { on_stack = f (on_stack live) }
changeRegs  f live = live { in_regs  = f (in_regs  live) }


dualLiveLattice :: DataflowLattice DualLive
dualLiveLattice =
      DataflowLattice "variables live in registers and on stack" empty add False
    where empty = DualLive emptyRegSet emptyRegSet
          -- | compute in the Tx monad to track whether anything has changed
          add new old = do stack <- add1 (on_stack new) (on_stack old)
                           regs  <- add1 (in_regs new)  (in_regs old)
                           return $ DualLive stack regs
          add1 = fact_add_to liveLattice

type LiveReloadFix a = FuelMonad (BackwardFixedPoint Middle Last DualLive a)

dualLivenessWithInsertion :: BlockSet -> (LGraph Middle Last) -> FuelMonad (LGraph Middle Last)
dualLivenessWithInsertion procPoints g@(LGraph entry _) =
  liftM zdfFpContents $ (res :: LiveReloadFix (LGraph Middle Last))
    where res = zdfBRewriteFromL RewriteDeep emptyBlockEnv "dual liveness with insertion"
                                 dualLiveLattice (dualLiveTransfers entry procPoints)
                                 (insertSpillAndReloadRewrites entry procPoints) empty g
          empty = fact_bot dualLiveLattice

dualLiveness :: BlockSet -> LGraph Middle Last -> FuelMonad (BlockEnv DualLive)
dualLiveness procPoints g@(LGraph entry _) =
  liftM zdfFpFacts $ (res :: LiveReloadFix ())
    where res = zdfSolveFromL emptyBlockEnv "dual liveness" dualLiveLattice
                              (dualLiveTransfers entry procPoints) empty g
          empty = fact_bot dualLiveLattice

dualLiveTransfers :: BlockId -> BlockSet -> BackwardTransfers Middle Last DualLive
dualLiveTransfers entry procPoints = BackwardTransfers first middle last
    where last   = lastDualLiveness
          middle = middleDualLiveness
          first id live = check live id $  -- live at procPoint => spill
            if id /= entry && elemBlockSet id procPoints then
              DualLive { on_stack = on_stack live `plusRegSet` in_regs live
                       , in_regs  = emptyRegSet }
            else live
          check live id x = if id == entry then noLiveOnEntry id (in_regs live) x else x
  
middleDualLiveness :: Middle -> DualLive -> DualLive
middleDualLiveness m live =
  changeStack updSlots $ changeRegs (middleLiveness m) (changeRegs regs_in live)
    where regs_in live = case m of MidForeignCall {} -> emptyRegSet
                                   _ -> live
          updSlots live = foldSlotsUsed reload (foldSlotsDefd spill live m) m
          spill  live s@(RegSlot r, _, _) = check s $ deleteFromRegSet live r
          spill  live _ = live
          reload live s@(RegSlot r, _, _) = check s $ extendRegSet live r
          reload live _ = live
          check (RegSlot (LocalReg _ ty), o, w) x
             | o == w && w == widthInBytes (typeWidth ty) = x
          check _ _ = panic "middleDualLiveness unsupported: slices"

lastDualLiveness :: Last -> (BlockId -> DualLive) -> DualLive
lastDualLiveness l env = last l
  where last (LastBranch id)          = env id
        last l@(LastCall _ Nothing  _ _ _) = changeRegs (gen l . kill l) empty
        last l@(LastCall _ (Just k) _ _ _) = 
            -- nothing can be live in registers at this point, unless safe foreign call
            let live = env k
                live_in = DualLive (on_stack live) (gen l emptyRegSet)
            in if isEmptyUniqSet (in_regs live) then live_in
               else pprTrace "Offending party:" (ppr k <+> ppr live) $
                    panic "live values in registers at call continuation"
        last l@(LastCondBranch _ t f)   =
            changeRegs (gen l . kill l) $ dualUnion (env t) (env f)
        last l@(LastSwitch _ tbl)       = changeRegs (gen l . kill l) $ dualUnionList $
                                                             map env (catMaybes tbl)
        empty = fact_bot dualLiveLattice
                      
gen  :: UserOfLocalRegs    a => a -> RegSet -> RegSet
gen  a live = foldRegsUsed extendRegSet     live a
kill :: DefinerOfLocalRegs a => a -> RegSet -> RegSet
kill a live = foldRegsDefd deleteFromRegSet live a

insertSpillAndReloadRewrites ::
  BlockId -> BlockSet -> BackwardRewrites Middle Last DualLive
insertSpillAndReloadRewrites entry procPoints =
  BackwardRewrites first middle last exit
    where middle = middleInsertSpillsAndReloads
          last _ _ = Nothing
          exit     = Nothing
          first id live =
            if id /= entry && elemBlockSet id procPoints then
              case map reload (uniqSetToList (in_regs live)) of
                [] -> Nothing
                is -> Just (mkMiddles is)
            else Nothing

middleInsertSpillsAndReloads :: Middle -> DualLive -> Maybe (AGraph Middle Last)
middleInsertSpillsAndReloads m live = middle m
  where middle (MidAssign (CmmLocal reg) (CmmLoad (CmmStackSlot (RegSlot reg') _) _))
          | reg == reg' = Nothing
        middle (MidAssign (CmmLocal reg) _) = 
            if reg `elemRegSet` on_stack live then -- must spill
                 my_trace "Spilling" (f4sep [text "spill" <+> ppr reg,
                                             text "after", ppr m]) $
                 Just $ mkMiddles $ [m, spill reg]
            else Nothing
        middle (MidForeignCall _ _ fs _) =
          case map spill  (filter (flip elemRegSet (on_stack live)) fs) ++
               map reload (uniqSetToList (kill fs (in_regs live))) of
            []      -> Nothing
            reloads -> Just (mkMiddles (m : reloads))
        middle _ = Nothing
                      
-- Generating spill and reload code
regSlot :: LocalReg -> CmmExpr
regSlot r = CmmStackSlot (RegSlot r) (widthInBytes $ typeWidth $ localRegType r)

spill, reload :: LocalReg -> Middle
spill  r = MidStore  (regSlot r) (CmmReg $ CmmLocal r)
reload r = MidAssign (CmmLocal r) (CmmLoad (regSlot r) $ localRegType r)

----------------------------------------------------------------
--- sinking reloads

-- The idea is to compute at each point the set of registers such that
-- on every path to the point, the register is defined by a Reload
-- instruction.  Then, if a use appears at such a point, we can safely
-- insert a Reload right before the use.  Finally, we can eliminate
-- the early reloads along with other dead assignments.

data AvailRegs = UniverseMinus RegSet
               | AvailRegs     RegSet


availRegsLattice :: DataflowLattice AvailRegs
availRegsLattice = DataflowLattice "register gotten from reloads" empty add False
    where empty = UniverseMinus emptyRegSet
          -- | compute in the Tx monad to track whether anything has changed
          add new old =
            let join = interAvail new old in
            if join `smallerAvail` old then aTx join else noTx join


interAvail :: AvailRegs -> AvailRegs -> AvailRegs
interAvail (UniverseMinus s) (UniverseMinus s') = UniverseMinus (s `plusRegSet`  s')
interAvail (AvailRegs     s) (AvailRegs     s') = AvailRegs (s `timesRegSet` s')
interAvail (AvailRegs     s) (UniverseMinus s') = AvailRegs (s  `minusRegSet` s')
interAvail (UniverseMinus s) (AvailRegs     s') = AvailRegs (s' `minusRegSet` s )

smallerAvail :: AvailRegs -> AvailRegs -> Bool
smallerAvail (AvailRegs     _) (UniverseMinus _)  = True
smallerAvail (UniverseMinus _) (AvailRegs     _)  = False
smallerAvail (AvailRegs     s) (AvailRegs    s')  = sizeUniqSet s < sizeUniqSet s'
smallerAvail (UniverseMinus s) (UniverseMinus s') = sizeUniqSet s > sizeUniqSet s'

extendAvail :: AvailRegs -> LocalReg -> AvailRegs
extendAvail (UniverseMinus s) r = UniverseMinus (deleteFromRegSet s r)
extendAvail (AvailRegs     s) r = AvailRegs (extendRegSet s r)

delFromAvail :: AvailRegs -> LocalReg -> AvailRegs
delFromAvail (UniverseMinus s) r = UniverseMinus (extendRegSet s r)
delFromAvail (AvailRegs     s) r = AvailRegs (deleteFromRegSet s r)

elemAvail :: AvailRegs -> LocalReg -> Bool
elemAvail (UniverseMinus s) r = not $ elemRegSet r s
elemAvail (AvailRegs     s) r = elemRegSet r s

type AvailFix = FuelMonad (ForwardFixedPoint Middle Last AvailRegs ())

cmmAvailableReloads :: LGraph Middle Last -> FuelMonad (BlockEnv AvailRegs)
cmmAvailableReloads g = liftM zdfFpFacts $ (res :: AvailFix)
    where res = zdfSolveFromL emptyBlockEnv "available reloads" availRegsLattice
                              avail_reloads_transfer empty g
          empty = fact_bot availRegsLattice

avail_reloads_transfer :: ForwardTransfers Middle Last AvailRegs
avail_reloads_transfer = ForwardTransfers (flip const) middleAvail lastAvail id

middleAvail :: Middle -> AvailRegs -> AvailRegs
middleAvail (MidAssign (CmmLocal r) (CmmLoad l _)) avail
               | l `isStackSlotOf` r = extendAvail avail r
middleAvail (MidAssign lhs _)        avail = foldRegsDefd delFromAvail avail lhs
middleAvail (MidStore l (CmmReg (CmmLocal r))) avail
               | l `isStackSlotOf` r = avail
middleAvail (MidStore (CmmStackSlot (RegSlot r) _) _) avail = delFromAvail avail r
middleAvail (MidStore {})            avail = avail
middleAvail (MidForeignCall {})      _     = AvailRegs emptyRegSet
middleAvail (MidComment {})          avail = avail

lastAvail :: Last -> AvailRegs -> LastOutFacts AvailRegs
lastAvail (LastCall _ (Just k) _ _ _) _ = LastOutFacts [(k, AvailRegs emptyRegSet)]
lastAvail l avail = LastOutFacts $ map (\id -> (id, avail)) $ succs l

type LateReloadFix = FuelMonad (ForwardFixedPoint Middle Last AvailRegs CmmGraph)

availRewrites :: ForwardRewrites Middle Last AvailRegs
availRewrites = ForwardRewrites first middle last exit
  where first _ _ = Nothing
        middle m avail = maybe_reload_before avail m (mkMiddle m)
        last   l avail = maybe_reload_before avail l (mkLast l)
        exit _ = Nothing
        maybe_reload_before avail node tail =
            let used = filterRegsUsed (elemAvail avail) node
            in  if isEmptyUniqSet used then Nothing
                else Just $ reloadTail used tail
        reloadTail regset t = foldl rel t $ uniqSetToList regset
          where rel t r = mkMiddle (reload r) <*> t


insertLateReloads :: (LGraph Middle Last) -> FuelMonad (LGraph Middle Last)
insertLateReloads g = liftM zdfFpContents $ (res :: LateReloadFix)
    where res = zdfFRewriteFromL RewriteShallow emptyBlockEnv "insert late reloads"
                                 availRegsLattice avail_reloads_transfer availRewrites bot g
          bot = fact_bot availRegsLattice
          
removeDeadAssignmentsAndReloads :: BlockSet -> (LGraph Middle Last) -> FuelMonad (LGraph Middle Last)
removeDeadAssignmentsAndReloads procPoints g@(LGraph entry _) =
   liftM zdfFpContents $ (res :: LiveReloadFix (LGraph Middle Last))
     where res = zdfBRewriteFromL RewriteDeep emptyBlockEnv "dead-assignment & -reload elim"
                   dualLiveLattice (dualLiveTransfers entry procPoints)
                   rewrites (fact_bot dualLiveLattice) g
           rewrites = BackwardRewrites nothing middleRemoveDeads nothing Nothing
           nothing _ _ = Nothing

middleRemoveDeads :: Middle -> DualLive -> Maybe (AGraph Middle Last)
middleRemoveDeads  (MidAssign (CmmLocal reg') _) live
       | not (reg' `elemRegSet` in_regs live) = Just emptyAGraph
middleRemoveDeads  _ _ = Nothing
                      


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

instance Outputable AvailRegs where
  ppr (UniverseMinus s) = if isEmptyUniqSet s then text "<everything available>"
                          else ppr_regs "available = all but" s
  ppr (AvailRegs     s) = if isEmptyUniqSet s then text "<nothing available>"
                          else ppr_regs "available = " s

my_trace :: String -> SDoc -> a -> a
my_trace = if False then pprTrace else \_ _ a -> a

f4sep :: [SDoc] -> SDoc
f4sep [] = fsep []
f4sep (d:ds) = fsep (d : map (nest 4) ds)

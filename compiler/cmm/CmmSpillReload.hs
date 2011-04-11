{-# LANGUAGE GADTs,NoMonoLocalBinds #-}
-- Norman likes local bindings
-- If this module lives on I'd like to get rid of this flag in due course

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
#if __GLASGOW_HASKELL__ >= 701
-- GHC 7.0.1 improved incomplete pattern warnings with GADTs
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
#endif

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
import Cmm
import CmmExpr
import CmmLive
import OptimizationFuel

import Control.Monad
import Outputable hiding (empty)
import qualified Outputable as PP
import UniqSet

import Compiler.Hoopl
import Data.Maybe
import Prelude hiding (succ, zip)

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
                                                (insertSpillAndReloadRewrites g procPoints)

dualLiveness :: BlockSet -> CmmGraph -> FuelUniqSM (BlockEnv DualLive)
dualLiveness procPoints g =
  liftM snd $ dataflowPassBwd g [] $ analBwd dualLiveLattice $ dualLiveTransfers (g_entry g) procPoints

dualLiveTransfers :: BlockId -> BlockSet -> (BwdTransfer CmmNode DualLive)
dualLiveTransfers entry procPoints = mkBTransfer3 first middle last
    where first :: CmmNode C O -> DualLive -> DualLive
          first (CmmEntry id) live = check live id $  -- live at procPoint => spill
            if id /= entry && setMember id procPoints
               then DualLive { on_stack = on_stack live `plusRegSet` in_regs live
                             , in_regs  = emptyRegSet }
               else live
            where check live id x = if id == entry then noLiveOnEntry id (in_regs live) x else x

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
                  check (RegSlot (LocalReg _ ty), o, w) x
                     | o == w && w == widthInBytes (typeWidth ty) = x
                  check _ _ = panic "middleDualLiveness unsupported: slices"
          last :: CmmNode O C -> FactBase DualLive -> DualLive
          last l fb = case l of
            CmmBranch id                   -> lkp id
            l@(CmmCall {cml_cont=Nothing}) -> changeRegs (gen l . kill l) empty
            l@(CmmCall {cml_cont=Just k})  -> call l k
            l@(CmmForeignCall {succ=k})    -> call l k
            l@(CmmCondBranch _ t f)        -> changeRegs (gen l . kill l) $ dualUnion (lkp t) (lkp f)
            l@(CmmSwitch _ tbl)            -> changeRegs (gen l . kill l) $ dualUnionList $ map lkp (catMaybes tbl)
            where empty = fact_bot dualLiveLattice
                  lkp id = empty `fromMaybe` lookupFact id fb
                  call l k = DualLive (on_stack (lkp k)) (gen l emptyRegSet)

gen  :: UserOfLocalRegs    a => a -> RegSet -> RegSet
gen  a live = foldRegsUsed extendRegSet     live a
kill :: DefinerOfLocalRegs a => a -> RegSet -> RegSet
kill a live = foldRegsDefd deleteFromRegSet live a

insertSpillAndReloadRewrites :: CmmGraph -> BlockSet -> CmmBwdRewrite DualLive
insertSpillAndReloadRewrites graph procPoints = deepBwdRw3 first middle nothing
  -- Beware: deepBwdRw with one polymorphic function seems more reasonable here,
  -- but GHC miscompiles it, see bug #4044.
    where first :: CmmNode C O -> Fact O DualLive -> CmmReplGraph C O
          first e@(CmmEntry id) live = return $
            if id /= (g_entry graph) && setMember id procPoints then
              case map reload (uniqSetToList spill_regs) of
                [] -> Nothing
                is -> Just $ mkFirst e <*> mkMiddles is
            else Nothing
              where
                -- If we are splitting procedures, we need the LastForeignCall
                -- to spill its results to the stack because they will only
                -- be used by a separate procedure (so they can't stay in LocalRegs).
                splitting = True
                spill_regs = if splitting then in_regs live
                             else in_regs live `minusRegSet` defs
                defs = case mapLookup id firstDefs of
                           Just defs -> defs
                           Nothing   -> emptyRegSet
                -- A LastForeignCall may contain some definitions, which take place
                -- on return from the function call. Therefore, we build a map (firstDefs)
                -- from BlockId to the set of variables defined on return to the BlockId.
                firstDefs = mapFold addLive emptyBlockMap (toBlockMap graph)
                addLive :: CmmBlock -> BlockEnv RegSet -> BlockEnv RegSet
                addLive b env = case lastNode b of
                                  CmmForeignCall {succ=k, res=defs} -> add k (mkRegSet defs) env
                                  _                                 -> env
                add bid defs env = mapInsert bid defs'' env
                  where defs'' = case mapLookup bid env of
                                   Just defs' -> timesRegSet defs defs'
                                   Nothing    -> defs

          middle :: CmmNode O O -> Fact O DualLive -> CmmReplGraph O O
          middle (CmmAssign (CmmLocal reg) (CmmLoad (CmmStackSlot (RegSlot reg') _) _)) _ | reg == reg' = return Nothing
          middle m@(CmmAssign (CmmLocal reg) _) live = return $
              if reg `elemRegSet` on_stack live then -- must spill
                   my_trace "Spilling" (f4sep [text "spill" <+> ppr reg,
                                               text "after"{-, ppr m-}]) $
                   Just $ mkMiddles $ [m, spill reg]
              else Nothing
          middle m@(CmmUnsafeForeignCall _ fs _) live = return $
            case map spill  (filter (flip elemRegSet (on_stack live)) fs) ++
                 map reload (uniqSetToList (kill fs (in_regs live))) of
              []      -> Nothing
              reloads -> Just $ mkMiddles (m : reloads)
          middle _ _ = return Nothing

          nothing _ _ = return Nothing

regSlot :: LocalReg -> CmmExpr
regSlot r = CmmStackSlot (RegSlot r) (widthInBytes $ typeWidth $ localRegType r)

spill, reload :: LocalReg -> CmmNode O O
spill  r = CmmStore  (regSlot r) (CmmReg $ CmmLocal r)
reload r = CmmAssign (CmmLocal r) (CmmLoad (regSlot r) $ localRegType r)

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
availRegsLattice = DataflowLattice "register gotten from reloads" empty add
    where empty = UniverseMinus emptyRegSet
          -- | compute in the Tx monad to track whether anything has changed
          add _ (OldFact old) (NewFact new) =
            if join `smallerAvail` old then (SomeChange, join) else (NoChange, old)
            where join = interAvail new old


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

cmmAvailableReloads :: CmmGraph -> FuelUniqSM (BlockEnv AvailRegs)
cmmAvailableReloads g =
  liftM snd $ dataflowPassFwd g [(g_entry g, fact_bot availRegsLattice)] $
                              analFwd availRegsLattice availReloadsTransfer

availReloadsTransfer :: FwdTransfer CmmNode AvailRegs
availReloadsTransfer = mkFTransfer3 (flip const) middleAvail ((mkFactBase availRegsLattice .) . lastAvail)

middleAvail :: CmmNode O O -> AvailRegs -> AvailRegs
middleAvail (CmmAssign (CmmLocal r) (CmmLoad l _)) avail
               | l `isStackSlotOf` r = extendAvail avail r
middleAvail (CmmAssign lhs _)        avail = foldRegsDefd delFromAvail avail lhs
middleAvail (CmmStore l (CmmReg (CmmLocal r))) avail
               | l `isStackSlotOf` r = avail
middleAvail (CmmStore (CmmStackSlot (RegSlot r) _) _) avail = delFromAvail avail r
middleAvail (CmmStore {})            avail = avail
middleAvail (CmmUnsafeForeignCall {}) _    = AvailRegs emptyRegSet
middleAvail (CmmComment {})          avail = avail

lastAvail :: CmmNode O C -> AvailRegs -> [(Label, AvailRegs)]
lastAvail (CmmCall _ (Just k) _ _ _) _ = [(k, AvailRegs emptyRegSet)]
lastAvail (CmmForeignCall {succ=k})  _ = [(k, AvailRegs emptyRegSet)]
lastAvail l avail = map (\id -> (id, avail)) $ successors l

insertLateReloads :: CmmGraph -> FuelUniqSM CmmGraph
insertLateReloads g =
  liftM fst $ dataflowPassFwd g [(g_entry g, fact_bot availRegsLattice)] $
                              analRewFwd availRegsLattice availReloadsTransfer rewrites
  where rewrites = mkFRewrite3 first middle last
        first _ _ = return Nothing
        middle m avail = return $ maybe_reload_before avail m (mkMiddle m)
        last   l avail = return $ maybe_reload_before avail l (mkLast l)
        maybe_reload_before avail node tail =
            let used = filterRegsUsed (elemAvail avail) node
            in  if isEmptyUniqSet used then Nothing
                                       else Just $ reloadTail used tail
        reloadTail regset t = foldl rel t $ uniqSetToList regset
          where rel t r = mkMiddle (reload r) <*> t

removeDeadAssignmentsAndReloads :: BlockSet -> CmmGraph -> FuelUniqSM CmmGraph
removeDeadAssignmentsAndReloads procPoints g =
   liftM fst $ dataflowPassBwd g [] $ analRewBwd dualLiveLattice
                                                 (dualLiveTransfers (g_entry g) procPoints)
                                                 rewrites
   where rewrites = deepBwdRw3 nothing middle nothing
         -- Beware: deepBwdRw with one polymorphic function seems more reasonable here,
         -- but GHC panics while compiling, see bug #4045.
         middle :: CmmNode O O -> Fact O DualLive -> CmmReplGraph O O
         middle (CmmAssign (CmmLocal reg') _) live | not (reg' `elemRegSet` in_regs live) = return $ Just emptyGraph
         middle _ _ = return Nothing

         nothing _ _ = return Nothing


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

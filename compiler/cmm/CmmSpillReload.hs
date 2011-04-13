{-# LANGUAGE GADTs, NoMonoLocalBinds, FlexibleContexts, ViewPatterns #-}
-- Norman likes local bindings
-- If this module lives on I'd like to get rid of this flag in due course

{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
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

  , rewriteAssignments
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
import UniqFM
import Unique

import Compiler.Hoopl hiding (Unique)
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
          middle _ _ = return Nothing

          nothing _ _ = return Nothing

regSlot :: LocalReg -> CmmExpr
regSlot r = CmmStackSlot (RegSlot r) (widthInBytes $ typeWidth $ localRegType r)

spill, reload :: LocalReg -> CmmNode O O
spill  r = CmmStore  (regSlot r) (CmmReg $ CmmLocal r)
reload r = CmmAssign (CmmLocal r) (CmmLoad (regSlot r) $ localRegType r)

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
         -- XXX maybe this should be somewhere else...
         middle (CmmStore lhs (CmmLoad rhs _)) _ | lhs == rhs = return $ Just emptyGraph
         middle _ _ = return Nothing

         nothing _ _ = return Nothing

----------------------------------------------------------------
--- Usage information

-- We decorate all register assignments with usage information,
-- that is, the maximum number of times the register is referenced
-- while it is live along all outgoing control paths.  There are a few
-- subtleties here:
--
--  - If a register goes dead, and then becomes live again, the usages
--    of the disjoint live range don't count towards the original range.
--
--          a = 1; // used once
--          b = a;
--          a = 2; // used once
--          c = a;
--
--  - A register may be used multiple times, but these all reside in
--    different control paths, such that any given execution only uses
--    it once. In that case, the usage count may still be 1.
--
--          a = 1; // used once
--          if (b) {
--              c = a + 3;
--          } else {
--              c = a + 1;
--          }
--
--    This policy corresponds to an inlining strategy that does not
--    duplicate computation but may increase binary size.
--
--  - If we naively implement a usage count, we have a counting to
--    infinity problem across joins.  Furthermore, knowing that
--    something is used 2 or more times in one runtime execution isn't
--    particularly useful for optimizations (inlining may be beneficial,
--    but there's no way of knowing that without register pressure
--    information.)
--
--          while (...) {
--              // first iteration, b used once
--              // second iteration, b used twice
--              // third iteration ...
--              a = b;
--          }
--          // b used zero times
--
--    There is an orthogonal question, which is that for every runtime
--    execution, the register may be used only once, but if we inline it
--    in every conditional path, the binary size might increase a lot.
--    But tracking this information would be tricky, because it violates
--    the finite lattice restriction Hoopl requires for termination;
--    we'd thus need to supply an alternate proof, which is probably
--    something we should defer until we actually have an optimization
--    that would take advantage of this.  (This might also interact
--    strangely with liveness information.)
--
--          a = ...;
--          // a is used one time, but in X different paths
--          case (b) of
--              1 -> ... a ...
--              2 -> ... a ...
--              3 -> ... a ...
--              ...
--
--  This analysis is very similar to liveness analysis; we just keep a
--  little extra info. (Maybe we should move it to CmmLive, and subsume
--  the old liveness analysis.)

data RegUsage = SingleUse | ManyUse
    deriving (Ord, Eq, Show)
-- Absence in map = ZeroUse

{-
-- minBound is bottom, maxBound is top, least-upper-bound is max
-- ToDo: Put this in Hoopl.  Note that this isn't as useful as I
-- originally hoped, because you usually want to leave out the bottom
-- element when you have things like this put in maps.  Maybe f is
-- useful on its own as a combining function.
boundedOrdLattice :: (Bounded a, Ord a) => String -> DataflowLattice a
boundedOrdLattice n = DataflowLattice n minBound f
    where f _ (OldFact x) (NewFact y)
            | x >= y    = (NoChange,   x)
            | otherwise = (SomeChange, y)
-}

-- Custom node type we'll rewrite to.  CmmAssign nodes to local
-- registers are replaced with AssignLocal nodes.
data WithRegUsage n e x where
    Plain       :: n e x -> WithRegUsage n e x
    AssignLocal :: LocalReg -> CmmExpr -> RegUsage -> WithRegUsage n O O

instance UserOfLocalRegs (n e x) => UserOfLocalRegs (WithRegUsage n e x) where
    foldRegsUsed f z (Plain n) = foldRegsUsed f z n
    foldRegsUsed f z (AssignLocal _ e _) = foldRegsUsed f z e

instance DefinerOfLocalRegs (n e x) => DefinerOfLocalRegs (WithRegUsage n e x) where
    foldRegsDefd f z (Plain n) = foldRegsDefd f z n
    foldRegsDefd f z (AssignLocal r _ _) = foldRegsDefd f z r

instance NonLocal n => NonLocal (WithRegUsage n) where
    entryLabel (Plain n) = entryLabel n
    successors (Plain n) = successors n

liftRegUsage :: Graph n e x -> Graph (WithRegUsage n) e x
liftRegUsage = mapGraph Plain

eraseRegUsage :: Graph (WithRegUsage CmmNode) e x -> Graph CmmNode e x
eraseRegUsage = mapGraph f
    where f :: WithRegUsage CmmNode e x -> CmmNode e x
          f (AssignLocal l e _) = CmmAssign (CmmLocal l) e
          f (Plain n) = n

type UsageMap = UniqFM RegUsage

usageLattice :: DataflowLattice UsageMap
usageLattice = DataflowLattice "usage counts for registers" emptyUFM (joinUFM f)
    where f _ (OldFact x) (NewFact y)
            | x >= y    = (NoChange,   x)
            | otherwise = (SomeChange, y)

-- We reuse the names 'gen' and 'kill', although we're doing something
-- slightly different from the Dragon Book
usageTransfer :: BwdTransfer (WithRegUsage CmmNode) UsageMap
usageTransfer = mkBTransfer3 first middle last
    where first _ f = f
          middle :: WithRegUsage CmmNode O O -> UsageMap -> UsageMap
          middle n f = gen_kill n f
          last :: WithRegUsage CmmNode O C -> FactBase UsageMap -> UsageMap
          -- Checking for CmmCall/CmmForeignCall is unnecessary, because
          -- spills/reloads have already occurred by the time we do this
          -- analysis.
          -- XXX Deprecated warning is puzzling: what label are we
          -- supposed to use?
          -- ToDo: With a bit more cleverness here, we can avoid
          -- disappointment and heartbreak associated with the inability
          -- to inline into CmmCall and CmmForeignCall by
          -- over-estimating the usage to be ManyUse.
          last n f = gen_kill n (joinOutFacts usageLattice n f)
          gen_kill a = gen a . kill a
          gen  a f = foldRegsUsed increaseUsage f a
          kill a f = foldRegsDefd delFromUFM f a
          increaseUsage f r = addToUFM_C combine f r SingleUse
            where combine _ _ = ManyUse

usageRewrite :: BwdRewrite FuelUniqSM (WithRegUsage CmmNode) UsageMap
usageRewrite = mkBRewrite3 first middle last
    where first  _ _ = return Nothing
          middle :: Monad m => WithRegUsage CmmNode O O -> UsageMap -> m (Maybe (Graph (WithRegUsage CmmNode) O O))
          middle (Plain (CmmAssign (CmmLocal l) e)) f
                     = return . Just
                     $ case lookupUFM f l of
                            Nothing    -> emptyGraph
                            Just usage -> mkMiddle (AssignLocal l e usage)
          middle _ _ = return Nothing
          last   _ _ = return Nothing

type CmmGraphWithRegUsage = GenCmmGraph (WithRegUsage CmmNode)
annotateUsage :: CmmGraph -> FuelUniqSM (CmmGraphWithRegUsage)
annotateUsage vanilla_g =
    let g = modifyGraph liftRegUsage vanilla_g
    in liftM fst $ dataflowPassBwd g [(g_entry g, fact_bot usageLattice)] $
                                   analRewBwd usageLattice usageTransfer usageRewrite

----------------------------------------------------------------
--- Assignment tracking

-- The idea is to maintain a map of local registers do expressions,
-- such that the value of that register is the same as the value of that
-- expression at any given time.  We can then do several things,
-- as described by Assignment.

-- Assignment describes the various optimizations that are valid
-- at a given point in the program.
data Assignment =
-- This assignment can always be inlined.  It is cheap or single-use.
                  AlwaysInline CmmExpr
-- This assignment should be sunk down to its first use.  (This will
-- increase code size if the register is used in multiple control flow
-- paths, but won't increase execution time, and the reduction of
-- register pressure is worth it.)
                | AlwaysSink CmmExpr
-- We cannot safely optimize occurrences of this local register. (This
-- corresponds to top in the lattice structure.)
                | NeverOptimize

-- Extract the expression that is being assigned to
xassign :: Assignment -> Maybe CmmExpr
xassign (AlwaysInline e) = Just e
xassign (AlwaysSink e)   = Just e
xassign NeverOptimize    = Nothing

-- Extracts the expression, but only if they're the same constructor
xassign2 :: (Assignment, Assignment) -> Maybe (CmmExpr, CmmExpr)
xassign2 (AlwaysInline e, AlwaysInline e') = Just (e, e')
xassign2 (AlwaysSink e, AlwaysSink e')     = Just (e, e')
xassign2 _ = Nothing

-- Note: We'd like to make decisions about "not optimizing" as soon as
-- possible, because this will make running the transfer function more
-- efficient.
type AssignmentMap = UniqFM Assignment

assignmentLattice :: DataflowLattice AssignmentMap
assignmentLattice = DataflowLattice "assignments for registers" emptyUFM (joinUFM add)
    where add _ (OldFact old) (NewFact new)
            = case (old, new) of
                (NeverOptimize, _) -> (NoChange,   NeverOptimize)
                (_, NeverOptimize) -> (SomeChange, NeverOptimize)
                (xassign2 -> Just (e, e'))
                    | e == e'   -> (NoChange, old)
                    | otherwise -> (SomeChange, NeverOptimize)
                _ -> (SomeChange, NeverOptimize)

-- Deletes sinks from assignment map, because /this/ is the place
-- where it will be sunk to.
deleteSinks :: UserOfLocalRegs n => n -> AssignmentMap -> AssignmentMap
deleteSinks n m = foldRegsUsed (adjustUFM f) m n
  where f (AlwaysSink _) = NeverOptimize
        f old = old

-- Invalidates any expressions that use a register.
invalidateUsersOf :: CmmReg -> AssignmentMap -> AssignmentMap
invalidateUsersOf reg = mapUFM (invalidateUsers' reg)
  where invalidateUsers' reg (xassign -> Just e) | reg `regUsedIn` e = NeverOptimize
        invalidateUsers' _ old = old

middleAssignment :: WithRegUsage CmmNode O O -> AssignmentMap -> AssignmentMap

-- Algorithm for annotated assignments:
--  1. Delete any sinking assignments that were used by this instruction
--  2. Add the assignment to our list of valid local assignments with
--     the correct optimization policy.
--  3. Look for all assignments that reference that register and
--     invalidate them.
middleAssignment n@(AssignLocal r e usage) assign
    = invalidateUsersOf (CmmLocal r) . add . deleteSinks n $ assign
      where add m = addToUFM m r
                  $ case usage of
                        SingleUse -> AlwaysInline e
                        ManyUse   -> decide e
            decide CmmLit{}       = AlwaysInline e
            decide CmmReg{}       = AlwaysInline e
            decide CmmLoad{}      = AlwaysSink e
            decide CmmStackSlot{} = AlwaysSink e
            decide CmmMachOp{}    = AlwaysSink e
            decide CmmRegOff{}    = AlwaysSink e

-- Algorithm for unannotated assignments of global registers:
-- 1. Delete any sinking assignments that were used by this instruction
-- 2. Look for all assignments that reference this register and
--    invalidate them.
middleAssignment (Plain n@(CmmAssign reg@(CmmGlobal _) _)) assign
    = invalidateUsersOf reg . deleteSinks n $ assign

-- Algorithm for unannotated assignments of *local* registers: do
-- nothing (it's a reload, so no state should have changed)
middleAssignment (Plain (CmmAssign (CmmLocal _) _)) assign = assign

-- Algorithm for stores:
--  1. Delete any sinking assignments that were used by this instruction
--  2. Look for all assignments that load from memory locations that
--     were clobbered by this store and invalidate them.
middleAssignment (Plain n@(CmmStore lhs rhs)) assign
    = mapUFM_Directly p . deleteSinks n $ assign
      -- ToDo: There's a missed opportunity here: even if a memory
      -- access we're attempting to sink gets clobbered at some
      -- location, it's still /better/ to sink it to right before the
      -- point where it gets clobbered.  How might we do this?
      -- Unfortunately, it's too late to change the assignment...
      where p r (xassign -> Just x) | (lhs, rhs) `clobbers` (r, x) = NeverOptimize
            p _ old = old

-- Assumption: Unsafe foreign calls don't clobber memory
middleAssignment (Plain n@(CmmUnsafeForeignCall{})) assign
    = foldRegsDefd (\m r -> addToUFM m r NeverOptimize) (deleteSinks n assign) n

middleAssignment (Plain (CmmComment {})) assign
    = assign

-- Assumptions:
--  * Stack slots do not overlap with any other memory locations
--  * Non stack-slot stores always conflict with each other.  (This is
--    not always the case; we could probably do something special for Hp)
--  * Stack slots for different areas do not overlap
--  * Stack slots within the same area and different offsets may
--    overlap; we need to do a size check (see 'overlaps').
clobbers :: (CmmExpr, CmmExpr) -> (Unique, CmmExpr) -> Bool
clobbers (ss@CmmStackSlot{}, CmmReg (CmmLocal r)) (u, CmmLoad (ss'@CmmStackSlot{}) _)
    | getUnique r == u, ss == ss' = False -- No-op on the stack slot (XXX: Do we need this special case?)
clobbers (CmmStackSlot (CallArea a) o, rhs) (_, expr) = f expr
    where f (CmmLoad (CmmStackSlot (CallArea a') o') t)
            = (a, o, widthInBytes (cmmExprWidth rhs)) `overlaps` (a', o', widthInBytes (typeWidth t))
          f (CmmLoad e _)    = containsStackSlot e
          f (CmmMachOp _ es) = or (map f es)
          f _                = False
          -- Maybe there's an invariant broken if this actually ever
          -- returns True
          containsStackSlot (CmmLoad{})      = True -- load of a load, all bets off
          containsStackSlot (CmmMachOp _ es) = or (map containsStackSlot es)
          containsStackSlot (CmmStackSlot{}) = True
          containsStackSlot _ = False
clobbers _ (_, e) = f e
    where f (CmmLoad (CmmStackSlot _ _) _) = False
          f (CmmLoad{}) = True -- conservative
          f (CmmMachOp _ es) = or (map f es)
          f _ = False

-- Check for memory overlapping.
-- Diagram:
--      4      8     12
--      s -w-  o
--      [ I32  ]
--      [    F64     ]
--      s'   -w'-    o'
type CallSubArea = (AreaId, Int, Int) -- area, offset, width
overlaps :: CallSubArea -> CallSubArea -> Bool
overlaps (a, _, _) (a', _, _) | a /= a' = False
overlaps (_, o, w) (_, o', w') =
    let s  = o  - w
        s' = o' - w'
    in (s' < o) && (s < o) -- Not LTE, because [ I32  ][ I32  ] is OK

lastAssignment :: WithRegUsage CmmNode O C -> AssignmentMap -> [(Label, AssignmentMap)]
-- Variables are dead across calls, so invalidating all mappings is justified
lastAssignment (Plain (CmmCall _ (Just k) _ _ _)) assign = [(k, mapUFM (const NeverOptimize) assign)]
lastAssignment (Plain (CmmForeignCall {succ=k}))  assign = [(k, mapUFM (const NeverOptimize) assign)]
lastAssignment l assign = map (\id -> (id, assign)) $ successors l

assignmentTransfer :: FwdTransfer (WithRegUsage CmmNode) AssignmentMap
assignmentTransfer = mkFTransfer3 (flip const) middleAssignment ((mkFactBase assignmentLattice .) . lastAssignment)

assignmentRewrite :: FwdRewrite FuelUniqSM (WithRegUsage CmmNode) AssignmentMap
assignmentRewrite = mkFRewrite3 first middle last
    where
        first _ _ = return Nothing
        middle (Plain m) assign = return $ rewrite assign (precompute assign m) mkMiddle m
        middle _ _ = return Nothing
        last (Plain l) assign = return $ rewrite assign (precompute assign l) mkLast l
        -- Tuple is (inline?, reloads)
        precompute assign n = foldRegsUsed f (False, []) n -- duplicates are harmless
            where f (i, l) r = case lookupUFM assign r of
                                Just (AlwaysSink e)   -> (i, (Plain (CmmAssign (CmmLocal r) e)):l)
                                Just (AlwaysInline _) -> (True, l)
                                Just NeverOptimize    -> (i, l)
                                -- This case can show up when we have
                                -- limited optimization fuel.
                                Nothing -> (i, l)
        rewrite _ (False, []) _ _ = Nothing
        -- Note [CmmCall Inline Hack]
        -- ToDo: Conservative hack: don't do any inlining on CmmCalls, since
        -- the code produced here tends to be unproblematic and I need
        -- to write lint passes to ensure that we don't put anything in
        -- the arguments that could be construed as a global register by
        -- some later translation pass.  (For example, slots will turn
        -- into dereferences of Sp).  This is the same hack in spirit as
        -- was in cmm/CmmOpt.hs.  Fix this up to only bug out if certain
        -- CmmExprs are involved.
        -- ToDo: We miss an opportunity here, where all possible
        -- inlinings should instead be sunk.
        rewrite _ (True, []) _ n | not (inlinable n) = Nothing -- see [CmmCall Inline Hack]
        rewrite assign (i, xs) mk n = Just $ mkMiddles xs <*> mk (Plain (inline i assign n))

        inline :: Bool -> AssignmentMap -> CmmNode e x -> CmmNode e x
        inline False _ n = n
        inline True  _ n | not (inlinable n) = n -- see [CmmCall Inline Hack]
        inline True assign n = mapExpDeep inlineExp n
            where inlineExp old@(CmmReg (CmmLocal r))
                    = case lookupUFM assign r of
                        Just (AlwaysInline x) -> x
                        _ -> old
                  inlineExp old@(CmmRegOff (CmmLocal r) i)
                    = case lookupUFM assign r of
                        Just (AlwaysInline x) -> CmmMachOp (MO_Add rep) [x, CmmLit (CmmInt (fromIntegral i) rep)]
                            where rep = typeWidth (localRegType r)
                        _ -> old
                  inlineExp old = old

        inlinable :: CmmNode e x -> Bool
        inlinable (CmmCall{}) = False
        inlinable (CmmForeignCall{}) = False
        inlinable _ = True

rewriteAssignments :: CmmGraph -> FuelUniqSM CmmGraph
rewriteAssignments g = do
  g'  <- annotateUsage g
  g'' <- liftM fst $ dataflowPassFwd g' [(g_entry g, fact_bot assignmentLattice)] $
                                     analRewFwd assignmentLattice assignmentTransfer assignmentRewrite
  return (modifyGraph eraseRegUsage g'')

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

-- ToDo: Outputable instance for UsageMap and AssignmentMap

my_trace :: String -> SDoc -> a -> a
my_trace = if False then pprTrace else \_ _ a -> a

f4sep :: [SDoc] -> SDoc
f4sep [] = fsep []
f4sep (d:ds) = fsep (d : map (nest 4) ds)

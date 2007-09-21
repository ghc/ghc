
module CmmSpillReload
  ( ExtendWithSpills(..)
  , DualLive(..)
  , dualLiveLattice, dualLiveness
  , insertSpillsAndReloads  --- XXX todo check live-in at entry against formals
  , dualLivenessWithInsertion
  , spillAndReloadComments

  , availRegsLattice
  , cmmAvailableReloads
  , insertLateReloads
  , insertLateReloads'
  , removeDeadAssignmentsAndReloads
  )
where

import CmmExpr
import CmmTx
import CmmLiveZ
import DFMonad
import MkZipCfg
import PprCmm()
import ZipCfg
import ZipCfgCmmRep
import ZipDataflow0

import FastString
import Maybes
import Outputable hiding (empty)
import qualified Outputable as PP
import Panic
import UniqSet
import UniqSupply

import Maybe
import Prelude hiding (zip)

-- The point of this module is to insert spills and reloads to
-- establish the invariant that at a call (or at any proc point with
-- an established protocol) all live variables not expected in
-- registers are sitting on the stack.  We use a backward analysis to
-- insert spills and reloads.  It should some day be followed by a
-- forward transformation to sink reloads as deeply as possible, so as
-- to reduce register pressure.

data ExtendWithSpills m
    = NotSpillOrReload m
    | Spill  RegSet
    | Reload RegSet

type M = ExtendWithSpills Middle

-- A variable can be expected to be live in a register, live on the
-- stack, or both.  This analysis ensures that spills and reloads are
-- inserted as needed to make sure that every live variable needed
-- after a call is available on the stack.  Spills are pushed back to
-- their reaching definitions, but reloads are dropped wherever needed
-- and will have to be sunk by a later forward transformation.

data DualLive = DualLive { on_stack :: RegSet, in_regs :: RegSet }

dualUnion :: DualLive -> DualLive -> DualLive
dualUnion (DualLive s r) (DualLive s' r') =
    DualLive (s `unionUniqSets` s') (r `unionUniqSets` r') 

dualUnionList :: [DualLive] -> DualLive
dualUnionList ls = DualLive ss rs
    where ss = unionManyUniqSets $ map on_stack ls
          rs = unionManyUniqSets $ map in_regs  ls

_changeStack, changeRegs :: (RegSet -> RegSet) -> DualLive -> DualLive
_changeStack f live = live { on_stack = f (on_stack live) }
changeRegs   f live = live { in_regs  = f (in_regs  live) }


dualLiveLattice :: DataflowLattice DualLive
dualLiveLattice =
      DataflowLattice "variables live in registers and on stack" empty add False
    where empty = DualLive emptyRegSet emptyRegSet
          -- | compute in the Tx monad to track whether anything has changed
          add new old = do stack <- add1 (on_stack new) (on_stack old)
                           regs  <- add1 (in_regs new)  (in_regs old)
                           return $ DualLive stack regs
          add1 = fact_add_to liveLattice

dualLivenessWithInsertion :: BPass M Last DualLive
dualLivenessWithInsertion = a_ft_b_unlimited dualLiveness insertSpillsAndReloads

dualLiveness :: BAnalysis M Last DualLive
dualLiveness = BComp "dual liveness" exit last middle first
    where exit   = empty
          last   = lastDualLiveness
          middle = middleDualLiveness
          first live _id = live
          empty = fact_bot dualLiveLattice

            -- ^ could take a proc-point set and choose to spill here,
            -- but it's probably better to run this pass, choose
            -- proc-point protocols, insert more CopyIn nodes, and run
            -- this pass again

middleDualLiveness :: DualLive -> M -> DualLive
middleDualLiveness live (Spill regs) = live'
    -- live-in on-stack requirements are satisfied;
    -- live-out in-regs obligations are created
    where live' = DualLive { on_stack = on_stack live `minusRegSet` regs
                           , in_regs  = in_regs  live `plusRegSet`  regs }

middleDualLiveness live (Reload regs) = live'
    -- live-in in-regs requirements are satisfied;
    -- live-out on-stack obligations are created
    where live' = DualLive { on_stack = on_stack live `plusRegSet`  regs
                           , in_regs  = in_regs  live `minusRegSet` regs }

middleDualLiveness live (NotSpillOrReload m) = changeRegs (middleLiveness m) live

lastDualLiveness :: (BlockId -> DualLive) -> Last -> DualLive
lastDualLiveness env l = last l
  where last (LastReturn)            = empty
        last (LastJump e)            = changeRegs (gen e) empty
        last (LastBranch id)         = env id
        last (LastCall tgt Nothing)  = changeRegs (gen tgt) empty
        last (LastCall tgt (Just k)) = 
            -- nothing can be live in registers at this point
            let live = env k in
            if  isEmptyUniqSet (in_regs live) then
                DualLive (on_stack live) (gen tgt emptyRegSet)
            else
                panic "live values in registers at call continuation"
        last (LastCondBranch e t f) = changeRegs (gen e) $ dualUnion (env t) (env f)
        last (LastSwitch e tbl)     = changeRegs (gen e) $ dualUnionList $
                                                             map env (catMaybes tbl)
        empty = fact_bot dualLiveLattice
                      
gen, kill :: UserOfLocalRegs a => a -> RegSet -> RegSet
gen  a live = foldRegsUsed extendRegSet      live a
kill a live = foldRegsUsed delOneFromUniqSet live a

insertSpillsAndReloads :: BFunctionalTransformation M Last DualLive
insertSpillsAndReloads = BComp "CPS spiller" exit last middle first
    where exit   = Nothing
          last   = \_ _ -> Nothing
          middle = middleInsertSpillsAndReloads
          first _ _ = Nothing
            -- ^ could take a proc-point set and choose to spill here,
            -- but it's probably better to run this pass, choose
            -- proc-point protocols, insert more CopyIn nodes, and run
            -- this pass again


middleInsertSpillsAndReloads :: DualLive -> M -> Maybe (Graph M Last)
middleInsertSpillsAndReloads _ (Spill _)  = Nothing
middleInsertSpillsAndReloads _ (Reload _) = Nothing
middleInsertSpillsAndReloads live m@(NotSpillOrReload nsr) = middle nsr
  where middle (MidAssign (CmmLocal reg) _) = 
            if reg `elemRegSet` on_stack live then -- must spill
                my_trace "Spilling" (f4sep [text "spill" <+> ppr reg,
                                            text "after", ppr m]) $
                Just $ graphOfMiddles [m, Spill $ mkRegSet [reg]]
            else
                Nothing
        middle (CopyIn _ formals _) = 
            -- only 'formals' can be in regs at this point
            let regs' = kill formals (in_regs live) -- live in regs; must reload
                is_stack_var r = elemRegSet r (on_stack live)
                needs_spilling = filterRegsUsed is_stack_var formals
                   -- a formal that is expected on the stack; must spill
            in  if isEmptyUniqSet regs' && isEmptyUniqSet needs_spilling then
                    Nothing
                else
                    let code  = if isEmptyUniqSet regs' then []
                                else Reload regs' : []
                        code' = if isEmptyUniqSet needs_spilling then code
                                else Spill needs_spilling : code
                    in
                    my_trace "At CopyIn" (f4sep [text "Triggered by ", ppr live,
                                                 ppr (Reload regs' :: M),
                                                 ppr (Spill needs_spilling :: M),
                                                 text "after", ppr m]) $
                    Just $ graphOfMiddles (m : code')
        middle _ = Nothing
                      
-- | For conversion back to vanilla C--
spillAndReloadComments :: M -> Middle
spillAndReloadComments (NotSpillOrReload m) = m
spillAndReloadComments (Spill  regs) = show_regs "Spill" regs
spillAndReloadComments (Reload regs) = show_regs "Reload" regs

show_regs :: String -> RegSet -> Middle
show_regs s regs = MidComment $ mkFastString $ showSDoc $ ppr_regs s regs


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
availRegsLattice = DataflowLattice "register gotten from reloads" empty add True
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

deleteFromAvail :: AvailRegs -> LocalReg -> AvailRegs
deleteFromAvail (UniverseMinus s) r = UniverseMinus (extendRegSet s r)
deleteFromAvail (AvailRegs     s) r = AvailRegs (deleteFromRegSet s r)

elemAvail :: AvailRegs -> LocalReg -> Bool
elemAvail (UniverseMinus s) r = not $ elemRegSet r s
elemAvail (AvailRegs     s) r = elemRegSet r s

cmmAvailableReloads :: LGraph M Last -> BlockEnv AvailRegs
cmmAvailableReloads g = env
    where env = runDFA availRegsLattice $
                do run_f_anal avail_reloads_transfer (fact_bot availRegsLattice) g
                   allFacts

avail_reloads_transfer :: FAnalysis M Last AvailRegs
avail_reloads_transfer = FComp "available-reloads analysis" first middle last exit
  where exit avail    = avail
        first avail _ = avail
        middle        = flip middleAvail
        last          = lastAvail


-- | The transfer equations use the traditional 'gen' and 'kill'
-- notations, which should be familiar from the dragon book.
agen, akill :: UserOfLocalRegs a => a -> AvailRegs -> AvailRegs
agen  a live = foldRegsUsed extendAvail     live a
akill a live = foldRegsUsed deleteFromAvail live a

middleAvail :: M -> AvailRegs -> AvailRegs
middleAvail (Spill _) = id
middleAvail (Reload regs) = agen regs
middleAvail (NotSpillOrReload m) = middle m
  where middle (MidComment {})                 = id
        middle (MidAssign lhs _expr)           = akill lhs
        middle (MidStore {})                   = id
        middle (MidUnsafeCall _tgt ress _args) = akill ress
        middle (MidAddToContext {})             = id
        middle (CopyIn _ formals _)            = akill formals
        middle (CopyOut {})                    = id

lastAvail :: AvailRegs -> Last -> LastOutFacts AvailRegs
lastAvail _ (LastCall _ (Just k)) = LastOutFacts [(k, AvailRegs emptyRegSet)]
lastAvail avail l = LastOutFacts $ map (\id -> (id, avail)) $ succs l

insertLateReloads :: LGraph M Last -> FuelMonad (LGraph M Last)
insertLateReloads g = mapM_blocks insertM g
    where env = cmmAvailableReloads g
          avail id = lookupBlockEnv env id `orElse` AvailRegs emptyRegSet
          insertM b = fuelConsumingPass "late reloads" (insert b)
          insert (Block id tail) fuel = propagate (ZFirst id) (avail id) tail fuel
          propagate h avail (ZTail m t) fuel =
              let (h', fuel') = maybe_add_reload h avail m fuel in
              propagate (ZHead h' m) (middleAvail m avail) t fuel'
          propagate h avail (ZLast l) fuel =
              let (h', fuel') = maybe_add_reload h avail l fuel in
              (zipht h' (ZLast l), fuel')
          maybe_add_reload h avail node fuel =
              let used = filterRegsUsed (elemAvail avail) node
              in  if not (canRewriteWithFuel fuel) || isEmptyUniqSet used then (h,fuel)
                  else (ZHead h (Reload used), oneLessFuel fuel)

insertLateReloads' :: UniqSupply -> (Graph M Last) -> FuelMonad (Graph M Last)
insertLateReloads' us g = 
    runDFM us availRegsLattice $
    f_shallow_rewrite avail_reloads_transfer insert bot g
  where bot = fact_bot availRegsLattice
        insert = null_f_ft { fc_middle_out = middle, fc_last_outs = last }
        middle :: AvailRegs -> M -> Maybe (Graph M Last)
        last   :: AvailRegs -> Last -> Maybe (Graph M Last)
        middle avail m = maybe_reload_before avail m (ZTail m (ZLast LastExit))
        last avail l   = maybe_reload_before avail l (ZLast (LastOther l))
        maybe_reload_before avail node tail =
            let used = filterRegsUsed (elemAvail avail) node
            in  if isEmptyUniqSet used then Nothing
                else Just $ graphOfZTail $ ZTail (Reload used) tail

_lateReloadsWithoutFuel :: LGraph M Last -> LGraph M Last
_lateReloadsWithoutFuel g = map_blocks insert g
    where env = cmmAvailableReloads g
          avail id = lookupBlockEnv env id `orElse` AvailRegs emptyRegSet
          insert (Block id tail) = propagate (ZFirst id) (avail id) tail
          propagate h avail (ZTail m t) =
            propagate (ZHead (maybe_add_reload h avail m) m) (middleAvail m avail) t 
          propagate h avail (ZLast l) =
            zipht (maybe_add_reload h avail l) (ZLast l)
          maybe_add_reload h avail node =
              let used = filterRegsUsed (elemAvail avail) node
              in  if isEmptyUniqSet used then h
                  else ZHead h (Reload used)


removeDeadAssignmentsAndReloads :: BPass M Last DualLive
removeDeadAssignmentsAndReloads = a_ft_b dualLiveness remove_deads
    where remove_deads = BComp "dead-assignment & -reload elim" exit last middle first
          exit   = Nothing
          last   = \_ _ -> Nothing
          middle = middleRemoveDeads
          first _ _ = Nothing

middleRemoveDeads :: DualLive -> M -> Maybe (Graph M Last)
middleRemoveDeads _ (Spill _)  = Nothing
middleRemoveDeads live (Reload s) =
    if sizeUniqSet worth_reloading < sizeUniqSet s then
        Just $ if isEmptyUniqSet worth_reloading then emptyGraph
               else graphOfMiddles [Reload worth_reloading]
    else
        Nothing
  where worth_reloading = intersectUniqSets s (in_regs live)
middleRemoveDeads live (NotSpillOrReload m) = middle m 
  where middle (MidAssign (CmmLocal reg') _)
               | not (reg' `elemRegSet` in_regs live) = Just emptyGraph
        middle _ = Nothing
                      


---------------------
-- register usage

instance UserOfLocalRegs m => UserOfLocalRegs (ExtendWithSpills m) where
    foldRegsUsed  f z (Spill  regs) = foldRegsUsed f z regs
    foldRegsUsed _f z (Reload _)    = z
    foldRegsUsed  f z (NotSpillOrReload m) = foldRegsUsed f z m

---------------------
-- prettyprinting

instance Outputable m => Outputable (ExtendWithSpills m) where
    ppr (Spill  regs) = ppr_regs "Spill"  regs
    ppr (Reload regs) = ppr_regs "Reload" regs
    ppr (NotSpillOrReload m) = ppr m

instance Outputable (LGraph M Last) where
    ppr = pprLgraph

instance DebugNodes M Last
                               
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

{-# OPTIONS -Wall -fno-warn-name-shadowing #-}

module CmmSpillReload
  ( ExtendWithSpills(..)
  , DualLive(..)
  , dualLiveLattice, dualLiveness
  , insertSpillsAndReloads  --- XXX todo check live-in at entry against formals
  , dualLivenessWithInsertion
  , spillAndReloadComments
  )
where
import CmmExpr
import CmmTx()
import CmmLiveZ
import DFMonad
import FastString
import Maybe
import MkZipCfg
import Outputable hiding (empty)
import qualified Outputable as PP
import Panic
import PprCmm()
import UniqSet
import ZipCfg
import ZipCfgCmm
import ZipDataflow

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
middleDualLiveness live m@(Spill regs) =
    -- live-in on-stack requirements are satisfied;
    -- live-out in-regs obligations are created
      my_trace "before" (f4sep [ppr m, text "liveness is", ppr live']) $
      live'
    where live' = DualLive { on_stack = on_stack live `minusRegSet` regs
                           , in_regs = in_regs live `plusRegSet` regs }

middleDualLiveness live m@(Reload regs) =
    -- live-in in-regs requirements are satisfied;
    -- live-out on-stack obligations are created
      my_trace "before" (f4sep [ppr m, text "liveness is", ppr live']) $
      live'
    where live' = DualLive { on_stack = on_stack live `plusRegSet` regs
                           , in_regs = in_regs live `minusRegSet` regs }

middleDualLiveness live (NotSpillOrReload m) = middle m live
  where middle (MidNop)                         = id 
        middle (MidComment {})                  = id 
        middle (MidAssign (CmmLocal reg') expr) = changeRegs (gen expr . kill reg')
        middle (MidAssign (CmmGlobal _) expr)   = changeRegs (gen expr) 
        middle (MidStore addr rval)             = changeRegs (gen addr . gen rval) 
        middle (MidUnsafeCall _ ress args)      = changeRegs (gen args . kill ress) 
        middle (CopyIn  _ formals _)            = changeRegs (kill formals)
        middle (CopyOut _ formals)              = changeRegs (gen  formals)

lastDualLiveness :: (BlockId -> DualLive) -> Last -> DualLive
lastDualLiveness env l = last l
  where last (LastReturn ress)            = changeRegs (gen ress) empty
        last (LastJump e args)            = changeRegs (gen e . gen args) empty
        last (LastBranch id args)         = changeRegs (gen args) $ env id
        last (LastCall tgt args Nothing)  = changeRegs (gen tgt. gen args) empty
        last (LastCall tgt args (Just k)) = 
            -- nothing can be live in registers at this point
            -- only 'formals' can be in regs at this point
            let live = env k in
            if  isEmptyUniqSet (in_regs live) then
                DualLive (on_stack live) (gen tgt $ gen args emptyRegSet)
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
middleInsertSpillsAndReloads live (NotSpillOrReload m) = middle m 
  where middle (MidAssign (CmmLocal reg') _) = 
            if reg' `elemRegSet` on_stack live then -- must spill
                my_trace "Spilling" (f4sep [text "spill" <+> ppr reg',
                                            text "after", ppr m]) $
                Just $ graphOfMiddles [NotSpillOrReload m, Spill $ mkRegSet [reg']]
            else
                Nothing
        middle (CopyIn _ formals _) = 
            -- only 'formals' can be in regs at this point
            let regs' = kill formals (in_regs live) -- live in regs; must reload
                is_stack_var r = elemRegSet r (on_stack live)
                needs_spilling = -- a formal that is expected on the stack; must spill
                   foldRegsUsed (\rs r -> if is_stack_var r then extendRegSet rs r
                                          else rs) emptyRegSet formals
            in  if isEmptyUniqSet regs' && isEmptyUniqSet needs_spilling then
                    Nothing
                else
                    let reload = if isEmptyUniqSet regs' then []
                                 else [Reload regs']
                        spill_reload = if isEmptyUniqSet needs_spilling then reload
                                       else Spill needs_spilling : reload
                        middles = NotSpillOrReload m : spill_reload
                    in
                    my_trace "At CopyIn" (f4sep [text "Triggered by ", ppr live,
                                                 ppr (Reload regs' :: M),
                                                 ppr (Spill needs_spilling :: M),
                                                 text "after", ppr m]) $
                    Just $ graphOfMiddles middles
        middle _ = Nothing
                      
-- | For conversion back to vanilla C--
spillAndReloadComments :: M -> Middle
spillAndReloadComments (NotSpillOrReload m) = m
spillAndReloadComments (Spill  regs) = show_regs "Spill" regs
spillAndReloadComments (Reload regs) = show_regs "Reload" regs

show_regs :: String -> RegSet -> Middle
show_regs s regs = MidComment $ mkFastString $ showSDoc $ ppr_regs s regs


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

my_trace :: String -> SDoc -> a -> a
my_trace = if False then pprTrace else \_ _ a -> a

f4sep :: [SDoc] -> SDoc
f4sep [] = fsep []
f4sep (d:ds) = fsep (d : map (nest 4) ds)

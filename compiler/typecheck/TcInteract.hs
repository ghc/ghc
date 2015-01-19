{-# LANGUAGE CPP #-}

module TcInteract (
     solveSimpleGivens,    -- Solves [EvVar],GivenLoc
     solveSimpleWanteds    -- Solves Cts
  ) where

#include "HsVersions.h"

import BasicTypes ()
import HsTypes ( hsIPNameFS )
import FastString
import TcCanonical
import TcFlatten
import VarSet
import Type
import Kind (isKind, isConstraintKind )
import Unify
import InstEnv( lookupInstEnv, instanceDFunId )
import CoAxiom(sfInteractTop, sfInteractInert)

import Var
import TcType
import PrelNames ( knownNatClassName, knownSymbolClassName, ipClassNameKey
                 , typeableClassName, callStackTyConKey )
import Id( idType )
import Class
import TyCon
import FunDeps
import FamInst
import Inst( tyVarsOfCt )

import TcEvidence
import Outputable

import TcRnTypes
import TcErrors
import TcSMonad
import Bag

import Data.List( partition, foldl', deleteFirstsBy )

import VarEnv

import Control.Monad
import Pair (Pair(..))
import Unique( hasKey )
import DynFlags
import Util

{-
**********************************************************************
*                                                                    *
*                      Main Interaction Solver                       *
*                                                                    *
**********************************************************************

Note [Basic Simplifier Plan]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
1. Pick an element from the WorkList if there exists one with depth
   less than our context-stack depth.

2. Run it down the 'stage' pipeline. Stages are:
      - canonicalization
      - inert reactions
      - spontaneous reactions
      - top-level intreactions
   Each stage returns a StopOrContinue and may have sideffected
   the inerts or worklist.

   The threading of the stages is as follows:
      - If (Stop) is returned by a stage then we start again from Step 1.
      - If (ContinueWith ct) is returned by a stage, we feed 'ct' on to
        the next stage in the pipeline.
4. If the element has survived (i.e. ContinueWith x) the last stage
   then we add him in the inerts and jump back to Step 1.

If in Step 1 no such element exists, we have exceeded our context-stack
depth and will simply fail.

Note [Unflatten after solving the simple wanteds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We unflatten after solving the wc_simples of an implication, and before attempting
to float. This means that

 * The fsk/fmv flatten-skolems only survive during solveSimples.  We don't
   need to worry about then across successive passes over the constraint tree.
   (E.g. we don't need the old ic_fsk field of an implication.

 * When floating an equality outwards, we don't need to worry about floating its
   associated flattening constraints.

 * Another tricky case becomes easy: Trac #4935
       type instance F True a b = a
       type instance F False a b = b

       [w] F c a b ~ gamma
       (c ~ True) => a ~ gamma
       (c ~ False) => b ~ gamma

   Obviously this is soluble with gamma := F c a b, and unflattening
   will do exactly that after solving the simple constraints and before
   attempting the implications.  Before, when we were not unflattening,
   we had to push Wanted funeqs in as new givens.  Yuk!

   Another example that becomes easy: indexed_types/should_fail/T7786
      [W] BuriedUnder sub k Empty ~ fsk
      [W] Intersect fsk inv ~ s
      [w] xxx[1] ~ s
      [W] forall[2] . (xxx[1] ~ Empty)
                   => Intersect (BuriedUnder sub k Empty) inv ~ Empty

Note [Running plugins on unflattened wanteds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There is an annoying mismatch between solveSimpleGivens and
solveSimpleWanteds, because the latter needs to fiddle with the inert
set, unflatten and and zonk the wanteds.  It passes the zonked wanteds
to runTcPluginsWanteds, which produces a replacement set of wanteds,
some additional insolubles and a flag indicating whether to go round
the loop again.  If so, prepareInertsForImplications is used to remove
the previous wanteds (which will still be in the inert set).  Note
that prepareInertsForImplications will discard the insolubles, so we
must keep track of them separately.
-}

solveSimpleGivens :: CtLoc -> [EvVar] -> TcS ()
solveSimpleGivens loc givens
  | null givens  -- Shortcut for common case
  = return ()
  | otherwise
  = go (map mk_given_ct givens)
  where
    mk_given_ct ev_id = mkNonCanonical (CtGiven { ctev_evtm = EvId ev_id
                                                , ctev_pred = evVarPred ev_id
                                                , ctev_loc  = loc })
    go givens = do { solveSimples (listToBag givens)
                   ; new_givens <- runTcPluginsGiven
                   ; when (notNull new_givens) (go new_givens)
                   }

solveSimpleWanteds :: Cts -> TcS WantedConstraints
solveSimpleWanteds = go emptyBag
  where
    go insols0 wanteds
      = do { solveSimples wanteds
           ; (implics, tv_eqs, fun_eqs, insols, others) <- getUnsolvedInerts
           ; unflattened_eqs <- unflatten tv_eqs fun_eqs
              -- See Note [Unflatten after solving the simple wanteds]

           ; zonked <- zonkSimples (others `andCts` unflattened_eqs)
             -- Postcondition is that the wl_simples are zonked

           ; (wanteds', insols', rerun) <- runTcPluginsWanted zonked
              -- See Note [Running plugins on unflattened wanteds]
           ; let all_insols = insols0 `unionBags` insols `unionBags` insols'
           ; if rerun then do { updInertTcS prepareInertsForImplications
                              ; go all_insols wanteds' }
                      else return (WC { wc_simple = wanteds'
                                      , wc_insol  = all_insols
                                      , wc_impl   = implics }) }


-- The main solver loop implements Note [Basic Simplifier Plan]
---------------------------------------------------------------
solveSimples :: Cts -> TcS ()
-- Returns the final InertSet in TcS
-- Has no effect on work-list or residual-iplications
-- The constraints are initially examined in left-to-right order

solveSimples cts
  = {-# SCC "solveSimples" #-}
    do { dyn_flags <- getDynFlags
       ; updWorkListTcS (\wl -> foldrBag extendWorkListCt wl cts)
       ; solve_loop (maxSubGoalDepth dyn_flags) }
  where
    solve_loop max_depth
      = {-# SCC "solve_loop" #-}
        do { sel <- selectNextWorkItem max_depth
           ; case sel of
              NoWorkRemaining     -- Done, successfuly (modulo frozen)
                -> return ()
              MaxDepthExceeded cnt ct -- Failure, depth exceeded
                -> wrapErrTcS $ solverDepthErrorTcS cnt (ctEvidence ct)
              NextWorkItem ct     -- More work, loop around!
                -> do { runSolverPipeline thePipeline ct; solve_loop max_depth } }


-- | Extract the (inert) givens and invoke the plugins on them.
-- Remove solved givens from the inert set and emit insolubles, but
-- return new work produced so that 'solveSimpleGivens' can feed it back
-- into the main solver.
runTcPluginsGiven :: TcS [Ct]
runTcPluginsGiven = do
  (givens,_,_) <- fmap splitInertCans getInertCans
  if null givens
    then return []
    else do
      p <- runTcPlugins (givens,[],[])
      let (solved_givens, _, _) = pluginSolvedCts p
      updInertCans (removeInertCts solved_givens)
      mapM_ emitInsoluble (pluginBadCts p)
      return (pluginNewCts p)

-- | Given a bag of (flattened, zonked) wanteds, invoke the plugins on
-- them and produce an updated bag of wanteds (possibly with some new
-- work) and a bag of insolubles.  The boolean indicates whether
-- 'solveSimpleWanteds' should feed the updated wanteds back into the
-- main solver.
runTcPluginsWanted :: Cts -> TcS (Cts, Cts, Bool)
runTcPluginsWanted zonked_wanteds
  | isEmptyBag zonked_wanteds = return (zonked_wanteds, emptyBag, False)
  | otherwise                 = do
    (given,derived,_) <- fmap splitInertCans getInertCans
    p <- runTcPlugins (given, derived, bagToList zonked_wanteds)
    let (solved_givens, solved_deriveds, solved_wanteds) = pluginSolvedCts p
        (_, _, wanteds) = pluginInputCts p
    updInertCans (removeInertCts $ solved_givens ++ solved_deriveds)
    mapM_ setEv solved_wanteds
    return ( listToBag $ pluginNewCts p ++ wanteds
           , listToBag $ pluginBadCts p
           , notNull (pluginNewCts p) )
  where
    setEv :: (EvTerm,Ct) -> TcS ()
    setEv (ev,ct) = case ctEvidence ct of
      CtWanted {ctev_evar = evar} -> setEvBind evar ev
      _ -> panic "runTcPluginsWanted.setEv: attempt to solve non-wanted!"

-- | A triple of (given, derived, wanted) constraints to pass to plugins
type SplitCts  = ([Ct], [Ct], [Ct])

-- | A solved triple of constraints, with evidence for wanteds
type SolvedCts = ([Ct], [Ct], [(EvTerm,Ct)])

-- | Represents collections of constraints generated by typechecker
-- plugins
data TcPluginProgress = TcPluginProgress
    { pluginInputCts  :: SplitCts
      -- ^ Original inputs to the plugins with solved/bad constraints
      -- removed, but otherwise unmodified
    , pluginSolvedCts :: SolvedCts
      -- ^ Constraints solved by plugins
    , pluginBadCts    :: [Ct]
      -- ^ Constraints reported as insoluble by plugins
    , pluginNewCts    :: [Ct]
      -- ^ New constraints emitted by plugins
    }

-- | Starting from a triple of (given, derived, wanted) constraints,
-- invoke each of the typechecker plugins in turn and return
--
--  * the remaining unmodified constraints,
--  * constraints that have been solved,
--  * constraints that are insoluble, and
--  * new work.
--
-- Note that new work generated by one plugin will not be seen by
-- other plugins on this pass (but the main constraint solver will be
-- re-invoked and they will see it later).  There is no check that new
-- work differs from the original constraints supplied to the plugin:
-- the plugin itself should perform this check if necessary.
runTcPlugins :: SplitCts -> TcS TcPluginProgress
runTcPlugins all_cts = do
    gblEnv <- getGblEnv
    foldM do_plugin initialProgress (tcg_tc_plugins gblEnv)
  where
    do_plugin :: TcPluginProgress -> TcPluginSolver -> TcS TcPluginProgress
    do_plugin p solver = do
        result <- runTcPluginTcS (uncurry3 solver (pluginInputCts p))
        return $ progress p result

    progress :: TcPluginProgress -> TcPluginResult -> TcPluginProgress
    progress p (TcPluginContradiction bad_cts) =
       p { pluginInputCts = discard bad_cts (pluginInputCts p)
         , pluginBadCts   = bad_cts ++ pluginBadCts p
         }
    progress p (TcPluginOk solved_cts new_cts) =
      p { pluginInputCts  = discard (map snd solved_cts) (pluginInputCts p)
        , pluginSolvedCts = add solved_cts (pluginSolvedCts p)
        , pluginNewCts    = new_cts ++ pluginNewCts p
        }

    initialProgress = TcPluginProgress all_cts ([], [], []) [] []

    discard :: [Ct] -> SplitCts -> SplitCts
    discard cts (xs, ys, zs) =
        (xs `without` cts, ys `without` cts, zs `without` cts)

    without :: [Ct] -> [Ct] -> [Ct]
    without = deleteFirstsBy eqCt

    eqCt :: Ct -> Ct -> Bool
    eqCt c c' = case (ctEvidence c, ctEvidence c') of
      (CtGiven   pred _ _, CtGiven   pred' _ _) -> pred `eqType` pred'
      (CtWanted  pred _ _, CtWanted  pred' _ _) -> pred `eqType` pred'
      (CtDerived pred _  , CtDerived pred' _  ) -> pred `eqType` pred'
      (_                 , _                  ) -> False

    add :: [(EvTerm,Ct)] -> SolvedCts -> SolvedCts
    add xs scs = foldl' addOne scs xs

    addOne :: SolvedCts -> (EvTerm,Ct) -> SolvedCts
    addOne (givens, deriveds, wanteds) (ev,ct) = case ctEvidence ct of
      CtGiven  {} -> (ct:givens, deriveds, wanteds)
      CtDerived{} -> (givens, ct:deriveds, wanteds)
      CtWanted {} -> (givens, deriveds, (ev,ct):wanteds)


type WorkItem = Ct
type SimplifierStage = WorkItem -> TcS (StopOrContinue Ct)

data SelectWorkItem
       = NoWorkRemaining      -- No more work left (effectively we're done!)
       | MaxDepthExceeded SubGoalCounter Ct
                              -- More work left to do but this constraint has exceeded
                              -- the maximum depth for one of the subgoal counters and we
                              -- must stop
       | NextWorkItem Ct      -- More work left, here's the next item to look at

selectNextWorkItem :: SubGoalDepth -- Max depth allowed
                   -> TcS SelectWorkItem
selectNextWorkItem max_depth
  = updWorkListTcS_return pick_next
  where
    pick_next :: WorkList -> (SelectWorkItem, WorkList)
    pick_next wl
      = case selectWorkItem wl of
          (Nothing,_)
              -> (NoWorkRemaining,wl)           -- No more work
          (Just ct, new_wl)
              | Just cnt <- subGoalDepthExceeded max_depth (ctLocDepth (ctLoc ct)) -- Depth exceeded
              -> (MaxDepthExceeded cnt ct,new_wl)
          (Just ct, new_wl)
              -> (NextWorkItem ct, new_wl)      -- New workitem and worklist

runSolverPipeline :: [(String,SimplifierStage)] -- The pipeline
                  -> WorkItem                   -- The work item
                  -> TcS ()
-- Run this item down the pipeline, leaving behind new work and inerts
runSolverPipeline pipeline workItem
  = do { initial_is <- getTcSInerts
       ; traceTcS "Start solver pipeline {" $
                  vcat [ ptext (sLit "work item = ") <+> ppr workItem
                       , ptext (sLit "inerts    = ") <+> ppr initial_is]

       ; bumpStepCountTcS    -- One step for each constraint processed
       ; final_res  <- run_pipeline pipeline (ContinueWith workItem)

       ; final_is <- getTcSInerts
       ; case final_res of
           Stop ev s       -> do { traceFireTcS ev s
                                 ; traceTcS "End solver pipeline (discharged) }"
                                       (ptext (sLit "inerts =") <+> ppr final_is)
                                 ; return () }
           ContinueWith ct -> do { traceFireTcS (ctEvidence ct) (ptext (sLit "Kept as inert"))
                                 ; traceTcS "End solver pipeline (not discharged) }" $
                                       vcat [ ptext (sLit "final_item =") <+> ppr ct
                                            , pprTvBndrs (varSetElems $ tyVarsOfCt ct)
                                            , ptext (sLit "inerts     =") <+> ppr final_is]
                                 ; insertInertItemTcS ct }
       }
  where run_pipeline :: [(String,SimplifierStage)] -> StopOrContinue Ct
                     -> TcS (StopOrContinue Ct)
        run_pipeline [] res        = return res
        run_pipeline _ (Stop ev s) = return (Stop ev s)
        run_pipeline ((stg_name,stg):stgs) (ContinueWith ct)
          = do { traceTcS ("runStage " ++ stg_name ++ " {")
                          (text "workitem   = " <+> ppr ct)
               ; res <- stg ct
               ; traceTcS ("end stage " ++ stg_name ++ " }") empty
               ; run_pipeline stgs res }

{-
Example 1:
  Inert:   {c ~ d, F a ~ t, b ~ Int, a ~ ty} (all given)
  Reagent: a ~ [b] (given)

React with (c~d)     ==> IR (ContinueWith (a~[b]))  True    []
React with (F a ~ t) ==> IR (ContinueWith (a~[b]))  False   [F [b] ~ t]
React with (b ~ Int) ==> IR (ContinueWith (a~[Int]) True    []

Example 2:
  Inert:  {c ~w d, F a ~g t, b ~w Int, a ~w ty}
  Reagent: a ~w [b]

React with (c ~w d)   ==> IR (ContinueWith (a~[b]))  True    []
React with (F a ~g t) ==> IR (ContinueWith (a~[b]))  True    []    (can't rewrite given with wanted!)
etc.

Example 3:
  Inert:  {a ~ Int, F Int ~ b} (given)
  Reagent: F a ~ b (wanted)

React with (a ~ Int)   ==> IR (ContinueWith (F Int ~ b)) True []
React with (F Int ~ b) ==> IR Stop True []    -- after substituting we re-canonicalize and get nothing
-}

thePipeline :: [(String,SimplifierStage)]
thePipeline = [ ("canonicalization",        TcCanonical.canonicalize)
              , ("interact with inerts",    interactWithInertsStage)
              , ("top-level reactions",     topReactionsStage) ]

{-
*********************************************************************************
*                                                                               *
                       The interact-with-inert Stage
*                                                                               *
*********************************************************************************

Note [The Solver Invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
We always add Givens first.  So you might think that the solver has
the invariant

   If the work-item is Given,
   then the inert item must Given

But this isn't quite true.  Suppose we have,
    c1: [W] beta ~ [alpha], c2 : [W] blah, c3 :[W] alpha ~ Int
After processing the first two, we get
     c1: [G] beta ~ [alpha], c2 : [W] blah
Now, c3 does not interact with the the given c1, so when we spontaneously
solve c3, we must re-react it with the inert set.  So we can attempt a
reaction between inert c2 [W] and work-item c3 [G].

It *is* true that [Solver Invariant]
   If the work-item is Given,
   AND there is a reaction
   then the inert item must Given
or, equivalently,
   If the work-item is Given,
   and the inert item is Wanted/Derived
   then there is no reaction
-}

-- Interaction result of  WorkItem <~> Ct

type StopNowFlag = Bool    -- True <=> stop after this interaction

interactWithInertsStage :: WorkItem -> TcS (StopOrContinue Ct)
-- Precondition: if the workitem is a CTyEqCan then it will not be able to
-- react with anything at this stage.

interactWithInertsStage wi
  = do { inerts <- getTcSInerts
       ; let ics = inert_cans inerts
       ; case wi of
             CTyEqCan    {} -> interactTyVarEq ics wi
             CFunEqCan   {} -> interactFunEq   ics wi
             CIrredEvCan {} -> interactIrred   ics wi
             CDictCan    {} -> interactDict    ics wi
             _ -> pprPanic "interactWithInerts" (ppr wi) }
                -- CHoleCan are put straight into inert_frozen, so never get here
                -- CNonCanonical have been canonicalised

data InteractResult = IRKeep | IRReplace | IRDelete
instance Outputable InteractResult where
  ppr IRKeep    = ptext (sLit "keep")
  ppr IRReplace = ptext (sLit "replace")
  ppr IRDelete  = ptext (sLit "delete")

solveOneFromTheOther :: CtEvidence  -- Inert
                     -> CtEvidence  -- WorkItem
                     -> TcS (InteractResult, StopNowFlag)
-- Preconditions:
-- 1) inert and work item represent evidence for the /same/ predicate
-- 2) ip/class/irred evidence (no coercions) only
solveOneFromTheOther ev_i ev_w
  | isDerived ev_w
  = return (IRKeep, True)

  | isDerived ev_i -- The inert item is Derived, we can just throw it away,
                   -- The ev_w is inert wrt earlier inert-set items,
                   -- so it's safe to continue on from this point
  = return (IRDelete, False)

  | CtWanted { ctev_evar = ev_id } <- ev_w
  = do { setEvBind ev_id (ctEvTerm ev_i)
       ; return (IRKeep, True) }

  | CtWanted { ctev_evar = ev_id } <- ev_i
  = do { setEvBind ev_id (ctEvTerm ev_w)
       ; return (IRReplace, True) }

  | otherwise      -- If both are Given, we already have evidence; no need to duplicate
                   -- But the work item *overrides* the inert item (hence IRReplace)
                   -- See Note [Shadowing of Implicit Parameters]
  = return (IRReplace, True)

{-
*********************************************************************************
*                                                                               *
                   interactIrred
*                                                                               *
*********************************************************************************
-}

-- Two pieces of irreducible evidence: if their types are *exactly identical*
-- we can rewrite them. We can never improve using this:
-- if we want ty1 :: Constraint and have ty2 :: Constraint it clearly does not
-- mean that (ty1 ~ ty2)
interactIrred :: InertCans -> Ct -> TcS (StopOrContinue Ct)

interactIrred inerts workItem@(CIrredEvCan { cc_ev = ev_w })
  | let pred = ctEvPred ev_w
        (matching_irreds, others) = partitionBag (\ct -> ctPred ct `tcEqType` pred)
                                                 (inert_irreds inerts)
  , (ct_i : rest) <- bagToList matching_irreds
  , let ctev_i = ctEvidence ct_i
  = ASSERT( null rest )
    do { (inert_effect, stop_now) <- solveOneFromTheOther ctev_i ev_w
       ; case inert_effect of
            IRKeep    -> return ()
            IRDelete  -> updInertIrreds (\_ -> others)
            IRReplace -> updInertIrreds (\_ -> others `snocCts` workItem)
                         -- These const upd's assume that solveOneFromTheOther
                         -- has no side effects on InertCans
       ; if stop_now then
            return (Stop ev_w (ptext (sLit "Irred equal") <+> parens (ppr inert_effect)))
       ; else
            continueWith workItem }

  | otherwise
  = continueWith workItem

interactIrred _ wi = pprPanic "interactIrred" (ppr wi)

{-
*********************************************************************************
*                                                                               *
                   interactDict
*                                                                               *
*********************************************************************************
-}

interactDict :: InertCans -> Ct -> TcS (StopOrContinue Ct)
interactDict inerts workItem@(CDictCan { cc_ev = ev_w, cc_class = cls, cc_tyargs = tys })
  -- don't ever try to solve CallStack IPs directly from other dicts,
  -- we always build new dicts instead.
  -- See Note [Overview of implicit CallStacks]
  | [_ip, ty] <- tys
  , isWanted ev_w
  , Just mkEvCs <- isCallStackIP (ctEvLoc ev_w) cls ty
  = do let ev_cs =
             case lookupInertDict inerts (ctEvLoc ev_w) cls tys of
               Just ev | isGiven ev -> mkEvCs (ctEvTerm ev)
               _ -> mkEvCs (EvCallStack EvCsEmpty)

       -- now we have ev_cs :: CallStack, but the evidence term should
       -- be a dictionary, so we have to coerce ev_cs to a
       -- dictionary for `IP ip CallStack`
       let ip_ty = mkClassPred cls tys
       let ev_tm = mkEvCast (EvCallStack ev_cs) (TcCoercion $ wrapIP ip_ty)
       addSolvedDict ev_w cls tys
       setEvBind (ctEvId ev_w) ev_tm
       stopWith ev_w "Wanted CallStack IP"

  | Just ctev_i <- lookupInertDict inerts (ctEvLoc ev_w) cls tys
  = do { (inert_effect, stop_now) <- solveOneFromTheOther ctev_i ev_w
       ; case inert_effect of
           IRKeep    -> return ()
           IRDelete  -> updInertDicts $ \ ds -> delDict ds cls tys
           IRReplace -> updInertDicts $ \ ds -> addDict ds cls tys workItem
       ; if stop_now then
            return (Stop ev_w (ptext (sLit "Dict equal") <+> parens (ppr inert_effect)))
         else
            continueWith workItem }

  | cls `hasKey` ipClassNameKey
  , isGiven ev_w
  = interactGivenIP inerts workItem

  | otherwise
  = do { mapBagM_ (addFunDepWork workItem) (findDictsByClass (inert_dicts inerts) cls)
               -- Standard thing: create derived fds and keep on going. Importantly we don't
               -- throw workitem back in the worklist because this can cause loops (see #5236)
       ; continueWith workItem  }

interactDict _ wi = pprPanic "interactDict" (ppr wi)

interactGivenIP :: InertCans -> Ct -> TcS (StopOrContinue Ct)
-- Work item is Given (?x:ty)
-- See Note [Shadowing of Implicit Parameters]
interactGivenIP inerts workItem@(CDictCan { cc_ev = ev, cc_class = cls
                                          , cc_tyargs = tys@(ip_str:_) })
  = do { updInertCans $ \cans -> cans { inert_dicts = addDict filtered_dicts cls tys workItem }
       ; stopWith ev "Given IP" }
  where
    dicts           = inert_dicts inerts
    ip_dicts        = findDictsByClass dicts cls
    other_ip_dicts  = filterBag (not . is_this_ip) ip_dicts
    filtered_dicts  = addDictsByClass dicts cls other_ip_dicts

    -- Pick out any Given constraints for the same implicit parameter
    is_this_ip (CDictCan { cc_ev = ev, cc_tyargs = ip_str':_ })
       = isGiven ev && ip_str `tcEqType` ip_str'
    is_this_ip _ = False

interactGivenIP _ wi = pprPanic "interactGivenIP" (ppr wi)

addFunDepWork :: Ct -> Ct -> TcS ()
addFunDepWork work_ct inert_ct
  = do {  let fd_eqns :: [Equation CtLoc]
              fd_eqns = [ eqn { fd_loc = derived_loc }
                        | eqn <- improveFromAnother inert_pred work_pred ]
       ; rewriteWithFunDeps fd_eqns
                -- We don't really rewrite tys2, see below _rewritten_tys2, so that's ok
                -- NB: We do create FDs for given to report insoluble equations that arise
                -- from pairs of Givens, and also because of floating when we approximate
                -- implications. The relevant test is: typecheck/should_fail/FDsFromGivens.hs
                -- Also see Note [When improvement happens]
    }
  where
    work_pred  = ctPred work_ct
    inert_pred = ctPred inert_ct
    work_loc   = ctLoc work_ct
    inert_loc  = ctLoc inert_ct
    derived_loc = work_loc { ctl_origin = FunDepOrigin1 work_pred  work_loc
                                                        inert_pred inert_loc }

{-
Note [Shadowing of Implicit Parameters]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the following example:

f :: (?x :: Char) => Char
f = let ?x = 'a' in ?x

The "let ?x = ..." generates an implication constraint of the form:

?x :: Char => ?x :: Char

Furthermore, the signature for `f` also generates an implication
constraint, so we end up with the following nested implication:

?x :: Char => (?x :: Char => ?x :: Char)

Note that the wanted (?x :: Char) constraint may be solved in
two incompatible ways:  either by using the parameter from the
signature, or by using the local definition.  Our intention is
that the local definition should "shadow" the parameter of the
signature, and we implement this as follows: when we add a new
*given* implicit parameter to the inert set, it replaces any existing
givens for the same implicit parameter.

This works for the normal cases but it has an odd side effect
in some pathological programs like this:

-- This is accepted, the second parameter shadows
f1 :: (?x :: Int, ?x :: Char) => Char
f1 = ?x

-- This is rejected, the second parameter shadows
f2 :: (?x :: Int, ?x :: Char) => Int
f2 = ?x

Both of these are actually wrong:  when we try to use either one,
we'll get two incompatible wnated constraints (?x :: Int, ?x :: Char),
which would lead to an error.

I can think of two ways to fix this:

  1. Simply disallow multiple constratits for the same implicit
    parameter---this is never useful, and it can be detected completely
    syntactically.

  2. Move the shadowing machinery to the location where we nest
     implications, and add some code here that will produce an
     error if we get multiple givens for the same implicit parameter.


*********************************************************************************
*                                                                               *
                   interactFunEq
*                                                                               *
*********************************************************************************
-}

interactFunEq :: InertCans -> Ct -> TcS (StopOrContinue Ct)
-- Try interacting the work item with the inert set
interactFunEq inerts workItem@(CFunEqCan { cc_ev = ev, cc_fun = tc
                                         , cc_tyargs = args, cc_fsk = fsk })
  | Just (CFunEqCan { cc_ev = ev_i, cc_fsk = fsk_i }) <- matching_inerts
  = if ev_i `canRewriteOrSame` ev
    then  -- Rewrite work-item using inert
      do { traceTcS "reactFunEq (discharge work item):" $
           vcat [ text "workItem =" <+> ppr workItem
                , text "inertItem=" <+> ppr ev_i ]
         ; reactFunEq ev_i fsk_i ev fsk
         ; stopWith ev "Inert rewrites work item" }
    else  -- Rewrite intert using work-item
      do { traceTcS "reactFunEq (rewrite inert item):" $
           vcat [ text "workItem =" <+> ppr workItem
                , text "inertItem=" <+> ppr ev_i ]
         ; updInertFunEqs $ \ feqs -> insertFunEq feqs tc args workItem
               -- Do the updInertFunEqs before the reactFunEq, so that
               -- we don't kick out the inertItem as well as consuming it!
         ; reactFunEq ev fsk ev_i fsk_i
         ; stopWith ev "Work item rewrites inert" }

  | Just ops <- isBuiltInSynFamTyCon_maybe tc
  = do { let matching_funeqs = findFunEqsByTyCon funeqs tc
       ; let interact = sfInteractInert ops args (lookupFlattenTyVar eqs fsk)
             do_one (CFunEqCan { cc_tyargs = iargs, cc_fsk = ifsk, cc_ev = iev })
                = mapM_ (unifyDerived (ctEvLoc iev) Nominal)
                        (interact iargs (lookupFlattenTyVar eqs ifsk))
             do_one ct = pprPanic "interactFunEq" (ppr ct)
       ; mapM_ do_one matching_funeqs
       ; traceTcS "builtInCandidates 1: " $ vcat [ ptext (sLit "Candidates:") <+> ppr matching_funeqs
                                                 , ptext (sLit "TvEqs:") <+> ppr eqs ]
       ; return (ContinueWith workItem) }

  | otherwise
  = return (ContinueWith workItem)
  where
    eqs    = inert_eqs inerts
    funeqs = inert_funeqs inerts
    matching_inerts = findFunEqs funeqs tc args

interactFunEq _ wi = pprPanic "interactFunEq" (ppr wi)

lookupFlattenTyVar :: TyVarEnv EqualCtList -> TcTyVar -> TcType
-- ^ Look up a flatten-tyvar in the inert nominal TyVarEqs;
-- this is used only when dealing with a CFunEqCan
lookupFlattenTyVar inert_eqs ftv
  = case lookupVarEnv inert_eqs ftv of
      Just (CTyEqCan { cc_rhs = rhs, cc_eq_rel = NomEq } : _) -> rhs
      _                                                       -> mkTyVarTy ftv

reactFunEq :: CtEvidence -> TcTyVar    -- From this  :: F tys ~ fsk1
           -> CtEvidence -> TcTyVar    -- Solve this :: F tys ~ fsk2
           -> TcS ()
reactFunEq from_this fsk1 (CtGiven { ctev_evtm = tm, ctev_loc = loc }) fsk2
  = do { let fsk_eq_co = mkTcSymCo (evTermCoercion tm)
                         `mkTcTransCo` ctEvCoercion from_this
                         -- :: fsk2 ~ fsk1
             fsk_eq_pred = mkTcEqPred (mkTyVarTy fsk2) (mkTyVarTy fsk1)
       ; new_ev <- newGivenEvVar loc (fsk_eq_pred, EvCoercion fsk_eq_co)
       ; emitWorkNC [new_ev] }

reactFunEq from_this fuv1 (CtWanted { ctev_evar = evar }) fuv2
  = dischargeFmv evar fuv2 (ctEvCoercion from_this) (mkTyVarTy fuv1)

reactFunEq _ _ solve_this@(CtDerived {}) _
  = pprPanic "reactFunEq" (ppr solve_this)

{-
Note [Cache-caused loops]
~~~~~~~~~~~~~~~~~~~~~~~~~
It is very dangerous to cache a rewritten wanted family equation as 'solved' in our
solved cache (which is the default behaviour or xCtEvidence), because the interaction
may not be contributing towards a solution. Here is an example:

Initial inert set:
  [W] g1 : F a ~ beta1
Work item:
  [W] g2 : F a ~ beta2
The work item will react with the inert yielding the _same_ inert set plus:
    i)   Will set g2 := g1 `cast` g3
    ii)  Will add to our solved cache that [S] g2 : F a ~ beta2
    iii) Will emit [W] g3 : beta1 ~ beta2
Now, the g3 work item will be spontaneously solved to [G] g3 : beta1 ~ beta2
and then it will react the item in the inert ([W] g1 : F a ~ beta1). So it
will set
      g1 := g ; sym g3
and what is g? Well it would ideally be a new goal of type (F a ~ beta2) but
remember that we have this in our solved cache, and it is ... g2! In short we
created the evidence loop:

        g2 := g1 ; g3
        g3 := refl
        g1 := g2 ; sym g3

To avoid this situation we do not cache as solved any workitems (or inert)
which did not really made a 'step' towards proving some goal. Solved's are
just an optimization so we don't lose anything in terms of completeness of
solving.


Note [Efficient Orientation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we are interacting two FunEqCans with the same LHS:
          (inert)  ci :: (F ty ~ xi_i)
          (work)   cw :: (F ty ~ xi_w)
We prefer to keep the inert (else we pass the work item on down
the pipeline, which is a bit silly).  If we keep the inert, we
will (a) discharge 'cw'
     (b) produce a new equality work-item (xi_w ~ xi_i)
Notice the orientation (xi_w ~ xi_i) NOT (xi_i ~ xi_w):
    new_work :: xi_w ~ xi_i
    cw := ci ; sym new_work
Why?  Consider the simplest case when xi1 is a type variable.  If
we generate xi1~xi2, porcessing that constraint will kick out 'ci'.
If we generate xi2~xi1, there is less chance of that happening.
Of course it can and should still happen if xi1=a, xi1=Int, say.
But we want to avoid it happening needlessly.

Similarly, if we *can't* keep the inert item (because inert is Wanted,
and work is Given, say), we prefer to orient the new equality (xi_i ~
xi_w).

Note [Carefully solve the right CFunEqCan]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   ---- OLD COMMENT, NOW NOT NEEDED
   ---- because we now allow multiple
   ---- wanted FunEqs with the same head
Consider the constraints
  c1 :: F Int ~ a      -- Arising from an application line 5
  c2 :: F Int ~ Bool   -- Arising from an application line 10
Suppose that 'a' is a unification variable, arising only from
flattening.  So there is no error on line 5; it's just a flattening
variable.  But there is (or might be) an error on line 10.

Two ways to combine them, leaving either (Plan A)
  c1 :: F Int ~ a      -- Arising from an application line 5
  c3 :: a ~ Bool       -- Arising from an application line 10
or (Plan B)
  c2 :: F Int ~ Bool   -- Arising from an application line 10
  c4 :: a ~ Bool       -- Arising from an application line 5

Plan A will unify c3, leaving c1 :: F Int ~ Bool as an error
on the *totally innocent* line 5.  An example is test SimpleFail16
where the expected/actual message comes out backwards if we use
the wrong plan.

The second is the right thing to do.  Hence the isMetaTyVarTy
test when solving pairwise CFunEqCan.


*********************************************************************************
*                                                                               *
                   interactTyVarEq
*                                                                               *
*********************************************************************************
-}

interactTyVarEq :: InertCans -> Ct -> TcS (StopOrContinue Ct)
-- CTyEqCans are always consumed, so always returns Stop
interactTyVarEq inerts workItem@(CTyEqCan { cc_tyvar = tv
                                          , cc_rhs = rhs
                                          , cc_ev = ev
                                          , cc_eq_rel = eq_rel })
  | (ev_i : _) <- [ ev_i | CTyEqCan { cc_ev = ev_i, cc_rhs = rhs_i }
                             <- findTyEqs inerts tv
                         , ev_i `canRewriteOrSame` ev
                         , rhs_i `tcEqType` rhs ]
  =  -- Inert:     a ~ b
     -- Work item: a ~ b
    do { when (isWanted ev) $
         setEvBind (ctev_evar ev) (ctEvTerm ev_i)
       ; stopWith ev "Solved from inert" }

  | Just tv_rhs <- getTyVar_maybe rhs
  , (ev_i : _) <- [ ev_i | CTyEqCan { cc_ev = ev_i, cc_rhs = rhs_i }
                             <- findTyEqs inerts tv_rhs
                         , ev_i `canRewriteOrSame` ev
                         , rhs_i `tcEqType` mkTyVarTy tv ]
  =  -- Inert:     a ~ b
     -- Work item: b ~ a
    do { when (isWanted ev) $
         setEvBind (ctev_evar ev)
                   (EvCoercion (mkTcSymCo (ctEvCoercion ev_i)))
       ; stopWith ev "Solved from inert (r)" }

  | otherwise
  = do { tclvl <- getTcLevel
       ; if canSolveByUnification tclvl ev eq_rel tv rhs
         then do { solveByUnification ev tv rhs
                 ; n_kicked <- kickOutRewritable Given NomEq tv
                               -- Given because the tv := xi is given
                               -- NomEq because only nom. equalities are solved
                               -- by unification
                 ; return (Stop ev (ptext (sLit "Spontaneously solved") <+> ppr_kicked n_kicked)) }

         else do { traceTcS "Can't solve tyvar equality"
                       (vcat [ text "LHS:" <+> ppr tv <+> dcolon <+> ppr (tyVarKind tv)
                             , ppWhen (isMetaTyVar tv) $
                               nest 4 (text "TcLevel of" <+> ppr tv
                                       <+> text "is" <+> ppr (metaTyVarTcLevel tv))
                             , text "RHS:" <+> ppr rhs <+> dcolon <+> ppr (typeKind rhs)
                             , text "TcLevel =" <+> ppr tclvl ])
                 ; n_kicked <- kickOutRewritable (ctEvFlavour ev)
                                                 (ctEvEqRel ev)
                                                 tv
                 ; updInertCans (\ ics -> addInertCan ics workItem)
                 ; return (Stop ev (ptext (sLit "Kept as inert") <+> ppr_kicked n_kicked)) } }

interactTyVarEq _ wi = pprPanic "interactTyVarEq" (ppr wi)

-- @trySpontaneousSolve wi@ solves equalities where one side is a
-- touchable unification variable.
-- Returns True <=> spontaneous solve happened
canSolveByUnification :: TcLevel -> CtEvidence -> EqRel
                      -> TcTyVar -> Xi -> Bool
canSolveByUnification tclvl gw eq_rel tv xi
  | ReprEq <- eq_rel   -- we never solve representational equalities this way.
  = False

  | isGiven gw   -- See Note [Touchables and givens]
  = False

  | isTouchableMetaTyVar tclvl tv
  = case metaTyVarInfo tv of
      SigTv -> is_tyvar xi
      _     -> True

  | otherwise    -- Untouchable
  = False
  where
    is_tyvar xi
      = case tcGetTyVar_maybe xi of
          Nothing -> False
          Just tv -> case tcTyVarDetails tv of
                       MetaTv { mtv_info = info }
                                   -> case info of
                                        SigTv -> True
                                        _     -> False
                       SkolemTv {} -> True
                       FlatSkol {} -> False
                       RuntimeUnk  -> True

solveByUnification :: CtEvidence -> TcTyVar -> Xi -> TcS ()
-- Solve with the identity coercion
-- Precondition: kind(xi) is a sub-kind of kind(tv)
-- Precondition: CtEvidence is Wanted or Derived
-- Precondition: CtEvidence is nominal
-- See [New Wanted Superclass Work] to see why solveByUnification
--     must work for Derived as well as Wanted
-- Returns: workItem where
--        workItem = the new Given constraint
--
-- NB: No need for an occurs check here, because solveByUnification always
--     arises from a CTyEqCan, a *canonical* constraint.  Its invariants
--     say that in (a ~ xi), the type variable a does not appear in xi.
--     See TcRnTypes.Ct invariants.
--
-- Post: tv is unified (by side effect) with xi;
--       we often write tv := xi
solveByUnification wd tv xi
  = do { let tv_ty = mkTyVarTy tv
       ; traceTcS "Sneaky unification:" $
                       vcat [text "Unifies:" <+> ppr tv <+> ptext (sLit ":=") <+> ppr xi,
                             text "Coercion:" <+> pprEq tv_ty xi,
                             text "Left Kind is:" <+> ppr (typeKind tv_ty),
                             text "Right Kind is:" <+> ppr (typeKind xi) ]

       ; let xi' = defaultKind xi
               -- We only instantiate kind unification variables
               -- with simple kinds like *, not OpenKind or ArgKind
               -- cf TcUnify.uUnboundKVar

       ; setWantedTyBind tv xi'
       ; when (isWanted wd) $
         setEvBind (ctEvId wd) (EvCoercion (mkTcNomReflCo xi')) }


ppr_kicked :: Int -> SDoc
ppr_kicked 0 = empty
ppr_kicked n = parens (int n <+> ptext (sLit "kicked out"))

kickOutRewritable :: CtFlavour    -- Flavour of the equality that is
                                  -- being added to the inert set
                  -> EqRel        -- of the new equality
                  -> TcTyVar      -- The new equality is tv ~ ty
                  -> TcS Int
kickOutRewritable new_flavour new_eq_rel new_tv
  | not ((new_flavour, new_eq_rel) `eqCanRewriteFR` (new_flavour, new_eq_rel))
  = return 0  -- If new_flavour can't rewrite itself, it can't rewrite
              -- anything else, so no need to kick out anything
              -- This is a common case: wanteds can't rewrite wanteds

  | otherwise
  = do { ics <- getInertCans
       ; let (kicked_out, ics') = kick_out new_flavour new_eq_rel new_tv ics
       ; setInertCans ics'
       ; updWorkListTcS (appendWorkList kicked_out)

       ; unless (isEmptyWorkList kicked_out) $
         csTraceTcS $
         hang (ptext (sLit "Kick out, tv =") <+> ppr new_tv)
            2 (vcat [ text "n-kicked =" <+> int (workListSize kicked_out)
                    , text "n-kept fun-eqs =" <+> int (sizeFunEqMap (inert_funeqs ics'))
                    , ppr kicked_out ])
       ; return (workListSize kicked_out) }

kick_out :: CtFlavour -> EqRel -> TcTyVar -> InertCans -> (WorkList, InertCans)
kick_out new_flavour new_eq_rel new_tv (IC { inert_eqs      = tv_eqs
                                           , inert_dicts    = dictmap
                                           , inert_funeqs   = funeqmap
                                           , inert_irreds   = irreds
                                           , inert_insols   = insols })
  = (kicked_out, inert_cans_in)
  where
                -- NB: Notice that don't rewrite
                -- inert_solved_dicts, and inert_solved_funeqs
                -- optimistically. But when we lookup we have to
                -- take the substitution into account
    inert_cans_in = IC { inert_eqs      = tv_eqs_in
                       , inert_dicts    = dicts_in
                       , inert_funeqs   = feqs_in
                       , inert_irreds   = irs_in
                       , inert_insols   = insols_in }

    kicked_out = WL { wl_eqs    = tv_eqs_out
                    , wl_funeqs = feqs_out
                    , wl_rest   = bagToList (dicts_out `andCts` irs_out
                                             `andCts` insols_out)
                    , wl_implics = emptyBag }

    (tv_eqs_out, tv_eqs_in) = foldVarEnv kick_out_eqs ([], emptyVarEnv) tv_eqs
    (feqs_out,   feqs_in)   = partitionFunEqs  kick_out_ct funeqmap
    (dicts_out,  dicts_in)  = partitionDicts   kick_out_ct dictmap
    (irs_out,    irs_in)    = partitionBag     kick_out_irred irreds
    (insols_out, insols_in) = partitionBag     kick_out_ct    insols
      -- Kick out even insolubles; see Note [Kick out insolubles]

    can_rewrite :: CtEvidence -> Bool
    can_rewrite = ((new_flavour, new_eq_rel) `eqCanRewriteFR`) . ctEvFlavourRole

    kick_out_ct :: Ct -> Bool
    kick_out_ct ct = kick_out_ctev (ctEvidence ct)

    kick_out_ctev :: CtEvidence -> Bool
    kick_out_ctev ev =  can_rewrite ev
                     && new_tv `elemVarSet` tyVarsOfType (ctEvPred ev)
         -- See Note [Kicking out inert constraints]

    kick_out_irred :: Ct -> Bool
    kick_out_irred ct =  can_rewrite (cc_ev ct)
                      && new_tv `elemVarSet` closeOverKinds (tyVarsOfCt ct)
          -- See Note [Kicking out Irreds]

    kick_out_eqs :: EqualCtList -> ([Ct], TyVarEnv EqualCtList)
                 -> ([Ct], TyVarEnv EqualCtList)
    kick_out_eqs eqs (acc_out, acc_in)
      = (eqs_out ++ acc_out, case eqs_in of
                               []      -> acc_in
                               (eq1:_) -> extendVarEnv acc_in (cc_tyvar eq1) eqs_in)
      where
        (eqs_in, eqs_out) = partition keep_eq eqs

    -- implements criteria K1-K3 in Note [The inert equalities] in TcFlatten
    keep_eq (CTyEqCan { cc_tyvar = tv, cc_rhs = rhs_ty, cc_ev = ev
                      , cc_eq_rel = eq_rel })
      | tv == new_tv
      = not (can_rewrite ev)  -- (K1)

      | otherwise
      = check_k2 && check_k3
      where
        check_k2 = not (ev `eqCanRewrite` ev)
                || not (can_rewrite ev)
                || not (new_tv `elemVarSet` tyVarsOfType rhs_ty)

        check_k3
          | can_rewrite ev
          = case eq_rel of
              NomEq  -> not (rhs_ty `eqType` mkTyVarTy new_tv)
              ReprEq -> not (isTyVarExposed new_tv rhs_ty)

          | otherwise
          = True

    keep_eq ct = pprPanic "keep_eq" (ppr ct)

{-
Note [Kicking out inert constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given a new (a -> ty) inert, we want to kick out an existing inert
constraint if
  a) the new constraint can rewrite the inert one
  b) 'a' is free in the inert constraint (so that it *will*)
     rewrite it if we kick it out.

For (b) we use tyVarsOfCt, which returns the type variables /and
the kind variables/ that are directly visible in the type. Hence we
will have exposed all the rewriting we care about to make the most
precise kinds visible for matching classes etc. No need to kick out
constraints that mention type variables whose kinds contain this
variable!  (Except see Note [Kicking out Irreds].)

Note [Kicking out Irreds]
~~~~~~~~~~~~~~~~~~~~~~~~~
There is an awkward special case for Irreds.  When we have a
kind-mis-matched equality constraint (a:k1) ~ (ty:k2), we turn it into
an Irred (see Note [Equalities with incompatible kinds] in
TcCanonical). So in this case the free kind variables of k1 and k2
are not visible.  More precisely, the type looks like
   (~) k1 (a:k1) (ty:k2)
because (~) has kind forall k. k -> k -> Constraint.  So the constraint
itself is ill-kinded.  We can "see" k1 but not k2.  That's why we use
closeOverKinds to make sure we see k2.

This is not pretty. Maybe (~) should have kind
   (~) :: forall k1 k1. k1 -> k2 -> Constraint

Note [Kick out insolubles]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have an insoluble alpha ~ [alpha], which is insoluble
because an occurs check.  And then we unify alpha := [Int].
Then we really want to rewrite the insouluble to [Int] ~ [[Int]].
Now it can be decomposed.  Otherwise we end up with a "Can't match
[Int] ~ [[Int]]" which is true, but a bit confusing because the
outer type constructors match.


Note [Avoid double unifications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The spontaneous solver has to return a given which mentions the unified unification
variable *on the left* of the equality. Here is what happens if not:
  Original wanted:  (a ~ alpha),  (alpha ~ Int)
We spontaneously solve the first wanted, without changing the order!
      given : a ~ alpha      [having unified alpha := a]
Now the second wanted comes along, but he cannot rewrite the given, so we simply continue.
At the end we spontaneously solve that guy, *reunifying*  [alpha := Int]

We avoid this problem by orienting the resulting given so that the unification
variable is on the left.  [Note that alternatively we could attempt to
enforce this at canonicalization]

See also Note [No touchables as FunEq RHS] in TcSMonad; avoiding
double unifications is the main reason we disallow touchable
unification variables as RHS of type family equations: F xis ~ alpha.



Note [Superclasses and recursive dictionaries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Overlaps with Note [SUPERCLASS-LOOP 1]
                  Note [SUPERCLASS-LOOP 2]
                  Note [Recursive instances and superclases]
    ToDo: check overlap and delete redundant stuff

Right before adding a given into the inert set, we must
produce some more work, that will bring the superclasses
of the given into scope. The superclass constraints go into
our worklist.

When we simplify a wanted constraint, if we first see a matching
instance, we may produce new wanted work. To (1) avoid doing this work
twice in the future and (2) to handle recursive dictionaries we may ``cache''
this item as given into our inert set WITHOUT adding its superclass constraints,
otherwise we'd be in danger of creating a loop [In fact this was the exact reason
for doing the isGoodRecEv check in an older version of the type checker].

But now we have added partially solved constraints to the worklist which may
interact with other wanteds. Consider the example:

Example 1:

    class Eq b => Foo a b        --- 0-th selector
    instance Eq a => Foo [a] a   --- fooDFun

and wanted (Foo [t] t). We are first going to see that the instance matches
and create an inert set that includes the solved (Foo [t] t) but not its superclasses:
       d1 :_g Foo [t] t                 d1 := EvDFunApp fooDFun d3
Our work list is going to contain a new *wanted* goal
       d3 :_w Eq t

Ok, so how do we get recursive dictionaries, at all:

Example 2:

    data D r = ZeroD | SuccD (r (D r));

    instance (Eq (r (D r))) => Eq (D r) where
        ZeroD     == ZeroD     = True
        (SuccD a) == (SuccD b) = a == b
        _         == _         = False;

    equalDC :: D [] -> D [] -> Bool;
    equalDC = (==);

We need to prove (Eq (D [])). Here's how we go:

        d1 :_w Eq (D [])

by instance decl, holds if
        d2 :_w Eq [D []]
        where   d1 = dfEqD d2

*BUT* we have an inert set which gives us (no superclasses):
        d1 :_g Eq (D [])
By the instance declaration of Eq we can show the 'd2' goal if
        d3 :_w Eq (D [])
        where   d2 = dfEqList d3
                d1 = dfEqD d2
Now, however this wanted can interact with our inert d1 to set:
        d3 := d1
and solve the goal. Why was this interaction OK? Because, if we chase the
evidence of d1 ~~> dfEqD d2 ~~-> dfEqList d3, so by setting d3 := d1 we
are really setting
        d3 := dfEqD2 (dfEqList d3)
which is FINE because the use of d3 is protected by the instance function
applications.

So, our strategy is to try to put solved wanted dictionaries into the
inert set along with their superclasses (when this is meaningful,
i.e. when new wanted goals are generated) but solve a wanted dictionary
from a given only in the case where the evidence variable of the
wanted is mentioned in the evidence of the given (recursively through
the evidence binds) in a protected way: more instance function applications
than superclass selectors.

Here are some more examples from GHC's previous type checker


Example 3:
This code arises in the context of "Scrap Your Boilerplate with Class"

    class Sat a
    class Data ctx a
    instance  Sat (ctx Char)             => Data ctx Char       -- dfunData1
    instance (Sat (ctx [a]), Data ctx a) => Data ctx [a]        -- dfunData2

    class Data Maybe a => Foo a

    instance Foo t => Sat (Maybe t)                             -- dfunSat

    instance Data Maybe a => Foo a                              -- dfunFoo1
    instance Foo a        => Foo [a]                            -- dfunFoo2
    instance                 Foo [Char]                         -- dfunFoo3

Consider generating the superclasses of the instance declaration
         instance Foo a => Foo [a]

So our problem is this
    [G] d0 : Foo t
    [W] d1 : Data Maybe [t]   -- Desired superclass

We may add the given in the inert set, along with its superclasses
[assuming we don't fail because there is a matching instance, see
 topReactionsStage, given case ]
  Inert:
    [G] d0 : Foo t
    [G] d01 : Data Maybe t   -- Superclass of d0
  WorkList
    [W] d1 : Data Maybe [t]

Solve d1 using instance dfunData2; d1 := dfunData2 d2 d3
  Inert:
    [G] d0 : Foo t
    [G] d01 : Data Maybe t   -- Superclass of d0
  Solved:
        d1 : Data Maybe [t]
  WorkList
    [W] d2 : Sat (Maybe [t])
    [W] d3 : Data Maybe t

Now, we may simplify d2 using dfunSat; d2 := dfunSat d4
  Inert:
    [G] d0 : Foo t
    [G] d01 : Data Maybe t   -- Superclass of d0
  Solved:
        d1 : Data Maybe [t]
        d2 : Sat (Maybe [t])
  WorkList:
    [W] d3 : Data Maybe t
    [W] d4 : Foo [t]

Now, we can just solve d3 from d01; d3 := d01
  Inert
    [G] d0 : Foo t
    [G] d01 : Data Maybe t   -- Superclass of d0
  Solved:
        d1 : Data Maybe [t]
        d2 : Sat (Maybe [t])
  WorkList
    [W] d4 : Foo [t]

Now, solve d4 using dfunFoo2;  d4 := dfunFoo2 d5
  Inert
    [G] d0 : Foo t
    [G] d01 : Data Maybe t   -- Superclass of d0
  Solved:
        d1 : Data Maybe [t]
        d2 : Sat (Maybe [t])
        d4 : Foo [t]
  WorkList:
    [W] d5 : Foo t

Now, d5 can be solved! d5 := d0

Result
   d1 := dfunData2 d2 d3
   d2 := dfunSat d4
   d3 := d01
   d4 := dfunFoo2 d5
   d5 := d0

      d0 :_g Foo t
      d1 :_s Data Maybe [t]           d1 := dfunData2 d2 d3
      d2 :_g Sat (Maybe [t])          d2 := dfunSat d4
      d4 :_g Foo [t]                  d4 := dfunFoo2 d5
      d5 :_g Foo t                    d5 := dfunFoo1 d7
  WorkList:
      d7 :_w Data Maybe t
      d6 :_g Data Maybe [t]
      d8 :_g Data Maybe t            d8 := EvDictSuperClass d5 0
      d01 :_g Data Maybe t

Now, two problems:
   [1] Suppose we pick d8 and we react him with d01. Which of the two givens should
       we keep? Well, we *MUST NOT* drop d01 because d8 contains recursive evidence
       that must not be used (look at case interactInert where both inert and workitem
       are givens). So we have several options:
       - Drop the workitem always (this will drop d8)
              This feels very unsafe -- what if the work item was the "good" one
              that should be used later to solve another wanted?
       - Don't drop anyone: the inert set may contain multiple givens!
              [This is currently implemented]

The "don't drop anyone" seems the most safe thing to do, so now we come to problem 2:
  [2] We have added both d6 and d01 in the inert set, and we are interacting our wanted
      d7. Now the [isRecDictEv] function in the ineration solver
      [case inert-given workitem-wanted] will prevent us from interacting d7 := d8
      precisely because chasing the evidence of d8 leads us to an unguarded use of d7.

      So, no interaction happens there. Then we meet d01 and there is no recursion
      problem there [isRectDictEv] gives us the OK to interact and we do solve d7 := d01!

Note [SUPERCLASS-LOOP 1]
~~~~~~~~~~~~~~~~~~~~~~~~
We have to be very, very careful when generating superclasses, lest we
accidentally build a loop. Here's an example:

  class S a

  class S a => C a where { opc :: a -> a }
  class S b => D b where { opd :: b -> b }

  instance C Int where
     opc = opd

  instance D Int where
     opd = opc

From (instance C Int) we get the constraint set {ds1:S Int, dd:D Int}
Simplifying, we may well get:
        $dfCInt = :C ds1 (opd dd)
        dd  = $dfDInt
        ds1 = $p1 dd
Notice that we spot that we can extract ds1 from dd.

Alas!  Alack! We can do the same for (instance D Int):

        $dfDInt = :D ds2 (opc dc)
        dc  = $dfCInt
        ds2 = $p1 dc

And now we've defined the superclass in terms of itself.
Two more nasty cases are in
        tcrun021
        tcrun033

Solution:
  - Satisfy the superclass context *all by itself*
    (tcSimplifySuperClasses)
  - And do so completely; i.e. no left-over constraints
    to mix with the constraints arising from method declarations


Note [SUPERCLASS-LOOP 2]
~~~~~~~~~~~~~~~~~~~~~~~~
We need to be careful when adding "the constaint we are trying to prove".
Suppose we are *given* d1:Ord a, and want to deduce (d2:C [a]) where

        class Ord a => C a where
        instance Ord [a] => C [a] where ...

Then we'll use the instance decl to deduce C [a] from Ord [a], and then add the
superclasses of C [a] to avails.  But we must not overwrite the binding
for Ord [a] (which is obtained from Ord a) with a superclass selection or we'll just
build a loop!

Here's another variant, immortalised in tcrun020
        class Monad m => C1 m
        class C1 m => C2 m x
        instance C2 Maybe Bool
For the instance decl we need to build (C1 Maybe), and it's no good if
we run around and add (C2 Maybe Bool) and its superclasses to the avails
before we search for C1 Maybe.

Here's another example
        class Eq b => Foo a b
        instance Eq a => Foo [a] a
If we are reducing
        (Foo [t] t)

we'll first deduce that it holds (via the instance decl).  We must not
then overwrite the Eq t constraint with a superclass selection!

At first I had a gross hack, whereby I simply did not add superclass constraints
in addWanted, though I did for addGiven and addIrred.  This was sub-optimal,
because it lost legitimate superclass sharing, and it still didn't do the job:
I found a very obscure program (now tcrun021) in which improvement meant the
simplifier got two bites a the cherry... so something seemed to be an Stop
first time, but reducible next time.

Now we implement the Right Solution, which is to check for loops directly
when adding superclasses.  It's a bit like the occurs check in unification.

Note [Recursive instances and superclases]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this code, which arises in the context of "Scrap Your
Boilerplate with Class".

    class Sat a
    class Data ctx a
    instance  Sat (ctx Char)             => Data ctx Char
    instance (Sat (ctx [a]), Data ctx a) => Data ctx [a]

    class Data Maybe a => Foo a

    instance Foo t => Sat (Maybe t)

    instance Data Maybe a => Foo a
    instance Foo a        => Foo [a]
    instance                 Foo [Char]

In the instance for Foo [a], when generating evidence for the superclasses
(ie in tcSimplifySuperClasses) we need a superclass (Data Maybe [a]).
Using the instance for Data, we therefore need
        (Sat (Maybe [a], Data Maybe a)
But we are given (Foo a), and hence its superclass (Data Maybe a).
So that leaves (Sat (Maybe [a])).  Using the instance for Sat means
we need (Foo [a]).  And that is the very dictionary we are bulding
an instance for!  So we must put that in the "givens".  So in this
case we have
        Given:  Foo a, Foo [a]
        Wanted: Data Maybe [a]

BUT we must *not not not* put the *superclasses* of (Foo [a]) in
the givens, which is what 'addGiven' would normally do. Why? Because
(Data Maybe [a]) is the superclass, so we'd "satisfy" the wanted
by selecting a superclass from Foo [a], which simply makes a loop.

On the other hand we *must* put the superclasses of (Foo a) in
the givens, as you can see from the derivation described above.

Conclusion: in the very special case of tcSimplifySuperClasses
we have one 'given' (namely the "this" dictionary) whose superclasses
must not be added to 'givens' by addGiven.

There is a complication though.  Suppose there are equalities
      instance (Eq a, a~b) => Num (a,b)
Then we normalise the 'givens' wrt the equalities, so the original
given "this" dictionary is cast to one of a different type.  So it's a
bit trickier than before to identify the "special" dictionary whose
superclasses must not be added. See test
   indexed-types/should_run/EqInInstance

We need a persistent property of the dictionary to record this
special-ness.  Current I'm using the InstLocOrigin (a bit of a hack,
but cool), which is maintained by dictionary normalisation.
Specifically, the InstLocOrigin is
             NoScOrigin
then the no-superclass thing kicks in.  WATCH OUT if you fiddle
with InstLocOrigin!


************************************************************************
*                                                                      *
*          Functional dependencies, instantiation of equations
*                                                                      *
************************************************************************

When we spot an equality arising from a functional dependency,
we now use that equality (a "wanted") to rewrite the work-item
constraint right away.  This avoids two dangers

 Danger 1: If we send the original constraint on down the pipeline
           it may react with an instance declaration, and in delicate
           situations (when a Given overlaps with an instance) that
           may produce new insoluble goals: see Trac #4952

 Danger 2: If we don't rewrite the constraint, it may re-react
           with the same thing later, and produce the same equality
           again --> termination worries.

To achieve this required some refactoring of FunDeps.lhs (nicer
now!).
-}

rewriteWithFunDeps :: [Equation CtLoc] -> TcS ()
-- NB: The returned constraints are all Derived
-- Post: returns no trivial equalities (identities) and all EvVars returned are fresh
rewriteWithFunDeps eqn_pred_locs
 = mapM_ instFunDepEqn eqn_pred_locs

instFunDepEqn :: Equation CtLoc -> TcS ()
-- Post: Returns the position index as well as the corresponding FunDep equality
instFunDepEqn (FDEqn { fd_qtvs = tvs, fd_eqs = eqs, fd_loc = loc })
  = do { (subst, _) <- instFlexiTcS tvs  -- Takes account of kind substitution
       ; mapM_ (do_one subst) eqs }
  where
    do_one subst (FDEq { fd_ty_left = ty1, fd_ty_right = ty2 })
       = unifyDerived loc Nominal $
         Pair (Type.substTy subst ty1) (Type.substTy subst ty2)

{-
*********************************************************************************
*                                                                               *
                       The top-reaction Stage
*                                                                               *
*********************************************************************************
-}

topReactionsStage :: WorkItem -> TcS (StopOrContinue Ct)
topReactionsStage wi
 = do { inerts <- getTcSInerts
      ; tir <- doTopReact inerts wi
      ; case tir of
          ContinueWith wi -> return (ContinueWith wi)
          Stop ev s       -> return (Stop ev (ptext (sLit "Top react:") <+> s)) }

doTopReact :: InertSet -> WorkItem -> TcS (StopOrContinue Ct)
-- The work item does not react with the inert set, so try interaction with top-level
-- instances. Note:
--
--   (a) The place to add superclasses in not here in doTopReact stage.
--       Instead superclasses are added in the worklist as part of the
--       canonicalization process. See Note [Adding superclasses].
--
--   (b) See Note [Given constraint that matches an instance declaration]
--       for some design decisions for given dictionaries.

doTopReact inerts work_item
  = do { traceTcS "doTopReact" (ppr work_item)
       ; case work_item of
           CDictCan {}  -> doTopReactDict inerts work_item
           CFunEqCan {} -> doTopReactFunEq work_item
           _  -> -- Any other work item does not react with any top-level equations
                 return (ContinueWith work_item)  }

--------------------
doTopReactDict :: InertSet -> Ct -> TcS (StopOrContinue Ct)
-- Try to use type-class instance declarations to simplify the constraint
doTopReactDict inerts work_item@(CDictCan { cc_ev = fl, cc_class = cls
                                          , cc_tyargs = xis })
  | not (isWanted fl)   -- Never use instances for Given or Derived constraints
  = try_fundeps_and_return

  | Just ev <- lookupSolvedDict inerts loc cls xis   -- Cached
  = do { setEvBind dict_id (ctEvTerm ev);
       ; stopWith fl "Dict/Top (cached)" }

  | otherwise  -- Not cached
   = do { lkup_inst_res <- matchClassInst inerts cls xis loc
         ; case lkup_inst_res of
               GenInst wtvs ev_term -> do { addSolvedDict fl cls xis
                                          ; solve_from_instance wtvs ev_term }
               NoInstance -> try_fundeps_and_return }
   where
     dict_id = ASSERT( isWanted fl ) ctEvId fl
     pred = mkClassPred cls xis
     loc = ctEvLoc fl

     solve_from_instance :: [CtEvidence] -> EvTerm -> TcS (StopOrContinue Ct)
      -- Precondition: evidence term matches the predicate workItem
     solve_from_instance evs ev_term
        | null evs
        = do { traceTcS "doTopReact/found nullary instance for" $
               ppr dict_id
             ; setEvBind dict_id ev_term
             ; stopWith fl "Dict/Top (solved, no new work)" }
        | otherwise
        = do { traceTcS "doTopReact/found non-nullary instance for" $
               ppr dict_id
             ; setEvBind dict_id ev_term
             ; let mk_new_wanted ev
                       = mkNonCanonical (ev {ctev_loc = bumpCtLocDepth CountConstraints loc })
             ; updWorkListTcS (extendWorkListCts (map mk_new_wanted evs))
             ; stopWith fl "Dict/Top (solved, more work)" }

     -- We didn't solve it; so try functional dependencies with
     -- the instance environment, and return
     -- NB: even if there *are* some functional dependencies against the
     -- instance environment, there might be a unique match, and if
     -- so we make sure we get on and solve it first. See Note [Weird fundeps]
     try_fundeps_and_return
       = do { instEnvs <- getInstEnvs
            ; let fd_eqns :: [Equation CtLoc]
                  fd_eqns = [ fd { fd_loc = loc { ctl_origin = FunDepOrigin2 pred (ctl_origin loc)
                                                                             inst_pred inst_loc } }
                            | fd@(FDEqn { fd_loc = inst_loc, fd_pred1 = inst_pred })
                            <- improveFromInstEnv instEnvs pred ]
            ; rewriteWithFunDeps fd_eqns
            ; continueWith work_item }

doTopReactDict _ w = pprPanic "doTopReactDict" (ppr w)

--------------------
doTopReactFunEq :: Ct -> TcS (StopOrContinue Ct)
doTopReactFunEq work_item@(CFunEqCan { cc_ev = old_ev, cc_fun = fam_tc
                                     , cc_tyargs = args , cc_fsk = fsk })
  = ASSERT(isTypeFamilyTyCon fam_tc) -- No associated data families
                                     -- have reached this far
    ASSERT( not (isDerived old_ev) )   -- CFunEqCan is never Derived
    -- Look up in top-level instances, or built-in axiom
    do { match_res <- matchFam fam_tc args   -- See Note [MATCHING-SYNONYMS]
       ; case match_res of {
           Nothing -> do { try_improvement; continueWith work_item } ;
           Just (ax_co, rhs_ty)

    -- Found a top-level instance

    | Just (tc, tc_args) <- tcSplitTyConApp_maybe rhs_ty
    , isTypeFamilyTyCon tc
    , tc_args `lengthIs` tyConArity tc    -- Short-cut
    -> shortCutReduction old_ev fsk ax_co tc tc_args
         -- Try shortcut; see Note [Short cut for top-level reaction]

    | isGiven old_ev  -- Not shortcut
    -> do { let final_co = mkTcSymCo (ctEvCoercion old_ev) `mkTcTransCo` ax_co
                -- final_co :: fsk ~ rhs_ty
          ; new_ev <- newGivenEvVar deeper_loc (mkTcEqPred (mkTyVarTy fsk) rhs_ty,
                                                EvCoercion final_co)
          ; emitWorkNC [new_ev]   -- Non-cannonical; that will mean we flatten rhs_ty
          ; stopWith old_ev "Fun/Top (given)" }

    | not (fsk `elemVarSet` tyVarsOfType rhs_ty)
    -> do { dischargeFmv (ctEvId old_ev) fsk ax_co rhs_ty
          ; traceTcS "doTopReactFunEq" $
            vcat [ text "old_ev:" <+> ppr old_ev
                 , nest 2 (text ":=") <+> ppr ax_co ]
          ; stopWith old_ev "Fun/Top (wanted)" }

    | otherwise -- We must not assign ufsk := ...ufsk...!
    -> do { alpha_ty <- newFlexiTcSTy (tyVarKind fsk)
          ; new_ev <- newWantedEvVarNC loc (mkTcEqPred alpha_ty rhs_ty)
          ; emitWorkNC [new_ev]
              -- By emitting this as non-canonical, we deal with all
              -- flattening, occurs-check, and ufsk := ufsk issues
          ; let final_co = ax_co `mkTcTransCo` mkTcSymCo (ctEvCoercion new_ev)
              --    ax_co :: fam_tc args ~ rhs_ty
              --       ev :: alpha ~ rhs_ty
              --     ufsk := alpha
              -- final_co :: fam_tc args ~ alpha
          ; dischargeFmv (ctEvId old_ev) fsk final_co alpha_ty
          ; traceTcS "doTopReactFunEq (occurs)" $
            vcat [ text "old_ev:" <+> ppr old_ev
                 , nest 2 (text ":=") <+> ppr final_co
                 , text "new_ev:" <+> ppr new_ev ]
          ; stopWith old_ev "Fun/Top (wanted)" } } }
  where
    loc = ctEvLoc old_ev
    deeper_loc = bumpCtLocDepth CountTyFunApps loc

    try_improvement
      | Just ops <- isBuiltInSynFamTyCon_maybe fam_tc
      = do { inert_eqs <- getInertEqs
           ; let eqns = sfInteractTop ops args (lookupFlattenTyVar inert_eqs fsk)
           ; mapM_ (unifyDerived loc Nominal) eqns }
      | otherwise
      = return ()

doTopReactFunEq w = pprPanic "doTopReactFunEq" (ppr w)

shortCutReduction :: CtEvidence -> TcTyVar -> TcCoercion
                  -> TyCon -> [TcType] -> TcS (StopOrContinue Ct)
shortCutReduction old_ev fsk ax_co fam_tc tc_args
  | isGiven old_ev
  = ASSERT( ctEvEqRel old_ev == NomEq )
    runFlatten $
    do { let fmode = mkFlattenEnv FM_FlattenAll old_ev
       ; (xis, cos) <- flatten_many fmode (repeat Nominal) tc_args
               -- ax_co :: F args ~ G tc_args
               -- cos   :: xis ~ tc_args
               -- old_ev :: F args ~ fsk
               -- G cos ; sym ax_co ; old_ev :: G xis ~ fsk

       ; new_ev <- newGivenEvVar deeper_loc
                         ( mkTcEqPred (mkTyConApp fam_tc xis) (mkTyVarTy fsk)
                         , EvCoercion (mkTcTyConAppCo Nominal fam_tc cos
                                        `mkTcTransCo` mkTcSymCo ax_co
                                        `mkTcTransCo` ctEvCoercion old_ev) )

       ; let new_ct = CFunEqCan { cc_ev = new_ev, cc_fun = fam_tc, cc_tyargs = xis, cc_fsk = fsk }
       ; emitFlatWork new_ct
       ; stopWith old_ev "Fun/Top (given, shortcut)" }

  | otherwise
  = ASSERT( not (isDerived old_ev) )   -- Caller ensures this
    ASSERT( ctEvEqRel old_ev == NomEq )
    runFlatten $
    do { let fmode = mkFlattenEnv FM_FlattenAll old_ev
       ; (xis, cos) <- flatten_many fmode (repeat Nominal) tc_args
               -- ax_co :: F args ~ G tc_args
               -- cos   :: xis ~ tc_args
               -- G cos ; sym ax_co ; old_ev :: G xis ~ fsk
               -- new_ev :: G xis ~ fsk
               -- old_ev :: F args ~ fsk := ax_co ; sym (G cos) ; new_ev

       ; new_ev <- newWantedEvVarNC loc (mkTcEqPred (mkTyConApp fam_tc xis) (mkTyVarTy fsk))
       ; setEvBind (ctEvId old_ev)
                   (EvCoercion (ax_co `mkTcTransCo` mkTcSymCo (mkTcTyConAppCo Nominal fam_tc cos)
                                      `mkTcTransCo` ctEvCoercion new_ev))

       ; let new_ct = CFunEqCan { cc_ev = new_ev, cc_fun = fam_tc, cc_tyargs = xis, cc_fsk = fsk }
       ; emitFlatWork new_ct
       ; stopWith old_ev "Fun/Top (wanted, shortcut)" }
  where
    loc = ctEvLoc old_ev
    deeper_loc = bumpCtLocDepth CountTyFunApps loc

dischargeFmv :: EvVar -> TcTyVar -> TcCoercion -> TcType -> TcS ()
-- (dischargeFmv x fmv co ty)
--     [W] x :: F tys ~ fuv
--        co :: F tys ~ ty
-- Precondition: fuv is not filled, and fuv `notElem` ty
--
-- Then set fuv := ty,
--      set x := co
--      kick out any inert things that are now rewritable
dischargeFmv evar fmv co xi
  = ASSERT2( not (fmv `elemVarSet` tyVarsOfType xi), ppr evar $$ ppr fmv $$ ppr xi )
    do { setWantedTyBind fmv xi
       ; setEvBind evar (EvCoercion co)
       ; n_kicked <- kickOutRewritable Given NomEq fmv
       ; traceTcS "dischargeFuv" (ppr fmv <+> equals <+> ppr xi $$ ppr_kicked n_kicked) }

{-
Note [Cached solved FunEqs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
When trying to solve, say (FunExpensive big-type ~ ty), it's important
to see if we have reduced (FunExpensive big-type) before, lest we
simply repeat it.  Hence the lookup in inert_solved_funeqs.  Moreover
we must use `canRewriteOrSame` because both uses might (say) be Wanteds,
and we *still* want to save the re-computation.

Note [MATCHING-SYNONYMS]
~~~~~~~~~~~~~~~~~~~~~~~~
When trying to match a dictionary (D tau) to a top-level instance, or a
type family equation (F taus_1 ~ tau_2) to a top-level family instance,
we do *not* need to expand type synonyms because the matcher will do that for us.


Note [RHS-FAMILY-SYNONYMS]
~~~~~~~~~~~~~~~~~~~~~~~~~~
The RHS of a family instance is represented as yet another constructor which is
like a type synonym for the real RHS the programmer declared. Eg:
    type instance F (a,a) = [a]
Becomes:
    :R32 a = [a]      -- internal type synonym introduced
    F (a,a) ~ :R32 a  -- instance

When we react a family instance with a type family equation in the work list
we keep the synonym-using RHS without expansion.

Note [FunDep and implicit parameter reactions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Currently, our story of interacting two dictionaries (or a dictionary
and top-level instances) for functional dependencies, and implicit
paramters, is that we simply produce new Derived equalities.  So for example

        class D a b | a -> b where ...
    Inert:
        d1 :g D Int Bool
    WorkItem:
        d2 :w D Int alpha

    We generate the extra work item
        cv :d alpha ~ Bool
    where 'cv' is currently unused.  However, this new item can perhaps be
    spontaneously solved to become given and react with d2,
    discharging it in favour of a new constraint d2' thus:
        d2' :w D Int Bool
        d2 := d2' |> D Int cv
    Now d2' can be discharged from d1

We could be more aggressive and try to *immediately* solve the dictionary
using those extra equalities, but that requires those equalities to carry
evidence and derived do not carry evidence.

If that were the case with the same inert set and work item we might dischard
d2 directly:

        cv :w alpha ~ Bool
        d2 := d1 |> D Int cv

But in general it's a bit painful to figure out the necessary coercion,
so we just take the first approach. Here is a better example. Consider:
    class C a b c | a -> b
And:
     [Given]  d1 : C T Int Char
     [Wanted] d2 : C T beta Int
In this case, it's *not even possible* to solve the wanted immediately.
So we should simply output the functional dependency and add this guy
[but NOT its superclasses] back in the worklist. Even worse:
     [Given] d1 : C T Int beta
     [Wanted] d2: C T beta Int
Then it is solvable, but its very hard to detect this on the spot.

It's exactly the same with implicit parameters, except that the
"aggressive" approach would be much easier to implement.

Note [When improvement happens]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We fire an improvement rule when

  * Two constraints match (modulo the fundep)
      e.g. C t1 t2, C t1 t3    where C a b | a->b
    The two match because the first arg is identical

Note that we *do* fire the improvement if one is Given and one is Derived (e.g. a
superclass of a Wanted goal) or if both are Given.

Example (tcfail138)
    class L a b | a -> b
    class (G a, L a b) => C a b

    instance C a b' => G (Maybe a)
    instance C a b  => C (Maybe a) a
    instance L (Maybe a) a

When solving the superclasses of the (C (Maybe a) a) instance, we get
  Given:  C a b  ... and hance by superclasses, (G a, L a b)
  Wanted: G (Maybe a)
Use the instance decl to get
  Wanted: C a b'
The (C a b') is inert, so we generate its Derived superclasses (L a b'),
and now we need improvement between that derived superclass an the Given (L a b)

Test typecheck/should_fail/FDsFromGivens also shows why it's a good idea to
emit Derived FDs for givens as well.

Note [Weird fundeps]
~~~~~~~~~~~~~~~~~~~~
Consider   class Het a b | a -> b where
              het :: m (f c) -> a -> m b

           class GHet (a :: * -> *) (b :: * -> *) | a -> b
           instance            GHet (K a) (K [a])
           instance Het a b => GHet (K a) (K b)

The two instances don't actually conflict on their fundeps,
although it's pretty strange.  So they are both accepted. Now
try   [W] GHet (K Int) (K Bool)
This triggers fudeps from both instance decls; but it also
matches a *unique* instance decl, and we should go ahead and
pick that one right now.  Otherwise, if we don't, it ends up
unsolved in the inert set and is reported as an error.

Trac #7875 is a case in point.

Note [Overriding implicit parameters]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   f :: (?x::a) -> Bool -> a

   g v = let ?x::Int = 3
         in (f v, let ?x::Bool = True in f v)

This should probably be well typed, with
   g :: Bool -> (Int, Bool)

So the inner binding for ?x::Bool *overrides* the outer one.
Hence a work-item Given overrides an inert-item Given.

Note [Given constraint that matches an instance declaration]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
What should we do when we discover that one (or more) top-level
instances match a given (or solved) class constraint? We have
two possibilities:

  1. Reject the program. The reason is that there may not be a unique
     best strategy for the solver. Example, from the OutsideIn(X) paper:
       instance P x => Q [x]
       instance (x ~ y) => R [x] y

       wob :: forall a b. (Q [b], R b a) => a -> Int

       g :: forall a. Q [a] => [a] -> Int
       g x = wob x

       will generate the impliation constraint:
            Q [a] => (Q [beta], R beta [a])
       If we react (Q [beta]) with its top-level axiom, we end up with a
       (P beta), which we have no way of discharging. On the other hand,
       if we react R beta [a] with the top-level we get  (beta ~ a), which
       is solvable and can help us rewrite (Q [beta]) to (Q [a]) which is
       now solvable by the given Q [a].

     However, this option is restrictive, for instance [Example 3] from
     Note [Recursive instances and superclases] will fail to work.

  2. Ignore the problem, hoping that the situations where there exist indeed
     such multiple strategies are rare: Indeed the cause of the previous
     problem is that (R [x] y) yields the new work (x ~ y) which can be
     *spontaneously* solved, not using the givens.

We are choosing option 2 below but we might consider having a flag as well.


Note [New Wanted Superclass Work]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Even in the case of wanted constraints, we may add some superclasses
as new given work. The reason is:

        To allow FD-like improvement for type families. Assume that
        we have a class
             class C a b | a -> b
        and we have to solve the implication constraint:
             C a b => C a beta
        Then, FD improvement can help us to produce a new wanted (beta ~ b)

        We want to have the same effect with the type family encoding of
        functional dependencies. Namely, consider:
             class (F a ~ b) => C a b
        Now suppose that we have:
               given: C a b
               wanted: C a beta
        By interacting the given we will get given (F a ~ b) which is not
        enough by itself to make us discharge (C a beta). However, we
        may create a new derived equality from the super-class of the
        wanted constraint (C a beta), namely derived (F a ~ beta).
        Now we may interact this with given (F a ~ b) to get:
                  derived :  beta ~ b
        But 'beta' is a touchable unification variable, and hence OK to
        unify it with 'b', replacing the derived evidence with the identity.

        This requires trySpontaneousSolve to solve *derived*
        equalities that have a touchable in their RHS, *in addition*
        to solving wanted equalities.

We also need to somehow use the superclasses to quantify over a minimal,
constraint see note [Minimize by Superclasses] in TcSimplify.


Finally, here is another example where this is useful.

Example 1:
----------
   class (F a ~ b) => C a b
And we are given the wanteds:
      w1 : C a b
      w2 : C a c
      w3 : b ~ c
We surely do *not* want to quantify over (b ~ c), since if someone provides
dictionaries for (C a b) and (C a c), these dictionaries can provide a proof
of (b ~ c), hence no extra evidence is necessary. Here is what will happen:

     Step 1: We will get new *given* superclass work,
             provisionally to our solving of w1 and w2

               g1: F a ~ b, g2 : F a ~ c,
               w1 : C a b, w2 : C a c, w3 : b ~ c

             The evidence for g1 and g2 is a superclass evidence term:

               g1 := sc w1, g2 := sc w2

     Step 2: The givens will solve the wanted w3, so that
               w3 := sym (sc w1) ; sc w2

     Step 3: Now, one may naively assume that then w2 can be solve from w1
             after rewriting with the (now solved equality) (b ~ c).

             But this rewriting is ruled out by the isGoodRectDict!

Conclusion, we will (correctly) end up with the unsolved goals
    (C a b, C a c)

NB: The desugarer needs be more clever to deal with equalities
    that participate in recursive dictionary bindings.
-}

data LookupInstResult
  = NoInstance
  | GenInst [CtEvidence] EvTerm

instance Outputable LookupInstResult where
  ppr NoInstance = text "NoInstance"
  ppr (GenInst ev t) = text "GenInst" <+> ppr ev <+> ppr t


matchClassInst :: InertSet -> Class -> [Type] -> CtLoc -> TcS LookupInstResult

matchClassInst _ clas [ ty ] _
  | className clas == knownNatClassName
  , Just n <- isNumLitTy ty = makeDict (EvNum n)

  | className clas == knownSymbolClassName
  , Just s <- isStrLitTy ty = makeDict (EvStr s)

  where
  {- This adds a coercion that will convert the literal into a dictionary
     of the appropriate type.  See Note [KnownNat & KnownSymbol and EvLit]
     in TcEvidence.  The coercion happens in 2 steps:

     Integer -> SNat n     -- representation of literal to singleton
     SNat n  -> KnownNat n -- singleton to dictionary

     The process is mirrored for Symbols:
     String    -> SSymbol n
     SSymbol n -> KnownSymbol n
  -}
  makeDict evLit
    | Just (_, co_dict) <- tcInstNewTyCon_maybe (classTyCon clas) [ty]
          -- co_dict :: KnownNat n ~ SNat n
    , [ meth ]   <- classMethods clas
    , Just tcRep <- tyConAppTyCon_maybe -- SNat
                      $ funResultTy         -- SNat n
                      $ dropForAlls         -- KnownNat n => SNat n
                      $ idType meth         -- forall n. KnownNat n => SNat n
    , Just (_, co_rep) <- tcInstNewTyCon_maybe tcRep [ty]
          -- SNat n ~ Integer
    = return (GenInst [] $ mkEvCast (EvLit evLit) (mkTcSymCo (mkTcTransCo co_dict co_rep)))

    | otherwise
    = panicTcS (text "Unexpected evidence for" <+> ppr (className clas)
                     $$ vcat (map (ppr . idType) (classMethods clas)))

matchClassInst _ clas [k,t] loc
  | className clas == typeableClassName = matchTypeableClass clas k t loc

matchClassInst inerts clas tys loc
   = do { dflags <- getDynFlags
        ; traceTcS "matchClassInst" $ vcat [ text "pred =" <+> ppr pred
                                           , text "inerts=" <+> ppr inerts ]
        ; instEnvs <- getInstEnvs
        ; case lookupInstEnv instEnvs clas tys of
            ([], _, _)               -- Nothing matches
                -> do { traceTcS "matchClass not matching" $
                        vcat [ text "dict" <+> ppr pred ]
                      ; return NoInstance }

            ([(ispec, inst_tys)], [], _) -- A single match
                | not (xopt Opt_IncoherentInstances dflags)
                , not (isEmptyBag unifiable_givens)
                -> -- See Note [Instance and Given overlap]
                   do { traceTcS "Delaying instance application" $
                          vcat [ text "Work item=" <+> pprType (mkClassPred clas tys)
                               , text "Relevant given dictionaries=" <+> ppr unifiable_givens ]
                      ; return NoInstance  }

                | otherwise
                -> do   { let dfun_id = instanceDFunId ispec
                        ; traceTcS "matchClass success" $
                          vcat [text "dict" <+> ppr pred,
                                text "witness" <+> ppr dfun_id
                                               <+> ppr (idType dfun_id) ]
                                  -- Record that this dfun is needed
                        ; match_one dfun_id inst_tys }

            (matches, _, _)    -- More than one matches
                               -- Defer any reactions of a multitude
                               -- until we learn more about the reagent
                -> do   { traceTcS "matchClass multiple matches, deferring choice" $
                          vcat [text "dict" <+> ppr pred,
                                text "matches" <+> ppr matches]
                        ; return NoInstance } }
   where
     pred = mkClassPred clas tys

     match_one :: DFunId -> [Maybe TcType] -> TcS LookupInstResult
                  -- See Note [DFunInstType: instantiating types] in InstEnv
     match_one dfun_id mb_inst_tys
       = do { checkWellStagedDFun pred dfun_id loc
            ; (tys, dfun_phi) <- instDFunType dfun_id mb_inst_tys
            ; let (theta, _) = tcSplitPhiTy dfun_phi
            ; if null theta then
                  return (GenInst [] (EvDFunApp dfun_id tys []))
              else do
            { evc_vars <- instDFunConstraints loc theta
            ; let new_ev_vars = freshGoals evc_vars
                      -- new_ev_vars are only the real new variables that can be emitted
                  dfun_app = EvDFunApp dfun_id tys (map (ctEvTerm . fst) evc_vars)
            ; return $ GenInst new_ev_vars dfun_app } }

     unifiable_givens :: Cts
     unifiable_givens = filterBag matchable $
                        findDictsByClass (inert_dicts $ inert_cans inerts) clas

     matchable (CDictCan { cc_class = clas_g, cc_tyargs = sys, cc_ev = fl })
       | isGiven fl
       , Just {} <- tcUnifyTys bind_meta_tv tys sys
       = ASSERT( clas_g == clas ) True
       | otherwise = False -- No overlap with a solved, already been taken care of
                           -- by the overlap check with the instance environment.

     matchable ct = pprPanic "Expecting dictionary!" (ppr ct)

     bind_meta_tv :: TcTyVar -> BindFlag
     -- Any meta tyvar may be unified later, so we treat it as
     -- bindable when unifying with givens. That ensures that we
     -- conservatively assume that a meta tyvar might get unified with
     -- something that matches the 'given', until demonstrated
     -- otherwise.
     bind_meta_tv tv | isMetaTyVar tv = BindMe
                     | otherwise      = Skolem

{- Note [Instance and Given overlap]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Example, from the OutsideIn(X) paper:
       instance P x => Q [x]
       instance (x ~ y) => R y [x]

       wob :: forall a b. (Q [b], R b a) => a -> Int

       g :: forall a. Q [a] => [a] -> Int
       g x = wob x

This will generate the impliation constraint:
            Q [a] => (Q [beta], R beta [a])
If we react (Q [beta]) with its top-level axiom, we end up with a
(P beta), which we have no way of discharging. On the other hand,
if we react R beta [a] with the top-level we get  (beta ~ a), which
is solvable and can help us rewrite (Q [beta]) to (Q [a]) which is
now solvable by the given Q [a].

The solution is that:
  In matchClassInst (and thus in topReact), we return a matching
  instance only when there is no Given in the inerts which is
  unifiable to this particular dictionary.

  We treat any meta-tyvar as "unifiable" for this purpose,
  *including* untouchable ones

The end effect is that, much as we do for overlapping instances, we
delay choosing a class instance if there is a possibility of another
instance OR a given to match our constraint later on. This fixes
Trac #4981 and #5002.

Other notes:

* This is arguably not easy to appear in practice due to our
  aggressive prioritization of equality solving over other
  constraints, but it is possible. I've added a test case in
  typecheck/should-compile/GivenOverlapping.hs

* Another "live" example is Trac #10195

* We ignore the overlap problem if -XIncoherentInstances is in force:
  see Trac #6002 for a worked-out example where this makes a
  difference.

* Moreover notice that our goals here are different than the goals of
  the top-level overlapping checks. There we are interested in
  validating the following principle:

      If we inline a function f at a site where the same global
      instance environment is available as the instance environment at
      the definition site of f then we should get the same behaviour.

  But for the Given Overlap check our goal is just related to completeness of
  constraint solving.
-}

-- | Assumes that we've checked that this is the 'Typeable' class,
-- and it was applied to the correc arugment.
matchTypeableClass :: Class -> Kind -> Type -> CtLoc -> TcS LookupInstResult
matchTypeableClass clas _k t loc

  -- See Note [No Typeable for qualified types]
  | isForAllTy t                               = return NoInstance
  -- Is the type of the form `C => t`?
  | Just (t1,_) <- splitFunTy_maybe t,
    isConstraintKind (typeKind t1)             = return NoInstance

  | Just (tc, ks) <- splitTyConApp_maybe t
  , all isKind ks                              = doTyCon tc ks
  | Just (f,kt)       <- splitAppTy_maybe t    = doTyApp f kt
  | Just _            <- isNumLitTy t          = mkSimpEv (EvTypeableTyLit t)
  | Just _            <- isStrLitTy t          = mkSimpEv (EvTypeableTyLit t)
  | otherwise                                  = return NoInstance

  where
  -- Representation for type constructor applied to some kinds
  doTyCon tc ks =
    case mapM kindRep ks of
      Nothing    -> return NoInstance
      Just kReps -> mkSimpEv (EvTypeableTyCon tc kReps)

  {- Representation for an application of a type to a type-or-kind.
  This may happen when the type expression starts with a type variable.
  Example (ignoring kind parameter):
    Typeable (f Int Char)                      -->
    (Typeable (f Int), Typeable Char)          -->
    (Typeable f, Typeable Int, Typeable Char)  --> (after some simp. steps)
    Typeable f
  -}
  doTyApp f tk
    | isKind tk = return NoInstance -- We can't solve until we know the ctr.
    | otherwise =
      do ct1 <- subGoal f
         ct2 <- subGoal tk
         let realSubs = [ c | (c,Fresh) <- [ct1,ct2] ]
         return $ GenInst realSubs
                $ EvTypeable $ EvTypeableTyApp (getEv ct1,f) (getEv ct2,tk)


  -- Representation for concrete kinds.  We just use the kind itself,
  -- but first check to make sure that it is "simple" (i.e., made entirely
  -- out of kind constructors).
  kindRep ki = do (_,ks) <- splitTyConApp_maybe ki
                  mapM_ kindRep ks
                  return ki

  getEv (ct,_fresh) = ctEvTerm ct

  -- Emit a `Typeable` constraint for the given type.
  subGoal ty = do let goal = mkClassPred clas [ typeKind ty, ty ]
                  newWantedEvVar loc goal

  mkSimpEv ev = return (GenInst [] (EvTypeable ev))

{- Note [No Typeable for polytype or for constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We do not support impredicative typeable, such as
   Typeable (forall a. a->a)
   Typeable (Eq a => a -> a)
   Typeable (() => Int)
   Typeable (((),()) => Int)

See Trac #9858.  For forall's the case is clear: we simply don't have
a TypeRep for them.  For qualified but not polymorphic types, like
(Eq a => a -> a), things are murkier.  But:

 * We don't need a TypeRep for these things.  TypeReps are for
   monotypes only.

  * Perhaps we could treat `=>` as another type constructor for `Typeable`
    purposes, and thus support things like `Eq Int => Int`, however,
    at the current state of affairs this would be an odd exception as
    no other class works with impredicative types.
    For now we leave it off, until we have a better story for impredicativity.
-}

-- | Is the constraint for an implicit CallStack parameter?
isCallStackIP :: CtLoc -> Class -> Type -> Maybe (EvTerm -> EvCallStack)
isCallStackIP loc cls ty
  | Just (tc, []) <- splitTyConApp_maybe ty
  , cls `hasKey` ipClassNameKey && tc `hasKey` callStackTyConKey
  = occOrigin (ctLocOrigin loc)
  where
  -- We only want to grab constraints that arose due to the use of an IP or a
  -- function call. See Note [Overview of implicit CallStacks]
  occOrigin (OccurrenceOf n)
    = Just (EvCsPushCall n locSpan)
  occOrigin (IPOccOrigin n)
    = Just (EvCsTop ('?' `consFS` hsIPNameFS n) locSpan)
  occOrigin _
    = Nothing
  locSpan
    = ctLocSpan loc
isCallStackIP _ _ _
  = Nothing

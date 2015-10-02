{-# LANGUAGE CPP #-}

module TcInteract (
     solveSimpleGivens,   -- Solves [EvVar],GivenLoc
     solveSimpleWanteds   -- Solves Cts
  ) where

#include "HsVersions.h"

import BasicTypes ( infinity, IntWithInf, intGtLimit )
import HsTypes ( hsIPNameFS )
import FastString
import TcCanonical
import TcFlatten
import VarSet
import Type
import Kind ( isKind )
import InstEnv( DFunInstType, lookupInstEnv, instanceDFunId )
import CoAxiom(sfInteractTop, sfInteractInert)

import Var
import TcType
import PrelNames ( knownNatClassName, knownSymbolClassName,
                   callStackTyConKey, typeableClassName )
import TysWiredIn ( ipClass, typeNatKind, typeSymbolKind )
import Id( idType )
import CoAxiom ( Eqn, CoAxiom(..), CoAxBranch(..), fromBranches )
import Class
import TyCon
import DataCon( dataConWrapId )
import FunDeps
import FamInst
import FamInstEnv
import Inst( tyVarsOfCt )
import Unify ( tcUnifyTyWithTFs )

import TcEvidence
import Outputable

import TcRnTypes
import TcSMonad
import Bag
import MonadUtils ( concatMapM )

import Data.List( partition, foldl', deleteFirstsBy )
import SrcLoc
import VarEnv

import Control.Monad
import Maybes( isJust )
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
   need to worry about them across successive passes over the constraint tree.
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
set, unflatten and zonk the wanteds.  It passes the zonked wanteds
to runTcPluginsWanteds, which produces a replacement set of wanteds,
some additional insolubles and a flag indicating whether to go round
the loop again.  If so, prepareInertsForImplications is used to remove
the previous wanteds (which will still be in the inert set).  Note
that prepareInertsForImplications will discard the insolubles, so we
must keep track of them separately.
-}

solveSimpleGivens :: CtLoc -> [EvVar] -> TcS Cts
-- Solves the givens, adding them to the inert set
-- Returns any insoluble givens, which represent inaccessible code,
-- taking those ones out of the inert set
solveSimpleGivens loc givens
  | null givens  -- Shortcut for common case
  = return emptyCts
  | otherwise
  = do { go (map mk_given_ct givens)
       ; takeGivenInsolubles }
  where
    mk_given_ct ev_id = mkNonCanonical (CtGiven { ctev_evar = ev_id
                                                , ctev_pred = evVarPred ev_id
                                                , ctev_loc  = loc })
    go givens = do { solveSimples (listToBag givens)
                   ; new_givens <- runTcPluginsGiven
                   ; when (notNull new_givens) (go new_givens)
                   }

solveSimpleWanteds :: Cts -> TcS WantedConstraints
-- NB: 'simples' may contain /derived/ equalities, floated
--     out from a nested implication. So don't discard deriveds!
solveSimpleWanteds simples
  = do { traceTcS "solveSimples {" (ppr simples)
       ; dflags <- getDynFlags
       ; (n,wc) <- go 1 (solverIterations dflags) (emptyWC { wc_simple = simples })
       ; traceTcS "solveSimples end }" $
             vcat [ ptext (sLit "iterations =") <+> ppr n
                  , ptext (sLit "residual =") <+> ppr wc ]
       ; return wc }
  where
    go :: Int -> IntWithInf -> WantedConstraints -> TcS (Int, WantedConstraints)
    go n limit wc
      | n `intGtLimit` limit
      = failTcS (hang (ptext (sLit "solveSimpleWanteds: too many iterations")
                       <+> parens (ptext (sLit "limit =") <+> ppr limit))
                    2 (vcat [ ptext (sLit "Set limit with -fsolver-iterations=n; n=0 for no limit")
                            , ptext (sLit "Simples =") <+> ppr simples
                            , ptext (sLit "WC =")      <+> ppr wc ]))

     | isEmptyBag (wc_simple wc)
     = return (n,wc)

     | otherwise
     = do { -- Solve
            (unif_count, wc1) <- solve_simple_wanteds wc

            -- Run plugins
          ; (rerun_plugin, wc2) <- runTcPluginsWanted wc1
             -- See Note [Running plugins on unflattened wanteds]

          ; if unif_count == 0 && not rerun_plugin
            then return (n, wc2)             -- Done
            else do { traceTcS "solveSimple going round again:" (ppr rerun_plugin)
                    ; go (n+1) limit wc2 } }      -- Loop


solve_simple_wanteds :: WantedConstraints -> TcS (Int, WantedConstraints)
-- Try solving these constraints
-- Affects the unification state (of course) but not the inert set
solve_simple_wanteds (WC { wc_simple = simples1, wc_insol = insols1, wc_impl = implics1 })
  = nestTcS $
    do { solveSimples simples1
       ; (implics2, tv_eqs, fun_eqs, insols2, others) <- getUnsolvedInerts
       ; (unif_count, unflattened_eqs) <- reportUnifications $
                                          unflatten tv_eqs fun_eqs
            -- See Note [Unflatten after solving the simple wanteds]
       ; return ( unif_count
                , WC { wc_simple = others `andCts` unflattened_eqs
                     , wc_insol  = insols1 `andCts` insols2
                     , wc_impl   = implics1 `unionBags` implics2 }) }

{- Note [The solveSimpleWanteds loop]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Solving a bunch of simple constraints is done in a loop,
(the 'go' loop of 'solveSimpleWanteds'):
  1. Try to solve them; unflattening may lead to improvement that
     was not exploitable during solving
  2. Try the plugin
  3. If step 1 did improvement during unflattening; or if the plugin
     wants to run again, go back to step 1

Non-obviously, improvement can also take place during
the unflattening that takes place in step (1). See TcFlatten,
See Note [Unflattening can force the solver to iterate]
-}

-- The main solver loop implements Note [Basic Simplifier Plan]
---------------------------------------------------------------
solveSimples :: Cts -> TcS ()
-- Returns the final InertSet in TcS
-- Has no effect on work-list or residual-implications
-- The constraints are initially examined in left-to-right order

solveSimples cts
  = {-# SCC "solveSimples" #-}
    do { updWorkListTcS (\wl -> foldrBag extendWorkListCt wl cts)
       ; solve_loop }
  where
    solve_loop
      = {-# SCC "solve_loop" #-}
        do { sel <- selectNextWorkItem
           ; case sel of
              Nothing -> return ()
              Just ct -> do { runSolverPipeline thePipeline ct
                            ; solve_loop } }

-- | Extract the (inert) givens and invoke the plugins on them.
-- Remove solved givens from the inert set and emit insolubles, but
-- return new work produced so that 'solveSimpleGivens' can feed it back
-- into the main solver.
runTcPluginsGiven :: TcS [Ct]
runTcPluginsGiven
  = do { plugins <- getTcPlugins
       ; if null plugins then return [] else
    do { givens <- getInertGivens
       ; if null givens then return [] else
    do { p <- runTcPlugins plugins (givens,[],[])
       ; let (solved_givens, _, _) = pluginSolvedCts p
       ; updInertCans (removeInertCts solved_givens)
       ; mapM_ emitInsoluble (pluginBadCts p)
       ; return (pluginNewCts p) } } }

-- | Given a bag of (flattened, zonked) wanteds, invoke the plugins on
-- them and produce an updated bag of wanteds (possibly with some new
-- work) and a bag of insolubles.  The boolean indicates whether
-- 'solveSimpleWanteds' should feed the updated wanteds back into the
-- main solver.
runTcPluginsWanted :: WantedConstraints -> TcS (Bool, WantedConstraints)
runTcPluginsWanted wc@(WC { wc_simple = simples1, wc_insol = insols1, wc_impl = implics1 })
  | isEmptyBag simples1
  = return (False, wc)
  | otherwise
  = do { plugins <- getTcPlugins
       ; if null plugins then return (False, wc) else

    do { given <- getInertGivens
       ; simples1 <- zonkSimples simples1    -- Plugin requires zonked inputs
       ; let (wanted, derived) = partition isWantedCt (bagToList simples1)
       ; p <- runTcPlugins plugins (given, derived, wanted)
       ; let (_, _,                solved_wanted)   = pluginSolvedCts p
             (_, unsolved_derived, unsolved_wanted) = pluginInputCts p
             new_wanted                             = pluginNewCts p

-- SLPJ: I'm deeply suspicious of this
--       ; updInertCans (removeInertCts $ solved_givens ++ solved_deriveds)

       ; mapM_ setEv solved_wanted
       ; return ( notNull (pluginNewCts p)
                , WC { wc_simple = listToBag new_wanted `andCts` listToBag unsolved_wanted
                                                        `andCts` listToBag unsolved_derived
                     , wc_insol  = listToBag (pluginBadCts p) `andCts` insols1
                     , wc_impl   = implics1 } ) } }
  where
    setEv :: (EvTerm,Ct) -> TcS ()
    setEv (ev,ct) = case ctEvidence ct of
      CtWanted {ctev_evar = evar} -> setWantedEvBind evar ev
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

getTcPlugins :: TcS [TcPluginSolver]
getTcPlugins = do { tcg_env <- getGblEnv; return (tcg_tc_plugins tcg_env) }

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
runTcPlugins :: [TcPluginSolver] -> SplitCts -> TcS TcPluginProgress
runTcPlugins plugins all_cts
  = foldM do_plugin initialProgress plugins
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
                                 ; traceTcS "End solver pipeline (kept as inert) }" $
                                       vcat [ ptext (sLit "final_item =") <+> ppr ct
                                            , pprTvBndrs (varSetElems $ tyVarsOfCt ct)
                                            , ptext (sLit "inerts     =") <+> ppr final_is]
                                 ; addInertCan ct }
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

data InteractResult
   = IRKeep      -- Keep the existing inert constraint in the inert set
   | IRReplace   -- Replace the existing inert constraint with the work item
   | IRDelete    -- Delete the existing inert constraint from the inert set

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
  | isDerived ev_w         -- Work item is Derived; just discard it
  = return (IRKeep, True)

  | isDerived ev_i            -- The inert item is Derived, we can just throw it away,
  = return (IRDelete, False)  -- The ev_w is inert wrt earlier inert-set items,
                              -- so it's safe to continue on from this point

  | CtWanted { ctev_loc = loc_w } <- ev_w
  , prohibitedSuperClassSolve (ctEvLoc ev_i) loc_w
  = return (IRDelete, False)

  | CtWanted { ctev_evar = ev_id } <- ev_w   -- Inert is Given or Wanted
  = do { setWantedEvBind ev_id (ctEvTerm ev_i)
       ; return (IRKeep, True) }

  | CtWanted { ctev_loc = loc_i } <- ev_i   -- Work item is Given
  , prohibitedSuperClassSolve (ctEvLoc ev_w) loc_i
  = return (IRKeep, False)  -- Just discard the un-usable Given
                            -- This never actually happens because
                            -- Givens get processed first

  | CtWanted { ctev_evar = ev_id } <- ev_i   -- Work item is Given
  = do { setWantedEvBind ev_id (ctEvTerm ev_w)
       ; return (IRReplace, True) }

  -- So they are both Given
  -- See Note [Replacement vs keeping]
  | lvl_i == lvl_w
  = do { binds <- getTcEvBindsMap
       ; return (same_level_strategy binds, True) }

  | otherwise   -- Both are Given, levels differ
  = return (different_level_strategy, True)
  where
     pred  = ctEvPred ev_i
     loc_i = ctEvLoc ev_i
     loc_w = ctEvLoc ev_w
     lvl_i = ctLocLevel loc_i
     lvl_w = ctLocLevel loc_w

     different_level_strategy
       | isIPPred pred, lvl_w > lvl_i = IRReplace
       | lvl_w < lvl_i                = IRReplace
       | otherwise                    = IRKeep

     same_level_strategy binds        -- Both Given
       | GivenOrigin (InstSC s_i) <- ctLocOrigin loc_i
       = case ctLocOrigin loc_w of
            GivenOrigin (InstSC s_w) | s_w < s_i -> IRReplace
                                     | otherwise -> IRKeep
            _                                    -> IRReplace

       | GivenOrigin (InstSC {}) <- ctLocOrigin loc_w
       = IRKeep

       | has_binding binds ev_w
       , not (has_binding binds ev_i)
       = IRReplace

       | otherwise = IRKeep

     has_binding binds ev = isJust (lookupEvBind binds (ctEvId ev))

{-
Note [Replacement vs keeping]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we have two Given constraints both of type (C tys), say, which should
we keep?  More subtle than you might think!

  * Constraints come from different levels (different_level_strategy)

      - For implicit parameters we want to keep the innermost (deepest)
        one, so that it overrides the outer one.
        See Note [Shadowing of Implicit Parameters]

      - For everything else, we want to keep the outermost one.  Reason: that
        makes it more likely that the inner one will turn out to be unused,
        and can be reported as redundant.  See Note [Tracking redundant constraints]
        in TcSimplify.

        It transpires that using the outermost one is reponsible for an
        8% performance improvement in nofib cryptarithm2, compared to
        just rolling the dice.  I didn't investigate why.

  * Constaints coming from the same level (i.e. same implication)

       - Always get rid of InstSC ones if possible, since they are less
         useful for solving.  If both are InstSC, choose the one with
         the smallest TypeSize
         See Note [Solving superclass constraints] in TcInstDcls

       - Keep the one that has a non-trivial evidence binding.
         Note [Tracking redundant constraints] again.
            Example:  f :: (Eq a, Ord a) => blah
            then we may find [G] sc_sel (d1::Ord a) :: Eq a
                             [G] d2 :: Eq a
            We want to discard d2 in favour of the superclass selection from
            the Ord dictionary.

  * Finally, when there is still a choice, use IRKeep rather than
    IRReplace, to avoid unnecessary munging of the inert set.

Doing the depth-check for implicit parameters, rather than making the work item
always overrride, is important.  Consider

    data T a where { T1 :: (?x::Int) => T Int; T2 :: T a }

    f :: (?x::a) => T a -> Int
    f T1 = ?x
    f T2 = 3

We have a [G] (?x::a) in the inert set, and at the pattern match on T1 we add
two new givens in the work-list:  [G] (?x::Int)
                                  [G] (a ~ Int)
Now consider these steps
  - process a~Int, kicking out (?x::a)
  - process (?x::Int), the inner given, adding to inert set
  - process (?x::a), the outer given, overriding the inner given
Wrong!  The depth-check ensures that the inner implicit parameter wins.
(Actually I think that the order in which the work-list is processed means
that this chain of events won't happen, but that's very fragile.)

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
  | Just mkEvCs <- isCallStackIP (ctEvLoc ev_w) cls tys
  , isWanted ev_w
  = do let ev_cs =
             case lookupInertDict inerts cls tys of
               Just ev | isGiven ev -> mkEvCs (ctEvTerm ev)
               _ -> mkEvCs (EvCallStack EvCsEmpty)

       -- now we have ev_cs :: CallStack, but the evidence term should
       -- be a dictionary, so we have to coerce ev_cs to a
       -- dictionary for `IP ip CallStack`
       let ip_ty = mkClassPred cls tys
       let ev_tm = mkEvCast (EvCallStack ev_cs) (TcCoercion $ wrapIP ip_ty)
       addSolvedDict ev_w cls tys
       setWantedEvBind (ctEvId ev_w) ev_tm
       stopWith ev_w "Wanted CallStack IP"

  | Just ctev_i <- lookupInertDict inerts cls tys
  = do { (inert_effect, stop_now) <- solveOneFromTheOther ctev_i ev_w
       ; case inert_effect of
           IRKeep    -> return ()
           IRDelete  -> updInertDicts $ \ ds -> delDict ds cls tys
           IRReplace -> updInertDicts $ \ ds -> addDict ds cls tys workItem
       ; if stop_now then
            return (Stop ev_w (ptext (sLit "Dict equal") <+> parens (ppr inert_effect)))
         else
            continueWith workItem }

  | cls == ipClass
  , isGiven ev_w
  = interactGivenIP inerts workItem

  | otherwise
  = do { addFunDepWork inerts ev_w cls
       ; continueWith workItem  }

interactDict _ wi = pprPanic "interactDict" (ppr wi)

addFunDepWork :: InertCans -> CtEvidence -> Class -> TcS ()
-- Add derived constraints from type-class functional dependencies.
addFunDepWork inerts work_ev cls
  = mapBagM_ add_fds (findDictsByClass (inert_dicts inerts) cls)
               -- No need to check flavour; fundeps work between
               -- any pair of constraints, regardless of flavour
               -- Importantly we don't throw workitem back in the
               -- worklist because this can cause loops (see #5236)
  where
    work_pred = ctEvPred work_ev
    work_loc  = ctEvLoc work_ev
    add_fds inert_ct
      = emitFunDepDeriveds $
        improveFromAnother derived_loc inert_pred work_pred
               -- We don't really rewrite tys2, see below _rewritten_tys2, so that's ok
               -- NB: We do create FDs for given to report insoluble equations that arise
               -- from pairs of Givens, and also because of floating when we approximate
               -- implications. The relevant test is: typecheck/should_fail/FDsFromGivens.hs
      where
        inert_pred = ctPred inert_ct
        inert_loc  = ctLoc inert_ct
        derived_loc = work_loc { ctl_origin = FunDepOrigin1 work_pred  work_loc
                                                            inert_pred inert_loc }

{-
*********************************************************************************
*                                                                               *
                   Implicit parameters
*                                                                               *
*********************************************************************************
-}

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
  = if ev_i `canDischarge` ev
    then  -- Rewrite work-item using inert
      do { traceTcS "reactFunEq (discharge work item):" $
           vcat [ text "workItem =" <+> ppr workItem
                , text "inertItem=" <+> ppr ev_i ]
         ; reactFunEq ev_i fsk_i ev fsk
         ; stopWith ev "Inert rewrites work item" }
    else  -- Rewrite inert using work-item
      ASSERT2( ev `canDischarge` ev_i, ppr ev $$ ppr ev_i )
      do { traceTcS "reactFunEq (rewrite inert item):" $
           vcat [ text "workItem =" <+> ppr workItem
                , text "inertItem=" <+> ppr ev_i ]
         ; updInertFunEqs $ \ feqs -> insertFunEq feqs tc args workItem
               -- Do the updInertFunEqs before the reactFunEq, so that
               -- we don't kick out the inertItem as well as consuming it!
         ; reactFunEq ev fsk ev_i fsk_i
         ; stopWith ev "Work item rewrites inert" }

  | otherwise   -- Try improvement
  = do { improveLocalFunEqs loc inerts tc args fsk
       ; continueWith workItem }
  where
    loc             = ctEvLoc ev
    funeqs          = inert_funeqs inerts
    matching_inerts = findFunEqs funeqs tc args

interactFunEq _ workItem = pprPanic "interactFunEq" (ppr workItem)

improveLocalFunEqs :: CtLoc -> InertCans -> TyCon -> [TcType] -> TcTyVar
                   -> TcS ()
-- Generate derived improvement equalities, by comparing
-- the current work item with inert CFunEqs
-- E.g.   x + y ~ z,   x + y' ~ z   =>   [D] y ~ y'
improveLocalFunEqs loc inerts fam_tc args fsk
  | not (null improvement_eqns)
  = do { traceTcS "interactFunEq improvements: " $
         vcat [ ptext (sLit "Eqns:") <+> ppr improvement_eqns
              , ptext (sLit "Candidates:") <+> ppr funeqs_for_tc
              , ptext (sLit "TvEqs:") <+> ppr tv_eqs ]
       ; mapM_ (unifyDerived loc Nominal) improvement_eqns }
  | otherwise
  = return ()
  where
    tv_eqs        = inert_model inerts
    funeqs        = inert_funeqs inerts
    funeqs_for_tc = findFunEqsByTyCon funeqs fam_tc
    rhs           = lookupFlattenTyVar tv_eqs fsk

    --------------------
    improvement_eqns
      | Just ops <- isBuiltInSynFamTyCon_maybe fam_tc
      =    -- Try built-in families, notably for arithmethic
         concatMap (do_one_built_in ops) funeqs_for_tc

      | Injective injective_args <- familyTyConInjectivityInfo fam_tc
      =    -- Try improvement from type families with injectivity annotations
         concatMap (do_one_injective injective_args) funeqs_for_tc

      | otherwise
      = []

    --------------------
    do_one_built_in ops (CFunEqCan { cc_tyargs = iargs, cc_fsk = ifsk })
      = sfInteractInert ops args rhs iargs (lookupFlattenTyVar tv_eqs ifsk)
    do_one_built_in _ _ = pprPanic "interactFunEq 1" (ppr fam_tc)

    --------------------
    -- See Note [Type inference for type families with injectivity]
    do_one_injective injective_args
                    (CFunEqCan { cc_tyargs = iargs, cc_fsk = ifsk })
      | rhs `tcEqType` lookupFlattenTyVar tv_eqs ifsk
      = [Pair arg iarg | (arg, iarg, True)
                           <- zip3 args iargs injective_args ]
      | otherwise
      = []
    do_one_injective _ _ = pprPanic "interactFunEq 2" (ppr fam_tc)

-------------
lookupFlattenTyVar :: InertModel -> TcTyVar -> TcType
-- ^ Look up a flatten-tyvar in the inert nominal TyVarEqs;
-- this is used only when dealing with a CFunEqCan
lookupFlattenTyVar model ftv
  = case lookupVarEnv model ftv of
      Just (CTyEqCan { cc_rhs = rhs, cc_eq_rel = NomEq }) -> rhs
      _                                                   -> mkTyVarTy ftv

reactFunEq :: CtEvidence -> TcTyVar    -- From this  :: F tys ~ fsk1
           -> CtEvidence -> TcTyVar    -- Solve this :: F tys ~ fsk2
           -> TcS ()
reactFunEq from_this fsk1 (CtGiven { ctev_evar = evar, ctev_loc = loc }) fsk2
  = do { let fsk_eq_co = mkTcSymCo (mkTcCoVarCo evar)
                         `mkTcTransCo` ctEvCoercion from_this
                         -- :: fsk2 ~ fsk1
             fsk_eq_pred = mkTcEqPred (mkTyVarTy fsk2) (mkTyVarTy fsk1)
       ; new_ev <- newGivenEvVar loc (fsk_eq_pred, EvCoercion fsk_eq_co)
       ; emitWorkNC [new_ev] }

reactFunEq from_this fuv1 ev fuv2
  = do { traceTcS "reactFunEq" (ppr from_this $$ ppr fuv1 $$ ppr ev $$ ppr fuv2)
       ; dischargeFmv ev fuv2 (ctEvCoercion from_this) (mkTyVarTy fuv1)
       ; traceTcS "reactFunEq done" (ppr from_this $$ ppr fuv1 $$ ppr ev $$ ppr fuv2) }

{-
Note [Type inference for type families with injectivity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have a type family with an injectivity annotation:
    type family F a b = r | r -> b

Then if we have two CFunEqCan constraints for F with the same RHS
   F s1 t1 ~ rhs
   F s2 t2 ~ rhs
then we can use the injectivity to get a new Derived constraint on
the injective argument
  [D] t1 ~ t2

That in turn can help GHC solve constraints that would otherwise require
guessing.  For example, consider the ambiguity check for
   f :: F Int b -> Int
We get the constraint
   [W] F Int b ~ F Int beta
where beta is a unification variable.  Injectivity lets us pick beta ~ b.

Injectivity information is also used at the call sites. For example:
   g = f True
gives rise to
   [W] F Int b ~ Bool
from which we can derive b.  This requires looking at the defining equations of
a type family, ie. finding equation with a matching RHS (Bool in this example)
and infering values of type variables (b in this example) from the LHS patterns
of the matching equation.  For closed type families we have to perform
additional apartness check for the selected equation to check that the selected
is guaranteed to fire for given LHS arguments.

These new constraints are simply *Derived* constraints; they have no evidence.
We could go further and offer evidence from decomposing injective type-function
applications, but that would require new evidence forms, and an extension to
FC, so we don't do that right now (Dec 14).

See also Note [Injective type families] in TyCon


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
                         , ev_i `canDischarge` ev
                         , rhs_i `tcEqType` rhs ]
  =  -- Inert:     a ~ b
     -- Work item: a ~ b
    do { setEvBindIfWanted ev (ctEvTerm ev_i)
       ; stopWith ev "Solved from inert" }

  | Just tv_rhs <- getTyVar_maybe rhs
  , (ev_i : _) <- [ ev_i | CTyEqCan { cc_ev = ev_i, cc_rhs = rhs_i }
                             <- findTyEqs inerts tv_rhs
                         , ev_i `canDischarge` ev
                         , rhs_i `tcEqType` mkTyVarTy tv ]
  =  -- Inert:     a ~ b
     -- Work item: b ~ a
    do { setEvBindIfWanted ev
                   (EvCoercion (mkTcSymCo (ctEvCoercion ev_i)))
       ; stopWith ev "Solved from inert (r)" }

  | otherwise
  = do { tclvl <- getTcLevel
       ; if canSolveByUnification tclvl ev eq_rel tv rhs
         then do { solveByUnification ev tv rhs
                 ; n_kicked <- kickOutAfterUnification tv
                 ; return (Stop ev (ptext (sLit "Solved by unification") <+> ppr_kicked n_kicked)) }

         else do { traceTcS "Can't solve tyvar equality"
                       (vcat [ text "LHS:" <+> ppr tv <+> dcolon <+> ppr (tyVarKind tv)
                             , ppWhen (isMetaTyVar tv) $
                               nest 4 (text "TcLevel of" <+> ppr tv
                                       <+> text "is" <+> ppr (metaTyVarTcLevel tv))
                             , text "RHS:" <+> ppr rhs <+> dcolon <+> ppr (typeKind rhs)
                             , text "TcLevel =" <+> ppr tclvl ])
                 ; addInertEq workItem
                 ; return (Stop ev (ptext (sLit "Kept as inert"))) } }

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

       ; unifyTyVar tv xi'
       ; setEvBindIfWanted wd (EvCoercion (mkTcNomReflCo xi')) }

ppr_kicked :: Int -> SDoc
ppr_kicked 0 = empty
ppr_kicked n = parens (int n <+> ptext (sLit "kicked out"))

{- Note [Avoid double unifications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

To achieve this required some refactoring of FunDeps.hs (nicer
now!).
-}

emitFunDepDeriveds :: [FunDepEqn CtLoc] -> TcS ()
emitFunDepDeriveds fd_eqns
  = mapM_ do_one_FDEqn fd_eqns
  where
    do_one_FDEqn (FDEqn { fd_qtvs = tvs, fd_eqs = eqs, fd_loc = loc })
     | null tvs  -- Common shortcut
     = mapM_ (unifyDerived loc Nominal) eqs
     | otherwise
     = do { (subst, _) <- instFlexiTcS tvs  -- Takes account of kind substitution
          ; mapM_ (do_one_eq loc subst) eqs }

    do_one_eq loc subst (Pair ty1 ty2)
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
 = do { tir <- doTopReact wi
      ; case tir of
          ContinueWith wi -> continueWith wi
          Stop ev s       -> return (Stop ev (ptext (sLit "Top react:") <+> s)) }

doTopReact :: WorkItem -> TcS (StopOrContinue Ct)
-- The work item does not react with the inert set, so try interaction with top-level
-- instances. Note:
--
--   (a) The place to add superclasses in not here in doTopReact stage.
--       Instead superclasses are added in the worklist as part of the
--       canonicalization process. See Note [Adding superclasses].

doTopReact work_item
  = do { traceTcS "doTopReact" (ppr work_item)
       ; case work_item of
           CDictCan {}  -> do { inerts <- getTcSInerts
                              ; doTopReactDict inerts work_item }
           CFunEqCan {} -> doTopReactFunEq work_item
           _  -> -- Any other work item does not react with any top-level equations
                 continueWith work_item  }

--------------------
doTopReactDict :: InertSet -> Ct -> TcS (StopOrContinue Ct)
-- Try to use type-class instance declarations to simplify the constraint
doTopReactDict inerts work_item@(CDictCan { cc_ev = fl, cc_class = cls
                                          , cc_tyargs = xis })
  | isGiven fl   -- Never use instances for Given constraints
  = do { try_fundep_improvement
       ; continueWith work_item }

  | Just ev <- lookupSolvedDict inerts cls xis   -- Cached
  = do { setEvBindIfWanted fl (ctEvTerm ev);
       ; stopWith fl "Dict/Top (cached)" }

  | isDerived fl  -- Use type-class instances for Deriveds, in the hope
                  -- of generating some improvements
                  -- C.f. Example 3 of Note [The improvement story]
                  -- It's easy because no evidence is involved
   = do { dflags <- getDynFlags
        ; lkup_inst_res <- matchClassInst dflags inerts cls xis dict_loc
        ; case lkup_inst_res of
               GenInst preds _ s -> do { emitNewDeriveds dict_loc preds
                                       ; unless s $
                                           insertSafeOverlapFailureTcS work_item
                                       ; stopWith fl "Dict/Top (solved)" }

               NoInstance        -> do { -- If there is no instance, try improvement
                                         try_fundep_improvement
                                       ; continueWith work_item } }

  | otherwise  -- Wanted, but not cached
   = do { dflags <- getDynFlags
        ; lkup_inst_res <- matchClassInst dflags inerts cls xis dict_loc
        ; case lkup_inst_res of
               GenInst theta mk_ev s -> do { addSolvedDict fl cls xis
                                           ; unless s $
                                               insertSafeOverlapFailureTcS work_item
                                           ; solve_from_instance theta mk_ev }
               NoInstance            -> do { try_fundep_improvement
                                           ; continueWith work_item } }
   where
     dict_pred   = mkClassPred cls xis
     dict_loc    = ctEvLoc fl
     dict_origin = ctLocOrigin dict_loc
     deeper_loc  = zap_origin (bumpCtLocDepth dict_loc)

     zap_origin loc  -- After applying an instance we can set ScOrigin to
                     -- infinity, so that prohibitedSuperClassSolve never fires
       | ScOrigin {} <- dict_origin
       = setCtLocOrigin loc (ScOrigin infinity)
       | otherwise
       = loc

     solve_from_instance :: [TcPredType] -> ([EvId] -> EvTerm) -> TcS (StopOrContinue Ct)
      -- Precondition: evidence term matches the predicate workItem
     solve_from_instance theta mk_ev
        | null theta
        = do { traceTcS "doTopReact/found nullary instance for" $ ppr fl
             ; setWantedEvBind (ctEvId fl) (mk_ev [])
             ; stopWith fl "Dict/Top (solved, no new work)" }
        | otherwise
        = do { checkReductionDepth deeper_loc dict_pred
             ; traceTcS "doTopReact/found non-nullary instance for" $ ppr fl
             ; evc_vars <- mapM (newWantedEvVar deeper_loc) theta
             ; setWantedEvBind (ctEvId fl) (mk_ev (map (ctEvId . fst) evc_vars))
             ; emitWorkNC (freshGoals evc_vars)
             ; stopWith fl "Dict/Top (solved, more work)" }

     -- We didn't solve it; so try functional dependencies with
     -- the instance environment, and return
     -- See also Note [Weird fundeps]
     try_fundep_improvement
        = do { traceTcS "try_fundeps" (ppr work_item)
             ; instEnvs <- getInstEnvs
             ; emitFunDepDeriveds $
               improveFromInstEnv instEnvs mk_ct_loc dict_pred }

     mk_ct_loc :: PredType   -- From instance decl
               -> SrcSpan    -- also from instance deol
               -> CtLoc
     mk_ct_loc inst_pred inst_loc
       = dict_loc { ctl_origin = FunDepOrigin2 dict_pred dict_origin
                                               inst_pred inst_loc }

doTopReactDict _ w = pprPanic "doTopReactDict" (ppr w)

--------------------
doTopReactFunEq :: Ct -> TcS (StopOrContinue Ct)
doTopReactFunEq work_item = do { fam_envs <- getFamInstEnvs
                               ; do_top_fun_eq fam_envs work_item }

do_top_fun_eq :: FamInstEnvs -> Ct -> TcS (StopOrContinue Ct)
do_top_fun_eq fam_envs work_item@(CFunEqCan { cc_ev = old_ev, cc_fun = fam_tc
                                            , cc_tyargs = args , cc_fsk = fsk })
  | Just (ax_co, rhs_ty) <- reduceTyFamApp_maybe fam_envs Nominal fam_tc args
                            -- Look up in top-level instances, or built-in axiom
                            -- See Note [MATCHING-SYNONYMS]
  = reduce_top_fun_eq old_ev fsk (TcCoercion ax_co) rhs_ty

  | otherwise
  = do { improveTopFunEqs (ctEvLoc old_ev) fam_envs fam_tc args fsk
       ; continueWith work_item }

do_top_fun_eq _ w = pprPanic "doTopReactFunEq" (ppr w)

reduce_top_fun_eq :: CtEvidence -> TcTyVar -> TcCoercion -> TcType
                  -> TcS (StopOrContinue Ct)
-- Found an applicable top-level axiom: use it to reduce
reduce_top_fun_eq old_ev fsk ax_co rhs_ty
  | Just (tc, tc_args) <- tcSplitTyConApp_maybe rhs_ty
  , isTypeFamilyTyCon tc
  , tc_args `lengthIs` tyConArity tc    -- Short-cut
  = shortCutReduction old_ev fsk ax_co tc tc_args
       -- Try shortcut; see Note [Short cut for top-level reaction]

  | isGiven old_ev  -- Not shortcut
  = do { let final_co = mkTcSymCo (ctEvCoercion old_ev) `mkTcTransCo` ax_co
              -- final_co :: fsk ~ rhs_ty
       ; new_ev <- newGivenEvVar deeper_loc (mkTcEqPred (mkTyVarTy fsk) rhs_ty,
                                             EvCoercion final_co)
       ; emitWorkNC [new_ev] -- Non-cannonical; that will mean we flatten rhs_ty
       ; stopWith old_ev "Fun/Top (given)" }

  -- So old_ev is Wanted or Derived
  | not (fsk `elemVarSet` tyVarsOfType rhs_ty)
  = do { dischargeFmv old_ev fsk ax_co rhs_ty
       ; traceTcS "doTopReactFunEq" $
         vcat [ text "old_ev:" <+> ppr old_ev
              , nest 2 (text ":=") <+> ppr ax_co ]
       ; stopWith old_ev "Fun/Top (wanted)" }

  | otherwise -- We must not assign ufsk := ...ufsk...!
  = do { alpha_ty <- newFlexiTcSTy (tyVarKind fsk)
       ; let pred = mkTcEqPred alpha_ty rhs_ty
       ; new_ev <- case old_ev of
           CtWanted {}  -> do { ev <- newWantedEvVarNC loc pred
                              ; updWorkListTcS (extendWorkListEq (mkNonCanonical ev))
                              ; return ev }
           CtDerived {} -> do { ev <- newDerivedNC loc pred
                              ; updWorkListTcS (extendWorkListDerived loc ev)
                              ; return ev }
           _ -> pprPanic "reduce_top_fun_eq" (ppr old_ev)

            -- By emitting this as non-canonical, we deal with all
            -- flattening, occurs-check, and ufsk := ufsk issues
       ; let final_co = ax_co `mkTcTransCo` mkTcSymCo (ctEvCoercion new_ev)
            --    ax_co :: fam_tc args ~ rhs_ty
            --       ev :: alpha ~ rhs_ty
            --     ufsk := alpha
            -- final_co :: fam_tc args ~ alpha
       ; dischargeFmv old_ev fsk final_co alpha_ty
       ; traceTcS "doTopReactFunEq (occurs)" $
         vcat [ text "old_ev:" <+> ppr old_ev
              , nest 2 (text ":=") <+> ppr final_co
              , text "new_ev:" <+> ppr new_ev ]
       ; stopWith old_ev "Fun/Top (wanted)" }
  where
    loc = ctEvLoc old_ev
    deeper_loc = bumpCtLocDepth loc

improveTopFunEqs :: CtLoc -> FamInstEnvs
                 -> TyCon -> [TcType] -> TcTyVar -> TcS ()
improveTopFunEqs loc fam_envs fam_tc args fsk
  = do { model <- getInertModel
       ; eqns <- improve_top_fun_eqs fam_envs fam_tc args
                                    (lookupFlattenTyVar model fsk)
       ; mapM_ (unifyDerived loc Nominal) eqns }

improve_top_fun_eqs :: FamInstEnvs
                    -> TyCon -> [TcType] -> TcType
                    -> TcS [Eqn]
improve_top_fun_eqs fam_envs fam_tc args rhs_ty
  | Just ops <- isBuiltInSynFamTyCon_maybe fam_tc
  = return (sfInteractTop ops args rhs_ty)

  -- see Note [Type inference for type families with injectivity]
  | isOpenTypeFamilyTyCon fam_tc
  , Injective injective_args <- familyTyConInjectivityInfo fam_tc
  = -- it is possible to have several compatible equations in an open type
    -- family but we only want to derive equalities from one such equation.
    concatMapM (injImproveEqns injective_args) (take 1 $
      buildImprovementData (lookupFamInstEnvByTyCon fam_envs fam_tc)
                           fi_tys fi_rhs (const Nothing))

  | Just ax <- isClosedSynFamilyTyConWithAxiom_maybe fam_tc
  , Injective injective_args <- familyTyConInjectivityInfo fam_tc
  = concatMapM (injImproveEqns injective_args) $
      buildImprovementData (fromBranches (co_ax_branches ax))
                           cab_lhs cab_rhs Just

  | otherwise
  = return []
     where
      buildImprovementData
          :: [a]                     -- axioms for a TF (FamInst or CoAxBranch)
          -> (a -> [Type])           -- get LHS of an axiom
          -> (a -> Type)             -- get RHS of an axiom
          -> (a -> Maybe CoAxBranch) -- Just => apartness check required
          -> [( [Type], TvSubst, TyVarSet, Maybe CoAxBranch )]
             -- Result:
             -- ( [arguments of a matching axiom]
             -- , RHS-unifying substitution
             -- , axiom variables without substitution
             -- , Maybe matching axiom [Nothing - open TF, Just - closed TF ] )
      buildImprovementData axioms axiomLHS axiomRHS wrap =
          [ (ax_args, subst, unsubstTvs, wrap axiom)
          | axiom <- axioms
          , let ax_args = axiomLHS axiom
          , let ax_rhs  = axiomRHS axiom
          , Just subst <- [tcUnifyTyWithTFs False ax_rhs rhs_ty]
          , let tvs           = tyVarsOfTypes ax_args
                notInSubst tv = not (tv `elemVarEnv` getTvSubstEnv subst)
                unsubstTvs    = filterVarSet notInSubst tvs ]

      injImproveEqns :: [Bool]
                     -> ([Type], TvSubst, TyVarSet, Maybe CoAxBranch)
                     -> TcS [Eqn]
      injImproveEqns inj_args (ax_args, theta, unsubstTvs, cabr) = do
        (theta', _) <- instFlexiTcS (varSetElems unsubstTvs)
        let subst = theta `unionTvSubst` theta'
        return [ Pair arg (substTy subst ax_arg)
               | case cabr of
                  Just cabr' -> apartnessCheck (substTys subst ax_args) cabr'
                  _          -> True
               , (arg, ax_arg, True) <- zip3 args ax_args inj_args ]


shortCutReduction :: CtEvidence -> TcTyVar -> TcCoercion
                  -> TyCon -> [TcType] -> TcS (StopOrContinue Ct)
-- See Note [Top-level reductions for type functions]
shortCutReduction old_ev fsk ax_co fam_tc tc_args
  | isGiven old_ev
  = ASSERT( ctEvEqRel old_ev == NomEq )
    do { (xis, cos) <- flattenManyNom old_ev tc_args
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
       ; emitWorkCt new_ct
       ; stopWith old_ev "Fun/Top (given, shortcut)" }

  | otherwise
  = ASSERT( not (isDerived old_ev) )   -- Caller ensures this
    ASSERT( ctEvEqRel old_ev == NomEq )
    do { (xis, cos) <- flattenManyNom old_ev tc_args
               -- ax_co :: F args ~ G tc_args
               -- cos   :: xis ~ tc_args
               -- G cos ; sym ax_co ; old_ev :: G xis ~ fsk
               -- new_ev :: G xis ~ fsk
               -- old_ev :: F args ~ fsk := ax_co ; sym (G cos) ; new_ev

       ; new_ev <- newWantedEvVarNC deeper_loc
                                    (mkTcEqPred (mkTyConApp fam_tc xis) (mkTyVarTy fsk))
       ; setWantedEvBind (ctEvId old_ev)
                   (EvCoercion (ax_co `mkTcTransCo` mkTcSymCo (mkTcTyConAppCo Nominal fam_tc cos)
                                      `mkTcTransCo` ctEvCoercion new_ev))

       ; let new_ct = CFunEqCan { cc_ev = new_ev, cc_fun = fam_tc, cc_tyargs = xis, cc_fsk = fsk }
       ; emitWorkCt new_ct
       ; stopWith old_ev "Fun/Top (wanted, shortcut)" }
  where
    loc = ctEvLoc old_ev
    deeper_loc = bumpCtLocDepth loc

dischargeFmv :: CtEvidence -> TcTyVar -> TcCoercion -> TcType -> TcS ()
-- (dischargeFmv x fmv co ty)
--     [W] ev :: F tys ~ fmv
--         co :: F tys ~ xi
-- Precondition: fmv is not filled, and fuv `notElem` xi
--
-- Then set fmv := xi,
--      set ev := co
--      kick out any inert things that are now rewritable
--
-- Does not evaluate 'co' if 'ev' is Derived
dischargeFmv ev fmv co xi
  = ASSERT2( not (fmv `elemVarSet` tyVarsOfType xi), ppr ev $$ ppr fmv $$ ppr xi )
    do { setEvBindIfWanted ev (EvCoercion co)
       ; unflattenFmv fmv xi
       ; n_kicked <- kickOutAfterUnification fmv
       ; traceTcS "dischargeFmv" (ppr fmv <+> equals <+> ppr xi $$ ppr_kicked n_kicked) }

{- Note [Top-level reductions for type functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c.f. Note [The flattening story] in TcFlatten

Suppose we have a CFunEqCan  F tys ~ fmv/fsk, and a matching axiom.
Here is what we do, in four cases:

* Wanteds: general firing rule
    (work item) [W]        x : F tys ~ fmv
    instantiate axiom: ax_co : F tys ~ rhs

   Then:
      Discharge   fmv := alpha
      Discharge   x := ax_co ; sym x2
      New wanted  [W] x2 : alpha ~ rhs  (Non-canonical)
   This is *the* way that fmv's get unified; even though they are
   "untouchable".

   NB: it can be the case that fmv appears in the (instantiated) rhs.
   In that case the new Non-canonical wanted will be loopy, but that's
   ok.  But it's good reason NOT to claim that it is canonical!

* Wanteds: short cut firing rule
  Applies when the RHS of the axiom is another type-function application
      (work item)        [W] x : F tys ~ fmv
      instantiate axiom: ax_co : F tys ~ G rhs_tys

  It would be a waste to create yet another fmv for (G rhs_tys).
  Instead (shortCutReduction):
      - Flatten rhs_tys (cos : rhs_tys ~ rhs_xis)
      - Add G rhs_xis ~ fmv to flat cache  (note: the same old fmv)
      - New canonical wanted   [W] x2 : G rhs_xis ~ fmv  (CFunEqCan)
      - Discharge x := ax_co ; G cos ; x2

* Givens: general firing rule
      (work item)        [G] g : F tys ~ fsk
      instantiate axiom: ax_co : F tys ~ rhs

   Now add non-canonical given (since rhs is not flat)
      [G] (sym g ; ax_co) : fsk ~ rhs  (Non-canonical)

* Givens: short cut firing rule
  Applies when the RHS of the axiom is another type-function application
      (work item)        [G] g : F tys ~ fsk
      instantiate axiom: ax_co : F tys ~ G rhs_tys

  It would be a waste to create yet another fsk for (G rhs_tys).
  Instead (shortCutReduction):
     - Flatten rhs_tys: flat_cos : tys ~ flat_tys
     - Add new Canonical given
          [G] (sym (G flat_cos) ; co ; g) : G flat_tys ~ fsk   (CFunEqCan)

Note [Cached solved FunEqs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
When trying to solve, say (FunExpensive big-type ~ ty), it's important
to see if we have reduced (FunExpensive big-type) before, lest we
simply repeat it.  Hence the lookup in inert_solved_funeqs.  Moreover
we must use `canDischarge` because both uses might (say) be Wanteds,
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
This triggers fundeps from both instance decls;
      [D] K Bool ~ K [a]
      [D] K Bool ~ K beta
And there's a risk of complaining about Bool ~ [a].  But in fact
the Wanted matches the second instance, so we never get as far
as the fundeps.

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
-}

-- | Indicates if Instance met the Safe Haskell overlapping instances safety
-- check.
--
-- See Note [Safe Haskell Overlapping Instances] in TcSimplify
-- See Note [Safe Haskell Overlapping Instances Implementation] in TcSimplify
type SafeOverlapping = Bool

data LookupInstResult
  = NoInstance
  | GenInst [TcPredType] ([EvId] -> EvTerm) SafeOverlapping

instance Outputable LookupInstResult where
  ppr NoInstance       = text "NoInstance"
  ppr (GenInst ev _ s) = text "GenInst" <+> ppr ev <+> ss
    where ss = text $ if s then "[safe]" else "[unsafe]"


matchClassInst, match_class_inst
   :: DynFlags -> InertSet -> Class -> [Type] -> CtLoc -> TcS LookupInstResult

matchClassInst dflags inerts clas tys loc
 = do { traceTcS "matchClassInst" $ vcat [ text "pred =" <+> ppr (mkClassPred clas tys) ]
      ; res <- match_class_inst dflags inerts clas tys loc
      ; traceTcS "matchClassInst result" $ ppr res
      ; return res }

-- First check whether there is an in-scope Given that could
-- match this constraint.  In that case, do not use top-level
-- instances.  See Note [Instance and Given overlap]
match_class_inst dflags inerts clas tys loc
  | not (xopt Opt_IncoherentInstances dflags)
  , let matchable_givens = matchableGivens loc pred inerts
  , not (isEmptyBag matchable_givens)
  = do { traceTcS "Delaying instance application" $
           vcat [ text "Work item=" <+> pprType pred
                , text "Potential matching givens:" <+> ppr matchable_givens ]
       ; return NoInstance }
  where
     pred = mkClassPred clas tys

match_class_inst _ _ clas [ ty ] _
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
    , let ev_tm = mkEvCast (EvLit evLit) (mkTcSymCo (mkTcTransCo co_dict co_rep))
    = return $ GenInst [] (\_ -> ev_tm) True

    | otherwise
    = panicTcS (text "Unexpected evidence for" <+> ppr (className clas)
                     $$ vcat (map (ppr . idType) (classMethods clas)))

match_class_inst _ _ clas ts _
  | isCTupleClass clas
  , let data_con = tyConSingleDataCon (classTyCon clas)
        tuple_ev = EvDFunApp (dataConWrapId data_con) ts
  = return (GenInst ts tuple_ev True)
            -- The dfun is the data constructor!

match_class_inst _ _ clas [k,t] _
  | className clas == typeableClassName
  = matchTypeableClass clas k t

match_class_inst dflags _ clas tys loc
   = do { instEnvs <- getInstEnvs
        ; let safeOverlapCheck = safeHaskell dflags `elem` [Sf_Safe, Sf_Trustworthy]
              (matches, unify, unsafeOverlaps) = lookupInstEnv True instEnvs clas tys
              safeHaskFail = safeOverlapCheck && not (null unsafeOverlaps)
        ; case (matches, unify, safeHaskFail) of

            -- Nothing matches
            ([], _, _)
                -> do { traceTcS "matchClass not matching" $
                        vcat [ text "dict" <+> ppr pred ]
                      ; return NoInstance }

            -- A single match (& no safe haskell failure)
            ([(ispec, inst_tys)], [], False)
                -> do   { let dfun_id = instanceDFunId ispec
                        ; traceTcS "matchClass success" $
                          vcat [text "dict" <+> ppr pred,
                                text "witness" <+> ppr dfun_id
                                               <+> ppr (idType dfun_id) ]
                                  -- Record that this dfun is needed
                        ; match_one (null unsafeOverlaps) dfun_id inst_tys }

            -- More than one matches (or Safe Haskell fail!). Defer any
            -- reactions of a multitude until we learn more about the reagent
            (matches, _, _)
                -> do   { traceTcS "matchClass multiple matches, deferring choice" $
                          vcat [text "dict" <+> ppr pred,
                                text "matches" <+> ppr matches]
                        ; return NoInstance } }
   where
     pred = mkClassPred clas tys

     match_one :: SafeOverlapping -> DFunId -> [DFunInstType] -> TcS LookupInstResult
                  -- See Note [DFunInstType: instantiating types] in InstEnv
     match_one so dfun_id mb_inst_tys
       = do { checkWellStagedDFun pred dfun_id loc
            ; (tys, theta) <- instDFunType dfun_id mb_inst_tys
            ; return $ GenInst theta (EvDFunApp dfun_id tys) so }


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

* The check is done *first*, so that it also covers classes
  with built-in instance solving, such as
     - constraint tuples
     - natural numbers
     - Typeable

* The given-overlap problem is arguably not easy to appear in practice
  due to our aggressive prioritization of equality solving over other
  constraints, but it is possible. I've added a test case in
  typecheck/should-compile/GivenOverlapping.hs

* Another "live" example is Trac #10195; another is #10177.

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

-- | Is the constraint for an implicit CallStack parameter?
-- i.e.   (IP "name" CallStack)
isCallStackIP :: CtLoc -> Class -> [Type] -> Maybe (EvTerm -> EvCallStack)
isCallStackIP loc cls tys
  | cls == ipClass
  , [_ip_name, ty] <- tys
  , Just (tc, _) <- splitTyConApp_maybe ty
  , tc `hasKey` callStackTyConKey
  = occOrigin (ctLocOrigin loc)
  | otherwise
  = Nothing
  where
    locSpan = ctLocSpan loc

    -- We only want to grab constraints that arose due to the use of an IP or a
    -- function call. See Note [Overview of implicit CallStacks]
    occOrigin (OccurrenceOf n) = Just (EvCsPushCall n locSpan)
    occOrigin (IPOccOrigin n)  = Just (EvCsTop ('?' `consFS` hsIPNameFS n) locSpan)
    occOrigin _                = Nothing

-- | Assumes that we've checked that this is the 'Typeable' class,
-- and it was applied to the correct argument.
matchTypeableClass :: Class -> Kind -> Type -> TcS LookupInstResult
matchTypeableClass clas k t

  -- See Note [No Typeable for qualified types]
  | isForAllTy t                               = return NoInstance

  -- Is the type of the form `C => t`?
  | isJust (tcSplitPredFunTy_maybe t)          = return NoInstance

  | eqType k typeNatKind                       = doTyLit knownNatClassName
  | eqType k typeSymbolKind                    = doTyLit knownSymbolClassName

  | Just (tc, ks) <- splitTyConApp_maybe t
  , all isKind ks                              = doTyCon tc ks

  | Just (f,kt)       <- splitAppTy_maybe t    = doTyApp f kt
  | otherwise                                  = return NoInstance

  where
  -- Representation for type constructor applied to some kinds
  doTyCon tc ks =
    case mapM kindRep ks of
      Nothing    -> return NoInstance
      Just kReps ->
        return $ GenInst [] (\_ -> EvTypeable (EvTypeableTyCon tc kReps) ) True

  {- Representation for an application of a type to a type-or-kind.
  This may happen when the type expression starts with a type variable.
  Example (ignoring kind parameter):
    Typeable (f Int Char)                      -->
    (Typeable (f Int), Typeable Char)          -->
    (Typeable f, Typeable Int, Typeable Char)  --> (after some simp. steps)
    Typeable f
  -}
  doTyApp f tk
    | isKind tk
    = return NoInstance -- We can't solve until we know the ctr.
    | otherwise
    = return $ GenInst [mk_typeable_pred f, mk_typeable_pred tk]
                       (\[t1,t2] -> EvTypeable $ EvTypeableTyApp (EvId t1,f) (EvId t2,tk))
                       True

  -- Representation for concrete kinds.  We just use the kind itself,
  -- but first check to make sure that it is "simple" (i.e., made entirely
  -- out of kind constructors).
  kindRep ki = do (_,ks) <- splitTyConApp_maybe ki
                  mapM_ kindRep ks
                  return ki

  -- Emit a `Typeable` constraint for the given type.
  mk_typeable_pred ty = mkClassPred clas [ typeKind ty, ty ]

  -- Given KnownNat / KnownSymbol, generate appropriate sub-goal
  -- and make evidence for a type-level literal.
  doTyLit c = do clas <- tcLookupClass c
                 let p = mkClassPred clas [ t ]
                 return $ GenInst [p] (\[i] -> EvTypeable
                                             $ EvTypeableTyLit (EvId i,t)) True

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

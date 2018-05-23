{-# LANGUAGE CPP #-}

module TcInteract (
     solveSimpleGivens,   -- Solves [Ct]
     solveSimpleWanteds,  -- Solves Cts
  ) where

#include "HsVersions.h"

import GhcPrelude

import BasicTypes ( SwapFlag(..), isSwapped,
                    infinity, IntWithInf, intGtLimit )
import TcCanonical
import TcFlatten
import TcUnify( canSolveByUnification )
import VarSet
import Type
import Kind( isConstraintKind )
import InstEnv( DFunInstType, lookupInstEnv
              , instanceDFunId, isOverlappable )
import CoAxiom( sfInteractTop, sfInteractInert )

import TcMType (newMetaTyVars)

import Var
import TcType
import Name
import RdrName ( lookupGRE_FieldLabel )
import PrelNames ( knownNatClassName, knownSymbolClassName,
                   typeableClassName,
                   coercibleTyConKey,
                   hasFieldClassName,
                   heqTyConKey, eqTyConKey, ipClassKey )
import TysWiredIn ( typeNatKind, typeSymbolKind, heqDataCon,
                    coercibleDataCon, constraintKindTyCon )
import TysPrim    ( eqPrimTyCon, eqReprPrimTyCon )
import Id( idType, isNaughtyRecordSelector )
import CoAxiom ( TypeEqn, CoAxiom(..), CoAxBranch(..), fromBranches )
import Class
import TyCon
import DataCon( dataConWrapId )
import FieldLabel
import FunDeps
import FamInst
import FamInstEnv
import Unify ( tcUnifyTyWithTFs )

import TcEvidence
import MkCore ( mkStringExprFS, mkNaturalExpr )
import Outputable

import TcRnTypes
import TcSMonad
import Bag
import MonadUtils ( concatMapM, foldlM )

import Data.List( partition, foldl', deleteFirstsBy )
import SrcLoc
import VarEnv

import Control.Monad
import Maybes( isJust )
import Pair (Pair(..))
import Unique( hasKey )
import DynFlags
import Util
import qualified GHC.LanguageExtensions as LangExt

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

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

solveSimpleGivens :: [Ct] -> TcS ()
solveSimpleGivens givens
  | null givens  -- Shortcut for common case
  = return ()
  | otherwise
  = do { traceTcS "solveSimpleGivens {" (ppr givens)
       ; go givens
       ; traceTcS "End solveSimpleGivens }" empty }
  where
    go givens = do { solveSimples (listToBag givens)
                   ; new_givens <- runTcPluginsGiven
                   ; when (notNull new_givens) $
                     go new_givens }

solveSimpleWanteds :: Cts -> TcS WantedConstraints
-- NB: 'simples' may contain /derived/ equalities, floated
--     out from a nested implication. So don't discard deriveds!
-- The result is not necessarily zonked
solveSimpleWanteds simples
  = do { traceTcS "solveSimpleWanteds {" (ppr simples)
       ; dflags <- getDynFlags
       ; (n,wc) <- go 1 (solverIterations dflags) (emptyWC { wc_simple = simples })
       ; traceTcS "solveSimpleWanteds end }" $
             vcat [ text "iterations =" <+> ppr n
                  , text "residual =" <+> ppr wc ]
       ; return wc }
  where
    go :: Int -> IntWithInf -> WantedConstraints -> TcS (Int, WantedConstraints)
    go n limit wc
      | n `intGtLimit` limit
      = failTcS (hang (text "solveSimpleWanteds: too many iterations"
                       <+> parens (text "limit =" <+> ppr limit))
                    2 (vcat [ text "Set limit with -fconstraint-solver-iterations=n; n=0 for no limit"
                            , text "Simples =" <+> ppr simples
                            , text "WC ="      <+> ppr wc ]))

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
            else do { traceTcS "solveSimple going round again:" $
                      ppr unif_count $$ ppr rerun_plugin
                    ; go (n+1) limit wc2 } }      -- Loop


solve_simple_wanteds :: WantedConstraints -> TcS (Int, WantedConstraints)
-- Try solving these constraints
-- Affects the unification state (of course) but not the inert set
-- The result is not necessarily zonked
solve_simple_wanteds (WC { wc_simple = simples1, wc_impl = implics1 })
  = nestTcS $
    do { solveSimples simples1
       ; (implics2, tv_eqs, fun_eqs, others) <- getUnsolvedInerts
       ; (unif_count, unflattened_eqs) <- reportUnifications $
                                          unflattenWanteds tv_eqs fun_eqs
            -- See Note [Unflatten after solving the simple wanteds]
       ; return ( unif_count
                , WC { wc_simple = others `andCts` unflattened_eqs
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
             insols                = pluginBadCts p
       ; updInertCans (removeInertCts solved_givens)
       ; updInertIrreds (\irreds -> extendCtsList irreds insols)
       ; return (pluginNewCts p) } } }

-- | Given a bag of (flattened, zonked) wanteds, invoke the plugins on
-- them and produce an updated bag of wanteds (possibly with some new
-- work) and a bag of insolubles.  The boolean indicates whether
-- 'solveSimpleWanteds' should feed the updated wanteds back into the
-- main solver.
runTcPluginsWanted :: WantedConstraints -> TcS (Bool, WantedConstraints)
runTcPluginsWanted wc@(WC { wc_simple = simples1, wc_impl = implics1 })
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
             insols                                 = pluginBadCts p

-- SLPJ: I'm deeply suspicious of this
--       ; updInertCans (removeInertCts $ solved_givens ++ solved_deriveds)

       ; mapM_ setEv solved_wanted
       ; return ( notNull (pluginNewCts p)
                , WC { wc_simple = listToBag new_wanted       `andCts`
                                   listToBag unsolved_wanted  `andCts`
                                   listToBag unsolved_derived `andCts`
                                   listToBag insols
                     , wc_impl   = implics1 } ) } }
  where
    setEv :: (EvTerm,Ct) -> TcS ()
    setEv (ev,ct) = case ctEvidence ct of
      CtWanted { ctev_dest = dest } -> setWantedEvTerm dest ev
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
    eqCt c c' = ctFlavour c == ctFlavour c'
             && ctPred c `tcEqType` ctPred c'

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
  = do { wl <- getWorkList
       ; inerts <- getTcSInerts
       ; tclevel <- getTcLevel
       ; traceTcS "----------------------------- " empty
       ; traceTcS "Start solver pipeline {" $
                  vcat [ text "tclevel =" <+> ppr tclevel
                       , text "work item =" <+> ppr workItem
                       , text "inerts =" <+> ppr inerts
                       , text "rest of worklist =" <+> ppr wl ]

       ; bumpStepCountTcS    -- One step for each constraint processed
       ; final_res  <- run_pipeline pipeline (ContinueWith workItem)

       ; case final_res of
           Stop ev s       -> do { traceFireTcS ev s
                                 ; traceTcS "End solver pipeline (discharged) }" empty
                                 ; return () }
           ContinueWith ct -> do { addInertCan ct
                                 ; traceFireTcS (ctEvidence ct) (text "Kept as inert")
                                 ; traceTcS "End solver pipeline (kept as inert) }" $
                                            (text "final_item =" <+> ppr ct) }
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
Now, c3 does not interact with the given c1, so when we spontaneously
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

interactWithInertsStage :: WorkItem -> TcS (StopOrContinue Ct)
-- Precondition: if the workitem is a CTyEqCan then it will not be able to
-- react with anything at this stage.

interactWithInertsStage wi
  = do { inerts <- getTcSInerts
       ; let ics = inert_cans inerts
       ; case wi of
             CTyEqCan  {} -> interactTyVarEq ics wi
             CFunEqCan {} -> interactFunEq   ics wi
             CIrredCan {} -> interactIrred   ics wi
             CDictCan  {} -> interactDict    ics wi
             _ -> pprPanic "interactWithInerts" (ppr wi) }
                -- CHoleCan are put straight into inert_frozen, so never get here
                -- CNonCanonical have been canonicalised

data InteractResult
   = KeepInert   -- Keep the inert item, and solve the work item from it
                 -- (if the latter is Wanted; just discard it if not)
   | KeepWork    -- Keep the work item, and solve the intert item from it

instance Outputable InteractResult where
  ppr KeepInert = text "keep inert"
  ppr KeepWork  = text "keep work-item"

solveOneFromTheOther :: CtEvidence  -- Inert
                     -> CtEvidence  -- WorkItem
                     -> TcS InteractResult
-- Precondition:
-- * inert and work item represent evidence for the /same/ predicate
--
-- We can always solve one from the other: even if both are wanted,
-- although we don't rewrite wanteds with wanteds, we can combine
-- two wanteds into one by solving one from the other

solveOneFromTheOther ev_i ev_w
  | isDerived ev_w         -- Work item is Derived; just discard it
  = return KeepInert

  | isDerived ev_i     -- The inert item is Derived, we can just throw it away,
  = return KeepWork    -- The ev_w is inert wrt earlier inert-set items,
                       -- so it's safe to continue on from this point

  | CtWanted { ctev_loc = loc_w } <- ev_w
  , prohibitedSuperClassSolve (ctEvLoc ev_i) loc_w
  = -- inert must be Given
    do { traceTcS "prohibitedClassSolve1" (ppr ev_i $$ ppr ev_w)
       ; return KeepWork }

  | CtWanted {} <- ev_w
       -- Inert is Given or Wanted
  = return KeepInert

  -- From here on the work-item is Given

  | CtWanted { ctev_loc = loc_i } <- ev_i
  , prohibitedSuperClassSolve (ctEvLoc ev_w) loc_i
  = do { traceTcS "prohibitedClassSolve2" (ppr ev_i $$ ppr ev_w)
       ; return KeepInert }      -- Just discard the un-usable Given
                                 -- This never actually happens because
                                 -- Givens get processed first

  | CtWanted {} <- ev_i
  = return KeepWork

  -- From here on both are Given
  -- See Note [Replacement vs keeping]

  | lvl_i == lvl_w
  = do { ev_binds_var <- getTcEvBindsVar
       ; binds <- getTcEvBindsMap ev_binds_var
       ; return (same_level_strategy binds) }

  | otherwise   -- Both are Given, levels differ
  = return (different_level_strategy)
  where
     pred  = ctEvPred ev_i
     loc_i = ctEvLoc ev_i
     loc_w = ctEvLoc ev_w
     lvl_i = ctLocLevel loc_i
     lvl_w = ctLocLevel loc_w
     ev_id_i = ctEvEvId ev_i
     ev_id_w = ctEvEvId ev_w

     different_level_strategy
       | isIPPred pred, lvl_w > lvl_i = KeepWork
       | lvl_w < lvl_i                = KeepWork
       | otherwise                    = KeepInert

     same_level_strategy binds        -- Both Given
       | GivenOrigin (InstSC s_i) <- ctLocOrigin loc_i
       = case ctLocOrigin loc_w of
            GivenOrigin (InstSC s_w) | s_w < s_i -> KeepWork
                                     | otherwise -> KeepInert
            _                                    -> KeepWork

       | GivenOrigin (InstSC {}) <- ctLocOrigin loc_w
       = KeepInert

       | has_binding binds ev_id_w
       , not (has_binding binds ev_id_i)
       , not (ev_id_i `elemVarSet` findNeededEvVars binds (unitVarSet ev_id_w))
       = KeepWork

       | otherwise
       = KeepInert

     has_binding binds ev_id = isJust (lookupEvBind binds ev_id)

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

  * Constraints coming from the same level (i.e. same implication)

       (a) Always get rid of InstSC ones if possible, since they are less
           useful for solving.  If both are InstSC, choose the one with
           the smallest TypeSize
           See Note [Solving superclass constraints] in TcInstDcls

       (b) Keep the one that has a non-trivial evidence binding.
              Example:  f :: (Eq a, Ord a) => blah
              then we may find [G] d3 :: Eq a
                               [G] d2 :: Eq a
                with bindings  d3 = sc_sel (d1::Ord a)
            We want to discard d2 in favour of the superclass selection from
            the Ord dictionary.
            Why? See Note [Tracking redundant constraints] in TcSimplify again.

       (c) But don't do (b) if the evidence binding depends transitively on the
           one without a binding.  Example (with RecursiveSuperClasses)
              class C a => D a
              class D a => C a
           Inert:     d1 :: C a, d2 :: D a
           Binds:     d3 = sc_sel d2, d2 = sc_sel d1
           Work item: d3 :: C a
           Then it'd be ridiculous to replace d1 with d3 in the inert set!
           Hence the findNeedEvVars test.  See Trac #14774.

  * Finally, when there is still a choice, use KeepInert rather than
    KeepWork, for two reasons:
      - to avoid unnecessary munging of the inert set.
      - to cut off superclass loops; see Note [Superclass loops] in TcCanonical

Doing the depth-check for implicit parameters, rather than making the work item
always override, is important.  Consider

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

Note [Multiple matching irreds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
You might think that it's impossible to have multiple irreds all match the
work item; after all, interactIrred looks for matches and solves one from the
other. However, note that interacting insoluble, non-droppable irreds does not
do this matching. We thus might end up with several insoluble, non-droppable,
matching irreds in the inert set. When another irred comes along that we have
not yet labeled insoluble, we can find multiple matches. These multiple matches
cause no harm, but it would be wrong to ASSERT that they aren't there (as we
once had done). This problem can be tickled by typecheck/should_compile/holes.

-}

-- Two pieces of irreducible evidence: if their types are *exactly identical*
-- we can rewrite them. We can never improve using this:
-- if we want ty1 :: Constraint and have ty2 :: Constraint it clearly does not
-- mean that (ty1 ~ ty2)
interactIrred :: InertCans -> Ct -> TcS (StopOrContinue Ct)

interactIrred inerts workItem@(CIrredCan { cc_ev = ev_w, cc_insol = insoluble })
  | insoluble  -- For insolubles, don't allow the constaint to be dropped
               -- which can happen with solveOneFromTheOther, so that
               -- we get distinct error messages with -fdefer-type-errors
               -- See Note [Do not add duplicate derived insolubles]
  , not (isDroppableCt workItem)
  = continueWith workItem

  | let (matching_irreds, others) = findMatchingIrreds (inert_irreds inerts) ev_w
  , ((ct_i, swap) : _rest) <- bagToList matching_irreds
        -- See Note [Multiple matching irreds]
  , let ev_i = ctEvidence ct_i
  = do { what_next <- solveOneFromTheOther ev_i ev_w
       ; traceTcS "iteractIrred" (ppr workItem $$ ppr what_next $$ ppr ct_i)
       ; case what_next of
            KeepInert -> do { setEvBindIfWanted ev_w (swap_me swap ev_i)
                            ; return (Stop ev_w (text "Irred equal" <+> parens (ppr what_next))) }
            KeepWork ->  do { setEvBindIfWanted ev_i (swap_me swap ev_w)
                            ; updInertIrreds (\_ -> others)
                            ; continueWith workItem } }

  | otherwise
  = continueWith workItem

  where
    swap_me :: SwapFlag -> CtEvidence -> EvExpr
    swap_me swap ev
      = case swap of
           NotSwapped -> ctEvExpr ev
           IsSwapped  -> evCoercion (mkTcSymCo (evTermCoercion (EvExpr (ctEvExpr ev))))

interactIrred _ wi = pprPanic "interactIrred" (ppr wi)

findMatchingIrreds :: Cts -> CtEvidence -> (Bag (Ct, SwapFlag), Bag Ct)
findMatchingIrreds irreds ev
  | EqPred eq_rel1 lty1 rty1 <- classifyPredType pred
    -- See Note [Solving irreducible equalities]
  = partitionBagWith (match_eq eq_rel1 lty1 rty1) irreds
  | otherwise
  = partitionBagWith match_non_eq irreds
  where
    pred = ctEvPred ev
    match_non_eq ct
      | ctPred ct `tcEqTypeNoKindCheck` pred = Left (ct, NotSwapped)
      | otherwise                            = Right ct

    match_eq eq_rel1 lty1 rty1 ct
      | EqPred eq_rel2 lty2 rty2 <- classifyPredType (ctPred ct)
      , eq_rel1 == eq_rel2
      , Just swap <- match_eq_help lty1 rty1 lty2 rty2
      = Left (ct, swap)
      | otherwise
      = Right ct

    match_eq_help lty1 rty1 lty2 rty2
      | lty1 `tcEqTypeNoKindCheck` lty2, rty1 `tcEqTypeNoKindCheck` rty2
      = Just NotSwapped
      | lty1 `tcEqTypeNoKindCheck` rty2, rty1 `tcEqTypeNoKindCheck` lty2
      = Just IsSwapped
      | otherwise
      = Nothing

{- Note [Solving irreducible equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (Trac #14333)
  [G] a b ~R# c d
  [W] c d ~R# a b
Clearly we should be able to solve this! Even though the constraints are
not decomposable. We solve this when looking up the work-item in the
irreducible constraints to look for an identical one.  When doing this
lookup, findMatchingIrreds spots the equality case, and matches either
way around. It has to return a swap-flag so we can generate evidence
that is the right way round too.

Note [Do not add duplicate derived insolubles]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general we *must* add an insoluble (Int ~ Bool) even if there is
one such there already, because they may come from distinct call
sites.  Not only do we want an error message for each, but with
-fdefer-type-errors we must generate evidence for each.  But for
*derived* insolubles, we only want to report each one once.  Why?

(a) A constraint (C r s t) where r -> s, say, may generate the same fundep
    equality many times, as the original constraint is successively rewritten.

(b) Ditto the successive iterations of the main solver itself, as it traverses
    the constraint tree. See example below.

Also for *given* insolubles we may get repeated errors, as we
repeatedly traverse the constraint tree.  These are relatively rare
anyway, so removing duplicates seems ok.  (Alternatively we could take
the SrcLoc into account.)

Note that the test does not need to be particularly efficient because
it is only used if the program has a type error anyway.

Example of (b): assume a top-level class and instance declaration:

  class D a b | a -> b
  instance D [a] [a]

Assume we have started with an implication:

  forall c. Eq c => { wc_simple = D [c] c [W] }

which we have simplified to:

  forall c. Eq c => { wc_simple = D [c] c [W]
                                  (c ~ [c]) [D] }

For some reason, e.g. because we floated an equality somewhere else,
we might try to re-solve this implication. If we do not do a
dropDerivedWC, then we will end up trying to solve the following
constraints the second time:

  (D [c] c) [W]
  (c ~ [c]) [D]

which will result in two Deriveds to end up in the insoluble set:

  wc_simple   = D [c] c [W]
               (c ~ [c]) [D], (c ~ [c]) [D]
-}

{-
*********************************************************************************
*                                                                               *
                   interactDict
*                                                                               *
*********************************************************************************

Note [Shortcut solving]
~~~~~~~~~~~~~~~~~~~~~~~
When we interact a [W] constraint with a [G] constraint that solves it, there is
a possibility that we could produce better code if instead we solved from a
top-level instance declaration (See #12791, #5835). For example:

    class M a b where m :: a -> b

    type C a b = (Num a, M a b)

    f :: C Int b => b -> Int -> Int
    f _ x = x + 1

The body of `f` requires a [W] `Num Int` instance. We could solve this
constraint from the givens because we have `C Int b` and that provides us a
solution for `Num Int`. This would let us produce core like the following
(with -O2):

    f :: forall b. C Int b => b -> Int -> Int
    f = \ (@ b) ($d(%,%) :: C Int b) _ (eta1 :: Int) ->
        + @ Int
          (GHC.Classes.$p1(%,%) @ (Num Int) @ (M Int b) $d(%,%))
          eta1
          A.f1

This is bad! We could do /much/ better if we solved [W] `Num Int` directly
from the instance that we have in scope:

    f :: forall b. C Int b => b -> Int -> Int
    f = \ (@ b) _ _ (x :: Int) ->
        case x of { GHC.Types.I# x1 -> GHC.Types.I# (GHC.Prim.+# x1 1#) }

** NB: It is important to emphasize that all this is purely an optimization:
** exactly the same programs should typecheck with or without this
** procedure.

Solving fully
~~~~~~~~~~~~~
There is a reason why the solver does not simply try to solve such
constraints with top-level instances. If the solver finds a relevant
instance declaration in scope, that instance may require a context
that can't be solved for. A good example of this is:

    f :: Ord [a] => ...
    f x = ..Need Eq [a]...

If we have instance `Eq a => Eq [a]` in scope and we tried to use it, we would
be left with the obligation to solve the constraint Eq a, which we cannot. So we
must be conservative in our attempt to use an instance declaration to solve the
[W] constraint we're interested in.

Our rule is that we try to solve all of the instance's subgoals
recursively all at once. Precisely: We only attempt to solve
constraints of the form `C1, ... Cm => C t1 ... t n`, where all the Ci
are themselves class constraints of the form `C1', ... Cm' => C' t1'
... tn'` and we only succeed if the entire tree of constraints is
solvable from instances.

An example that succeeds:

    class Eq a => C a b | b -> a where
      m :: b -> a

    f :: C [Int] b => b -> Bool
    f x = m x == []

We solve for `Eq [Int]`, which requires `Eq Int`, which we also have. This
produces the following core:

    f :: forall b. C [Int] b => b -> Bool
    f = \ (@ b) ($dC :: C [Int] b) (x :: b) ->
        GHC.Classes.$fEq[]_$s$c==
          (m @ [Int] @ b $dC x) (GHC.Types.[] @ Int)

An example that fails:

    class Eq a => C a b | b -> a where
      m :: b -> a

    f :: C [a] b => b -> Bool
    f x = m x == []

Which, because solving `Eq [a]` demands `Eq a` which we cannot solve, produces:

    f :: forall a b. C [a] b => b -> Bool
    f = \ (@ a) (@ b) ($dC :: C [a] b) (eta :: b) ->
        ==
          @ [a]
          (A.$p1C @ [a] @ b $dC)
          (m @ [a] @ b $dC eta)
          (GHC.Types.[] @ a)

Note [Shortcut solving: type families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have (Trac #13943)
  class Take (n :: Nat) where ...
  instance {-# OVERLAPPING #-}                    Take 0 where ..
  instance {-# OVERLAPPABLE #-} (Take (n - 1)) => Take n where ..

And we have [W] Take 3.  That only matches one instance so we get
[W] Take (3-1).  Really we should now flatten to reduce the (3-1) to 2, and
so on -- but that is reproducing yet more of the solver.  Sigh.  For now,
we just give up (remember all this is just an optimisation).

But we must not just naively try to lookup (Take (3-1)) in the
InstEnv, or it'll (wrongly) appear not to match (Take 0) and get a
unique match on the (Take n) instance.  That leads immediately to an
infinite loop.  Hence the check that 'preds' have no type families
(isTyFamFree).

Note [Shortcut solving: overlap]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
  instance {-# OVERLAPPABLE #-} C a where ...
and we are typechecking
  f :: C a => a -> a
  f = e  -- Gives rise to [W] C a

We don't want to solve the wanted constraint with the overlappable
instance; rather we want to use the supplied (C a)! That was the whole
point of it being overlappable!  Trac #14434 wwas an example.

Note [Shortcut solving: incoherence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This optimization relies on coherence of dictionaries to be correct. When we
cannot assume coherence because of IncoherentInstances then this optimization
can change the behavior of the user's code.

The following four modules produce a program whose output would change depending
on whether we apply this optimization when IncoherentInstances is in effect:

#########
    {-# LANGUAGE MultiParamTypeClasses #-}
    module A where

    class A a where
      int :: a -> Int

    class A a => C a b where
      m :: b -> a -> a

#########
    {-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
    module B where

    import A

    instance A a where
      int _ = 1

    instance C a [b] where
      m _ = id

#########
    {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
    {-# LANGUAGE IncoherentInstances #-}
    module C where

    import A

    instance A Int where
      int _ = 2

    instance C Int [Int] where
      m _ = id

    intC :: C Int a => a -> Int -> Int
    intC _ x = int x

#########
    module Main where

    import A
    import B
    import C

    main :: IO ()
    main = print (intC [] (0::Int))

The output of `main` if we avoid the optimization under the effect of
IncoherentInstances is `1`. If we were to do the optimization, the output of
`main` would be `2`.

Note [Shortcut try_solve_from_instance]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The workhorse of the short-cut solver is
    try_solve_from_instance :: CtLoc
                            -> (EvBindMap, DictMap CtEvidence)
                            -> CtEvidence       -- Solve this
                            -> MaybeT TcS (EvBindMap, DictMap CtEvidence)
Note that:

* The CtEvidence is teh goal to be solved

* The MaybeT anages early failure if we find a subgoal that
  cannot be solved from instances.

* The (EvBindMap, DictMap CtEvidence) is an accumulating purely-functional
  state that allows try_solve_from_instance to augmennt the evidence
  bindings and inert_solved_dicts as it goes.

  If it succeeds, we commit all these bindings and solved dicts to the
  main TcS InertSet.  If not, we abandon it all entirely.

Passing along the solved_dicts important for two reasons:

* We need to be able to handle recursive super classes. The
  solved_dicts state  ensures that we remember what we have already
  tried to solve to avoid looping.

* As Trac #15164 showed, it can be important to exploit sharing between
  goals. E.g. To solve G we may need G1 and G2. To solve G1 we may need H;
  and to solve G2 we may need H. If we don't spot this sharing we may
  solve H twice; and if this pattern repeats we may get exponentially bad
  behaviour.
-}

interactDict :: InertCans -> Ct -> TcS (StopOrContinue Ct)
interactDict inerts workItem@(CDictCan { cc_ev = ev_w, cc_class = cls, cc_tyargs = tys })
  | Just ev_i <- lookupInertDict inerts (ctEvLoc ev_w) cls tys
  = -- There is a matching dictionary in the inert set
    do { -- First to try to solve it /completely/ from top level instances
         -- See Note [Shortcut solving]
         dflags <- getDynFlags
       ; short_cut_worked <- shortCutSolver dflags ev_w ev_i
       ; if short_cut_worked
         then stopWith ev_w "interactDict/solved from instance"
         else

    do { -- We were unable to solve the [W] constraint from in-scope
         -- instances so we solve it from the matching inert we found
         what_next <- solveOneFromTheOther ev_i ev_w
       ; traceTcS "lookupInertDict" (ppr what_next)
       ; case what_next of
           KeepInert -> do { setEvBindIfWanted ev_w (ctEvExpr ev_i)
                           ; return $ Stop ev_w (text "Dict equal" <+> parens (ppr what_next)) }
           KeepWork  -> do { setEvBindIfWanted ev_i (ctEvExpr ev_w)
                           ; updInertDicts $ \ ds -> delDict ds cls tys
                           ; continueWith workItem } } }

  | cls `hasKey` ipClassKey
  , isGiven ev_w
  = interactGivenIP inerts workItem

  | otherwise
  = do { addFunDepWork inerts ev_w cls
       ; continueWith workItem  }

interactDict _ wi = pprPanic "interactDict" (ppr wi)

-- See Note [Shortcut solving]
shortCutSolver :: DynFlags
               -> CtEvidence -- Work item
               -> CtEvidence -- Inert we want to try to replace
               -> TcS Bool   -- True <=> success
shortCutSolver dflags ev_w ev_i
  | isWanted ev_w
 && isGiven ev_i
 -- We are about to solve a [W] constraint from a [G] constraint. We take
 -- a moment to see if we can get a better solution using an instance.
 -- Note that we only do this for the sake of performance. Exactly the same
 -- programs should typecheck regardless of whether we take this step or
 -- not. See Note [Shortcut solving]

 && not (xopt LangExt.IncoherentInstances dflags)
 -- If IncoherentInstances is on then we cannot rely on coherence of proofs
 -- in order to justify this optimization: The proof provided by the
 -- [G] constraint's superclass may be different from the top-level proof.
 -- See Note [Shortcut solving: incoherence]

 && gopt Opt_SolveConstantDicts dflags
 -- Enabled by the -fsolve-constant-dicts flag
  = do { ev_binds_var <- getTcEvBindsVar
       ; ev_binds <- ASSERT2( not (isNoEvBindsVar ev_binds_var ), ppr ev_w )
                     getTcEvBindsMap ev_binds_var
       ; solved_dicts <- getSolvedDicts

       ; mb_stuff <- runMaybeT $ try_solve_from_instance loc_w
                                   (ev_binds, solved_dicts) ev_w

       ; case mb_stuff of
           Nothing -> return False
           Just (ev_binds', solved_dicts')
              -> do { setTcEvBindsMap ev_binds_var ev_binds'
                    ; setSolvedDicts solved_dicts'
                    ; return True } }

  | otherwise
  = return False
  where
    -- This `CtLoc` is used only to check the well-staged condition of any
    -- candidate DFun. Our subgoals all have the same stage as our root
    -- [W] constraint so it is safe to use this while solving them.
    loc_w = ctEvLoc ev_w

    try_solve_from_instance   -- See Note [Shortcut try_solve_from_instance]
      :: CtLoc -> (EvBindMap, DictMap CtEvidence) -> CtEvidence
      -> MaybeT TcS (EvBindMap, DictMap CtEvidence)
    try_solve_from_instance loc (ev_binds, solved_dicts) ev
      | let pred = ctEvPred ev
      , ClassPred cls tys <- classifyPredType pred
      = do { inst_res <- lift $ match_class_inst dflags True cls tys loc_w
           ; case inst_res of
               GenInst { lir_new_theta = preds
                       , lir_mk_ev = mk_ev
                       , lir_safe_over = safeOverlap }
                 | safeOverlap
                 , all isTyFamFree preds  -- Note [Shortcut solving: type families]
                 -> do { let solved_dicts' = addDict solved_dicts cls tys ev
                             loc'          = bumpCtLocDepth loc
                             -- solved_dicts': it is important that we add our goal
                             -- to the cache before we solve! Otherwise we may end
                             -- up in a loop while solving recursive dictionaries.

                       ; lift $ traceTcS "shortCutSolver: found instance" (ppr preds)
                       ; lift $ checkReductionDepth loc' pred

                       ; evc_vs <- mapM (new_wanted_cached solved_dicts') preds
                                  -- Emit work for subgoals but use our local cache
                                  -- so we can solve recursive dictionaries.

                       ; let ev_tm     = mk_ev (map getEvExpr evc_vs)
                             ev_binds' = extendEvBinds ev_binds $
                                         mkWantedEvBind (ctEvEvId ev) ev_tm

                       ; foldlM (try_solve_from_instance loc')
                                (ev_binds', solved_dicts')
                                (freshGoals evc_vs) }

               _ -> mzero }
      | otherwise = mzero


    -- Use a local cache of solved dicts while emitting EvVars for new work
    -- We bail out of the entire computation if we need to emit an EvVar for
    -- a subgoal that isn't a ClassPred.
    new_wanted_cached :: DictMap CtEvidence -> TcPredType -> MaybeT TcS MaybeNew
    new_wanted_cached cache pty
      | ClassPred cls tys <- classifyPredType pty
      = lift $ case findDict cache loc_w cls tys of
          Just ctev -> return $ Cached (ctEvExpr ctev)
          Nothing -> Fresh <$> newWantedNC loc_w pty
      | otherwise = mzero

addFunDepWork :: InertCans -> CtEvidence -> Class -> TcS ()
-- Add derived constraints from type-class functional dependencies.
addFunDepWork inerts work_ev cls
  | isImprovable work_ev
  = mapBagM_ add_fds (findDictsByClass (inert_dicts inerts) cls)
               -- No need to check flavour; fundeps work between
               -- any pair of constraints, regardless of flavour
               -- Importantly we don't throw workitem back in the
               -- worklist because this can cause loops (see #5236)
  | otherwise
  = return ()
  where
    work_pred = ctEvPred work_ev
    work_loc  = ctEvLoc work_ev

    add_fds inert_ct
      | isImprovable inert_ev
      = do { traceTcS "addFunDepWork" (vcat
                [ ppr work_ev
                , pprCtLoc work_loc, ppr (isGivenLoc work_loc)
                , pprCtLoc inert_loc, ppr (isGivenLoc inert_loc)
                , pprCtLoc derived_loc, ppr (isGivenLoc derived_loc) ]) ;

        emitFunDepDeriveds $
        improveFromAnother derived_loc inert_pred work_pred
               -- We don't really rewrite tys2, see below _rewritten_tys2, so that's ok
               -- NB: We do create FDs for given to report insoluble equations that arise
               -- from pairs of Givens, and also because of floating when we approximate
               -- implications. The relevant test is: typecheck/should_fail/FDsFromGivens.hs
        }
      | otherwise
      = return ()
      where
        inert_ev   = ctEvidence inert_ct
        inert_pred = ctEvPred inert_ev
        inert_loc  = ctEvLoc inert_ev
        derived_loc = work_loc { ctl_depth  = ctl_depth work_loc `maxSubGoalDepth`
                                              ctl_depth inert_loc
                               , ctl_origin = FunDepOrigin1 work_pred  work_loc
                                                            inert_pred inert_loc }

{-
**********************************************************************
*                                                                    *
                   Implicit parameters
*                                                                    *
**********************************************************************
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

{- Note [Shadowing of Implicit Parameters]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

Similarly, consider
   f :: (?x::a) => Bool -> a

   g v = let ?x::Int = 3
         in (f v, let ?x::Bool = True in f v)

This should probably be well typed, with
   g :: Bool -> (Int, Bool)

So the inner binding for ?x::Bool *overrides* the outer one.

All this works for the normal cases but it has an odd side effect in
some pathological programs like this:
-- This is accepted, the second parameter shadows
f1 :: (?x :: Int, ?x :: Char) => Char
f1 = ?x

-- This is rejected, the second parameter shadows
f2 :: (?x :: Int, ?x :: Char) => Int
f2 = ?x

Both of these are actually wrong:  when we try to use either one,
we'll get two incompatible wanted constraints (?x :: Int, ?x :: Char),
which would lead to an error.

I can think of two ways to fix this:

  1. Simply disallow multiple constraints for the same implicit
    parameter---this is never useful, and it can be detected completely
    syntactically.

  2. Move the shadowing machinery to the location where we nest
     implications, and add some code here that will produce an
     error if we get multiple givens for the same implicit parameter.


**********************************************************************
*                                                                    *
                   interactFunEq
*                                                                    *
**********************************************************************
-}

interactFunEq :: InertCans -> Ct -> TcS (StopOrContinue Ct)
-- Try interacting the work item with the inert set
interactFunEq inerts work_item@(CFunEqCan { cc_ev = ev, cc_fun = tc
                                          , cc_tyargs = args, cc_fsk = fsk })
  | Just inert_ct@(CFunEqCan { cc_ev = ev_i
                             , cc_fsk = fsk_i })
         <- findFunEq (inert_funeqs inerts) tc args
  , pr@(swap_flag, upgrade_flag) <- ev_i `funEqCanDischarge` ev
  = do { traceTcS "reactFunEq (rewrite inert item):" $
         vcat [ text "work_item =" <+> ppr work_item
              , text "inertItem=" <+> ppr ev_i
              , text "(swap_flag, upgrade)" <+> ppr pr ]
       ; if isSwapped swap_flag
         then do {   -- Rewrite inert using work-item
                   let work_item' | upgrade_flag = upgradeWanted work_item
                                  | otherwise    = work_item
                 ; updInertFunEqs $ \ feqs -> insertFunEq feqs tc args work_item'
                      -- Do the updInertFunEqs before the reactFunEq, so that
                      -- we don't kick out the inertItem as well as consuming it!
                 ; reactFunEq ev fsk ev_i fsk_i
                 ; stopWith ev "Work item rewrites inert" }
         else do {   -- Rewrite work-item using inert
                 ; when upgrade_flag $
                   updInertFunEqs $ \ feqs -> insertFunEq feqs tc args
                                                 (upgradeWanted inert_ct)
                 ; reactFunEq ev_i fsk_i ev fsk
                 ; stopWith ev "Inert rewrites work item" } }

  | otherwise   -- Try improvement
  = do { improveLocalFunEqs ev inerts tc args fsk
       ; continueWith work_item }

interactFunEq _ work_item = pprPanic "interactFunEq" (ppr work_item)

upgradeWanted :: Ct -> Ct
-- We are combining a [W] F tys ~ fmv1 and [D] F tys ~ fmv2
-- so upgrade the [W] to [WD] before putting it in the inert set
upgradeWanted ct = ct { cc_ev = upgrade_ev (cc_ev ct) }
  where
    upgrade_ev ev = ASSERT2( isWanted ev, ppr ct )
                    ev { ctev_nosh = WDeriv }

improveLocalFunEqs :: CtEvidence -> InertCans -> TyCon -> [TcType] -> TcTyVar
                   -> TcS ()
-- Generate derived improvement equalities, by comparing
-- the current work item with inert CFunEqs
-- E.g.   x + y ~ z,   x + y' ~ z   =>   [D] y ~ y'
--
-- See Note [FunDep and implicit parameter reactions]
improveLocalFunEqs work_ev inerts fam_tc args fsk
  | isGiven work_ev -- See Note [No FunEq improvement for Givens]
    || not (isImprovable work_ev)
  = return ()

  | not (null improvement_eqns)
  = do { traceTcS "interactFunEq improvements: " $
         vcat [ text "Eqns:" <+> ppr improvement_eqns
              , text "Candidates:" <+> ppr funeqs_for_tc
              , text "Inert eqs:" <+> ppr ieqs ]
       ; emitFunDepDeriveds improvement_eqns }

  | otherwise
  = return ()

  where
    ieqs          = inert_eqs inerts
    funeqs        = inert_funeqs inerts
    funeqs_for_tc = findFunEqsByTyCon funeqs fam_tc
    rhs           = lookupFlattenTyVar ieqs fsk
    work_loc      = ctEvLoc work_ev
    work_pred     = ctEvPred work_ev
    fam_inj_info  = tyConInjectivityInfo fam_tc

    --------------------
    improvement_eqns :: [FunDepEqn CtLoc]
    improvement_eqns
      | Just ops <- isBuiltInSynFamTyCon_maybe fam_tc
      =    -- Try built-in families, notably for arithmethic
         concatMap (do_one_built_in ops) funeqs_for_tc

      | Injective injective_args <- fam_inj_info
      =    -- Try improvement from type families with injectivity annotations
        concatMap (do_one_injective injective_args) funeqs_for_tc

      | otherwise
      = []

    --------------------
    do_one_built_in ops (CFunEqCan { cc_tyargs = iargs, cc_fsk = ifsk, cc_ev = inert_ev })
      = mk_fd_eqns inert_ev (sfInteractInert ops args rhs iargs
                                             (lookupFlattenTyVar ieqs ifsk))

    do_one_built_in _ _ = pprPanic "interactFunEq 1" (ppr fam_tc)

    --------------------
    -- See Note [Type inference for type families with injectivity]
    do_one_injective inj_args (CFunEqCan { cc_tyargs = inert_args
                                         , cc_fsk = ifsk, cc_ev = inert_ev })
      | isImprovable inert_ev
      , rhs `tcEqType` lookupFlattenTyVar ieqs ifsk
      = mk_fd_eqns inert_ev $
            [ Pair arg iarg
            | (arg, iarg, True) <- zip3 args inert_args inj_args ]
      | otherwise
      = []

    do_one_injective _ _ = pprPanic "interactFunEq 2" (ppr fam_tc)

    --------------------
    mk_fd_eqns :: CtEvidence -> [TypeEqn] -> [FunDepEqn CtLoc]
    mk_fd_eqns inert_ev eqns
      | null eqns  = []
      | otherwise  = [ FDEqn { fd_qtvs = [], fd_eqs = eqns
                             , fd_pred1 = work_pred
                             , fd_pred2 = ctEvPred inert_ev
                             , fd_loc   = loc } ]
      where
        inert_loc = ctEvLoc inert_ev
        loc = inert_loc { ctl_depth = ctl_depth inert_loc `maxSubGoalDepth`
                                      ctl_depth work_loc }

-------------
reactFunEq :: CtEvidence -> TcTyVar    -- From this  :: F args1 ~ fsk1
           -> CtEvidence -> TcTyVar    -- Solve this :: F args2 ~ fsk2
           -> TcS ()
reactFunEq from_this fsk1 solve_this fsk2
  = do { traceTcS "reactFunEq"
            (vcat [ppr from_this, ppr fsk1, ppr solve_this, ppr fsk2])
       ; dischargeFunEq solve_this fsk2 (ctEvCoercion from_this) (mkTyVarTy fsk1)
       ; traceTcS "reactFunEq done" (ppr from_this $$ ppr fsk1 $$
                                     ppr solve_this $$ ppr fsk2) }

{- Note [Type inference for type families with injectivity]
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
    (i)   Will set g2 := g1 `cast` g3
    (ii)  Will add to our solved cache that [S] g2 : F a ~ beta2
    (iii) Will emit [W] g3 : beta1 ~ beta2
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


**********************************************************************
*                                                                    *
                   interactTyVarEq
*                                                                    *
**********************************************************************
-}

inertsCanDischarge :: InertCans -> TcTyVar -> TcType -> CtFlavourRole
                   -> Maybe ( CtEvidence  -- The evidence for the inert
                            , SwapFlag    -- Whether we need mkSymCo
                            , Bool)       -- True <=> keep a [D] version
                                          --          of the [WD] constraint
inertsCanDischarge inerts tv rhs fr
  | (ev_i : _) <- [ ev_i | CTyEqCan { cc_ev = ev_i, cc_rhs = rhs_i
                                    , cc_eq_rel = eq_rel }
                             <- findTyEqs inerts tv
                         , (ctEvFlavour ev_i, eq_rel) `eqCanDischargeFR` fr
                         , rhs_i `tcEqType` rhs ]
  =  -- Inert:     a ~ ty
     -- Work item: a ~ ty
    Just (ev_i, NotSwapped, keep_deriv ev_i)

  | Just tv_rhs <- getTyVar_maybe rhs
  , (ev_i : _) <- [ ev_i | CTyEqCan { cc_ev = ev_i, cc_rhs = rhs_i
                                    , cc_eq_rel = eq_rel }
                             <- findTyEqs inerts tv_rhs
                         , (ctEvFlavour ev_i, eq_rel) `eqCanDischargeFR` fr
                         , rhs_i `tcEqType` mkTyVarTy tv ]
  =  -- Inert:     a ~ b
     -- Work item: b ~ a
     Just (ev_i, IsSwapped, keep_deriv ev_i)

  | otherwise
  = Nothing

  where
    keep_deriv ev_i
      | Wanted WOnly  <- ctEvFlavour ev_i  -- inert is [W]
      , (Wanted WDeriv, _) <- fr           -- work item is [WD]
      = True   -- Keep a derived verison of the work item
      | otherwise
      = False  -- Work item is fully discharged

interactTyVarEq :: InertCans -> Ct -> TcS (StopOrContinue Ct)
-- CTyEqCans are always consumed, so always returns Stop
interactTyVarEq inerts workItem@(CTyEqCan { cc_tyvar = tv
                                          , cc_rhs = rhs
                                          , cc_ev = ev
                                          , cc_eq_rel = eq_rel })
  | Just (ev_i, swapped, keep_deriv)
       <- inertsCanDischarge inerts tv rhs (ctEvFlavour ev, eq_rel)
  = do { setEvBindIfWanted ev $
         evCoercion (maybeSym swapped $
                     tcDowngradeRole (eqRelRole eq_rel)
                                     (ctEvRole ev_i)
                                     (ctEvCoercion ev_i))

       ; let deriv_ev = CtDerived { ctev_pred = ctEvPred ev
                                  , ctev_loc  = ctEvLoc  ev }
       ; when keep_deriv $
         emitWork [workItem { cc_ev = deriv_ev }]
         -- As a Derived it might not be fully rewritten,
         -- so we emit it as new work

       ; stopWith ev "Solved from inert" }

  | ReprEq <- eq_rel   -- See Note [Do not unify representational equalities]
  = unsolved_inert

  | isGiven ev         -- See Note [Touchables and givens]
  = unsolved_inert

  | otherwise
  = do { tclvl <- getTcLevel
       ; if canSolveByUnification tclvl tv rhs
         then do { solveByUnification ev tv rhs
                 ; n_kicked <- kickOutAfterUnification tv
                 ; return (Stop ev (text "Solved by unification" <+> pprKicked n_kicked)) }

         else unsolved_inert }

  where
    unsolved_inert
      = do { traceTcS "Can't solve tyvar equality"
                (vcat [ text "LHS:" <+> ppr tv <+> dcolon <+> ppr (tyVarKind tv)
                      , ppWhen (isMetaTyVar tv) $
                        nest 4 (text "TcLevel of" <+> ppr tv
                                <+> text "is" <+> ppr (metaTyVarTcLevel tv))
                      , text "RHS:" <+> ppr rhs <+> dcolon <+> ppr (typeKind rhs) ])
           ; addInertEq workItem
           ; stopWith ev "Kept as inert" }

interactTyVarEq _ wi = pprPanic "interactTyVarEq" (ppr wi)

solveByUnification :: CtEvidence -> TcTyVar -> Xi -> TcS ()
-- Solve with the identity coercion
-- Precondition: kind(xi) equals kind(tv)
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
                       vcat [text "Unifies:" <+> ppr tv <+> text ":=" <+> ppr xi,
                             text "Coercion:" <+> pprEq tv_ty xi,
                             text "Left Kind is:" <+> ppr (typeKind tv_ty),
                             text "Right Kind is:" <+> ppr (typeKind xi) ]

       ; unifyTyVar tv xi
       ; setEvBindIfWanted wd (evCoercion (mkTcNomReflCo xi)) }

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

Note [Do not unify representational equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider   [W] alpha ~R# b
where alpha is touchable. Should we unify alpha := b?

Certainly not!  Unifying forces alpha and be to be the same; but they
only need to be representationally equal types.

For example, we might have another constraint [W] alpha ~# N b
where
  newtype N b = MkN b
and we want to get alpha := N b.

See also Trac #15144, which was caused by unifying a representational
equality (in the unflattener).


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

Note [FunDep and implicit parameter reactions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Currently, our story of interacting two dictionaries (or a dictionary
and top-level instances) for functional dependencies, and implicit
parameters, is that we simply produce new Derived equalities.  So for example

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
-}

emitFunDepDeriveds :: [FunDepEqn CtLoc] -> TcS ()
-- See Note [FunDep and implicit parameter reactions]
emitFunDepDeriveds fd_eqns
  = mapM_ do_one_FDEqn fd_eqns
  where
    do_one_FDEqn (FDEqn { fd_qtvs = tvs, fd_eqs = eqs, fd_loc = loc })
     | null tvs  -- Common shortcut
     = do { traceTcS "emitFunDepDeriveds 1" (ppr (ctl_depth loc) $$ ppr eqs $$ ppr (isGivenLoc loc))
          ; mapM_ (unifyDerived loc Nominal) eqs }
     | otherwise
     = do { traceTcS "emitFunDepDeriveds 2" (ppr (ctl_depth loc) $$ ppr eqs)
          ; subst <- instFlexi tvs  -- Takes account of kind substitution
          ; mapM_ (do_one_eq loc subst) eqs }

    do_one_eq loc subst (Pair ty1 ty2)
       = unifyDerived loc Nominal $
         Pair (Type.substTyUnchecked subst ty1) (Type.substTyUnchecked subst ty2)

{-
**********************************************************************
*                                                                    *
                       The top-reaction Stage
*                                                                    *
**********************************************************************
-}

topReactionsStage :: WorkItem -> TcS (StopOrContinue Ct)
topReactionsStage wi
 = do { tir <- doTopReact wi
      ; case tir of
          ContinueWith wi -> continueWith wi
          Stop ev s       -> return (Stop ev (text "Top react:" <+> s)) }

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
doTopReactFunEq :: Ct -> TcS (StopOrContinue Ct)
doTopReactFunEq work_item@(CFunEqCan { cc_ev = old_ev, cc_fun = fam_tc
                                     , cc_tyargs = args, cc_fsk = fsk })

  | fsk `elemVarSet` tyCoVarsOfTypes args
  = no_reduction    -- See Note [FunEq occurs-check principle]

  | otherwise  -- Note [Reduction for Derived CFunEqCans]
  = do { match_res <- matchFam fam_tc args
                           -- Look up in top-level instances, or built-in axiom
                           -- See Note [MATCHING-SYNONYMS]
       ; case match_res of
           Nothing         -> no_reduction
           Just match_info -> reduce_top_fun_eq old_ev fsk match_info }
  where
    no_reduction
      = do { improveTopFunEqs old_ev fam_tc args fsk
           ; continueWith work_item }

doTopReactFunEq w = pprPanic "doTopReactFunEq" (ppr w)

reduce_top_fun_eq :: CtEvidence -> TcTyVar -> (TcCoercion, TcType)
                  -> TcS (StopOrContinue Ct)
-- We have found an applicable top-level axiom: use it to reduce
-- Precondition: fsk is not free in rhs_ty
reduce_top_fun_eq old_ev fsk (ax_co, rhs_ty)
  | not (isDerived old_ev)  -- Precondition of shortCutReduction
  , Just (tc, tc_args) <- tcSplitTyConApp_maybe rhs_ty
  , isTypeFamilyTyCon tc
  , tc_args `lengthIs` tyConArity tc    -- Short-cut
  = -- RHS is another type-family application
    -- Try shortcut; see Note [Top-level reductions for type functions]
    do { shortCutReduction old_ev fsk ax_co tc tc_args
       ; stopWith old_ev "Fun/Top (shortcut)" }

  | otherwise
  = ASSERT2( not (fsk `elemVarSet` tyCoVarsOfType rhs_ty)
           , ppr old_ev $$ ppr rhs_ty )
           -- Guaranteed by Note [FunEq occurs-check principle]
    do { dischargeFunEq old_ev fsk ax_co rhs_ty
       ; traceTcS "doTopReactFunEq" $
         vcat [ text "old_ev:" <+> ppr old_ev
              , nest 2 (text ":=") <+> ppr ax_co ]
       ; stopWith old_ev "Fun/Top" }

improveTopFunEqs :: CtEvidence -> TyCon -> [TcType] -> TcTyVar -> TcS ()
-- See Note [FunDep and implicit parameter reactions]
improveTopFunEqs ev fam_tc args fsk
  | isGiven ev            -- See Note [No FunEq improvement for Givens]
    || not (isImprovable ev)
  = return ()

  | otherwise
  = do { ieqs <- getInertEqs
       ; fam_envs <- getFamInstEnvs
       ; eqns <- improve_top_fun_eqs fam_envs fam_tc args
                                    (lookupFlattenTyVar ieqs fsk)
       ; traceTcS "improveTopFunEqs" (vcat [ ppr fam_tc <+> ppr args <+> ppr fsk
                                          , ppr eqns ])
       ; mapM_ (unifyDerived loc Nominal) eqns }
  where
    loc = ctEvLoc ev  -- ToDo: this location is wrong; it should be FunDepOrigin2
                      -- See Trac #14778

improve_top_fun_eqs :: FamInstEnvs
                    -> TyCon -> [TcType] -> TcType
                    -> TcS [TypeEqn]
improve_top_fun_eqs fam_envs fam_tc args rhs_ty
  | Just ops <- isBuiltInSynFamTyCon_maybe fam_tc
  = return (sfInteractTop ops args rhs_ty)

  -- see Note [Type inference for type families with injectivity]
  | isOpenTypeFamilyTyCon fam_tc
  , Injective injective_args <- tyConInjectivityInfo fam_tc
  , let fam_insts = lookupFamInstEnvByTyCon fam_envs fam_tc
  = -- it is possible to have several compatible equations in an open type
    -- family but we only want to derive equalities from one such equation.
    do { let improvs = buildImprovementData fam_insts
                           fi_tvs fi_tys fi_rhs (const Nothing)

       ; traceTcS "improve_top_fun_eqs2" (ppr improvs)
       ; concatMapM (injImproveEqns injective_args) $
         take 1 improvs }

  | Just ax <- isClosedSynFamilyTyConWithAxiom_maybe fam_tc
  , Injective injective_args <- tyConInjectivityInfo fam_tc
  = concatMapM (injImproveEqns injective_args) $
    buildImprovementData (fromBranches (co_ax_branches ax))
                         cab_tvs cab_lhs cab_rhs Just

  | otherwise
  = return []

  where
      buildImprovementData
          :: [a]                     -- axioms for a TF (FamInst or CoAxBranch)
          -> (a -> [TyVar])          -- get bound tyvars of an axiom
          -> (a -> [Type])           -- get LHS of an axiom
          -> (a -> Type)             -- get RHS of an axiom
          -> (a -> Maybe CoAxBranch) -- Just => apartness check required
          -> [( [Type], TCvSubst, [TyVar], Maybe CoAxBranch )]
             -- Result:
             -- ( [arguments of a matching axiom]
             -- , RHS-unifying substitution
             -- , axiom variables without substitution
             -- , Maybe matching axiom [Nothing - open TF, Just - closed TF ] )
      buildImprovementData axioms axiomTVs axiomLHS axiomRHS wrap =
          [ (ax_args, subst, unsubstTvs, wrap axiom)
          | axiom <- axioms
          , let ax_args = axiomLHS axiom
                ax_rhs  = axiomRHS axiom
                ax_tvs  = axiomTVs axiom
          , Just subst <- [tcUnifyTyWithTFs False ax_rhs rhs_ty]
          , let notInSubst tv = not (tv `elemVarEnv` getTvSubstEnv subst)
                unsubstTvs    = filter (notInSubst <&&> isTyVar) ax_tvs ]
                   -- The order of unsubstTvs is important; it must be
                   -- in telescope order e.g. (k:*) (a:k)

      injImproveEqns :: [Bool]
                     -> ([Type], TCvSubst, [TyCoVar], Maybe CoAxBranch)
                     -> TcS [TypeEqn]
      injImproveEqns inj_args (ax_args, subst, unsubstTvs, cabr)
        = do { subst <- instFlexiX subst unsubstTvs
                  -- If the current substitution bind [k -> *], and
                  -- one of the un-substituted tyvars is (a::k), we'd better
                  -- be sure to apply the current substitution to a's kind.
                  -- Hence instFlexiX.   Trac #13135 was an example.

             ; return [ Pair (substTyUnchecked subst ax_arg) arg
                        -- NB: the ax_arg part is on the left
                        -- see Note [Improvement orientation]
                      | case cabr of
                          Just cabr' -> apartnessCheck (substTys subst ax_args) cabr'
                          _          -> True
                      , (ax_arg, arg, True) <- zip3 ax_args args inj_args ] }


shortCutReduction :: CtEvidence -> TcTyVar -> TcCoercion
                  -> TyCon -> [TcType] -> TcS ()
-- See Note [Top-level reductions for type functions]
-- Previously, we flattened the tc_args here, but there's no need to do so.
-- And, if we did, this function would have all the complication of
-- TcCanonical.canCFunEqCan. See Note [canCFunEqCan]
shortCutReduction old_ev fsk ax_co fam_tc tc_args
  = ASSERT( ctEvEqRel old_ev == NomEq)
               -- ax_co :: F args ~ G tc_args
               -- old_ev :: F args ~ fsk
    do { new_ev <- case ctEvFlavour old_ev of
           Given -> newGivenEvVar deeper_loc
                         ( mkPrimEqPred (mkTyConApp fam_tc tc_args) (mkTyVarTy fsk)
                         , evCoercion (mkTcSymCo ax_co
                                       `mkTcTransCo` ctEvCoercion old_ev) )

           Wanted {} ->
             do { (new_ev, new_co) <- newWantedEq deeper_loc Nominal
                                        (mkTyConApp fam_tc tc_args) (mkTyVarTy fsk)
                ; setWantedEq (ctev_dest old_ev) $ ax_co `mkTcTransCo` new_co
                ; return new_ev }

           Derived -> pprPanic "shortCutReduction" (ppr old_ev)

       ; let new_ct = CFunEqCan { cc_ev = new_ev, cc_fun = fam_tc
                                , cc_tyargs = tc_args, cc_fsk = fsk }
       ; updWorkListTcS (extendWorkListFunEq new_ct) }
  where
    deeper_loc = bumpCtLocDepth (ctEvLoc old_ev)

{- Note [Top-level reductions for type functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c.f. Note [The flattening story] in TcFlatten

Suppose we have a CFunEqCan  F tys ~ fmv/fsk, and a matching axiom.
Here is what we do, in four cases:

* Wanteds: general firing rule
    (work item) [W]        x : F tys ~ fmv
    instantiate axiom: ax_co : F tys ~ rhs

   Then:
      Discharge   fmv := rhs
      Discharge   x := ax_co ; sym x2
   This is *the* way that fmv's get unified; even though they are
   "untouchable".

   NB: Given Note [FunEq occurs-check principle], fmv does not appear
   in tys, and hence does not appear in the instantiated RHS.  So
   the unification can't make an infinite type.

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

Note [FunEq occurs-check principle]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
I have spent a lot of time finding a good way to deal with
CFunEqCan constraints like
    F (fuv, a) ~ fuv
where flatten-skolem occurs on the LHS.  Now in principle we
might may progress by doing a reduction, but in practice its
hard to find examples where it is useful, and easy to find examples
where we fall into an infinite reduction loop.  A rule that works
very well is this:

  *** FunEq occurs-check principle ***

      Do not reduce a CFunEqCan
          F tys ~ fsk
      if fsk appears free in tys
      Instead we treat it as stuck.

Examples:

* Trac #5837 has [G] a ~ TF (a,Int), with an instance
    type instance TF (a,b) = (TF a, TF b)
  This readily loops when solving givens.  But with the FunEq occurs
  check principle, it rapidly gets stuck which is fine.

* Trac #12444 is a good example, explained in comment:2.  We have
    type instance F (Succ x) = Succ (F x)
    [W] alpha ~ Succ (F alpha)
  If we allow the reduction to happen, we get an infinite loop

Note [Cached solved FunEqs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
When trying to solve, say (FunExpensive big-type ~ ty), it's important
to see if we have reduced (FunExpensive big-type) before, lest we
simply repeat it.  Hence the lookup in inert_solved_funeqs.  Moreover
we must use `funEqCanDischarge` because both uses might (say) be Wanteds,
and we *still* want to save the re-computation.

Note [MATCHING-SYNONYMS]
~~~~~~~~~~~~~~~~~~~~~~~~
When trying to match a dictionary (D tau) to a top-level instance, or a
type family equation (F taus_1 ~ tau_2) to a top-level family instance,
we do *not* need to expand type synonyms because the matcher will do that for us.

Note [Improvement orientation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A very delicate point is the orientation of derived equalities
arising from injectivity improvement (Trac #12522).  Suppse we have
  type family F x = t | t -> x
  type instance F (a, Int) = (Int, G a)
where G is injective; and wanted constraints

  [W] TF (alpha, beta) ~ fuv
  [W] fuv ~ (Int, <some type>)

The injectivity will give rise to derived constraints

  [D] gamma1 ~ alpha
  [D] Int ~ beta

The fresh unification variable gamma1 comes from the fact that we
can only do "partial improvement" here; see Section 5.2 of
"Injective type families for Haskell" (HS'15).

Now, it's very important to orient the equations this way round,
so that the fresh unification variable will be eliminated in
favour of alpha.  If we instead had
   [D] alpha ~ gamma1
then we would unify alpha := gamma1; and kick out the wanted
constraint.  But when we grough it back in, it'd look like
   [W] TF (gamma1, beta) ~ fuv
and exactly the same thing would happen again!  Infinite loop.

This all seems fragile, and it might seem more robust to avoid
introducing gamma1 in the first place, in the case where the
actual argument (alpha, beta) partly matches the improvement
template.  But that's a bit tricky, esp when we remember that the
kinds much match too; so it's easier to let the normal machinery
handle it.  Instead we are careful to orient the new derived
equality with the template on the left.  Delicate, but it works.

Note [No FunEq improvement for Givens]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We don't do improvements (injectivity etc) for Givens. Why?

* It generates Derived constraints on skolems, which don't do us
  much good, except perhaps identify inaccessible branches.
  (They'd be perfectly valid though.)

* For type-nat stuff the derived constraints include type families;
  e.g.  (a < b), (b < c) ==> a < c If we generate a Derived for this,
  we'll generate a Derived/Wanted CFunEqCan; and, since the same
  InertCans (after solving Givens) are used for each iteration, that
  massively confused the unflattening step (TcFlatten.unflatten).

  In fact it led to some infinite loops:
     indexed-types/should_compile/T10806
     indexed-types/should_compile/T10507
     polykinds/T10742

Note [Reduction for Derived CFunEqCans]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
You may wonder if it's important to use top-level instances to
simplify [D] CFunEqCan's.  But it is.  Here's an example (T10226).

   type instance F    Int = Int
   type instance FInv Int = Int

Suppose we have to solve
    [WD] FInv (F alpha) ~ alpha
    [WD] F alpha ~ Int

  --> flatten
    [WD] F alpha ~ fuv0
    [WD] FInv fuv0 ~ fuv1  -- (A)
    [WD] fuv1 ~ alpha
    [WD] fuv0 ~ Int        -- (B)

  --> Rewwrite (A) with (B), splitting it
    [WD] F alpha ~ fuv0
    [W] FInv fuv0 ~ fuv1
    [D] FInv Int ~ fuv1    -- (C)
    [WD] fuv1 ~ alpha
    [WD] fuv0 ~ Int

  --> Reduce (C) with top-level instance
      **** This is the key step ***
    [WD] F alpha ~ fuv0
    [W] FInv fuv0 ~ fuv1
    [D] fuv1 ~ Int        -- (D)
    [WD] fuv1 ~ alpha     -- (E)
    [WD] fuv0 ~ Int

  --> Rewrite (D) with (E)
    [WD] F alpha ~ fuv0
    [W] FInv fuv0 ~ fuv1
    [D] alpha ~ Int       -- (F)
    [WD] fuv1 ~ alpha
    [WD] fuv0 ~ Int

  --> unify (F)  alpha := Int, and that solves it

Another example is indexed-types/should_compile/T10634
-}

{- *******************************************************************
*                                                                    *
         Top-level reaction for class constraints (CDictCan)
*                                                                    *
**********************************************************************-}

doTopReactDict :: InertSet -> Ct -> TcS (StopOrContinue Ct)
-- Try to use type-class instance declarations to simplify the constraint
doTopReactDict inerts work_item@(CDictCan { cc_ev = fl, cc_class = cls
                                          , cc_tyargs = xis })
  | isGiven fl   -- Never use instances for Given constraints
  = do { try_fundep_improvement
       ; continueWith work_item }

  | Just ev <- lookupSolvedDict inerts dict_loc cls xis   -- Cached
  = do { setEvBindIfWanted fl (ctEvExpr ev)
       ; stopWith fl "Dict/Top (cached)" }

  | otherwise  -- Wanted or Derived, but not cached
   = do { dflags <- getDynFlags
        ; lkup_inst_res <- matchClassInst dflags inerts cls xis dict_loc
        ; case lkup_inst_res of
               GenInst { lir_new_theta = theta
                       , lir_mk_ev     = mk_ev
                       , lir_safe_over = s } ->
                 do { traceTcS "doTopReact/found instance for" $ ppr fl
                    ; checkReductionDepth deeper_loc dict_pred
                    ; unless s $ insertSafeOverlapFailureTcS work_item
                    ; if isDerived fl then finish_derived theta
                                      else finish_wanted  theta mk_ev }
               NoInstance ->
                 do { when (isImprovable fl) $
                      try_fundep_improvement
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

     finish_wanted :: [TcPredType]
                   -> ([EvExpr] -> EvTerm) -> TcS (StopOrContinue Ct)
      -- Precondition: evidence term matches the predicate workItem
     finish_wanted theta mk_ev
        = do { addSolvedDict fl cls xis
             ; evc_vars <- mapM (newWanted deeper_loc) theta
             ; setWantedEvBind (ctEvEvId fl) (mk_ev (map getEvExpr evc_vars))
             ; emitWorkNC (freshGoals evc_vars)
             ; stopWith fl "Dict/Top (solved wanted)" }

     finish_derived theta  -- Use type-class instances for Deriveds, in the hope
       =                   -- of generating some improvements
                           -- C.f. Example 3 of Note [The improvement story]
                           -- It's easy because no evidence is involved
         do { emitNewDeriveds deeper_loc theta
            ; traceTcS "finish_derived" (ppr (ctl_depth deeper_loc))
            ; stopWith fl "Dict/Top (solved derived)" }

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


{- *******************************************************************
*                                                                    *
                       Class lookup
*                                                                    *
**********************************************************************-}

-- | Indicates if Instance met the Safe Haskell overlapping instances safety
-- check.
--
-- See Note [Safe Haskell Overlapping Instances] in TcSimplify
-- See Note [Safe Haskell Overlapping Instances Implementation] in TcSimplify
type SafeOverlapping = Bool

data LookupInstResult
  = NoInstance
  | GenInst { lir_new_theta :: [TcPredType]
            , lir_mk_ev     :: [EvExpr] -> EvTerm
            , lir_safe_over :: SafeOverlapping }

instance Outputable LookupInstResult where
  ppr NoInstance = text "NoInstance"
  ppr (GenInst { lir_new_theta = ev
               , lir_safe_over = s })
    = text "GenInst" <+> vcat [ppr ev, ss]
    where ss = text $ if s then "[safe]" else "[unsafe]"


matchClassInst :: DynFlags -> InertSet -> Class -> [Type] -> CtLoc -> TcS LookupInstResult
matchClassInst dflags inerts clas tys loc
-- First check whether there is an in-scope Given that could
-- match this constraint.  In that case, do not use top-level
-- instances.  See Note [Instance and Given overlap]
  | not (xopt LangExt.IncoherentInstances dflags)
  , not (naturallyCoherentClass clas)
  , let matchable_givens = matchableGivens loc pred inerts
  , not (isEmptyBag matchable_givens)
  = do { traceTcS "Delaying instance application" $
           vcat [ text "Work item=" <+> pprClassPred clas tys
                , text "Potential matching givens:" <+> ppr matchable_givens ]
       ; return NoInstance }
  where
     pred = mkClassPred clas tys

matchClassInst dflags _ clas tys loc
 = do { traceTcS "matchClassInst" $ text "pred =" <+> ppr (mkClassPred clas tys) <+> char '{'
      ; res <- match_class_inst dflags False clas tys loc
      ; traceTcS "} matchClassInst result" $ ppr res
      ; return res }

match_class_inst :: DynFlags
                 -> Bool      -- True <=> caller is the short-cut solver
                              -- See Note [Shortcut solving: overlap]
                 -> Class -> [Type] -> CtLoc -> TcS LookupInstResult
match_class_inst dflags short_cut clas tys loc
  | cls_name == knownNatClassName     = matchKnownNat        clas tys
  | cls_name == knownSymbolClassName  = matchKnownSymbol     clas tys
  | isCTupleClass clas                = matchCTuple          clas tys
  | cls_name == typeableClassName     = matchTypeable        clas tys
  | clas `hasKey` heqTyConKey         = matchLiftedEquality       tys
  | clas `hasKey` coercibleTyConKey   = matchLiftedCoercible      tys
  | cls_name == hasFieldClassName     = matchHasField dflags short_cut clas tys loc
  | otherwise                         = matchInstEnv dflags short_cut clas tys loc
  where
    cls_name = className clas

-- | If a class is "naturally coherent", then we needn't worry at all, in any
-- way, about overlapping/incoherent instances. Just solve the thing!
-- See Note [Naturally coherent classes]
-- See also Note [The equality class story] in TysPrim.
naturallyCoherentClass :: Class -> Bool
naturallyCoherentClass cls
  = isCTupleClass cls
    || cls `hasKey` heqTyConKey
    || cls `hasKey` eqTyConKey
    || cls `hasKey` coercibleTyConKey

{- Note [Instance and Given overlap]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Example, from the OutsideIn(X) paper:
       instance P x => Q [x]
       instance (x ~ y) => R y [x]

       wob :: forall a b. (Q [b], R b a) => a -> Int

       g :: forall a. Q [a] => [a] -> Int
       g x = wob x

From 'g' we get the impliation constraint:
            forall a. Q [a] => (Q [beta], R beta [a])
If we react (Q [beta]) with its top-level axiom, we end up with a
(P beta), which we have no way of discharging. On the other hand,
if we react R beta [a] with the top-level we get  (beta ~ a), which
is solvable and can help us rewrite (Q [beta]) to (Q [a]) which is
now solvable by the given Q [a].

The partial solution is that:
  In matchClassInst (and thus in topReact), we return a matching
  instance only when there is no Given in the inerts which is
  unifiable to this particular dictionary.

  We treat any meta-tyvar as "unifiable" for this purpose,
  *including* untouchable ones.  But not skolems like 'a' in
  the implication constraint above.

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

* Flatten-skolems: we do not treat a flatten-skolem as unifiable
  for this purpose.
  E.g.   f :: Eq (F a) => [a] -> [a]
         f xs = ....(xs==xs).....
  Here we get [W] Eq [a], and we don't want to refrain from solving
  it because of the given (Eq (F a)) constraint!

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

* The solution is only a partial one.  Consider the above example with
       g :: forall a. Q [a] => [a] -> Int
       g x = let v = wob x
             in v
  and suppose we have -XNoMonoLocalBinds, so that we attempt to find the most
  general type for 'v'.  When generalising v's type we'll simplify its
  Q [alpha] constraint, but we don't have Q [a] in the 'givens', so we
  will use the instance declaration after all. Trac #11948 was a case
  in point.

All of this is disgustingly delicate, so to discourage people from writing
simplifiable class givens, we warn about signatures that contain them;
see TcValidity Note [Simplifiable given constraints].

Note [Naturally coherent classes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A few built-in classes are "naturally coherent".  This term means that
the "instance" for the class is bidirectional with its superclass(es).
For example, consider (~~), which behaves as if it was defined like
this:
  class a ~# b => a ~~ b
  instance a ~# b => a ~~ b
(See Note [The equality types story] in TysPrim.)

Faced with [W] t1 ~~ t2, it's always OK to reduce it to [W] t1 ~# t2,
without worrying about Note [Instance and Given overlap].  Why?  Because
if we had [G] s1 ~~ s2, then we'd get the superclass [G] s1 ~# s2, and
so the reduction of the [W] constraint does not risk losing any solutions.

On the other hand, it can be fatal to /fail/ to reduce such
equalities, on the grounds of Note [Instance and Given overlap],
because many good things flow from [W] t1 ~# t2.

The same reasoning applies to

* (~~)        heqTyCOn
* (~)         eqTyCon
* Coercible   coercibleTyCon

And less obviously to:

* Tuple classes.  For reasons described in TcSMonad
  Note [Tuples hiding implicit parameters], we may have a constraint
     [W] (?x::Int, C a)
  with an exactly-matching Given constraint.  We must decompose this
  tuple and solve the components separately, otherwise we won't solve
  it at all!  It is perfectly safe to decompose it, because again the
  superclasses invert the instance;  e.g.
      class (c1, c2) => (% c1, c2 %)
      instance (c1, c2) => (% c1, c2 %)
  Example in Trac #14218

Exammples: T5853, T10432, T5315, T9222, T2627b, T3028b

PS: the term "naturally coherent" doesn't really seem helpful.
Perhaps "invertible" or something?  I left it for now though.
-}


{- *******************************************************************
*                                                                    *
                Class lookup in the instance environment
*                                                                    *
**********************************************************************-}

matchInstEnv :: DynFlags -> Bool -> Class -> [Type] -> CtLoc -> TcS LookupInstResult
matchInstEnv dflags short_cut_solver clas tys loc
   = do { instEnvs <- getInstEnvs
        ; let safeOverlapCheck = safeHaskell dflags `elem` [Sf_Safe, Sf_Trustworthy]
              (matches, unify, unsafeOverlaps) = lookupInstEnv True instEnvs clas tys
              safeHaskFail = safeOverlapCheck && not (null unsafeOverlaps)
        ; case (matches, unify, safeHaskFail) of

            -- Nothing matches
            ([], _, _)
                -> do { traceTcS "matchClass not matching" (ppr pred)
                      ; return NoInstance }

            -- A single match (& no safe haskell failure)
            ([(ispec, inst_tys)], [], False)
                | short_cut_solver
                , isOverlappable ispec
                -- If the instance has OVERLAPPABLE or OVERLAPS or INCOHERENT
                -- then don't let the short-cut solver choose it, because a
                -- later instance might overlap it.  Trac #14434 is an example
                -- See Note [Shortcut solving: overlap]
                -> do { traceTcS "matchClass: ingnoring overlappable" (ppr pred)
                      ; return NoInstance }

                | otherwise
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
            ; return $ GenInst { lir_new_theta = theta
                               , lir_mk_ev     = EvExpr . evDFunApp dfun_id tys
                               , lir_safe_over = so } }


{- ********************************************************************
*                                                                     *
                   Class lookup for CTuples
*                                                                     *
***********************************************************************-}

matchCTuple :: Class -> [Type] -> TcS LookupInstResult
matchCTuple clas tys   -- (isCTupleClass clas) holds
  = return (GenInst { lir_new_theta = tys
                    , lir_mk_ev     = tuple_ev
                    , lir_safe_over = True })
            -- The dfun *is* the data constructor!
  where
     data_con = tyConSingleDataCon (classTyCon clas)
     tuple_ev = EvExpr . evDFunApp (dataConWrapId data_con) tys

{- ********************************************************************
*                                                                     *
                   Class lookup for Literals
*                                                                     *
***********************************************************************-}

{-
Note [KnownNat & KnownSymbol and EvLit]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A part of the type-level literals implementation are the classes
"KnownNat" and "KnownSymbol", which provide a "smart" constructor for
defining singleton values.  Here is the key stuff from GHC.TypeLits

  class KnownNat (n :: Nat) where
    natSing :: SNat n

  newtype SNat (n :: Nat) = SNat Integer

Conceptually, this class has infinitely many instances:

  instance KnownNat 0       where natSing = SNat 0
  instance KnownNat 1       where natSing = SNat 1
  instance KnownNat 2       where natSing = SNat 2
  ...

In practice, we solve `KnownNat` predicates in the type-checker
(see typecheck/TcInteract.hs) because we can't have infinitely many instances.
The evidence (aka "dictionary") for `KnownNat` is of the form `EvLit (EvNum n)`.

We make the following assumptions about dictionaries in GHC:
  1. The "dictionary" for classes with a single method---like `KnownNat`---is
     a newtype for the type of the method, so using a evidence amounts
     to a coercion, and
  2. Newtypes use the same representation as their definition types.

So, the evidence for `KnownNat` is just a value of the representation type,
wrapped in two newtype constructors: one to make it into a `SNat` value,
and another to make it into a `KnownNat` dictionary.

Also note that `natSing` and `SNat` are never actually exposed from the
library---they are just an implementation detail.  Instead, users see
a more convenient function, defined in terms of `natSing`:

  natVal :: KnownNat n => proxy n -> Integer

The reason we don't use this directly in the class is that it is simpler
and more efficient to pass around an integer rather than an entire function,
especially when the `KnowNat` evidence is packaged up in an existential.

The story for kind `Symbol` is analogous:
  * class KnownSymbol
  * newtype SSymbol
  * Evidence: a Core literal (e.g. mkNaturalExpr)
-}

matchKnownNat :: Class -> [Type] -> TcS LookupInstResult
matchKnownNat clas [ty]     -- clas = KnownNat
  | Just n <- isNumLitTy ty = do
        et <- mkNaturalExpr n
        makeLitDict clas ty et
matchKnownNat _ _           = return NoInstance

matchKnownSymbol :: Class -> [Type] -> TcS LookupInstResult
matchKnownSymbol clas [ty]  -- clas = KnownSymbol
  | Just s <- isStrLitTy ty = do
        et <- mkStringExprFS s
        makeLitDict clas ty et
matchKnownSymbol _ _       = return NoInstance

makeLitDict :: Class -> Type -> EvExpr -> TcS LookupInstResult
-- makeLitDict adds a coercion that will convert the literal into a dictionary
-- of the appropriate type.  See Note [KnownNat & KnownSymbol and EvLit]
-- in TcEvidence.  The coercion happens in 2 steps:
--
--     Integer -> SNat n     -- representation of literal to singleton
--     SNat n  -> KnownNat n -- singleton to dictionary
--
--     The process is mirrored for Symbols:
--     String    -> SSymbol n
--     SSymbol n -> KnownSymbol n
makeLitDict clas ty et
    | Just (_, co_dict) <- tcInstNewTyCon_maybe (classTyCon clas) [ty]
          -- co_dict :: KnownNat n ~ SNat n
    , [ meth ]   <- classMethods clas
    , Just tcRep <- tyConAppTyCon_maybe -- SNat
                      $ funResultTy         -- SNat n
                      $ dropForAlls         -- KnownNat n => SNat n
                      $ idType meth         -- forall n. KnownNat n => SNat n
    , Just (_, co_rep) <- tcInstNewTyCon_maybe tcRep [ty]
          -- SNat n ~ Integer
    , let ev_tm = EvExpr $ mkEvCast et (mkTcSymCo (mkTcTransCo co_dict co_rep))
    = return $ GenInst { lir_new_theta = []
                       , lir_mk_ev     = \_ -> ev_tm
                       , lir_safe_over = True }

    | otherwise
    = panicTcS (text "Unexpected evidence for" <+> ppr (className clas)
                     $$ vcat (map (ppr . idType) (classMethods clas)))

{- ********************************************************************
*                                                                     *
                   Class lookup for Typeable
*                                                                     *
***********************************************************************-}

-- | Assumes that we've checked that this is the 'Typeable' class,
-- and it was applied to the correct argument.
matchTypeable :: Class -> [Type] -> TcS LookupInstResult
matchTypeable clas [k,t]  -- clas = Typeable
  -- For the first two cases, See Note [No Typeable for polytypes or qualified types]
  | isForAllTy k                      = return NoInstance   -- Polytype
  | isJust (tcSplitPredFunTy_maybe t) = return NoInstance   -- Qualified type

  -- Now cases that do work
  | k `eqType` typeNatKind                 = doTyLit knownNatClassName         t
  | k `eqType` typeSymbolKind              = doTyLit knownSymbolClassName      t
  | isConstraintKind t                     = doTyConApp clas t constraintKindTyCon []
  | Just (arg,ret) <- splitFunTy_maybe t   = doFunTy    clas t arg ret
  | Just (tc, ks) <- splitTyConApp_maybe t -- See Note [Typeable (T a b c)]
  , onlyNamedBndrsApplied tc ks            = doTyConApp clas t tc ks
  | Just (f,kt)   <- splitAppTy_maybe t    = doTyApp    clas t f kt

matchTypeable _ _ = return NoInstance

-- | Representation for a type @ty@ of the form @arg -> ret@.
doFunTy :: Class -> Type -> Type -> Type -> TcS LookupInstResult
doFunTy clas ty arg_ty ret_ty
  = do { let preds = map (mk_typeable_pred clas) [arg_ty, ret_ty]
             build_ev [arg_ev, ret_ev] =
                 evTypeable ty $ EvTypeableTrFun (EvExpr arg_ev) (EvExpr ret_ev)
             build_ev _ = panic "TcInteract.doFunTy"
       ; return $ GenInst preds build_ev True
       }

-- | Representation for type constructor applied to some kinds.
-- 'onlyNamedBndrsApplied' has ensured that this application results in a type
-- of monomorphic kind (e.g. all kind variables have been instantiated).
doTyConApp :: Class -> Type -> TyCon -> [Kind] -> TcS LookupInstResult
doTyConApp clas ty tc kind_args
  | Just _ <- tyConRepName_maybe tc
  = return $ GenInst (map (mk_typeable_pred clas) kind_args)
                     (\kinds -> evTypeable ty $ EvTypeableTyCon tc (map EvExpr kinds))
                     True
  | otherwise
  = return NoInstance

-- | Representation for TyCon applications of a concrete kind. We just use the
-- kind itself, but first we must make sure that we've instantiated all kind-
-- polymorphism, but no more.
onlyNamedBndrsApplied :: TyCon -> [KindOrType] -> Bool
onlyNamedBndrsApplied tc ks
 = all isNamedTyConBinder used_bndrs &&
   not (any isNamedTyConBinder leftover_bndrs)
 where
   bndrs                        = tyConBinders tc
   (used_bndrs, leftover_bndrs) = splitAtList ks bndrs

doTyApp :: Class -> Type -> Type -> KindOrType -> TcS LookupInstResult
-- Representation for an application of a type to a type-or-kind.
--  This may happen when the type expression starts with a type variable.
--  Example (ignoring kind parameter):
--    Typeable (f Int Char)                      -->
--    (Typeable (f Int), Typeable Char)          -->
--    (Typeable f, Typeable Int, Typeable Char)  --> (after some simp. steps)
--    Typeable f
doTyApp clas ty f tk
  | isForAllTy (typeKind f)
  = return NoInstance -- We can't solve until we know the ctr.
  | otherwise
  = return $ GenInst (map (mk_typeable_pred clas) [f, tk])
                     (\[t1,t2] -> evTypeable ty $ EvTypeableTyApp (EvExpr t1) (EvExpr t2))
                     True

-- Emit a `Typeable` constraint for the given type.
mk_typeable_pred :: Class -> Type -> PredType
mk_typeable_pred clas ty = mkClassPred clas [ typeKind ty, ty ]

  -- Typeable is implied by KnownNat/KnownSymbol. In the case of a type literal
  -- we generate a sub-goal for the appropriate class. See #10348 for what
  -- happens when we fail to do this.
doTyLit :: Name -> Type -> TcS LookupInstResult
doTyLit kc t = do { kc_clas <- tcLookupClass kc
                  ; let kc_pred    = mkClassPred kc_clas [ t ]
                        mk_ev [ev] = evTypeable t $ EvTypeableTyLit (EvExpr ev)
                        mk_ev _    = panic "doTyLit"
                  ; return (GenInst [kc_pred] mk_ev True) }

{- Note [Typeable (T a b c)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For type applications we always decompose using binary application,
via doTyApp, until we get to a *kind* instantiation.  Example
   Proxy :: forall k. k -> *

To solve Typeable (Proxy (* -> *) Maybe) we
  - First decompose with doTyApp,
    to get (Typeable (Proxy (* -> *))) and Typeable Maybe
  - Then solve (Typeable (Proxy (* -> *))) with doTyConApp

If we attempt to short-cut by solving it all at once, via
doTyConApp

(this note is sadly truncated FIXME)


Note [No Typeable for polytypes or qualified types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

{- ********************************************************************
*                                                                     *
                   Class lookup for lifted equality
*                                                                     *
***********************************************************************-}

-- See also Note [The equality types story] in TysPrim
matchLiftedEquality :: [Type] -> TcS LookupInstResult
matchLiftedEquality args
  = return (GenInst { lir_new_theta = [ mkTyConApp eqPrimTyCon args ]
                    , lir_mk_ev     = EvExpr . evDFunApp (dataConWrapId heqDataCon) args
                    , lir_safe_over = True })

-- See also Note [The equality types story] in TysPrim
matchLiftedCoercible :: [Type] -> TcS LookupInstResult
matchLiftedCoercible args@[k, t1, t2]
  = return (GenInst { lir_new_theta = [ mkTyConApp eqReprPrimTyCon args' ]
                    , lir_mk_ev     = EvExpr . evDFunApp (dataConWrapId coercibleDataCon)
                                                args
                    , lir_safe_over = True })
  where
    args' = [k, k, t1, t2]
matchLiftedCoercible args = pprPanic "matchLiftedCoercible" (ppr args)


{- ********************************************************************
*                                                                     *
              Class lookup for overloaded record fields
*                                                                     *
***********************************************************************-}

{-
Note [HasField instances]
~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have

    data T y = MkT { foo :: [y] }

and `foo` is in scope.  Then GHC will automatically solve a constraint like

    HasField "foo" (T Int) b

by emitting a new wanted

    T alpha -> [alpha] ~# T Int -> b

and building a HasField dictionary out of the selector function `foo`,
appropriately cast.

The HasField class is defined (in GHC.Records) thus:

    class HasField (x :: k) r a | x r -> a where
      getField :: r -> a

Since this is a one-method class, it is represented as a newtype.
Hence we can solve `HasField "foo" (T Int) b` by taking an expression
of type `T Int -> b` and casting it using the newtype coercion.
Note that

    foo :: forall y . T y -> [y]

so the expression we construct is

    foo @alpha |> co

where

    co :: (T alpha -> [alpha]) ~# HasField "foo" (T Int) b

is built from

    co1 :: (T alpha -> [alpha]) ~# (T Int -> b)

which is the new wanted, and

    co2 :: (T Int -> b) ~# HasField "foo" (T Int) b

which can be derived from the newtype coercion.

If `foo` is not in scope, or has a higher-rank or existentially
quantified type, then the constraint is not solved automatically, but
may be solved by a user-supplied HasField instance.  Similarly, if we
encounter a HasField constraint where the field is not a literal
string, or does not belong to the type, then we fall back on the
normal constraint solver behaviour.
-}

-- See Note [HasField instances]
matchHasField :: DynFlags -> Bool -> Class -> [Type] -> CtLoc -> TcS LookupInstResult
matchHasField dflags short_cut clas tys loc
  = do { fam_inst_envs <- getFamInstEnvs
       ; rdr_env       <- getGlobalRdrEnvTcS
       ; case tys of
           -- We are matching HasField {k} x r a...
           [_k_ty, x_ty, r_ty, a_ty]
               -- x should be a literal string
             | Just x <- isStrLitTy x_ty
               -- r should be an applied type constructor
             , Just (tc, args) <- tcSplitTyConApp_maybe r_ty
               -- use representation tycon (if data family); it has the fields
             , let r_tc = fstOf3 (tcLookupDataFamInst fam_inst_envs tc args)
               -- x should be a field of r
             , Just fl <- lookupTyConFieldLabel x r_tc
               -- the field selector should be in scope
             , Just gre <- lookupGRE_FieldLabel rdr_env fl

             -> do { sel_id <- tcLookupId (flSelector fl)
                   ; (tv_prs, preds, sel_ty) <- tcInstType newMetaTyVars sel_id

                         -- The first new wanted constraint equates the actual
                         -- type of the selector with the type (r -> a) within
                         -- the HasField x r a dictionary.  The preds will
                         -- typically be empty, but if the datatype has a
                         -- "stupid theta" then we have to include it here.
                   ; let theta = mkPrimEqPred sel_ty (mkFunTy r_ty a_ty) : preds

                         -- Use the equality proof to cast the selector Id to
                         -- type (r -> a), then use the newtype coercion to cast
                         -- it to a HasField dictionary.
                         mk_ev (ev1:evs) = EvExpr $ evSelector sel_id tvs evs `evCast` co
                           where
                             co = mkTcSubCo (evTermCoercion (EvExpr ev1))
                                      `mkTcTransCo` mkTcSymCo co2
                         mk_ev [] = panic "matchHasField.mk_ev"

                         Just (_, co2) = tcInstNewTyCon_maybe (classTyCon clas)
                                                              tys

                         tvs = mkTyVarTys (map snd tv_prs)

                     -- The selector must not be "naughty" (i.e. the field
                     -- cannot have an existentially quantified type), and
                     -- it must not be higher-rank.
                   ; if not (isNaughtyRecordSelector sel_id) && isTauTy sel_ty
                     then do { addUsedGRE True gre
                             ; return GenInst { lir_new_theta = theta
                                              , lir_mk_ev     = mk_ev
                                              , lir_safe_over = True
                                              } }
                     else matchInstEnv dflags short_cut clas tys loc }

           _ -> matchInstEnv dflags short_cut clas tys loc }

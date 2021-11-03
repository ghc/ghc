{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}

module GHC.Tc.Solver.Interact (
     solveSimpleGivens,   -- Solves [Ct]
     solveSimpleWanteds,  -- Solves Cts
  ) where

#include "HsVersions.h"

import GHC.Prelude
import GHC.Types.Basic ( SwapFlag(..),
                         infinity, IntWithInf, intGtLimit )
import GHC.Tc.Solver.Canonical
import GHC.Types.Var.Set
import GHC.Core.Type as Type
import GHC.Core.InstEnv         ( DFunInstType )

import GHC.Types.Var
import GHC.Tc.Utils.TcType
import GHC.Builtin.Names ( coercibleTyConKey, heqTyConKey, eqTyConKey, ipClassKey )
import GHC.Core.Coercion.Axiom ( CoAxBranch (..), CoAxiom (..), TypeEqn, fromBranches, sfInteractInert, sfInteractTop )
import GHC.Core.Class
import GHC.Core.TyCon
import GHC.Tc.Instance.FunDeps
import GHC.Tc.Instance.Family
import GHC.Tc.Instance.Class ( InstanceWhat(..), safeOverlap )
import GHC.Core.FamInstEnv
import GHC.Core.Unify ( tcUnifyTyWithTFs, ruleMatchTyKiX )

import GHC.Tc.Types.Evidence
import GHC.Utils.Outputable
import GHC.Utils.Panic

import GHC.Tc.Types
import GHC.Tc.Types.Constraint
import GHC.Core.Predicate
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.TcMType( promoteMetaTyVarTo )
import GHC.Tc.Solver.Monad
import GHC.Data.Bag
import GHC.Utils.Monad ( concatMapM, foldlM )

import GHC.Core
import Data.List( partition, deleteFirstsBy )
import GHC.Types.SrcLoc
import GHC.Types.Var.Env

import Control.Monad
import GHC.Data.Pair (Pair(..))
import GHC.Types.Unique( hasKey )
import GHC.Driver.Session
import GHC.Utils.Misc
import qualified GHC.LanguageExtensions as LangExt
import Data.List.NonEmpty ( NonEmpty(..) )

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
      - top-level interactions
   Each stage returns a StopOrContinue and may have sideffected
   the inerts or worklist.

   The threading of the stages is as follows:
      - If (Stop) is returned by a stage then we start again from Step 1.
      - If (ContinueWith ct) is returned by a stage, we feed 'ct' on to
        the next stage in the pipeline.
4. If the element has survived (i.e. ContinueWith x) the last stage
   then we add it in the inerts and jump back to Step 1.

If in Step 1 no such element exists, we have exceeded our context-stack
depth and will simply fail.
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
            wc1 <- solve_simple_wanteds wc

            -- Run plugins
          ; (rerun_plugin, wc2) <- runTcPluginsWanted wc1

          ; if rerun_plugin
            then do { traceTcS "solveSimple going round again:" (ppr rerun_plugin)
                    ; go (n+1) limit wc2 }   -- Loop
            else return (n, wc2) }           -- Done


solve_simple_wanteds :: WantedConstraints -> TcS WantedConstraints
-- Try solving these constraints
-- Affects the unification state (of course) but not the inert set
-- The result is not necessarily zonked
solve_simple_wanteds (WC { wc_simple = simples1, wc_impl = implics1, wc_holes = holes })
  = nestTcS $
    do { solveSimples simples1
       ; (implics2, unsolved) <- getUnsolvedInerts
       ; return (WC { wc_simple = unsolved
                    , wc_impl   = implics1 `unionBags` implics2
                    , wc_holes  = holes }) }

{- Note [The solveSimpleWanteds loop]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Solving a bunch of simple constraints is done in a loop,
(the 'go' loop of 'solveSimpleWanteds'):
  1. Try to solve them
  2. Try the plugin
  3. If the plugin wants to run again, go back to step 1
-}

-- The main solver loop implements Note [Basic Simplifier Plan]
---------------------------------------------------------------
solveSimples :: Cts -> TcS ()
-- Returns the final InertSet in TcS
-- Has no effect on work-list or residual-implications
-- The constraints are initially examined in left-to-right order

solveSimples cts
  = {-# SCC "solveSimples" #-}
    do { updWorkListTcS (\wl -> foldr extendWorkListCt wl cts)
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

-- | Given a bag of (rewritten, zonked) wanteds, invoke the plugins on
-- them and produce an updated bag of wanteds (possibly with some new
-- work) and a bag of insolubles.  The boolean indicates whether
-- 'solveSimpleWanteds' should feed the updated wanteds back into the
-- main solver.
runTcPluginsWanted :: WantedConstraints -> TcS (Bool, WantedConstraints)
runTcPluginsWanted wc@(WC { wc_simple = simples1 })
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
                , wc { wc_simple = listToBag new_wanted       `andCts`
                                   listToBag unsolved_wanted  `andCts`
                                   listToBag unsolved_derived `andCts`
                                   listToBag insols } ) } }
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
thePipeline = [ ("canonicalization",        GHC.Tc.Solver.Canonical.canonicalize)
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
-- Precondition: if the workitem is a CEqCan then it will not be able to
-- react with anything at this stage (except, maybe, via a type family
-- dependency)

interactWithInertsStage wi
  = do { inerts <- getTcSInerts
       ; let ics = inert_cans inerts
       ; case wi of
             CEqCan    {} -> interactEq    ics wi
             CIrredCan {} -> interactIrred ics wi
             CDictCan  {} -> interactDict  ics wi
             _ -> pprPanic "interactWithInerts" (ppr wi) }
                -- CNonCanonical have been canonicalised

data InteractResult
   = KeepInert   -- Keep the inert item, and solve the work item from it
                 -- (if the latter is Wanted; just discard it if not)
   | KeepWork    -- Keep the work item, and solve the inert item from it

   | KeepBoth    -- See Note [KeepBoth]

instance Outputable InteractResult where
  ppr KeepBoth  = text "keep both"
  ppr KeepInert = text "keep inert"
  ppr KeepWork  = text "keep work-item"

{- Note [KeepBoth]
~~~~~~~~~~~~~~~~~~
Consider
   Inert:     [WD] C ty1 ty2
   Work item: [D]  C ty1 ty2

Here we can simply drop the work item. But what about
   Inert:     [W] C ty1 ty2
   Work item: [D] C ty1 ty2

Here we /cannot/ drop the work item, becuase we lose the [D] form, and
that is essential for e.g. fundeps, see isImprovable.  We could zap
the inert item to [WD], but the simplest thing to do is simply to keep
both. (They probably started as [WD] and got split; this is relatively
rare and it doesn't seem worth trying to put them back together again.)
-}

solveOneFromTheOther :: CtEvidence  -- Inert    (Dict or Irred)
                     -> CtEvidence  -- WorkItem (same predicate as inert)
                     -> TcS InteractResult
-- Precondition:
-- * inert and work item represent evidence for the /same/ predicate
-- * Both are CDictCan or CIrredCan
--
-- We can always solve one from the other: even if both are wanted,
-- although we don't rewrite wanteds with wanteds, we can combine
-- two wanteds into one by solving one from the other

solveOneFromTheOther ev_i ev_w
  | CtDerived {} <- ev_w         -- Work item is Derived
  = case ev_i of
      CtWanted { ctev_nosh = WOnly } -> return KeepBoth
      _                              -> return KeepInert

  | CtDerived {} <- ev_i         -- Inert item is Derived
  = case ev_w of
      CtWanted { ctev_nosh = WOnly } -> return KeepBoth
      _                              -> return KeepWork
              -- The ev_w is inert wrt earlier inert-set items,
              -- so it's safe to continue on from this point

  -- After this, neither ev_i or ev_w are Derived
  | CtWanted { ctev_loc = loc_w } <- ev_w
  , prohibitedSuperClassSolve (ctEvLoc ev_i) loc_w
  = -- inert must be Given
    do { traceTcS "prohibitedClassSolve1" (ppr ev_i $$ ppr ev_w)
       ; return KeepWork }

  | CtWanted { ctev_nosh = nosh_w } <- ev_w
       -- Inert is Given or Wanted
  = case ev_i of
      CtWanted { ctev_nosh = WOnly }
          | WDeriv <- nosh_w           -> return KeepWork
      _                                -> return KeepInert
      -- Consider work  item [WD] C ty1 ty2
      --          inert item [W]  C ty1 ty2
      -- Then we must keep the work item.  But if the
      -- work item was       [W]  C ty1 ty2
      -- then we are free to discard the work item in favour of inert
      -- Remember, no Deriveds at this point

  -- From here on the work-item is Given

  | CtWanted { ctev_loc = loc_i } <- ev_i
  , prohibitedSuperClassSolve loc_w loc_i
  = do { traceTcS "prohibitedClassSolve2" (ppr ev_i $$ ppr ev_w)
       ; return KeepInert }      -- Just discard the un-usable Given
                                 -- This never actually happens because
                                 -- Givens get processed first

  | CtWanted {} <- ev_i
  = return KeepWork

  -- From here on both are Given
  -- See Note [Replacement vs keeping]

  | lvl_i == lvl_w
  = return same_level_strategy

  | otherwise   -- Both are Given, levels differ
  = return different_level_strategy
  where
     pred  = ctEvPred ev_i
     loc_i = ctEvLoc ev_i
     loc_w = ctEvLoc ev_w
     lvl_i = ctLocLevel loc_i
     lvl_w = ctLocLevel loc_w

     different_level_strategy  -- Both Given
       | isIPLikePred pred = if lvl_w > lvl_i then KeepWork  else KeepInert
       | otherwise         = if lvl_w > lvl_i then KeepInert else KeepWork
       -- See Note [Replacement vs keeping] (the different-level bullet)
       -- For the isIPLikePred case see Note [Shadowing of Implicit Parameters]

     same_level_strategy -- Both Given
       = case (un_given (ctLocOrigin loc_i), un_given (ctLocOrigin loc_w)) of
              -- case 2(b) from Note [Replacement vs keeping]
           (InstSC _depth_i size_i, InstSC _depth_w size_w) | size_w < size_i -> KeepWork
                                                            | otherwise       -> KeepInert

              -- case 2(c) from Note [Replacement vs keeping]
           (InstSC depth_i _, OtherSC depth_w _)  -> choose_shallower depth_i depth_w
           (OtherSC depth_i _, InstSC depth_w _)  -> choose_shallower depth_i depth_w
           (OtherSC depth_i _, OtherSC depth_w _) -> choose_shallower depth_i depth_w

              -- case 2(a) from Note [Replacement vs keeping]
           (InstSC {}, _)                         -> KeepWork
           (OtherSC {}, _)                        -> KeepWork

             -- case 2(d) from Note [Replacement vs keeping]
           _                                      -> KeepInert

     un_given (GivenOrigin skol_info) = skol_info
     un_given orig = pprPanic "Given without GivenOrigin in solveOneFromTheOther" $
                     vcat [ppr ev_i, ppr ev_w, ppr orig]

     choose_shallower depth_i depth_w | depth_w < depth_i = KeepWork
                                      | otherwise         = KeepInert
       -- favor KeepInert in the equals case, according to 2(d) from the Note

{-
Note [Replacement vs keeping]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we have two Given constraints both of type (C tys), say, which should
we keep?  More subtle than you might think! This is all implemented in
solveOneFromTheOther.

  1) Constraints come from different levels (different_level_strategy)

      - For implicit parameters we want to keep the innermost (deepest)
        one, so that it overrides the outer one.
        See Note [Shadowing of Implicit Parameters]

      - For everything else, we want to keep the outermost one.  Reason: that
        makes it more likely that the inner one will turn out to be unused,
        and can be reported as redundant.  See Note [Tracking redundant constraints]
        in GHC.Tc.Solver.

        It transpires that using the outermost one is responsible for an
        8% performance improvement in nofib cryptarithm2, compared to
        just rolling the dice.  I didn't investigate why.

  2) Constraints coming from the same level (i.e. same implication)

       (a) Prefer constraints that are not superclass selections. Example:

             f :: (Eq a, Ord a) => a -> Bool
             f x = x == x

           Eager superclass expansion gives us two [G] Eq a constraints. We
           want to keep the one from the user-written Eq a, not the superclass
           selection. This means we report the Ord a as redundant with
           -Wredundant-constraints, not the Eq a.

           Getting this wrong was #20602. See also
           Note [Tracking redundant constraints] in GHC.Tc.Solver.

       (b) If both are InstSC, choose the one with the smallest TypeSize,
           according to Note [Solving superclass constraints] in GHC.Tc.TyCl.Instance.

       (c) If both are superclass selections (but not both InstSC), choose the one
           with the shallower superclass-selection depth, in the hope of identifying
           more correct redundant constraints. This is really a generalization of
           point (a).

       (d) Finally, when there is still a choice, use KeepInert rather than
           KeepWork, for two reasons:
             - to avoid unnecessary munging of the inert set.
             - to cut off superclass loops; see Note [Superclass loops] in GHC.Tc.Solver.Canonical

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

interactIrred inerts workItem@(CIrredCan { cc_ev = ev_w, cc_reason = reason })
  | isInsolubleReason reason
               -- For insolubles, don't allow the constraint to be dropped
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
            KeepBoth  -> continueWith workItem
            KeepInert -> do { setEvBindIfWanted ev_w (swap_me swap ev_i)
                            ; return (Stop ev_w (text "Irred equal" <+> parens (ppr what_next))) }
            KeepWork ->  do { setEvBindIfWanted ev_i (swap_me swap ev_w)
                            ; updInertIrreds (\_ -> others)
                            ; continueWith workItem } }

  | otherwise
  = continueWith workItem

  where
    swap_me :: SwapFlag -> CtEvidence -> EvTerm
    swap_me swap ev
      = case swap of
           NotSwapped -> ctEvTerm ev
           IsSwapped  -> evCoercion (mkTcSymCo (evTermCoercion (ctEvTerm ev)))

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
Consider (#14333)
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

  forall c. Eq c => { wc_simple = [W] D [c] c }

which we have simplified to, with a Derived constraing coming from
D's functional dependency:

  forall c. Eq c => { wc_simple = [W] D [c] c [W]
                                  [D] (c ~ [c]) }

When iterating the solver, we might try to re-solve this
implication. If we do not do a dropDerivedWC, then we will end up
trying to solve the following constraints the second time:

  [W] (D [c] c)
  [D] (c ~ [c])

which will result in two Deriveds to end up in the insoluble set:

  wc_simple = [W] D [c] c
              [D] (c ~ [c])
              [D] (c ~ [c])
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
Suppose we have (#13943)
  class Take (n :: Nat) where ...
  instance {-# OVERLAPPING #-}                    Take 0 where ..
  instance {-# OVERLAPPABLE #-} (Take (n - 1)) => Take n where ..

And we have [W] Take 3.  That only matches one instance so we get
[W] Take (3-1).  Really we should now rewrite to reduce the (3-1) to 2, and
so on -- but that is reproducing yet more of the solver.  Sigh.  For now,
we just give up (remember all this is just an optimisation).

But we must not just naively try to lookup (Take (3-1)) in the
InstEnv, or it'll (wrongly) appear not to match (Take 0) and get a
unique match on the (Take n) instance.  That leads immediately to an
infinite loop.  Hence the check that 'preds' have no type families
(isTyFamFree).

Note [Shortcut solving: incoherence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This optimization relies on coherence of dictionaries to be correct. When we
cannot assume coherence because of IncoherentInstances then this optimization
can change the behavior of the user's code.

The following four modules produce a program whose output would change depending
on whether we apply this optimization when IncoherentInstances is in effect:

=========
    {-# LANGUAGE MultiParamTypeClasses #-}
    module A where

    class A a where
      int :: a -> Int

    class A a => C a b where
      m :: b -> a -> a

=========
    {-# LANGUAGE FlexibleInstances     #-}
    {-# LANGUAGE MultiParamTypeClasses #-}
    module B where

    import A

    instance A a where
      int _ = 1

    instance C a [b] where
      m _ = id

=========
    {-# LANGUAGE FlexibleContexts      #-}
    {-# LANGUAGE FlexibleInstances     #-}
    {-# LANGUAGE IncoherentInstances   #-}
    {-# LANGUAGE MultiParamTypeClasses #-}
    module C where

    import A

    instance A Int where
      int _ = 2

    instance C Int [Int] where
      m _ = id

    intC :: C Int a => a -> Int -> Int
    intC _ x = int x

=========
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
    try_solve_from_instance :: (EvBindMap, DictMap CtEvidence)
                            -> CtEvidence       -- Solve this
                            -> MaybeT TcS (EvBindMap, DictMap CtEvidence)
Note that:

* The CtEvidence is the goal to be solved

* The MaybeT manages early failure if we find a subgoal that
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

* As #15164 showed, it can be important to exploit sharing between
  goals. E.g. To solve G we may need G1 and G2. To solve G1 we may need H;
  and to solve G2 we may need H. If we don't spot this sharing we may
  solve H twice; and if this pattern repeats we may get exponentially bad
  behaviour.
-}

interactDict :: InertCans -> Ct -> TcS (StopOrContinue Ct)
interactDict inerts workItem@(CDictCan { cc_ev = ev_w, cc_class = cls, cc_tyargs = tys })
  | Just ct_i <- lookupInertDict inerts (ctEvLoc ev_w) cls tys
  , let ev_i = ctEvidence ct_i
  = -- There is a matching dictionary in the inert set
    do { -- First to try to solve it /completely/ from top level instances
         -- See Note [Shortcut solving]
         dflags <- getDynFlags
       ; short_cut_worked <- shortCutSolver dflags ev_w ev_i
       ; if short_cut_worked
         then stopWith ev_w "interactDict/solved from instance"
         else

    do { -- Ths short-cut solver didn't fire, so we
         -- solve ev_w from the matching inert ev_i we found
         what_next <- solveOneFromTheOther ev_i ev_w
       ; traceTcS "lookupInertDict" (ppr what_next)
       ; case what_next of
           KeepBoth  -> continueWith workItem
           KeepInert -> do { setEvBindIfWanted ev_w (ctEvTerm ev_i)
                           ; return $ Stop ev_w (text "Dict equal" <+> parens (ppr what_next)) }
           KeepWork  -> do { setEvBindIfWanted ev_i (ctEvTerm ev_w)
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

 && not (isIPLikePred (ctEvPred ev_w))   -- Not for implicit parameters (#18627)

 && not (xopt LangExt.IncoherentInstances dflags)
 -- If IncoherentInstances is on then we cannot rely on coherence of proofs
 -- in order to justify this optimization: The proof provided by the
 -- [G] constraint's superclass may be different from the top-level proof.
 -- See Note [Shortcut solving: incoherence]

 && gopt Opt_SolveConstantDicts dflags
 -- Enabled by the -fsolve-constant-dicts flag

  = do { ev_binds_var <- getTcEvBindsVar
       ; ev_binds <- ASSERT2( not (isCoEvBindsVar ev_binds_var ), ppr ev_w )
                     getTcEvBindsMap ev_binds_var
       ; solved_dicts <- getSolvedDicts

       ; mb_stuff <- runMaybeT $ try_solve_from_instance
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
      :: (EvBindMap, DictMap CtEvidence) -> CtEvidence
      -> MaybeT TcS (EvBindMap, DictMap CtEvidence)
    try_solve_from_instance (ev_binds, solved_dicts) ev
      | let pred = ctEvPred ev
            loc  = ctEvLoc  ev
      , ClassPred cls tys <- classifyPredType pred
      = do { inst_res <- lift $ matchGlobalInst dflags True cls tys
           ; case inst_res of
               OneInst { cir_new_theta = preds
                       , cir_mk_ev     = mk_ev
                       , cir_what      = what }
                 | safeOverlap what
                 , all isTyFamFree preds  -- Note [Shortcut solving: type families]
                 -> do { let solved_dicts' = addDict solved_dicts cls tys ev
                             -- solved_dicts': it is important that we add our goal
                             -- to the cache before we solve! Otherwise we may end
                             -- up in a loop while solving recursive dictionaries.

                       ; lift $ traceTcS "shortCutSolver: found instance" (ppr preds)
                       ; loc' <- lift $ checkInstanceOK loc what pred
                       ; lift $ checkReductionDepth loc' pred


                       ; evc_vs <- mapM (new_wanted_cached loc' solved_dicts') preds
                                  -- Emit work for subgoals but use our local cache
                                  -- so we can solve recursive dictionaries.

                       ; let ev_tm     = mk_ev (map getEvExpr evc_vs)
                             ev_binds' = extendEvBinds ev_binds $
                                         mkWantedEvBind (ctEvEvId ev) ev_tm

                       ; foldlM try_solve_from_instance
                                (ev_binds', solved_dicts')
                                (freshGoals evc_vs) }

               _ -> mzero }
      | otherwise = mzero


    -- Use a local cache of solved dicts while emitting EvVars for new work
    -- We bail out of the entire computation if we need to emit an EvVar for
    -- a subgoal that isn't a ClassPred.
    new_wanted_cached :: CtLoc -> DictMap CtEvidence -> TcPredType -> MaybeT TcS MaybeNew
    new_wanted_cached loc cache pty
      | ClassPred cls tys <- classifyPredType pty
      = lift $ case findDict cache loc_w cls tys of
          Just ctev -> return $ Cached (ctEvExpr ctev)
          Nothing   -> Fresh <$> newWantedNC loc pty
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
                               , ctl_origin = FunDepOrigin1 work_pred
                                                            (ctLocOrigin work_loc)
                                                            (ctLocSpan work_loc)
                                                            inert_pred
                                                            (ctLocOrigin inert_loc)
                                                            (ctLocSpan inert_loc) }

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

See ticket #17104 for a rather tricky example of this overriding
behaviour.

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

improveLocalFunEqs :: CtEvidence -> InertCans -> TyCon -> [TcType] -> TcType
                   -> TcS ()
-- Generate derived improvement equalities, by comparing
-- the current work item with inert CFunEqs
-- E.g.   x + y ~ z,   x + y' ~ z   =>   [D] y ~ y'
--
-- See Note [FunDep and implicit parameter reactions]
-- Precondition: isImprovable work_ev
improveLocalFunEqs work_ev inerts fam_tc args rhs
  = ASSERT( isImprovable work_ev )
    unless (null improvement_eqns) $
    do { traceTcS "interactFunEq improvements: " $
                   vcat [ text "Eqns:" <+> ppr improvement_eqns
                        , text "Candidates:" <+> ppr funeqs_for_tc
                        , text "Inert eqs:" <+> ppr (inert_eqs inerts) ]
       ; emitFunDepDeriveds improvement_eqns }
  where
    funeqs        = inert_funeqs inerts
    funeqs_for_tc = [ funeq_ct | EqualCtList (funeq_ct :| _)
                                   <- findFunEqsByTyCon funeqs fam_tc
                               , NomEq == ctEqRel funeq_ct ]
                                  -- representational equalities don't interact
                                  -- with type family dependencies
    work_loc      = ctEvLoc work_ev
    work_pred     = ctEvPred work_ev
    fam_inj_info  = tyConInjectivityInfo fam_tc

    --------------------
    improvement_eqns :: [FunDepEqn CtLoc]
    improvement_eqns
      | Just ops <- isBuiltInSynFamTyCon_maybe fam_tc
      =    -- Try built-in families, notably for arithmethic
        concatMap (do_one_built_in ops rhs) funeqs_for_tc

      | Injective injective_args <- fam_inj_info
      =    -- Try improvement from type families with injectivity annotations
        concatMap (do_one_injective injective_args rhs) funeqs_for_tc

      | otherwise
      = []

    --------------------
    do_one_built_in ops rhs (CEqCan { cc_lhs = TyFamLHS _ iargs, cc_rhs = irhs, cc_ev = inert_ev })
      = mk_fd_eqns inert_ev (sfInteractInert ops args rhs iargs irhs)

    do_one_built_in _ _ _ = pprPanic "interactFunEq 1" (ppr fam_tc)

    --------------------
    -- See Note [Type inference for type families with injectivity]
    do_one_injective inj_args rhs (CEqCan { cc_lhs = TyFamLHS _ inert_args
                                          , cc_rhs = irhs, cc_ev = inert_ev })
      | isImprovable inert_ev
      , rhs `tcEqType` irhs
      = mk_fd_eqns inert_ev $ [ Pair arg iarg
                              | (arg, iarg, True) <- zip3 args inert_args inj_args ]
      | otherwise
      = []

    do_one_injective _ _ _ = pprPanic "interactFunEq 2" (ppr fam_tc)

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

{- Note [Type inference for type families with injectivity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have a type family with an injectivity annotation:
    type family F a b = r | r -> b

Then if we have an equality like F s1 t1 ~ F s2 t2,
we can use the injectivity to get a new Derived constraint on
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
and inferring values of type variables (b in this example) from the LHS patterns
of the matching equation.  For closed type families we have to perform
additional apartness check for the selected equation to check that the selected
is guaranteed to fire for given LHS arguments.

These new constraints are simply *Derived* constraints; they have no evidence.
We could go further and offer evidence from decomposing injective type-function
applications, but that would require new evidence forms, and an extension to
FC, so we don't do that right now (Dec 14).

We generate these Deriveds in three places, depending on how we notice the
injectivity.

1. When we have a [W/D] F tys1 ~ F tys2. This is handled in canEqCanLHS2, and
described in Note [Decomposing equality] in GHC.Tc.Solver.Canonical.

2. When we have [W] F tys1 ~ T and [W] F tys2 ~ T. Note that neither of these
constraints rewrites the other, as they have different LHSs. This is done
in improveLocalFunEqs, called during the interactWithInertsStage.

3. When we have [W] F tys ~ T and an equation for F that looks like F tys' = T.
This is done in improve_top_fun_eqs, called from the top-level reactions stage.

See also Note [Injective type families] in GHC.Core.TyCon

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

**********************************************************************
*                                                                    *
                   interactEq
*                                                                    *
**********************************************************************
-}

{- Note [Combining equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
   Inert:     g1 :: a ~ t
   Work item: g2 :: a ~ t

Then we can simply solve g2 from g1, thus g2 := g1.  Easy!
But it's not so simple:

* If t is a type variable, the equalties might be oriented differently:
      e.g. (g1 :: a~b) and (g2 :: b~a)
  So we look both ways round.  Hence the SwapFlag result to
  inertsCanDischarge.

* We can only do g2 := g1 if g1 can discharge g2; that depends on
  (a) the role and (b) the flavour.  E.g. a representational equality
  cannot discharge a nominal one; a Wanted cannot discharge a Given.
  The predicate is eqCanDischargeFR.

* If the inert is [W] and the work-item is [WD] we don't want to
  forget the [D] part; hence the Bool result of inertsCanDischarge.

* Visibility. Suppose  S :: forall k. k -> Type, and consider unifying
      S @Type (a::Type)  ~   S @(Type->Type) (b::Type->Type)
  From the first argument we get (Type ~ Type->Type); from the second
  argument we get (a ~ b) which in turn gives (Type ~ Type->Type).
  See typecheck/should_fail/T16204c.

  That first argument is invisible in the source program (aside from
  visible type application), so we'd much prefer to get the error from
  the second. We track visiblity in the uo_visible field of a TypeEqOrigin.
  We use this to prioritise visible errors (see GHC.Tc.Errors.tryReporters,
  the partition on isVisibleOrigin).

  So when combining two otherwise-identical equalites, we want to
  keep the visible one, and discharge the invisible one.  Hence the
  call to strictly_more_visible.
-}

inertsCanDischarge :: InertCans -> Ct
                   -> Maybe ( CtEvidence  -- The evidence for the inert
                            , SwapFlag    -- Whether we need mkSymCo
                            , Bool)       -- True <=> keep a [D] version
                                          --          of the [WD] constraint
inertsCanDischarge inerts (CEqCan { cc_lhs = lhs_w, cc_rhs = rhs_w
                                  , cc_ev = ev_w, cc_eq_rel = eq_rel })
  | (ev_i : _) <- [ ev_i | CEqCan { cc_ev = ev_i, cc_rhs = rhs_i
                                  , cc_eq_rel = eq_rel }
                             <- findEq inerts lhs_w
                         , rhs_i `tcEqType` rhs_w
                         , inert_beats_wanted ev_i eq_rel ]
  =  -- Inert:     a ~ ty
     -- Work item: a ~ ty
    Just (ev_i, NotSwapped, keep_deriv ev_i)

  | Just rhs_lhs <- canEqLHS_maybe rhs_w
  , (ev_i : _) <- [ ev_i | CEqCan { cc_ev = ev_i, cc_rhs = rhs_i
                                  , cc_eq_rel = eq_rel }
                             <- findEq inerts rhs_lhs
                         , rhs_i `tcEqType` canEqLHSType lhs_w
                         , inert_beats_wanted ev_i eq_rel ]
  =  -- Inert:     a ~ b
     -- Work item: b ~ a
     Just (ev_i, IsSwapped, keep_deriv ev_i)

  where
    loc_w  = ctEvLoc ev_w
    flav_w = ctEvFlavour ev_w
    fr_w   = (flav_w, eq_rel)

    inert_beats_wanted ev_i eq_rel
      = -- eqCanDischargeFR:    see second bullet of Note [Combining equalities]
        -- strictly_more_visible: see last bullet of Note [Combining equalities]
        fr_i`eqCanDischargeFR` fr_w
        && not ((loc_w `strictly_more_visible` ctEvLoc ev_i)
                 && (fr_w `eqCanDischargeFR` fr_i))
      where
        fr_i = (ctEvFlavour ev_i, eq_rel)

    -- See Note [Combining equalities], third bullet
    keep_deriv ev_i
      | Wanted WOnly  <- ctEvFlavour ev_i  -- inert is [W]
      , Wanted WDeriv <- flav_w            -- work item is [WD]
      = True   -- Keep a derived version of the work item
      | otherwise
      = False  -- Work item is fully discharged

    -- See Note [Combining equalities], final bullet
    strictly_more_visible loc1 loc2
       = not (isVisibleOrigin (ctLocOrigin loc2)) &&
         isVisibleOrigin (ctLocOrigin loc1)

inertsCanDischarge _ _ = Nothing


interactEq :: InertCans -> Ct -> TcS (StopOrContinue Ct)
interactEq inerts workItem@(CEqCan { cc_lhs = lhs
                                   , cc_rhs = rhs
                                   , cc_ev = ev
                                   , cc_eq_rel = eq_rel })
  | Just (ev_i, swapped, keep_deriv) <- inertsCanDischarge inerts workItem
  = do { setEvBindIfWanted ev $
         evCoercion (maybeTcSymCo swapped $
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
  = do { traceTcS "Not unifying representational equality" (ppr workItem)
       ; continueWith workItem }

  | otherwise
  = case lhs of
       TyVarLHS tv -> tryToSolveByUnification workItem ev tv rhs

       TyFamLHS tc args -> do { when (isImprovable ev) $
                                 -- Try improvement, if possible
                                improveLocalFunEqs ev inerts tc args rhs
                              ; continueWith workItem }

interactEq _ wi = pprPanic "interactEq" (ppr wi)

----------------------
-- We have a meta-tyvar on the left, and metaTyVarUpateOK has said "yes"
-- So try to solve by unifying.
-- Three reasons why not:
--    Skolem escape
--    Given equalities (GADTs)
--    Unifying a TyVarTv with a non-tyvar type
tryToSolveByUnification :: Ct -> CtEvidence
                        -> TcTyVar   -- LHS tyvar
                        -> TcType    -- RHS
                        -> TcS (StopOrContinue Ct)
tryToSolveByUnification work_item ev tv rhs
  = do { is_touchable <- touchabilityTest (ctEvFlavour ev) tv rhs
       ; traceTcS "tryToSolveByUnification" (vcat [ ppr tv <+> char '~' <+> ppr rhs
                                                  , ppr is_touchable ])

       ; case is_touchable of
           Untouchable -> continueWith work_item
           -- For the latter two cases see Note [Solve by unification]
           TouchableSameLevel -> solveByUnification ev tv rhs
           TouchableOuterLevel free_metas tv_lvl
             -> do { wrapTcS $ mapM_ (promoteMetaTyVarTo tv_lvl) free_metas
                   ; setUnificationFlag tv_lvl
                   ; solveByUnification ev tv rhs } }

solveByUnification :: CtEvidence -> TcTyVar -> Xi -> TcS (StopOrContinue Ct)
-- Solve with the identity coercion
-- Precondition: kind(xi) equals kind(tv)
-- Precondition: CtEvidence is Wanted or Derived
-- Precondition: CtEvidence is nominal
-- Returns: workItem where
--        workItem = the new Given constraint
--
-- NB: No need for an occurs check here, because solveByUnification always
--     arises from a CEqCan, a *canonical* constraint.  Its invariant (TyEq:OC)
--     says that in (a ~ xi), the type variable a does not appear in xi.
--     See GHC.Tc.Types.Constraint.Ct invariants.
--
-- Post: tv is unified (by side effect) with xi;
--       we often write tv := xi
solveByUnification wd tv xi
  = do { let tv_ty = mkTyVarTy tv
       ; traceTcS "Sneaky unification:" $
                       vcat [text "Unifies:" <+> ppr tv <+> text ":=" <+> ppr xi,
                             text "Coercion:" <+> pprEq tv_ty xi,
                             text "Left Kind is:" <+> ppr (tcTypeKind tv_ty),
                             text "Right Kind is:" <+> ppr (tcTypeKind xi) ]
       ; unifyTyVar tv xi
       ; setEvBindIfWanted wd (evCoercion (mkTcNomReflCo xi))
       ; n_kicked <- kickOutAfterUnification tv
       ; return (Stop wd (text "Solved by unification" <+> pprKicked n_kicked)) }

{- Note [Avoid double unifications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The spontaneous solver has to return a given which mentions the unified unification
variable *on the left* of the equality. Here is what happens if not:
  Original wanted:  (a ~ alpha),  (alpha ~ Int)
We spontaneously solve the first wanted, without changing the order!
      given : a ~ alpha      [having unified alpha := a]
Now the second wanted comes along, but it cannot rewrite the given, so we simply continue.
At the end we spontaneously solve that guy, *reunifying*  [alpha := Int]

We avoid this problem by orienting the resulting given so that the unification
variable is on the left.  [Note that alternatively we could attempt to
enforce this at canonicalization]

See also Note [No touchables as FunEq RHS] in GHC.Tc.Solver.Monad; avoiding
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

See also #15144, which was caused by unifying a representational
equality.

Note [Solve by unification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we solve
   alpha[n] ~ ty
by unification, there are two cases to consider

* TouchableSameLevel: if the ambient level is 'n', then
  we can simply update alpha := ty, and do nothing else

* TouchableOuterLevel free_metas n: if the ambient level is greater than
  'n' (the level of alpha), in addition to setting alpha := ty we must
  do two other things:

  1. Promote all the free meta-vars of 'ty' to level n.  After all,
     alpha[n] is at level n, and so if we set, say,
          alpha[n] := Maybe beta[m],
     we must ensure that when unifying beta we do skolem-escape checks
     etc relevant to level n.  Simple way to do that: promote beta to
     level n.

  2. Set the Unification Level Flag to record that a level-n unification has
     taken place. See Note [The Unification Level Flag] in GHC.Tc.Solver.Monad

NB: TouchableSameLevel is just an optimisation for TouchableOuterLevel. Promotion
would be a no-op, and setting the unification flag unnecessarily would just
make the solver iterate more often.  (We don't need to iterate when unifying
at the ambient level because of the kick-out mechanism.)


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
           may produce new insoluble goals: see #4952

 Danger 2: If we don't rewrite the constraint, it may re-react
           with the same thing later, and produce the same equality
           again --> termination worries.

To achieve this required some refactoring of GHC.Tc.Instance.FunDeps (nicer
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

#7875 is a case in point.
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
     = do { traceTcS "emitFunDepDeriveds 2" (ppr (ctl_depth loc) $$ ppr tvs $$ ppr eqs)
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
-- The work item does not react with the inert set,
-- so try interaction with top-level instances. Note:
topReactionsStage work_item
  = do { traceTcS "doTopReact" (ppr work_item)
       ; case work_item of
           CDictCan {}  -> do { inerts <- getTcSInerts
                              ; doTopReactDict inerts work_item }
           CEqCan {}    -> doTopReactEq    work_item
           CIrredCan {} -> doTopReactOther work_item
           _  -> -- Any other work item does not react with any top-level equations
                 continueWith work_item  }


--------------------
doTopReactOther :: Ct -> TcS (StopOrContinue Ct)
-- Try local quantified constraints for
--     CEqCan    e.g.  (lhs ~# ty)
-- and CIrredCan e.g.  (c a)
--
-- Why equalities? See GHC.Tc.Solver.Canonical
-- Note [Equality superclasses in quantified constraints]
doTopReactOther work_item
  | isGiven ev
  = continueWith work_item

  | EqPred eq_rel t1 t2 <- classifyPredType pred
  = doTopReactEqPred work_item eq_rel t1 t2

  | otherwise
  = do { res <- matchLocalInst pred loc
       ; case res of
           OneInst {} -> chooseInstance work_item res
           _          -> continueWith work_item }

  where
    ev   = ctEvidence work_item
    loc  = ctEvLoc ev
    pred = ctEvPred ev

doTopReactEqPred :: Ct -> EqRel -> TcType -> TcType -> TcS (StopOrContinue Ct)
doTopReactEqPred work_item eq_rel t1 t2
  -- See Note [Looking up primitive equalities in quantified constraints]
  | Just (cls, tys) <- boxEqPred eq_rel t1 t2
  = do { res <- matchLocalInst (mkClassPred cls tys) loc
       ; case res of
           OneInst { cir_mk_ev = mk_ev }
             -> chooseInstance work_item
                    (res { cir_mk_ev = mk_eq_ev cls tys mk_ev })
           _ -> continueWith work_item }

  | otherwise
  = continueWith work_item
  where
    ev   = ctEvidence work_item
    loc = ctEvLoc ev

    mk_eq_ev cls tys mk_ev evs
      = case (mk_ev evs) of
          EvExpr e -> EvExpr (Var sc_id `mkTyApps` tys `App` e)
          ev       -> pprPanic "mk_eq_ev" (ppr ev)
      where
        [sc_id] = classSCSelIds cls

{- Note [Looking up primitive equalities in quantified constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For equalities (a ~# b) look up (a ~ b), and then do a superclass
selection. This avoids having to support quantified constraints whose
kind is not Constraint, such as (forall a. F a ~# b)

See
 * Note [Evidence for quantified constraints] in GHC.Core.Predicate
 * Note [Equality superclasses in quantified constraints]
   in GHC.Tc.Solver.Canonical
-}

--------------------
doTopReactEq :: Ct -> TcS (StopOrContinue Ct)
doTopReactEq work_item@(CEqCan { cc_ev = old_ev, cc_lhs = TyFamLHS fam_tc args
                               , cc_rhs = rhs })
  = do { improveTopFunEqs old_ev fam_tc args rhs
       ; doTopReactOther work_item }
doTopReactEq work_item = doTopReactOther work_item

improveTopFunEqs :: CtEvidence -> TyCon -> [TcType] -> TcType -> TcS ()
-- See Note [FunDep and implicit parameter reactions]
improveTopFunEqs ev fam_tc args rhs
  | not (isImprovable ev)
  = return ()

  | otherwise
  = do { fam_envs <- getFamInstEnvs
       ; eqns <- improve_top_fun_eqs fam_envs fam_tc args rhs
       ; traceTcS "improveTopFunEqs" (vcat [ ppr fam_tc <+> ppr args <+> ppr rhs
                                          , ppr eqns ])
       ; mapM_ (unifyDerived loc Nominal) eqns }
  where
    loc = bumpCtLocDepth (ctEvLoc ev)
        -- ToDo: this location is wrong; it should be FunDepOrigin2
        -- See #14778

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
                  -- Hence instFlexiX.   #13135 was an example.

             ; return [ Pair (substTyUnchecked subst ax_arg) arg
                        -- NB: the ax_arg part is on the left
                        -- see Note [Improvement orientation]
                      | case cabr of
                          Just cabr' -> apartnessCheck (substTys subst ax_args) cabr'
                          _          -> True
                      , (ax_arg, arg, True) <- zip3 ax_args args inj_args ] }

{-
Note [MATCHING-SYNONYMS]
~~~~~~~~~~~~~~~~~~~~~~~~
When trying to match a dictionary (D tau) to a top-level instance, or a
type family equation (F taus_1 ~ tau_2) to a top-level family instance,
we do *not* need to expand type synonyms because the matcher will do that for us.

Note [Improvement orientation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A very delicate point is the orientation of derived equalities
arising from injectivity improvement (#12522).  Suppose we have
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

-}

{- *******************************************************************
*                                                                    *
         Top-level reaction for class constraints (CDictCan)
*                                                                    *
**********************************************************************-}

doTopReactDict :: InertSet -> Ct -> TcS (StopOrContinue Ct)
-- Try to use type-class instance declarations to simplify the constraint
doTopReactDict inerts work_item@(CDictCan { cc_ev = ev, cc_class = cls
                                          , cc_tyargs = xis })
  | isGiven ev   -- Never use instances for Given constraints
  = do { try_fundep_improvement
       ; continueWith work_item }

  | Just solved_ev <- lookupSolvedDict inerts dict_loc cls xis   -- Cached
  = do { setEvBindIfWanted ev (ctEvTerm solved_ev)
       ; stopWith ev "Dict/Top (cached)" }

  | otherwise  -- Wanted or Derived, but not cached
   = do { dflags <- getDynFlags
        ; lkup_res <- matchClassInst dflags inerts cls xis dict_loc
        ; case lkup_res of
               OneInst { cir_what = what }
                  -> do { insertSafeOverlapFailureTcS what work_item
                        ; addSolvedDict what ev cls xis
                        ; chooseInstance work_item lkup_res }
               _  ->  -- NoInstance or NotSure
                     do { when (isImprovable ev) $
                          try_fundep_improvement
                        ; continueWith work_item } }
   where
     dict_pred   = mkClassPred cls xis
     dict_loc    = ctEvLoc ev
     dict_origin = ctLocOrigin dict_loc

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


chooseInstance :: Ct -> ClsInstResult -> TcS (StopOrContinue Ct)
chooseInstance work_item
               (OneInst { cir_new_theta = theta
                        , cir_what      = what
                        , cir_mk_ev     = mk_ev })
  = do { traceTcS "doTopReact/found instance for" $ ppr ev
       ; deeper_loc <- checkInstanceOK loc what pred
       ; if isDerived ev
         then -- Use type-class instances for Deriveds, in the hope
              -- of generating some improvements
              -- C.f. Example 3 of Note [The improvement story]
              -- It's easy because no evidence is involved
           do { dflags <- getDynFlags
              ; unless (subGoalDepthExceeded dflags (ctLocDepth deeper_loc)) $
                emitNewDeriveds deeper_loc theta
                  -- If we have a runaway Derived, let's not issue a
                  -- "reduction stack overflow" error, which is not particularly
                  -- friendly. Instead, just drop the Derived.
              ; traceTcS "finish_derived" (ppr (ctl_depth deeper_loc))
              ; stopWith ev "Dict/Top (solved derived)" }

         else -- wanted
           do { checkReductionDepth deeper_loc pred
              ; evb <- getTcEvBindsVar
              ; if isCoEvBindsVar evb
                then continueWith work_item
                  -- See Note [Instances in no-evidence implications]

                else
           do { evc_vars <- mapM (newWanted deeper_loc) theta
              ; setEvBindIfWanted ev (mk_ev (map getEvExpr evc_vars))
              ; emitWorkNC (freshGoals evc_vars)
              ; stopWith ev "Dict/Top (solved wanted)" }}}
  where
     ev         = ctEvidence work_item
     pred       = ctEvPred ev
     loc        = ctEvLoc ev

chooseInstance work_item lookup_res
  = pprPanic "chooseInstance" (ppr work_item $$ ppr lookup_res)

checkInstanceOK :: CtLoc -> InstanceWhat -> TcPredType -> TcS CtLoc
-- Check that it's OK to use this insstance:
--    (a) the use is well staged in the Template Haskell sense
-- Returns the CtLoc to used for sub-goals
-- Probably also want to call checkReductionDepth, but this function
-- does not do so to enable special handling for Deriveds in chooseInstance
checkInstanceOK loc what pred
  = do { checkWellStagedDFun loc what pred
       ; return deeper_loc }
  where
     deeper_loc = zap_origin (bumpCtLocDepth loc)
     origin     = ctLocOrigin loc

     zap_origin loc  -- After applying an instance we can set ScOrigin to
                     -- infinity, so that prohibitedSuperClassSolve never fires
       | ScOrigin {} <- origin
       = setCtLocOrigin loc (ScOrigin infinity)
       | otherwise
       = loc

{- Note [Instances in no-evidence implications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In #15290 we had
  [G] forall p q. Coercible p q => Coercible (m p) (m q))
  [W] forall <no-ev> a. m (Int, IntStateT m a)
                          ~R#
                        m (Int, StateT Int m a)

The Given is an ordinary quantified constraint; the Wanted is an implication
equality that arises from
  [W] (forall a. t1) ~R# (forall a. t2)

But because the (t1 ~R# t2) is solved "inside a type" (under that forall a)
we can't generate any term evidence.  So we can't actually use that
lovely quantified constraint.  Alas!

This test arranges to ignore the instance-based solution under these
(rare) circumstances.   It's sad, but I  really don't see what else we can do.
-}


matchClassInst :: DynFlags -> InertSet
               -> Class -> [Type]
               -> CtLoc -> TcS ClsInstResult
matchClassInst dflags inerts clas tys loc
-- First check whether there is an in-scope Given that could
-- match this constraint.  In that case, do not use any instance
-- whether top level, or local quantified constraints.
-- See Note [Instance and Given overlap]
  | not (xopt LangExt.IncoherentInstances dflags)
  , not (naturallyCoherentClass clas)
  , let matchable_givens = matchableGivens loc pred inerts
  , not (isEmptyBag matchable_givens)
  = do { traceTcS "Delaying instance application" $
           vcat [ text "Work item=" <+> pprClassPred clas tys
                , text "Potential matching givens:" <+> ppr matchable_givens ]
       ; return NotSure }

  | otherwise
  = do { traceTcS "matchClassInst" $ text "pred =" <+> ppr pred <+> char '{'
       ; local_res <- matchLocalInst pred loc
       ; case local_res of
           OneInst {} ->  -- See Note [Local instances and incoherence]
                do { traceTcS "} matchClassInst local match" $ ppr local_res
                   ; return local_res }

           NotSure -> -- In the NotSure case for local instances
                      -- we don't want to try global instances
                do { traceTcS "} matchClassInst local not sure" empty
                   ; return local_res }

           NoInstance  -- No local instances, so try global ones
              -> do { global_res <- matchGlobalInst dflags False clas tys
                    ; traceTcS "} matchClassInst global result" $ ppr global_res
                    ; return global_res } }
  where
    pred = mkClassPred clas tys

-- | If a class is "naturally coherent", then we needn't worry at all, in any
-- way, about overlapping/incoherent instances. Just solve the thing!
-- See Note [Naturally coherent classes]
-- See also Note [The equality class story] in "GHC.Builtin.Types.Prim".
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

From 'g' we get the implication constraint:
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
tickets #4981 and #5002.

Other notes:

* The check is done *first*, so that it also covers classes
  with built-in instance solving, such as
     - constraint tuples
     - natural numbers
     - Typeable

* See also Note [What might equal later?] in GHC.Tc.Solver.Monad.

* The given-overlap problem is arguably not easy to appear in practice
  due to our aggressive prioritization of equality solving over other
  constraints, but it is possible. I've added a test case in
  typecheck/should-compile/GivenOverlapping.hs

* Another "live" example is #10195; another is #10177.

* We ignore the overlap problem if -XIncoherentInstances is in force:
  see #6002 for a worked-out example where this makes a
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
  will use the instance declaration after all. #11948 was a case
  in point.

All of this is disgustingly delicate, so to discourage people from writing
simplifiable class givens, we warn about signatures that contain them;
see GHC.Tc.Validity Note [Simplifiable given constraints].

Note [Naturally coherent classes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A few built-in classes are "naturally coherent".  This term means that
the "instance" for the class is bidirectional with its superclass(es).
For example, consider (~~), which behaves as if it was defined like
this:
  class a ~# b => a ~~ b
  instance a ~# b => a ~~ b
(See Note [The equality types story] in GHC.Builtin.Types.Prim.)

Faced with [W] t1 ~~ t2, it's always OK to reduce it to [W] t1 ~# t2,
without worrying about Note [Instance and Given overlap].  Why?  Because
if we had [G] s1 ~~ s2, then we'd get the superclass [G] s1 ~# s2, and
so the reduction of the [W] constraint does not risk losing any solutions.

On the other hand, it can be fatal to /fail/ to reduce such
equalities, on the grounds of Note [Instance and Given overlap],
because many good things flow from [W] t1 ~# t2.

The same reasoning applies to

* (~~)        heqTyCon
* (~)         eqTyCon
* Coercible   coercibleTyCon

And less obviously to:

* Tuple classes.  For reasons described in GHC.Tc.Solver.Monad
  Note [Tuples hiding implicit parameters], we may have a constraint
     [W] (?x::Int, C a)
  with an exactly-matching Given constraint.  We must decompose this
  tuple and solve the components separately, otherwise we won't solve
  it at all!  It is perfectly safe to decompose it, because again the
  superclasses invert the instance;  e.g.
      class (c1, c2) => (% c1, c2 %)
      instance (c1, c2) => (% c1, c2 %)
  Example in #14218

Exammples: T5853, T10432, T5315, T9222, T2627b, T3028b

PS: the term "naturally coherent" doesn't really seem helpful.
Perhaps "invertible" or something?  I left it for now though.

Note [Local instances and incoherence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   f :: forall b c. (Eq b, forall a. Eq a => Eq (c a))
                 => c b -> Bool
   f x = x==x

We get [W] Eq (c b), and we must use the local instance to solve it.

BUT that wanted also unifies with the top-level Eq [a] instance,
and Eq (Maybe a) etc.  We want the local instance to "win", otherwise
we can't solve the wanted at all.  So we mark it as Incohherent.
According to Note [Rules for instance lookup] in GHC.Core.InstEnv, that'll
make it win even if there are other instances that unify.

Moreover this is not a hack!  The evidence for this local instance
will be constructed by GHC at a call site... from the very instances
that unify with it here.  It is not like an incoherent user-written
instance which might have utterly different behaviour.

Consdider  f :: Eq a => blah.  If we have [W] Eq a, we certainly
get it from the Eq a context, without worrying that there are
lots of top-level instances that unify with [W] Eq a!  We'll use
those instances to build evidence to pass to f. That's just the
nullary case of what's happening here.
-}

matchLocalInst :: TcPredType -> CtLoc -> TcS ClsInstResult
-- Look up the predicate in Given quantified constraints,
-- which are effectively just local instance declarations.
matchLocalInst pred loc
  = do { inerts@(IS { inert_cans = ics }) <- getTcSInerts
       ; case match_local_inst inerts (inert_insts ics) of
           ([], False) -> do { traceTcS "No local instance for" (ppr pred)
                             ; return NoInstance }
           ([(dfun_ev, inst_tys)], unifs)
             | not unifs
             -> do { let dfun_id = ctEvEvId dfun_ev
                   ; (tys, theta) <- instDFunType dfun_id inst_tys
                   ; let result = OneInst { cir_new_theta = theta
                                          , cir_mk_ev     = evDFunApp dfun_id tys
                                          , cir_what      = LocalInstance }
                   ; traceTcS "Local inst found:" (ppr result)
                   ; return result }
           _ -> do { traceTcS "Multiple local instances for" (ppr pred)
                   ; return NotSure }}
  where
    pred_tv_set = tyCoVarsOfType pred

    match_local_inst :: InertSet
                     -> [QCInst]
                     -> ( [(CtEvidence, [DFunInstType])]
                        , Bool )      -- True <=> Some unify but do not match
    match_local_inst _inerts []
      = ([], False)
    match_local_inst inerts (qci@(QCI { qci_tvs = qtvs, qci_pred = qpred
                               , qci_ev = ev })
                             : qcis)
      | let in_scope = mkInScopeSet (qtv_set `unionVarSet` pred_tv_set)
      , Just tv_subst <- ruleMatchTyKiX qtv_set (mkRnEnv2 in_scope)
                                        emptyTvSubstEnv qpred pred
      , let match = (ev, map (lookupVarEnv tv_subst) qtvs)
      = (match:matches, unif)

      | otherwise
      = ASSERT2( disjointVarSet qtv_set (tyCoVarsOfType pred)
               , ppr qci $$ ppr pred )
            -- ASSERT: unification relies on the
            -- quantified variables being fresh
        (matches, unif || this_unif)
      where
        qtv_set = mkVarSet qtvs
        this_unif = mightEqualLater inerts qpred (ctEvLoc ev) pred loc
        (matches, unif) = match_local_inst inerts qcis

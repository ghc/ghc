{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}

module GHC.Tc.Solver.Interact (
     solveSimpleGivens,   -- Solves [Ct]
     solveSimpleWanteds   -- Solves Cts
  ) where

import GHC.Prelude

import GHC.Tc.Solver.Canonical
import GHC.Tc.Solver.Dict
import GHC.Tc.Errors.Types
import GHC.Tc.Utils.TcType
import GHC.Tc.Instance.FunDeps
import GHC.Tc.Instance.Class ( safeOverlap )
import GHC.Tc.Types.Evidence
import GHC.Tc.Types
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.Origin
import GHC.Tc.Solver.Types
import GHC.Tc.Solver.InertSet
import GHC.Tc.Solver.Monad

import GHC.Core.InstEnv     ( Coherence(..) )
import GHC.Core.Class
import GHC.Core.Predicate
import GHC.Core.Coercion

import GHC.Builtin.Names ( ipClassKey )

import GHC.Types.Unique( hasKey )
import GHC.Types.Basic ( SwapFlag(..), IntWithInf, intGtLimit )

import GHC.Data.Bag

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Monad ( foldlM )
import GHC.Utils.Misc

import GHC.Driver.Session

import qualified GHC.LanguageExtensions as LangExt

import Data.List( deleteFirstsBy )
import Data.Function ( on )

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad

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
   Each stage returns a StopOrContinue and may have sideeffected
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
      = failTcS $ TcRnSimplifierTooManyIterations simples limit wc
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
solve_simple_wanteds (WC { wc_simple = simples1, wc_impl = implics1, wc_errors = errs })
  = nestTcS $
    do { solveSimples simples1
       ; (implics2, unsolved) <- getUnsolvedInerts
       ; return (WC { wc_simple = unsolved
                    , wc_impl   = implics1 `unionBags` implics2
                    , wc_errors = errs }) }

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
  = do { solvers <- getTcPluginSolvers
       ; if null solvers then return [] else
    do { givens <- getInertGivens
       ; if null givens then return [] else
    do { p <- runTcPluginSolvers solvers (givens,[])
       ; let (solved_givens, _) = pluginSolvedCts p
             insols             = pluginBadCts p
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
  = do { solvers <- getTcPluginSolvers
       ; if null solvers then return (False, wc) else

    do { given <- getInertGivens
       ; wanted <- zonkSimples simples1    -- Plugin requires zonked inputs
       ; p <- runTcPluginSolvers solvers (given, bagToList wanted)
       ; let (_, solved_wanted)   = pluginSolvedCts p
             (_, unsolved_wanted) = pluginInputCts p
             new_wanted                             = pluginNewCts p
             insols                                 = pluginBadCts p

-- SLPJ: I'm deeply suspicious of this
--       ; updInertCans (removeInertCts $ solved_givens)

       ; mapM_ setEv solved_wanted
       ; return ( notNull (pluginNewCts p)
                , wc { wc_simple = listToBag new_wanted       `andCts`
                                   listToBag unsolved_wanted  `andCts`
                                   listToBag insols } ) } }
  where
    setEv :: (EvTerm,Ct) -> TcS ()
    setEv (ev,ct) = case ctEvidence ct of
      CtWanted { ctev_dest = dest } -> setWantedEvTerm dest IsCoherent ev -- TODO: plugins should be able to signal non-coherence
      _ -> panic "runTcPluginsWanted.setEv: attempt to solve non-wanted!"

-- | A pair of (given, wanted) constraints to pass to plugins
type SplitCts  = ([Ct], [Ct])

-- | A solved pair of constraints, with evidence for wanteds
type SolvedCts = ([Ct], [(EvTerm,Ct)])

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

getTcPluginSolvers :: TcS [TcPluginSolver]
getTcPluginSolvers
  = do { tcg_env <- getGblEnv; return (tcg_tc_plugin_solvers tcg_env) }

-- | Starting from a pair of (given, wanted) constraints,
-- invoke each of the typechecker constraint-solving plugins in turn and return
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
runTcPluginSolvers :: [TcPluginSolver] -> SplitCts -> TcS TcPluginProgress
runTcPluginSolvers solvers all_cts
  = do { ev_binds_var <- getTcEvBindsVar
       ; foldM (do_plugin ev_binds_var) initialProgress solvers }
  where
    do_plugin :: EvBindsVar -> TcPluginProgress -> TcPluginSolver -> TcS TcPluginProgress
    do_plugin ev_binds_var p solver = do
        result <- runTcPluginTcS (uncurry (solver ev_binds_var) (pluginInputCts p))
        return $ progress p result

    progress :: TcPluginProgress -> TcPluginSolveResult -> TcPluginProgress
    progress p
      (TcPluginSolveResult
        { tcPluginInsolubleCts = bad_cts
        , tcPluginSolvedCts    = solved_cts
        , tcPluginNewCts       = new_cts
        }
      ) =
        p { pluginInputCts  = discard (bad_cts ++ map snd solved_cts) (pluginInputCts p)
          , pluginSolvedCts = add solved_cts (pluginSolvedCts p)
          , pluginNewCts    = new_cts ++ pluginNewCts p
          , pluginBadCts    = bad_cts ++ pluginBadCts p
          }

    initialProgress = TcPluginProgress all_cts ([], []) [] []

    discard :: [Ct] -> SplitCts -> SplitCts
    discard cts (xs, ys) =
        (xs `without` cts, ys `without` cts)

    without :: [Ct] -> [Ct] -> [Ct]
    without = deleteFirstsBy eqCt

    eqCt :: Ct -> Ct -> Bool
    eqCt c c' = ctFlavour c == ctFlavour c'
             && ctPred c `tcEqType` ctPred c'

    add :: [(EvTerm,Ct)] -> SolvedCts -> SolvedCts
    add xs scs = foldl' addOne scs xs

    addOne :: SolvedCts -> (EvTerm,Ct) -> SolvedCts
    addOne (givens, wanteds) (ev,ct) = case ctEvidence ct of
      CtGiven  {} -> (ct:givens, wanteds)
      CtWanted {} -> (givens, (ev,ct):wanteds)


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
   and the inert item is Wanted
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
             CIrredCan    {} -> interactIrred   ics wi
             CDictCan     {} -> interactDict    ics wi
             CEqCan       {} -> continueWith wi  -- "Canonicalisation" stage is
                                                 -- full solver for equalities
             _ -> pprPanic "interactWithInerts" (ppr wi) }
                -- CNonCanonical have been canonicalised

data InteractResult
   = KeepInert   -- Keep the inert item, and solve the work item from it
                 -- (if the latter is Wanted; just discard it if not)
   | KeepWork    -- Keep the work item, and solve the inert item from it

instance Outputable InteractResult where
  ppr KeepInert = text "keep inert"
  ppr KeepWork  = text "keep work-item"

solveOneFromTheOther :: Ct  -- Inert    (Dict or Irred)
                     -> Ct  -- WorkItem (same predicate as inert)
                     -> InteractResult
-- Precondition:
-- * inert and work item represent evidence for the /same/ predicate
-- * Both are CDictCan or CIrredCan
--
-- We can always solve one from the other: even if both are wanted,
-- although we don't rewrite wanteds with wanteds, we can combine
-- two wanteds into one by solving one from the other

solveOneFromTheOther ct_i ct_w
  | CtWanted { ctev_loc = loc_w } <- ev_w
  , prohibitedSuperClassSolve loc_i loc_w
  -- See Note [Solving superclass constraints] in GHC.Tc.TyCl.Instance
  = -- Inert must be Given
    KeepWork

  | CtWanted {} <- ev_w
  = -- Inert is Given or Wanted
    case ev_i of
      CtGiven {} -> KeepInert
        -- work is Wanted; inert is Given: easy choice.

      CtWanted {} -- Both are Wanted
        -- If only one has no pending superclasses, use it
        -- Otherwise we can get infinite superclass expansion (#22516)
        -- in silly cases like   class C T b => C a b where ...
        | not is_psc_i, is_psc_w     -> KeepInert
        | is_psc_i,     not is_psc_w -> KeepWork

        -- If only one is a WantedSuperclassOrigin (arising from expanding
        -- a Wanted class constraint), keep the other: wanted superclasses
        -- may be unexpected by users
        | not is_wsc_orig_i, is_wsc_orig_w     -> KeepInert
        | is_wsc_orig_i,     not is_wsc_orig_w -> KeepWork

        -- otherwise, just choose the lower span
        -- reason: if we have something like (abs 1) (where the
        -- Num constraint cannot be satisfied), it's better to
        -- get an error about abs than about 1.
        -- This test might become more elaborate if we see an
        -- opportunity to improve the error messages
        | ((<) `on` ctLocSpan) loc_i loc_w -> KeepInert
        | otherwise                        -> KeepWork

  -- From here on the work-item is Given

  | CtWanted { ctev_loc = loc_i } <- ev_i
  , prohibitedSuperClassSolve loc_w loc_i
  = KeepInert   -- Just discard the un-usable Given
                -- This never actually happens because
                -- Givens get processed first

  | CtWanted {} <- ev_i
  = KeepWork

  -- From here on both are Given
  -- See Note [Replacement vs keeping]

  | lvl_i == lvl_w
  = same_level_strategy

  | otherwise   -- Both are Given, levels differ
  = different_level_strategy
  where
     ev_i  = ctEvidence ct_i
     ev_w  = ctEvidence ct_w

     pred  = ctEvPred ev_i

     loc_i  = ctEvLoc ev_i
     loc_w  = ctEvLoc ev_w
     orig_i = ctLocOrigin loc_i
     orig_w = ctLocOrigin loc_w
     lvl_i  = ctLocLevel loc_i
     lvl_w  = ctLocLevel loc_w

     is_psc_w = isPendingScDict ct_w
     is_psc_i = isPendingScDict ct_i

     is_wsc_orig_i = isWantedSuperclassOrigin orig_i
     is_wsc_orig_w = isWantedSuperclassOrigin orig_w

     different_level_strategy  -- Both Given
       | isIPLikePred pred = if lvl_w > lvl_i then KeepWork  else KeepInert
       | otherwise         = if lvl_w > lvl_i then KeepInert else KeepWork
       -- See Note [Replacement vs keeping] part (1)
       -- For the isIPLikePred case see Note [Shadowing of Implicit Parameters]

     same_level_strategy -- Both Given
       = case (orig_i, orig_w) of

           (GivenSCOrigin _ depth_i blocked_i, GivenSCOrigin _ depth_w blocked_w)
             | blocked_i, not blocked_w -> KeepWork  -- Case 2(a) from
             | not blocked_i, blocked_w -> KeepInert -- Note [Replacement vs keeping]

             -- Both blocked or both not blocked

             | depth_w < depth_i -> KeepWork   -- Case 2(c) from
             | otherwise         -> KeepInert  -- Note [Replacement vs keeping]

           (GivenSCOrigin {}, _) -> KeepWork  -- Case 2(b) from Note [Replacement vs keeping]

           _ -> KeepInert  -- Case 2(d) from Note [Replacement vs keeping]

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

       (a) If both are GivenSCOrigin, choose the one that is unblocked if possible
           according to Note [Solving superclass constraints] in GHC.Tc.TyCl.Instance.

       (b) Prefer constraints that are not superclass selections. Example:

             f :: (Eq a, Ord a) => a -> Bool
             f x = x == x

           Eager superclass expansion gives us two [G] Eq a constraints. We
           want to keep the one from the user-written Eq a, not the superclass
           selection. This means we report the Ord a as redundant with
           -Wredundant-constraints, not the Eq a.

           Getting this wrong was #20602. See also
           Note [Tracking redundant constraints] in GHC.Tc.Solver.

       (c) If both are GivenSCOrigin, chooose the one with the shallower
           superclass-selection depth, in the hope of identifying more correct
           redundant constraints. This is really a generalization of point (b),
           because the superclass depth of a non-superclass constraint is 0.

           (If the levels differ, we definitely won't have both with GivenSCOrigin.)

       (d) Finally, when there is still a choice, use KeepInert rather than
           KeepWork, for two reasons:
             - to avoid unnecessary munging of the inert set.
             - to cut off superclass loops; see Note [Superclass loops] in GHC.Tc.Solver.Canonical

Doing the level-check for implicit parameters, rather than making the work item
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
Wrong!  The level-check ensures that the inner implicit parameter wins.
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

interactIrred inerts ct_w@(CIrredCan { cc_ev = ev_w, cc_reason = reason })
  | isInsolubleReason reason
               -- For insolubles, don't allow the constraint to be dropped
               -- which can happen with solveOneFromTheOther, so that
               -- we get distinct error messages with -fdefer-type-errors
  = continueWith ct_w

  | let (matching_irreds, others) = findMatchingIrreds (inert_irreds inerts) ev_w
  , ((ct_i, swap) : _rest) <- bagToList matching_irreds
        -- See Note [Multiple matching irreds]
  , let ev_i = ctEvidence ct_i
  = do { traceTcS "iteractIrred" $
         vcat [ text "wanted:" <+> (ppr ct_w $$ ppr (ctOrigin ct_w))
              , text "inert: " <+> (ppr ct_i $$ ppr (ctOrigin ct_i)) ]
       ; case solveOneFromTheOther ct_i ct_w of
            KeepInert -> do { setEvBindIfWanted ev_w IsCoherent (swap_me swap ev_i)
                            ; return (Stop ev_w (text "Irred equal:KeepInert" <+> ppr ct_w)) }
            KeepWork ->  do { setEvBindIfWanted ev_i IsCoherent (swap_me swap ev_w)
                            ; updInertIrreds (\_ -> others)
                            ; continueWith ct_w } }

  | otherwise
  = continueWith ct_w

  where
    swap_me :: SwapFlag -> CtEvidence -> EvTerm
    swap_me swap ev
      = case swap of
           NotSwapped -> ctEvTerm ev
           IsSwapped  -> evCoercion (mkSymCo (evTermCoercion (ctEvTerm ev)))

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
  state that allows try_solve_from_instance to augment the evidence
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

Note [No Given/Given fundeps]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We do not create constraints from:
* Given/Given interactions via functional dependencies or type family
  injectivity annotations.
* Given/instance fundep interactions via functional dependencies or
  type family injectivity annotations.

In this Note, all these interactions are called just "fundeps".

We ingore such fundeps for several reasons:

1. These fundeps will never serve a purpose in accepting more
   programs: Given constraints do not contain metavariables that could
   be unified via exploring fundeps. They *could* be useful in
   discovering inaccessible code. However, the constraints will be
   Wanteds, and as such will cause errors (not just warnings) if they
   go unsolved. Maybe there is a clever way to get the right
   inaccessible code warnings, but the path forward is far from
   clear. #12466 has further commentary.

2. Furthermore, here is a case where a Given/instance interaction is actively
   harmful (from dependent/should_compile/RaeJobTalk):

       type family a == b :: Bool
       type family Not a = r | r -> a where
         Not False = True
         Not True  = False

       [G] Not (a == b) ~ True

   Reacting this Given with the equations for Not produces

      [W] a == b ~ False

   This is indeed a true consequence, and would make sense as a fresh Given.
   But we don't have a way to produce evidence for fundeps, as a Wanted it
   is /harmful/: we can't prove it, and so we'll report an error and reject
   the program. (Previously fundeps gave rise to Deriveds, which
   carried no evidence, so it didn't matter that they could not be proved.)

3. #20922 showed a subtle different problem with Given/instance fundeps.
      type family ZipCons (as :: [k]) (bssx :: [[k]]) = (r :: [[k]]) | r -> as bssx where
        ZipCons (a ': as) (bs ': bss) = (a ': bs) ': ZipCons as bss
        ...

      tclevel = 4
      [G] ZipCons is1 iss ~ (i : is2) : jss

   (The tclevel=4 means that this Given is at level 4.)  The fundep tells us that
   'iss' must be of form (is2 : beta[4]) where beta[4] is a fresh unification
   variable; we don't know what type it stands for. So we would emit
      [W] iss ~ is2 : beta

   Again we can't prove that equality; and worse we'll rewrite iss to
   (is2:beta) in deeply nested constraints inside this implication,
   where beta is untouchable (under other equality constraints), leading
   to other insoluble constraints.

The bottom line: since we have no evidence for them, we should ignore Given/Given
and Given/instance fundeps entirely.
-}

interactDict :: InertCans -> Ct -> TcS (StopOrContinue Ct)
interactDict inerts ct_w@(CDictCan { cc_ev = ev_w, cc_class = cls, cc_tyargs = tys })
  | Just ct_i <- lookupInertDict inerts (ctEvLoc ev_w) cls tys
  , let ev_i  = ctEvidence ct_i
        loc_i = ctEvLoc ev_i
        loc_w = ctEvLoc ev_w
  = -- There is a matching dictionary in the inert set
    do { -- First to try to solve it /completely/ from top level instances
         -- See Note [Shortcut solving]
         dflags <- getDynFlags
       ; short_cut_worked <- shortCutSolver dflags ev_w ev_i
       ; if short_cut_worked
         then stopWith ev_w "interactDict/solved from instance"

         -- Next see if we are in "loopy-superclass" land.  If so,
         -- we don't want to replace the (Given) inert with the
         -- (Wanted) work-item, or vice versa; we want to hang on
         -- to both, and try to solve the work-item via an instance.
         -- See Note [Solving superclass constraints] in GHC.Tc.TyCl.Instance
         else if prohibitedSuperClassSolve loc_i loc_w
         then continueWith ct_w
         else
    do { -- The short-cut solver didn't fire, and loopy superclasses
         -- are dealt with, so we can either solve
         -- the inert from the work-item or vice-versa.
       ; case solveOneFromTheOther ct_i ct_w of
           KeepInert -> do { traceTcS "lookupInertDict:KeepInert" (ppr ct_w)
                           ; setEvBindIfWanted ev_w IsCoherent (ctEvTerm ev_i)
                           ; return $ Stop ev_w (text "Dict equal" <+> ppr ct_w) }
           KeepWork  -> do { traceTcS "lookupInertDict:KeepWork" (ppr ct_w)
                           ; setEvBindIfWanted ev_i IsCoherent (ctEvTerm ev_w)
                           ; updInertDicts $ \ ds -> delDict ds cls tys
                           ; continueWith ct_w } } }

  | cls `hasKey` ipClassKey
  , isGiven ev_w
  = interactGivenIP inerts ct_w

  | otherwise
  = do { addFunDepWork inerts ev_w cls
       ; continueWith ct_w  }

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
       ; ev_binds <- assertPpr (not (isCoEvBindsVar ev_binds_var )) (ppr ev_w) $
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
               OneInst { cir_new_theta   = preds
                       , cir_mk_ev       = mk_ev
                       , cir_coherence   = coherence
                       , cir_what        = what }
                 | safeOverlap what
                 , all isTyFamFree preds  -- Note [Shortcut solving: type families]
                 -> do { let solved_dicts' = addDict solved_dicts cls tys ev
                             -- solved_dicts': it is important that we add our goal
                             -- to the cache before we solve! Otherwise we may end
                             -- up in a loop while solving recursive dictionaries.

                       ; lift $ traceTcS "shortCutSolver: found instance" (ppr preds)
                       ; loc' <- lift $ checkInstanceOK loc what pred
                       ; lift $ checkReductionDepth loc' pred


                       ; evc_vs <- mapM (new_wanted_cached ev loc' solved_dicts') preds
                                  -- Emit work for subgoals but use our local cache
                                  -- so we can solve recursive dictionaries.

                       ; let ev_tm     = mk_ev (map getEvExpr evc_vs)
                             ev_binds' = extendEvBinds ev_binds $
                                         mkWantedEvBind (ctEvEvId ev) coherence ev_tm

                       ; foldlM try_solve_from_instance
                                (ev_binds', solved_dicts')
                                (freshGoals evc_vs) }

               _ -> mzero }
      | otherwise = mzero


    -- Use a local cache of solved dicts while emitting EvVars for new work
    -- We bail out of the entire computation if we need to emit an EvVar for
    -- a subgoal that isn't a ClassPred.
    new_wanted_cached :: CtEvidence -> CtLoc
                      -> DictMap CtEvidence -> TcPredType -> MaybeT TcS MaybeNew
    new_wanted_cached ev_w loc cache pty
      | ClassPred cls tys <- classifyPredType pty
      = lift $ case findDict cache loc_w cls tys of
          Just ctev -> return $ Cached (ctEvExpr ctev)
          Nothing   -> Fresh <$> newWantedNC loc (ctEvRewriters ev_w) pty
      | otherwise = mzero

addFunDepWork :: InertCans -> CtEvidence -> Class -> TcS ()
-- Add wanted constraints from type-class functional dependencies.
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
      = do { traceTcS "addFunDepWork" (vcat
                [ ppr work_ev
                , pprCtLoc work_loc, ppr (isGivenLoc work_loc)
                , pprCtLoc inert_loc, ppr (isGivenLoc inert_loc)
                , pprCtLoc derived_loc, ppr (isGivenLoc derived_loc) ])

           ; unless (isGiven work_ev && isGiven inert_ev) $
             emitFunDepWanteds (ctEvRewriters work_ev) $
             improveFromAnother (derived_loc, inert_rewriters) inert_pred work_pred
               -- We don't really rewrite tys2, see below _rewritten_tys2, so that's ok
               -- Do not create FDs from Given/Given interactions: See Note [No Given/Given fundeps]
        }
      where
        inert_ev   = ctEvidence inert_ct
        inert_pred = ctEvPred inert_ev
        inert_loc  = ctEvLoc inert_ev
        inert_rewriters = ctRewriters inert_ct
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
                       The top-reaction Stage
*                                                                    *
**********************************************************************
-}

topReactionsStage :: WorkItem -> TcS (StopOrContinue Ct)
-- The work item does not react with the inert set,
-- so try interaction with top-level instances.
topReactionsStage work_item
  = do { traceTcS "doTopReact" (ppr work_item)
       ; case work_item of

           CDictCan {} ->
             do { inerts <- getTcSInerts
                ; doTopReactDict inerts work_item }

           CEqCan {} -> continueWith work_item  -- "Canonicalisation" stage is
                                                -- full solver for equalities

           CIrredCan {} ->
             doTopReactOther work_item

           -- Any other work item does not react with any top-level equations
           _  -> continueWith work_item }

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

  | otherwise
  = do { res <- matchLocalInst pred loc
       ; case res of
           OneInst {} -> chooseInstance work_item res
           _          -> continueWith work_item }

  where
    ev   = ctEvidence work_item
    loc  = ctEvLoc ev
    pred = ctEvPred ev


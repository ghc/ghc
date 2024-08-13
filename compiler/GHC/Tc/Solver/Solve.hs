{-# LANGUAGE RecursiveDo #-}

module GHC.Tc.Solver.Solve (
     solveSimpleGivens,   -- Solves [Ct]
     solveSimpleWanteds   -- Solves Cts
  ) where

import GHC.Prelude

import GHC.Tc.Solver.Dict
import GHC.Tc.Solver.Equality( solveEquality )
import GHC.Tc.Solver.Irred( solveIrred )
import GHC.Tc.Solver.Rewrite( rewrite )
import GHC.Tc.Errors.Types
import GHC.Tc.Utils.TcType
import GHC.Tc.Types.Evidence
import GHC.Tc.Types
import GHC.Tc.Types.Origin
import GHC.Tc.Types.Constraint
import GHC.Tc.Solver.InertSet
import GHC.Tc.Solver.Monad

import GHC.Core.Predicate
import GHC.Core.Reduction
import GHC.Core.Coercion
import GHC.Core.Class( classHasSCs )

import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Types.Basic ( IntWithInf, intGtLimit )

import GHC.Data.Bag

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc

import GHC.Driver.Session

import Data.List( deleteFirstsBy )

import Control.Monad
import Data.Semigroup as S
import Data.Void( Void )

{-
**********************************************************************
*                                                                    *
*                      Main Solver                                   *
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
    -- See Note [The solveSimpleWanteds loop]
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

{-
************************************************************************
*                                                                      *
           Solving flat constraints: solveSimples
*                                                                      *
********************************************************************* -}

-- The main solver loop implements Note [Basic Simplifier Plan]
---------------------------------------------------------------
solveSimples :: Cts -> TcS ()
-- Returns the final InertSet in TcS
-- Has no effect on work-list or residual-implications
-- The constraints are initially examined in left-to-right order

solveSimples cts
  = {-# SCC "solveSimples" #-}
    do { emitWork cts; solve_loop }
  where
    solve_loop
      = {-# SCC "solve_loop" #-}
        do { sel <- selectNextWorkItem
           ; case sel of
              Nothing -> return ()
              Just ct -> do { solveOne ct
                            ; solve_loop } }

solveOne :: Ct -> TcS ()  -- Solve one constraint
solveOne workItem
  = do { wl      <- getWorkList
       ; inerts  <- getInertSet
       ; tclevel <- getTcLevel
       ; traceTcS "----------------------------- " empty
       ; traceTcS "Start solver pipeline {" $
                  vcat [ text "tclevel =" <+> ppr tclevel
                       , text "work item =" <+> ppr workItem
                       , text "inerts =" <+> ppr inerts
                       , text "rest of worklist =" <+> ppr wl ]

       ; bumpStepCountTcS    -- One step for each constraint processed
       ; solve workItem }
  where
    solve :: Ct -> TcS ()
    solve ct
      = do { traceTcS "solve {" (text "workitem = " <+> ppr ct)
           ; res <- runSolverStage (solveCt ct)
           ; traceTcS "end solve }" (ppr res)
           ; case res of
               StartAgain ct -> do { traceTcS "Go round again" (ppr ct)
                                   ; solve ct }

               Stop ev s -> do { traceFireTcS ev s
                               ; traceTcS "End solver pipeline }" empty
                               ; return () }

               -- ContinueWith can't happen: res :: SolverStage Void
               -- solveCt either solves the constraint, or puts
               -- the unsolved constraint in the inert set.
            }

{- *********************************************************************
*                                                                      *
*              Solving one constraint: solveCt
*                                                                      *
************************************************************************

Note [Canonicalization]
~~~~~~~~~~~~~~~~~~~~~~~
Canonicalization converts a simple constraint to a canonical form. It is
unary (i.e. treats individual constraints one at a time).

Constraints originating from user-written code come into being as
CNonCanonicals. We know nothing about these constraints. So, first:

     Classify CNonCanoncal constraints, depending on whether they
     are equalities, class predicates, or other.

Then proceed depending on the shape of the constraint. Generally speaking,
each constraint gets rewritten and then decomposed into one of several forms
(see type Ct in GHC.Tc.Types).

When an already-canonicalized constraint gets kicked out of the inert set,
it must be recanonicalized. But we know a bit about its shape from the
last time through, so we can skip the classification step.
-}

solveCt :: Ct -> SolverStage Void
-- The Void result tells us that solveCt cannot return
-- a ContinueWith; it must return Stop or StartAgain.
solveCt (CNonCanonical ev)                   = solveNC ev
solveCt (CIrredCan (IrredCt { ir_ev = ev })) = solveNC ev

solveCt (CEqCan (EqCt { eq_ev = ev, eq_eq_rel = eq_rel
                           , eq_lhs = lhs, eq_rhs = rhs }))
  = solveEquality ev eq_rel (canEqLHSType lhs) rhs

solveCt (CQuantCan (QCI { qci_ev = ev, qci_pend_sc = pend_sc }))
  = do { ev <- rewriteEvidence ev
         -- It is (much) easier to rewrite and re-classify than to
         -- rewrite the pieces and build a Reduction that will rewrite
         -- the whole constraint
       ; case classifyPredType (ctEvPred ev) of
           ForAllPred tvs th p -> Stage $ solveForAll ev tvs th p pend_sc
           _ -> pprPanic "SolveCt" (ppr ev) }

solveCt (CDictCan (DictCt { di_ev = ev, di_pend_sc = pend_sc }))
  = do { ev <- rewriteEvidence ev
         -- It is easier to rewrite and re-classify than to rewrite
         -- the pieces and build a Reduction that will rewrite the
         -- whole constraint
       ; case classifyPredType (ctEvPred ev) of
           ClassPred cls tys
             -> solveDict (DictCt { di_ev = ev, di_cls = cls
                                  , di_tys = tys, di_pend_sc = pend_sc })
           _ -> pprPanic "solveCt" (ppr ev) }

------------------
solveNC :: CtEvidence -> SolverStage Void
solveNC ev
  = -- Instead of rewriting the evidence before classifying, it's possible we
    -- can make progress without the rewrite. Try this first.
    -- For insolubles (all of which are equalities), do /not/ rewrite the arguments
    -- In #14350 doing so led entire-unnecessary and ridiculously large
    -- type function expansion.  Instead, canEqNC just applies
    -- the substitution to the predicate, and may do decomposition;
    --    e.g. a ~ [a], where [G] a ~ [Int], can decompose
    case classifyPredType (ctEvPred ev) of {
        EqPred eq_rel ty1 ty2 -> solveEquality ev eq_rel ty1 ty2 ;
        _ ->

    -- Do rewriting on the constraint, especially zonking
    do { ev <- rewriteEvidence ev
       ; let irred = IrredCt { ir_ev = ev, ir_reason = IrredShapeReason }

    -- And then re-classify
       ; case classifyPredType (ctEvPred ev) of
           ClassPred cls tys     -> solveDictNC ev cls tys
           ForAllPred tvs th p   -> Stage $ solveForAllNC ev tvs th p
           IrredPred {}          -> solveIrred irred
           EqPred eq_rel ty1 ty2 -> solveEquality ev eq_rel ty1 ty2
              -- This case only happens if (say) `c` is unified with `a ~# b`,
              -- but that is rare becuase it requires c :: CONSTRAINT UnliftedRep

    }}


{- *********************************************************************
*                                                                      *
*                      Quantified constraints
*                                                                      *
********************************************************************* -}

{- Note [Quantified constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The -XQuantifiedConstraints extension allows type-class contexts like this:

  data Rose f x = Rose x (f (Rose f x))

  instance (Eq a, forall b. Eq b => Eq (f b))
        => Eq (Rose f a)  where
    (Rose x1 rs1) == (Rose x2 rs2) = x1==x2 && rs1 == rs2

Note the (forall b. Eq b => Eq (f b)) in the instance contexts.
This quantified constraint is needed to solve the
 [W] (Eq (f (Rose f x)))
constraint which arises form the (==) definition.

The wiki page is
  https://gitlab.haskell.org/ghc/ghc/wikis/quantified-constraints
which in turn contains a link to the GHC Proposal where the change
is specified, and a Haskell Symposium paper about it.

We implement two main extensions to the design in the paper:

 1. We allow a variable in the instance head, e.g.
      f :: forall m a. (forall b. m b) => D (m a)
    Notice the 'm' in the head of the quantified constraint, not
    a class.

 2. We support superclasses to quantified constraints.
    For example (contrived):
      f :: (Ord b, forall b. Ord b => Ord (m b)) => m a -> m a -> Bool
      f x y = x==y
    Here we need (Eq (m a)); but the quantified constraint deals only
    with Ord.  But we can make it work by using its superclass.

Here are the moving parts
  * Language extension {-# LANGUAGE QuantifiedConstraints #-}
    and add it to ghc-boot-th:GHC.LanguageExtensions.Type.Extension

  * A new form of evidence, EvDFun, that is used to discharge
    such wanted constraints

  * checkValidType gets some changes to accept forall-constraints
    only in the right places.

  * Predicate.Pred gets a new constructor ForAllPred, and
    and classifyPredType analyses a PredType to decompose
    the new forall-constraints

  * GHC.Tc.Solver.Monad.InertCans gets an extra field, inert_insts,
    which holds all the Given forall-constraints.  In effect,
    such Given constraints are like local instance decls.

  * When trying to solve a class constraint, via
    GHC.Tc.Solver.Instance.Class.matchInstEnv, use the InstEnv from inert_insts
    so that we include the local Given forall-constraints
    in the lookup.  (See GHC.Tc.Solver.Monad.getInstEnvs.)

  * `solveForAll` deals with solving a forall-constraint.  See
       Note [Solving a Wanted forall-constraint]

  * We augment the kick-out code to kick out an inert
    forall constraint if it can be rewritten by a new
    type equality; see GHC.Tc.Solver.Monad.kick_out_rewritable

Note that a quantified constraint is never /inferred/
(by GHC.Tc.Solver.simplifyInfer).  A function can only have a
quantified constraint in its type if it is given an explicit
type signature.

-}

solveForAllNC :: CtEvidence -> [TcTyVar] -> TcThetaType -> TcPredType
              -> TcS (StopOrContinue Void)
-- NC: this came from CNonCanonical, so we have not yet expanded superclasses
-- Precondition: already rewritten by inert set
solveForAllNC ev tvs theta pred
  | isGiven ev  -- See Note [Eagerly expand given superclasses]
  , Just (cls, tys) <- cls_pred_tys_maybe
  = do { dflags <- getDynFlags
       ; sc_cts <- mkStrictSuperClasses (givensFuel dflags) ev tvs theta cls tys
       -- givensFuel dflags: See Note [Expanding Recursive Superclasses and ExpansionFuel]
       ; emitWork (listToBag sc_cts)
       ; solveForAll ev tvs theta pred doNotExpand }
       -- doNotExpand: as we have already (eagerly) expanded superclasses for this class

  | otherwise
  = do { dflags <- getDynFlags
       ; let fuel | Just (cls, _) <- cls_pred_tys_maybe
                  , classHasSCs cls = qcsFuel dflags
                  -- See invariants (a) and (b) in QCI.qci_pend_sc
                  -- qcsFuel dflags: See Note [Expanding Recursive Superclasses and ExpansionFuel]
                  -- See Note [Quantified constraints]
                  | otherwise = doNotExpand
       ; solveForAll ev tvs theta pred fuel }
  where
    cls_pred_tys_maybe = getClassPredTys_maybe pred

solveForAll :: CtEvidence -> [TcTyVar] -> TcThetaType -> PredType -> ExpansionFuel
            -> TcS (StopOrContinue Void)
-- Precondition: already rewritten by inert set
solveForAll ev@(CtWanted { ctev_dest = dest, ctev_rewriters = rewriters, ctev_loc = loc })
            tvs theta pred _fuel
  = -- See Note [Solving a Wanted forall-constraint]
    setSrcSpan (getCtLocEnvLoc $ ctLocEnv loc) $
    -- This setSrcSpan is important: the emitImplicationTcS uses that
    -- TcLclEnv for the implication, and that in turn sets the location
    -- for the Givens when solving the constraint (#21006)
    do { let empty_subst = mkEmptySubst $ mkInScopeSet $
                           tyCoVarsOfTypes (pred:theta) `delVarSetList` tvs
             is_qc = IsQC (ctLocOrigin loc)

         -- rec {..}: see Note [Keeping SkolemInfo inside a SkolemTv]
         --           in GHC.Tc.Utils.TcType
         -- Very like the code in tcSkolDFunType
       ; rec { skol_info <- mkSkolemInfo skol_info_anon
             ; (subst, skol_tvs) <- tcInstSkolTyVarsX skol_info empty_subst tvs
             ; let inst_pred  = substTy    subst pred
                   inst_theta = substTheta subst theta
                   skol_info_anon = InstSkol is_qc (get_size inst_pred) }

       ; given_ev_vars <- mapM newEvVar inst_theta
       ; (lvl, (w_id, wanteds))
             <- pushLevelNoWorkList (ppr skol_info) $
                do { let loc' = setCtLocOrigin loc (ScOrigin is_qc NakedSc)
                         -- Set the thing to prove to have a ScOrigin, so we are
                         -- careful about its termination checks.
                         -- See (QC-INV) in Note [Solving a Wanted forall-constraint]
                   ; wanted_ev <- newWantedEvVarNC loc' rewriters inst_pred
                   ; return ( ctEvEvId wanted_ev
                            , unitBag (mkNonCanonical wanted_ev)) }

      ; ev_binds <- emitImplicationTcS lvl skol_info_anon skol_tvs given_ev_vars wanteds

      ; setWantedEvTerm dest EvCanonical $
        EvFun { et_tvs = skol_tvs, et_given = given_ev_vars
              , et_binds = ev_binds, et_body = w_id }

      ; stopWith ev "Wanted forall-constraint" }
  where
    -- Getting the size of the head is a bit horrible
    -- because of the special treament for class predicates
    get_size pred = case classifyPredType pred of
                      ClassPred cls tys -> pSizeClassPred cls tys
                      _                 -> pSizeType pred

 -- See Note [Solving a Given forall-constraint]
solveForAll ev@(CtGiven {}) tvs _theta pred fuel
  = do { addInertForAll qci
       ; stopWith ev "Given forall-constraint" }
  where
    qci = QCI { qci_ev = ev, qci_tvs = tvs
              , qci_pred = pred, qci_pend_sc = fuel }

{- Note [Solving a Wanted forall-constraint]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Solving a wanted forall (quantified) constraint
  [W] df :: forall ab. (Eq a, Ord b) => C x a b
is delightfully easy.   Just build an implication constraint
    forall ab. (g1::Eq a, g2::Ord b) => [W] d :: C x a
and discharge df thus:
    df = /\ab. \g1 g2. let <binds> in d
where <binds> is filled in by solving the implication constraint.
All the machinery is to hand; there is little to do.

The tricky point is about termination: see #19690.  We want to maintain
the invariant (QC-INV):

  (QC-INV) Every quantified constraint returns a non-bottom dictionary

just as every top-level instance declaration guarantees to return a non-bottom
dictionary.  But as #19690 shows, it is possible to get a bottom dicionary
by superclass selection if we aren't careful.  The situation is very similar
to that described in Note [Recursive superclasses] in GHC.Tc.TyCl.Instance;
and we use the same solution:

* Give the Givens a CtOrigin of (GivenOrigin (InstSkol IsQC head_size))
* Give the Wanted a CtOrigin of (ScOrigin IsQC NakedSc)

Both of these things are done in solveForAll.  Now the mechanism described
in Note [Solving superclass constraints] in GHC.Tc.TyCl.Instance takes over.

Note [Solving a Given forall-constraint]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For a Given constraint
  [G] df :: forall ab. (Eq a, Ord b) => C x a b
we just add it to TcS's local InstEnv of known instances,
via addInertForall.  Then, if we look up (C x Int Bool), say,
we'll find a match in the InstEnv.


************************************************************************
*                                                                      *
                  Evidence transformation
*                                                                      *
************************************************************************
-}

rewriteEvidence :: CtEvidence -> SolverStage CtEvidence
-- (rewriteEvidence old_ev new_pred co do_next)
-- Main purpose: create new evidence for new_pred;
--                 unless new_pred is cached already
-- * Calls do_next with (new_ev :: new_pred), with same wanted/given flag as old_ev
-- * If old_ev was wanted, create a binding for old_ev, in terms of new_ev
-- * If old_ev was given, AND not cached, create a binding for new_ev, in terms of old_ev
-- * Stops if new_ev is already cached
--
--        Old evidence    New predicate is               Return new evidence
--        flavour                                        of same flavor
--        -------------------------------------------------------------------
--        Wanted          Already solved or in inert     Stop
--                        Not                            do_next new_evidence
--
--        Given           Already in inert               Stop
--                        Not                            do_next new_evidence

{- Note [Rewriting with Refl]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If the coercion is just reflexivity then you may re-use the same
evidence variable.  But be careful!  Although the coercion is Refl, new_pred
may reflect the result of unification alpha := ty, so new_pred might
not _look_ the same as old_pred, and it's vital to proceed from now on
using new_pred.

The rewriter preserves type synonyms, so they should appear in new_pred
as well as in old_pred; that is important for good error messages.

If we are rewriting with Refl, then there are no new rewriters to add to
the rewriter set. We check this with an assertion.
 -}


rewriteEvidence ev
  = Stage $ do { traceTcS "rewriteEvidence" (ppr ev)
               ; (redn, rewriters) <- rewrite ev (ctEvPred ev)
               ; finish_rewrite ev redn rewriters }

finish_rewrite :: CtEvidence   -- ^ old evidence
               -> Reduction    -- ^ new predicate + coercion, of type <type of old evidence> ~ new predicate
               -> RewriterSet  -- ^ See Note [Wanteds rewrite Wanteds]
                               -- in GHC.Tc.Types.Constraint
               -> TcS (StopOrContinue CtEvidence)
finish_rewrite old_ev (Reduction co new_pred) rewriters
  | isReflCo co -- See Note [Rewriting with Refl]
  = assert (isEmptyRewriterSet rewriters) $
    continueWith (setCtEvPredType old_ev new_pred)

finish_rewrite ev@(CtGiven { ctev_evar = old_evar, ctev_loc = loc })
                (Reduction co new_pred) rewriters
  = assert (isEmptyRewriterSet rewriters) $ -- this is a Given, not a wanted
    do { new_ev <- newGivenEvVar loc (new_pred, new_tm)
       ; continueWith new_ev }
  where
    -- mkEvCast optimises ReflCo
    ev_rw_role = ctEvRewriteRole ev
    new_tm = assert (coercionRole co == ev_rw_role)
             mkEvCast (evId old_evar)
                (downgradeRole Representational ev_rw_role co)

finish_rewrite ev@(CtWanted { ctev_dest = dest
                             , ctev_loc = loc
                             , ctev_rewriters = rewriters })
                (Reduction co new_pred) new_rewriters
  = do { mb_new_ev <- newWanted loc rewriters' new_pred
       ; let ev_rw_role = ctEvRewriteRole ev
       ; massert (coercionRole co == ev_rw_role)
       ; setWantedEvTerm dest EvCanonical $
            mkEvCast (getEvExpr mb_new_ev)
                     (downgradeRole Representational ev_rw_role (mkSymCo co))
       ; case mb_new_ev of
            Fresh  new_ev -> continueWith new_ev
            Cached _      -> stopWith ev "Cached wanted" }
  where
    rewriters' = rewriters S.<> new_rewriters

{- *******************************************************************
*                                                                    *
*                      Typechecker plugins
*                                                                    *
******************************************************************* -}

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
    do { traceTcS "runTcPluginsGiven {" (ppr givens)
       ; p <- runTcPluginSolvers solvers (givens,[])
       ; let (solved_givens, _) = pluginSolvedCts p
             insols             = map (ctIrredCt PluginReason) (pluginBadCts p)
       ; updInertCans (removeInertCts solved_givens .
                       updIrreds (addIrreds insols) )
       ; traceTcS "runTcPluginsGiven }" $
         vcat [ text "solved_givens:" <+> ppr solved_givens
              , text "insols:" <+> ppr insols
              , text "new:" <+> ppr (pluginNewCts p) ]
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

       ; traceTcS "Running plugins (" (vcat [ text "Given:" <+> ppr given
                                            , text "Watned:" <+> ppr wanted ])
       ; p <- runTcPluginSolvers solvers (given, bagToList wanted)
       ; let (_, solved_wanted)   = pluginSolvedCts p
             (_, unsolved_wanted) = pluginInputCts p
             new_wanted     = pluginNewCts p
             insols         = pluginBadCts p
             all_new_wanted = listToBag new_wanted       `andCts`
                              listToBag unsolved_wanted  `andCts`
                              listToBag insols

-- SLPJ: I'm deeply suspicious of this
--       ; updInertCans (removeInertCts $ solved_givens)

       ; mapM_ setEv solved_wanted

       ; traceTcS "Finished plugins }" (ppr new_wanted)
       ; return ( notNull (pluginNewCts p)
                , wc { wc_simple = all_new_wanted } ) } }
  where
    setEv :: (EvTerm,Ct) -> TcS ()
    setEv (ev,ct) = case ctEvidence ct of
      CtWanted { ctev_dest = dest } -> setWantedEvTerm dest EvCanonical ev
           -- TODO: plugins should be able to signal non-canonicity
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
    without = deleteFirstsBy eq_ct

    eq_ct :: Ct -> Ct -> Bool
    eq_ct c c' = ctFlavour c == ctFlavour c'
              && ctPred c `tcEqType` ctPred c'

    add :: [(EvTerm,Ct)] -> SolvedCts -> SolvedCts
    add xs scs = foldl' addOne scs xs

    addOne :: SolvedCts -> (EvTerm,Ct) -> SolvedCts
    addOne (givens, wanteds) (ev,ct) = case ctEvidence ct of
      CtGiven  {} -> (ct:givens, wanteds)
      CtWanted {} -> (givens, (ev,ct):wanteds)



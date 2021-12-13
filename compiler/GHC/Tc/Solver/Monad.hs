{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- | Monadic definitions for the constraint solver
module GHC.Tc.Solver.Monad (

    -- The TcS monad
    TcS, runTcS, runTcSDeriveds, runTcSDerivedsEarlyAbort, runTcSWithEvBinds,
    runTcSInerts, failTcS, warnTcS, addErrTcS, wrapTcS, runTcSEqualities,
    nestTcS, nestImplicTcS, setEvBindsTcS,
    emitImplicationTcS, emitTvImplicationTcS,

    selectNextWorkItem,
    getWorkList,
    updWorkListTcS,
    pushLevelNoWorkList,

    runTcPluginTcS, addUsedGRE, addUsedGREs, keepAlive,
    matchGlobalInst, TcM.ClsInstResult(..),

    QCInst(..),

    -- Tracing etc
    panicTcS, traceTcS,
    traceFireTcS, bumpStepCountTcS, csTraceTcS,
    wrapErrTcS, wrapWarnTcS,
    resetUnificationFlag, setUnificationFlag,

    -- Evidence creation and transformation
    MaybeNew(..), freshGoals, isFresh, getEvExpr,

    newTcEvBinds, newNoTcEvBinds,
    newWantedEq, newWantedEq_SI, emitNewWantedEq,
    newWanted, newWanted_SI, newWantedEvVar,
    newWantedNC, newWantedEvVarNC,
    newDerivedNC,
    newBoundEvVarId,
    unifyTyVar, reportUnifications, touchabilityTest, TouchabilityTestResult(..),
    setEvBind, setWantedEq,
    setWantedEvTerm, setEvBindIfWanted,
    newEvVar, newGivenEvVar, newGivenEvVars,
    emitNewDeriveds, emitNewDerivedEq,
    checkReductionDepth,
    getSolvedDicts, setSolvedDicts,

    getInstEnvs, getFamInstEnvs,                -- Getting the environments
    getTopEnv, getGblEnv, getLclEnv,
    getTcEvBindsVar, getTcLevel,
    getTcEvTyCoVars, getTcEvBindsMap, setTcEvBindsMap,
    tcLookupClass, tcLookupId,

    -- Inerts
    updInertTcS, updInertCans, updInertDicts, updInertIrreds,
    getHasGivenEqs, setInertCans,
    getInertEqs, getInertCans, getInertGivens,
    getInertInsols, getInnermostGivenEqLevel,
    getTcSInerts, setTcSInerts,
    getUnsolvedInerts,
    removeInertCts, getPendingGivenScs,
    addInertCan, insertFunEq, addInertForAll,
    emitWorkNC, emitWork,
    isImprovable,
    lookupInertDict,

    -- The Model
    kickOutAfterUnification,

    -- Inert Safe Haskell safe-overlap failures
    addInertSafehask, insertSafeOverlapFailureTcS, updInertSafehask,
    getSafeOverlapFailures,

    -- Inert solved dictionaries
    addSolvedDict, lookupSolvedDict,

    -- Irreds
    foldIrreds,

    -- The family application cache
    lookupFamAppInert, lookupFamAppCache, extendFamAppCache,
    pprKicked,

    instDFunType,                              -- Instantiation

    -- MetaTyVars
    newFlexiTcSTy, instFlexi, instFlexiX,
    cloneMetaTyVar,
    tcInstSkolTyVarsX,

    TcLevel,
    isFilledMetaTyVar_maybe, isFilledMetaTyVar,
    zonkTyCoVarsAndFV, zonkTcType, zonkTcTypes, zonkTcTyVar, zonkCo,
    zonkTyCoVarsAndFVList,
    zonkSimples, zonkWC,
    zonkTyCoVarKind,

    -- References
    newTcRef, readTcRef, writeTcRef, updTcRef,

    -- Misc
    getDefaultInfo, getDynFlags, getGlobalRdrEnvTcS,
    matchFam, matchFamTcM,
    checkWellStagedDFun,
    pprEq,                                   -- Smaller utils, re-exported from TcM
                                             -- TODO (DV): these are only really used in the
                                             -- instance matcher in GHC.Tc.Solver. I am wondering
                                             -- if the whole instance matcher simply belongs
                                             -- here

    breakTyVarCycle_maybe, rewriterView
) where

import GHC.Prelude

import GHC.Driver.Env

import qualified GHC.Tc.Utils.Instantiate as TcM
import GHC.Core.InstEnv
import GHC.Tc.Instance.Family as FamInst
import GHC.Core.FamInstEnv

import qualified GHC.Tc.Utils.Monad    as TcM
import qualified GHC.Tc.Utils.TcMType  as TcM
import qualified GHC.Tc.Instance.Class as TcM( matchGlobalInst, ClsInstResult(..) )
import qualified GHC.Tc.Utils.Env      as TcM
       ( checkWellStaged, tcGetDefaultTys, tcLookupClass, tcLookupId, topIdLvl )
import GHC.Tc.Instance.Class( InstanceWhat(..), safeOverlap, instanceReturnsDictCon )
import GHC.Tc.Utils.TcType
import GHC.Driver.Session
import GHC.Core.Type
import qualified GHC.Core.TyCo.Rep as Rep  -- this needs to be used only very locally
import GHC.Core.Coercion
import GHC.Core.Reduction

import GHC.Tc.Solver.Types
import GHC.Tc.Solver.InertSet

import GHC.Tc.Types.Evidence
import GHC.Core.Class
import GHC.Core.TyCon
import GHC.Tc.Errors   ( solverDepthErrorTcS )
import GHC.Tc.Errors.Types

import GHC.Types.Name
import GHC.Types.TyThing
import GHC.Unit.Module ( HasModule, getModule )
import GHC.Types.Name.Reader ( GlobalRdrEnv, GlobalRdrElt )
import qualified GHC.Rename.Env as TcM
import GHC.Types.Var
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Logger
import GHC.Utils.Misc (HasDebugCallStack)
import GHC.Data.Bag as Bag
import GHC.Types.Unique.Supply
import GHC.Tc.Types
import GHC.Tc.Types.Origin
import GHC.Tc.Types.Constraint
import GHC.Tc.Utils.Unify
import GHC.Core.Predicate

import GHC.Types.Unique.Set

import Control.Monad
import GHC.Utils.Monad
import Data.IORef
import GHC.Exts (oneShot)
import Data.List ( mapAccumL, partition )
import Data.List.NonEmpty ( NonEmpty(..) )

#if defined(DEBUG)
import GHC.Data.Graph.Directed
#endif

{- *********************************************************************
*                                                                      *
             Shadow constraints and improvement
*                                                                      *
************************************************************************

Note [The improvement story and derived shadows]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Because Wanteds cannot rewrite Wanteds (see Note [Wanteds do not
rewrite Wanteds] in GHC.Tc.Types.Constraint), we may miss some opportunities for
solving.  Here's a classic example (indexed-types/should_fail/T4093a)

    Ambiguity check for f: (Foo e ~ Maybe e) => Foo e

    We get [G] Foo e ~ Maybe e    (CEqCan)
           [W] Foo ee ~ Foo e     (CEqCan)       -- ee is a unification variable
           [W] Foo ee ~ Maybe ee  (CEqCan)

    The first Wanted gets rewritten to

           [W] Foo ee ~ Maybe e

    But now we appear to be stuck, since we don't rewrite Wanteds with
    Wanteds.  This is silly because we can see that ee := e is the
    only solution.

The basic plan is
  * generate Derived constraints that shadow Wanted constraints
  * allow Derived to rewrite Derived
  * in order to cause some unifications to take place
  * that in turn solve the original Wanteds

The ONLY reason for all these Derived equalities is to tell us how to
unify a variable: that is, what Mark Jones calls "improvement".

The same idea is sometimes also called "saturation"; find all the
equalities that must hold in any solution.

Or, equivalently, you can think of the derived shadows as implementing
the "model": a non-idempotent but no-occurs-check substitution,
reflecting *all* *Nominal* equalities (a ~N ty) that are not
immediately soluble by unification.

More specifically, here's how it works (Oct 16):

* Wanted constraints are born as [WD]; this behaves like a
  [W] and a [D] paired together.

* When we are about to add a [WD] to the inert set, if it can
  be rewritten by a [D] a ~ ty, then we split it into [W] and [D],
  putting the latter into the work list (see maybeEmitShadow).

In the example above, we get to the point where we are stuck:
    [WD] Foo ee ~ Foo e
    [WD] Foo ee ~ Maybe ee

But now when [WD] Foo ee ~ Maybe ee is about to be added, we'll
split it into [W] and [D], since the inert [WD] Foo ee ~ Foo e
can rewrite it.  Then:
    work item: [D] Foo ee ~ Maybe ee
    inert:     [W] Foo ee ~ Maybe ee
               [WD] Foo ee ~ Maybe e

See Note [Splitting WD constraints].  Now the work item is rewritten
by the [WD] and we soon get ee := e.

Additional notes:

  * The derived shadow equalities live in inert_eqs, along with
    the Givens and Wanteds; see Note [EqualCtList invariants]
    in GHC.Tc.Solver.Types.

  * We make Derived shadows only for Wanteds, not Givens.  So we
    have only [G], not [GD] and [G] plus splitting.  See
    Note [Add derived shadows only for Wanteds]

  * We also get Derived equalities from functional dependencies
    and type-function injectivity; see calls to unifyDerived.

  * It's worth having [WD] rather than just [W] and [D] because
    * efficiency: silly to process the same thing twice
    * inert_dicts is a finite map keyed by
      the type; it's inconvenient for it to map to TWO constraints

Another example requiring Deriveds is in
Note [Put touchable variables on the left] in GHC.Tc.Solver.Canonical.

Note [Splitting WD constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We are about to add a [WD] constraint to the inert set; and we
know that the inert set has fully rewritten it.  Should we split
it into [W] and [D], and put the [D] in the work list for further
work?

* CDictCan (C tys):
  Yes if the inert set could rewrite tys to make the class constraint,
  or type family, fire.  That is, yes if the inert_eqs intersects
  with the free vars of tys.  For this test we use
  (anyRewritableTyVar True) which ignores casts and coercions in tys,
  because rewriting the casts or coercions won't make the thing fire
  more often.

* CEqCan (lhs ~ ty): Yes if the inert set could rewrite 'lhs' or 'ty'.
  We need to check both 'lhs' and 'ty' against the inert set:
    - Inert set contains  [D] a ~ ty2
      Then we want to put [D] a ~ ty in the worklist, so we'll
      get [D] ty ~ ty2 with consequent good things

    - Inert set contains [D] b ~ a, where b is in ty.
      We can't just add [WD] a ~ ty[b] to the inert set, because
      that breaks the inert-set invariants.  If we tried to
      canonicalise another [D] constraint mentioning 'a', we'd
      get an infinite loop

  Moreover we must use (anyRewritableTyVar False) for the RHS,
  because even tyvars in the casts and coercions could give
  an infinite loop if we don't expose it

* CIrredCan: Yes if the inert set can rewrite the constraint.
  We used to think splitting irreds was unnecessary, but
  see Note [Splitting Irred WD constraints]

* Others: nothing is gained by splitting.

Note [Splitting Irred WD constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Splitting Irred constraints can make a difference. Here is the
scenario:

  a[sk] :: F v     -- F is a type family
  beta :: alpha

  work item: [WD] a ~ beta

This is heterogeneous, so we emit a kind equality and make the work item an
inert Irred.

  work item: [D] F v ~ alpha
  inert: [WD] (a |> co) ~ beta (CIrredCan)

Can't make progress on the work item. Add to inert set. This kicks out the
old inert, because a [D] can rewrite a [WD].

  work item: [WD] (a |> co) ~ beta
  inert: [D] F v ~ alpha (CEqCan)

Can't make progress on this work item either (although GHC tries by
decomposing the cast and rewriting... but that doesn't make a difference),
which is still hetero. Emit a new kind equality and add to inert set. But,
critically, we split the Irred.

  work list:
   [D] F v ~ alpha (CEqCan)
   [D] (a |> co) ~ beta (CIrred) -- this one was split off
  inert:
   [W] (a |> co) ~ beta
   [D] F v ~ alpha

We quickly solve the first work item, as it's the same as an inert.

  work item: [D] (a |> co) ~ beta
  inert:
   [W] (a |> co) ~ beta
   [D] F v ~ alpha

We decompose the cast, yielding

  [D] a ~ beta

We then rewrite the kinds. The lhs kind is F v, which flattens to alpha.

  co' :: F v ~ alpha
  [D] (a |> co') ~ beta

Now this equality is homo-kinded. So we swizzle it around to

  [D] beta ~ (a |> co')

and set beta := a |> co', and go home happy.

If we don't split the Irreds, we loop. This is all dangerously subtle.

This is triggered by test case typecheck/should_compile/SplitWD.

Note [Add derived shadows only for Wanteds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We only add shadows for Wanted constraints. That is, we have
[WD] but not [GD]; and maybeEmitShaodw looks only at [WD]
constraints.

It does just possibly make sense ot add a derived shadow for a
Given. If we created a Derived shadow of a Given, it could be
rewritten by other Deriveds, and that could, conceivably, lead to a
useful unification.

But (a) I have been unable to come up with an example of this
        happening
    (b) see #12660 for how adding the derived shadows
        of a Given led to an infinite loop.
    (c) It's unlikely that rewriting derived Givens will lead
        to a unification because Givens don't mention touchable
        unification variables

For (b) there may be other ways to solve the loop, but simply
reraining from adding derived shadows of Givens is particularly
simple.  And it's more efficient too!

Still, here's one possible reason for adding derived shadows
for Givens.  Consider
           work-item [G] a ~ [b], inerts has [D] b ~ a.
If we added the derived shadow (into the work list)
         [D] a ~ [b]
When we process it, we'll rewrite to a ~ [a] and get an
occurs check.  Without it we'll miss the occurs check (reporting
inaccessible code); but that's probably OK.

Note [Keep CDictCan shadows as CDictCan]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
  class C a => D a b
and [G] D a b, [G] C a in the inert set.  Now we insert
[D] b ~ c.  We want to kick out a derived shadow for [D] D a b,
so we can rewrite it with the new constraint, and perhaps get
instance reduction or other consequences.

BUT we do not want to kick out a *non-canonical* (D a b). If we
did, we would do this:
  - rewrite it to [D] D a c, with pend_sc = True
  - use expandSuperClasses to add C a
  - go round again, which solves C a from the givens
This loop goes on for ever and triggers the simpl_loop limit.

Solution: kick out the CDictCan which will have pend_sc = False,
because we've already added its superclasses.  So we won't re-add
them.  If we forget the pend_sc flag, our cunning scheme for avoiding
generating superclasses repeatedly will fail.

See #11379 for a case of this.

Note [Do not do improvement for WOnly]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We do improvement between two constraints (e.g. for injectivity
or functional dependencies) only if both are "improvable". And
we improve a constraint wrt the top-level instances only if
it is improvable.

Improvable:     [G] [WD] [D}
Not improvable: [W]

Reasons:

* It's less work: fewer pairs to compare

* Every [W] has a shadow [D] so nothing is lost

* Consider [WD] C Int b,  where 'b' is a skolem, and
    class C a b | a -> b
    instance C Int Bool
  We'll do a fundep on it and emit [D] b ~ Bool
  That will kick out constraint [WD] C Int b
  Then we'll split it to [W] C Int b (keep in inert)
                     and [D] C Int b (in work list)
  When processing the latter we'll rewrite it to
        [D] C Int Bool
  At that point it would be /stupid/ to interact it
  with the inert [W] C Int b in the inert set; after all,
  it's the very constraint from which the [D] C Int Bool
  was split!  We can avoid this by not doing improvement
  on [W] constraints. This came up in #12860.
-}

maybeEmitShadow :: InertCans -> Ct -> TcS Ct
-- See Note [The improvement story and derived shadows]
maybeEmitShadow ics ct
  | let ev = ctEvidence ct
  , CtWanted { ctev_pred = pred, ctev_loc = loc
             , ctev_nosh = WDeriv } <- ev
  , shouldSplitWD (inert_eqs ics) (inert_funeqs ics) ct
  = do { traceTcS "Emit derived shadow" (ppr ct)
       ; let derived_ev = CtDerived { ctev_pred = pred
                                    , ctev_loc  = loc }
             shadow_ct = ct { cc_ev = derived_ev }
               -- Te shadow constraint keeps the canonical shape.
               -- This just saves work, but is sometimes important;
               -- see Note [Keep CDictCan shadows as CDictCan]
       ; emitWork [shadow_ct]

       ; let ev' = ev { ctev_nosh = WOnly }
             ct' = ct { cc_ev = ev' }
                 -- Record that it now has a shadow
                 -- This is /the/ place we set the flag to WOnly
       ; return ct' }

  | otherwise
  = return ct

shouldSplitWD :: InertEqs -> FunEqMap EqualCtList -> Ct -> Bool
-- Precondition: 'ct' is [WD], and is inert
-- True <=> we should split ct ito [W] and [D] because
--          the inert_eqs can make progress on the [D]
-- See Note [Splitting WD constraints]

shouldSplitWD inert_eqs fun_eqs (CDictCan { cc_tyargs = tys })
  = should_split_match_args inert_eqs fun_eqs tys
    -- NB True: ignore coercions
    -- See Note [Splitting WD constraints]

shouldSplitWD inert_eqs fun_eqs (CEqCan { cc_lhs = TyVarLHS tv, cc_rhs = ty
                                        , cc_eq_rel = eq_rel })
  =  tv `elemDVarEnv` inert_eqs
  || anyRewritableCanEqLHS eq_rel (canRewriteTv inert_eqs) (canRewriteTyFam fun_eqs) ty
  -- NB False: do not ignore casts and coercions
  -- See Note [Splitting WD constraints]

shouldSplitWD inert_eqs fun_eqs (CEqCan { cc_ev = ev, cc_eq_rel = eq_rel })
  = anyRewritableCanEqLHS eq_rel (canRewriteTv inert_eqs) (canRewriteTyFam fun_eqs)
                          (ctEvPred ev)

shouldSplitWD inert_eqs fun_eqs (CIrredCan { cc_ev = ev })
  = anyRewritableCanEqLHS (ctEvEqRel ev) (canRewriteTv inert_eqs)
                          (canRewriteTyFam fun_eqs) (ctEvPred ev)

shouldSplitWD _ _ _ = False   -- No point in splitting otherwise

should_split_match_args :: InertEqs -> FunEqMap EqualCtList -> [TcType] -> Bool
-- True if the inert_eqs can rewrite anything in the argument types
should_split_match_args inert_eqs fun_eqs tys
  = any (anyRewritableCanEqLHS NomEq (canRewriteTv inert_eqs) (canRewriteTyFam fun_eqs)) tys
    -- See Note [Splitting WD constraints]

canRewriteTv :: InertEqs -> EqRel -> TyVar -> Bool
canRewriteTv inert_eqs eq_rel tv
  | Just (EqualCtList (ct :| _)) <- lookupDVarEnv inert_eqs tv
  , CEqCan { cc_eq_rel = eq_rel1 } <- ct
  = eq_rel1 `eqCanRewrite` eq_rel
  | otherwise
  = False

canRewriteTyFam :: FunEqMap EqualCtList -> EqRel -> TyCon -> [Type] -> Bool
canRewriteTyFam fun_eqs eq_rel tf args
  | Just (EqualCtList (ct :| _)) <- findFunEq fun_eqs tf args
  , CEqCan { cc_eq_rel = eq_rel1 } <- ct
  = eq_rel1 `eqCanRewrite` eq_rel
  | otherwise
  = False

isImprovable :: CtEvidence -> Bool
-- See Note [Do not do improvement for WOnly]
isImprovable (CtWanted { ctev_nosh = WOnly }) = False
isImprovable _                                = True


{- *********************************************************************
*                                                                      *
                   Inert instances: inert_insts
*                                                                      *
********************************************************************* -}

addInertForAll :: QCInst -> TcS ()
-- Add a local Given instance, typically arising from a type signature
addInertForAll new_qci
  = do { ics  <- getInertCans
       ; ics1 <- add_qci ics

       -- Update given equalities. C.f updateGivenEqs
       ; tclvl <- getTcLevel
       ; let pred         = qci_pred new_qci
             not_equality = isClassPred pred && not (isEqPred pred)
                  -- True <=> definitely not an equality
                  -- A qci_pred like (f a) might be an equality

             ics2 | not_equality = ics1
                  | otherwise    = ics1 { inert_given_eq_lvl = tclvl
                                        , inert_given_eqs    = True }

       ; setInertCans ics2 }
  where
    add_qci :: InertCans -> TcS InertCans
    -- See Note [Do not add duplicate quantified instances]
    add_qci ics@(IC { inert_insts = qcis })
      | any same_qci qcis
      = do { traceTcS "skipping duplicate quantified instance" (ppr new_qci)
           ; return ics }

      | otherwise
      = do { traceTcS "adding new inert quantified instance" (ppr new_qci)
           ; return (ics { inert_insts = new_qci : qcis }) }

    same_qci old_qci = tcEqType (ctEvPred (qci_ev old_qci))
                                (ctEvPred (qci_ev new_qci))

{- Note [Do not add duplicate quantified instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this (#15244):

  f :: (C g, D g) => ....
  class S g => C g where ...
  class S g => D g where ...
  class (forall a. Eq a => Eq (g a)) => S g where ...

Then in f's RHS there are two identical quantified constraints
available, one via the superclasses of C and one via the superclasses
of D.  The two are identical, and it seems wrong to reject the program
because of that. But without doing duplicate-elimination we will have
two matching QCInsts when we try to solve constraints arising from f's
RHS.

The simplest thing is simply to eliminate duplicates, which we do here.
-}

{- *********************************************************************
*                                                                      *
                  Adding an inert
*                                                                      *
************************************************************************

Note [Adding an equality to the InertCans]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When adding an equality to the inerts:

* Split [WD] into [W] and [D] if the inerts can rewrite the latter;
  done by maybeEmitShadow.

* Kick out any constraints that can be rewritten by the thing
  we are adding.  Done by kickOutRewritable.

* Note that unifying a:=ty, is like adding [G] a~ty; just use
  kickOutRewritable with Nominal, Given.  See kickOutAfterUnification.
-}

addInertCan :: Ct -> TcS ()
-- Precondition: item /is/ canonical
-- See Note [Adding an equality to the InertCans]
addInertCan ct =
    do { traceTcS "addInertCan {" $
         text "Trying to insert new inert item:" <+> ppr ct
       ; mkTcS (\TcSEnv{tcs_abort_on_insoluble=abort_flag} ->
                 when (abort_flag && insolubleEqCt ct) TcM.failM)
       ; ics <- getInertCans
       ; ct  <- maybeEmitShadow ics ct
       ; ics <- maybeKickOut ics ct
       ; tclvl <- getTcLevel
       ; setInertCans (addInertItem tclvl ics ct)

       ; traceTcS "addInertCan }" $ empty }

maybeKickOut :: InertCans -> Ct -> TcS InertCans
-- For a CEqCan, kick out any inert that can be rewritten by the CEqCan
maybeKickOut ics ct
  | CEqCan { cc_lhs = lhs, cc_ev = ev, cc_eq_rel = eq_rel } <- ct
  = do { (_, ics') <- kickOutRewritable (ctEvFlavour ev, eq_rel) lhs ics
       ; return ics' }
  | otherwise
  = return ics

-----------------------------------------
kickOutRewritable  :: CtFlavourRole  -- Flavour/role of the equality that
                                      -- is being added to the inert set
                   -> CanEqLHS        -- The new equality is lhs ~ ty
                   -> InertCans
                   -> TcS (Int, InertCans)
kickOutRewritable new_fr new_lhs ics
  = do { let (kicked_out, ics') = kickOutRewritableLHS new_fr new_lhs ics
             n_kicked = workListSize kicked_out

       ; unless (n_kicked == 0) $
         do { updWorkListTcS (appendWorkList kicked_out)

              -- The famapp-cache contains Given evidence from the inert set.
              -- If we're kicking out Givens, we need to remove this evidence
              -- from the cache, too.
            ; let kicked_given_ev_vars =
                    [ ev_var | ct <- wl_eqs kicked_out
                             , CtGiven { ctev_evar = ev_var } <- [ctEvidence ct] ]
            ; when (new_fr `eqCanRewriteFR` (Given, NomEq) &&
                   -- if this isn't true, no use looking through the constraints
                    not (null kicked_given_ev_vars)) $
              do { traceTcS "Given(s) have been kicked out; drop from famapp-cache"
                            (ppr kicked_given_ev_vars)
                 ; dropFromFamAppCache (mkVarSet kicked_given_ev_vars) }

            ; csTraceTcS $
              hang (text "Kick out, lhs =" <+> ppr new_lhs)
                 2 (vcat [ text "n-kicked =" <+> int n_kicked
                         , text "kicked_out =" <+> ppr kicked_out
                         , text "Residual inerts =" <+> ppr ics' ]) }

       ; return (n_kicked, ics') }

kickOutAfterUnification :: TcTyVar -> TcS Int
kickOutAfterUnification new_tv
  = do { ics <- getInertCans
       ; (n_kicked, ics2) <- kickOutRewritable (Given,NomEq)
                                                 (TyVarLHS new_tv) ics
                     -- Given because the tv := xi is given; NomEq because
                     -- only nominal equalities are solved by unification

       ; setInertCans ics2
       ; return n_kicked }

-- See Wrinkle (2) in Note [Equalities with incompatible kinds] in GHC.Tc.Solver.Canonical
kickOutAfterFillingCoercionHole :: CoercionHole -> Coercion -> TcS ()
kickOutAfterFillingCoercionHole hole filled_co
  = do { ics <- getInertCans
       ; let (kicked_out, ics') = kick_out ics
             n_kicked           = workListSize kicked_out

       ; unless (n_kicked == 0) $
         do { updWorkListTcS (appendWorkList kicked_out)
            ; csTraceTcS $
              hang (text "Kick out, hole =" <+> ppr hole)
                 2 (vcat [ text "n-kicked =" <+> int n_kicked
                         , text "kicked_out =" <+> ppr kicked_out
                         , text "Residual inerts =" <+> ppr ics' ]) }

       ; setInertCans ics' }
  where
    holes_of_co = coercionHolesOfCo filled_co

    kick_out :: InertCans -> (WorkList, InertCans)
    kick_out ics@(IC { inert_blocked = blocked })
      = let (to_kick, to_keep) = partitionBagWith kick_ct blocked

            kicked_out = extendWorkListCts (bagToList to_kick) emptyWorkList
            ics'       = ics { inert_blocked = to_keep }
        in
        (kicked_out, ics')

    kick_ct :: Ct -> Either Ct Ct
         -- Left: kick out; Right: keep. But even if we keep, we may need
         -- to update the set of blocking holes
    kick_ct ct@(CIrredCan { cc_reason = HoleBlockerReason holes })
      | hole `elementOfUniqSet` holes
      = let new_holes = holes `delOneFromUniqSet` hole
                              `unionUniqSets` holes_of_co
            updated_ct = ct { cc_reason = HoleBlockerReason new_holes }
        in
        if isEmptyUniqSet new_holes
        then Left updated_ct
        else Right updated_ct

      | otherwise
      = Right ct

    kick_ct other = pprPanic "kickOutAfterFillingCoercionHole" (ppr other)

--------------
addInertSafehask :: InertCans -> Ct -> InertCans
addInertSafehask ics item@(CDictCan { cc_class = cls, cc_tyargs = tys })
  = ics { inert_safehask = addDictCt (inert_dicts ics) (classTyCon cls) tys item }

addInertSafehask _ item
  = pprPanic "addInertSafehask: can't happen! Inserting " $ ppr item

insertSafeOverlapFailureTcS :: InstanceWhat -> Ct -> TcS ()
-- See Note [Safe Haskell Overlapping Instances Implementation] in GHC.Tc.Solver
insertSafeOverlapFailureTcS what item
  | safeOverlap what = return ()
  | otherwise        = updInertCans (\ics -> addInertSafehask ics item)

getSafeOverlapFailures :: TcS Cts
-- See Note [Safe Haskell Overlapping Instances Implementation] in GHC.Tc.Solver
getSafeOverlapFailures
 = do { IC { inert_safehask = safehask } <- getInertCans
      ; return $ foldDicts consCts safehask emptyCts }

--------------
addSolvedDict :: InstanceWhat -> CtEvidence -> Class -> [Type] -> TcS ()
-- Conditionally add a new item in the solved set of the monad
-- See Note [Solved dictionaries] in GHC.Tc.Solver.InertSet
addSolvedDict what item cls tys
  | isWanted item
  , instanceReturnsDictCon what
  = do { traceTcS "updSolvedSetTcs:" $ ppr item
       ; updInertTcS $ \ ics ->
             ics { inert_solved_dicts = addDict (inert_solved_dicts ics) cls tys item } }
  | otherwise
  = return ()

getSolvedDicts :: TcS (DictMap CtEvidence)
getSolvedDicts = do { ics <- getTcSInerts; return (inert_solved_dicts ics) }

setSolvedDicts :: DictMap CtEvidence -> TcS ()
setSolvedDicts solved_dicts
  = updInertTcS $ \ ics ->
    ics { inert_solved_dicts = solved_dicts }

{- *********************************************************************
*                                                                      *
                  Other inert-set operations
*                                                                      *
********************************************************************* -}

updInertTcS :: (InertSet -> InertSet) -> TcS ()
-- Modify the inert set with the supplied function
updInertTcS upd_fn
  = do { is_var <- getTcSInertsRef
       ; wrapTcS (do { curr_inert <- TcM.readTcRef is_var
                     ; TcM.writeTcRef is_var (upd_fn curr_inert) }) }

getInertCans :: TcS InertCans
getInertCans = do { inerts <- getTcSInerts; return (inert_cans inerts) }

setInertCans :: InertCans -> TcS ()
setInertCans ics = updInertTcS $ \ inerts -> inerts { inert_cans = ics }

updRetInertCans :: (InertCans -> (a, InertCans)) -> TcS a
-- Modify the inert set with the supplied function
updRetInertCans upd_fn
  = do { is_var <- getTcSInertsRef
       ; wrapTcS (do { inerts <- TcM.readTcRef is_var
                     ; let (res, cans') = upd_fn (inert_cans inerts)
                     ; TcM.writeTcRef is_var (inerts { inert_cans = cans' })
                     ; return res }) }

updInertCans :: (InertCans -> InertCans) -> TcS ()
-- Modify the inert set with the supplied function
updInertCans upd_fn
  = updInertTcS $ \ inerts -> inerts { inert_cans = upd_fn (inert_cans inerts) }

updInertDicts :: (DictMap Ct -> DictMap Ct) -> TcS ()
-- Modify the inert set with the supplied function
updInertDicts upd_fn
  = updInertCans $ \ ics -> ics { inert_dicts = upd_fn (inert_dicts ics) }

updInertSafehask :: (DictMap Ct -> DictMap Ct) -> TcS ()
-- Modify the inert set with the supplied function
updInertSafehask upd_fn
  = updInertCans $ \ ics -> ics { inert_safehask = upd_fn (inert_safehask ics) }

updInertIrreds :: (Cts -> Cts) -> TcS ()
-- Modify the inert set with the supplied function
updInertIrreds upd_fn
  = updInertCans $ \ ics -> ics { inert_irreds = upd_fn (inert_irreds ics) }

getInertEqs :: TcS (DTyVarEnv EqualCtList)
getInertEqs = do { inert <- getInertCans; return (inert_eqs inert) }

getInnermostGivenEqLevel :: TcS TcLevel
getInnermostGivenEqLevel = do { inert <- getInertCans
                              ; return (inert_given_eq_lvl inert) }

getInertInsols :: TcS Cts
-- Returns insoluble equality constraints and TypeError constraints,
-- specifically including Givens.
--
-- Note that this function only inspects irreducible constraints;
-- a DictCan constraint such as 'Eq (TypeError msg)' is not
-- considered to be an insoluble constraint by this function.
--
-- See Note [Pattern match warnings with insoluble Givens] in GHC.Tc.Solver.
getInertInsols = do { inert <- getInertCans
                    ; return $ filterBag insolubleCt (inert_irreds inert) }

getInertGivens :: TcS [Ct]
-- Returns the Given constraints in the inert set
getInertGivens
  = do { inerts <- getInertCans
       ; let all_cts = foldDicts (:) (inert_dicts inerts)
                     $ foldFunEqs (\ecl out -> equalCtListToList ecl ++ out)
                                  (inert_funeqs inerts)
                     $ concatMap equalCtListToList (dVarEnvElts (inert_eqs inerts))
       ; return (filter isGivenCt all_cts) }

getPendingGivenScs :: TcS [Ct]
-- Find all inert Given dictionaries, or quantified constraints,
--     whose cc_pend_sc flag is True
--     and that belong to the current level
-- Set their cc_pend_sc flag to False in the inert set, and return that Ct
getPendingGivenScs = do { lvl <- getTcLevel
                        ; updRetInertCans (get_sc_pending lvl) }

get_sc_pending :: TcLevel -> InertCans -> ([Ct], InertCans)
get_sc_pending this_lvl ic@(IC { inert_dicts = dicts, inert_insts = insts })
  = assertPpr (all isGivenCt sc_pending) (ppr sc_pending)
       -- When getPendingScDics is called,
       -- there are never any Wanteds in the inert set
    (sc_pending, ic { inert_dicts = dicts', inert_insts = insts' })
  where
    sc_pending = sc_pend_insts ++ sc_pend_dicts

    sc_pend_dicts = foldDicts get_pending dicts []
    dicts' = foldr add dicts sc_pend_dicts

    (sc_pend_insts, insts') = mapAccumL get_pending_inst [] insts

    get_pending :: Ct -> [Ct] -> [Ct]  -- Get dicts with cc_pend_sc = True
                                       -- but flipping the flag
    get_pending dict dicts
        | Just dict' <- isPendingScDict dict
        , belongs_to_this_level (ctEvidence dict)
        = dict' : dicts
        | otherwise
        = dicts

    add :: Ct -> DictMap Ct -> DictMap Ct
    add ct@(CDictCan { cc_class = cls, cc_tyargs = tys }) dicts
        = addDictCt dicts (classTyCon cls) tys ct
    add ct _ = pprPanic "getPendingScDicts" (ppr ct)

    get_pending_inst :: [Ct] -> QCInst -> ([Ct], QCInst)
    get_pending_inst cts qci@(QCI { qci_ev = ev })
       | Just qci' <- isPendingScInst qci
       , belongs_to_this_level ev
       = (CQuantCan qci' : cts, qci')
       | otherwise
       = (cts, qci)

    belongs_to_this_level ev = ctLocLevel (ctEvLoc ev) == this_lvl
    -- We only want Givens from this level; see (3a) in
    -- Note [The superclass story] in GHC.Tc.Solver.Canonical

getUnsolvedInerts :: TcS ( Bag Implication
                         , Cts )   -- All simple constraints
-- Return all the unsolved [Wanted] or [Derived] constraints
--
-- Post-condition: the returned simple constraints are all fully zonked
--                     (because they come from the inert set)
--                 the unsolved implics may not be
getUnsolvedInerts
 = do { IC { inert_eqs      = tv_eqs
           , inert_funeqs   = fun_eqs
           , inert_irreds   = irreds
           , inert_blocked  = blocked
           , inert_dicts    = idicts
           } <- getInertCans

      ; let unsolved_tv_eqs   = foldTyEqs add_if_unsolved tv_eqs emptyCts
            unsolved_fun_eqs  = foldFunEqs add_if_unsolveds fun_eqs emptyCts
            unsolved_irreds   = Bag.filterBag is_unsolved irreds
            unsolved_blocked  = blocked  -- all blocked equalities are W/D
            unsolved_dicts    = foldDicts add_if_unsolved idicts emptyCts
            unsolved_others   = unionManyBags [ unsolved_irreds
                                              , unsolved_dicts
                                              , unsolved_blocked ]

      ; implics <- getWorkListImplics

      ; traceTcS "getUnsolvedInerts" $
        vcat [ text " tv eqs =" <+> ppr unsolved_tv_eqs
             , text "fun eqs =" <+> ppr unsolved_fun_eqs
             , text "others =" <+> ppr unsolved_others
             , text "implics =" <+> ppr implics ]

      ; return ( implics, unsolved_tv_eqs `unionBags`
                          unsolved_fun_eqs `unionBags`
                          unsolved_others) }
  where
    add_if_unsolved :: Ct -> Cts -> Cts
    add_if_unsolved ct cts | is_unsolved ct = ct `consCts` cts
                           | otherwise      = cts

    add_if_unsolveds :: EqualCtList -> Cts -> Cts
    add_if_unsolveds new_cts old_cts = foldr add_if_unsolved old_cts
                                             (equalCtListToList new_cts)

    is_unsolved ct = not (isGivenCt ct)   -- Wanted or Derived

getHasGivenEqs :: TcLevel           -- TcLevel of this implication
               -> TcS ( HasGivenEqs -- are there Given equalities?
                      , Cts )       -- Insoluble equalities arising from givens
-- See Note [Tracking Given equalities] in GHC.Tc.Solver.InertSet
getHasGivenEqs tclvl
  = do { inerts@(IC { inert_irreds       = irreds
                    , inert_given_eqs    = given_eqs
                    , inert_given_eq_lvl = ge_lvl })
              <- getInertCans
       ; let insols = filterBag insolubleEqCt irreds
                      -- Specifically includes ones that originated in some
                      -- outer context but were refined to an insoluble by
                      -- a local equality; so do /not/ add ct_given_here.

             -- See Note [HasGivenEqs] in GHC.Tc.Types.Constraint, and
             -- Note [Tracking Given equalities] in GHC.Tc.Solver.InertSet
             has_ge | ge_lvl == tclvl = MaybeGivenEqs
                    | given_eqs       = LocalGivenEqs
                    | otherwise       = NoGivenEqs

       ; traceTcS "getHasGivenEqs" $
         vcat [ text "given_eqs:" <+> ppr given_eqs
              , text "ge_lvl:" <+> ppr ge_lvl
              , text "ambient level:" <+> ppr tclvl
              , text "Inerts:" <+> ppr inerts
              , text "Insols:" <+> ppr insols]
       ; return (has_ge, insols) }

{- Note [Unsolved Derived equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In getUnsolvedInerts, we return a derived equality from the inert_eqs
because it is a candidate for floating out of this implication.  We
only float equalities with a meta-tyvar on the left, so we only pull
those out here.

Note [What might equal later?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We must determine whether a Given might later equal a Wanted. We
definitely need to account for the possibility that any metavariable
might be arbitrarily instantiated. Yet we do *not* want
to allow skolems in to be instantiated, as we've already rewritten
with respect to any Givens. (We're solving a Wanted here, and so
all Givens have already been processed.)

This is best understood by example.

1. C alpha  ~?  C Int

   That given certainly might match later.

2. C a  ~?  C Int

   No. No new givens are going to arise that will get the `a` to rewrite
   to Int.

3. C alpha[tv]   ~?  C Int

   That alpha[tv] is a TyVarTv, unifiable only with other type variables.
   It cannot equal later.

4. C (F alpha)   ~?   C Int

   Sure -- that can equal later, if we learn something useful about alpha.

5. C (F alpha[tv])  ~?  C Int

   This, too, might equal later. Perhaps we have [G] F b ~ Int elsewhere.
   Or maybe we have C (F alpha[tv] beta[tv]), these unify with each other,
   and F x x = Int. Remember: returning True doesn't commit ourselves to
   anything.

6. C (F a)  ~?  C a

   No, this won't match later. If we could rewrite (F a) or a, we would
   have by now.

7. C (Maybe alpha)  ~?  C alpha

   We say this cannot equal later, because it would require
   alpha := Maybe (Maybe (Maybe ...)). While such a type can be contrived,
   we choose not to worry about it. See Note [Infinitary substitution in lookup]
   in GHC.Core.InstEnv. Getting this wrong let to #19107, tested in
   typecheck/should_compile/T19107.

8. C cbv   ~?  C Int
   where cbv = F a

   The cbv is a cycle-breaker var which stands for F a. See
   Note [Type variable cycles] in GHC.Tc.Solver.Canonical.
   This is just like case 6, and we say "no". Saying "no" here is
   essential in getting the parser to type-check, with its use of DisambECP.

9. C cbv   ~?   C Int
   where cbv = F alpha

   Here, we might indeed equal later. Distinguishing between
   this case and Example 8 is why we need the InertSet in mightEqualLater.

10. C (F alpha, Int)  ~?  C (Bool, F alpha)

   This cannot equal later, because F a would have to equal both Bool and
   Int.

To deal with type family applications, we use the Core flattener. See
Note [Flattening type-family applications when matching instances] in GHC.Core.Unify.
The Core flattener replaces all type family applications with
fresh variables. The next question: should we allow these fresh
variables in the domain of a unifying substitution?

A type family application that mentions only skolems (example 6) is settled:
any skolems would have been rewritten w.r.t. Givens by now. These type family
applications match only themselves. A type family application that mentions
metavariables, on the other hand, can match anything. So, if the original type
family application contains a metavariable, we use BindMe to tell the unifier
to allow it in the substitution. On the other hand, a type family application
with only skolems is considered rigid.

This treatment fixes #18910 and is tested in
typecheck/should_compile/InstanceGivenOverlap{,2}
-}

removeInertCts :: [Ct] -> InertCans -> InertCans
-- ^ Remove inert constraints from the 'InertCans', for use when a
-- typechecker plugin wishes to discard a given.
removeInertCts cts icans = foldl' removeInertCt icans cts

removeInertCt :: InertCans -> Ct -> InertCans
removeInertCt is ct =
  case ct of

    CDictCan  { cc_class = cl, cc_tyargs = tys } ->
      is { inert_dicts = delDict (inert_dicts is) cl tys }

    CEqCan    { cc_lhs  = lhs, cc_rhs = rhs } -> delEq is lhs rhs

    CQuantCan {}     -> panic "removeInertCt: CQuantCan"
    CIrredCan {}     -> panic "removeInertCt: CIrredEvCan"
    CNonCanonical {} -> panic "removeInertCt: CNonCanonical"
    CSpecialCan _ special_pred _ ->
      pprPanic "removeInertCt" (ppr "CSpecialCan" <+> parens (ppr special_pred))

-- | Looks up a family application in the inerts.
lookupFamAppInert :: TyCon -> [Type] -> TcS (Maybe (Reduction, CtFlavourRole))
lookupFamAppInert fam_tc tys
  = do { IS { inert_cans = IC { inert_funeqs = inert_funeqs } } <- getTcSInerts
       ; return (lookup_inerts inert_funeqs) }
  where
    lookup_inerts inert_funeqs
      | Just (EqualCtList (CEqCan { cc_ev = ctev, cc_rhs = rhs } :| _))
          <- findFunEq inert_funeqs fam_tc tys
      = Just (mkReduction (ctEvCoercion ctev) rhs
             ,ctEvFlavourRole ctev)
      | otherwise = Nothing

lookupInInerts :: CtLoc -> TcPredType -> TcS (Maybe CtEvidence)
-- Is this exact predicate type cached in the solved or canonicals of the InertSet?
lookupInInerts loc pty
  | ClassPred cls tys <- classifyPredType pty
  = do { inerts <- getTcSInerts
       ; return (lookupSolvedDict inerts loc cls tys `mplus`
                 fmap ctEvidence (lookupInertDict (inert_cans inerts) loc cls tys)) }
  | otherwise -- NB: No caching for equalities, IPs, holes, or errors
  = return Nothing

-- | Look up a dictionary inert.
lookupInertDict :: InertCans -> CtLoc -> Class -> [Type] -> Maybe Ct
lookupInertDict (IC { inert_dicts = dicts }) loc cls tys
  = case findDict dicts loc cls tys of
      Just ct -> Just ct
      _       -> Nothing

-- | Look up a solved inert.
lookupSolvedDict :: InertSet -> CtLoc -> Class -> [Type] -> Maybe CtEvidence
-- Returns just if exactly this predicate type exists in the solved.
lookupSolvedDict (IS { inert_solved_dicts = solved }) loc cls tys
  = case findDict solved loc cls tys of
      Just ev -> Just ev
      _       -> Nothing

---------------------------
lookupFamAppCache :: TyCon -> [Type] -> TcS (Maybe Reduction)
lookupFamAppCache fam_tc tys
  = do { IS { inert_famapp_cache = famapp_cache } <- getTcSInerts
       ; case findFunEq famapp_cache fam_tc tys of
           result@(Just redn) ->
             do { traceTcS "famapp_cache hit" (vcat [ ppr (mkTyConApp fam_tc tys)
                                                    , ppr redn ])
                ; return result }
           Nothing -> return Nothing }

extendFamAppCache :: TyCon -> [Type] -> Reduction -> TcS ()
-- NB: co :: rhs ~ F tys, to match expectations of rewriter
extendFamAppCache tc xi_args stuff@(Reduction _ ty)
  = do { dflags <- getDynFlags
       ; when (gopt Opt_FamAppCache dflags) $
    do { traceTcS "extendFamAppCache" (vcat [ ppr tc <+> ppr xi_args
                                            , ppr ty ])
            -- 'co' can be bottom, in the case of derived items
       ; updInertTcS $ \ is@(IS { inert_famapp_cache = fc }) ->
            is { inert_famapp_cache = insertFunEq fc tc xi_args stuff } } }

-- Remove entries from the cache whose evidence mentions variables in the
-- supplied set
dropFromFamAppCache :: VarSet -> TcS ()
dropFromFamAppCache varset
  = do { inerts@(IS { inert_famapp_cache = famapp_cache }) <- getTcSInerts
       ; let filtered = filterTcAppMap check famapp_cache
       ; setTcSInerts $ inerts { inert_famapp_cache = filtered } }
  where
    check :: Reduction -> Bool
    check redn
      = not (anyFreeVarsOfCo (`elemVarSet` varset) $ reductionCoercion redn)

{- *********************************************************************
*                                                                      *
                   Irreds
*                                                                      *
********************************************************************* -}

foldIrreds :: (Ct -> b -> b) -> Cts -> b -> b
foldIrreds k irreds z = foldr k z irreds

{-
************************************************************************
*                                                                      *
*              The TcS solver monad                                    *
*                                                                      *
************************************************************************

Note [The TcS monad]
~~~~~~~~~~~~~~~~~~~~
The TcS monad is a weak form of the main Tc monad

All you can do is
    * fail
    * allocate new variables
    * fill in evidence variables

Filling in a dictionary evidence variable means to create a binding
for it, so TcS carries a mutable location where the binding can be
added.  This is initialised from the innermost implication constraint.
-}

data TcSEnv
  = TcSEnv {
      tcs_ev_binds    :: EvBindsVar,

      tcs_unified     :: IORef Int,
         -- The number of unification variables we have filled
         -- The important thing is whether it is non-zero

      tcs_unif_lvl  :: IORef (Maybe TcLevel),
         -- The Unification Level Flag
         -- Outermost level at which we have unified a meta tyvar
         -- Starts at Nothing, then (Just i), then (Just j) where j<i
         -- See Note [The Unification Level Flag]

      tcs_count     :: IORef Int, -- Global step count

      tcs_inerts    :: IORef InertSet, -- Current inert set

      -- Whether to throw an exception if we come across an insoluble constraint.
      -- Used to fail-fast when checking for hole-fits. See Note [Speeding up
      -- valid hole-fits].
      tcs_abort_on_insoluble :: Bool,

      -- See Note [WorkList priorities] in GHC.Tc.Solver.InertSet
      tcs_worklist  :: IORef WorkList -- Current worklist
    }

---------------
newtype TcS a = TcS { unTcS :: TcSEnv -> TcM a } deriving (Functor)


-- | Smart constructor for 'TcS', as describe in Note [The one-shot state
-- monad trick] in "GHC.Utils.Monad".
mkTcS :: (TcSEnv -> TcM a) -> TcS a
mkTcS f = TcS (oneShot f)

instance Applicative TcS where
  pure x = mkTcS $ \_ -> return x
  (<*>) = ap

instance Monad TcS where
  m >>= k   = mkTcS $ \ebs -> do
    unTcS m ebs >>= (\r -> unTcS (k r) ebs)

instance MonadIO TcS where
  liftIO act = TcS $ \_env -> liftIO act

instance MonadFail TcS where
  fail err  = mkTcS $ \_ -> fail err

instance MonadUnique TcS where
   getUniqueSupplyM = wrapTcS getUniqueSupplyM

instance HasModule TcS where
   getModule = wrapTcS getModule

instance MonadThings TcS where
   lookupThing n = wrapTcS (lookupThing n)

-- Basic functionality
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wrapTcS :: TcM a -> TcS a
-- Do not export wrapTcS, because it promotes an arbitrary TcM to TcS,
-- and TcS is supposed to have limited functionality
wrapTcS action = mkTcS $ \_env -> action -- a TcM action will not use the TcEvBinds

wrapErrTcS :: TcM a -> TcS a
-- The thing wrapped should just fail
-- There's no static check; it's up to the user
-- Having a variant for each error message is too painful
wrapErrTcS = wrapTcS

wrapWarnTcS :: TcM a -> TcS a
-- The thing wrapped should just add a warning, or no-op
-- There's no static check; it's up to the user
wrapWarnTcS = wrapTcS

panicTcS  :: SDoc -> TcS a
failTcS   :: TcRnMessage -> TcS a
warnTcS, addErrTcS :: TcRnMessage -> TcS ()
failTcS      = wrapTcS . TcM.failWith
warnTcS msg  = wrapTcS (TcM.addDiagnostic msg)
addErrTcS    = wrapTcS . TcM.addErr
panicTcS doc = pprPanic "GHC.Tc.Solver.Canonical" doc

traceTcS :: String -> SDoc -> TcS ()
traceTcS herald doc = wrapTcS (TcM.traceTc herald doc)
{-# INLINE traceTcS #-}  -- see Note [INLINE conditional tracing utilities]

runTcPluginTcS :: TcPluginM a -> TcS a
runTcPluginTcS = wrapTcS . runTcPluginM

instance HasDynFlags TcS where
    getDynFlags = wrapTcS getDynFlags

getGlobalRdrEnvTcS :: TcS GlobalRdrEnv
getGlobalRdrEnvTcS = wrapTcS TcM.getGlobalRdrEnv

bumpStepCountTcS :: TcS ()
bumpStepCountTcS = mkTcS $ \env ->
  do { let ref = tcs_count env
     ; n <- TcM.readTcRef ref
     ; TcM.writeTcRef ref (n+1) }

csTraceTcS :: SDoc -> TcS ()
csTraceTcS doc
  = wrapTcS $ csTraceTcM (return doc)
{-# INLINE csTraceTcS #-}  -- see Note [INLINE conditional tracing utilities]

traceFireTcS :: CtEvidence -> SDoc -> TcS ()
-- Dump a rule-firing trace
traceFireTcS ev doc
  = mkTcS $ \env -> csTraceTcM $
    do { n <- TcM.readTcRef (tcs_count env)
       ; tclvl <- TcM.getTcLevel
       ; return (hang (text "Step" <+> int n
                       <> brackets (text "l:" <> ppr tclvl <> comma <>
                                    text "d:" <> ppr (ctLocDepth (ctEvLoc ev)))
                       <+> doc <> colon)
                     4 (ppr ev)) }
{-# INLINE traceFireTcS #-}  -- see Note [INLINE conditional tracing utilities]

csTraceTcM :: TcM SDoc -> TcM ()
-- Constraint-solver tracing, -ddump-cs-trace
csTraceTcM mk_doc
  = do { logger <- getLogger
       ; when (  logHasDumpFlag logger Opt_D_dump_cs_trace
                  || logHasDumpFlag logger Opt_D_dump_tc_trace)
              ( do { msg <- mk_doc
                   ; TcM.dumpTcRn False
                       Opt_D_dump_cs_trace
                       "" FormatText
                       msg }) }
{-# INLINE csTraceTcM #-}  -- see Note [INLINE conditional tracing utilities]

runTcS :: TcS a                -- What to run
       -> TcM (a, EvBindMap)
runTcS tcs
  = do { ev_binds_var <- TcM.newTcEvBinds
       ; res <- runTcSWithEvBinds ev_binds_var tcs
       ; ev_binds <- TcM.getTcEvBindsMap ev_binds_var
       ; return (res, ev_binds) }

-- | This variant of 'runTcS' will keep solving, even when only Deriveds
-- are left around. It also doesn't return any evidence, as callers won't
-- need it.
runTcSDeriveds :: TcS a -> TcM a
runTcSDeriveds tcs
  = do { ev_binds_var <- TcM.newTcEvBinds
       ; runTcSWithEvBinds ev_binds_var tcs }


-- | This variant of 'runTcSDeriveds' will immediatley fail upon encountering an
-- insoluble ct. See Note [Speeding up valid-hole fits]
runTcSDerivedsEarlyAbort :: TcS a -> TcM a
runTcSDerivedsEarlyAbort tcs
  = do { ev_binds_var <- TcM.newTcEvBinds
       ; runTcSWithEvBinds' True True ev_binds_var tcs }

-- | This can deal only with equality constraints.
runTcSEqualities :: TcS a -> TcM a
runTcSEqualities thing_inside
  = do { ev_binds_var <- TcM.newNoTcEvBinds
       ; runTcSWithEvBinds ev_binds_var thing_inside }

-- | A variant of 'runTcS' that takes and returns an 'InertSet' for
-- later resumption of the 'TcS' session.
runTcSInerts :: InertSet -> TcS a -> TcM (a, InertSet)
runTcSInerts inerts tcs = do
  ev_binds_var <- TcM.newTcEvBinds
  runTcSWithEvBinds' False False ev_binds_var $ do
    setTcSInerts inerts
    a <- tcs
    new_inerts <- getTcSInerts
    return (a, new_inerts)

runTcSWithEvBinds :: EvBindsVar
                  -> TcS a
                  -> TcM a
runTcSWithEvBinds = runTcSWithEvBinds' True False

runTcSWithEvBinds' :: Bool -- ^ Restore type variable cycles afterwards?
                           -- Don't if you want to reuse the InertSet.
                           -- See also Note [Type variable cycles]
                           -- in GHC.Tc.Solver.Canonical
                   -> Bool
                   -> EvBindsVar
                   -> TcS a
                   -> TcM a
runTcSWithEvBinds' restore_cycles abort_on_insoluble ev_binds_var tcs
  = do { unified_var <- TcM.newTcRef 0
       ; step_count <- TcM.newTcRef 0
       ; inert_var <- TcM.newTcRef emptyInert
       ; wl_var <- TcM.newTcRef emptyWorkList
       ; unif_lvl_var <- TcM.newTcRef Nothing
       ; let env = TcSEnv { tcs_ev_binds           = ev_binds_var
                          , tcs_unified            = unified_var
                          , tcs_unif_lvl           = unif_lvl_var
                          , tcs_count              = step_count
                          , tcs_inerts             = inert_var
                          , tcs_abort_on_insoluble = abort_on_insoluble
                          , tcs_worklist           = wl_var }

             -- Run the computation
       ; res <- unTcS tcs env

       ; count <- TcM.readTcRef step_count
       ; when (count > 0) $
         csTraceTcM $ return (text "Constraint solver steps =" <+> int count)

       ; when restore_cycles $
         do { inert_set <- TcM.readTcRef inert_var
            ; restoreTyVarCycles inert_set }

#if defined(DEBUG)
       ; ev_binds <- TcM.getTcEvBindsMap ev_binds_var
       ; checkForCyclicBinds ev_binds
#endif

       ; return res }

----------------------------
#if defined(DEBUG)
checkForCyclicBinds :: EvBindMap -> TcM ()
checkForCyclicBinds ev_binds_map
  | null cycles
  = return ()
  | null coercion_cycles
  = TcM.traceTc "Cycle in evidence binds" $ ppr cycles
  | otherwise
  = pprPanic "Cycle in coercion bindings" $ ppr coercion_cycles
  where
    ev_binds = evBindMapBinds ev_binds_map

    cycles :: [[EvBind]]
    cycles = [c | CyclicSCC c <- stronglyConnCompFromEdgedVerticesUniq edges]

    coercion_cycles = [c | c <- cycles, any is_co_bind c]
    is_co_bind (EvBind { eb_lhs = b }) = isEqPrimPred (varType b)

    edges :: [ Node EvVar EvBind ]
    edges = [ DigraphNode bind bndr (nonDetEltsUniqSet (evVarsOfTerm rhs))
            | bind@(EvBind { eb_lhs = bndr, eb_rhs = rhs}) <- bagToList ev_binds ]
            -- It's OK to use nonDetEltsUFM here as
            -- stronglyConnCompFromEdgedVertices is still deterministic even
            -- if the edges are in nondeterministic order as explained in
            -- Note [Deterministic SCC] in GHC.Data.Graph.Directed.
#endif

----------------------------
setEvBindsTcS :: EvBindsVar -> TcS a -> TcS a
setEvBindsTcS ref (TcS thing_inside)
 = TcS $ \ env -> thing_inside (env { tcs_ev_binds = ref })

nestImplicTcS :: EvBindsVar
              -> TcLevel -> TcS a
              -> TcS a
nestImplicTcS ref inner_tclvl (TcS thing_inside)
  = TcS $ \ TcSEnv { tcs_unified            = unified_var
                   , tcs_inerts             = old_inert_var
                   , tcs_count              = count
                   , tcs_unif_lvl           = unif_lvl
                   , tcs_abort_on_insoluble = abort_on_insoluble
                   } ->
    do { inerts <- TcM.readTcRef old_inert_var
       ; let nest_inert = inerts { inert_cycle_breakers = []
                                 , inert_cans = (inert_cans inerts)
                                                   { inert_given_eqs = False } }
                 -- All other InertSet fields are inherited
       ; new_inert_var <- TcM.newTcRef nest_inert
       ; new_wl_var    <- TcM.newTcRef emptyWorkList
       ; let nest_env = TcSEnv { tcs_count              = count     -- Inherited
                               , tcs_unif_lvl           = unif_lvl  -- Inherited
                               , tcs_ev_binds           = ref
                               , tcs_unified            = unified_var
                               , tcs_inerts             = new_inert_var
                               , tcs_abort_on_insoluble = abort_on_insoluble
                               , tcs_worklist           = new_wl_var }
       ; res <- TcM.setTcLevel inner_tclvl $
                thing_inside nest_env

       ; out_inert_set <- TcM.readTcRef new_inert_var
       ; restoreTyVarCycles out_inert_set

#if defined(DEBUG)
       -- Perform a check that the thing_inside did not cause cycles
       ; ev_binds <- TcM.getTcEvBindsMap ref
       ; checkForCyclicBinds ev_binds
#endif
       ; return res }

nestTcS ::  TcS a -> TcS a
-- Use the current untouchables, augmenting the current
-- evidence bindings, and solved dictionaries
-- But have no effect on the InertCans, or on the inert_famapp_cache
-- (we want to inherit the latter from processing the Givens)
nestTcS (TcS thing_inside)
  = TcS $ \ env@(TcSEnv { tcs_inerts = inerts_var }) ->
    do { inerts <- TcM.readTcRef inerts_var
       ; new_inert_var <- TcM.newTcRef inerts
       ; new_wl_var    <- TcM.newTcRef emptyWorkList
       ; let nest_env = env { tcs_inerts   = new_inert_var
                            , tcs_worklist = new_wl_var }

       ; res <- thing_inside nest_env

       ; new_inerts <- TcM.readTcRef new_inert_var

       -- we want to propagate the safe haskell failures
       ; let old_ic = inert_cans inerts
             new_ic = inert_cans new_inerts
             nxt_ic = old_ic { inert_safehask = inert_safehask new_ic }

       ; TcM.writeTcRef inerts_var  -- See Note [Propagate the solved dictionaries]
                        (inerts { inert_solved_dicts = inert_solved_dicts new_inerts
                                , inert_cans = nxt_ic })

       ; return res }

emitImplicationTcS :: TcLevel -> SkolemInfoAnon
                   -> [TcTyVar]        -- Skolems
                   -> [EvVar]          -- Givens
                   -> Cts              -- Wanteds
                   -> TcS TcEvBinds
-- Add an implication to the TcS monad work-list
emitImplicationTcS new_tclvl skol_info skol_tvs givens wanteds
  = do { let wc = emptyWC { wc_simple = wanteds }
       ; imp <- wrapTcS $
                do { ev_binds_var <- TcM.newTcEvBinds
                   ; imp <- TcM.newImplication
                   ; return (imp { ic_tclvl  = new_tclvl
                                 , ic_skols  = skol_tvs
                                 , ic_given  = givens
                                 , ic_wanted = wc
                                 , ic_binds  = ev_binds_var
                                 , ic_info   = skol_info }) }

       ; emitImplication imp
       ; return (TcEvBinds (ic_binds imp)) }

emitTvImplicationTcS :: TcLevel -> SkolemInfoAnon
                     -> [TcTyVar]        -- Skolems
                     -> Cts              -- Wanteds
                     -> TcS ()
-- Just like emitImplicationTcS but no givens and no bindings
emitTvImplicationTcS new_tclvl skol_info skol_tvs wanteds
  = do { let wc = emptyWC { wc_simple = wanteds }
       ; imp <- wrapTcS $
                do { ev_binds_var <- TcM.newNoTcEvBinds
                   ; imp <- TcM.newImplication
                   ; return (imp { ic_tclvl  = new_tclvl
                                 , ic_skols  = skol_tvs
                                 , ic_wanted = wc
                                 , ic_binds  = ev_binds_var
                                 , ic_info   = skol_info }) }

       ; emitImplication imp }


{- Note [Propagate the solved dictionaries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's really quite important that nestTcS does not discard the solved
dictionaries from the thing_inside.
Consider
   Eq [a]
   forall b. empty =>  Eq [a]
We solve the simple (Eq [a]), under nestTcS, and then turn our attention to
the implications.  It's definitely fine to use the solved dictionaries on
the inner implications, and it can make a significant performance difference
if you do so.
-}

-- Getters and setters of GHC.Tc.Utils.Env fields
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- Getter of inerts and worklist
getTcSInertsRef :: TcS (IORef InertSet)
getTcSInertsRef = TcS (return . tcs_inerts)

getTcSWorkListRef :: TcS (IORef WorkList)
getTcSWorkListRef = TcS (return . tcs_worklist)

getTcSInerts :: TcS InertSet
getTcSInerts = getTcSInertsRef >>= readTcRef

setTcSInerts :: InertSet -> TcS ()
setTcSInerts ics = do { r <- getTcSInertsRef; writeTcRef r ics }

getWorkListImplics :: TcS (Bag Implication)
getWorkListImplics
  = do { wl_var <- getTcSWorkListRef
       ; wl_curr <- readTcRef wl_var
       ; return (wl_implics wl_curr) }

pushLevelNoWorkList :: SDoc -> TcS a -> TcS (TcLevel, a)
-- Push the level and run thing_inside
-- However, thing_inside should not generate any work items
#if defined(DEBUG)
pushLevelNoWorkList err_doc (TcS thing_inside)
  = TcS (\env -> TcM.pushTcLevelM $
                 thing_inside (env { tcs_worklist = wl_panic })
        )
  where
    wl_panic  = pprPanic "GHC.Tc.Solver.Monad.buildImplication" err_doc
                         -- This panic checks that the thing-inside
                         -- does not emit any work-list constraints
#else
pushLevelNoWorkList _ (TcS thing_inside)
  = TcS (\env -> TcM.pushTcLevelM (thing_inside env))  -- Don't check
#endif

updWorkListTcS :: (WorkList -> WorkList) -> TcS ()
updWorkListTcS f
  = do { wl_var <- getTcSWorkListRef
       ; updTcRef wl_var f }

emitWorkNC :: [CtEvidence] -> TcS ()
emitWorkNC evs
  | null evs
  = return ()
  | otherwise
  = emitWork (map mkNonCanonical evs)

emitWork :: [Ct] -> TcS ()
emitWork [] = return ()   -- avoid printing, among other work
emitWork cts
  = do { traceTcS "Emitting fresh work" (vcat (map ppr cts))
       ; updWorkListTcS (extendWorkListCts cts) }

emitImplication :: Implication -> TcS ()
emitImplication implic
  = updWorkListTcS (extendWorkListImplic implic)

newTcRef :: a -> TcS (TcRef a)
newTcRef x = wrapTcS (TcM.newTcRef x)

readTcRef :: TcRef a -> TcS a
readTcRef ref = wrapTcS (TcM.readTcRef ref)

writeTcRef :: TcRef a -> a -> TcS ()
writeTcRef ref val = wrapTcS (TcM.writeTcRef ref val)

updTcRef :: TcRef a -> (a->a) -> TcS ()
updTcRef ref upd_fn = wrapTcS (TcM.updTcRef ref upd_fn)

getTcEvBindsVar :: TcS EvBindsVar
getTcEvBindsVar = TcS (return . tcs_ev_binds)

getTcLevel :: TcS TcLevel
getTcLevel = wrapTcS TcM.getTcLevel

getTcEvTyCoVars :: EvBindsVar -> TcS TyCoVarSet
getTcEvTyCoVars ev_binds_var
  = wrapTcS $ TcM.getTcEvTyCoVars ev_binds_var

getTcEvBindsMap :: EvBindsVar -> TcS EvBindMap
getTcEvBindsMap ev_binds_var
  = wrapTcS $ TcM.getTcEvBindsMap ev_binds_var

setTcEvBindsMap :: EvBindsVar -> EvBindMap -> TcS ()
setTcEvBindsMap ev_binds_var binds
  = wrapTcS $ TcM.setTcEvBindsMap ev_binds_var binds

unifyTyVar :: TcTyVar -> TcType -> TcS ()
-- Unify a meta-tyvar with a type
-- We keep track of how many unifications have happened in tcs_unified,
--
-- We should never unify the same variable twice!
unifyTyVar tv ty
  = assertPpr (isMetaTyVar tv) (ppr tv) $
    TcS $ \ env ->
    do { TcM.traceTc "unifyTyVar" (ppr tv <+> text ":=" <+> ppr ty)
       ; TcM.writeMetaTyVar tv ty
       ; TcM.updTcRef (tcs_unified env) (+1) }

reportUnifications :: TcS a -> TcS (Int, a)
reportUnifications (TcS thing_inside)
  = TcS $ \ env ->
    do { inner_unified <- TcM.newTcRef 0
       ; res <- thing_inside (env { tcs_unified = inner_unified })
       ; n_unifs <- TcM.readTcRef inner_unified
       ; TcM.updTcRef (tcs_unified env) (+ n_unifs)
       ; return (n_unifs, res) }

data TouchabilityTestResult
  -- See Note [Solve by unification] in GHC.Tc.Solver.Interact
  -- which points out that having TouchableSameLevel is just an optimisation;
  -- we could manage with TouchableOuterLevel alone (suitably renamed)
  = TouchableSameLevel
  | TouchableOuterLevel [TcTyVar]   -- Promote these
                        TcLevel     -- ..to this level
  | Untouchable

instance Outputable TouchabilityTestResult where
  ppr TouchableSameLevel            = text "TouchableSameLevel"
  ppr (TouchableOuterLevel tvs lvl) = text "TouchableOuterLevel" <> parens (ppr lvl <+> ppr tvs)
  ppr Untouchable                   = text "Untouchable"

touchabilityTest :: CtFlavour -> TcTyVar -> TcType -> TcS TouchabilityTestResult
-- This is the key test for untouchability:
-- See Note [Unification preconditions] in GHC.Tc.Utils.Unify
-- and Note [Solve by unification] in GHC.Tc.Solver.Interact
touchabilityTest flav tv1 rhs
  | flav /= Given  -- See Note [Do not unify Givens]
  , MetaTv { mtv_tclvl = tv_lvl, mtv_info = info } <- tcTyVarDetails tv1
  , canSolveByUnification info rhs
  = do { ambient_lvl  <- getTcLevel
       ; given_eq_lvl <- getInnermostGivenEqLevel

       ; if | tv_lvl `sameDepthAs` ambient_lvl
            -> return TouchableSameLevel

            | tv_lvl `deeperThanOrSame` given_eq_lvl   -- No intervening given equalities
            , all (does_not_escape tv_lvl) free_skols  -- No skolem escapes
            -> return (TouchableOuterLevel free_metas tv_lvl)

            | otherwise
            -> return Untouchable }
  | otherwise
  = return Untouchable
  where
     (free_metas, free_skols) = partition isPromotableMetaTyVar $
                                nonDetEltsUniqSet               $
                                tyCoVarsOfType rhs

     does_not_escape tv_lvl fv
       | isTyVar fv = tv_lvl `deeperThanOrSame` tcTyVarLevel fv
       | otherwise  = True
       -- Coercion variables are not an escape risk
       -- If an implication binds a coercion variable, it'll have equalities,
       -- so the "intervening given equalities" test above will catch it
       -- Coercion holes get filled with coercions, so again no problem.

{- Note [Do not unify Givens]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this GADT match
   data T a where
      T1 :: T Int
      ...

   f x = case x of
           T1 -> True
           ...

So we get f :: T alpha[1] -> beta[1]
          x :: T alpha[1]
and from the T1 branch we get the implication
   forall[2] (alpha[1] ~ Int) => beta[1] ~ Bool

Now, clearly we don't want to unify alpha:=Int!  Yet at the moment we
process [G] alpha[1] ~ Int, we don't have any given-equalities in the
inert set, and hence there are no given equalities to make alpha untouchable.

NB: if it were alpha[2] ~ Int, this argument wouldn't hold.  But that
never happens: invariant (GivenInv) in Note [TcLevel invariants]
in GHC.Tc.Utils.TcType.

Simple solution: never unify in Givens!
-}

getDefaultInfo ::  TcS ([Type], (Bool, Bool))
getDefaultInfo = wrapTcS TcM.tcGetDefaultTys

getWorkList :: TcS WorkList
getWorkList = do { wl_var <- getTcSWorkListRef
                 ; wrapTcS (TcM.readTcRef wl_var) }

selectNextWorkItem :: TcS (Maybe Ct)
-- Pick which work item to do next
-- See Note [Prioritise equalities]
selectNextWorkItem
  = do { wl_var <- getTcSWorkListRef
       ; wl <- readTcRef wl_var
       ; case selectWorkItem wl of {
           Nothing -> return Nothing ;
           Just (ct, new_wl) ->
    do { -- checkReductionDepth (ctLoc ct) (ctPred ct)
         -- This is done by GHC.Tc.Solver.Interact.chooseInstance
       ; writeTcRef wl_var new_wl
       ; return (Just ct) } } }

-- Just get some environments needed for instance looking up and matching
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

getInstEnvs :: TcS InstEnvs
getInstEnvs = wrapTcS $ TcM.tcGetInstEnvs

getFamInstEnvs :: TcS (FamInstEnv, FamInstEnv)
getFamInstEnvs = wrapTcS $ FamInst.tcGetFamInstEnvs

getTopEnv :: TcS HscEnv
getTopEnv = wrapTcS $ TcM.getTopEnv

getGblEnv :: TcS TcGblEnv
getGblEnv = wrapTcS $ TcM.getGblEnv

getLclEnv :: TcS TcLclEnv
getLclEnv = wrapTcS $ TcM.getLclEnv

tcLookupClass :: Name -> TcS Class
tcLookupClass c = wrapTcS $ TcM.tcLookupClass c

tcLookupId :: Name -> TcS Id
tcLookupId n = wrapTcS $ TcM.tcLookupId n

-- Setting names as used (used in the deriving of Coercible evidence)
-- Too hackish to expose it to TcS? In that case somehow extract the used
-- constructors from the result of solveInteract
addUsedGREs :: [GlobalRdrElt] -> TcS ()
addUsedGREs gres = wrapTcS  $ TcM.addUsedGREs gres

addUsedGRE :: Bool -> GlobalRdrElt -> TcS ()
addUsedGRE warn_if_deprec gre = wrapTcS $ TcM.addUsedGRE warn_if_deprec gre

keepAlive :: Name -> TcS ()
keepAlive = wrapTcS . TcM.keepAlive

-- Various smaller utilities [TODO, maybe will be absorbed in the instance matcher]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

checkWellStagedDFun :: CtLoc -> InstanceWhat -> PredType -> TcS ()
-- Check that we do not try to use an instance before it is available.  E.g.
--    instance Eq T where ...
--    f x = $( ... (\(p::T) -> p == p)... )
-- Here we can't use the equality function from the instance in the splice

checkWellStagedDFun loc what pred
  | TopLevInstance { iw_dfun_id = dfun_id } <- what
  , let bind_lvl = TcM.topIdLvl dfun_id
  , bind_lvl > impLevel
  = wrapTcS $ TcM.setCtLocM loc $
    do { use_stage <- TcM.getStage
       ; TcM.checkWellStaged pp_thing bind_lvl (thLevel use_stage) }

  | otherwise
  = return ()    -- Fast path for common case
  where
    pp_thing = text "instance for" <+> quotes (ppr pred)

pprEq :: TcType -> TcType -> SDoc
pprEq ty1 ty2 = pprParendType ty1 <+> char '~' <+> pprParendType ty2

isFilledMetaTyVar_maybe :: TcTyVar -> TcS (Maybe Type)
isFilledMetaTyVar_maybe tv = wrapTcS (TcM.isFilledMetaTyVar_maybe tv)

isFilledMetaTyVar :: TcTyVar -> TcS Bool
isFilledMetaTyVar tv = wrapTcS (TcM.isFilledMetaTyVar tv)

zonkTyCoVarsAndFV :: TcTyCoVarSet -> TcS TcTyCoVarSet
zonkTyCoVarsAndFV tvs = wrapTcS (TcM.zonkTyCoVarsAndFV tvs)

zonkTyCoVarsAndFVList :: [TcTyCoVar] -> TcS [TcTyCoVar]
zonkTyCoVarsAndFVList tvs = wrapTcS (TcM.zonkTyCoVarsAndFVList tvs)

zonkCo :: Coercion -> TcS Coercion
zonkCo = wrapTcS . TcM.zonkCo

zonkTcType :: TcType -> TcS TcType
zonkTcType ty = wrapTcS (TcM.zonkTcType ty)

zonkTcTypes :: [TcType] -> TcS [TcType]
zonkTcTypes tys = wrapTcS (TcM.zonkTcTypes tys)

zonkTcTyVar :: TcTyVar -> TcS TcType
zonkTcTyVar tv = wrapTcS (TcM.zonkTcTyVar tv)

zonkSimples :: Cts -> TcS Cts
zonkSimples cts = wrapTcS (TcM.zonkSimples cts)

zonkWC :: WantedConstraints -> TcS WantedConstraints
zonkWC wc = wrapTcS (TcM.zonkWC wc)

zonkTyCoVarKind :: TcTyCoVar -> TcS TcTyCoVar
zonkTyCoVarKind tv = wrapTcS (TcM.zonkTyCoVarKind tv)

----------------------------
pprKicked :: Int -> SDoc
pprKicked 0 = empty
pprKicked n = parens (int n <+> text "kicked out")

{- *********************************************************************
*                                                                      *
*              The Unification Level Flag                              *
*                                                                      *
********************************************************************* -}

{- Note [The Unification Level Flag]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a deep tree of implication constraints
   forall[1] a.                              -- Outer-implic
      C alpha[1]                               -- Simple
      forall[2] c. ....(C alpha[1])....        -- Implic-1
      forall[2] b. ....(alpha[1] ~ Int)....    -- Implic-2

The (C alpha) is insoluble until we know alpha.  We solve alpha
by unifying alpha:=Int somewhere deep inside Implic-2. But then we
must try to solve the Outer-implic all over again. This time we can
solve (C alpha) both in Outer-implic, and nested inside Implic-1.

When should we iterate solving a level-n implication?
Answer: if any unification of a tyvar at level n takes place
        in the ic_implics of that implication.

* What if a unification takes place at level n-1? Then don't iterate
  level n, because we'll iterate level n-1, and that will in turn iterate
  level n.

* What if a unification takes place at level n, in the ic_simples of
  level n?  No need to track this, because the kick-out mechanism deals
  with it.  (We can't drop kick-out in favour of iteration, because kick-out
  works for skolem-equalities, not just unifications.)

So the monad-global Unification Level Flag, kept in tcs_unif_lvl keeps
track of
  - Whether any unifications at all have taken place (Nothing => no unifications)
  - If so, what is the outermost level that has seen a unification (Just lvl)

The iteration done in the simplify_loop/maybe_simplify_again loop in GHC.Tc.Solver.

It helpful not to iterate unless there is a chance of progress.  #8474 is
an example:

  * There's a deeply-nested chain of implication constraints.
       ?x:alpha => ?y1:beta1 => ... ?yn:betan => [W] ?x:Int

  * From the innermost one we get a [D] alpha[1] ~ Int,
    so we can unify.

  * It's better not to iterate the inner implications, but go all the
    way out to level 1 before iterating -- because iterating level 1
    will iterate the inner levels anyway.

(In the olden days when we "floated" thse Derived constraints, this was
much, much more important -- we got exponential behaviour, as each iteration
produced the same Derived constraint.)
-}


resetUnificationFlag :: TcS Bool
-- We are at ambient level i
-- If the unification flag = Just i, reset it to Nothing and return True
-- Otherwise leave it unchanged and return False
resetUnificationFlag
  = TcS $ \env ->
    do { let ref = tcs_unif_lvl env
       ; ambient_lvl <- TcM.getTcLevel
       ; mb_lvl <- TcM.readTcRef ref
       ; TcM.traceTc "resetUnificationFlag" $
         vcat [ text "ambient:" <+> ppr ambient_lvl
              , text "unif_lvl:" <+> ppr mb_lvl ]
       ; case mb_lvl of
           Nothing       -> return False
           Just unif_lvl | ambient_lvl `strictlyDeeperThan` unif_lvl
                         -> return False
                         | otherwise
                         -> do { TcM.writeTcRef ref Nothing
                               ; return True } }

setUnificationFlag :: TcLevel -> TcS ()
-- (setUnificationFlag i) sets the unification level to (Just i)
-- unless it already is (Just j) where j <= i
setUnificationFlag lvl
  = TcS $ \env ->
    do { let ref = tcs_unif_lvl env
       ; mb_lvl <- TcM.readTcRef ref
       ; case mb_lvl of
           Just unif_lvl | lvl `deeperThanOrSame` unif_lvl
                         -> return ()
           _ -> TcM.writeTcRef ref (Just lvl) }


{- *********************************************************************
*                                                                      *
*                Instantiation etc.
*                                                                      *
********************************************************************* -}

-- Instantiations
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

instDFunType :: DFunId -> [DFunInstType] -> TcS ([TcType], TcThetaType)
instDFunType dfun_id inst_tys
  = wrapTcS $ TcM.instDFunType dfun_id inst_tys

newFlexiTcSTy :: Kind -> TcS TcType
newFlexiTcSTy knd = wrapTcS (TcM.newFlexiTyVarTy knd)

cloneMetaTyVar :: TcTyVar -> TcS TcTyVar
cloneMetaTyVar tv = wrapTcS (TcM.cloneMetaTyVar tv)

instFlexi :: [TKVar] -> TcS TCvSubst
instFlexi = instFlexiX emptyTCvSubst

instFlexiX :: TCvSubst -> [TKVar] -> TcS TCvSubst
instFlexiX subst tvs
  = wrapTcS (foldlM instFlexiHelper subst tvs)

instFlexiHelper :: TCvSubst -> TKVar -> TcM TCvSubst
instFlexiHelper subst tv
  = do { uniq <- TcM.newUnique
       ; details <- TcM.newMetaDetails TauTv
       ; let name = setNameUnique (tyVarName tv) uniq
             kind = substTyUnchecked subst (tyVarKind tv)
             ty'  = mkTyVarTy (mkTcTyVar name kind details)
       ; TcM.traceTc "instFlexi" (ppr ty')
       ; return (extendTvSubst subst tv ty') }

matchGlobalInst :: DynFlags
                -> Bool      -- True <=> caller is the short-cut solver
                             -- See Note [Shortcut solving: overlap]
                -> Class -> [Type] -> TcS TcM.ClsInstResult
matchGlobalInst dflags short_cut cls tys
  = wrapTcS (TcM.matchGlobalInst dflags short_cut cls tys)

tcInstSkolTyVarsX :: SkolemInfo -> TCvSubst -> [TyVar] -> TcS (TCvSubst, [TcTyVar])
tcInstSkolTyVarsX skol_info subst tvs = wrapTcS $ TcM.tcInstSkolTyVarsX skol_info subst tvs

-- Creating and setting evidence variables and CtFlavors
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data MaybeNew = Fresh CtEvidence | Cached EvExpr

isFresh :: MaybeNew -> Bool
isFresh (Fresh {})  = True
isFresh (Cached {}) = False

freshGoals :: [MaybeNew] -> [CtEvidence]
freshGoals mns = [ ctev | Fresh ctev <- mns ]

getEvExpr :: MaybeNew -> EvExpr
getEvExpr (Fresh ctev) = ctEvExpr ctev
getEvExpr (Cached evt) = evt

setEvBind :: EvBind -> TcS ()
setEvBind ev_bind
  = do { evb <- getTcEvBindsVar
       ; wrapTcS $ TcM.addTcEvBind evb ev_bind }

-- | Mark variables as used filling a coercion hole
useVars :: CoVarSet -> TcS ()
useVars co_vars
  = do { ev_binds_var <- getTcEvBindsVar
       ; let ref = ebv_tcvs ev_binds_var
       ; wrapTcS $
         do { tcvs <- TcM.readTcRef ref
            ; let tcvs' = tcvs `unionVarSet` co_vars
            ; TcM.writeTcRef ref tcvs' } }

-- | Equalities only
setWantedEq :: HasDebugCallStack => TcEvDest -> Coercion -> TcS ()
setWantedEq (HoleDest hole) co
  = do { useVars (coVarsOfCo co)
       ; fillCoercionHole hole co }
setWantedEq (EvVarDest ev) _ = pprPanic "setWantedEq" (ppr ev)

-- | Good for both equalities and non-equalities
setWantedEvTerm :: TcEvDest -> EvTerm -> TcS ()
setWantedEvTerm (HoleDest hole) tm
  | Just co <- evTermCoercion_maybe tm
  = do { useVars (coVarsOfCo co)
       ; fillCoercionHole hole co }
  | otherwise
  = -- See Note [Yukky eq_sel for a HoleDest]
    do { let co_var = coHoleCoVar hole
       ; setEvBind (mkWantedEvBind co_var tm)
       ; fillCoercionHole hole (mkTcCoVarCo co_var) }

setWantedEvTerm (EvVarDest ev_id) tm
  = setEvBind (mkWantedEvBind ev_id tm)

{- Note [Yukky eq_sel for a HoleDest]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
How can it be that a Wanted with HoleDest gets evidence that isn't
just a coercion? i.e. evTermCoercion_maybe returns Nothing.

Consider [G] forall a. blah => a ~ T
         [W] S ~# T

Then doTopReactEqPred carefully looks up the (boxed) constraint (S ~
T) in the quantified constraints, and wraps the (boxed) evidence it
gets back in an eq_sel to extract the unboxed (S ~# T).  We can't put
that term into a coercion, so we add a value binding
    h = eq_sel (...)
and the coercion variable h to fill the coercion hole.
We even re-use the CoHole's Id for this binding!

Yuk!
-}

fillCoercionHole :: CoercionHole -> Coercion -> TcS ()
fillCoercionHole hole co
  = do { wrapTcS $ TcM.fillCoercionHole hole co
       ; kickOutAfterFillingCoercionHole hole co }

setEvBindIfWanted :: CtEvidence -> EvTerm -> TcS ()
setEvBindIfWanted ev tm
  = case ev of
      CtWanted { ctev_dest = dest } -> setWantedEvTerm dest tm
      _                             -> return ()

newTcEvBinds :: TcS EvBindsVar
newTcEvBinds = wrapTcS TcM.newTcEvBinds

newNoTcEvBinds :: TcS EvBindsVar
newNoTcEvBinds = wrapTcS TcM.newNoTcEvBinds

newEvVar :: TcPredType -> TcS EvVar
newEvVar pred = wrapTcS (TcM.newEvVar pred)

newGivenEvVar :: CtLoc -> (TcPredType, EvTerm) -> TcS CtEvidence
-- Make a new variable of the given PredType,
-- immediately bind it to the given term
-- and return its CtEvidence
-- See Note [Bind new Givens immediately] in GHC.Tc.Types.Constraint
newGivenEvVar loc (pred, rhs)
  = do { new_ev <- newBoundEvVarId pred rhs
       ; return (CtGiven { ctev_pred = pred, ctev_evar = new_ev, ctev_loc = loc }) }

-- | Make a new 'Id' of the given type, bound (in the monad's EvBinds) to the
-- given term
newBoundEvVarId :: TcPredType -> EvTerm -> TcS EvVar
newBoundEvVarId pred rhs
  = do { new_ev <- newEvVar pred
       ; setEvBind (mkGivenEvBind new_ev rhs)
       ; return new_ev }

newGivenEvVars :: CtLoc -> [(TcPredType, EvTerm)] -> TcS [CtEvidence]
newGivenEvVars loc pts = mapM (newGivenEvVar loc) pts

emitNewWantedEq :: CtLoc -> Role -> TcType -> TcType -> TcS Coercion
-- | Emit a new Wanted equality into the work-list
emitNewWantedEq loc role ty1 ty2
  = do { (ev, co) <- newWantedEq loc role ty1 ty2
       ; updWorkListTcS (extendWorkListEq (mkNonCanonical ev))
       ; return co }

-- | Make a new equality CtEvidence
newWantedEq :: CtLoc -> Role -> TcType -> TcType
            -> TcS (CtEvidence, Coercion)
newWantedEq = newWantedEq_SI WDeriv

newWantedEq_SI :: ShadowInfo -> CtLoc -> Role
               -> TcType -> TcType
               -> TcS (CtEvidence, Coercion)
newWantedEq_SI si loc role ty1 ty2
  = do { hole <- wrapTcS $ TcM.newCoercionHole pty
       ; traceTcS "Emitting new coercion hole" (ppr hole <+> dcolon <+> ppr pty)
       ; return ( CtWanted { ctev_pred = pty, ctev_dest = HoleDest hole
                           , ctev_nosh = si
                           , ctev_loc = loc}
                , mkHoleCo hole ) }
  where
    pty = mkPrimEqPredRole role ty1 ty2

-- no equalities here. Use newWantedEq instead
newWantedEvVarNC :: CtLoc -> TcPredType -> TcS CtEvidence
newWantedEvVarNC = newWantedEvVarNC_SI WDeriv

newWantedEvVarNC_SI :: ShadowInfo -> CtLoc -> TcPredType -> TcS CtEvidence
-- Don't look up in the solved/inerts; we know it's not there
newWantedEvVarNC_SI si loc pty
  = do { new_ev <- newEvVar pty
       ; traceTcS "Emitting new wanted" (ppr new_ev <+> dcolon <+> ppr pty $$
                                         pprCtLoc loc)
       ; return (CtWanted { ctev_pred = pty, ctev_dest = EvVarDest new_ev
                          , ctev_nosh = si
                          , ctev_loc = loc })}

newWantedEvVar :: CtLoc -> TcPredType -> TcS MaybeNew
newWantedEvVar = newWantedEvVar_SI WDeriv

newWantedEvVar_SI :: ShadowInfo -> CtLoc -> TcPredType -> TcS MaybeNew
-- For anything except ClassPred, this is the same as newWantedEvVarNC
newWantedEvVar_SI si loc pty
  = do { mb_ct <- lookupInInerts loc pty
       ; case mb_ct of
            Just ctev
              | not (isDerived ctev)
              -> do { traceTcS "newWantedEvVar/cache hit" $ ppr ctev
                    ; return $ Cached (ctEvExpr ctev) }
            _ -> do { ctev <- newWantedEvVarNC_SI si loc pty
                    ; return (Fresh ctev) } }

newWanted :: CtLoc -> PredType -> TcS MaybeNew
-- Deals with both equalities and non equalities. Tries to look
-- up non-equalities in the cache
newWanted = newWanted_SI WDeriv

newWanted_SI :: ShadowInfo -> CtLoc -> PredType -> TcS MaybeNew
newWanted_SI si loc pty
  | Just (role, ty1, ty2) <- getEqPredTys_maybe pty
  = Fresh . fst <$> newWantedEq_SI si loc role ty1 ty2
  | otherwise
  = newWantedEvVar_SI si loc pty

-- deals with both equalities and non equalities. Doesn't do any cache lookups.
newWantedNC :: CtLoc -> PredType -> TcS CtEvidence
newWantedNC loc pty
  | Just (role, ty1, ty2) <- getEqPredTys_maybe pty
  = fst <$> newWantedEq loc role ty1 ty2
  | otherwise
  = newWantedEvVarNC loc pty

emitNewDeriveds :: CtLoc -> [TcPredType] -> TcS ()
emitNewDeriveds loc preds
  | null preds
  = return ()
  | otherwise
  = do { evs <- mapM (newDerivedNC loc) preds
       ; traceTcS "Emitting new deriveds" (ppr evs)
       ; updWorkListTcS (extendWorkListDeriveds evs) }

emitNewDerivedEq :: CtLoc -> Role -> TcType -> TcType -> TcS ()
-- Create new equality Derived and put it in the work list
-- There's no caching, no lookupInInerts
emitNewDerivedEq loc role ty1 ty2
  = do { ev <- newDerivedNC loc (mkPrimEqPredRole role ty1 ty2)
       ; traceTcS "Emitting new derived equality" (ppr ev $$ pprCtLoc loc)
       ; updWorkListTcS (extendWorkListEq (mkNonCanonical ev)) }
         -- Very important: put in the wl_eqs
         -- See Note [Prioritise equalities] in GHC.Tc.Solver.InertSet
         -- (Avoiding fundep iteration)

newDerivedNC :: CtLoc -> TcPredType -> TcS CtEvidence
newDerivedNC loc pred
  = return $ CtDerived { ctev_pred = pred, ctev_loc = loc }

-- --------- Check done in GHC.Tc.Solver.Interact.selectNewWorkItem???? ---------
-- | Checks if the depth of the given location is too much. Fails if
-- it's too big, with an appropriate error message.
checkReductionDepth :: CtLoc -> TcType   -- ^ type being reduced
                    -> TcS ()
checkReductionDepth loc ty
  = do { dflags <- getDynFlags
       ; when (subGoalDepthExceeded dflags (ctLocDepth loc)) $
         wrapErrTcS $
         solverDepthErrorTcS loc ty }

matchFam :: TyCon -> [Type] -> TcS (Maybe ReductionN)
matchFam tycon args = wrapTcS $ matchFamTcM tycon args

matchFamTcM :: TyCon -> [Type] -> TcM (Maybe ReductionN)
-- Given (F tys) return (ty, co), where co :: F tys ~N ty
matchFamTcM tycon args
  = do { fam_envs <- FamInst.tcGetFamInstEnvs
       ; let match_fam_result
              = reduceTyFamApp_maybe fam_envs Nominal tycon args
       ; TcM.traceTc "matchFamTcM" $
         vcat [ text "Matching:" <+> ppr (mkTyConApp tycon args)
              , ppr_res match_fam_result ]
       ; return match_fam_result }
  where
    ppr_res Nothing = text "Match failed"
    ppr_res (Just (Reduction co ty))
      = hang (text "Match succeeded:")
          2 (vcat [ text "Rewrites to:" <+> ppr ty
                  , text "Coercion:" <+> ppr co ])

{-
************************************************************************
*                                                                      *
              Breaking type variable cycles
*                                                                      *
************************************************************************
-}

-- | Conditionally replace all type family applications in the RHS with fresh
-- variables, emitting givens that relate the type family application to the
-- variable. See Note [Type variable cycles] in GHC.Tc.Solver.Canonical.
-- This only works under conditions as described in the Note; otherwise, returns
-- Nothing.
breakTyVarCycle_maybe :: CtEvidence
                      -> CheckTyEqResult   -- result of checkTypeEq
                      -> CanEqLHS
                      -> TcType     -- RHS
                      -> TcS (Maybe (TcTyVar, ReductionN))
                         -- new RHS that doesn't have any type families
                         -- TcTyVar is the LHS tv; convenient for the caller
breakTyVarCycle_maybe (ctLocOrigin . ctEvLoc -> CycleBreakerOrigin _) _ _ _
  -- see Detail (7) of Note
  = return Nothing

breakTyVarCycle_maybe ev cte_result (TyVarLHS lhs_tv) rhs
  | NomEq <- eq_rel

  , cte_result `cterHasOnlyProblem` cteSolubleOccurs
     -- only do this if the only problem is a soluble occurs-check
     -- See Detail (8) of the Note.

  = do { should_break <- final_check
       ; if should_break then do { redn <- go rhs
                                 ; return (Just (lhs_tv, redn)) }
                         else return Nothing }
  where
    flavour = ctEvFlavour ev
    eq_rel  = ctEvEqRel ev

    final_check
      | Given <- flavour
      = return True
      | ctFlavourContainsDerived flavour
      = do { result <- touchabilityTest Derived lhs_tv rhs
           ; return $ case result of
               Untouchable -> False
               _           -> True }
      | otherwise
      = return False

    -- This could be considerably more efficient. See Detail (5) of Note.
    go :: TcType -> TcS ReductionN
    go ty | Just ty' <- rewriterView ty = go ty'
    go (Rep.TyConApp tc tys)
      | isTypeFamilyTyCon tc  -- worried about whether this type family is not actually
                              -- causing trouble? See Detail (5) of Note.
      = do { let (fun_args, extra_args) = splitAt (tyConArity tc) tys
                 fun_app                = mkTyConApp tc fun_args
                 fun_app_kind           = tcTypeKind fun_app
           ; fun_redn <- emit_work fun_app_kind fun_app
           ; arg_redns <- unzipRedns <$> mapM go extra_args
           ; return $ mkAppRedns fun_redn arg_redns }
              -- Worried that this substitution will change kinds?
              -- See Detail (3) of Note

      | otherwise
      = do { arg_redns <- unzipRedns <$> mapM go tys
           ; return $ mkTyConAppRedn Nominal tc arg_redns }

    go (Rep.AppTy ty1 ty2)
      = mkAppRedn <$> go ty1 <*> go ty2
    go (Rep.FunTy vis w arg res)
      = mkFunRedn Nominal vis <$> go w <*> go arg <*> go res
    go (Rep.CastTy ty cast_co)
      = mkCastRedn1 Nominal ty cast_co <$> go ty
    go ty@(Rep.TyVarTy {})    = skip ty
    go ty@(Rep.LitTy {})      = skip ty
    go ty@(Rep.ForAllTy {})   = skip ty  -- See Detail (1) of Note
    go ty@(Rep.CoercionTy {}) = skip ty  -- See Detail (2) of Note

    skip ty = return $ mkReflRedn Nominal ty

    emit_work :: TcKind         -- of the function application
              -> TcType         -- original function application
              -> TcS ReductionN -- rewritten type (the fresh tyvar)
    emit_work fun_app_kind fun_app = case flavour of
      Given ->
        do { new_tv <- wrapTcS (TcM.newCycleBreakerTyVar fun_app_kind)
           ; let new_ty     = mkTyVarTy new_tv
                 given_pred = mkHeteroPrimEqPred fun_app_kind fun_app_kind
                                                 fun_app new_ty
                 given_term = evCoercion $ mkNomReflCo new_ty  -- See Detail (4) of Note
           ; new_given <- newGivenEvVar new_loc (given_pred, given_term)
           ; traceTcS "breakTyVarCycle replacing type family in Given" (ppr new_given)
           ; emitWorkNC [new_given]
           ; updInertTcS $ \is ->
               is { inert_cycle_breakers = (new_tv, fun_app) :
                                           inert_cycle_breakers is }
           ; return $ mkReflRedn Nominal new_ty }
                -- Why reflexive? See Detail (4) of the Note

      _derived_or_wd ->
        do { new_tv <- wrapTcS (TcM.newFlexiTyVar fun_app_kind)
           ; let new_ty = mkTyVarTy new_tv
           ; co <- emitNewWantedEq new_loc Nominal new_ty fun_app
           ; return $ mkReduction (mkSymCo co) new_ty }

      -- See Detail (7) of the Note
    new_loc = updateCtLocOrigin (ctEvLoc ev) CycleBreakerOrigin

-- does not fit scenario from Note
breakTyVarCycle_maybe _ _ _ _ = return Nothing

-- | Fill in CycleBreakerTvs with the variables they stand for.
-- See Note [Type variable cycles] in GHC.Tc.Solver.Canonical.
restoreTyVarCycles :: InertSet -> TcM ()
restoreTyVarCycles is
  = forM_ (inert_cycle_breakers is) $ \ (cycle_breaker_tv, orig_ty) ->
    TcM.writeMetaTyVar cycle_breaker_tv orig_ty

-- Unwrap a type synonym only when either:
--   The type synonym is forgetful, or
--   the type synonym mentions a type family in its expansion
-- See Note [Rewriting synonyms] in GHC.Tc.Solver.Rewrite.
rewriterView :: TcType -> Maybe TcType
rewriterView ty@(Rep.TyConApp tc _)
  | isForgetfulSynTyCon tc || (isTypeSynonymTyCon tc && not (isFamFreeTyCon tc))
  = tcView ty
rewriterView _other = Nothing

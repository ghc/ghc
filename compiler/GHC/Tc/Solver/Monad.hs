{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Monadic definitions for the constraint solver
module GHC.Tc.Solver.Monad (

    -- The TcS monad
    TcS(..), TcSEnv(..),
    runTcS, runTcSEarlyAbort, runTcSWithEvBinds, runTcSInerts,
    failTcS, warnTcS, addErrTcS, wrapTcS, ctLocWarnTcS,
    runTcSEqualities,
    nestTcS, nestImplicTcS, tryShortCutTcS, nestFunDepsTcS,
    setEvBindsTcS, setTcLevelTcS, updTcEvBinds,

    selectNextWorkItem,
    getWorkList,
    updWorkListTcS,
    pushLevelNoWorkList, pushTcLevelM_,

    runTcPluginTcS, recordUsedGREs,
    matchGlobalInst, TcM.ClsInstResult(..),

    QCInst(..),

    -- TcSMode
    TcSMode(..), getTcSMode, setTcSMode, vanillaTcSMode,

    -- The pipeline
    StopOrContinue(..), continueWith, stopWith,
    startAgainWith, SolverStage(Stage, runSolverStage), simpleStage,
    stopWithStage, nopStage,

    -- Tracing etc
    panicTcS, traceTcS, tryEarlyAbortTcS,
    traceFireTcS, bumpStepCountTcS, csTraceTcS,
    wrapErrTcS, wrapWarnTcS,

    -- Evidence creation and transformation
    MaybeNew(..), freshGoals, isFresh, getEvExpr,
    CanonicalEvidence(..),

    newTcEvBinds, newNoTcEvBinds,
    newWantedEq,
    newWanted,
    newWantedNC, newWantedEvVarNC,
    newBoundEvVarId,
    unifyTyVar, reportFineGrainUnifications, reportCoarseGrainUnifications,
    setEvBind, setWantedEq, setWantedDict, setEqIfWanted, setDictIfWanted,
    fillCoercionHole,
    newEvVar, newGivenEv, emitNewGivens,
    emitChildEqs, bumpReductionDepth,

    getInstEnvs, getFamInstEnvs,                -- Getting the environments
    getTopEnv, getGblEnv, getLclEnv, setSrcSpan,
    getTcEvBindsVar, getTcLevel,
    getTcEvTyCoVars, getTcEvBindsMap, setTcEvBindsMap,
    tcLookupClass, tcLookupId, tcLookupTyCon,

    -- Inerts
    updInertSet, updInertCans,
    getHasGivenEqs, setInertCans,
    getInertEqs, getInertCans, getInertGivens,
    getInertInsols, getInnermostGivenEqLevel,
    getInertSet, setInertSet,
    getUnsolvedInerts,
    removeInertCts, getPendingGivenScs,
    insertFunEq, addInertQCI,
    updInertDicts, updInertIrreds,
    emitWorkNC, emitWork,
    lookupInertDict,

    -- The Model
    recordUnification, kickOutRewritable, kickOutAfterUnification,

    -- Inert Safe Haskell safe-overlap failures
    insertSafeOverlapFailureTcS,
    getSafeOverlapFailures,

    -- Inert solved dictionaries
    getSolvedDicts, setSolvedDicts,
    updSolvedDicts, lookupSolvedDict,

    -- Irreds
    foldIrreds,

    -- The family application cache
    lookupFamAppInert, lookupFamAppCache, extendFamAppCache,
    pprKicked,

    -- Instantiation
    instDFunType,

    -- Unification
    wrapUnifier, wrapUnifierAndEmit, uPairsTcM,

    -- MetaTyVars
    newFlexiTcSTy, instFlexiX, instFlexiXTcM,
    cloneMetaTyVar,
    tcInstSkolTyVarsX,

    TcLevel,
    isFilledMetaTyVar_maybe, isFilledMetaTyVar, isUnfilledMetaTyVar,
    zonkTyCoVarsAndFV, zonkTcType, zonkTcTypes, zonkTcTyVar, zonkCo,
    zonkTyCoVarsAndFVList,
    zonkSimples, zonkWC,
    zonkTyCoVarKind,

    -- References
    newTcRef, readTcRef, writeTcRef, updTcRef,

    -- Misc
    getDefaultInfo, getDynFlags, getGlobalRdrEnvTcS,
    matchFam, matchFamTcM,
    checkWellLevelledDFun,
    pprEq,

    -- Enforcing invariants for type equalities
    checkTypeEq
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
       ( tcGetDefaultTys
       , tcLookupClass, tcLookupId, tcLookupTyCon
       )
import GHC.Tc.Zonk.Monad ( ZonkM )
import qualified GHC.Tc.Zonk.TcType  as TcM

import GHC.Driver.DynFlags

import GHC.Tc.Instance.Class( safeOverlap, instanceReturnsDictCon )
import GHC.Utils.Misc


import GHC.Tc.Solver.Types
import GHC.Tc.Solver.InertSet
import GHC.Tc.Errors.Types

import GHC.Tc.Utils.TcType
import GHC.Tc.Utils.Unify

import GHC.Tc.Types.Evidence
import GHC.Tc.Types
import GHC.Tc.Types.Origin
import GHC.Tc.Types.CtLoc
import GHC.Tc.Types.Constraint

import GHC.Builtin.Names ( callStackTyConName, exceptionContextTyConName )

import GHC.Core.Type
import GHC.Core.TyCo.Rep as Rep
import GHC.Core.TyCo.Tidy
import GHC.Core.Coercion
import GHC.Core.Coercion.Axiom( TypeEqn )
import GHC.Core.Predicate
import GHC.Core.Reduction
import GHC.Core.Class
import GHC.Core.TyCon
import GHC.Core.Unify (typesAreApart)

import GHC.LanguageExtensions as LangExt
import GHC.Rename.Env
import qualified GHC.Rename.Env as TcM

import GHC.Types.Name
import GHC.Types.TyThing
import GHC.Types.Name.Reader
import GHC.Types.DefaultEnv ( DefaultEnv )
import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Types.Unique.Supply
import GHC.Types.Id
import GHC.Types.Basic (allImportLevels)
import GHC.Types.ThLevelIndex (thLevelIndexFromImportLevel)
import GHC.Types.SrcLoc

import GHC.Unit.Module
import GHC.Unit.Module.Graph

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Logger

import GHC.Data.Bag as Bag
import GHC.Data.Pair

import GHC.Utils.Monad

import GHC.Exts (oneShot)

import Control.Monad
import Data.Foldable hiding ( foldr1 )
import Data.IORef
import qualified Data.Set as Set
import Data.Maybe( catMaybes )
import Data.List ( mapAccumL )

#if defined(DEBUG)
import GHC.Types.Unique.Set (nonDetEltsUniqSet)
import GHC.Data.Graph.Directed
#endif


{- *********************************************************************
*                                                                      *
               SolverStage and StopOrContinue
*                                                                      *
********************************************************************* -}

{- Note [The SolverStage monad]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The SolverStage monad allows us to write simple code like that in
GHC.Tc.Solver.solveEquality.   At the time of writing it looked like
this (may get out of date but the idea is clear):

solveEquality :: ... -> SolverStage Void
solveEquality ev eq_rel ty1 ty2
  = do { Pair ty1' ty2' <- zonkEqTypes ev eq_rel ty1 ty2
       ; mb_canon <- canonicaliseEquality ev' eq_rel ty1' ty2'
       ; case mb_canon of {
            Left irred_ct -> do { tryQCsIrredEqCt irred_ct
                                ; solveIrred irred_ct } ;
            Right eq_ct   -> do { tryInertEqs eq_ct
                                ; tryFunDeps  eq_ct
                                ; tryQCsEqCt  eq_ct
                                ; simpleStage (updInertEqs eq_ct)
                                ; stopWithStage (eqCtEvidence eq_ct) ".." }}}

Each sub-stage can elect to
  (a) ContinueWith: continue to the next stage
  (b) StartAgain:   start again at the beginning of the pipeline
  (c) Stop:         stop altogether; constraint is solved

These three possiblities are described by the `StopOrContinue` data type.
The `SolverStage` monad does the plumbing.

Notes:

(SM1) Each individual stage pretty quickly drops down into
         TcS (StopOrContinue a)
    because the monadic plumbing of `SolverStage` is relatively ineffienct,
    with that three-way split.

(SM2) We use `SolverStage Void` to express the idea that ContinueWith is
    impossible; we don't need to pattern match on it as a possible outcome:A
    see GHC.Tc.Solver.Solve.solveOne.   To that end, ContinueWith is strict.
-}

data StopOrContinue a
  = StartAgain Ct     -- Constraint is not solved, but some unifications
                      --   happened, so go back to the beginning of the pipeline

  | ContinueWith !a   -- The constraint was not solved, although it may have
                      --   been rewritten.  It is strict so that
                      --   ContinueWith Void can't happen; see (SM2) in
                      --   Note [The SolverStage monad]

  | Stop CtEvidence   -- The (rewritten) constraint was solved
         SDoc         -- Tells how it was solved
                      -- Any new sub-goals have been put on the work list
  deriving (Functor)

instance Outputable a => Outputable (StopOrContinue a) where
  ppr (Stop ev s)      = text "Stop" <> parens (s $$ text "ev:" <+> ppr ev)
  ppr (ContinueWith w) = text "ContinueWith" <+> ppr w
  ppr (StartAgain w)   = text "StartAgain" <+> ppr w

newtype SolverStage a = Stage { runSolverStage :: TcS (StopOrContinue a) }
  deriving( Functor )

instance Applicative SolverStage where
  pure x = Stage (return (ContinueWith x))
  (<*>)  = ap

instance Monad SolverStage where
  return          = pure
  (Stage m) >>= k = Stage $
                    do { soc <- m
                       ; case soc of
                           StartAgain x   -> return (StartAgain x)
                           Stop ev d      -> return (Stop ev d)
                           ContinueWith x -> runSolverStage (k x) }

nopStage :: a -> SolverStage a
nopStage res = Stage (continueWith res)

simpleStage :: TcS a -> SolverStage a
-- Always does a ContinueWith; no Stop or StartAgain
simpleStage thing = Stage (do { res <- thing; continueWith res })

startAgainWith :: Ct -> TcS (StopOrContinue a)
startAgainWith ct = return (StartAgain ct)

continueWith :: a -> TcS (StopOrContinue a)
continueWith ct = return (ContinueWith ct)

stopWith :: CtEvidence -> String -> TcS (StopOrContinue a)
stopWith ev s = return (Stop ev (text s))

stopWithStage :: CtEvidence -> String -> SolverStage a
stopWithStage ev s = Stage (stopWith ev s)


{- *********************************************************************
*                                                                      *
                   Inert instances: inert_qcis
*                                                                      *
********************************************************************* -}

addInertQCI :: QCInst -> TcS ()
-- Add a local quantified constraint, typically arising from a type signature
addInertQCI new_qci
  = do { ics  <- getInertCans
       ; ics1 <- add_qci ics

       -- Update given equalities. C.f updateGivenEqs
       ; tclvl <- getTcLevel
       ; let body_pred    = qci_body new_qci
             not_equality = isClassPred body_pred && not (isEqClassPred body_pred)
                  -- True <=> definitely not an equality
                  -- A qci_body like (f a) might be an equality

             ics2 | not_equality = ics1
                  | otherwise    = ics1 { inert_given_eq_lvl = tclvl
                                        , inert_given_eqs    = True }

       ; setInertCans ics2 }
  where
    add_qci :: InertCans -> TcS InertCans
    -- See Note [Do not add duplicate quantified instances]
    add_qci ics@(IC { inert_qcis = qcis })
      | any same_qci qcis
      = do { traceTcS "skipping duplicate quantified instance" (ppr new_qci)
           ; return ics }

      | otherwise
      = do { traceTcS "adding new inert quantified instance" (ppr new_qci)
           ; return (ics { inert_qcis = new_qci : qcis }) }

    same_qci old_qci = tcEqType (ctEvPred (qci_ev old_qci))
                                (ctEvPred (qci_ev new_qci))

{- Note [Do not add duplicate quantified instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As an optimisation, we avoid adding duplicate quantified instances to the
inert set; we use a simple duplicate check using tcEqType for simplicity,
even though it doesn't account for superficial differences, e.g. it will count
the following two constraints as different (#22223):

  - forall a b. C a b
  - forall b a. C a b

The main logic that allows us to pick local instances, even in the presence of
duplicates, is explained in Note [Use only the best matching quantified constraint]
in GHC.Tc.Solver.Dict.
-}

updInertDicts :: DictCt -> TcS ()
updInertDicts dict_ct
  = do { traceTcS "Adding inert dict" (ppr dict_ct)

       -- For Given implicit parameters (only), delete any existing
       -- Givens for the same implicit parameter.
       -- See Note [Shadowing of implicit parameters]
       ; deleteGivenIPs dict_ct

       -- Add the new constraint to the inert set
       ; updInertCans (updDicts (addDict dict_ct)) }

deleteGivenIPs :: DictCt -> TcS ()
-- Special magic when adding a Given implicit parameter to the inert set
-- For [G] ?x::ty, remove any existing /Givens/ mentioning ?x,
--    from /both/ inert_cans /and/ inert_solved_dicts (#23761)
-- See Note [Shadowing of implicit parameters]
deleteGivenIPs (DictCt { di_cls = cls, di_ev = ev, di_tys = tys })
  | isGiven ev
  , Just (str_ty, _) <- isIPPred_maybe cls tys
  = updInertSet $ \ inerts@(IS { inert_cans = ics, inert_solved_dicts = solved }) ->
    inerts { inert_cans         = updDicts (filterDicts (keep_can str_ty)) ics
           , inert_solved_dicts = filterDicts (keep_solved str_ty) solved }
  | otherwise
  = return ()
  where
    keep_can, keep_solved :: Type -> DictCt -> Bool
    -- keep_can: we keep an inert dictionary UNLESS
    --   (1) it is a Given
    --   (2) it binds an implicit parameter (?str :: ty) for the given 'str'
    --       regardless of 'ty', possibly via its superclasses
    -- The test is a bit conservative, hence `mightMentionIP` and `typesAreApart`
    -- See Note [Using typesAreApart when calling mightMentionIP]
    -- in GHC.Core.Predicate
    --
    -- keep_solved: same as keep_can, but for /all/ constraints not just Givens
    --
    -- Why two functions?  See (SIP3) in Note [Shadowing of implicit parameters]
    keep_can str (DictCt { di_ev = ev, di_cls = cls, di_tys = tys })
      = not (isGiven ev                -- (1)
          && mentions_ip str cls tys)  -- (2)
    keep_solved str (DictCt { di_cls = cls, di_tys = tys })
      = not (mentions_ip str cls tys)

    -- mentions_ip: the inert constraint might provide evidence
    -- for an implicit parameter (?str :: ty) for the given 'str'
    mentions_ip str cls tys
      = mightMentionIP (not . typesAreApart str) (const True) cls tys

updInertIrreds :: IrredCt -> TcS ()
updInertIrreds irred
  = do { tc_lvl <- getTcLevel
       ; updInertCans $ addIrredToCans tc_lvl irred }

{- *********************************************************************
*                                                                      *
                  Kicking out
*                                                                      *
************************************************************************
-}

{- Note [Kick out after filling a coercion hole]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we solve `kco` and have `co` in the inert set whose rewriter-set includes `kco`,
we should kick out `co` so that we can now unify it, which might unlock other stuff.
See Note [Unify only if the rewriter set is empty] in GHC.Tc.Solver.Equality for
why solving `kco` might unlock `co`, especially (URW2).

Hence `kickOutAfterFillingCoercionHole`.  It looks at inert constraints that are
  * Wanted
  * Of the form  alpha ~ rhs, where alpha is a unification variable
and kicks out any that will have an empty rewriter set after filling the hole.

Wrinkles:
(KOC1) If co's rewriter-set is {kco, xco}, there is no point in kicking it out,
       because it still can't be unified.  So we only kick out if the co's
       rewriter-set becomes empty.

(KOC2) `kco` might itself have a rewriter set.  This is fairly common.  E.g.
               kco : a[sk] ~ beta[tau]
       Assuming we can't unify it for some reason, we may swap that over to give
               kco2 : beta[tau] ~ a[sk]      kco := sym kco2
       So `kco` is "solved" but the coercion that solves it has free coercion holes!
       So rather than kick out `co`, we should just change its rewriter-set to depend
       on `kco2` instead of `kco`.  Hence the `new_holes` passed to
       `kickOutAfterFillingCoercionHole`

(KOC3) It's possible that this could just go ahead and unify, but could there
       be occurs-check problems? Seems simpler just to kick out.

(KOC4) Kick-out is undesirable, so it'd be better to solve `kco` before `co`.  So
       the solver prioritises equalities with an empty rewriter set, to try to
       avoid unnecessary kick-out.  See GHC.Tc.Types.Constraint
       Note [Prioritise Wanteds with empty CoHoleSet] esp (PER1)
-}

kickOutRewritable  :: KickOutSpec -> CtFlavourRole -> TcS ()
kickOutRewritable ko_spec new_fr
  = do { ics <- getInertCans
       ; let (kicked_out, ics') = kickOutRewritableLHS ko_spec new_fr ics
             n_kicked = lengthBag kicked_out
       ; setInertCans ics'

       ; unless (isEmptyBag kicked_out) $
         do { emitWork kicked_out

              -- The famapp-cache contains Given evidence from the inert set.
              -- If we're kicking out Givens, we need to remove this evidence
              -- from the cache, too.
            ; let kicked_given_ev_vars = foldr add_one emptyVarSet kicked_out
                  add_one :: Ct -> VarSet -> VarSet
                  add_one ct vs | CtGiven (GivenCt { ctev_evar = ev_var }) <- ctEvidence ct
                                = vs `extendVarSet` ev_var
                                | otherwise = vs

            ; when (new_fr `eqCanRewriteFR` (Given, NomEq) &&
                   -- if this isn't true, no use looking through the constraints
                    not (isEmptyVarSet kicked_given_ev_vars)) $
              do { traceTcS "Given(s) have been kicked out; drop from famapp-cache"
                            (ppr kicked_given_ev_vars)
                 ; dropFromFamAppCache kicked_given_ev_vars }

            ; csTraceTcS $
              hang (text "Kick out")
                 2 (vcat [ text "n-kicked =" <+> int n_kicked
                         , text "kicked_out =" <+> ppr kicked_out
                         , text "Residual inerts =" <+> ppr ics' ]) } }

kickOutAfterUnification :: TcTyVarSet -> TcS ()
kickOutAfterUnification tv_set
  | isEmptyVarSet tv_set
  = return ()
  | otherwise
  = do { n_kicked <- kickOutRewritable (KOAfterUnify tv_set) (Given, NomEq)
                     -- Given because the tv := xi is given; NomEq because
                     -- only nominal equalities are solved by unification
       ; traceTcS "kickOutAfterUnification" (ppr tv_set $$ text "n_kicked =" <+> ppr n_kicked)
       ; return n_kicked }

kickOutAfterFillingCoercionHole :: CoercionHole -> CoercionPlusHoles -> TcS ()
-- See Note [Kick out after filling a coercion hole]
kickOutAfterFillingCoercionHole hole (CPH { cph_holes = new_holes })
  = do { ics <- getInertCans
       ; let (kicked_out, ics') = kick_out ics
             n_kicked           = length kicked_out

       ; unless (n_kicked == 0) $
         do { updWorkListTcS (extendWorkListRewrittenEqs kicked_out)
            ; csTraceTcS $
              hang (text "Kick out, hole =" <+> ppr hole)
                 2 (vcat [ text "n-kicked =" <+> int n_kicked
                         , text "kicked_out =" <+> ppr kicked_out
                         , text "Residual inerts =" <+> ppr ics' ]) }

       ; setInertCans ics' }
  where
    kick_out :: InertCans -> ([EqCt], InertCans)
    kick_out ics@(IC { inert_eqs = eqs })
      = (eqs_to_kick, ics { inert_eqs = eqs_to_keep })
      where
        (eqs_to_kick, eqs_to_keep) = transformAndPartitionTyVarEqs kick_out_eq eqs

    kick_out_eq :: EqCt -> Either EqCt EqCt
    kick_out_eq eq_ct@(EqCt { eq_ev = ev, eq_lhs = lhs })
      | CtWanted (wev@(WantedCt { ctev_rewriters = rewriters })) <- ev
      , TyVarLHS tv <- lhs
      , isMetaTyVar tv
      , hole `elemCoHoleSet` rewriters
      , let holes' = (rewriters `delCoHoleSet` hole) `mappend` new_holes
                     -- Adding new_holes and deleting hole: see (KOC2)
            eq_ct' = eq_ct { eq_ev = CtWanted (wev { ctev_rewriters = holes' }) }
      = if isEmptyCoHoleSet holes'
        then Left eq_ct'    -- Kick out, if new rewriter set is empty (KOC1)
        else Right eq_ct'   -- Keep, but with trimmed holes see (KOC2)
      | otherwise
      = Right eq_ct

--------------
insertSafeOverlapFailureTcS :: InstanceWhat -> DictCt -> TcS ()
-- See Note [Safe Haskell Overlapping Instances Implementation] in GHC.Tc.Solver
insertSafeOverlapFailureTcS what item
  | safeOverlap what
  = return ()
  | otherwise
  = updInertSet (\is -> is { inert_safehask = addDict item (inert_safehask is) })

getSafeOverlapFailures :: TcS (Bag DictCt)
-- See Note [Safe Haskell Overlapping Instances Implementation] in GHC.Tc.Solver
getSafeOverlapFailures
 = do { IS { inert_safehask = safehask } <- getInertSet
      ; return $ foldDicts consBag safehask emptyBag }

--------------
updSolvedDicts :: InstanceWhat -> DictCt -> TcS ()
-- Conditionally add a new item in the solved set of the monad
-- See Note [Solved dictionaries] in GHC.Tc.Solver.InertSet
updSolvedDicts what dict_ct@(DictCt { di_cls = cls, di_tys = tys, di_ev = ev })
  | isWanted ev
  , instanceReturnsDictCon what
  = do { is_callstack    <- is_tyConTy isCallStackTy        callStackTyConName
       ; is_exceptionCtx <- is_tyConTy isExceptionContextTy exceptionContextTyConName
       ; let contains_callstack_or_exceptionCtx =
               mightMentionIP
                 (const True)
                    -- NB: the name of the call-stack IP is irrelevant
                    -- e.g (?foo :: CallStack) counts!
                 (is_callstack <||> is_exceptionCtx)
                 cls tys
       -- See Note [Don't add HasCallStack constraints to the solved set]
       ; unless contains_callstack_or_exceptionCtx $
    do { traceTcS "updSolvedDicts:" $ ppr dict_ct
       ; updInertSet $ \ ics ->
           ics { inert_solved_dicts = addSolvedDict dict_ct (inert_solved_dicts ics) }
       } }
  | otherwise
  = return ()
  where

    -- Return a predicate that decides whether a type is CallStack
    -- or ExceptionContext, accounting for e.g. type family reduction, as
    -- per Note [Using typesAreApart when calling mightMentionIP].
    --
    -- See Note [Using isCallStackTy in mightMentionIP].
    is_tyConTy :: (Type -> Bool) -> Name -> TcS (Type -> Bool)
    is_tyConTy is_eq tc_name
      = do { (mb_tc, _) <- wrapTcS $ TcM.tryTc $ TcM.tcLookupTyCon tc_name
           ; case mb_tc of
              Just tc ->
                return $ \ ty -> not (typesAreApart ty (mkTyConTy tc))
              Nothing ->
                return is_eq
           }

{- Note [Don't add HasCallStack constraints to the solved set]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We must not add solved Wanted dictionaries that mention HasCallStack constraints
to the solved set, or we might fail to accumulate the proper call stack, as was
reported in #25529.

Recall that HasCallStack constraints (and the related HasExceptionContext
constraints) are implicit parameter constraints, and are accumulated as per
Note [Overview of implicit CallStacks] in GHC.Tc.Types.Evidence.

When we solve a Wanted that contains a HasCallStack constraint, we don't want
to cache the result, because re-using that solution means re-using the call-stack
in a different context!

See also Note [Shadowing of implicit parameters], which deals with a similar
problem with Given implicit parameter constraints.

Note [Using isCallStackTy in mightMentionIP]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To implement Note [Don't add HasCallStack constraints to the solved set],
we need to check whether a constraint contains a HasCallStack or HasExceptionContext
constraint. We do this using the 'mentionsIP' function, but as per
Note [Using typesAreApart when calling mightMentionIP] we don't want to simply do:

  mightMentionIP
    (const True) -- (ignore the implicit parameter string)
    (isCallStackTy <||> isExceptionContextTy)

because this does not account for e.g. a type family that reduces to CallStack.
The predicate we want to use instead is:

    \ ty -> not (typesAreApart ty callStackTy && typesAreApart ty exceptionContextTy)

However, this is made difficult by the fact that CallStack and ExceptionContext
are not wired-in types; they are only known-key. This means we must look them
up using 'tcLookupTyCon'. However, this might fail, e.g. if we are in the middle
of typechecking ghc-internal and these data-types have not been typechecked yet!

In that case, we simply fall back to the naive 'isCallStackTy'/'isExceptionContextTy'
logic.

Note that it would be somewhat painful to wire-in ExceptionContext: at the time
of writing (March 2025), this would require wiring in the ExceptionAnnotation
class, as well as SomeExceptionAnnotation, which is a data type with existentials.
-}

getSolvedDicts :: TcS (DictMap DictCt)
getSolvedDicts = do { ics <- getInertSet; return (inert_solved_dicts ics) }

setSolvedDicts :: DictMap DictCt -> TcS ()
setSolvedDicts solved_dicts
  = updInertSet $ \ ics ->
    ics { inert_solved_dicts = solved_dicts }

{- *********************************************************************
*                                                                      *
                  Other inert-set operations
*                                                                      *
********************************************************************* -}

updInertSet :: (InertSet -> InertSet) -> TcS ()
-- Modify the inert set with the supplied function
updInertSet upd_fn
  = do { is_var <- getInertSetRef
       ; wrapTcS (do { curr_inert <- TcM.readTcRef is_var
                     ; TcM.writeTcRef is_var (upd_fn curr_inert) }) }

getInertCans :: TcS InertCans
getInertCans = do { inerts <- getInertSet; return (inert_cans inerts) }

setInertCans :: InertCans -> TcS ()
setInertCans ics = updInertSet $ \ inerts -> inerts { inert_cans = ics }

updRetInertCans :: (InertCans -> (a, InertCans)) -> TcS a
-- Modify the inert set with the supplied function
updRetInertCans upd_fn
  = do { is_var <- getInertSetRef
       ; wrapTcS (do { inerts <- TcM.readTcRef is_var
                     ; let (res, cans') = upd_fn (inert_cans inerts)
                     ; TcM.writeTcRef is_var (inerts { inert_cans = cans' })
                     ; return res }) }

updInertCans :: (InertCans -> InertCans) -> TcS ()
-- Modify the inert set with the supplied function
updInertCans upd_fn
  = updInertSet $ \ inerts -> inerts { inert_cans = upd_fn (inert_cans inerts) }

getInertEqs :: TcS InertEqs
getInertEqs = do { inert <- getInertCans; return (inert_eqs inert) }

getInnermostGivenEqLevel :: TcS TcLevel
getInnermostGivenEqLevel = do { inert <- getInertCans
                              ; return (inert_given_eq_lvl inert) }

-- | Retrieves all insoluble constraints from the inert set,
-- specifically including Given constraints.
--
-- This consists of:
--
--  - insoluble equalities, such as @Int ~# Bool@;
--  - constraints that are custom type errors, of the form
--    @TypeError msg@ or @Maybe (TypeError msg)@, but not constraints such as
--    @F x (TypeError msg)@ in which the type error is nested under
--    a type family application,
--  - unsatisfiable constraints, of the form @Unsatisfiable msg@.
--
-- The inclusion of Givens is important for pattern match warnings, as we
-- want to consider a pattern match that introduces insoluble Givens to be
-- redundant (see Note [Pattern match warnings with insoluble Givens] in GHC.Tc.Solver).
getInertInsols :: TcS Cts
getInertInsols
  -- See Note [When is a constraint insoluble?]
  = do { inert_cts <- getInertCts
       ; return $ filterBag insolubleCt inert_cts }

getInertCts :: TcS Cts
getInertCts
  = do { inerts <- getInertCans
       ; return $
          unionManyBags
            [ fmap CIrredCan $ inert_irreds inerts
            , foldDicts  (consBag . CDictCan) (inert_dicts  inerts) emptyBag
            , foldFunEqs (consBag . CEqCan  ) (inert_funeqs inerts) emptyBag
            , foldTyEqs  (consBag . CEqCan  ) (inert_eqs    inerts) emptyBag
            ] }

getInertGivens :: TcS [Ct]
-- Returns the Given constraints in the inert set
getInertGivens
  = do { all_cts <- getInertCts
       ; return (filter isGivenCt $ bagToList all_cts) }

getPendingGivenScs :: TcS [Ct]
-- Find all inert Given dictionaries, or quantified constraints, such that
--     1. cc_pend_sc flag has fuel strictly > 0
--     2. belongs to the current level
-- For each such dictionary:
-- * Return it (with unmodified cc_pend_sc) in sc_pending
-- * Modify the dict in the inert set to have cc_pend_sc = doNotExpand
--   to record that we have expanded superclasses for this dict
getPendingGivenScs = do { lvl <- getTcLevel
                        ; updRetInertCans (get_sc_pending lvl) }

get_sc_pending :: TcLevel -> InertCans -> ([Ct], InertCans)
get_sc_pending this_lvl ic@(IC { inert_dicts = dicts, inert_qcis = insts })
  = assertPpr (all isGivenCt sc_pending) (ppr sc_pending)
       -- When getPendingScDics is called,
       -- there are never any Wanteds in the inert set
    (sc_pending, ic { inert_dicts = dicts', inert_qcis = insts' })
  where
    sc_pending = sc_pend_insts ++ map CDictCan sc_pend_dicts

    sc_pend_dicts :: [DictCt]
    sc_pend_dicts = foldDicts get_pending dicts []
    dicts' = foldr exhaustAndAdd dicts sc_pend_dicts

    (sc_pend_insts, insts') = mapAccumL get_pending_inst [] insts

    exhaustAndAdd :: DictCt -> DictMap DictCt -> DictMap DictCt
    exhaustAndAdd ct dicts = addDict (ct {di_pend_sc = doNotExpand}) dicts
    -- Exhaust the fuel for this constraint before adding it as
    -- we don't want to expand these constraints again

    get_pending :: DictCt -> [DictCt] -> [DictCt]  -- Get dicts with cc_pend_sc > 0
    get_pending dict dicts
        | isPendingScDictCt dict
        , belongs_to_this_level (dictCtEvidence dict)
        = dict : dicts
        | otherwise
        = dicts

    get_pending_inst :: [Ct] -> QCInst -> ([Ct], QCInst)
    get_pending_inst cts qci@(QCI { qci_ev = ev })
       | Just qci' <- pendingScInst_maybe qci
       , belongs_to_this_level ev
       = (CQuantCan qci : cts, qci')
       -- qci' have their fuel exhausted
       -- we don't want to expand these constraints again
       -- qci is expanded
       | otherwise
       = (cts, qci)

    belongs_to_this_level ev = ctLocLevel (ctEvLoc ev) `sameDepthAs` this_lvl
    -- We only want Givens from this level; see (3a) in
    -- Note [The superclass story] in GHC.Tc.Solver.Dict

getUnsolvedInerts :: TcS Cts    -- All simple constraints
-- Return all the unsolved [Wanted] constraints
--
-- Post-condition: the returned simple constraints are all fully zonked
--                     (because they come from the inert set)
--                 the unsolved implics may not be
getUnsolvedInerts
 = do { IC { inert_eqs    = tv_eqs
           , inert_funeqs = fun_eqs
           , inert_irreds = irreds
           , inert_dicts  = idicts
           , inert_qcis   = qcis
           } <- getInertCans

      ; let unsolved_tv_eqs  = foldTyEqs  (add_if_unsolved CEqCan)    tv_eqs emptyCts
            unsolved_fun_eqs = foldFunEqs (add_if_unsolved CEqCan)    fun_eqs emptyCts
            unsolved_irreds  = foldr      (add_if_unsolved CIrredCan) emptyCts irreds
            unsolved_dicts   = foldDicts  (add_if_unsolved CDictCan)  idicts emptyCts
            unsolved_qcis    = foldr      (add_if_unsolved CQuantCan) emptyCts qcis

      ; traceTcS "getUnsolvedInerts" $
        vcat [ text " tv eqs =" <+> ppr unsolved_tv_eqs
             , text "fun eqs =" <+> ppr unsolved_fun_eqs
             , text "dicts =" <+> ppr unsolved_dicts
             , text "irreds =" <+> ppr unsolved_irreds ]

      ; return ( unsolved_tv_eqs  `unionBags`
                 unsolved_fun_eqs `unionBags`
                 unsolved_irreds  `unionBags`
                 unsolved_dicts   `unionBags`
                 unsolved_qcis ) }
  where
    add_if_unsolved :: (a -> Ct) -> a -> Cts -> Cts
    add_if_unsolved mk_ct thing cts
      | isWantedCt ct = ct `consCts` cts
      | otherwise     = cts
      where
        ct = mk_ct thing


getHasGivenEqs :: TcLevel             -- TcLevel of this implication
               -> TcS ( HasGivenEqs   -- are there Given equalities?
                      , InertIrreds ) -- Insoluble equalities arising from givens
-- See Note [Tracking Given equalities] in GHC.Tc.Solver.InertSet
getHasGivenEqs tclvl
  = do { inerts@(IC { inert_irreds       = irreds
                    , inert_given_eqs    = given_eqs
                    , inert_given_eq_lvl = ge_lvl })
              <- getInertCans
       ; let given_insols = filterBag insoluble_given_equality irreds
                      -- Specifically includes ones that originated in some
                      -- outer context but were refined to an insoluble by
                      -- a local equality; so no level-check needed

             -- See Note [HasGivenEqs] in GHC.Tc.Types.Constraint, and
             -- Note [Tracking Given equalities] in GHC.Tc.Solver.InertSet
             has_ge | ge_lvl `sameDepthAs` tclvl = MaybeGivenEqs
                    | given_eqs                  = LocalGivenEqs
                    | otherwise                  = NoGivenEqs

       ; traceTcS "getHasGivenEqs" $
         vcat [ text "given_eqs:" <+> ppr given_eqs
              , text "ge_lvl:" <+> ppr ge_lvl
              , text "ambient level:" <+> ppr tclvl
              , text "Inerts:" <+> ppr inerts
              , text "Insols:" <+> ppr given_insols]
       ; return (has_ge, given_insols) }
  where
    insoluble_given_equality :: IrredCt -> Bool
    -- Check for unreachability; specifically do not include UserError/Unsatisfiable
    insoluble_given_equality (IrredCt { ir_ev = ev, ir_reason = reason })
       = isInsolubleReason reason && isGiven ev

removeInertCts :: [Ct] -> InertCans -> InertCans
-- ^ Remove inert constraints from the 'InertCans', for use when a
-- typechecker plugin wishes to discard a given.
removeInertCts cts icans = foldl' removeInertCt icans cts

removeInertCt :: InertCans -> Ct -> InertCans
removeInertCt is ct
  = case ct of
      CDictCan dict_ct -> is { inert_dicts = delDict dict_ct (inert_dicts is) }
      CEqCan    eq_ct  -> delEq    eq_ct is
      CIrredCan ir_ct  -> delIrred ir_ct is
      CQuantCan {}     -> panic "removeInertCt: CQuantCan"
      CNonCanonical {} -> panic "removeInertCt: CNonCanonical"

-- | Looks up a family application in the inerts.
lookupFamAppInert :: (CtFlavourRole -> Bool)  -- can it rewrite the target?
                  -> TyCon -> [Type] -> TcS (Maybe EqCt)
lookupFamAppInert rewrite_pred fam_tc tys
  = do { IS { inert_cans = IC { inert_funeqs = inert_funeqs } } <- getInertSet
       ; return (lookup_inerts inert_funeqs) }
  where
    lookup_inerts inert_funeqs
      = case findFunEq inert_funeqs fam_tc tys of
          Nothing              -> Nothing
          Just (ecl :: [EqCt]) -> find (rewrite_pred . eqCtFlavourRole) ecl

---------------------------
lookupFamAppCache :: TyCon -> [Type] -> TcS (Maybe Reduction)
lookupFamAppCache fam_tc tys
  = do { IS { inert_famapp_cache = famapp_cache } <- getInertSet
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
       ; updInertSet $ \ is@(IS { inert_famapp_cache = fc }) ->
            is { inert_famapp_cache = insertFunEq fc tc xi_args stuff } } }

-- Remove entries from the cache whose evidence mentions variables in the
-- supplied set
dropFromFamAppCache :: VarSet -> TcS ()
dropFromFamAppCache varset
  = updInertSet (\inerts@(IS { inert_famapp_cache = famapp_cache }) ->
                   inerts { inert_famapp_cache = filterTcAppMap check famapp_cache })
  where
    check :: Reduction -> Bool
    check redn
      = not (anyFreeVarsOfCo (`elemVarSet` varset) $ reductionCoercion redn)

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

-- | The mode for the constraint solving monad.
data TcSMode
  = TcSMode -- See Note [TcSMode], where each field is documented
            { tcsmResumable        :: Bool
                   -- ^ Do not restore type-equality cycles
            , tcsmEarlyAbort       :: Bool
                   -- ^ Abort early on insoluble constraints
            , tcsmSkipOverlappable :: Bool
                   -- ^ Do not select an OVERLAPPABLE instance
            , tcsmFullySolveQCIs   :: Bool
                   -- ^ Fully solve quantified constraints
            }

vanillaTcSMode :: TcSMode
vanillaTcSMode = TcSMode { tcsmResumable        = False
                         , tcsmEarlyAbort       = False
                         , tcsmSkipOverlappable = False
                         , tcsmFullySolveQCIs   = False }

instance Outputable TcSMode where
  ppr (TcSMode { tcsmResumable = pm, tcsmEarlyAbort = ea
               , tcsmSkipOverlappable = so, tcsmFullySolveQCIs = fs })
    = text "TcSMode" <> (braces $ cat $ punctuate comma $ catMaybes $
                         [ pp_one pm "Resumable", pp_one ea "EarlyAbort"
                         , pp_one so "SkipOverlappable", pp_one fs "FullySolveQCIs" ])
      -- We get something like TcSMode{EarlyAbort,FullySolveQCIs},
      -- mentioning just the flags that are on
    where
      pp_one True s  = Just (text s)
      pp_one False _ = Nothing

{- Note [TcSMode]
~~~~~~~~~~~~~~~~~
The constraint solver can operate in different modes:

* `tcsmResumable`: Used by the pattern match overlap checker.  The idea is that
  the returned InertSet will later be resumed, so we do not want to restore
  type-equality cycles See also Note [Type equality cycles] in GHC.Tc.Solver.Equality

* `tcsmEarlyAbort`: Abort (fail in the monad) as soon as we come across an
  insoluble constraint. This is used to fail-fast when checking for hole-fits.
  See Note [Speeding up valid hole-fits].

* `tcsmSkipOverlappable`: don't use OVERLAPPABLE instances.  Used by the
  short-cut solver.  See Note [Shortcut solving] in GHC.Tc.Solver.Dict

* `tcsmFullSolveQCIs`: fully solve quantified constraints, or leave them alone.
  Used (only) for SPECIALISE pragmas;
  see (NFS1) in Note [Handling new-form SPECIALISE pragmas] in GHC.Tc.Gen.Sig
-}

data TcSEnv
  = TcSEnv {
      tcs_ev_binds  :: EvBindsVar,

      tcs_what  :: WhatUnifications,
         -- Level of the outermost meta-tyvar that we have unified
         -- See Note [WhatUnifications] in GHC.Tc.Utils.Unify

      tcs_count     :: TcRef Int, -- Global step count

      tcs_inerts    :: TcRef InertSet, -- Current inert set

      -- | The mode of operation for the constraint solver.
      -- See Note [TcSMode]
      tcs_mode :: TcSMode,

      tcs_worklist :: TcRef WorkList
    }

---------------
newtype TcS a = TcS { unTcS :: TcSEnv -> TcM a }
  deriving (Functor)

instance MonadFix TcS where
  mfix k = TcS $ \env -> mfix (\x -> unTcS (k x) env)

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

liftZonkTcS :: ZonkM a -> TcS a
liftZonkTcS = wrapTcS . TcM.liftZonkM

wrap2TcS :: (TcM a -> TcM a) -> TcS a -> TcS a
wrap2TcS fn (TcS thing) = mkTcS $ \env -> fn (thing env)

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
panicTcS doc = pprPanic "GHC.Tc.Solver.Monad" doc

tryEarlyAbortTcS :: TcS ()
-- Abort (fail in the monad) if the mode is TcSEarlyAbort
tryEarlyAbortTcS
  = mkTcS (\env -> when (tcsmEarlyAbort (tcs_mode env)) TcM.failM)

-- | Emit a warning within the 'TcS' monad at the location given by the 'CtLoc'.
ctLocWarnTcS :: CtLoc -> TcRnMessage -> TcS ()
ctLocWarnTcS loc msg = wrapTcS $ TcM.setCtLocM loc $ TcM.addDiagnostic msg

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

-- | This variant of 'runTcS' will immediately fail upon encountering an
-- insoluble ct. See Note [Speeding up valid hole-fits]. Its one usage
-- site does not need the ev_binds, so we do not return them.
runTcSEarlyAbort :: TcS a -> TcM a
runTcSEarlyAbort tcs
  = do { ev_binds_var <- TcM.newTcEvBinds
       ; runTcSWithEvBinds' mode ev_binds_var tcs }
  where
    mode = vanillaTcSMode { tcsmEarlyAbort = True }

-- | This can deal only with equality constraints.
runTcSEqualities :: TcS a -> TcM a
runTcSEqualities thing_inside
  = do { ev_binds_var <- TcM.newNoTcEvBinds
       ; runTcSWithEvBinds ev_binds_var thing_inside }

-- | A variant of 'runTcS' that takes and returns an 'InertSet' for
-- later resumption of the 'TcS' session.
runTcSInerts :: InertSet -> TcS a -> TcM (a, InertSet)
runTcSInerts inerts tcs
  = do { ev_binds_var <- TcM.newTcEvBinds
       ; runTcSWithEvBinds' (vanillaTcSMode { tcsmResumable = True })
                             ev_binds_var $
         do { setInertSet inerts
            ; a <- tcs
            ; new_inerts <- getInertSet
            ; return (a, new_inerts) } }

runTcSWithEvBinds :: EvBindsVar
                  -> TcS a
                  -> TcM a
runTcSWithEvBinds = runTcSWithEvBinds' vanillaTcSMode

runTcSWithEvBinds' :: TcSMode
                   -> EvBindsVar
                   -> TcS a
                   -> TcM a
runTcSWithEvBinds' mode ev_binds_var thing_inside
  = do { step_count  <- TcM.newTcRef 0

       -- Make a fresh, empty inert set
       -- Subtle point: see (TGE6) in Note [Tracking Given equalities]
       --               in GHC.Tc.Solver.InertSet
       ; tc_lvl      <- TcM.getTcLevel
       ; inert_var   <- TcM.newTcRef (emptyInertSet tc_lvl)

       ; wl_var      <- TcM.newTcRef emptyWorkList
       ; unif_lvl_var <- TcM.newTcRef infiniteTcLevel
       ; let env = TcSEnv { tcs_ev_binds = ev_binds_var
                          , tcs_what     = WU_Coarse unif_lvl_var
                          , tcs_count    = step_count
                          , tcs_inerts   = inert_var
                          , tcs_mode     = mode
                          , tcs_worklist = wl_var }

             -- Run the computation
       ; res <- unTcS thing_inside env

       ; count <- TcM.readTcRef step_count
       ; when (count > 0) $
         csTraceTcM $ return (text "Constraint solver steps =" <+> int count)

       -- Restore tyvar cycles: see Note [Type equality cycles] in
       --                       GHC.Tc.Solver.Equality
       -- But /not/ when tcsmResumable is set: see Note [TcSMode]
       ; unless (tcsmResumable mode) $
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
    is_co_bind (EvBind { eb_lhs = b }) = isEqPred (varType b)

    edges :: [ Node EvVar EvBind ]
    edges = [ DigraphNode bind bndr (nonDetEltsUniqSet (nestedEvIdsOfTerm rhs))
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

setTcLevelTcS :: TcLevel -> TcS a -> TcS a
setTcLevelTcS lvl (TcS thing_inside)
 = TcS $ \ env -> TcM.setTcLevel lvl (thing_inside env)

{- Note [nestImplicTcS]
~~~~~~~~~~~~~~~~~~~~~~~
`nestImplicTcS` is used to build a nested scope when we begin solving an implication.

(NI1) One subtle point is that `nestImplicTcS` uses `resetInertCans` to
    initialise the `InertSet` of the nested scope to the `inert_givens` (/not/
    the `inert_cans`) of the current inert set.  It is super-important not to
    pollute the sub-solving problem with the unsolved Wanteds of the current
    scope.

    Whenever we do `solveSimpleGivens`, we snapshot the `inert_cans` into `inert_givens`.
    (At that moment there should be no Wanteds.)
-}

nestImplicTcS :: SkolemInfoAnon -> EvBindsVar
              -> TcLevel -> TcS a
              -> TcS a
-- See Note [nestImplicTcS]
nestImplicTcS skol_info ev_binds_var inner_tclvl (TcS thing_inside)
  = TcS $ \ env@(TcSEnv { tcs_inerts = old_inert_var }) ->
    do { old_inerts <- TcM.readTcRef old_inert_var
       ; let nest_inert = mk_nested_inerts old_inerts
       ; new_inert_var <- TcM.newTcRef nest_inert
       ; new_wl_var    <- TcM.newTcRef emptyWorkList
       ; let nest_env = env { tcs_ev_binds = ev_binds_var
                            , tcs_inerts   = new_inert_var
                            , tcs_worklist = new_wl_var }
       ; res <- TcM.setTcLevel inner_tclvl $
                thing_inside nest_env

       ; out_inert_set <- TcM.readTcRef new_inert_var
       ; restoreTyVarCycles out_inert_set

#if defined(DEBUG)
       -- Perform a check that the thing_inside did not cause cycles
       ; ev_binds <- TcM.getTcEvBindsMap ev_binds_var
       ; checkForCyclicBinds ev_binds
#endif
       ; return res }
  where
    mk_nested_inerts old_inerts
      -- For an implication that comes from a static form (static e),
      -- start with a completely empty inert set; in particular, no Givens
      -- See (SF3) in Note [Grand plan for static forms]
      -- in GHC.Iface.Tidy.StaticPtrTable
      | StaticFormSkol <- skol_info
      = emptyInertSet inner_tclvl

      | otherwise
      = pushCycleBreakerVarStack $
        resetInertCans           $  -- See (NI1) in Note [nestImplicTcS]
        old_inerts

nestFunDepsTcS :: TcS a -> TcS a
nestFunDepsTcS (TcS thing_inside)
  = TcS $ \ env@(TcSEnv { tcs_inerts = inerts_var }) ->
    do { inerts <- TcM.readTcRef inerts_var
       ; let nest_inerts = resetInertCans inerts
                 -- resetInertCans: like nestImplicTcS
       ; new_inert_var <- TcM.newTcRef nest_inerts
       ; new_wl_var    <- TcM.newTcRef emptyWorkList
       ; let nest_env = env { tcs_inerts   = new_inert_var
                            , tcs_worklist = new_wl_var }

       ; TcM.traceTc "nestFunDepsTcS {" empty
       ; res <- thing_inside nest_env
       ; TcM.traceTc "nestFunDepsTcS }" empty

       -- Unlike nestTcS, do /not/ do `updateInertsWith`; we are going to
       -- abandon everything about this sub-computation except its unifications

       ; return res }

nestTcS :: TcS a -> TcS a
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
                        -- Inherit tcs_ev_binds from caller

       ; res <- thing_inside nest_env

       ; new_inerts <- TcM.readTcRef new_inert_var
       ; TcM.updTcRef inerts_var (`updateInertsWith` new_inerts)

       ; return res }

tryShortCutTcS :: TcS Bool -> TcS Bool
-- Like nestTcS, but
--   (a) be a no-op if the nested computation returns False
--   (b) if (but only if) success, propagate nested bindings to the caller
-- Use only by the short-cut solver;
--   see Note [Shortcut solving] in GHC.Tc.Solver.Dict
tryShortCutTcS (TcS thing_inside)
  = TcS $ \ env@(TcSEnv { tcs_mode = mode
                        , tcs_inerts = inerts_var
                        , tcs_ev_binds = old_ev_binds_var }) ->
    do { -- Initialise a fresh inert set, with no Givens and no Wanteds
         --    (i.e. empty `inert_cans`)
         -- But inherit all the InertSet cache fields; in particular
         --  * the given_eq_lvl, so we don't accidentally unify a
         --    unification variable from outside a GADT match
         --  * the `solved_dicts`; see wrinkle (SCS3) of Note [Shortcut solving]
         --  * the `famapp_cache`; similarly
         old_inerts <- TcM.readTcRef inerts_var
       ; let given_eq_lvl = inert_given_eq_lvl (inert_cans old_inerts)
             new_inerts   = old_inerts { inert_cans = emptyInertCans given_eq_lvl }
       ; new_inert_var <- TcM.newTcRef new_inerts

       ; new_wl_var       <- TcM.newTcRef emptyWorkList
       ; new_ev_binds_var <- TcM.cloneEvBindsVar old_ev_binds_var
       ; let nest_env = env { tcs_mode     = mode { tcsmSkipOverlappable = True }
                            , tcs_ev_binds = new_ev_binds_var
                            , tcs_inerts   = new_inert_var
                            , tcs_worklist = new_wl_var }

       ; TcM.traceTc "tryTcS {" $
         vcat [ text "old_ev_binds:" <+> ppr old_ev_binds_var
              , text "new_ev_binds:" <+> ppr new_ev_binds_var
              , ppr old_inerts ]
       ; solved <- thing_inside nest_env
       ; TcM.traceTc "tryTcS }" (ppr solved)

       ; if not solved
         then return False
         else do {  -- Successfully solved
                   -- Add the new bindings to the existing ones
                 ; TcM.updTcEvBinds old_ev_binds_var new_ev_binds_var

                 -- Update the existing inert set
                 ; new_inerts <- TcM.readTcRef new_inert_var
                 ; TcM.updTcRef inerts_var (`updateInertsWith` new_inerts)

                 ; TcM.traceTc "tryTcS update" (ppr (inert_solved_dicts new_inerts))

                 ; return True } }

updateInertsWith :: InertSet -> InertSet -> InertSet
-- Update the current inert set with bits from a nested solve,
-- that finished with a new inert set
-- In particular, propagate:
--    - solved dictionaires; see Note [Propagate the solved dictionaries]
--    - Safe Haskell failures
updateInertsWith current_inerts
                 (IS { inert_solved_dicts = new_solved
                     , inert_safehask     = new_safehask })
  = current_inerts { inert_solved_dicts = new_solved
                   , inert_safehask     = new_safehask }

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

getTcSMode :: TcS TcSMode
getTcSMode = TcS (return . tcs_mode)

setTcSMode :: TcSMode -> TcS a -> TcS a
setTcSMode mode thing_inside
  = TcS (\env -> unTcS thing_inside (env { tcs_mode = mode }))

-- Getter of inerts and worklist
getInertSetRef :: TcS (IORef InertSet)
getInertSetRef = TcS (return . tcs_inerts)

getInertSet :: TcS InertSet
getInertSet = getInertSetRef >>= readTcRef

setInertSet :: InertSet -> TcS ()
setInertSet is = do { r <- getInertSetRef; writeTcRef r is }


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

getTcEvTyCoVars :: EvBindsVar -> TcS [TcCoercion]
getTcEvTyCoVars ev_binds_var
  = wrapTcS $ TcM.getTcEvTyCoVars ev_binds_var

getTcEvBindsMap :: EvBindsVar -> TcS EvBindMap
getTcEvBindsMap ev_binds_var
  = wrapTcS $ TcM.getTcEvBindsMap ev_binds_var

setTcEvBindsMap :: EvBindsVar -> EvBindMap -> TcS ()
setTcEvBindsMap ev_binds_var binds
  = wrapTcS $ TcM.setTcEvBindsMap ev_binds_var binds

updTcEvBinds :: EvBindsVar -> EvBindsVar -> TcS ()
updTcEvBinds evb nested_evb
  = wrapTcS $ TcM.updTcEvBinds evb nested_evb

getDefaultInfo ::  TcS (DefaultEnv, Bool)
getDefaultInfo = wrapTcS TcM.tcGetDefaultTys


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

setSrcSpan :: RealSrcSpan -> TcS a -> TcS a
setSrcSpan ss = wrap2TcS (TcM.setSrcSpan (RealSrcSpan ss mempty))

tcLookupClass :: Name -> TcS Class
tcLookupClass c = wrapTcS $ TcM.tcLookupClass c

tcLookupId :: Name -> TcS Id
tcLookupId n = wrapTcS $ TcM.tcLookupId n

tcLookupTyCon :: Name -> TcS TyCon
tcLookupTyCon n = wrapTcS $ TcM.tcLookupTyCon n

-- Any use of this function is a bit suspect, because it violates the
-- pure veneer of TcS. But it's just about warnings around unused imports
-- and local constructors (GHC will issue fewer warnings than it otherwise
-- might), so it's not worth losing sleep over.
recordUsedGREs :: Bag GlobalRdrElt -> TcS ()
recordUsedGREs gres
  = do { wrapTcS $ TcM.addUsedGREs NoDeprecationWarnings gre_list
         -- If a newtype constructor was imported, don't warn about not
         -- importing it...
       ; wrapTcS $ traverse_ (TcM.keepAlive . greName) gre_list }
         -- ...and similarly, if a newtype constructor was defined in the same
         -- module, don't warn about it being unused.
         -- See Note [Tracking unused binding and imports] in GHC.Tc.Utils.

  where
    gre_list = bagToList gres

-- Various smaller utilities [TODO, maybe will be absorbed in the instance matcher]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

checkWellLevelledDFun :: CtLoc -> InstanceWhat -> PredType -> TcS ()
-- Check that we do not try to use an instance before it is available.  E.g.
--    instance Eq T where ...
--    f x = $( ... (\(p::T) -> p == p)... )
-- Here we can't use the equality function from the instance in the splice

checkWellLevelledDFun loc what pred
  = do
      mbind_lvl <- checkWellLevelledInstanceWhat what
      case mbind_lvl of
        Just (bind_lvls, is_local) ->
          wrapTcS $ TcM.setCtLocM loc $ do
              { use_lvl <- thLevelIndex <$> TcM.getThLevel
              ; dflags <- getDynFlags
              ; checkCrossLevelClsInst dflags (LevelCheckInstance what pred) bind_lvls use_lvl is_local  }
        -- If no level information is returned for an InstanceWhat, then it's safe to use
        -- at any level.
        Nothing -> return ()


-- TODO: Unify this with checkCrossLevelLifting function
checkCrossLevelClsInst :: DynFlags -> LevelCheckReason
                       -> Set.Set ThLevelIndex -> ThLevelIndex
                       -> Bool -> TcM ()
checkCrossLevelClsInst dflags reason bind_lvls use_lvl_idx is_local
  -- If the Id is imported, then allow with ImplicitStagePersistence
  | not is_local
  , xopt LangExt.ImplicitStagePersistence dflags
  = return ()
  -- NB: Do this check after the ImplicitStagePersistence check, because
  -- it will do some computation to work out the levels of instances.
  | use_lvl_idx `Set.member` bind_lvls = return ()
  -- With ImplicitStagePersistence, using later than bound is fine
  | xopt LangExt.ImplicitStagePersistence dflags
  , any (use_lvl_idx >=) bind_lvls  = return ()
  | otherwise = TcM.addErrTc (TcRnBadlyLevelled reason bind_lvls use_lvl_idx Nothing ErrorWithoutFlag)



-- | Returns the ThLevel of evidence for the solved constraint (if it has evidence)
-- See Note [Well-levelled instance evidence]
checkWellLevelledInstanceWhat :: HasCallStack => InstanceWhat -> TcS (Maybe (Set.Set ThLevelIndex, Bool))
checkWellLevelledInstanceWhat what
  | TopLevInstance { iw_dfun_id = dfun_id } <- what
    = Just <$> checkNameVisibleLevels (idName dfun_id)
  | BuiltinTypeableInstance tc <- what
    -- The typeable instance is always defined in the same module as the TyCon.
    = Just <$> checkNameVisibleLevels (tyConName tc)
  | otherwise = return Nothing

-- | Check the levels at which the given name is visible, including a boolean
-- indicating if the name is local or not.
checkNameVisibleLevels :: Name -> TcS (Set.Set ThLevelIndex, Bool)
checkNameVisibleLevels name = do
  cur_mod <- extractModule <$> getGblEnv
  if nameIsLocalOrFrom cur_mod name
    then return (Set.singleton topLevelIndex, True)
    else do
      lvls <- checkModuleVisibleLevels (nameModule name)
      return (lvls, False)

{- Note [Using the module graph to compute TH level visiblities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When typechecking a module M, in order to implement GHC proposal #682
(see Note [Explicit Level Imports] in GHC.Tc.Gen.Head), we need to be able to
compute the Template Haskell levels that typeclass instances are visible at in M.

To do this, we use the "level 0 imports" module graph, which we query via
GHC.Unit.Module.Graph.mgQueryZero. For example, if we want all modules that are
visible at level -1 from M, we do the following:

  1. start with all the direct of M imports at level -1, i.e. the "splice imports"
  2. then look at all modules that are reachable from these using only level 0
     normal imports, using 'mgQueryZero'.

This works precisely because, as specified in the proposal, with -XNoImplicitStagePersistence,
modules only export items at level 0. In particular, instances are only exported
at level 0.

See the SI36 test for an illustration.
-}

-- | Check which TH levels a module is visable at
--
-- Used to check visibility of instances (do not use this for normal identifiers).
checkModuleVisibleLevels :: Module -> TcS (Set.Set ThLevelIndex)
checkModuleVisibleLevels check_mod = do
  cur_mod <- extractModule <$> getGblEnv
  hsc_env <- getTopEnv

  -- 0. The keys for the scope of the current module.
  let mkKey s m = (ModuleScope (moduleToMnk m NotBoot) s)
      cur_mod_scope_key s = mkKey s cur_mod

  -- 1. is_visible checks that a specific key is visible from the given level in the
  -- current module.
  let is_visible :: ImportLevel -> ZeroScopeKey -> Bool
      is_visible s k = mgQueryZero (hsc_mod_graph hsc_env) (cur_mod_scope_key s) k

  -- 2. The key we are looking for, either the module itself in the home package or the
  -- module unit id of the module we are checking.
  let instance_key = if moduleUnitId check_mod `Set.member` hsc_all_home_unit_ids hsc_env
                       then mkKey NormalLevel check_mod
                       else UnitScope (moduleUnitId check_mod)

  -- 3. For each level, check if the key is visible from that level.
  let lvls = [ thLevelIndexFromImportLevel lvl | lvl <- allImportLevels, is_visible lvl instance_key]
  return $ Set.fromList lvls

{-
Note [Well-levelled instance evidence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Evidence for instances must obey the same level restrictions as normal bindings.
In particular, it is forbidden to use an instance in a top-level splice in the
module which the instance is defined. This is because the evidence is bound at
the top-level and top-level definitions are forbidden from being using in top-level splices in
the same module.

For example, suppose you have a function..  foo :: Show a => Code Q a -> Code Q ()
then the following program is disallowed,

```
data T a = T a deriving (Show)

main :: IO ()
main =
  let x = $$(foo [|| T () ||])
  in return ()
```

because the `foo` function (used in a top-level splice) requires `Show T` evidence,
which is defined at the top-level and therefore fails with an error that we have violated
the stage restriction.

```
Main.hs:10:14: error: [GHC-28914]
    • Level error: instance for ‘Show (T ())’ is bound at level 0
      but used at level -1
    • In the expression: foo [|| T () ||]
      In the typed Template Haskell splice: $$(foo [|| T () ||])
      In the expression: $$(foo [|| T () ||])
   |
10 |   let x = $$(foo [|| T () ||])
   |              ^^^
```

Solving a `Typeable (T t1 ...tn)` constraint generates code that relies on
`$tcT`, the `TypeRep` for `T`; and we must check that this reference to `$tcT`
is well levelled.  It's easy to know the level of `$tcT`: for imported TyCons it
will be the level of the imported TyCon Name, and for local TyCons it will be `toplevel`.

Therefore the `InstanceWhat` type had to be extended with
a special case for `Typeable`, which recorded the TyCon the evidence was for and
could them be used to check that we were not attempting to evidence in a level incorrect
manner.

-}

pprEq :: TcType -> TcType -> SDoc
pprEq ty1 ty2 = pprParendType ty1 <+> char '~' <+> pprParendType ty2

isFilledMetaTyVar_maybe :: TcTyVar -> TcS (Maybe Type)
isFilledMetaTyVar_maybe tv = wrapTcS (TcM.isFilledMetaTyVar_maybe tv)

isFilledMetaTyVar :: TcTyVar -> TcS Bool
isFilledMetaTyVar tv = wrapTcS (TcM.isFilledMetaTyVar tv)

isUnfilledMetaTyVar :: TcTyVar -> TcS Bool
isUnfilledMetaTyVar tv = wrapTcS $ TcM.isUnfilledMetaTyVar tv

zonkTyCoVarsAndFV :: TcTyCoVarSet -> TcS TcTyCoVarSet
zonkTyCoVarsAndFV tvs = liftZonkTcS (TcM.zonkTyCoVarsAndFV tvs)

zonkTyCoVarsAndFVList :: [TcTyCoVar] -> TcS [TcTyCoVar]
zonkTyCoVarsAndFVList tvs = liftZonkTcS (TcM.zonkTyCoVarsAndFVList tvs)

zonkCo :: Coercion -> TcS Coercion
zonkCo = wrapTcS . fmap TcM.liftZonkM TcM.zonkCo

zonkTcType :: TcType -> TcS TcType
zonkTcType ty = liftZonkTcS (TcM.zonkTcType ty)

zonkTcTypes :: [TcType] -> TcS [TcType]
zonkTcTypes tys = liftZonkTcS (TcM.zonkTcTypes tys)

zonkTcTyVar :: TcTyVar -> TcS TcType
zonkTcTyVar tv = liftZonkTcS (TcM.zonkTcTyVar tv)

zonkSimples :: Cts -> TcS Cts
zonkSimples cts = liftZonkTcS (TcM.zonkSimples cts)

zonkWC :: WantedConstraints -> TcS WantedConstraints
zonkWC wc = liftZonkTcS (TcM.zonkWC wc)

zonkTyCoVarKind :: TcTyCoVar -> TcS TcTyCoVar
zonkTyCoVarKind tv = liftZonkTcS (TcM.zonkTyCoVarKind tv)

----------------------------
pprKicked :: Int -> SDoc
pprKicked 0 = empty
pprKicked n = parens (int n <+> text "kicked out")


{- *********************************************************************
*                                                                      *
*              The work list
*                                                                      *
********************************************************************* -}


getTcSWorkListRef :: TcS (IORef WorkList)
getTcSWorkListRef = TcS (return . tcs_worklist)

getWorkList :: TcS WorkList
getWorkList = do { wl_var <- getTcSWorkListRef
                 ; readTcRef wl_var }

updWorkListTcS :: (WorkList -> WorkList) -> TcS ()
updWorkListTcS f
  = do { wl_var <- getTcSWorkListRef
       ; updTcRef wl_var f }

emitWorkNC :: [CtEvidence] -> TcS ()
emitWorkNC evs
  | null evs
  = return ()
  | otherwise
  = emitWork (listToBag (map mkNonCanonical evs))

emitWork :: Cts -> TcS ()
emitWork cts
  | isEmptyBag cts    -- Avoid printing, among other work
  = return ()
  | otherwise
  = do { traceTcS "Emitting fresh work" (pprBag cts)
       ; updWorkListTcS (extendWorkListCts cts) }

selectNextWorkItem :: TcS (Maybe Ct)
-- Pick which work item to do next
-- See Note [Prioritise equalities]
--
-- Postcondition: if the returned item is a Wanted equality,
--                then its rewriter set is fully zonked.
--
-- Suppose a constraint c1 is rewritten by another, c2.  When c2
-- gets solved, c1 has no rewriters, and can be prioritised; see
-- Note [Prioritise Wanteds with empty CoHoleSet] in
-- GHC.Tc.Types.Constraint wrinkle (PER1)

-- ToDo: if wl_rw_eqs is long, we'll re-zonk it each time we pick
--       a new item from wl_rest.  Bad.
selectNextWorkItem
  = do { wl_var <- getTcSWorkListRef
       ; wl     <- readTcRef wl_var

       ; case wl of { WL { wl_eqs_N = eqs_N, wl_eqs_X = eqs_X
                         , wl_rw_eqs = rw_eqs, wl_rest = rest }
           | ct:cts <- eqs_N  -> pick_me ct (wl { wl_eqs_N  = cts })
           | ct:cts <- eqs_X  -> pick_me ct (wl { wl_eqs_X  = cts })
           | otherwise        -> try_rws [] rw_eqs
           where
             pick_me :: Ct -> WorkList -> TcS (Maybe Ct)
             pick_me ct new_wl
               = do { writeTcRef wl_var new_wl
                    ; return (Just ct) }

             -- try_rws looks through rw_eqs to find one that has an empty
             -- rewriter set, after zonking.  If none such, call try_rest.
             try_rws acc (ct:cts)
                = do { ct' <- liftZonkTcS (TcM.zonkCtCoHoleSet ct)
                     ; if ctHasNoRewriters ct'
                       then pick_me ct' (wl { wl_rw_eqs = cts ++ acc })
                       else try_rws (ct':acc) cts }
             try_rws acc [] = try_rest acc

             try_rest zonked_rws
               | ct:cts <- rest       = pick_me ct (wl { wl_rw_eqs = zonked_rws, wl_rest = cts })
               | ct:cts <- zonked_rws = pick_me ct (wl { wl_rw_eqs = cts })
               | otherwise            = return Nothing
     } }


pushTcLevelM_ :: TcS a -> TcS a
pushTcLevelM_ (TcS thing_inside)
  = TcS (\env -> TcM.pushTcLevelM_ (thing_inside env))

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


{- *********************************************************************
*                                                                      *
*              Tracking unifications in TcS
*                                                                      *
********************************************************************* -}

unifyTyVar :: TcTyVar -> TcType -> TcS ()
-- Unify a meta-tyvar with a type
-- We should never unify the same variable twice!
-- C.f. GHC.Tc.Utils.Unify.unifyTyVar
unifyTyVar tv ty
  = assertPpr (isMetaTyVar tv) (ppr tv) $
    do { liftZonkTcS (TcM.writeMetaTyVar tv ty)  -- Produces a trace message
       ; what_uni <- getWhatUnifications
       ; wrapTcS $ recordUnification what_uni tv }

reportFineGrainUnifications :: TcS a -> TcS (TcTyVarSet, a)
-- Record what unifications were done by thing_inside
-- Remember to propagate the information to the enclosing context
reportFineGrainUnifications (TcS thing_inside)
  = TcS $ \ env@(TcSEnv { tcs_what = outer_wu }) ->
    do { (unif_tvs, res) <- report_fine_grain_unifs env thing_inside
       ; recordUnifications outer_wu unif_tvs
       ; return (unif_tvs, res) }

reportCoarseGrainUnifications :: TcS a -> TcS (TcLevel, a)
-- Record whether any useful unifications are done by thing_inside
-- Specifically: return the TcLevel of the outermost (smallest level)
--   unification variable that has been unified, or infiniteTcLevel if none
-- Remember to propagate the information to the enclosing context
reportCoarseGrainUnifications (TcS thing_inside)
  = TcS $ \ env@(TcSEnv { tcs_what = outer_what }) ->
    case outer_what of
      WU_None -> report_coarse_grain_unifs env thing_inside

      WU_Coarse outer_ul_ref
        -> do { (inner_ul, res) <- report_coarse_grain_unifs env thing_inside

              -- Propagate to outer_ul_ref
              ; outer_ul <- TcM.readTcRef outer_ul_ref
              ; unless (inner_ul `deeperThanOrSame` outer_ul) $
                TcM.writeTcRef outer_ul_ref inner_ul

              ; TcM.traceTc "reportCoarse(Coarse)" $
                vcat [ text "outer_ul" <+> ppr outer_ul
                     , text "inner_ul" <+> ppr inner_ul]
              ; return (inner_ul, res) }

      WU_Fine outer_tvs_ref
        -> do { (unif_tvs,res) <- report_fine_grain_unifs env thing_inside

              -- Propagate to outer_tvs_rev
              ; TcM.updTcRef outer_tvs_ref (`unionVarSet` unif_tvs)

              ; let outermost_unif_lvl = minTcTyVarSetLevel unif_tvs
              ; TcM.traceTc "reportCoarse(Fine)" $
                vcat [ text "unif_tvs" <+> ppr unif_tvs
                     , text "unif_happened" <+> ppr outermost_unif_lvl ]
              ; return (outermost_unif_lvl, res) }

report_coarse_grain_unifs :: TcSEnv -> (TcSEnv -> TcM a)
                          -> TcM (TcLevel, a)
-- Returns the level number of the outermost
-- unification variable that is unified
report_coarse_grain_unifs env thing_inside
  = do { inner_ul_ref <- TcM.newTcRef infiniteTcLevel
       ; res <- thing_inside (env { tcs_what = WU_Coarse inner_ul_ref })
       ; inner_ul <- TcM.readTcRef inner_ul_ref
       ; TcM.traceTc "report_coarse" $
         text "inner_lvl ="   <+> ppr inner_ul
       ; return (inner_ul, res) }

report_fine_grain_unifs :: TcSEnv -> (TcSEnv -> TcM a)
                        -> TcM (TcTyVarSet, a)
report_fine_grain_unifs env thing_inside
  = do { unif_tvs_ref <- TcM.newTcRef emptyVarSet

       ; res <- thing_inside (env { tcs_what = WU_Fine unif_tvs_ref })

       ; unif_tvs    <- TcM.readTcRef unif_tvs_ref
       ; ambient_lvl <- TcM.getTcLevel

       -- Keep only variables from this or outer levels
       ; let is_interesting unif_tv
                = ambient_lvl `deeperThanOrSame` tcTyVarLevel unif_tv
             interesting_tvs = filterVarSet is_interesting unif_tvs
       ; TcM.traceTc "report_fine" (ppr unif_tvs $$ ppr interesting_tvs)
       ; return (interesting_tvs, res) }

getWhatUnifications :: TcS WhatUnifications
getWhatUnifications
  = TcS $ \env -> return (tcs_what env)


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

instFlexiX :: Subst -> [TKVar] -> TcS ([TcTyVar], Subst)
instFlexiX subst tvs = wrapTcS (instFlexiXTcM subst tvs)

instFlexiXTcM :: Subst -> [TKVar] -> TcM ([TcTyVar], Subst)
-- Makes fresh tyvar, extends the substitution, and the in-scope set
-- Takes account of the case [k::Type, a::k, ...],
-- where we must substitute for k in a's kind
instFlexiXTcM subst []
  = return ([], subst)
instFlexiXTcM subst (tv:tvs)
  = do { uniq <- TcM.newUnique
       ; details <- TcM.newMetaDetails TauTv
       ; let name   = setNameUnique (tyVarName tv) uniq
             kind   = substTyUnchecked subst (tyVarKind tv)
             tv'    = mkTcTyVar name kind details
             subst' = extendTvSubstWithClone subst tv tv'
       ; (tvs', subst'') <- instFlexiXTcM subst' tvs
       ; return (tv':tvs', subst'') }

matchGlobalInst :: DynFlags -> Class -> [Type] -> CtLoc -> TcS TcM.ClsInstResult
matchGlobalInst dflags cls tys loc
  = do { mode <- getTcSMode
       ; let skip_overlappable = tcsmSkipOverlappable mode
       ; wrapTcS $ TcM.matchGlobalInst dflags skip_overlappable cls tys (Just loc) }

tcInstSkolTyVarsX :: SkolemInfo -> Subst -> [TyVar] -> TcS (Subst, [TcTyVar])
tcInstSkolTyVarsX skol_info subst tvs = wrapTcS $ TcM.tcInstSkolTyVarsX skol_info subst tvs

-- Creating and setting evidence variables and CtFlavors
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data MaybeNew = Fresh WantedCtEvidence | Cached EvExpr

isFresh :: MaybeNew -> Bool
isFresh (Fresh {})  = True
isFresh (Cached {}) = False

freshGoals :: [MaybeNew] -> [WantedCtEvidence]
freshGoals mns = [ ctev | Fresh ctev <- mns ]

getEvExpr :: MaybeNew -> EvExpr
getEvExpr (Fresh ctev) = ctEvExpr (CtWanted ctev)
getEvExpr (Cached evt) = evt

setEvBind :: EvBind -> TcS ()
setEvBind ev_bind
  = do { evb <- getTcEvBindsVar
       ; wrapTcS $ TcM.addTcEvBind evb ev_bind }

setEqIfWanted :: CtEvidence -> CoercionPlusHoles -> TcS ()
setEqIfWanted ev co_plus_holes
  = case ev of
      CtWanted (WantedCt { ctev_dest = dest })
         -> setWantedEq dest co_plus_holes
      _  -> return ()

setWantedEq :: HasDebugCallStack => TcEvDest -> CoercionPlusHoles -> TcS ()
-- ^ Equalities only
setWantedEq dest co_plus_holes
  = case dest of
      HoleDest hole -> fillCoercionHole hole co_plus_holes
      EvVarDest ev  -> pprPanic "setWantedEq: EvVarDest" (ppr ev)

setDictIfWanted :: CtEvidence -> CanonicalEvidence -> EvTerm -> TcS ()
setDictIfWanted ev canonical tm
  = case ev of
      CtWanted (WantedCt { ctev_dest = dest })
         -> setWantedDict dest canonical tm
      _  -> return ()

setWantedDict :: TcEvDest -> CanonicalEvidence -> EvTerm -> TcS ()
-- ^ Dictionaries only
setWantedDict dest canonical tm
  = case dest of
      EvVarDest ev_id -> setEvBind (mkWantedEvBind ev_id canonical tm)
      HoleDest h      -> pprPanic "setWantedEq: HoleDest" (ppr h)

fillCoercionHole :: CoercionHole -> CoercionPlusHoles -> TcS ()
fillCoercionHole hole co_plus_holes@(CPH { cph_co = co })
  = do { ev_binds_var <- getTcEvBindsVar
       ; wrapTcS $ do { -- Record usage of the free vars of this coercion
                        TcM.updTcRef (ebv_tcvs ev_binds_var) (co :)
                      ; -- Fill the hole
                        TcM.fillCoercionHole hole co_plus_holes }
       ; kickOutAfterFillingCoercionHole hole co_plus_holes }

newTcEvBinds :: TcS EvBindsVar
newTcEvBinds = wrapTcS TcM.newTcEvBinds

newNoTcEvBinds :: TcS EvBindsVar
newNoTcEvBinds = wrapTcS TcM.newNoTcEvBinds

newEvVar :: TcPredType -> TcS EvVar
newEvVar pred = wrapTcS (TcM.newEvVar pred)

newGivenEv :: CtLoc -> (TcPredType, EvTerm) -> TcS GivenCtEvidence
-- Make a new variable of the given PredType,
-- immediately bind it to the given term, and return its CtEvidence
-- See Note [Bind new Givens immediately] in GHC.Tc.Types.Constraint
--
-- The `pred` can be an /equality predicate/ t1 ~# t2;
--   see (EQC1) in Note [Solving equality classes] in GHC.Tc.Solver.Dict
newGivenEv loc (pred, rhs)
  = do { new_ev <- newBoundEvVarId pred rhs
       ; return $ GivenCt { ctev_pred = pred, ctev_evar = new_ev, ctev_loc = loc } }

-- | Make a new 'Id' of the given type, bound (in the monad's EvBinds) to the
-- given term
newBoundEvVarId :: TcPredType -> EvTerm -> TcS EvVar
newBoundEvVarId pred rhs
  = do { new_ev <- newEvVar pred
       ; setEvBind (mkGivenEvBind new_ev rhs)
       ; return new_ev }

emitNewGivens :: CtLoc -> [(Role,TcCoercion)] -> TcS ()
emitNewGivens loc pts
  = do { traceTcS "emitNewGivens" (ppr pts)
       ; gs <- mapM (newGivenEv loc) $
                [ (mkEqPredRole role ty1 ty2, evCoercion co)
                | (role, co) <- pts
                , let Pair ty1 ty2 = coercionKind co
                , not (ty1 `tcEqType` ty2) ] -- Kill reflexive Givens at birth
       ; emitWorkNC (map CtGiven gs) }

emitChildEqs :: CtEvidence -> Cts -> TcS ()
-- Emit a bunch of equalities into the work list
-- See Note [Work-list ordering] in GHC.Tc.Solver.Equality
--
-- All the constraints in `cts` share the same rewriter set so,
-- rather than looking at it one by one, we pass it to
-- extendWorkListChildEqs; just a small optimisation.
emitChildEqs ev eqs
  | isEmptyBag eqs
  = return ()
  | otherwise
  = updWorkListTcS (extendWorkListChildEqs ev eqs)

-- | Create a new Wanted constraint holding a coercion hole
-- for an equality between the two types at the given 'Role'.
newWantedEq :: CtLoc -> CoHoleSet -> Role -> TcType -> TcType
            -> TcS (WantedCtEvidence, CoercionHole)
newWantedEq loc rewriters role ty1 ty2
  = do { hole <- wrapTcS $ TcM.newCoercionHole pty
       ; let wtd = WantedCt { ctev_pred      = pty
                            , ctev_dest      = HoleDest hole
                            , ctev_loc       = loc
                            , ctev_rewriters = rewriters }
       ; return (wtd, hole) }
  where
    pty = mkEqPredRole role ty1 ty2

-- | Create a new Wanted constraint holding an evidence variable.
--
-- Don't use this for equality constraints: use 'newWantedEq' instead.
newWantedEvVarNC :: CtLoc -> CoHoleSet
                 -> TcPredType -> TcS WantedCtEvidence
-- Don't look up in the solved/inerts; we know it's not there
newWantedEvVarNC loc rewriters pty
  = assertPpr (not (isEqPred pty)) (ppr pty) $
    do { new_ev <- newEvVar pty
       ; traceTcS "Emitting new wanted" (ppr new_ev <+> dcolon <+> ppr pty $$
                                         pprCtLoc loc)
       ; return $ WantedCt { ctev_pred      = pty
                           , ctev_dest      = EvVarDest new_ev
                           , ctev_loc       = loc
                           , ctev_rewriters = rewriters }
       }

-- | `newWantedEvVar` makes up a fresh evidence variable for the given predicate.
-- For ClassPred, check if this exact predicate type is in the solved dicts
--      But do /not/ look in the inert_cans, because doing so bypasses all
--      all the careful tests in tryInertDicts, leading to
--      #20666 (prohibitedSuperClassSolve) and #26117 (short-cut solve)
-- We do no such caching forIPs, holes, or errors; so for anything
-- except ClassPred, this is the same as newWantedEvVarNC
--
-- Don't use this for equality constraints: this function is only for
-- constraints with 'EvVarDest'.
newWantedEvVar :: CtLoc -> CoHoleSet
               -> TcPredType -> TcS MaybeNew
newWantedEvVar loc rewriters pty
  = case classifyPredType pty of
      EqPred {} -> pprPanic "newWantedEvVar: HoleDestPred" (ppr pty)
      ClassPred cls tys
        -> do { inerts <- getInertSet
              ; case lookupSolvedDict inerts cls tys of
                 Just ev -> do { traceTcS "newWantedEvVar/cache hit" $ ppr ev
                               ; return $ Cached (ctEvExpr ev) }
                 Nothing -> do { ev <- newWantedEvVarNC loc rewriters pty
                               ; return (Fresh ev) } }
      _other -> do { ev <- newWantedEvVarNC loc rewriters pty
                   ; return (Fresh ev) }

-- | Create a new Wanted constraint, potentially looking up
-- non-equality constraints in the cache instead of creating
-- a new one from scratch.
--
-- Deals with both equality and non-equality constraints.
newWanted :: CtLoc -> CoHoleSet -> PredType -> TcS MaybeNew
newWanted loc rewriters pty
  | Just (role, ty1, ty2) <- getEqPredTys_maybe pty
  = Fresh . fst <$> newWantedEq loc rewriters role ty1 ty2
  | otherwise
  = newWantedEvVar loc rewriters pty

-- | Create a new Wanted constraint.
--
-- Deals with both equality and non-equality constraints.
--
-- Does not attempt to re-use non-equality constraints that already
-- exist in the inert set.
newWantedNC :: CtLoc -> CoHoleSet -> PredType -> TcS WantedCtEvidence
newWantedNC loc rewriters pty
  | Just (role, ty1, ty2) <- getEqPredTys_maybe pty
  = fst <$> newWantedEq loc rewriters role ty1 ty2
  | otherwise
  = newWantedEvVarNC loc rewriters pty

-- | Checks if the depth of the given location is too much. Fails if
-- it's too big, with an appropriate error message.
bumpReductionDepth :: CtLoc
                   -> TcType   -- ^ type or constraint being reduced
                   -> TcS CtLoc
bumpReductionDepth loc ty
  = do { dflags <- getDynFlags
       ; when (subGoalDepthExceeded (reductionDepth dflags) (ctLocDepth loc)) $
         wrapErrTcS $ solverDepthError loc ty
       ; return (bumpCtLocDepth loc) }

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

solverDepthError :: CtLoc -> TcType -> TcM a
solverDepthError loc ty
  = TcM.setCtLocM loc $
    do { (ty, env0) <- TcM.liftZonkM $
           do { ty   <- TcM.zonkTcType ty
              ; env0 <- TcM.tcInitTidyEnv
              ; return (ty, env0) }
       ; let (tidy_env, tidy_ty)  = tidyOpenTypeX env0 ty
             msg = TcRnSolverDepthError tidy_ty depth
       ; TcM.failWithTcM (tidy_env, msg) }
  where
    depth = ctLocDepth loc

{-
************************************************************************
*                                                                      *
              Unification
*                                                                      *
************************************************************************

Note [wrapUnifier]
~~~~~~~~~~~~~~~~~~
When decomposing equalities we often create new wanted constraints for
(s ~ t).  But what if s=t?  Then it'd be faster to return Refl right away.

Rather than making an equality test (which traverses the structure of the type,
perhaps fruitlessly), we call uType (via wrapUnifier) to traverse the common
structure, and bales out when it finds a difference by creating a new deferred
Wanted constraint.  But where it succeeds in finding common structure, it just
builds a coercion to reflect it.

This is all much faster than creating a new constraint, putting it in the
work list, picking it out, canonicalising it, etc etc.
-}

uPairsTcM :: UnifyEnv -> [TypeEqn] -> TcM ()
uPairsTcM uenv eqns = mapM_ (\(Pair ty1 ty2) -> uType uenv ty1 ty2) eqns

wrapUnifierAndEmit :: CtEvidence -> Role
                   -> (UnifyEnv -> TcM TcCoercion)  -- Some calls to uType
                   -> TcS CoercionPlusHoles
-- Like wrapUnifier, but
--    emits any unsolved equalities into the work-list
--    kicks out any inert constraints that mention unified variables
--    returns a CoHoleSet describing the new unsolved goals
wrapUnifierAndEmit ev role do_unifications
  = do { (unifs, (co, eqs)) <- reportFineGrainUnifications $
                               wrapUnifier (ctEvRewriters ev) (ctEvLoc ev) role $
                               do_unifications

       -- Emit the deferred constraints
       ; emitChildEqs ev eqs

       -- Kick out any inert constraints mentioning the unified variables
       ; kickOutAfterUnification unifs

       ; return (CPH { cph_co = co, cph_holes = rewriterSetFromCts eqs }) }

wrapUnifier :: CoHoleSet -> CtLoc -> Role
             -> (UnifyEnv -> TcM a)  -- Some calls to uType
             -> TcS (a, Bag Ct)
-- Invokes the do_unifications argument, with a suitable UnifyEnv.
-- Very good short-cut when the two types are equal, or nearly so
--    See Note [wrapUnifier]
-- The (Bag Ct) are the deferred constraints; we emit them but
-- also return them
wrapUnifier rws loc role do_unifications
  = do { given_eq_lvl <- getInnermostGivenEqLevel
       ; what_uni     <- getWhatUnifications

       ; wrapTcS $
         do { defer_ref    <- TcM.newTcRef emptyBag
            ; let env = UE { u_role         = role
                           , u_given_eq_lvl = given_eq_lvl
                           , u_rewriters    = rws
                           , u_loc          = loc
                           , u_defer        = defer_ref
                           , u_what         = what_uni }
              -- u_rewriters: the rewriter set and location from
              -- the parent constraint `ev` are inherited in any
              -- new constraints spat out by the unifier
              --
              -- u_what: likewise inherit the WhatUnifications flag,
              --         so that unifications done here are visible
              --         to the caller

            ; res <- do_unifications env

            ; cts     <- TcM.readTcRef defer_ref
            ; return (res, cts) } }


{-
************************************************************************
*                                                                      *
              Breaking type variable cycles
*                                                                      *
************************************************************************
-}

checkTypeEq :: CtEvidence -> EqRel -> CanEqLHS -> TcType
            -> TcS (PuResult Ct Reduction)
-- Used for general CanEqLHSs, ones that do
-- not have a touchable type variable on the LHS (i.e. not unifying)
checkTypeEq ev eq_rel lhs rhs =
  case ev of
    CtGiven {} ->
      do { traceTcS "checkTypeEq {" (vcat [ text "lhs:" <+> ppr lhs
                                          , text "rhs:" <+> ppr rhs ])
         ; check_result <- wrapTcS (checkTyEqRhs given_flags rhs)
         ; traceTcS "checkTypeEq }" (ppr check_result)
         ; case check_result of
              PuFail reason -> return (PuFail reason)
              PuOK prs redn -> do { new_givens <- mapBagM mk_new_given prs
                                  ; emitWork new_givens
                                  ; updInertSet (addCycleBreakerBindings prs)
                                  ; return (pure redn) } }
    CtWanted {} -> wrapTcS (checkTyEqRhs wanted_flags rhs)
  where
    wanted_flags :: TyEqFlags TcM Ct
    wanted_flags = notUnifying_TEFTask occ_prob lhs
                   -- checkTypeEq deals only with the non-unifying case

    given_flags :: TyEqFlags TcM (TcTyVar, TcType)
    given_flags = wanted_flags { tef_fam_app = mkTEFA_Break ev eq_rel BreakGiven }
        -- TEFA_Break used for: [G] a ~ Maybe (F a)
        --                   or [W] F a ~ Maybe (F a)

    -- occ_prob: see Note [Occurs check and representational equality]
    occ_prob = case eq_rel of
                 NomEq  -> cteInsolubleOccurs
                 ReprEq -> cteSolubleOccurs

    ---------------------------
    mk_new_given :: (TcTyVar, TcType) -> TcS Ct
    mk_new_given (new_tv, fam_app)
      = mkNonCanonical . CtGiven <$> newGivenEv cb_loc (given_pred, given_term)
      where
        new_ty     = mkTyVarTy new_tv
        given_pred = mkNomEqPred fam_app new_ty
        given_term = evCoercion $ mkNomReflCo new_ty  -- See Detail (4) of Note

    -- See Detail (7) of the Note
    cb_loc = updateCtLocOrigin (ctEvLoc ev) CycleBreakerOrigin

-------------------------
-- | Fill in CycleBreakerTvs with the variables they stand for.
-- See Note [Type equality cycles] in GHC.Tc.Solver.Equality
restoreTyVarCycles :: InertSet -> TcM ()
restoreTyVarCycles is
  = TcM.liftZonkM
  $ forAllCycleBreakerBindings_ (inert_cycle_breakers is) TcM.writeMetaTyVar
{-# SPECIALISE forAllCycleBreakerBindings_ ::
      CycleBreakerVarStack -> (TcTyVar -> TcType -> ZonkM ()) -> ZonkM () #-}


{- Note [Occurs check and representational equality]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(a ~R# b a) is soluble if b later turns out to be Identity
So we treat this as a "soluble occurs check".

Note [Don't cycle-break Wanteds when not unifying]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consdier
  [W] a[2] ~ Maybe (F a[2])

Should we cycle-break this Wanted, thus?

  [W] a[2] ~ Maybe delta[2]
  [W] delta[2] ~ F a[2]

For a start, this is dodgy because we might just unify delta, thus undoing
what we have done, and getting an infinite loop in the solver.  Even if we
somehow prevented ourselves from doing so, is there any merit in the split?
Maybe: perhaps we can use that equality on `a` to unlock other constraints?
Consider
  type instance F (Maybe _) = Bool

  [G] g1: a ~ Maybe Bool
  [W] w1: a ~ Maybe (F a)

If we loop-break w1 to get
  [W] w1': a ~ Maybe gamma
  [W] w3:  gamma ~ F a
Now rewrite w3 with w1'
  [W] w3':  gamma ~ F (Maybe gamma)
Now use the type instance to get
  gamma := Bool
Now we are left with
  [W] w1': a ~ Maybe Bool
which we can solve from the Given.

BUT in this situation we could have rewritten the
/original/ Wanted from the Given, like this:
  [W] w1': Maybe Bool ~ Maybe (F (Maybe Bool))
and that is readily soluble.

In short: loop-breaking Wanteds, when we aren't unifying,
seems of no merit.  Hence TEFA_Recurse, rather than TEFA_Break,
in `wanted_flags` in `checkTypeEq`.
-}

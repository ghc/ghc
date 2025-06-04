{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Monadic definitions for the constraint solver
module GHC.Tc.Solver.Monad (

    -- The TcS monad
    TcS(..), TcSEnv(..), TcSMode(..),
    runTcS, runTcSEarlyAbort, runTcSWithEvBinds, runTcSInerts,
    runTcSSpecPrag,
    failTcS, warnTcS, addErrTcS, wrapTcS, ctLocWarnTcS,
    runTcSEqualities,
    nestTcS, nestImplicTcS, setEvBindsTcS, setTcLevelTcS,
    emitImplicationTcS, emitTvImplicationTcS,
    emitImplication,
    emitFunDepWanteds,

    selectNextWorkItem,
    getWorkList,
    updWorkListTcS,
    pushLevelNoWorkList,

    runTcPluginTcS, recordUsedGREs,
    matchGlobalInst, TcM.ClsInstResult(..),

    QCInst(..),

    -- The pipeline
    StopOrContinue(..), continueWith, stopWith,
    startAgainWith, SolverStage(Stage, runSolverStage), simpleStage,
    stopWithStage, nopStage,

    -- Tracing etc
    panicTcS, traceTcS, tryEarlyAbortTcS,
    traceFireTcS, bumpStepCountTcS, csTraceTcS,
    wrapErrTcS, wrapWarnTcS,
    resetUnificationFlag, setUnificationFlag,

    -- Evidence creation and transformation
    MaybeNew(..), freshGoals, isFresh, getEvExpr,
    CanonicalEvidence(..),

    newTcEvBinds, newNoTcEvBinds,
    newWantedEq, emitNewWantedEq,
    newWanted,
    newWantedNC, newWantedEvVarNC,
    newBoundEvVarId,
    unifyTyVar, reportUnifications,
    setEvBind, setWantedEq,
    setWantedEvTerm, setEvBindIfWanted,
    newEvVar, newGivenEvVar, emitNewGivens,
    checkReductionDepth,
    getSolvedDicts, setSolvedDicts,

    getInstEnvs, getFamInstEnvs,                -- Getting the environments
    getTopEnv, getGblEnv, getLclEnv, setSrcSpan,
    getTcEvBindsVar, getTcLevel,
    getTcEvTyCoVars, getTcEvBindsMap, setTcEvBindsMap,
    tcLookupClass, tcLookupId, tcLookupTyCon,

    getUnifiedRef,


    -- Inerts
    updInertSet, updInertCans,
    getHasGivenEqs, setInertCans,
    getInertEqs, getInertCans, getInertGivens,
    getInertInsols, getInnermostGivenEqLevel,
    getInertSet, setInertSet,
    getUnsolvedInerts,
    removeInertCts, getPendingGivenScs,
    insertFunEq, addInertForAll,
    updInertDicts, updInertIrreds,
    emitWorkNC, emitWork,
    lookupInertDict,

    -- The Model
    kickOutAfterUnification, kickOutRewritable,

    -- Inert Safe Haskell safe-overlap failures
    addInertSafehask, insertSafeOverlapFailureTcS, updInertSafehask,
    getSafeOverlapFailures,

    -- Inert solved dictionaries
    updSolvedDicts, lookupSolvedDict,

    -- Irreds
    foldIrreds,

    -- The family application cache
    lookupFamAppInert, lookupFamAppCache, extendFamAppCache,
    pprKicked,

    -- Instantiation
    instDFunType,

    -- Unification
    wrapUnifierX, wrapUnifierTcS, unifyFunDeps, uPairsTcM, unifyForAllBody,

    -- MetaTyVars
    newFlexiTcSTy, instFlexiX,
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
import GHC.Tc.Instance.FunDeps( FunDepEqn(..) )
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

import GHC.Builtin.Names ( unsatisfiableClassNameKey, callStackTyConName, exceptionContextTyConName )

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

import GHC.Types.Name
import GHC.Types.TyThing
import GHC.Types.Name.Reader
import GHC.Types.DefaultEnv ( DefaultEnv )
import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Types.Unique.Supply
import GHC.Types.Unique.Set( elementOfUniqSet )
import GHC.Types.Id
import GHC.Types.Basic (allImportLevels)
import GHC.Types.ThLevelIndex (thLevelIndexFromImportLevel)

import GHC.Unit.Module
import qualified GHC.Rename.Env as TcM

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
import Data.List ( mapAccumL )
import Data.List.NonEmpty ( nonEmpty )
import qualified Data.List.NonEmpty as NE
import qualified Data.Semigroup as S
import GHC.Types.SrcLoc
import GHC.Rename.Env
import GHC.LanguageExtensions as LangExt

#if defined(DEBUG)
import GHC.Types.Unique.Set (nonDetEltsUniqSet)
import GHC.Data.Graph.Directed
#endif

import qualified Data.Set as Set
import GHC.Unit.Module.Graph

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
  (a) ContinueWith: continue to the next stasge
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
updInertDicts dict_ct@(DictCt { di_cls = cls, di_ev = ev, di_tys = tys })
  = do { traceTcS "Adding inert dict" (ppr dict_ct $$ ppr cls  <+> ppr tys)

       ; if | isGiven ev, Just (str_ty, _) <- isIPPred_maybe cls tys
            -> -- See (SIP1) and (SIP2) in Note [Shadowing of implicit parameters]
               -- Update /both/ inert_cans /and/ inert_solved_dicts.
               updInertSet $ \ inerts@(IS { inert_cans = ics, inert_solved_dicts = solved }) ->
               inerts { inert_cans         = updDicts (filterDicts (does_not_mention_ip_for str_ty)) ics
                      , inert_solved_dicts = filterDicts (does_not_mention_ip_for str_ty) solved }
            | otherwise
            -> return ()
       -- Add the new constraint to the inert set
       ; updInertCans (updDicts (addDict dict_ct)) }
  where
    -- Does this class constraint or any of its superclasses mention
    -- an implicit parameter (?str :: ty) for the given 'str' and any type 'ty'?
    does_not_mention_ip_for :: Type -> DictCt -> Bool
    does_not_mention_ip_for str_ty (DictCt { di_cls = cls, di_tys = tys })
      = not $ mentionsIP (not . typesAreApart str_ty) (const True) cls tys
        -- See Note [Using typesAreApart when calling mentionsIP]
        -- in GHC.Core.Predicate

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


-----------------------------------------
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

kickOutAfterUnification :: [TcTyVar] -> TcS ()
kickOutAfterUnification tv_list = case nonEmpty tv_list of
    Nothing -> return ()
    Just tvs -> do
       { let tv_set = mkVarSet tv_list

       ; n_kicked <- kickOutRewritable (KOAfterUnify tv_set) (Given, NomEq)
                     -- Given because the tv := xi is given; NomEq because
                     -- only nominal equalities are solved by unification

       -- Set the unification flag if we have done outer unifications
       -- that might affect an earlier implication constraint
       ; let min_tv_lvl = foldr1 minTcLevel (NE.map tcTyVarLevel tvs)
       ; ambient_lvl <- getTcLevel
       ; when (ambient_lvl `strictlyDeeperThan` min_tv_lvl) $
         setUnificationFlag min_tv_lvl

       ; traceTcS "kickOutAfterUnification" (ppr tvs $$ text "n_kicked =" <+> ppr n_kicked)
       ; return n_kicked }

kickOutAfterFillingCoercionHole :: CoercionHole -> TcS ()
-- See Wrinkle (URW2) in Note [Unify only if the rewriter set is empty]
-- in GHC.Tc.Solver.Equality
--
-- It's possible that this could just go ahead and unify, but could there
-- be occurs-check problems? Seems simpler just to kick out.
kickOutAfterFillingCoercionHole hole
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
        (eqs_to_kick, eqs_to_keep) = partitionInertEqs kick_out_eq eqs

    kick_out_eq :: EqCt -> Bool    -- True: kick out; False: keep.
    kick_out_eq (EqCt { eq_ev = ev ,eq_lhs = lhs })
      | CtWanted (WantedCt { ctev_rewriters = RewriterSet rewriters }) <- ev
      , TyVarLHS tv <- lhs
      , isMetaTyVar tv
      = hole `elementOfUniqSet` rewriters
      | otherwise
      = False

--------------
addInertSafehask :: InertCans -> DictCt -> InertCans
addInertSafehask ics item
  = ics { inert_safehask = addDict item (inert_dicts ics) }

insertSafeOverlapFailureTcS :: InstanceWhat -> DictCt -> TcS ()
-- See Note [Safe Haskell Overlapping Instances Implementation] in GHC.Tc.Solver
insertSafeOverlapFailureTcS what item
  | safeOverlap what = return ()
  | otherwise        = updInertCans (\ics -> addInertSafehask ics item)

getSafeOverlapFailures :: TcS (Bag DictCt)
-- See Note [Safe Haskell Overlapping Instances Implementation] in GHC.Tc.Solver
getSafeOverlapFailures
 = do { IC { inert_safehask = safehask } <- getInertCans
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
               mentionsIP
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
    -- per Note [Using typesAreApart when calling mentionsIP].
    --
    -- See Note [Using isCallStackTy in mentionsIP].
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

Note [Using isCallStackTy in mentionsIP]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To implement Note [Don't add HasCallStack constraints to the solved set],
we need to check whether a constraint contains a HasCallStack or HasExceptionContext
constraint. We do this using the 'mentionsIP' function, but as per
Note [Using typesAreApart when calling mentionsIP] we don't want to simply do:

  mentionsIP
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

updInertSafehask :: (DictMap DictCt -> DictMap DictCt) -> TcS ()
-- Modify the inert set with the supplied function
updInertSafehask upd_fn
  = updInertCans $ \ ics -> ics { inert_safehask = upd_fn (inert_safehask ics) }

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
--  - constraints that are top-level custom type errors, of the form
--    @TypeError msg@, but not constraints such as @Eq (TypeError msg)@
--    in which the type error is nested;
--  - unsatisfiable constraints, of the form @Unsatisfiable msg@.
--
-- The inclusion of Givens is important for pattern match warnings, as we
-- want to consider a pattern match that introduces insoluble Givens to be
-- redundant (see Note [Pattern match warnings with insoluble Givens] in GHC.Tc.Solver).
getInertInsols :: TcS Cts
getInertInsols
  = do { inert <- getInertCans
       ; let insols = filterBag insolubleIrredCt (inert_irreds inert)
             unsats = findDictsByTyConKey (inert_dicts inert) unsatisfiableClassNameKey
       ; return $ fmap CDictCan unsats `unionBags` fmap CIrredCan insols }

getInertGivens :: TcS [Ct]
-- Returns the Given constraints in the inert set
getInertGivens
  = do { inerts <- getInertCans
       ; let all_cts = foldIrreds ((:) . CIrredCan) (inert_irreds inerts)
                     $ foldDicts  ((:) . CDictCan)  (inert_dicts inerts)
                     $ foldFunEqs ((:) . CEqCan)    (inert_funeqs inerts)
                     $ foldTyEqs  ((:) . CEqCan)    (inert_eqs inerts)
                     $ []
       ; return (filter isGivenCt all_cts) }

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
get_sc_pending this_lvl ic@(IC { inert_dicts = dicts, inert_insts = insts })
  = assertPpr (all isGivenCt sc_pending) (ppr sc_pending)
       -- When getPendingScDics is called,
       -- there are never any Wanteds in the inert set
    (sc_pending, ic { inert_dicts = dicts', inert_insts = insts' })
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

getUnsolvedInerts :: TcS ( Bag Implication
                         , Cts )   -- All simple constraints
-- Return all the unsolved [Wanted] constraints
--
-- Post-condition: the returned simple constraints are all fully zonked
--                     (because they come from the inert set)
--                 the unsolved implics may not be
getUnsolvedInerts
 = do { IC { inert_eqs     = tv_eqs
           , inert_funeqs  = fun_eqs
           , inert_irreds  = irreds
           , inert_dicts   = idicts
           } <- getInertCans

      ; let unsolved_tv_eqs  = foldTyEqs  (add_if_unsolved CEqCan)    tv_eqs emptyCts
            unsolved_fun_eqs = foldFunEqs (add_if_unsolved CEqCan)    fun_eqs emptyCts
            unsolved_irreds  = foldr      (add_if_unsolved CIrredCan) emptyCts irreds
            unsolved_dicts   = foldDicts  (add_if_unsolved CDictCan)  idicts emptyCts

      ; implics <- getWorkListImplics

      ; traceTcS "getUnsolvedInerts" $
        vcat [ text " tv eqs =" <+> ppr unsolved_tv_eqs
             , text "fun eqs =" <+> ppr unsolved_fun_eqs
             , text "dicts =" <+> ppr unsolved_dicts
             , text "irreds =" <+> ppr unsolved_irreds
             , text "implics =" <+> ppr implics ]

      ; return ( implics, unsolved_tv_eqs `unionBags`
                          unsolved_fun_eqs `unionBags`
                          unsolved_irreds `unionBags`
                          unsolved_dicts ) }
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

lookupInInerts :: CtLoc -> TcPredType -> TcS (Maybe CtEvidence)
-- Is this exact predicate type cached in the solved or canonicals of the InertSet?
lookupInInerts loc pty
  | ClassPred cls tys <- classifyPredType pty
  = do { inerts <- getInertSet
       ; let mb_solved = lookupSolvedDict inerts loc cls tys
             mb_inert  = fmap dictCtEvidence (lookupInertDict (inert_cans inerts) loc cls tys)
       ; return $ do -- Maybe monad
            found_ev <- mb_solved `mplus` mb_inert

            -- We're about to "solve" the wanted we're looking up, so we
            -- must make sure doing so wouldn't run afoul of
            -- Note [Solving superclass constraints] in GHC.Tc.TyCl.Instance.
            -- Forgetting this led to #20666.
            guard $ not (prohibitedSuperClassSolve (ctEvLoc found_ev) loc)

            return found_ev }
  | otherwise -- NB: No caching for equalities, IPs, holes, or errors
  = return Nothing

-- | Look up a dictionary inert.
lookupInertDict :: InertCans -> CtLoc -> Class -> [Type] -> Maybe DictCt
lookupInertDict (IC { inert_dicts = dicts }) loc cls tys
  = findDict dicts loc cls tys

-- | Look up a solved inert.
lookupSolvedDict :: InertSet -> CtLoc -> Class -> [Type] -> Maybe CtEvidence
-- Returns just if exactly this predicate type exists in the solved.
lookupSolvedDict (IS { inert_solved_dicts = solved }) loc cls tys
  = fmap dictCtEvidence (findDict solved loc cls tys)

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

-- | See Note [TcSMode]
data TcSMode
  = TcSVanilla    -- ^ Normal constraint solving
  | TcSPMCheck    -- ^ Used when doing patterm match overlap checks
  | TcSEarlyAbort -- ^ Abort early on insoluble constraints
  | TcSSpecPrag   -- ^ Fully solve all constraints
  deriving (Eq)

{- Note [TcSMode]
~~~~~~~~~~~~~~~~~
The constraint solver can operate in different modes:

* TcSVanilla: Normal constraint solving mode. This is the default.

* TcSPMCheck: Used by the pattern match overlap checker.
      Like TcSVanilla, but the idea is that the returned InertSet will
      later be resumed, so we do not want to restore type-equality cycles
      See also Note [Type equality cycles] in GHC.Tc.Solver.Equality

* TcSEarlyAbort: Abort (fail in the monad) as soon as we come across an
  insoluble constraint. This is used to fail-fast when checking for hole-fits.
  See Note [Speeding up valid hole-fits].

* TcSSpecPrag: Solve constraints fully or not at all. This is described in
  Note [TcSSpecPrag].

  This mode is currently used in one place only: when solving constraints
  arising from specialise pragmas.
  See Note [Fully solving constraints for specialisation] in GHC.Tc.Gen.Sig.
-}

data TcSEnv
  = TcSEnv {
      tcs_ev_binds    :: EvBindsVar,

      tcs_unified     :: IORef Int,
         -- The number of unification variables we have filled
         -- The important thing is whether it is non-zero, so it
         -- could equally well be a Bool instead of an Int.

      tcs_unif_lvl  :: IORef (Maybe TcLevel),
         -- The Unification Level Flag
         -- Outermost level at which we have unified a meta tyvar
         -- Starts at Nothing, then (Just i), then (Just j) where j<i
         -- See Note [The Unification Level Flag]

      tcs_count     :: IORef Int, -- Global step count

      tcs_inerts    :: IORef InertSet, -- Current inert set

      -- | The mode of operation for the constraint solver.
      -- See Note [TcSMode]
      tcs_mode :: TcSMode,

      tcs_worklist :: IORef WorkList
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
  = mkTcS (\env -> when (tcs_mode env == TcSEarlyAbort) TcM.failM)

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
       ; runTcSWithEvBinds' TcSEarlyAbort ev_binds_var tcs }

-- | Run the 'TcS' monad in 'TcSSpecPrag' mode, which either fully solves
-- individual Wanted quantified constraints or leaves them alone.
--
-- See Note [TcSSpecPrag].
runTcSSpecPrag :: EvBindsVar -> TcS a -> TcM a
runTcSSpecPrag ev_binds_var tcs
  = runTcSWithEvBinds' TcSSpecPrag ev_binds_var tcs

{- Note [TcSSpecPrag]
~~~~~~~~~~~~~~~~~~~~~
The TcSSpecPrag mode is a specialized constraint solving mode that guarantees
that Wanted quantified constraints are either:
  - Fully solved with no free evidence variables, or
  - Left completely untouched (no simplification at all)

Examples:

  * [W] forall x. Eq x => Eq (f x).

    In TcSSpecPrag mode, we **do not** process this quantified constraint by
    creating a new implication constraint; we leave it alone instead.

  * [W] Eq (Maybe Int).

    This constraint is solved fully, using two top-level Eq instances.

  * [W] forall x. Eq x => Eq [x].

    This constraint is solved fully as well, using the Eq instance for lists.

This functionality is crucially used by the specialiser, for which taking an
irreversible constraint solving steps is actively harmful, as described in
Note [Fully solving constraints for specialisation] in GHC.Tc.Gen.Sig.

Note that currently we **do not** refrain from using top-level instances,
even though we also can't run them in reverse; this isn't a problem for the
specialiser (which is currently the sole consumer of this functionality).

The implementation is as follows: in TcSFullySolveMode, when we are about to
solve a Wanted quantified constraint by emitting an implication, we call the
special function `solveCompletelyIfRequired`. This function recursively calls
the solver but in TcSVanilla mode (i.e. full-blown solving, with no restrictions).
If this recursive call manages to solve all the remaining constraints fully,
then we proceed with that outcome (i.e. we continue with that inert set, etc).
Otherwise, we discard everything that happened in the recursive call, and
continue with the original quantified constraint unchanged.

In the future, we could consider re-using this functionality for the short-cut
solver (see Note [Shortcut solving] in GHC.Tc.Solver.Dict), but we would have to
be wary of the performance implications.
-}

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
  runTcSWithEvBinds' TcSPMCheck ev_binds_var $ do
    setInertSet inerts
    a <- tcs
    new_inerts <- getInertSet
    return (a, new_inerts)

runTcSWithEvBinds :: EvBindsVar
                  -> TcS a
                  -> TcM a
runTcSWithEvBinds = runTcSWithEvBinds' TcSVanilla

runTcSWithEvBinds' :: TcSMode
                   -> EvBindsVar
                   -> TcS a
                   -> TcM a
runTcSWithEvBinds' mode ev_binds_var tcs
  = do { unified_var <- TcM.newTcRef 0
       ; step_count  <- TcM.newTcRef 0

       -- Make a fresh, empty inert set
       -- Subtle point: see (TGE6) in Note [Tracking Given equalities]
       --               in GHC.Tc.Solver.InertSet
       ; tc_lvl      <- TcM.getTcLevel
       ; inert_var   <- TcM.newTcRef (emptyInert tc_lvl)

       ; wl_var      <- TcM.newTcRef emptyWorkList
       ; unif_lvl_var <- TcM.newTcRef Nothing
       ; let env = TcSEnv { tcs_ev_binds           = ev_binds_var
                          , tcs_unified            = unified_var
                          , tcs_unif_lvl           = unif_lvl_var
                          , tcs_count              = step_count
                          , tcs_inerts             = inert_var
                          , tcs_mode               = mode
                          , tcs_worklist           = wl_var }

             -- Run the computation
       ; res <- unTcS tcs env

       ; count <- TcM.readTcRef step_count
       ; when (count > 0) $
         csTraceTcM $ return (text "Constraint solver steps =" <+> int count)

       -- Restore tyvar cycles: see Note [Type equality cycles] in
       --                       GHC.Tc.Solver.Equality
       -- But /not/ in TCsPMCheck mode: see Note [TcSMode]
       ; case mode of
            TcSPMCheck -> return ()
            _ -> do { inert_set <- TcM.readTcRef inert_var
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

setTcLevelTcS :: TcLevel -> TcS a -> TcS a
setTcLevelTcS lvl (TcS thing_inside)
 = TcS $ \ env -> TcM.setTcLevel lvl (thing_inside env)

nestImplicTcS :: EvBindsVar
              -> TcLevel -> TcS a
              -> TcS a
nestImplicTcS ref inner_tclvl (TcS thing_inside)
  = TcS $ \ TcSEnv { tcs_unified            = unified_var
                   , tcs_inerts             = old_inert_var
                   , tcs_count              = count
                   , tcs_unif_lvl           = unif_lvl
                   , tcs_mode               = mode
                   } ->
    do { inerts <- TcM.readTcRef old_inert_var
       ; let nest_inert = inerts { inert_cycle_breakers = pushCycleBreakerVarStack
                                                            (inert_cycle_breakers inerts)
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
                               , tcs_mode               = mode
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

getUnifiedRef :: TcS (IORef Int)
getUnifiedRef = TcS (return . tcs_unified)

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
       ; TcM.liftZonkM $ TcM.writeMetaTyVar tv ty
       ; TcM.updTcRef (tcs_unified env) (+1) }

reportUnifications :: TcS a -> TcS (Int, a)
-- Record how many unifications are done by thing_inside
-- We could return a Bool instead of an Int;
-- all that matters is whether it is no-zero
reportUnifications (TcS thing_inside)
  = TcS $ \ env ->
    do { inner_unified <- TcM.newTcRef 0
       ; res <- thing_inside (env { tcs_unified = inner_unified })
       ; n_unifs <- TcM.readTcRef inner_unified
       ; TcM.updTcRef (tcs_unified env) (+ n_unifs)
       ; return (n_unifs, res) }

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
Main.hs:12:14: error:
    • GHC stage restriction:
        instance for ‘Show
                        (T ())’ is used in a top-level splice, quasi-quote, or annotation,
        and must be imported, not defined locally
    • In the expression: foo [|| T () ||]
      In the Template Haskell splice $$(foo [|| T () ||])
      In the expression: $$(foo [|| T () ||])
   |
12 |   let x = $$(foo [|| T () ||])
   |
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

getWorkListImplics :: TcS (Bag Implication)
getWorkListImplics
  = do { wl_var <- getTcSWorkListRef
       ; wl_curr <- readTcRef wl_var
       ; return (wl_implics wl_curr) }

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

emitImplication :: Implication -> TcS ()
emitImplication implic
  = updWorkListTcS (extendWorkListImplic implic)

selectNextWorkItem :: TcS (Maybe Ct)
-- Pick which work item to do next
-- See Note [Prioritise equalities]
--
-- Postcondition: if the returned item is a Wanted equality,
--                then its rewriter set is fully zonked.
--
-- Suppose a constraint c1 is rewritten by another, c2.  When c2
-- gets solved, c1 has no rewriters, and can be prioritised; see
-- Note [Prioritise Wanteds with empty RewriterSet] in
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
                 -- NB: no need for checkReductionDepth (ctLoc ct) (ctPred ct)
                 -- This is done by GHC.Tc.Solver.Dict.chooseInstance

             -- try_rws looks through rw_eqs to find one that has an empty
             -- rewriter set, after zonking.  If none such, call try_rest.
             try_rws acc (ct:cts)
                = do { ct' <- liftZonkTcS (TcM.zonkCtRewriterSet ct)
                     ; if ctHasNoRewriters ct'
                       then pick_me ct' (wl { wl_rw_eqs = cts ++ acc })
                       else try_rws (ct':acc) cts }
             try_rws acc [] = try_rest acc

             try_rest zonked_rws
               | ct:cts <- rest       = pick_me ct (wl { wl_rw_eqs = zonked_rws, wl_rest = cts })
               | ct:cts <- zonked_rws = pick_me ct (wl { wl_rw_eqs = cts })
               | otherwise            = return Nothing
     } }


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

The iteration is done in the simplify_loop/maybe_simplify_again loop in GHC.Tc.Solver.

It helpful not to iterate unless there is a chance of progress.  #8474 is
an example:

  * There's a deeply-nested chain of implication constraints.
       ?x:alpha => ?y1:beta1 => ... ?yn:betan => [W] ?x:Int

  * From the innermost one we get a [W] alpha[1] ~ Int,
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

instFlexiX :: Subst -> [TKVar] -> TcS Subst
instFlexiX subst tvs = wrapTcS (instFlexiXTcM subst tvs)

instFlexiXTcM :: Subst -> [TKVar] -> TcM Subst
-- Makes fresh tyvar, extends the substitution, and the in-scope set
-- Takes account of the case [k::Type, a::k, ...],
-- where we must substitute for k in a's kind
instFlexiXTcM subst []
  = return subst
instFlexiXTcM subst (tv:tvs)
  = do { uniq <- TcM.newUnique
       ; details <- TcM.newMetaDetails TauTv
       ; let name   = setNameUnique (tyVarName tv) uniq
             kind   = substTyUnchecked subst (tyVarKind tv)
             tv'    = mkTcTyVar name kind details
             subst' = extendTvSubstWithClone subst tv tv'
       ; instFlexiXTcM subst' tvs  }

matchGlobalInst :: DynFlags
                -> Bool      -- True <=> caller is the short-cut solver
                             -- See Note [Shortcut solving: overlap]
                -> Class -> [Type] -> CtLoc -> TcS TcM.ClsInstResult
matchGlobalInst dflags short_cut cls tys loc
  = wrapTcS $ TcM.matchGlobalInst dflags short_cut cls tys (Just loc)

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
setWantedEq (EvVarDest ev) _ = pprPanic "setWantedEq: EvVarDest" (ppr ev)

-- | Good for both equalities and non-equalities
setWantedEvTerm :: TcEvDest -> CanonicalEvidence -> EvTerm -> TcS ()
setWantedEvTerm (HoleDest hole) _canonical tm
  | Just co <- evTermCoercion_maybe tm
  = do { useVars (coVarsOfCo co)
       ; fillCoercionHole hole co }
  | otherwise
  = -- See Note [Yukky eq_sel for a HoleDest]
    do { let co_var = coHoleCoVar hole
       ; setEvBind (mkWantedEvBind co_var EvCanonical tm)
       ; fillCoercionHole hole (mkCoVarCo co_var) }

setWantedEvTerm (EvVarDest ev_id) canonical tm
  = setEvBind (mkWantedEvBind ev_id canonical tm)

{- Note [Yukky eq_sel for a HoleDest]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
How can it be that a Wanted with HoleDest gets evidence that isn't
just a coercion? i.e. evTermCoercion_maybe returns Nothing.

Consider [G] forall a. blah => a ~ T
         [W] S ~# T

Then doTopReactEqPred carefully looks up the (boxed) constraint (S ~ T)
in the quantified constraints, and wraps the (boxed) evidence it
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
       ; kickOutAfterFillingCoercionHole hole }

setEvBindIfWanted :: CtEvidence -> CanonicalEvidence -> EvTerm -> TcS ()
setEvBindIfWanted ev canonical tm
  = case ev of
      CtWanted (WantedCt { ctev_dest = dest }) -> setWantedEvTerm dest canonical tm
      _                                        -> return ()

newTcEvBinds :: TcS EvBindsVar
newTcEvBinds = wrapTcS TcM.newTcEvBinds

newNoTcEvBinds :: TcS EvBindsVar
newNoTcEvBinds = wrapTcS TcM.newNoTcEvBinds

newEvVar :: TcPredType -> TcS EvVar
newEvVar pred = wrapTcS (TcM.newEvVar pred)

newGivenEvVar :: CtLoc -> (TcPredType, EvTerm) -> TcS GivenCtEvidence
-- Make a new variable of the given PredType,
-- immediately bind it to the given term
-- and return its CtEvidence
-- See Note [Bind new Givens immediately] in GHC.Tc.Types.Constraint
newGivenEvVar loc (pred, rhs)
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
       ; gs <- mapM (newGivenEvVar loc) $
                [ (mkEqPredRole role ty1 ty2, evCoercion co)
                | (role, co) <- pts
                , let Pair ty1 ty2 = coercionKind co
                , not (ty1 `tcEqType` ty2) ] -- Kill reflexive Givens at birth
       ; emitWorkNC (map CtGiven gs) }

emitNewWantedEq :: CtLoc -> RewriterSet -> Role -> TcType -> TcType -> TcS Coercion
-- | Emit a new Wanted equality into the work-list
emitNewWantedEq loc rewriters role ty1 ty2
  = do { (wtd, co) <- newWantedEq loc rewriters role ty1 ty2
       ; updWorkListTcS (extendWorkListEq rewriters (mkNonCanonical $ CtWanted wtd))
       ; return co }

-- | Create a new Wanted constraint holding a coercion hole
-- for an equality between the two types at the given 'Role'.
newWantedEq :: CtLoc -> RewriterSet -> Role -> TcType -> TcType
            -> TcS (WantedCtEvidence, Coercion)
newWantedEq loc rewriters role ty1 ty2
  = do { hole <- wrapTcS $ TcM.newCoercionHole pty
       ; let wtd = WantedCt { ctev_pred      = pty
                            , ctev_dest      = HoleDest hole
                            , ctev_loc       = loc
                            , ctev_rewriters = rewriters }
       ; return (wtd, mkHoleCo hole) }
  where
    pty = mkEqPredRole role ty1 ty2

-- | Create a new Wanted constraint holding an evidence variable.
--
-- Don't use this for equality constraints: use 'newWantedEq' instead.
newWantedEvVarNC :: CtLoc -> RewriterSet
                 -> TcPredType -> TcS WantedCtEvidence
-- Don't look up in the solved/inerts; we know it's not there
newWantedEvVarNC loc rewriters pty
  = assertPpr (not (isEqPred pty)) (ppr pty) $
    do { new_ev <- newEvVar pty
       ; traceTcS "Emitting new wanted" (ppr new_ev <+> dcolon <+> ppr pty $$
                                         pprCtLoc loc)
       ; return $
           WantedCt { ctev_pred      = pty
                    , ctev_dest      = EvVarDest new_ev
                    , ctev_loc       = loc
                    , ctev_rewriters = rewriters }
       }

-- | Like 'newWantedEvVarNC', except it might look up in the inert set
-- to see if an inert already exists, and uses that instead of creating
-- a new Wanted constraint.
--
-- Don't use this for equality constraints: this function is only for
-- constraints with 'EvVarDest'.
newWantedEvVar :: CtLoc -> RewriterSet
               -> TcPredType -> TcS MaybeNew
-- For anything except ClassPred, this is the same as newWantedEvVarNC
newWantedEvVar loc rewriters pty
  = assertPpr (not (isEqPred pty))
      (vcat [ text "newWantedEvVar: HoleDestPred"
            , text "pty:" <+> ppr pty ]) $
    do { mb_ct <- lookupInInerts loc pty
       ; case mb_ct of
            Just ctev
              -> do { traceTcS "newWantedEvVar/cache hit" $ ppr ctev
                    ; return $ Cached (ctEvExpr ctev) }
            _ -> do { ctev <- newWantedEvVarNC loc rewriters pty
                    ; return (Fresh ctev) } }

-- | Create a new Wanted constraint, potentially looking up
-- non-equality constraints in the cache instead of creating
-- a new one from scratch.
--
-- Deals with both equality and non-equality constraints.
newWanted :: CtLoc -> RewriterSet -> PredType -> TcS MaybeNew
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
newWantedNC :: CtLoc -> RewriterSet -> PredType -> TcS WantedCtEvidence
newWantedNC loc rewriters pty
  | Just (role, ty1, ty2) <- getEqPredTys_maybe pty
  = fst <$> newWantedEq loc rewriters role ty1 ty2
  | otherwise
  = newWantedEvVarNC loc rewriters pty

-- | Checks if the depth of the given location is too much. Fails if
-- it's too big, with an appropriate error message.
checkReductionDepth :: CtLoc -> TcType   -- ^ type being reduced
                    -> TcS ()
checkReductionDepth loc ty
  = do { dflags <- getDynFlags
       ; when (subGoalDepthExceeded (reductionDepth dflags) (ctLocDepth loc)) $
         wrapErrTcS $ solverDepthError loc ty }

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
              Emitting equalities arising from fundeps
*                                                                      *
************************************************************************
-}

emitFunDepWanteds :: CtEvidence  -- The work item
                  -> [FunDepEqn (CtLoc, RewriterSet)]
                  -> TcS Bool  -- True <=> some unification happened

emitFunDepWanteds _ [] = return False -- common case noop
-- See Note [FunDep and implicit parameter reactions]

emitFunDepWanteds ev fd_eqns
  = unifyFunDeps ev Nominal do_fundeps
  where
    do_fundeps :: UnifyEnv -> TcM ()
    do_fundeps env = mapM_ (do_one env) fd_eqns

    do_one :: UnifyEnv -> FunDepEqn (CtLoc, RewriterSet) -> TcM ()
    do_one uenv (FDEqn { fd_qtvs = tvs, fd_eqs = eqs, fd_loc = (loc, rewriters) })
      = do { eqs' <- instantiate_eqs tvs (reverse eqs)
                     -- (reverse eqs): See Note [Reverse order of fundep equations]
           ; uPairsTcM env_one eqs' }
      where
        env_one = uenv { u_rewriters = u_rewriters uenv S.<> rewriters
                       , u_loc       = loc }

    instantiate_eqs :: [TyVar] -> [TypeEqn] -> TcM [TypeEqn]
    instantiate_eqs tvs eqs
      | null tvs
      = return eqs
      | otherwise
      = do { TcM.traceTc "emitFunDepWanteds 2" (ppr tvs $$ ppr eqs)
           ; subst <- instFlexiXTcM emptySubst tvs  -- Takes account of kind substitution
           ; return [ Pair (substTyUnchecked subst' ty1) ty2
                           -- ty2 does not mention fd_qtvs, so no need to subst it.
                           -- See GHC.Tc.Instance.Fundeps Note [Improving against instances]
                           --     Wrinkle (1)
                    | Pair ty1 ty2 <- eqs
                    , let subst' = extendSubstInScopeSet subst (tyCoVarsOfType ty1) ]
                          -- The free vars of ty1 aren't just fd_qtvs: ty1 is the result
                          -- of matching with the [W] constraint. So we add its free
                          -- vars to InScopeSet, to satisfy substTy's invariants, even
                          -- though ty1 will never (currently) be a poytype, so this
                          -- InScopeSet will never be looked at.
           }

{-
************************************************************************
*                                                                      *
              Unification
*                                                                      *
************************************************************************

Note [wrapUnifierTcS]
~~~~~~~~~~~~~~~~~~~
When decomposing equalities we often create new wanted constraints for
(s ~ t).  But what if s=t?  Then it'd be faster to return Refl right away.

Rather than making an equality test (which traverses the structure of the type,
perhaps fruitlessly), we call uType (via wrapUnifierTcS) to traverse the common
structure, and bales out when it finds a difference by creating a new deferred
Wanted constraint.  But where it succeeds in finding common structure, it just
builds a coercion to reflect it.

This is all much faster than creating a new constraint, putting it in the
work list, picking it out, canonicalising it, etc etc.

Note [unifyFunDeps]
~~~~~~~~~~~~~~~~~~~
The Bool returned by `unifyFunDeps` is True if we have unified a variable
that occurs in the constraint we are trying to solve; it is not in the
inert set so `wrapUnifierTcS` won't kick it out.  Instead we want to send it
back to the start of the pipeline.  Hence the Bool.

It's vital that we don't return (not (null unified)) because the fundeps
may create fresh variables; unifying them (alone) should not make us send
the constraint back to the start, or we'll get an infinite loop.  See
Note [Fundeps with instances, and equality orientation] in GHC.Tc.Solver.Dict
and Note [Improvement orientation] in GHC.Tc.Solver.Equality.
-}

uPairsTcM :: UnifyEnv -> [TypeEqn] -> TcM ()
uPairsTcM uenv eqns = mapM_ (\(Pair ty1 ty2) -> uType uenv ty1 ty2) eqns

unifyFunDeps :: CtEvidence -> Role
             -> (UnifyEnv -> TcM ())
             -> TcS Bool
unifyFunDeps ev role do_unifications
  = do { (_, _, unified) <- wrapUnifierTcS ev role do_unifications
       ; return (any (`elemVarSet` fvs) unified) }
         -- See Note [unifyFunDeps]
  where
    fvs = tyCoVarsOfType (ctEvPred ev)

unifyForAllBody :: CtEvidence -> Role -> (UnifyEnv -> TcM a)
                -> TcS (a, Cts)
-- We /return/ the equality constraints we generate,
-- rather than emitting them into the monad.
-- See See (SF5) in Note [Solving forall equalities] in GHC.Tc.Solver.Equality
unifyForAllBody ev role unify_body
  = do { (res, cts, unified) <- wrapUnifierX ev role unify_body
         -- Ignore the rewriters. They are used in wrapUnifierTcS only
         -- as an optimistion to prioritise the work list; but they are
         -- /also/ stored in each individual constraint we return.

       -- Kick out any inert constraint that we have unified
       ; _ <- kickOutAfterUnification unified

       ; return (res, cts) }

wrapUnifierTcS :: CtEvidence -> Role
               -> (UnifyEnv -> TcM a)  -- Some calls to uType
               -> TcS (a, Bag Ct, [TcTyVar])
-- Invokes the do_unifications argument, with a suitable UnifyEnv.
-- Emit deferred equalities and kick-out from the inert set as a
-- result of any unifications.
-- Very good short-cut when the two types are equal, or nearly so
-- See Note [wrapUnifierTcS]
--
-- The [TcTyVar] is the list of unification variables that were
-- unified the process; the (Bag Ct) are the deferred constraints.

wrapUnifierTcS ev role do_unifications
  = do { (res, cts, unified) <- wrapUnifierX ev role do_unifications

       -- Emit the deferred constraints
       -- See Note [Work-list ordering] in GHC.Tc.Solved.Equality
       --
       -- All the constraints in `cts` share the same rewriter set so,
       -- rather than looking at it one by one, we pass it to
       -- extendWorkListChildEqs; just a small optimisation.
       ; unless (isEmptyBag cts) $
         updWorkListTcS (extendWorkListChildEqs ev cts)

       -- And kick out any inert constraint that we have unified
       ; _ <- kickOutAfterUnification unified

       ; return (res, cts, unified) }

wrapUnifierX :: CtEvidence -> Role
             -> (UnifyEnv -> TcM a)  -- Some calls to uType
             -> TcS (a, Bag Ct, [TcTyVar])
wrapUnifierX ev role do_unifications
  = do { unif_count_ref <- getUnifiedRef
       ; wrapTcS $
         do { defer_ref   <- TcM.newTcRef emptyBag
            ; unified_ref <- TcM.newTcRef []
            ; let env = UE { u_role      = role
                           , u_rewriters = ctEvRewriters ev
                           , u_loc       = ctEvLoc ev
                           , u_defer     = defer_ref
                           , u_unified   = Just unified_ref}
              -- u_rewriters: the rewriter set and location from
              -- the parent constraint `ev` are inherited in any
              -- new constraints spat out by the unifier

            ; res <- do_unifications env

            ; cts     <- TcM.readTcRef defer_ref
            ; unified <- TcM.readTcRef unified_ref

            -- Don't forget to update the count of variables
            -- unified, lest we forget to iterate (#24146)
            ; unless (null unified) $
              TcM.updTcRef unif_count_ref (+ (length unified))

            ; return (res, cts, unified) } }


{-
************************************************************************
*                                                                      *
              Breaking type variable cycles
*                                                                      *
************************************************************************
-}

checkTypeEq :: CtEvidence -> EqRel -> CanEqLHS -> TcType
            -> TcS (PuResult () Reduction)
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
    CtWanted {} ->
      do { check_result <- wrapTcS (checkTyEqRhs wanted_flags rhs)
         ; case check_result of
              PuFail reason -> return (PuFail reason)
              PuOK cts redn -> do { emitWork cts
                                  ; return (pure redn) } }
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
      = mkNonCanonical . CtGiven <$> newGivenEvVar cb_loc (given_pred, given_term)
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

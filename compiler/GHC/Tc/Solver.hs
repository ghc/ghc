{-# LANGUAGE MultiWayIf, RecursiveDo, TupleSections #-}

module GHC.Tc.Solver(
       InferMode(..), simplifyInfer, findInferredDiff,
       growThetaTyVars,
       simplifyAmbiguityCheck,
       simplifyDefault,
       simplifyTop, simplifyTopImplic,
       simplifyInteractive,
       solveEqualities,
       pushLevelAndSolveEqualities, pushLevelAndSolveEqualitiesX,
       reportUnsolvedEqualities,
       simplifyWantedsTcM,
       tcCheckGivens,
       tcCheckWanteds,
       tcNormalise,

       captureTopConstraints,

       simplifyTopWanteds,

       promoteTyVarSet, simplifyAndEmitFlatConstraints,

       -- For Rules we need these
       solveWanteds,
       approximateWC

  ) where

import GHC.Prelude

import GHC.Data.Bag
import GHC.Core.Class
import GHC.Core
import GHC.Core.DataCon
import GHC.Core.Make
import GHC.Core.Coercion( mkNomReflCo )
import GHC.Driver.DynFlags
import GHC.Data.FastString
import GHC.Data.List.SetOps
import GHC.Types.Name
import GHC.Types.DefaultEnv ( ClassDefaults (..), defaultList )
import GHC.Types.Unique.Set
import GHC.Types.Id
import GHC.Utils.Outputable
import GHC.Builtin.Utils
import GHC.Builtin.Names
import GHC.Tc.Errors
import GHC.Tc.Errors.Types
import GHC.Tc.Types.Evidence
import GHC.Tc.Solver.Solve   ( solveSimpleGivens, solveSimpleWanteds )
import GHC.Tc.Solver.Dict    ( makeSuperClasses, solveCallStack )
import GHC.Tc.Solver.Rewrite ( rewriteType )
import GHC.Tc.Utils.Unify    ( buildTvImplication, touchabilityAndShapeTest
                             , simpleUnifyCheck, UnifyCheckCaller(..) )
import GHC.Tc.Utils.TcMType as TcM
import GHC.Tc.Utils.Monad   as TcM
import GHC.Tc.Zonk.TcType     as TcM
import GHC.Tc.Solver.InertSet
import GHC.Tc.Solver.Monad  as TcS
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.CtLoc( mkGivenLoc )
import GHC.Tc.Instance.FunDeps
import GHC.Core.Predicate
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.TcType
import GHC.Core.Type
import GHC.Core.Ppr
import GHC.Core.TyCon    ( TyCon, TyConBinder, isTypeFamilyTyCon )
import GHC.Builtin.Types
import GHC.Core.Unify    ( tcMatchTyKis )
import GHC.Unit.Module ( getModule )
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Types.TyThing ( MonadThings(lookupId) )
import GHC.Types.Var
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Types.Basic
import GHC.Types.Id.Make  ( unboxedUnitExpr )
import GHC.Types.Error
import qualified GHC.LanguageExtensions as LangExt

import Control.Monad
import Control.Monad.Trans.Class        ( lift )
import Control.Monad.Trans.State.Strict ( StateT(runStateT), put )
import Data.Foldable      ( toList, traverse_ )
import Data.List          ( partition, intersect )
import Data.List.NonEmpty ( NonEmpty(..), nonEmpty )
import qualified Data.List.NonEmpty as NE
import GHC.Data.Maybe     ( isJust, mapMaybe, catMaybes )
import Data.Monoid     ( First(..) )

{-
*********************************************************************************
*                                                                               *
*                           External interface                                  *
*                                                                               *
*********************************************************************************
-}

captureTopConstraints :: TcM a -> TcM (a, WantedConstraints)
-- (captureTopConstraints m) runs m, and returns the type constraints it
-- generates plus the constraints produced by static forms inside.
-- If it fails with an exception, it reports any insolubles
-- (out of scope variables) before doing so
--
-- captureTopConstraints is used exclusively by GHC.Tc.Module at the top
-- level of a module.
--
-- Importantly, if captureTopConstraints propagates an exception, it
-- reports any insoluble constraints first, lest they be lost
-- altogether.  This is important, because solveEqualities (maybe
-- other things too) throws an exception without adding any error
-- messages; it just puts the unsolved constraints back into the
-- monad. See GHC.Tc.Utils.Monad Note [Constraints and errors]
-- #16376 is an example of what goes wrong if you don't do this.
--
-- NB: the caller should bring any environments into scope before
-- calling this, so that the reportUnsolved has access to the most
-- complete GlobalRdrEnv
captureTopConstraints thing_inside
  = do { static_wc_var <- TcM.newTcRef emptyWC ;
       ; (mb_res, lie) <- TcM.updGblEnv (\env -> env { tcg_static_wc = static_wc_var } ) $
                          TcM.tryCaptureConstraints thing_inside
       ; stWC <- TcM.readTcRef static_wc_var

       -- See GHC.Tc.Utils.Monad Note [Constraints and errors]
       -- If the thing_inside threw an exception, but generated some insoluble
       -- constraints, report the latter before propagating the exception
       -- Otherwise they will be lost altogether
       ; case mb_res of
           Just res -> return (res, lie `andWC` stWC)
           Nothing  -> do { _ <- simplifyTop lie; failM } }
                -- This call to simplifyTop is the reason
                -- this function is here instead of GHC.Tc.Utils.Monad
                -- We call simplifyTop so that it does defaulting
                -- (esp of runtime-reps) before reporting errors

simplifyTopImplic :: Bag Implication -> TcM ()
simplifyTopImplic implics
  = do { empty_binds <- simplifyTop (mkImplicWC implics)

       -- Since all the inputs are implications the returned bindings will be empty
       ; massertPpr (isEmptyBag empty_binds) (ppr empty_binds)

       ; return () }

simplifyTop :: WantedConstraints -> TcM (Bag EvBind)
-- Simplify top-level constraints
-- Usually these will be implications,
-- but when there is nothing to quantify we don't wrap
-- in a degenerate implication, so we do that here instead
simplifyTop wanteds
  = do { traceTc "simplifyTop {" $ text "wanted = " <+> ppr wanteds
       ; ((final_wc, unsafe_ol), binds1) <- runTcS $
            do { final_wc <- simplifyTopWanteds wanteds
               ; unsafe_ol <- getSafeOverlapFailures
               ; return (final_wc, unsafe_ol) }
       ; traceTc "End simplifyTop }" empty

       ; binds2 <- reportUnsolved final_wc

       ; traceTc "reportUnsolved (unsafe overlapping) {" empty
       ; unless (isEmptyBag unsafe_ol) $ do {
           -- grab current error messages and clear, warnAllUnsolved will
           -- update error messages which we'll grab and then restore saved
           -- messages.
           ; errs_var  <- getErrsVar
           ; saved_msg <- TcM.readTcRef errs_var
           ; TcM.writeTcRef errs_var emptyMessages

           ; warnAllUnsolved $ emptyWC { wc_simple = fmap CDictCan unsafe_ol }

           ; whyUnsafe <- getWarningMessages <$> TcM.readTcRef errs_var
           ; TcM.writeTcRef errs_var saved_msg
           ; recordUnsafeInfer (mkMessages whyUnsafe)
           }
       ; traceTc "reportUnsolved (unsafe overlapping) }" empty

       ; return (evBindMapBinds binds1 `unionBags` binds2) }

pushLevelAndSolveEqualities :: SkolemInfoAnon -> [TyConBinder] -> TcM a -> TcM a
-- Push level, and solve all resulting equalities
-- If there are any unsolved equalities, report them
-- and fail (in the monad)
--
-- Panics if we solve any non-equality constraints.  (In runTCSEqualities
-- we use an error thunk for the evidence bindings.)
pushLevelAndSolveEqualities skol_info_anon tcbs thing_inside
  = do { (tclvl, wanted, res) <- pushLevelAndSolveEqualitiesX
                                      "pushLevelAndSolveEqualities" thing_inside
       ; report_unsolved_equalities skol_info_anon (binderVars tcbs) tclvl wanted
       ; return res }

pushLevelAndSolveEqualitiesX :: String -> TcM a
                             -> TcM (TcLevel, WantedConstraints, a)
-- Push the level, gather equality constraints, and then solve them.
-- Returns any remaining unsolved equalities.
-- Does not report errors.
--
-- Panics if we solve any non-equality constraints.  (In runTCSEqualities
-- we use an error thunk for the evidence bindings.)
pushLevelAndSolveEqualitiesX callsite thing_inside
  = do { traceTc "pushLevelAndSolveEqualitiesX {" (text "Called from" <+> text callsite)
       ; (tclvl, (wanted, res))
            <- pushTcLevelM $
               do { (res, wanted) <- captureConstraints thing_inside
                  ; wanted <- runTcSEqualities (simplifyTopWanteds wanted)
                  ; return (wanted,res) }
       ; traceTc "pushLevelAndSolveEqualities }" (vcat [ text "Residual:" <+> ppr wanted
                                                       , text "Level:" <+> ppr tclvl ])
       ; return (tclvl, wanted, res) }

-- | Type-check a thing that emits only equality constraints, solving any
-- constraints we can and re-emitting constraints that we can't.
-- Use this variant only when we'll get another crack at it later
-- See Note [Failure in local type signatures]
--
-- Panics if we solve any non-equality constraints.  (In runTCSEqualities
-- we use an error thunk for the evidence bindings.)
solveEqualities :: String -> TcM a -> TcM a
solveEqualities callsite thing_inside
  = do { traceTc "solveEqualities {" (text "Called from" <+> text callsite)
       ; (res, wanted)   <- captureConstraints thing_inside
       ; simplifyAndEmitFlatConstraints wanted
            -- simplifyAndEmitFlatConstraints fails outright unless
            --  the only unsolved constraints are soluble-looking
            --  equalities that can float out
       ; traceTc "solveEqualities }" empty
       ; return res }

simplifyAndEmitFlatConstraints :: WantedConstraints -> TcM ()
-- See Note [Failure in local type signatures]
simplifyAndEmitFlatConstraints wanted
  = do { -- Solve and zonk to establish the
         -- preconditions for floatKindEqualities
         wanted <- runTcSEqualities (solveWanteds wanted)
       ; wanted <- TcM.liftZonkM $ TcM.zonkWC wanted

       ; traceTc "emitFlatConstraints {" (ppr wanted)
       ; case floatKindEqualities wanted of
           Nothing -> do { traceTc "emitFlatConstraints } failing" (ppr wanted)
                         -- Emit the bad constraints, wrapped in an implication
                         -- See Note [Wrapping failing kind equalities]
                         ; tclvl  <- TcM.getTcLevel
                         ; implic <- buildTvImplication unkSkolAnon [] (pushTcLevel tclvl) wanted
                                        --                  ^^^^^^   |  ^^^^^^^^^^^^^^^^^
                                        -- it's OK to use unkSkol    |  we must increase the TcLevel,
                                        -- because we don't bind     |  as explained in
                                        -- any skolem variables here |  Note [Wrapping failing kind equalities]
                         ; emitImplication implic
                         ; failM }
           Just (simples, errs)
              -> do { _ <- promoteTyVarSet (tyCoVarsOfCts simples)
                    ; traceTc "emitFlatConstraints }" $
                      vcat [ text "simples:" <+> ppr simples
                           , text "errs:   " <+> ppr errs ]
                      -- Holes and other delayed errors don't need promotion
                    ; emitDelayedErrors errs
                    ; emitSimples simples } }

floatKindEqualities :: WantedConstraints -> Maybe (Bag Ct, Bag DelayedError)
-- Float out all the constraints from the WantedConstraints,
-- Return Nothing if any constraints can't be floated (captured
-- by skolems), or if there is an insoluble constraint, or
-- IC_Telescope telescope error
-- Precondition 1: we have tried to solve the 'wanteds', both so that
--    the ic_status field is set, and because solving can make constraints
--    more floatable.
-- Precondition 2: the 'wanteds' are zonked, since floatKindEqualities
--    is not monadic
-- See Note [floatKindEqualities vs approximateWC]
floatKindEqualities wc = float_wc emptyVarSet wc
  where
    float_wc :: TcTyCoVarSet -> WantedConstraints -> Maybe (Bag Ct, Bag DelayedError)
    float_wc trapping_tvs (WC { wc_simple = simples
                              , wc_impl = implics
                              , wc_errors = errs })
      | all is_floatable simples
      = do { (inner_simples, inner_errs)
                <- flatMapBagPairM (float_implic trapping_tvs) implics
           ; return ( simples `unionBags` inner_simples
                    , errs `unionBags` inner_errs) }
      | otherwise
      = Nothing
      where
        is_floatable ct
           | insolubleCt ct = False
           | otherwise      = tyCoVarsOfCt ct `disjointVarSet` trapping_tvs

    float_implic :: TcTyCoVarSet -> Implication -> Maybe (Bag Ct, Bag DelayedError)
    float_implic trapping_tvs (Implic { ic_wanted = wanted, ic_given_eqs = given_eqs
                                      , ic_skols = skols, ic_status = status })
      | isInsolubleStatus status
      = Nothing   -- A short cut /plus/ we must keep track of IC_BadTelescope
      | otherwise
      = do { (simples, holes) <- float_wc new_trapping_tvs wanted
           ; when (not (isEmptyBag simples) && given_eqs == MaybeGivenEqs) $
             Nothing
                 -- If there are some constraints to float out, but we can't
                 -- because we don't float out past local equalities
                 -- (c.f GHC.Tc.Solver.approximateWC), then fail
           ; return (simples, holes) }
      where
        new_trapping_tvs = trapping_tvs `extendVarSetList` skols


{- Note [Failure in local type signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When kind checking a type signature, we like to fail fast if we can't
solve all the kind equality constraints, for two reasons:

  * A kind-bogus type signature may cause a cascade of knock-on
    errors if we let it pass

  * More seriously, we don't have a convenient term-level place to add
    deferred bindings for unsolved kind-equality constraints.  In
    earlier GHCs this led to un-filled-in coercion holes, which caused
    GHC to crash with "fvProv falls into a hole" See #11563, #11520,
    #11516, #11399

But what about /local/ type signatures, mentioning in-scope type
variables for which there might be 'given' equalities?  For these we
might not be able to solve all the equalities locally. Here's an
example (T15076b):

  class (a ~ b) => C a b
  data SameKind :: k -> k -> Type where { SK :: SameKind a b }

  bar :: forall (a :: Type) (b :: Type).
         C a b => Proxy a -> Proxy b -> ()
  bar _ _ = const () (undefined :: forall (x :: a) (y :: b). SameKind x y)

Consider the type signature on 'undefined'. It's ill-kinded unless
a~b.  But the superclass of (C a b) means that indeed (a~b). So all
should be well. BUT it's hard to see that when kind-checking the signature
for undefined.  We want to emit a residual (a~b) constraint, to solve
later.

Another possibility is that we might have something like
   F alpha ~ [Int]
where alpha is bound further out, which might become soluble
"later" when we learn more about alpha.  So we want to emit
those residual constraints.

BUT it's no good simply wrapping all unsolved constraints from
a type signature in an implication constraint to solve later. The
problem is that we are going to /use/ that signature, including
instantiate it.  Say we have
     f :: forall a.  (forall b. blah) -> blah2
     f x = <body>
To typecheck the definition of f, we have to instantiate those
foralls.  Moreover, any unsolved kind equalities will be coercion
holes in the type.  If we naively wrap them in an implication like
     forall a. (co1:k1~k2,  forall b.  co2:k3~k4)
hoping to solve it later, we might end up filling in the holes
co1 and co2 with coercions involving 'a' and 'b' -- but by now
we've instantiated the type.  Chaos!

Moreover, the unsolved constraints might be skolem-escape things, and
if we proceed with f bound to a nonsensical type, we get a cascade of
follow-up errors. For example polykinds/T12593, T15577, and many others.

So here's the plan (see tcHsSigType):

* pushLevelAndSolveEqualitiesX: try to solve the constraints

* kindGeneraliseSome: do kind generalisation

* buildTvImplication: build an implication for the residual, unsolved
  constraint

* simplifyAndEmitFlatConstraints: try to float out every unsolved equality
  inside that implication, in the hope that it constrains only global
  type variables, not the locally-quantified ones.

  * If we fail, or find an insoluble constraint, emit the implication,
    so that the errors will be reported, and fail.

  * If we succeed in floating all the equalities, promote them and
    re-emit them as flat constraint, not wrapped at all (since they
    don't mention any of the quantified variables.

* Note that this float-and-promote step means that anonymous
  wildcards get floated to top level, as we want; see
  Note [Checking partial type signatures] in GHC.Tc.Gen.HsType.

All this is done:

* In GHC.Tc.Gen.HsType.tcHsSigType, as above

* solveEqualities. Use this when there no kind-generalisation
  step to complicate matters; then we don't need to push levels,
  and can solve the equalities immediately without needing to
  wrap it in an implication constraint.  (You'll generally see
  a kindGeneraliseNone nearby.)

* In GHC.Tc.TyCl and GHC.Tc.TyCl.Instance; see calls to
  pushLevelAndSolveEqualitiesX, followed by quantification, and
  then reportUnsolvedEqualities.

  NB: we call reportUnsolvedEqualities before zonkTcTypeToType
  because the latter does not expect to see any un-filled-in
  coercions, which will happen if we have unsolved equalities.
  By calling reportUnsolvedEqualities first, which fails after
  reporting errors, we avoid that happening.

See also #18062, #11506

Note [Wrapping failing kind equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In simplifyAndEmitFlatConstraints, if we fail to get down to simple
flat constraints we will
* re-emit the constraints so that they are reported
* fail in the monad
But there is a Terrible Danger that, if -fdefer-type-errors is on, and
we just re-emit an insoluble constraint like (* ~ (*->*)), that we'll
report only a warning and proceed with compilation.  But if we ever fail
in the monad it should be fatal; we should report an error and stop after
the type checker.  If not, chaos results: #19142.

Our solution is this:
* Even with -fdefer-type-errors, inside an implication with no place for
  value bindings (ic_binds = CoEvBindsVar), report failing equalities as
  errors.  We have to do this anyway; see GHC.Tc.Errors
  Note [Failing equalities with no evidence bindings].

* Right here in simplifyAndEmitFlatConstraints, use buildTvImplication
  to wrap the failing constraint in a degenerate implication (no
  skolems, no theta), with ic_binds = CoEvBindsVar.  This setting of
  `ic_binds` means that any failing equalities will lead to an
  error not a warning, irrespective of -fdefer-type-errors: see
  Note [Failing equalities with no evidence bindings] in GHC.Tc.Errors,
  and `maybeSwitchOffDefer` in that module.

  We still take care to bump the TcLevel of the implication.  Partly,
  that ensures that nested implications have increasing level numbers
  which seems nice.  But more specifically, suppose the outer level
  has a Given `(C ty)`, which has pending (not-yet-expanded)
  superclasses. Consider what happens when we process this implication
  constraint (which we have re-emitted) in that context:
    - in the inner implication we'll call `getPendingGivenScs`,
    - we /do not/ want to get the `(C ty)` from the outer level,
    lest we try to add an evidence term for the superclass,
    which we can't do because we have specifically set
    `ic_binds` = `CoEvBindsVar`.
    - as `getPendingGivenSCcs is careful to only get Givens from
    the /current/ level, and we bumped the `TcLevel` of the implication,
    we're OK.

  TL;DR: bump the `TcLevel` when creating the nested implication.
  If we don't we get a panic in `GHC.Tc.Utils.Monad.addTcEvBind` (#20043).


We re-emit the implication rather than reporting the errors right now,
so that the error messages are improved by other solving and defaulting.
e.g. we prefer
    Cannot match 'Type->Type' with 'Type'
to  Cannot match 'Type->Type' with 'TYPE r0'


Note [floatKindEqualities vs approximateWC]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
floatKindEqualities and approximateWC are strikingly similar to each
other, but

* floatKindEqualites tries to float /all/ equalities, and fails if
  it can't, or if any implication is insoluble.
* approximateWC just floats out any constraints
  (not just equalities) that can float; it never fails.
-}


reportUnsolvedEqualities :: SkolemInfo -> [TcTyVar] -> TcLevel
                         -> WantedConstraints -> TcM ()
-- Reports all unsolved wanteds provided; fails in the monad if there are any.
--
-- The provided SkolemInfo and [TcTyVar] arguments are used in an implication to
-- provide skolem info for any errors.
reportUnsolvedEqualities skol_info skol_tvs tclvl wanted
  = report_unsolved_equalities (getSkolemInfo skol_info) skol_tvs tclvl wanted

report_unsolved_equalities :: SkolemInfoAnon -> [TcTyVar] -> TcLevel
                           -> WantedConstraints -> TcM ()
report_unsolved_equalities skol_info_anon skol_tvs tclvl wanted
  | isEmptyWC wanted
  = return ()

  | otherwise  -- NB: we build an implication /even if skol_tvs is empty/,
               -- just to ensure that our level invariants hold, specifically
               -- (WantedInv).  See Note [TcLevel invariants].
  = checkNoErrs $   -- Fail
    do { implic <- buildTvImplication skol_info_anon skol_tvs tclvl wanted
       ; reportAllUnsolved (mkImplicWC (unitBag implic)) }


-- | Simplify top-level constraints, but without reporting any unsolved
-- constraints nor unsafe overlapping.
simplifyTopWanteds :: WantedConstraints -> TcS WantedConstraints
simplifyTopWanteds wanteds
  = do { -- Solve the constraints
         wc_first_go <- nestTcS (solveWanteds wanteds)

         -- Now try defaulting:
         -- see Note [Top-level Defaulting Plan]
       ; tryDefaulting wc_first_go }

--------------------------
tryDefaulting :: WantedConstraints -> TcS WantedConstraints
tryDefaulting wc
 = do { dflags <- getDynFlags
      ; traceTcS "tryDefaulting:before" (ppr wc)
      ; wc1 <- tryTyVarDefaulting dflags wc
      ; wc2 <- tryConstraintDefaulting wc1
      ; wc3 <- tryTypeClassDefaulting wc2
      ; wc4 <- tryUnsatisfiableGivens wc3
      ; traceTcS "tryDefaulting:after" (ppr wc)
      ; return wc4 }

solveAgainIf :: Bool -> WantedConstraints -> TcS WantedConstraints
-- If the Bool is true, solve the wanted constraints again
-- See Note [Must simplify after defaulting]
solveAgainIf False wc = return wc
solveAgainIf True  wc = nestTcS (solveWanteds wc)

--------------------------
tryTyVarDefaulting  :: DynFlags -> WantedConstraints -> TcS WantedConstraints
tryTyVarDefaulting dflags wc
  | isEmptyWC wc
  = return wc
  | insolubleWC wc
  , gopt Opt_PrintExplicitRuntimeReps dflags -- See Note [Defaulting insolubles]
  = return wc
  | otherwise
  = do { -- Need to zonk first, as the WantedConstraints are not yet zonked.
       ; free_tvs <- TcS.zonkTyCoVarsAndFVList (tyCoVarsOfWCList wc)
       ; let defaultable_tvs = filter can_default free_tvs
             can_default tv
               =   isTyVar tv
                   -- Weed out coercion variables.

                && isMetaTyVar tv
                   -- Weed out runtime-skolems in GHCi, which we definitely
                   -- shouldn't try to default.

                && not (tv `elemVarSet` nonDefaultableTyVarsOfWC wc)
                   -- Weed out variables for which defaulting would be unhelpful,
                   -- e.g. alpha appearing in [W] alpha[conc] ~# rr[sk].

       ; unification_s <- mapM defaultTyVarTcS defaultable_tvs -- Has unification side effects
       ; solveAgainIf (or unification_s) wc }
             -- solveAgainIf: see Note [Must simplify after defaulting]

----------------------------
-- | If an implication contains a Given of the form @Unsatisfiable msg@,
-- use it to solve all Wanteds within the implication.
-- See point (C) in Note [Implementation of Unsatisfiable constraints] in GHC.Tc.Errors.
--
-- This does a complete walk over the implication tree.
tryUnsatisfiableGivens :: WantedConstraints -> TcS WantedConstraints
tryUnsatisfiableGivens wc =
  do { (final_wc, did_work) <- (`runStateT` False) $ go_wc wc
     ; solveAgainIf did_work final_wc }
  where
    go_wc (WC { wc_simple = wtds, wc_impl = impls, wc_errors = errs })
      = do impls' <- mapMaybeBagM go_impl impls
           return $ WC { wc_simple = wtds, wc_impl = impls', wc_errors = errs }
    go_impl impl
      | isSolvedStatus (ic_status impl)
      = return $ Just impl
      -- Is there a Given with type "Unsatisfiable msg"?
      -- If so, use it to solve all other Wanteds.
      | unsat_given:_ <- mapMaybe unsatisfiableEv_maybe (ic_given impl)
      = do { put True
           ; lift $ solveImplicationUsingUnsatGiven unsat_given impl }
      -- Otherwise, recurse.
      | otherwise
      = do { wcs' <- go_wc (ic_wanted impl)
           ; lift $ setImplicationStatus $ impl { ic_wanted = wcs' } }

-- | Is this evidence variable the evidence for an 'Unsatisfiable' constraint?
--
-- If so, return the variable itself together with the error message type.
unsatisfiableEv_maybe :: EvVar -> Maybe (EvVar, Type)
unsatisfiableEv_maybe v = (v,) <$> isUnsatisfiableCt_maybe (idType v)

-- | We have an implication with an 'Unsatisfiable' Given; use that Given to
-- solve all the other Wanted constraints, including those nested within
-- deeper implications.
solveImplicationUsingUnsatGiven :: (EvVar, Type) -> Implication -> TcS (Maybe Implication)
solveImplicationUsingUnsatGiven
  unsat_given@(given_ev,_)
  impl@(Implic { ic_wanted = wtd, ic_tclvl = tclvl, ic_binds = ev_binds_var, ic_need_inner = inner })
  | isCoEvBindsVar ev_binds_var
  -- We can't use Unsatisfiable evidence in kinds.
  -- See Note [Coercion evidence only] in GHC.Tc.Types.Evidence.
  = return $ Just impl
  | otherwise
  = do { wcs <- nestImplicTcS ev_binds_var tclvl $ go_wc wtd
       ; setImplicationStatus $
         impl { ic_wanted = wcs
              , ic_need_inner = inner `extendVarSet` given_ev } }
  where
    go_wc :: WantedConstraints -> TcS WantedConstraints
    go_wc wc@(WC { wc_simple = wtds, wc_impl = impls })
      = do { mapBagM_ go_simple wtds
           ; impls <- mapMaybeBagM (solveImplicationUsingUnsatGiven unsat_given) impls
           ; return $ wc { wc_simple = emptyBag, wc_impl = impls } }
    go_simple :: Ct -> TcS ()
    go_simple ct = case ctEvidence ct of
      CtWanted { ctev_pred = pty, ctev_dest = dst }
        -> do { ev_expr <- unsatisfiableEvExpr unsat_given pty
              ; setWantedEvTerm dst EvNonCanonical $ EvExpr ev_expr }
      _ -> return ()

-- | Create an evidence expression for an arbitrary constraint using
-- evidence for an "Unsatisfiable" Given.
--
-- See Note [Evidence terms from Unsatisfiable Givens]
unsatisfiableEvExpr :: (EvVar, ErrorMsgType) -> PredType -> TcS EvExpr
unsatisfiableEvExpr (unsat_ev, given_msg) wtd_ty
  = do { mod <- getModule
         -- If we're typechecking GHC.TypeError, return a bogus expression;
         -- it's only used for the ambiguity check, which throws the evidence away anyway.
         -- This avoids problems with circularity; where we are trying to look
         -- up the "unsatisfiable" Id while we are in the middle of typechecking it.
       ; if mod == gHC_INTERNAL_TYPEERROR then return (Var unsat_ev) else
    do { unsatisfiable_id <- tcLookupId unsatisfiableIdName

         -- See Note [Evidence terms from Unsatisfiable Givens]
         -- for a description of what evidence term we are constructing here.

       ; let -- (##) -=> wtd_ty
             fun_ty = mkFunTy visArgConstraintLike ManyTy unboxedUnitTy wtd_ty
             mkDictBox = case boxingDataCon fun_ty of
               BI_Box { bi_data_con = mkDictBox } -> mkDictBox
               _ -> pprPanic "unsatisfiableEvExpr: no DictBox!" (ppr wtd_ty)
             dictBox = dataConTyCon mkDictBox
       ; ev_bndr <- mkSysLocalM (fsLit "ct") ManyTy fun_ty
             -- Dict ((##) -=> wtd_ty)
       ; let scrut_ty = mkTyConApp dictBox [fun_ty]
             -- unsatisfiable @{LiftedRep} @given_msg @(Dict ((##) -=> wtd_ty)) unsat_ev
             scrut =
               mkCoreApps (Var unsatisfiable_id)
                 [ Type liftedRepTy
                 , Type given_msg
                 , Type scrut_ty
                 , Var unsat_ev ]
             -- case scrut of { MkDictBox @((##) -=> wtd_ty)) ct -> ct (# #) }
             ev_expr =
               mkWildCase scrut (unrestricted $ scrut_ty) wtd_ty
               [ Alt (DataAlt mkDictBox) [ev_bndr] $
                   mkCoreApps (Var ev_bndr) [unboxedUnitExpr]
               ]
        ; return ev_expr } }

{- Note [Evidence terms from Unsatisfiable Givens]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An Unsatisfiable Given constraint, of the form [G] Unsatisfiable msg, should be
able to solve ANY Wanted constraint whatsoever.

Recall that we have

  unsatisfiable :: forall {rep} (msg :: ErrorMessage) (a :: TYPE rep)
                .  Unsatisfiable msg => a

We want to use this function, together with the evidence
[G] unsat_ev :: Unsatisfiable msg, to solve any other constraint [W] wtd_ty.

We could naively think that a valid evidence term for the Wanted might be:

  wanted_ev = unsatisfiable @{rep} @msg @wtd_ty unsat_ev

Unfortunately, this is a kind error: "wtd_ty :: CONSTRAINT rep", but
"unsatisfiable" expects the third type argument to be of kind "TYPE rep".

Instead, we use a boxing data constructor to box the constraint into a type.
In the end, we construct the following evidence for the implication:

  [G] unsat_ev :: Unsatisfiable msg
    ==>
      [W] wtd_ev :: wtd_ty

  wtd_ev =
    case unsatisfiable @{LiftedRep} @msg @(Dict ((##) -=> wtd_ty)) unsat_ev of
      MkDictBox ct -> ct (# #)

Note that we play the same trick with the function arrow -=> that we did
in order to define "unsatisfiable" in terms of "unsatisfiableLifted", as described
in Note [The Unsatisfiable representation-polymorphism trick] in base:GHC.TypeError.
This allows us to indirectly box constraints with different representations
(such as primitive equality constraints).
-}

-- | A 'TcS' action which can may solve a `Ct`
type CtDefaultingStrategy = Ct -> TcS Bool
  -- True <=> I solved the constraint

--------------------------------
tryConstraintDefaulting :: WantedConstraints -> TcS WantedConstraints
-- See Note [Overview of implicit CallStacks] in GHC.Tc.Types.Evidence
tryConstraintDefaulting wc
  | isEmptyWC wc
  = return wc
  | otherwise
  = do { (n_unifs, better_wc) <- reportUnifications (go_wc wc)
         -- We may have done unifications; so solve again
       ; solveAgainIf (n_unifs > 0) better_wc }
  where
    go_wc :: WantedConstraints -> TcS WantedConstraints
    go_wc wc@(WC { wc_simple = simples, wc_impl = implics })
      = do { mb_simples <- mapMaybeBagM go_simple simples
           ; mb_implics <- mapMaybeBagM go_implic implics
           ; return (wc { wc_simple = mb_simples, wc_impl   = mb_implics }) }

    go_simple :: Ct -> TcS (Maybe Ct)
    go_simple ct = do { solved <- tryCtDefaultingStrategy ct
                      ; if solved then return Nothing
                                  else return (Just ct) }

    go_implic :: Implication -> TcS (Maybe Implication)
    -- The Maybe is because solving the CallStack constraint
    -- may well allow us to discard the implication entirely
    go_implic implic
      | isSolvedStatus (ic_status implic)
      = return (Just implic)  -- Nothing to solve inside here
      | otherwise
      = do { wanteds <- setEvBindsTcS (ic_binds implic) $
                        -- defaultCallStack sets a binding, so
                        -- we must set the correct binding group
                        go_wc (ic_wanted implic)
           ; setImplicationStatus (implic { ic_wanted = wanteds }) }

tryCtDefaultingStrategy :: CtDefaultingStrategy
-- The composition of all the CtDefaultingStrategies we want
tryCtDefaultingStrategy
  = foldr1 combineStrategies
    [ defaultCallStack
    , defaultExceptionContext
    , defaultEquality ]

-- | Default @ExceptionContext@ constraints to @emptyExceptionContext@.
defaultExceptionContext :: CtDefaultingStrategy
defaultExceptionContext ct
  | ClassPred cls tys <- classifyPredType (ctPred ct)
  , isJust (isExceptionContextPred cls tys)
  = do { warnTcS $ TcRnDefaultedExceptionContext (ctLoc ct)
       ; empty_ec_id <- lookupId emptyExceptionContextName
       ; let ev = ctEvidence ct
             ev_tm = mkEvCast (Var empty_ec_id) (wrapIP (ctEvPred ev))
       ; setEvBindIfWanted ev EvCanonical ev_tm
         -- EvCanonical: see Note [CallStack and ExecptionContext hack]
         --              in GHC.Tc.Solver.Dict
       ; return True }
  | otherwise
  = return False

-- | Default any remaining @CallStack@ constraints to empty @CallStack@s.
-- See Note [Overview of implicit CallStacks] in GHC.Tc.Types.Evidence
defaultCallStack :: CtDefaultingStrategy
defaultCallStack ct
  | ClassPred cls tys <- classifyPredType (ctPred ct)
  , isJust (isCallStackPred cls tys)
  = do { solveCallStack (ctEvidence ct) EvCsEmpty
       ; return True }
  | otherwise
  = return False

defaultEquality :: CtDefaultingStrategy
-- See Note [Defaulting equalities]
defaultEquality ct
  | EqPred NomEq ty1 ty2 <- classifyPredType (ctPred ct)
  , Just tv1 <- getTyVar_maybe ty1
  = do { -- Remember: `ct` may not be zonked;
         -- see (DE3) in Note [Defaulting equalities]
         z_ty1 <- TcS.zonkTcTyVar tv1
       ; z_ty2 <- TcS.zonkTcType  ty2
       ; case getTyVar_maybe z_ty1 of
           Just z_tv1 | defaultable z_tv1 z_ty2
                      -> do { default_tv z_tv1 z_ty2
                            ; return True }
           _          -> return False }
   | otherwise
   = return False
  where
    defaultable tv1 ty2
      =  -- Do the standard unification checks;
         --   c.f. uUnfilledVar2 in GHC.Tc.Utils.Unify
         -- EXCEPT drop the untouchability test
         tyVarKind tv1 `tcEqType` typeKind ty2
      && touchabilityAndShapeTest topTcLevel tv1 ty2
          -- topTcLevel makes the untoucability test vacuous,
          -- which is the Whole Point of `defaultEquality`
          -- See (DE2) in Note [Defaulting equalities]
      && simpleUnifyCheck UC_Defaulting tv1 ty2

    default_tv tv1 ty2
      = do { unifyTyVar tv1 ty2   -- NB: unifyTyVar adds to the
                                  -- TcS unification counter
           ; setEvBindIfWanted (ctEvidence ct) EvCanonical $
             evCoercion (mkNomReflCo ty2) }

combineStrategies :: CtDefaultingStrategy -> CtDefaultingStrategy -> CtDefaultingStrategy
combineStrategies default1 default2 ct
  = do { solved <- default1 ct
       ; case solved of
           True  -> return True  -- default1 solved it!
           False -> default2 ct  -- default1 failed, try default2
       }


{- Note [When to do type-class defaulting]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In GHC 7.6 and 7.8.2, we did type-class defaulting only if insolubleWC
was false, on the grounds that defaulting can't help solve insoluble
constraints.  But if we *don't* do defaulting we may report a whole
lot of errors that would be solved by defaulting; these errors are
quite spurious because fixing the single insoluble error means that
defaulting happens again, which makes all the other errors go away.
This is jolly confusing: #9033.

So it seems better to always do type-class defaulting.

However, always doing defaulting does mean that we'll do it in
situations like this (#5934):
   run :: (forall s. GenST s) -> Int
   run = fromInteger 0
We don't unify the return type of fromInteger with the given function
type, because the latter involves foralls.  So we're left with
    (Num alpha, alpha ~ (forall s. GenST s) -> Int)
Now we do defaulting, get alpha := Integer, and report that we can't
match Integer with (forall s. GenST s) -> Int.  That's not totally
stupid, but perhaps a little strange.

Another potential alternative would be to suppress *all* non-insoluble
errors if there are *any* insoluble errors, anywhere, but that seems
too drastic.

Note [Defaulting equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  f :: forall a. (forall t. (F t ~ Int) => a -> Int) -> Int

  g :: Int
  g = f id

We'll typecheck
  id :: forall t. (F t ~ Int) => alpha[1] -> Int
where the `alpha[1]` comes from instantiating `f`. So we'll end up
with the implication constraint
   forall[2] t. (F t ~ Int) => alpha[1] ~ Int
And that can't be solved because `alpha` is untouchable under the
equality (F t ~ Int).

This is tiresome, and gave rise to user complaints: #25125 and #25029.
Moreover, in this case there is no good reason not to unify alpha:=Int.
Doing so solves the constraint, and since `alpha` is not otherwise
constrained, it does no harm.  So the new plan is this:

  * For the Wanted constraint
        [W] alpha ~ ty
    if the only reason for not unifying is untouchability, then during
    top-level defaulting, go ahead and unify

In top-level defaulting, we already do several other somewhat-ad-hoc,
but terribly convenient, unifications. This is just one more.

Wrinkles:

(DE1) Note carefully that this does not threaten principal types.
  The original worry about unifying untouchable type variables was this:

     data T a where
       T1 :: T Bool
     f x = case x of T1 -> True

  Should we infer f :: T a -> Bool, or f :: T a -> a.  Both are valid, but
  neither is more general than the other

(DE2) We still can't unify if there is a skolem-escape check, or an occurs check,
  or it it'd mean unifying a TyVarTv with a non-tyvar.  It's only the
  "untouchability test" that we lift.  We can lift it by saying that the innermost
  given equality is at top level.

(DE3) The contraint we are looking at may not be fully zonked; for example,
  an earlier deafaulting might have affected it. So we zonk-on-the fly in
  `defaultEquality`.

Note [Don't default in syntactic equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When there are unsolved syntactic equalities such as

  rr[sk] ~S# alpha[conc]

we should not default alpha, lest we obtain a poor error message such as

  Couldn't match kind `rr' with `LiftedRep'

We would rather preserve the original syntactic equality to be
reported to the user, especially as the concrete metavariable alpha
might store an informative origin for the user.

Note [Must simplify after defaulting]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We may have a deeply buried constraint
    (t:*) ~ (a:Open)
which we couldn't solve because of the kind incompatibility, and 'a' is free.
Then when we default 'a' we can solve the constraint.  And we want to do
that before starting in on type classes.  We MUST do it before reporting
errors, because it isn't an error!  #7967 was due to this.

Note [Top-level Defaulting Plan]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We have considered two design choices for where/when to apply defaulting.
   (i) Do it in SimplCheck mode only /whenever/ you try to solve some
       simple constraints, maybe deep inside the context of implications.
       This used to be the case in GHC 7.4.1.
   (ii) Do it in a tight loop at simplifyTop, once all other constraints have
        finished. This is the current story.

Option (i) had many disadvantages:
   a) Firstly, it was deep inside the actual solver.
   b) Secondly, it was dependent on the context (Infer a type signature,
      or Check a type signature, or Interactive) since we did not want
      to always start defaulting when inferring (though there is an exception to
      this, see Note [Default while Inferring]).
   c) It plainly did not work. Consider typecheck/should_compile/DfltProb2.hs:
          f :: Int -> Bool
          f x = const True (\y -> let w :: a -> a
                                      w a = const a (y+1)
                                  in w y)
      We will get an implication constraint (for beta the type of y):
               [untch=beta] forall a. 0 => Num beta
      which we really cannot default /while solving/ the implication, since beta is
      untouchable.

Instead our new defaulting story is to pull defaulting out of the solver loop and
go with option (ii), implemented at SimplifyTop. Namely:
     - First, have a go at solving the residual constraint of the whole
       program
     - Try to approximate it with a simple constraint
     - Figure out derived defaulting equations for that simple constraint
     - Go round the loop again if you did manage to get some equations

Now, that has to do with class defaulting. However there exists type variable /kind/
defaulting. Again this is done at the top-level and the plan is:
     - At the top-level, once you had a go at solving the constraint, do
       figure out /all/ the touchable unification variables of the wanted constraints.
     - Apply defaulting to their kinds

More details in Note [DefaultTyVar].

Note [Safe Haskell Overlapping Instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In Safe Haskell, we apply an extra restriction to overlapping instances. The
motive is to prevent untrusted code provided by a third-party, changing the
behavior of trusted code through type-classes. This is due to the global and
implicit nature of type-classes that can hide the source of the dictionary.

Another way to state this is: if a module M compiles without importing another
module N, changing M to import N shouldn't change the behavior of M.

Overlapping instances with type-classes can violate this principle. However,
overlapping instances aren't always unsafe. They are just unsafe when the most
selected dictionary comes from untrusted code (code compiled with -XSafe) and
overlaps instances provided by other modules.

In particular, in Safe Haskell at a call site with overlapping instances, we
apply the following rule to determine if it is a 'unsafe' overlap:

 1) Most specific instance, I1, defined in an `-XSafe` compiled module.
 2) I1 is an orphan instance or a MPTC.
 3) At least one overlapped instance, Ix, is both:
    A) from a different module than I1
    B) Ix is not marked `OVERLAPPABLE`

This is a slightly involved heuristic, but captures the situation of an
imported module N changing the behavior of existing code. For example, if
condition (2) isn't violated, then the module author M must depend either on a
type-class or type defined in N.

Secondly, when should these heuristics be enforced? We enforced them when the
type-class method call site is in a module marked `-XSafe` or `-XTrustworthy`.
This allows `-XUnsafe` modules to operate without restriction, and for Safe
Haskell inference to infer modules with unsafe overlaps as unsafe.

One alternative design would be to also consider if an instance was imported as
a `safe` import or not and only apply the restriction to instances imported
safely. However, since instances are global and can be imported through more
than one path, this alternative doesn't work.

Note [Safe Haskell Overlapping Instances Implementation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
How is this implemented? It's complicated! So we'll step through it all:

 1) `InstEnv.lookupInstEnv` -- Performs instance resolution, so this is where
    we check if a particular type-class method call is safe or unsafe. We do this
    through the return type, `ClsInstLookupResult`, where the last parameter is a
    list of instances that are unsafe to overlap. When the method call is safe,
    the list is null.

 2) `GHC.Tc.Solver.Dict.matchClassInst` -- This module drives the instance resolution
    / dictionary generation. The return type is `ClsInstResult`, which either
    says no instance matched, or one found, and if it was a safe or unsafe
    overlap.

 3) `GHC.Tc.Solver.Dict.tryInstances` -- Takes a dictionary / class constraint and
     tries to resolve it by calling (in part) `matchClassInst`. The resolving
     mechanism has a work list (of constraints) that it process one at a time. If
     the constraint can't be resolved, it's added to an inert set. When compiling
     an `-XSafe` or `-XTrustworthy` module, we follow this approach as we know
     compilation should fail. These are handled as normal constraint resolution
     failures from here-on (see step 6).

     Otherwise, we may be inferring safety (or using `-Wunsafe`), and
     compilation should succeed, but print warnings and/or mark the compiled module
     as `-XUnsafe`. In this case, we call `insertSafeOverlapFailureTcS` which adds
     the unsafe (but resolved!) constraint to the `inert_safehask` field of
     `InertCans`.

 4) `GHC.Tc.Solver.simplifyTop`:
       * Call simplifyTopWanteds, the top-level function for driving the simplifier for
         constraint resolution.

       * Once finished, call `getSafeOverlapFailures` to retrieve the
         list of overlapping instances that were successfully resolved,
         but unsafe. Remember, this is only applicable for generating warnings
         (`-Wunsafe`) or inferring a module unsafe. `-XSafe` and `-XTrustworthy`
         cause compilation failure by not resolving the unsafe constraint at all.

       * For unresolved constraints (all types), call `GHC.Tc.Errors.reportUnsolved`,
         while for resolved but unsafe overlapping dictionary constraints, call
         `GHC.Tc.Errors.warnAllUnsolved`. Both functions convert constraints into a
         warning message for the user.

       * In the case of `warnAllUnsolved` for resolved, but unsafe
         dictionary constraints, we collect the generated warning
         message (pop it) and call `GHC.Tc.Utils.Monad.recordUnsafeInfer` to
         mark the module we are compiling as unsafe, passing the
         warning message along as the reason.

 5) `GHC.Tc.Errors.*Unsolved` -- Generates error messages for constraints by
    actually calling `InstEnv.lookupInstEnv` again! Yes, confusing, but all we
    know is the constraint that is unresolved or unsafe. For dictionary, all we
    know is that we need a dictionary of type C, but not what instances are
    available and how they overlap. So we once again call `lookupInstEnv` to
    figure that out so we can generate a helpful error message.

 6) `GHC.Tc.Utils.Monad.recordUnsafeInfer` -- Save the unsafe result and reason in
      IORefs called `tcg_safe_infer` and `tcg_safe_infer_reason`.

 7) `GHC.Driver.Main.tcRnModule'` -- Reads `tcg_safe_infer` after type-checking, calling
    `GHC.Driver.Main.markUnsafeInfer` (passing the reason along) when safe-inference
    failed.

Note [No defaulting in the ambiguity check]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When simplifying constraints for the ambiguity check, we use
solveWanteds, not simplifyTopWanteds, so that we do no defaulting.
#11947 was an example:
   f :: Num a => Int -> Int
This is ambiguous of course, but we don't want to default the
(Num alpha) constraint to (Num Int)!  Doing so gives a defaulting
warning, but no error.

Note [Defaulting insolubles]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If a set of wanteds is insoluble, we have no hope of accepting the
program. Yet we do not stop constraint solving, etc., because we may
simplify the wanteds to produce better error messages. So, once
we have an insoluble constraint, everything we do is just about producing
helpful error messages.

Should we default in this case or not? Let's look at an example (tcfail004):

  (f,g) = (1,2,3)

With defaulting, we get a conflict between (a0,b0) and (Integer,Integer,Integer).
Without defaulting, we get a conflict between (a0,b0) and (a1,b1,c1). I (Richard)
find the latter more helpful. Several other test cases (e.g. tcfail005) suggest
similarly. So: we should not do class defaulting with insolubles.

On the other hand, RuntimeRep-defaulting is different. Witness tcfail078:

  f :: Integer i => i
  f =               0

Without RuntimeRep-defaulting, we GHC suggests that Integer should have kind
TYPE r0 -> Constraint and then complains that r0 is actually untouchable
(presumably, because it can't be sure if `Integer i` entails an equality).
If we default, we are told of a clash between (* -> Constraint) and Constraint.
The latter seems far better, suggesting we *should* do RuntimeRep-defaulting
even on insolubles.

But, evidently, not always. Witness UnliftedNewtypesInfinite:

  newtype Foo = FooC (# Int#, Foo #)

This should fail with an occurs-check error on the kind of Foo (with -XUnliftedNewtypes).
If we default RuntimeRep-vars, we get

  Expecting a lifted type, but ‘(# Int#, Foo #)’ is unlifted

which is just plain wrong.

Another situation in which we don't want to default involves concrete metavariables.

In equalities such as   alpha[conc] ~# rr[sk]  ,  alpha[conc] ~# RR beta[tau]
for a type family RR (all at kind RuntimeRep), we would prefer to report a
representation-polymorphism error rather than default alpha and get error:

  Could not unify `rr` with `Lifted` / Could not unify `RR b0` with `Lifted`

which is very confusing. For this reason, we weed out the concrete
metavariables participating in such equalities in nonDefaultableTyVarsOfWC.
Just looking at insolublity is not enough, as `alpha[conc] ~# RR beta[tau]` could
become soluble after defaulting beta (see also #21430).

Conclusion: we should do RuntimeRep-defaulting on insolubles only when the
user does not want to hear about RuntimeRep stuff -- that is, when
-fprint-explicit-runtime-reps is not set.
However, we must still take care not to default concrete type variables
participating in an equality with a non-concrete type, as seen in the
last example above.
-}

------------------
simplifyAmbiguityCheck :: Type -> WantedConstraints -> TcM ()
simplifyAmbiguityCheck ty wc
  = do { traceTc "simplifyAmbiguityCheck {" $
         text "type = " <+> ppr ty $$ text "wanted = " <+> ppr wc

       ; (final_wc, _) <- runTcS $ do { wc1 <- solveWanteds wc
                                      ; tryUnsatisfiableGivens wc1 }
             -- NB: no defaulting!  See Note [No defaulting in the ambiguity check]
             -- Note: we do still use Unsatisfiable Givens to solve Wanteds,
             -- see Wrinkle [Ambiguity] under point (C) of
             -- Note [Implementation of Unsatisfiable constraints] in GHC.Tc.Errors.

       ; discardResult (reportUnsolved final_wc)

       ; traceTc "End simplifyAmbiguityCheck }" empty }

------------------
simplifyInteractive :: WantedConstraints -> TcM (Bag EvBind)
simplifyInteractive wanteds
  = traceTc "simplifyInteractive" empty >>
    simplifyTop wanteds

------------------
simplifyDefault :: ThetaType    -- Wanted; has no type variables in it
                -> TcM Bool     -- Return if the constraint is soluble
simplifyDefault theta
  = do { traceTc "simplifyDefault" empty
       ; wanteds  <- newWanteds DefaultOrigin theta
       ; (unsolved, _) <- runTcS (solveWanteds (mkSimpleWC wanteds))
       ; return (isEmptyWC unsolved) }

------------------
{- Note [Pattern match warnings with insoluble Givens]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A pattern match on a GADT can introduce new type-level information, which needs
to be analysed in order to get the expected pattern match warnings.

For example:

> type IsBool :: Type -> Constraint
> type family IsBool a where
>   IsBool Bool = ()
>   IsBool b    = b ~ Bool
>
> data T a where
>   MkTInt  :: Int -> T Int
>   MkTBool :: IsBool b => b -> T b
>
> f :: T Int -> Int
> f (MkTInt i) = i

The pattern matching performed by `f` is complete: we can't ever call
`f (MkTBool b)`, as type-checking that application would require producing
evidence for `Int ~ Bool`, which can't be done.

The pattern match checker uses `tcCheckGivens` to accumulate all the Given
constraints, and relies on `tcCheckGivens` to return Nothing if the
Givens become insoluble.   `tcCheckGivens` in turn relies on `insolubleCt`
to identify these insoluble constraints.  So the precise definition of
`insolubleCt` has a big effect on pattern match overlap warnings.

To detect this situation, we check whether there are any insoluble Given
constraints. In the example above, the insoluble constraint was an
equality constraint, but it is also important to detect custom type errors:

> type NotInt :: Type -> Constraint
> type family NotInt a where
>   NotInt Int = TypeError (Text "That's Int, silly.")
>   NotInt _   = ()
>
> data R a where
>   MkT1 :: a -> R a
>   MkT2 :: NotInt a => R a
>
> foo :: R Int -> Int
> foo (MkT1 x) = x

To see that we can't call `foo (MkT2)`, we must detect that `NotInt Int` is insoluble
because it is a custom type error.
Failing to do so proved quite inconvenient for users, as evidence by the
tickets #11503 #14141 #16377 #20180.
Test cases: T11503, T14141.

Examples of constraints that tcCheckGivens considers insoluble:
  - Int ~ Bool,
  - Coercible Float Word,
  - TypeError msg.

Non-examples:
  - constraints which we know aren't satisfied,
    e.g. Show (Int -> Int) when no such instance is in scope,
  - Eq (TypeError msg),
  - C (Int ~ Bool), with @class C (c :: Constraint)@.
-}

tcCheckGivens :: InertSet -> Bag EvVar -> TcM (Maybe InertSet)
-- ^ Return (Just new_inerts) if the Givens are satisfiable, Nothing if definitely
-- contradictory.
--
-- See Note [Pattern match warnings with insoluble Givens] above.
tcCheckGivens inerts given_ids = do
  (sat, new_inerts) <- runTcSInerts inerts $ do
    traceTcS "checkGivens {" (ppr inerts <+> ppr given_ids)
    lcl_env <- TcS.getLclEnv
    let given_loc = mkGivenLoc topTcLevel (getSkolemInfo unkSkol) (mkCtLocEnv lcl_env)
    let given_cts = mkGivens given_loc (bagToList given_ids)
    -- See Note [Superclasses and satisfiability]
    solveSimpleGivens given_cts
    insols <- getInertInsols
    insols <- try_harder insols
    traceTcS "checkGivens }" (ppr insols)
    return (isEmptyBag insols)
  return $ if sat then Just new_inerts else Nothing
  where
    try_harder :: Cts -> TcS Cts
    -- Maybe we have to search up the superclass chain to find
    -- an unsatisfiable constraint.  Example: pmcheck/T3927b.
    -- At the moment we try just once
    try_harder insols
      | not (isEmptyBag insols)   -- We've found that it's definitely unsatisfiable
      = return insols             -- Hurrah -- stop now.
      | otherwise
      = do { pending_given <- getPendingGivenScs
           ; new_given <- makeSuperClasses pending_given
           ; solveSimpleGivens new_given
           ; getInertInsols }

tcCheckWanteds :: InertSet -> ThetaType -> TcM Bool
-- ^ Return True if the Wanteds are soluble, False if not
tcCheckWanteds inerts wanteds = do
  cts <- newWanteds PatCheckOrigin wanteds
  (sat, _new_inerts) <- runTcSInerts inerts $ do
    traceTcS "checkWanteds {" (ppr inerts <+> ppr wanteds)
    -- See Note [Superclasses and satisfiability]
    wcs <- solveWanteds (mkSimpleWC cts)
    traceTcS "checkWanteds }" (ppr wcs)
    return (isSolvedWC wcs)
  return sat

-- | Normalise a type as much as possible using the given constraints.
-- See @Note [tcNormalise]@.
tcNormalise :: InertSet -> Type -> TcM Type
tcNormalise inerts ty
  = do { norm_loc <- getCtLocM PatCheckOrigin Nothing
       ; (res, _new_inerts) <- runTcSInerts inerts $
             do { traceTcS "tcNormalise {" (ppr inerts)
                ; ty' <- rewriteType norm_loc ty
                ; traceTcS "tcNormalise }" (ppr ty')
                ; pure ty' }
       ; return res }

{- Note [Superclasses and satisfiability]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Expand superclasses before starting, because (Int ~ Bool), has
(Int ~~ Bool) as a superclass, which in turn has (Int ~N# Bool)
as a superclass, and it's the latter that is insoluble.  See
Note [The equality types story] in GHC.Builtin.Types.Prim.

If we fail to prove unsatisfiability we (arbitrarily) try just once to
find superclasses, using try_harder.  Reason: we might have a type
signature
   f :: F op (Implements push) => ..
where F is a type function.  This happened in #3972.

We could do more than once but we'd have to have /some/ limit: in the
the recursive case, we would go on forever in the common case where
the constraints /are/ satisfiable (#10592 comment:12!).

For straightforward situations without type functions the try_harder
step does nothing.

Note [tcNormalise]
~~~~~~~~~~~~~~~~~~
tcNormalise is a rather atypical entrypoint to the constraint solver. Whereas
most invocations of the constraint solver are intended to simplify a set of
constraints or to decide if a particular set of constraints is satisfiable,
the purpose of tcNormalise is to take a type, plus some locally solved
constraints in the form of an InertSet, and normalise the type as much as
possible with respect to those constraints.

It does *not* reduce type or data family applications or look through newtypes.

Why is this useful? As one example, when coverage-checking an EmptyCase
expression, it's possible that the type of the scrutinee will only reduce
if some local equalities are solved for. See "Wrinkle: Local equalities"
in Note [Type normalisation] in "GHC.HsToCore.Pmc".

To accomplish its stated goal, tcNormalise first initialises the solver monad
with the given InertCans, then uses rewriteType to simplify the desired type
with respect to the Givens in the InertCans.

***********************************************************************************
*                                                                                 *
*                            Inference
*                                                                                 *
***********************************************************************************

Note [Inferring the type of a let-bound variable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   f x = rhs

To infer f's type we do the following:
 * Gather the constraints for the RHS with ambient level *one more than*
   the current one.  This is done by the call
        pushLevelAndCaptureConstraints (tcMonoBinds...)
   in GHC.Tc.Gen.Bind.tcPolyInfer

 * Call simplifyInfer to simplify the constraints and decide what to
   quantify over. We pass in the level used for the RHS constraints,
   here called rhs_tclvl.

This ensures that the implication constraint we generate, if any,
has a strictly-increased level compared to the ambient level outside
the let binding.

Note [Inferring principal types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We don't always infer principal types. For instance, the inferred type for

> f x = show [x]

is

> f :: Show a => a -> String

This is not the most general type if we allow flexible contexts.
Indeed, if we try to write the following

> g :: Show [a] => a -> String
> g x = f x

we get the error:

  * Could not deduce (Show a) arising from a use of `f'
    from the context: Show [a]

Though replacing f x in the right-hand side of g with the definition
of f x works, the call to f x does not. This is the hallmark of
unprincip{led,al} types.

Another example:

> class C a
> class D a where
>   d :: a
> instance C a => D a where
>   d = undefined
> h _ = d   -- argument is to avoid the monomorphism restriction

The inferred type for h is

> h :: C a => t -> a

even though

> h :: D a => t -> a

is more general.

The fix is easy: don't simplify constraints before inferring a type.
That is, have the inferred type quantify over all constraints that arise
in a definition's right-hand side, even if they are simplifiable.
Unfortunately, this would yield all manner of unwieldy types,
and so we won't do so.
-}

-- | How should we choose which constraints to quantify over?
data InferMode = ApplyMR          -- ^ Apply the monomorphism restriction,
                                  -- never quantifying over any constraints
               | EagerDefaulting  -- ^ See Note [TcRnExprMode] in "GHC.Tc.Module",
                                  -- the :type +d case; this mode refuses
                                  -- to quantify over any defaultable constraint
               | NoRestrictions   -- ^ Quantify over any constraint that
                                  -- satisfies pickQuantifiablePreds

instance Outputable InferMode where
  ppr ApplyMR         = text "ApplyMR"
  ppr EagerDefaulting = text "EagerDefaulting"
  ppr NoRestrictions  = text "NoRestrictions"

simplifyInfer :: TcLevel               -- Used when generating the constraints
              -> InferMode
              -> [TcIdSigInst]         -- Any signatures (possibly partial)
              -> [(Name, TcTauType)]   -- Variables to be generalised,
                                       -- and their tau-types
              -> WantedConstraints
              -> TcM ([TcTyVar],    -- Quantify over these type variables
                      [EvVar],      -- ... and these constraints (fully zonked)
                      TcEvBinds,    -- ... binding these evidence variables
                      Bool)         -- True <=> the residual constraints are insoluble

simplifyInfer rhs_tclvl infer_mode sigs name_taus wanteds
  | isEmptyWC wanteds
   = do { -- When quantifying, we want to preserve any order of variables as they
          -- appear in partial signatures. cf. decideQuantifiedTyVars
          let psig_tv_tys = [ mkTyVarTy tv | sig <- partial_sigs
                                           , (_,Bndr tv _) <- sig_inst_skols sig ]
              psig_theta  = [ pred | sig <- partial_sigs
                                   , pred <- sig_inst_theta sig ]

       ; dep_vars <- candidateQTyVarsOfTypes (psig_tv_tys ++ psig_theta ++ map snd name_taus)

       ; skol_info <- mkSkolemInfo (InferSkol name_taus)
       ; qtkvs <- quantifyTyVars skol_info DefaultNonStandardTyVars dep_vars
       ; traceTc "simplifyInfer: empty WC" (ppr name_taus $$ ppr qtkvs)
       ; return (qtkvs, [], emptyTcEvBinds, False) }

  | otherwise
  = do { traceTc "simplifyInfer {"  $ vcat
             [ text "sigs =" <+> ppr sigs
             , text "binds =" <+> ppr name_taus
             , text "rhs_tclvl =" <+> ppr rhs_tclvl
             , text "infer_mode =" <+> ppr infer_mode
             , text "(unzonked) wanted =" <+> ppr wanteds
             ]

       ; let psig_theta = concatMap sig_inst_theta partial_sigs

       -- First do full-blown solving
       -- NB: we must gather up all the bindings from doing
       -- this solving; hence (runTcSWithEvBinds ev_binds_var).
       -- And note that since there are nested implications,
       -- calling solveWanteds will side-effect their evidence
       -- bindings, so we can't just revert to the input
       -- constraint.

       ; ev_binds_var <- TcM.newTcEvBinds
       ; psig_evs     <- newWanteds AnnOrigin psig_theta
       ; wanted_transformed
            <- setTcLevel rhs_tclvl $
               runTcSWithEvBinds ev_binds_var $
               solveWanteds (mkSimpleWC psig_evs `andWC` wanteds)
               -- psig_evs : see Note [Add signature contexts as wanteds]
               -- See Note [Inferring principal types]

       -- Find quant_pred_candidates, the predicates that
       -- we'll consider quantifying over
       -- NB1: wanted_transformed does not include anything provable from
       --      the psig_theta; it's just the extra bit
       -- NB2: We do not do any defaulting when inferring a type, this can lead
       --      to less polymorphic types, see Note [Default while Inferring]
       ; wanted_transformed <- TcM.liftZonkM $ TcM.zonkWC wanted_transformed
       ; let definite_error = insolubleWC wanted_transformed
                              -- See Note [Quantification with errors]
             quant_pred_candidates
               | definite_error = []
               | otherwise      = ctsPreds (approximateWC False wanted_transformed)

       -- Decide what type variables and constraints to quantify
       -- NB: quant_pred_candidates is already fully zonked
       -- NB: bound_theta are constraints we want to quantify over,
       --     including the psig_theta, which we always quantify over
       -- NB: bound_theta are fully zonked
       -- rec {..}: see Note [Keeping SkolemInfo inside a SkolemTv]
       --           in GHC.Tc.Utils.TcType
       ; rec { (qtvs, bound_theta, co_vars) <- decideQuantification skol_info infer_mode rhs_tclvl
                                                     name_taus partial_sigs
                                                     quant_pred_candidates
             ; bound_theta_vars <- mapM TcM.newEvVar bound_theta

             ; let full_theta = map idType bound_theta_vars
             ; skol_info <- mkSkolemInfo (InferSkol [ (name, mkPhiTy full_theta ty)
                                                    | (name, ty) <- name_taus ])
       }


       -- Now emit the residual constraint
       ; emitResidualConstraints rhs_tclvl ev_binds_var
                                 name_taus co_vars qtvs bound_theta_vars
                                 wanted_transformed

         -- All done!
       ; traceTc "} simplifyInfer/produced residual implication for quantification" $
         vcat [ text "quant_pred_candidates =" <+> ppr quant_pred_candidates
              , text "psig_theta ="     <+> ppr psig_theta
              , text "bound_theta ="    <+> pprCoreBinders bound_theta_vars
              , text "qtvs ="           <+> ppr qtvs
              , text "definite_error =" <+> ppr definite_error ]

       ; return ( qtvs, bound_theta_vars, TcEvBinds ev_binds_var, definite_error ) }
         -- NB: bound_theta_vars must be fully zonked
  where
    partial_sigs = filter isPartialSig sigs

--------------------
emitResidualConstraints :: TcLevel -> EvBindsVar
                        -> [(Name, TcTauType)]
                        -> CoVarSet -> [TcTyVar] -> [EvVar]
                        -> WantedConstraints -> TcM ()
-- Emit the remaining constraints from the RHS.
emitResidualConstraints rhs_tclvl ev_binds_var
                        name_taus co_vars qtvs full_theta_vars wanteds
  | isEmptyWC wanteds
  = return ()

  | otherwise
  = do { wanted_simple <- TcM.liftZonkM $ TcM.zonkSimples (wc_simple wanteds)
       ; let (outer_simple, inner_simple) = partitionBag is_mono wanted_simple
             is_mono ct
               | Just ct_ev_id <- wantedEvId_maybe ct
               = ct_ev_id `elemVarSet` co_vars
               | otherwise
               = False
             -- Reason for the partition:
             -- see Note [Emitting the residual implication in simplifyInfer]

-- Already done by defaultTyVarsAndSimplify
--      ; _ <- TcM.promoteTyVarSet (tyCoVarsOfCts outer_simple)

        ; let inner_wanted = wanteds { wc_simple = inner_simple }
        ; implics <- if isEmptyWC inner_wanted
                     then return emptyBag
                     else do implic1 <- newImplication
                             return $ unitBag $
                                      implic1  { ic_tclvl     = rhs_tclvl
                                               , ic_skols     = qtvs
                                               , ic_given     = full_theta_vars
                                               , ic_wanted    = inner_wanted
                                               , ic_binds     = ev_binds_var
                                               , ic_given_eqs = MaybeGivenEqs
                                               , ic_info      = skol_info }

        ; emitConstraints (emptyWC { wc_simple = outer_simple
                                   , wc_impl   = implics }) }
  where
    full_theta = map idType full_theta_vars
    skol_info = InferSkol [ (name, mkPhiTy full_theta ty)
                          | (name, ty) <- name_taus ]
    -- We don't add the quantified variables here, because they are
    -- also bound in ic_skols and we want them to be tidied
    -- uniformly.

--------------------
findInferredDiff :: TcThetaType -> TcThetaType -> TcM TcThetaType
-- Given a partial type signature f :: (C a, D a, _) => blah
-- and the inferred constraints (X a, D a, Y a, C a)
-- compute the difference, which is what will fill in the "_" underscore,
-- In this case the diff is (X a, Y a).
findInferredDiff annotated_theta inferred_theta
  | null annotated_theta   -- Short cut the common case when the user didn't
  = return inferred_theta  -- write any constraints in the partial signature
  | otherwise
  = pushTcLevelM_ $
    do { lcl_env   <- TcM.getLclEnv
       ; given_ids <- mapM TcM.newEvVar annotated_theta
       ; wanteds   <- newWanteds AnnOrigin inferred_theta
       ; let given_loc = mkGivenLoc topTcLevel (getSkolemInfo unkSkol) (mkCtLocEnv lcl_env)
             given_cts = mkGivens given_loc given_ids

       ; (residual, _) <- runTcS $
                          do { _ <- solveSimpleGivens given_cts
                             ; solveSimpleWanteds (listToBag (map mkNonCanonical wanteds)) }
         -- NB: There are no meta tyvars fromn this level annotated_theta
         -- because we have either promoted them or unified them
         -- See `Note [Quantification and partial signatures]` Wrinkle 2

       ; return (map (box_pred . ctPred) $
                 bagToList               $
                 wc_simple residual) }
  where
     box_pred :: PredType -> PredType
     box_pred pred = case classifyPredType pred of
                        EqPred rel ty1 ty2
                          | Just (cls,tys) <- boxEqPred rel ty1 ty2
                          -> mkClassPred cls tys
                          | otherwise
                          -> pprPanic "findInferredDiff" (ppr pred)
                        _other -> pred

{- Note [Emitting the residual implication in simplifyInfer]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   f = e
where f's type is inferred to be something like (a, Proxy k (Int |> co))
and we have an as-yet-unsolved, or perhaps insoluble, constraint
   [W] co :: Type ~ k
We can't form types like (forall co. blah), so we can't generalise over
the coercion variable, and hence we can't generalise over things free in
its kind, in the case 'k'.  But we can still generalise over 'a'.  So
we'll generalise to
   f :: forall a. (a, Proxy k (Int |> co))
Now we do NOT want to form the residual implication constraint
   forall a. [W] co :: Type ~ k
because then co's eventual binding (which will be a value binding if we
use -fdefer-type-errors) won't scope over the entire binding for 'f' (whose
type mentions 'co').  Instead, just as we don't generalise over 'co', we
should not bury its constraint inside the implication.  Instead, we must
put it outside.

That is the reason for the partitionBag in emitResidualConstraints,
which takes the CoVars free in the inferred type, and pulls their
constraints out.  (NB: this set of CoVars should be closed-over-kinds.)

All rather subtle; see #14584.

Note [Add signature contexts as wanteds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this (#11016):
  f2 :: (?x :: Int) => _
  f2 = ?x

or this
  class C a b | a -> b
  g :: C p q => p -> q
  f3 :: C Int b => _
  f3 = g (3::Int)

We'll use plan InferGen because there are holes in the type.  But:
 * For f2 we want to have the (?x :: Int) constraint floating around
   so that the functional dependencies kick in.  Otherwise the
   occurrence of ?x on the RHS produces constraint (?x :: alpha), and
   we won't unify alpha:=Int.

 * For f3 want the (C Int b) constraint from the partial signature
   to meet the (C Int beta) constraint we get from the call to g; again,
   fundeps

Solution: in simplifyInfer, we add the constraints from the signature
as extra Wanteds.

Why Wanteds?  Wouldn't it be neater to treat them as Givens?  Alas
that would mess up (GivenInv) in Note [TcLevel invariants].  Consider
    f :: (Eq a, _) => blah1
    f = ....g...
    g :: (Eq b, _) => blah2
    g = ...f...

Then we have two psig_theta constraints (Eq a[tv], Eq b[tv]), both with
TyVarTvs inside.  Ultimately a[tv] := b[tv], but only when we've solved
all those constraints.  And both have level 1, so we can't put them as
Givens when solving at level 1.

Best to treat them as Wanteds.

But see also #20076, which would be solved if they were Givens.


************************************************************************
*                                                                      *
                Quantification
*                                                                      *
************************************************************************

Note [Deciding quantification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If the monomorphism restriction does not apply, then we quantify as follows:

* Step 1: decidePromotedTyVars.
  Take the global tyvars, and "grow" them using functional dependencies
     E.g.  if x:alpha is in the environment, and alpha ~ [beta] (which can
          happen because alpha is untouchable here) then do not quantify over
          beta, because alpha fixes beta, and beta is effectively free in
          the environment too; this logic extends to general fundeps, not
          just equalities

  We also account for the monomorphism restriction; if it applies,
  add the free vars of all the constraints.

  Result is mono_tvs; we will promote all of these to the outer levek,
  and certainly not quantify over them.

* Step 2: defaultTyVarsAndSimplify.
  Default any non-promoted tyvars (i.e ones that are definitely
  not going to become further constrained), and re-simplify the
  candidate constraints.

  Motivation for re-simplification (#7857): imagine we have a
  constraint (C (a->b)), where 'a :: TYPE l1' and 'b :: TYPE l2' are
  not free in the envt, and instance forall (a::*) (b::*). (C a) => C
  (a -> b) The instance doesn't match while l1,l2 are polymorphic, but
  it will match when we default them to LiftedRep.

  This is all very tiresome.

  This step also promotes the mono_tvs from Step 1. See
  Note [Promote monomorphic tyvars]. In fact, the *only*
  use of the mono_tvs from Step 1 is to promote them here.
  This promotion effectively stops us from quantifying over them
  later, in Step 3. Because the actual variables to quantify
  over are determined in Step 3 (not in Step 1), it is OK for
  the mono_tvs to be missing some variables free in the
  environment. This is why removing the psig_qtvs is OK in
  decidePromotedTyVars. Test case for this scenario: T14479.

* Step 3: decideQuantifiedTyVars.
  Decide which variables to quantify over, as follows:

  - Take the free vars of the partial-type-signature types and constraints,
    and the tau-type (zonked_tau_tvs), and then "grow"
    them using all the constraints.  These are grown_tcvs.
    See Note [growThetaTyVars vs closeWrtFunDeps].

  - Use quantifyTyVars to quantify over the free variables of all the types
    involved, but only those in the grown_tcvs.

  Result is qtvs.

* Step 4: Filter the constraints using pickQuantifiablePreds and the
  qtvs. We have to zonk the constraints first, so they "see" the
  freshly created skolems.

Note [Unconditionally resimplify constraints when quantifying]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
During quantification (in defaultTyVarsAndSimplify, specifically), we re-invoke
the solver to simplify the constraints before quantifying them. We do this for
two reasons, enumerated below. We could, in theory, detect when either of these
cases apply and simplify only then, but collecting this information is bothersome,
and simplifying redundantly causes no real harm. Note that this code path
happens only for definitions
  * without a type signature
  * when -XMonoLocalBinds does not apply
  * with unsolved constraints
and so the performance cost will be small.

1. Defaulting

Defaulting the variables handled by defaultTyVar may unlock instance simplifications.
Example (typecheck/should_compile/T20584b):

  with (t :: Double) (u :: String) = printf "..." t u

We know the types of t and u, but we do not know the return type of `with`. So, we
assume `with :: alpha`, where `alpha :: TYPE rho`. The type of printf is
  printf :: PrintfType r => String -> r
The occurrence of printf is instantiated with a fresh var beta. We then get
  beta := Double -> String -> alpha
and
  [W] PrintfType (Double -> String -> alpha)

Module Text.Printf exports
  instance (PrintfArg a, PrintfType r) => PrintfType (a -> r)
and it looks like that instance should apply.

But I have elided some key details: (->) is polymorphic over multiplicity and
runtime representation. Here it is in full glory:
  [W] PrintfType ((Double :: Type) %m1 -> (String :: Type) %m2 -> (alpha :: TYPE rho))
  instance (PrintfArg a, PrintfType r) => PrintfType ((a :: Type) %Many -> (r :: Type))

Because we do not know that m1 is Many, we cannot use the instance. (Perhaps a better instance
would have an explicit equality constraint to the left of =>, but that's not what we have.)
Then, in defaultTyVarsAndSimplify, we get m1 := Many, m2 := Many, and rho := LiftedRep.
Yet it's too late to simplify the quantified constraint, and thus GHC infers
  wait :: PrintfType (Double -> String -> t) => Double -> String -> t
which is silly. Simplifying again after defaulting solves this problem.

2. Interacting functional dependencies

Suppose we have

  class C a b | a -> b

and we are running simplifyInfer over

  forall[2] x. () => [W] C a beta1[1]
  forall[2] y. () => [W] C a beta2[1]

These are two implication constraints, both of which contain a
wanted for the class C. Neither constraint mentions the bound
skolem. We might imagine that these constraints could thus float
out of their implications and then interact, causing beta1 to unify
with beta2, but constraints do not currently float out of implications.

Unifying the beta1 and beta2 is important. Without doing so, then we might
infer a type like (C a b1, C a b2) => a -> a, which will fail to pass the
ambiguity check, which will say (rightly) that it cannot unify b1 with b2, as
required by the fundep interactions. This happens in the parsec library, and
in test case typecheck/should_compile/FloatFDs.

If we re-simplify, however, the two fundep constraints will interact, causing
a unification between beta1 and beta2, and all will be well. The key step
is that this simplification happens *after* the call to approximateWC in
simplifyInfer.

-}

decideQuantification
  :: SkolemInfo
  -> InferMode
  -> TcLevel
  -> [(Name, TcTauType)]   -- Variables to be generalised
  -> [TcIdSigInst]         -- Partial type signatures (if any)
  -> [PredType]            -- Candidate theta; already zonked
  -> TcM ( [TcTyVar]       -- Quantify over these (skolems)
         , [PredType]      -- and this context (fully zonked)
         , CoVarSet)
-- See Note [Deciding quantification]
decideQuantification skol_info infer_mode rhs_tclvl name_taus psigs candidates
  = do { -- Step 1: find the mono_tvs
       ; (candidates, co_vars, mono_tvs0)
             <- decidePromotedTyVars infer_mode name_taus psigs candidates

       -- Step 2: default any non-mono tyvars, and re-simplify
       -- This step may do some unification, but result candidates is zonked
       ; candidates <- defaultTyVarsAndSimplify rhs_tclvl candidates

       -- Step 3: decide which kind/type variables to quantify over
       ; qtvs <- decideQuantifiedTyVars skol_info name_taus psigs candidates

       -- Step 4: choose which of the remaining candidate
       --         predicates to actually quantify over
       -- NB: decideQuantifiedTyVars turned some meta tyvars
       -- into quantified skolems, so we have to zonk again
       ; (candidates, psig_theta) <- TcM.liftZonkM $
          do { candidates <- TcM.zonkTcTypes candidates
             ; psig_theta <- TcM.zonkTcTypes (concatMap sig_inst_theta psigs)
             ; return (candidates, psig_theta) }
       ; min_theta  <- pickQuantifiablePreds (mkVarSet qtvs) mono_tvs0 candidates

       -- Take account of partial type signatures
       -- See Note [Constraints in partial type signatures]
       ; let min_psig_theta = mkMinimalBySCs id psig_theta
       ; theta <- if
           | null psigs -> return min_theta                 -- Case (P3)
           | not (all has_extra_constraints_wildcard psigs) -- Case (P2)
             -> return min_psig_theta
           | otherwise                                      -- Case (P1)
             -> do { diff <- findInferredDiff min_psig_theta min_theta
                   ; return (min_psig_theta ++ diff) }

       ; traceTc "decideQuantification"
           (vcat [ text "infer_mode:" <+> ppr infer_mode
                 , text "candidates:" <+> ppr candidates
                 , text "psig_theta:" <+> ppr psig_theta
                 , text "co_vars:"    <+> ppr co_vars
                 , text "qtvs:"       <+> ppr qtvs
                 , text "theta:"      <+> ppr theta ])
       ; return (qtvs, theta, co_vars) }

  where
    has_extra_constraints_wildcard (TISI { sig_inst_wcx = Just {} }) = True
    has_extra_constraints_wildcard _                                 = False

{- Note [Constraints in partial type signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have decided to quantify over min_theta, say (Eq a, C a, Ix a).
Then we distinguish three cases:

(P1) No partial type signatures: just quantify over min_theta

(P2) Partial type signatures with no extra_constraints wildcard:
      e.g.   f :: (Eq a, C a) => a -> _
     Quantify over psig_theta: the user has explicitly specified the
     entire context.

     That may mean we have an unsolved residual constraint (Ix a) arising
     from the RHS of the function. But so be it: the user said (Eq a, C a).

(P3) Partial type signature with an extra_constraints wildcard.
      e.g.   f :: (Eq a, C a, _) => a -> a
    Quantify over (psig_theta ++ diff)
      where diff = min_theta - psig_theta, using findInferredDiff.
    In our example, diff = Ix a

Some rationale and observations

* See Note [When the MR applies] in GHC.Tc.Gen.Bind.

* We always want to quantify over psig_theta (if present).  The user specified
  it!  And pickQuantifiableCandidates might have dropped some
  e.g. CallStack constraints.  c.f #14658
       equalities (a ~ Bool)

* In case (P3) we ask that /all/ the signatures have an extra-constraints
  wildcard.  It's a bit arbitrary; not clear what the "right" thing is.

* In (P2) we encounter #20076:
     f :: Eq [a] => a -> _
     f x = [x] == [x]
  From the RHS we get [W] Eq [a].  We simplify those Wanteds in simplifyInfer,
  to get (Eq a).  But then we quantify over the user-specified (Eq [a]), leaving
  a residual implication constraint (forall a. Eq [a] => [W] Eq a), which is
  insoluble.  Idea: in simplifyInfer we could put the /un-simplified/ constraints
  in the residual -- at least in the case like #20076 where the partial signature
  fully specifies the final constraint. Maybe: a battle for another day.

* It's helpful to use the same "find difference" algorithm, `findInferredDiff`,
  here as we use in GHC.Tc.Gen.Bind.chooseInferredQuantifiers (#20921)

  At least for single functions we would like to quantify f over precisely the
  same theta as <quant-theta>, so that we get to take the short-cut path in
  `GHC.Tc.Gen.Bind.mkExport`, and avoid calling `tcSubTypeSigma` for impedance
  matching. Why avoid?  Because it falls over for ambiguous types (#20921).

  We can get precisely the same theta by using the same algorithm,
  `findInferredDiff`.

* All of this goes wrong if we have (a) mutual recursion, (b) multiple
  partial type signatures, (c) with different constraints, and (d)
  ambiguous types.  Something like
    f :: forall a. Eq a => F a -> _
    f x = (undefined :: a) == g x undefined
    g :: forall b. Show b => F b -> _ -> b
    g x y = let _ = (f y, show x) in x
  But that's a battle for another day.
-}

decidePromotedTyVars :: InferMode
                     -> [(Name,TcType)]
                     -> [TcIdSigInst]
                     -> [PredType]
                     -> TcM ([PredType], CoVarSet, TcTyVarSet)
-- We are about to generalise over type variables at level N
-- Each must be either
--    (P) promoted
--    (D) defaulted
--    (Q) quantified
-- This function finds (P), the type variables that we are going to promote:
--   (a) Mentioned in a constraint we can't generalise (the MR)
--   (b) Mentioned in the kind of a CoVar; we can't quantify over a CoVar,
--       so we must not quantify over a type variable free in its kind
--   (c) Connected by an equality or fundep to
--          * a type variable at level < N, or
--          * A tyvar subject to (a), (b) or (c)
-- Having found all such level-N tyvars that we can't generalise,
-- promote them, to eliminate them from further consideration.
--
-- Also return CoVars that appear free in the final quantified types
--   we can't quantify over these, and we must make sure they are in scope
decidePromotedTyVars infer_mode name_taus psigs candidates
  = do { tc_lvl <- TcM.getTcLevel
       ; (no_quant, maybe_quant) <- pick infer_mode candidates

       -- If possible, we quantify over partial-sig qtvs, so they are
       -- not mono. Need to zonk them because they are meta-tyvar TyVarTvs
       ; (psig_qtvs, psig_theta, taus) <- TcM.liftZonkM $
          do { psig_qtvs <- zonkTcTyVarsToTcTyVars $ binderVars $
                            concatMap (map snd . sig_inst_skols) psigs
             ; psig_theta <- mapM TcM.zonkTcType $
                             concatMap sig_inst_theta psigs
             ; taus <- mapM (TcM.zonkTcType . snd) name_taus
             ; return (psig_qtvs, psig_theta, taus) }

       ; let psig_tys = mkTyVarTys psig_qtvs ++ psig_theta

             -- (b) The co_var_tvs are tvs mentioned in the types of covars or
             -- coercion holes. We can't quantify over these covars, so we
             -- must include the variable in their types in the mono_tvs.
             -- E.g.  If we can't quantify over co :: k~Type, then we can't
             --       quantify over k either!  Hence closeOverKinds
             -- Recall that coVarsOfTypes also returns coercion holes
             co_vars = coVarsOfTypes (psig_tys ++ taus ++ candidates)
             co_var_tvs = closeOverKinds co_vars

             mono_tvs0 = filterVarSet (not . isQuantifiableTv tc_lvl) $
                         tyCoVarsOfTypes candidates
               -- We need to grab all the non-quantifiable tyvars in the
               -- types so that we can grow this set to find other
               -- non-quantifiable tyvars. This can happen with something like
               --    f x y = ...
               --      where z = x 3
               -- The body of z tries to unify the type of x (call it alpha[1])
               -- with (beta[2] -> gamma[2]). This unification fails because
               -- alpha is untouchable, leaving [W] alpha[1] ~ (beta[2] -> gamma[2]).
               -- We need to know not to quantify over beta or gamma, because they
               -- are in the equality constraint with alpha. Actual test case:
               -- typecheck/should_compile/tc213

             mono_tvs1 = mono_tvs0 `unionVarSet` co_var_tvs

               -- mono_tvs1 is now the set of variables from an outer scope
               -- (that's mono_tvs0) and the set of covars, closed over kinds.
               -- Given this set of variables we know we will not quantify,
               -- we want to find any other variables that are determined by this
               -- set, by functional dependencies or equalities. We thus use
               -- closeWrtFunDeps to find all further variables determined by this root
               -- set. See Note [growThetaTyVars vs closeWrtFunDeps]

             non_ip_candidates = filterOut isIPLikePred candidates
               -- implicit params don't really determine a type variable
               -- (that is, we might have IP "c" Bool and IP "c" Int in different
               -- places within the same program), and
               -- skipping this causes implicit params to monomorphise too many
               -- variables; see Note [Inheriting implicit parameters] in GHC.Tc.Solver.
               -- Skipping causes typecheck/should_compile/tc219 to fail.

             mono_tvs2 = closeWrtFunDeps non_ip_candidates mono_tvs1
               -- mono_tvs2 now contains any variable determined by the "root
               -- set" of monomorphic tyvars in mono_tvs1.

             constrained_tvs = filterVarSet (isQuantifiableTv tc_lvl) $
                               closeWrtFunDeps non_ip_candidates (tyCoVarsOfTypes no_quant)
                                `minusVarSet` mono_tvs2
             -- constrained_tvs: the tyvars that we are not going to
             -- quantify /solely/ because of the monomorphism restriction
             --
             -- (`minusVarSet` mono_tvs2): a type variable is only
             --   "constrained" (so that the MR bites) if it is not
             --   free in the environment (#13785) or is determined
             --   by some variable that is free in the env't

             mono_tvs = (mono_tvs2 `unionVarSet` constrained_tvs)
                        `delVarSetList` psig_qtvs
             -- (`delVarSetList` psig_qtvs): if the user has explicitly
             --   asked for quantification, then that request "wins"
             --   over the MR.
             --
             -- What if a psig variable is also free in the environment
             -- (i.e. says "no" to isQuantifiableTv)? That's OK: explanation
             -- in Step 2 of Note [Deciding quantification].

           -- Warn about the monomorphism restriction
       ; when (case infer_mode of { ApplyMR -> True; _ -> False}) $ do
           let dia = TcRnMonomorphicBindings (map fst name_taus)
           diagnosticTc (constrained_tvs `intersectsVarSet` tyCoVarsOfTypes taus) dia

       -- Promote the mono_tvs: see Note [Promote monomorphic tyvars]
       ; _ <- promoteTyVarSet mono_tvs

       ; traceTc "decidePromotedTyVars" $ vcat
           [ text "infer_mode =" <+> ppr infer_mode
           , text "psigs =" <+> ppr psigs
           , text "psig_qtvs =" <+> ppr psig_qtvs
           , text "mono_tvs0 =" <+> ppr mono_tvs0
           , text "no_quant =" <+> ppr no_quant
           , text "maybe_quant =" <+> ppr maybe_quant
           , text "mono_tvs =" <+> ppr mono_tvs
           , text "co_vars =" <+> ppr co_vars ]

       ; return (maybe_quant, co_vars, mono_tvs0) }
  where
    pick :: InferMode -> [PredType] -> TcM ([PredType], [PredType])
    -- Split the candidates into ones we definitely
    -- won't quantify, and ones that we might
    pick ApplyMR         cand = return (cand, [])
    pick NoRestrictions  cand = return ([], cand)
    pick EagerDefaulting cand = do { os <- xoptM LangExt.OverloadedStrings
                                   ; return (partition (is_int_ct os) cand) }

    -- is_int_ct returns True for a constraint we should /not/ quantify
    -- For EagerDefaulting, do not quantify over
    -- over any interactive class constraint
    is_int_ct ovl_strings pred
      = case classifyPredType pred of
           ClassPred cls _ -> isInteractiveClass ovl_strings cls
           _               -> False

-------------------
defaultTyVarsAndSimplify :: TcLevel
                         -> [PredType]          -- Assumed zonked
                         -> TcM [PredType]      -- Guaranteed zonked
-- Default any tyvar free in the constraints;
-- and re-simplify in case the defaulting allows further simplification
defaultTyVarsAndSimplify rhs_tclvl candidates
  = do {  -- Default any kind/levity vars
       ; DV {dv_kvs = cand_kvs, dv_tvs = cand_tvs}
                <- candidateQTyVarsOfTypes candidates
         -- NB1: decidePromotedTyVars has promoted any type variable fixed by the
         --      type envt, so they won't be chosen by candidateQTyVarsOfTypes
         -- NB2: Defaulting for variables free in tau_tys is done later, by quantifyTyVars
         --      Hence looking only at 'candidates'
         -- NB3: Any covars should already be handled by
         --      the logic in decidePromotedTyVars, which looks at
         --      the constraints generated

       ; poly_kinds  <- xoptM LangExt.PolyKinds
       ; let default_kv | poly_kinds = default_tv
                        | otherwise  = defaultTyVar DefaultKindVars
             default_tv = defaultTyVar (NonStandardDefaulting DefaultNonStandardTyVars)
       ; mapM_ default_kv (dVarSetElems cand_kvs)
       ; mapM_ default_tv (dVarSetElems (cand_tvs `minusDVarSet` cand_kvs))

       ; simplify_cand candidates
       }
  where
    -- See Note [Unconditionally resimplify constraints when quantifying]
    simplify_cand [] = return []  -- Fast path
    simplify_cand candidates
      = do { clone_wanteds <- newWanteds DefaultOrigin candidates
           ; WC { wc_simple = simples } <- setTcLevel rhs_tclvl $
                                           simplifyWantedsTcM clone_wanteds
              -- Discard evidence; simples is fully zonked

           ; let new_candidates = ctsPreds simples
           ; traceTc "Simplified after defaulting" $
                      vcat [ text "Before:" <+> ppr candidates
                           , text "After:"  <+> ppr new_candidates ]
           ; return new_candidates }

------------------
decideQuantifiedTyVars
   :: SkolemInfo
   -> [(Name,TcType)]   -- Annotated theta and (name,tau) pairs
   -> [TcIdSigInst]     -- Partial signatures
   -> [PredType]        -- Candidates, zonked
   -> TcM [TyVar]
-- Fix what tyvars we are going to quantify over, and quantify them
decideQuantifiedTyVars skol_info name_taus psigs candidates
  = do {     -- Why psig_tys? We try to quantify over everything free in here
             -- See Note [Quantification and partial signatures]
             --     Wrinkles 2 and 3
       ; (psig_tv_tys, psig_theta, tau_tys) <- TcM.liftZonkM $
         do { psig_tv_tys <- mapM TcM.zonkTcTyVar [ tv | sig <- psigs
                                                       , (_,Bndr tv _) <- sig_inst_skols sig ]
            ; psig_theta  <- mapM TcM.zonkTcType [ pred | sig <- psigs
                                                        , pred <- sig_inst_theta sig ]
            ; tau_tys     <- mapM (TcM.zonkTcType . snd) name_taus
            ; return (psig_tv_tys, psig_theta, tau_tys) }

       ; let -- Try to quantify over variables free in these types
             psig_tys = psig_tv_tys ++ psig_theta
             seed_tys = psig_tys ++ tau_tys

             -- Now "grow" those seeds to find ones reachable via 'candidates'
             -- See Note [growThetaTyVars vs closeWrtFunDeps]
             grown_tcvs = growThetaTyVars candidates (tyCoVarsOfTypes seed_tys)

       -- Now we have to classify them into kind variables and type variables
       -- (sigh) just for the benefit of -XNoPolyKinds; see quantifyTyVars
       --
       -- Keep the psig_tys first, so that candidateQTyVarsOfTypes produces
       -- them in that order, so that the final qtvs quantifies in the same
       -- order as the partial signatures do (#13524)
       ; dv@DV {dv_kvs = cand_kvs, dv_tvs = cand_tvs} <- candidateQTyVarsOfTypes $
                                                         psig_tys ++ candidates ++ tau_tys
       ; let pick     = (`dVarSetIntersectVarSet` grown_tcvs)
             dvs_plus = dv { dv_kvs = pick cand_kvs, dv_tvs = pick cand_tvs }

       ; traceTc "decideQuantifiedTyVars" (vcat
           [ text "tau_tys =" <+> ppr tau_tys
           , text "candidates =" <+> ppr candidates
           , text "cand_kvs =" <+> ppr cand_kvs
           , text "cand_tvs =" <+> ppr cand_tvs
           , text "tau_tys =" <+> ppr tau_tys
           , text "seed_tys =" <+> ppr seed_tys
           , text "seed_tcvs =" <+> ppr (tyCoVarsOfTypes seed_tys)
           , text "grown_tcvs =" <+> ppr grown_tcvs
           , text "dvs =" <+> ppr dvs_plus])

       ; quantifyTyVars skol_info DefaultNonStandardTyVars dvs_plus }

------------------
-- | When inferring types, should we quantify over a given predicate?
-- See Note [pickQuantifiablePreds]
pickQuantifiablePreds
  :: TyVarSet           -- Quantifying over these
  -> TcTyVarSet         -- mono_tvs0: variables mentioned a candidate
                        --   constraint that come from some outer level
  -> TcThetaType        -- Proposed constraints to quantify
  -> TcM TcThetaType    -- A subset that we can actually quantify
-- This function decides whether a particular constraint should be
-- quantified over, given the type variables that are being quantified
pickQuantifiablePreds qtvs mono_tvs0 theta
  = do { tc_lvl <- TcM.getTcLevel
       ; let is_nested = not (isTopTcLevel tc_lvl)
       ; return (mkMinimalBySCs id $  -- See Note [Minimize by Superclasses]
                 mapMaybe (pick_me is_nested) theta) }
  where
    pick_me is_nested pred
      = let pred_tvs = tyCoVarsOfType pred
            mentions_qtvs = pred_tvs `intersectsVarSet` qtvs
        in case classifyPredType pred of

          ClassPred cls tys
            | Just {} <- isCallStackPred cls tys
              -- NEVER infer a CallStack constraint.  Otherwise we let
              -- the constraints bubble up to be solved from the outer
              -- context, or be defaulted when we reach the top-level.
              -- See Note [Overview of implicit CallStacks] in GHC.Tc.Types.Evidence
            -> Nothing

            | isIPClass cls
            -> Just pred -- See Note [Inheriting implicit parameters]

            | not mentions_qtvs
            -> Nothing   -- Don't quantify over predicates that don't
                         -- mention any of the quantified type variables

            | is_nested
            -> Just pred

            -- From here on, we are thinking about top-level defns only

            | pred_tvs `subVarSet` (qtvs `unionVarSet` mono_tvs0)
              -- See Note [Do not quantify over constraints that determine a variable]
            -> Just pred

            | otherwise
            -> Nothing

          EqPred eq_rel ty1 ty2
            | mentions_qtvs
            , quantify_equality eq_rel ty1 ty2
            , Just (cls, tys) <- boxEqPred eq_rel ty1 ty2
              -- boxEqPred: See Note [Lift equality constraints when quantifying]
            -> Just (mkClassPred cls tys)
            | otherwise
            -> Nothing

          IrredPred {} | mentions_qtvs -> Just pred
                       | otherwise     -> Nothing

          ForAllPred {} -> Nothing

    -- See Note [Quantifying over equality constraints]
    quantify_equality NomEq  ty1 ty2 = quant_fun ty1 || quant_fun ty2
    quantify_equality ReprEq _   _   = True

    quant_fun ty
      = case tcSplitTyConApp_maybe ty of
          Just (tc, tys) | isTypeFamilyTyCon tc
                         -> tyCoVarsOfTypes tys `intersectsVarSet` qtvs
          _ -> False

------------------
growThetaTyVars :: ThetaType -> TyCoVarSet -> TyCoVarSet
-- See Note [growThetaTyVars vs closeWrtFunDeps]
growThetaTyVars theta tcvs
  | null theta = tcvs
  | otherwise  = transCloVarSet mk_next seed_tcvs
  where
    seed_tcvs = tcvs `unionVarSet` tyCoVarsOfTypes ips
    (ips, non_ips) = partition isIPLikePred theta
                         -- See Note [Inheriting implicit parameters]

    mk_next :: VarSet -> VarSet -- Maps current set to newly-grown ones
    mk_next so_far = foldr (grow_one so_far) emptyVarSet non_ips
    grow_one so_far pred tcvs
       | pred_tcvs `intersectsVarSet` so_far = tcvs `unionVarSet` pred_tcvs
       | otherwise                           = tcvs
       where
         pred_tcvs = tyCoVarsOfType pred


{- Note [Promote monomorphic tyvars]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Promote any type variables that are free in the environment.  Eg
   f :: forall qtvs. bound_theta => zonked_tau
The free vars of f's type become free in the envt, and hence will show
up whenever 'f' is called.  They may currently at rhs_tclvl, but they
had better be unifiable at the outer_tclvl!  Example: envt mentions
alpha[1]
           tau_ty = beta[2] -> beta[2]
           constraints = alpha ~ [beta]
we don't quantify over beta (since it is fixed by envt)
so we must promote it!  The inferred type is just
  f :: beta -> beta

NB: promoteTyVarSet ignores coercion variables

Note [pickQuantifiablePreds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When pickQuantifiablePreds is called we have decided what type
variables to quantify over, `qtvs`. The only quesion is: which of the
unsolved candidate predicates should we quantify over?  Call them
`picked_theta`.

Note that will leave behind a residual implication
     forall qtvs. picked_theta => unsolved_constraints
For the members of unsolved_constraints that we select for picked_theta
it is easy to solve, by identity.  For the others we just hope that
we can solve them.

So which of the candidates should we pick to quantify over?  In some
situations we distinguish top-level from nested bindings.  The point
about nested binding is that
 (a) the types may mention type variables free in the environment
 (b) all of the call sites are statically visible, reducing the
     worries about "spooky action at a distance".

First, never pick a constraint that doesn't mention any of the quantified
variables `qtvs`.  Picking such a constraint essentially moves the solving of
the constraint from this function definition to call sites.  But because the
constraint mentions no quantified variables, call sites have no advantage
over the definition site. Well, not quite: there could be new constraints
brought into scope by a pattern-match against a constrained (e.g. GADT)
constructor.  Example

      data T a where { T1 :: T1 Bool; ... }

      f :: forall a. a -> T a -> blah
      f x t = let g y = x&&y    -- This needs a~Bool
            in case t of
                  T1 -> g True
                  ....

At g's call site we have `a~Bool`, so we /could/ infer
     g :: forall . (a~Bool) => Bool -> Bool  -- qtvs = {}

This is all very contrived, and probably just postponse type errors to
the call site.  If that's what you want, write a type signature.

Actually, implicit parameters is an exception to the "no quantified vars"
rule (see Note [Inheriting implicit parameters]) so we can't actually
simply test this case first.

Now we consider the different sorts of constraints:

* For ClassPred constraints:

  * Never pick a CallStack constraint.
    See Note [Overview of implicit CallStacks]

  * Always pick an implicit-parameter constraint.
    Note [Inheriting implicit parameters]

  * For /top-level/ class constraints see
    Note [Do not quantify over constraints that determine a variable]

* For EqPred constraints see Note [Quantifying over equality constraints]

* For IrredPred constraints, we allow anything that mentions the quantified
  type variables.

* A ForAllPred should not appear: the candidates come from approximateWC.

Notice that we do /not/ consult -XFlexibleContexts here.  For example,
we allow `pickQuantifiablePreds` to quantify over a constraint like
`Num [a]`; then if we don't have `-XFlexibleContexts` we'll get an
error from `checkValidType` but (critically) it includes the helpful
suggestion of adding `-XFlexibleContexts`.  See #10608, #10351.

Note [Lift equality constraints when quantifying]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We can't quantify over a constraint (t1 ~# t2) because that isn't a
predicate type; see Note [Types for coercions, predicates, and evidence]
in GHC.Core.TyCo.Rep.

So we have to 'lift' it to (t1 ~ t2).  Similarly (~R#) must be lifted
to Coercible.

This tiresome lifting is the reason that pick_me (in
pickQuantifiablePreds) returns a Maybe rather than a Bool.

Note [Inheriting implicit parameters]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this:

        f x = (x::Int) + ?y

where f is *not* a top-level binding.
From the RHS of f we'll get the constraint (?y::Int).
There are two types we might infer for f:

        f :: Int -> Int

(so we get ?y from the context of f's definition), or

        f :: (?y::Int) => Int -> Int

At first you might think the first was better, because then
?y behaves like a free variable of the definition, rather than
having to be passed at each call site.  But of course, the WHOLE
IDEA is that ?y should be passed at each call site (that's what
dynamic binding means) so we'd better infer the second.

BOTTOM LINE: when *inferring types* you must quantify over implicit
parameters, *even if* they don't mention the bound type variables.
Reason: because implicit parameters, uniquely, have local instance
declarations. See pickQuantifiablePreds.

Note [Quantifying over equality constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Should we quantify over an equality constraint (s ~ t)
in pickQuantifiablePreds?

* It is always /sound/ to quantify over a constraint -- those
  quantified constraints will need to be proved at each call site.

* We definitely don't want to quantify over (Maybe a ~ Bool), to get
     f :: forall a. (Maybe a ~ Bool) => blah
  That simply postpones a type error from the function definition site to
  its call site.  Fortunately we have already filtered out insoluble
  constraints: see `definite_error` in `simplifyInfer`.

* What about (a ~ T alpha b), where we are about to quantify alpha, `a` and
  `b` are in-scope skolems, and `T` is a data type.  It's pretty unlikely
  that this will be soluble at a call site, so we don't quantify over it.

* What about `(F beta ~ Int)` where we are going to quantify `beta`?
  Should we quantify over the (F beta ~ Int), to get
     f :: forall b. (F b ~ Int) => blah
  Aha!  Perhaps yes, because at the call site we will instantiate `b`, and
  perhaps we have `instance F Bool = Int`. So we *do* quantify over a
  type-family equality where the arguments mention the quantified variables.

This is all a bit ad-hoc.

Note [Do not quantify over constraints that determine a variable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (typecheck/should_compile/tc231), where we're trying to infer
the type of a top-level declaration. We have
  class Zork s a b | a -> b
and the candidate constraint at the end of simplifyInfer is
  [W] Zork alpha[1] (Z [Char]) beta[1]
We definitely want to quantify over `alpha` (which is mentioned in the
tau-type).

But we do *not* want to quantify over `beta`: it is determined by the
functional dependency on Zork: note that the second argument to Zork
in the Wanted is a variable-free `Z [Char]`.  Quantifying over it
would be "Henry Ford polymorphism".  (Presumably we don't have an
instance in scope that tells us what `beta` actually is.)  Instead
we promote `beta[1]` to `beta[0]`, in `decidePromotedTyVars`.

The question here: do we want to quantify over the constraint, to
give the type
   forall a. Zork a (Z [Char]) beta[0] => blah
Definitely not.  Since we're not quantifying over beta, it has been
promoted; and then will be zapped to Any in the final zonk.  So we end
up with a (perhaps exported) type involving
  forall a. Zork a (Z [Char]) Any => blah
No no no:

  Key principle: we never want to show the programmer
                 a type with `Any` in it.

What we really want (to catch the Zork example) is this:

   Quantify over the constraint only if all its free variables are
   (a) quantified, or
   (b) appears in the type of something in the environment (mono_tvs0).

To understand (b) consider

  class C a b where { op :: a -> b -> () }

  mr = 3                      -- mr :: alpha
  f1 x = op x mr              -- f1 :: forall b. b -> (), plus [W] C b alpha
  intify = mr + (4 :: Int)

In `f1` should we quantify over that `(C b alpha)`?  Answer: since `alpha`
is free in the type envt, yes we should.  After all, if we'd typechecked
`intify` first, we'd have set `alpha := Int`, and /then/ we'd certainly
quantify.  The delicate Zork situation applies when beta is completely
unconstrained (not free in the environment) -- except by the fundep.

Another way to put it: let's say `alpha` is in `mono_tvs0`. It must be that
some variable `x` has `alpha` free in its type. If we are at top-level (and we
are, because nested decls don't go through this path all), then `x` must also
be at top-level. And, by induction, `x` will not have Any in its type when all
is said and done. The induction is well-founded because, if `x` is mutually
recursive with the definition at hand, then their constraints get processed
together (or `x` has a type signature, in which case the type doesn't have
`Any`). So the key thing is that we must not introduce a new top-level
unconstrained variable here.

However this regrettably-subtle reasoning is needed only for /top-level/
declarations.  For /nested/ decls we can see all the calls, so we'll
instantiate that quantifed `Zork a (Z [Char]) beta` constraint at call sites,
and either solve it or not (probably not).  We won't be left with a
still-callable function with Any in its type.  So for nested definitions we
don't make this tricky test.

Historical note: we had a different, and more complicated test
before, but it was utterly wrong: #23199.

Note [Quantification and partial signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When choosing type variables to quantify, the basic plan is to
quantify over all type variables that are
 * free in the tau_tvs, and
 * not forced to be monomorphic (mono_tvs),
   for example by being free in the environment.

However, in the case of a partial type signature, we are doing inference
*in the presence of a type signature*. For example:
   f :: _ -> a
   f x = ...
or
   g :: (Eq _a) => _b -> _b
In both cases we use plan InferGen, and hence call simplifyInfer.  But
those 'a' variables are skolems (actually TyVarTvs), and we should be
sure to quantify over them.  This leads to several wrinkles:

* Wrinkle 1.  In the case of a type error
     f :: _ -> Maybe a
     f x = True && x
  The inferred type of 'f' is f :: Bool -> Bool, but there's a
  left-over error of form (Maybe a ~ Bool).  The error-reporting
  machine expects to find a binding site for the skolem 'a', so we
  add it to the quantified tyvars.

* Wrinkle 2.  Consider the partial type signature
     f :: (Eq _) => Int -> Int
     f x = x
  In normal cases that makes sense; e.g.
     g :: Eq _a => _a -> _a
     g x = x
  where the signature makes the type less general than it could
  be. But for 'f' we must therefore quantify over the user-annotated
  constraints, to get
     f :: forall a. Eq a => Int -> Int
  (thereby correctly triggering an ambiguity error later).  If we don't
  we'll end up with a strange open type
     f :: Eq alpha => Int -> Int
  which isn't ambiguous but is still very wrong.

  Bottom line: Try to quantify over any variable free in psig_theta,
  just like the tau-part of the type.

* Wrinkle 3 (#13482). Also consider
    f :: forall a. _ => Int -> Int
    f x = if (undefined :: a) == undefined then x else 0
  Here we get an (Eq a) constraint, but it's not mentioned in the
  psig_theta nor the type of 'f'.  But we still want to quantify
  over 'a' even if the monomorphism restriction is on.

* Wrinkle 4 (#14479)
    foo :: Num a => a -> a
    foo xxx = g xxx
      where
        g :: forall b. Num b => _ -> b
        g y = xxx + y

  In the signature for 'g', we cannot quantify over 'b' because it turns out to
  get unified with 'a', which is free in g's environment.  So we carefully
  refrain from bogusly quantifying, in GHC.Tc.Solver.decidePromotedTyVars.  We
  report the error later, in GHC.Tc.Gen.Bind.chooseInferredQuantifiers.

Note [growThetaTyVars vs closeWrtFunDeps]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC has two functions, growThetaTyVars and closeWrtFunDeps, both with
the same type and similar behavior. This Note outlines the differences
and why we use one or the other.

Both functions take a list of constraints. We will call these the
*candidates*.

closeWrtFunDeps takes a set of "determined" type variables and finds the
closure of that set with respect to the functional dependencies
within the class constraints in the set of candidates. So, if we
have

  class C a b | a -> b
  class D a b   -- no fundep
  candidates = {C (Maybe a) (Either b c), D (Maybe a) (Either d e)}

then closeWrtFunDeps {a} will return the set {a,b,c}.
This is because, if `a` is determined, then `b` and `c` are, too,
by functional dependency. closeWrtFunDeps called with any seed set not including
`a` will just return its argument, as only `a` determines any other
type variable (in this example).

growThetaTyVars operates similarly, but it behaves as if every
constraint has a functional dependency among all its arguments.
So, continuing our example, growThetaTyVars {a} will return
{a,b,c,d,e}. Put another way, growThetaTyVars grows the set of
variables to include all variables that are mentioned in the same
constraint (transitively).

We use closeWrtFunDeps in places where we need to know which variables are
*always* determined by some seed set. This includes
  * when determining the mono-tyvars in decidePromotedTyVars. If `a`
    is going to be monomorphic, we need b and c to be also: they
    are determined by the choice for `a`.
  * when checking instance coverage, in
    GHC.Tc.Instance.FunDeps.checkInstCoverage

On the other hand, we use growThetaTyVars where we need to know
which variables *might* be determined by some seed set. This includes
  * deciding quantification (GHC.Tc.Gen.Bind.chooseInferredQuantifiers
    and decideQuantifiedTyVars
How can `a` determine (say) `d` in the example above without a fundep?
Suppose we have
  instance (b ~ a, c ~ a) => D (Maybe [a]) (Either b c)
Now, if `a` turns out to be a list, it really does determine b and c.
The danger in overdoing quantification is the creation of an ambiguous
type signature, but this is conveniently caught in the validity checker.

Note [Quantification with errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we find that the RHS of the definition has some absolutely-insoluble
constraints (including especially "variable not in scope"), we

* Abandon all attempts to find a context to quantify over,
  and instead make the function fully-polymorphic in whatever
  type we have found

* Return a flag from simplifyInfer, indicating that we found an
  insoluble constraint.  This flag is used to suppress the ambiguity
  check for the inferred type, which may well be bogus, and which
  tends to obscure the real error.  This fix feels a bit clunky,
  but I failed to come up with anything better.

Reasons:
    - Avoid downstream errors
    - Do not perform an ambiguity test on a bogus type, which might well
      fail spuriously, thereby obfuscating the original insoluble error.
      #14000 is an example

I tried an alternative approach: simply failM, after emitting the
residual implication constraint; the exception will be caught in
GHC.Tc.Gen.Bind.tcPolyBinds, which gives all the binders in the group the type
(forall a. a).  But that didn't work with -fdefer-type-errors, because
the recovery from failM emits no code at all, so there is no function
to run!   But -fdefer-type-errors aspires to produce a runnable program.

Note [Default while Inferring]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Our current plan is that defaulting only happens at simplifyTop and
not simplifyInfer.  This may lead to some insoluble deferred constraints.
Example:

instance D g => C g Int b

constraint inferred = (forall b. 0 => C gamma alpha b) /\ Num alpha
type inferred       = gamma -> gamma

Now, if we try to default (alpha := Int) we will be able to refine the implication to
  (forall b. 0 => C gamma Int b)
which can then be simplified further to
  (forall b. 0 => D gamma)
Finally, we /can/ approximate this implication with (D gamma) and infer the quantified
type:  forall g. D g => g -> g

Instead what will currently happen is that we will get a quantified type
(forall g. g -> g) and an implication:
       forall g. 0 => (forall b. 0 => C g alpha b) /\ Num alpha

Which, even if the simplifyTop defaults (alpha := Int) we will still be left with an
unsolvable implication:
       forall g. 0 => (forall b. 0 => D g)

The concrete example would be:
       h :: C g a s => g -> a -> ST s a
       f (x::gamma) = (\_ -> x) (runST (h x (undefined::alpha)) + 1)

But it is quite tedious to do defaulting and resolve the implication constraints, and
we have not observed code breaking because of the lack of defaulting in inference, so
we don't do it for now.



Note [Minimize by Superclasses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we quantify over a constraint, in simplifyInfer we need to
quantify over a constraint that is minimal in some sense: For
instance, if the final wanted constraint is (Eq alpha, Ord alpha),
we'd like to quantify over Ord alpha, because we can just get Eq alpha
from superclass selection from Ord alpha. This minimization is what
mkMinimalBySCs does. Then, simplifyInfer uses the minimal constraint
to check the original wanted.


Note [Avoid unnecessary constraint simplification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -------- NB NB NB (Jun 12) -------------
    This note not longer applies; see the notes with #4361.
    But I'm leaving it in here so we remember the issue.)
    ----------------------------------------
When inferring the type of a let-binding, with simplifyInfer,
try to avoid unnecessarily simplifying class constraints.
Doing so aids sharing, but it also helps with delicate
situations like

   instance C t => C [t] where ..

   f :: C [t] => ....
   f x = let g y = ...(constraint C [t])...
         in ...
When inferring a type for 'g', we don't want to apply the
instance decl, because then we can't satisfy (C t).  So we
just notice that g isn't quantified over 't' and partition
the constraints before simplifying.

This only half-works, but then let-generalisation only half-works.

*********************************************************************************
*                                                                                 *
*                                 Main Simplifier                                 *
*                                                                                 *
***********************************************************************************

-}

simplifyWantedsTcM :: [CtEvidence] -> TcM WantedConstraints
-- Solve the specified Wanted constraints
-- Discard the evidence binds
-- Postcondition: fully zonked
simplifyWantedsTcM wanted
  = do { traceTc "simplifyWantedsTcM {" (ppr wanted)
       ; (result, _) <- runTcS (solveWanteds (mkSimpleWC wanted))
       ; result <- TcM.liftZonkM $ TcM.zonkWC result
       ; traceTc "simplifyWantedsTcM }" (ppr result)
       ; return result }

solveWanteds :: WantedConstraints -> TcS WantedConstraints
solveWanteds wc@(WC { wc_errors = errs })
  | isEmptyWC wc  -- Fast path
  = return wc
  | otherwise
  = do { cur_lvl <- TcS.getTcLevel
       ; traceTcS "solveWanteds {" $
         vcat [ text "Level =" <+> ppr cur_lvl
              , ppr wc ]

       ; dflags <- getDynFlags
       ; solved_wc <- simplify_loop 0 (solverIterations dflags) True wc

       ; errs' <- simplifyDelayedErrors errs
       ; let final_wc = solved_wc { wc_errors = errs' }

       ; ev_binds_var <- getTcEvBindsVar
       ; bb <- TcS.getTcEvBindsMap ev_binds_var
       ; traceTcS "solveWanteds }" $
                 vcat [ text "final wc =" <+> ppr final_wc
                      , text "current evbinds  =" <+> ppr (evBindMapBinds bb) ]

       ; return final_wc }

simplify_loop :: Int -> IntWithInf -> Bool
              -> WantedConstraints -> TcS WantedConstraints
-- Do a round of solving, and call maybe_simplify_again to iterate
-- The 'definitely_redo_implications' flags is False if the only reason we
-- are iterating is that we have added some new Wanted superclasses
-- hoping for fundeps to help us; see Note [Superclass iteration]
--
-- Does not affect wc_holes at all; reason: wc_holes never affects anything
-- else, so we do them once, at the end in solveWanteds
simplify_loop n limit definitely_redo_implications
              wc@(WC { wc_simple = simples, wc_impl = implics })
  = do { csTraceTcS $
         text "simplify_loop iteration=" <> int n
         <+> (parens $ hsep [ text "definitely_redo =" <+> ppr definitely_redo_implications <> comma
                            , int (lengthBag simples) <+> text "simples to solve" ])
       ; traceTcS "simplify_loop: wc =" (ppr wc)

       ; (unifs1, wc1) <- reportUnifications $  -- See Note [Superclass iteration]
                          solveSimpleWanteds simples
                -- Any insoluble constraints are in 'simples' and so get rewritten
                -- See Note [Rewrite insolubles] in GHC.Tc.Solver.InertSet

       ; wc2 <- if not definitely_redo_implications  -- See Note [Superclass iteration]
                   && unifs1 == 0                    -- for this conditional
                   && isEmptyBag (wc_impl wc1)
                then return (wc { wc_simple = wc_simple wc1 })  -- Short cut
                else do { implics2 <- solveNestedImplications $
                                      implics `unionBags` (wc_impl wc1)
                        ; return (wc { wc_simple = wc_simple wc1
                                     , wc_impl = implics2 }) }

       ; unif_happened <- resetUnificationFlag
       ; csTraceTcS $ text "unif_happened" <+> ppr unif_happened
         -- Note [The Unification Level Flag] in GHC.Tc.Solver.Monad
       ; maybe_simplify_again (n+1) limit unif_happened wc2 }

maybe_simplify_again :: Int -> IntWithInf -> Bool
                     -> WantedConstraints -> TcS WantedConstraints
maybe_simplify_again n limit unif_happened wc@(WC { wc_simple = simples })
  | n `intGtLimit` limit
  = do { -- Add an error (not a warning) if we blow the limit,
         -- Typically if we blow the limit we are going to report some other error
         -- (an unsolved constraint), and we don't want that error to suppress
         -- the iteration limit warning!
         addErrTcS $ TcRnSimplifierTooManyIterations simples limit wc
       ; return wc }

  | unif_happened
  = simplify_loop n limit True wc

  | superClassesMightHelp wc
  = -- We still have unsolved goals, and apparently no way to solve them,
    -- so try expanding superclasses at this level, both Given and Wanted
    do { pending_given <- getPendingGivenScs
       ; let (pending_wanted, simples1) = getPendingWantedScs simples
       ; if null pending_given && null pending_wanted
           then return wc  -- After all, superclasses did not help
           else
    do { new_given  <- makeSuperClasses pending_given
       ; new_wanted <- makeSuperClasses pending_wanted
       ; solveSimpleGivens new_given -- Add the new Givens to the inert set
       ; traceTcS "maybe_simplify_again" (vcat [ text "pending_given" <+> ppr pending_given
                                               , text "new_given" <+> ppr new_given
                                               , text "pending_wanted" <+> ppr pending_wanted
                                               , text "new_wanted" <+> ppr new_wanted ])
       ; simplify_loop n limit (not (null pending_given)) $
         wc { wc_simple = simples1 `unionBags` listToBag new_wanted } } }
         -- (not (null pending_given)): see Note [Superclass iteration]

  | otherwise
  = return wc

{- Note [Superclass iteration]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this implication constraint
    forall a.
       [W] d: C Int beta
       forall b. blah
where
  class D a b | a -> b
  class D a b => C a b
We will expand d's superclasses, giving [W] D Int beta, in the hope of geting
fundeps to unify beta.  Doing so is usually fruitless (no useful fundeps),
and if so it seems a pity to waste time iterating the implications (forall b. blah)
(If we add new Given superclasses it's a different matter: it's really worth looking
at the implications.)

Hence the definitely_redo_implications flag to simplify_loop.  It's usually
True, but False in the case where the only reason to iterate is new Wanted
superclasses.  In that case we check whether the new Wanteds actually led to
any new unifications, and iterate the implications only if so.
-}

{- Note [Expanding Recursive Superclasses and ExpansionFuel]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the class declaration (T21909)

    class C [a] => C a where
       foo :: a -> Int

and suppose during type inference we obtain an implication constraint:

    forall a. C a => C [[a]]

To solve this implication constraint, we first expand one layer of the superclass
of Given constraints, but not for Wanted constraints.
(See Note [Eagerly expand given superclasses] and Note [Why adding superclasses can help]
in GHC.Tc.Solver.Dict.) We thus get:

    [G] g1 :: C a
    [G] g2 :: C [a]    -- new superclass layer from g1
    [W] w1 :: C [[a]]

Now, we cannot solve `w1` directly from `g1` or `g2` as we may not have
any instances for C. So we expand a layer of superclasses of each Wanteds and Givens
that we haven't expanded yet.
This is done in `maybe_simplify_again`. And we get:

    [G] g1 :: C a
    [G] g2 :: C [a]
    [G] g3 :: C [[a]]    -- new superclass layer from g2, can solve w1
    [W] w1 :: C [[a]]
    [W] w2 :: C [[[a]]]  -- new superclass layer from w1, not solvable

Now, although we can solve `w1` using `g3` (obtained from expanding `g2`),
we have a new wanted constraint `w2` (obtained from expanding `w1`) that cannot be solved.
We thus make another go at solving in `maybe_simplify_again` by expanding more
layers of superclasses. This looping is futile as Givens will never be able to catch up with Wanteds.

Side Note: In principle we don't actually need to /solve/ `w2`, as it is a superclass of `w1`
but we only expand it to expose any functional dependencies (see Note [The superclass story])
But `w2` is a wanted constraint, so we will try to solve it like any other,
even though ultimately we will discard its evidence.

Solution: Simply bound the maximum number of layers of expansion for
Givens and Wanteds, with ExpansionFuel.  Give the Givens more fuel
(say 3 layers) than the Wanteds (say 1 layer). Now the Givens will
win.  The Wanteds don't need much fuel: we are only expanding at all
to expose functional dependencies, and wantedFuel=1 means we will
expand a full recursive layer.  If the superclass hierarchy is
non-recursive (the normal case) one layer is therefore full expansion.

The default value for wantedFuel = Constants.max_WANTEDS_FUEL = 1.
The default value for givenFuel  = Constants.max_GIVENS_FUEL = 3.
Both are configurable via the `-fgivens-fuel` and `-fwanteds-fuel`
compiler flags.

There are two preconditions for the default fuel values:
   (1) default givenFuel >= default wantedsFuel
   (2) default givenFuel < solverIterations

Precondition (1) ensures that we expand givens at least as many times as we expand wanted constraints
preferably givenFuel > wantedsFuel to avoid issues like T21909 while
the precondition (2) ensures that we do not reach the solver iteration limit and fail with a
more meaningful error message (see T19627)

This also applies for quantified constraints; see `-fqcs-fuel` compiler flag and `QCI.qci_pend_sc` field.
-}


solveNestedImplications :: Bag Implication
                        -> TcS (Bag Implication)
-- Precondition: the TcS inerts may contain unsolved simples which have
-- to be converted to givens before we go inside a nested implication.
solveNestedImplications implics
  | isEmptyBag implics
  = return (emptyBag)
  | otherwise
  = do { traceTcS "solveNestedImplications starting {" empty
       ; unsolved_implics <- mapBagM solveImplication implics

       -- ... and we are back in the original TcS inerts
       -- Notice that the original includes the _insoluble_simples so it was safe to ignore
       -- them in the beginning of this function.
       ; traceTcS "solveNestedImplications end }" $
                  vcat [ text "unsolved_implics =" <+> ppr unsolved_implics ]

       ; return (catBagMaybes unsolved_implics) }

solveImplication :: Implication    -- Wanted
                 -> TcS (Maybe Implication) -- Simplified implication (empty or singleton)
-- Precondition: The TcS monad contains an empty worklist and given-only inerts
-- which after trying to solve this implication we must restore to their original value
solveImplication imp@(Implic { ic_tclvl  = tclvl
                             , ic_binds  = ev_binds_var
                             , ic_given  = given_ids
                             , ic_wanted = wanteds
                             , ic_info   = info
                             , ic_status = status })
  | isSolvedStatus status
  = return (Just imp)  -- Do nothing

  | otherwise  -- Even for IC_Insoluble it is worth doing more work
               -- The insoluble stuff might be in one sub-implication
               -- and other unsolved goals in another; and we want to
               -- solve the latter as much as possible
  = do { inerts <- getInertSet
       ; traceTcS "solveImplication {" (ppr imp $$ text "Inerts" <+> ppr inerts)

       -- commented out; see `where` clause below
       -- ; when debugIsOn check_tc_level

         -- Solve the nested constraints
       ; (has_given_eqs, given_insols, residual_wanted)
            <- nestImplicTcS ev_binds_var tclvl $
               do { let loc    = mkGivenLoc tclvl info (ic_env imp)
                        givens = mkGivens loc given_ids
                  ; solveSimpleGivens givens

                  ; residual_wanted <- solveWanteds wanteds

                  ; (has_eqs, given_insols) <- getHasGivenEqs tclvl
                        -- Call getHasGivenEqs /after/ solveWanteds, because
                        -- solveWanteds can augment the givens, via expandSuperClasses,
                        -- to reveal given superclass equalities

                  ; return (has_eqs, given_insols, residual_wanted) }

       ; traceTcS "solveImplication 2"
           (ppr given_insols $$ ppr residual_wanted)
       ; let final_wanted = residual_wanted `addInsols` given_insols
             -- Don't lose track of the insoluble givens,
             -- which signal unreachable code; put them in ic_wanted

       ; res_implic <- setImplicationStatus (imp { ic_given_eqs = has_given_eqs
                                                 , ic_wanted = final_wanted })

       ; evbinds <- TcS.getTcEvBindsMap ev_binds_var
       ; tcvs    <- TcS.getTcEvTyCoVars ev_binds_var
       ; traceTcS "solveImplication end }" $ vcat
             [ text "has_given_eqs =" <+> ppr has_given_eqs
             , text "res_implic =" <+> ppr res_implic
             , text "implication evbinds =" <+> ppr (evBindMapBinds evbinds)
             , text "implication tvcs =" <+> ppr tcvs ]

       ; return res_implic }

    -- TcLevels must be strictly increasing (see (ImplicInv) in
    -- Note [TcLevel invariants] in GHC.Tc.Utils.TcType),
    -- and in fact I think they should always increase one level at a time.

    -- Though sensible, this check causes lots of testsuite failures. It is
    -- remaining commented out for now.
    {-
    check_tc_level = do { cur_lvl <- TcS.getTcLevel
                        ; massertPpr (tclvl == pushTcLevel cur_lvl)
                                     (text "Cur lvl =" <+> ppr cur_lvl $$ text "Imp lvl =" <+> ppr tclvl) }
    -}

----------------------
setImplicationStatus :: Implication -> TcS (Maybe Implication)
-- Finalise the implication returned from solveImplication,
-- setting the ic_status field
-- Precondition: the ic_status field is not already IC_Solved
-- Return Nothing if we can discard the implication altogether
setImplicationStatus implic@(Implic { ic_status     = old_status
                                    , ic_info       = info
                                    , ic_wanted     = wc
                                    , ic_given      = givens })
 | assertPpr (not (isSolvedStatus old_status)) (ppr info) $
   -- Precondition: we only set the status if it is not already solved
   not (isSolvedWC pruned_wc)
 = do { traceTcS "setImplicationStatus(not-all-solved) {" (ppr implic)

      ; implic <- neededEvVars implic

      ; let new_status | insolubleWC pruned_wc = IC_Insoluble
                       | otherwise             = IC_Unsolved
            new_implic = implic { ic_status = new_status
                                , ic_wanted = pruned_wc }

      ; traceTcS "setImplicationStatus(not-all-solved) }" (ppr new_implic)

      ; return $ Just new_implic }

 | otherwise  -- Everything is solved
              -- Set status to IC_Solved,
              -- and compute the dead givens and outer needs
              -- See Note [Tracking redundant constraints]
 = do { traceTcS "setImplicationStatus(all-solved) {" (ppr implic)

      ; implic@(Implic { ic_need_inner = need_inner
                       , ic_need_outer = need_outer }) <- neededEvVars implic

      ; bad_telescope <- checkBadTelescope implic

      ; let warn_givens = findUnnecessaryGivens info need_inner givens

            discard_entire_implication  -- Can we discard the entire implication?
              =  null warn_givens           -- No warning from this implication
              && not bad_telescope
              && isEmptyWC pruned_wc        -- No live children
              && isEmptyVarSet need_outer   -- No needed vars to pass up to parent

            final_status
              | bad_telescope = IC_BadTelescope
              | otherwise     = IC_Solved { ics_dead = warn_givens }
            final_implic = implic { ic_status = final_status
                                  , ic_wanted = pruned_wc }

      ; traceTcS "setImplicationStatus(all-solved) }" $
        vcat [ text "discard:" <+> ppr discard_entire_implication
             , text "new_implic:" <+> ppr final_implic ]

      ; return $ if discard_entire_implication
                 then Nothing
                 else Just final_implic }
 where
   WC { wc_simple = simples, wc_impl = implics, wc_errors = errs } = wc

   pruned_implics = filterBag keep_me implics
   pruned_wc = WC { wc_simple = simples
                  , wc_impl   = pruned_implics
                  , wc_errors = errs }   -- do not prune holes; these should be reported

   keep_me :: Implication -> Bool
   keep_me ic
     | IC_Solved { ics_dead = dead_givens } <- ic_status ic
                          -- Fully solved
     , null dead_givens   -- No redundant givens to report
     , isEmptyBag (wc_impl (ic_wanted ic))
           -- And no children that might have things to report
     = False       -- Tnen we don't need to keep it
     | otherwise
     = True        -- Otherwise, keep it

findUnnecessaryGivens :: SkolemInfoAnon -> VarSet -> [EvVar] -> [EvVar]
findUnnecessaryGivens info need_inner givens
  | not (warnRedundantGivens info)   -- Don't report redundant constraints at all
  = []

  | not (null unused_givens)         -- Some givens are literally unused
  = unused_givens

  | otherwise                       -- All givens are used, but some might
  = redundant_givens                -- still be redundant e.g. (Eq a, Ord a)

  where
    in_instance_decl = case info of { InstSkol {} -> True; _ -> False }
                       -- See Note [Redundant constraints in instance decls]

    unused_givens = filterOut is_used givens

    is_used given =  is_type_error given
                  || given `elemVarSet` need_inner
                  || (in_instance_decl && is_improving (idType given))

    minimal_givens = mkMinimalBySCs evVarPred givens
    is_minimal = (`elemVarSet` mkVarSet minimal_givens)
    redundant_givens
      | in_instance_decl = []
      | otherwise        = filterOut is_minimal givens

    -- See #15232
    is_type_error id = isTopLevelUserTypeError (idType id)

    is_improving pred -- (transSuperClasses p) does not include p
      = any isImprovementPred (pred : transSuperClasses pred)

{- Note [Redundant constraints in instance decls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Instance declarations are special in two ways:

* We don't report unused givens if they can give rise to improvement.
  Example (#10100):
    class Add a b ab | a b -> ab, a ab -> b
    instance Add Zero b b
    instance Add a b ab => Add (Succ a) b (Succ ab)
  The context (Add a b ab) for the instance is clearly unused in terms
  of evidence, since the dictionary has no fields.  But it is still
  needed!  With the context, a wanted constraint
     Add (Succ Zero) beta (Succ Zero)
  we will reduce to (Add Zero beta Zero), and thence we get beta := Zero.
  But without the context we won't find beta := Zero.

  This only matters in instance declarations.

* We don't report givens that are a superclass of another given. E.g.
       class Ord r => UserOfRegs r a where ...
       instance (Ord r, UserOfRegs r CmmReg) => UserOfRegs r CmmExpr where
  The (Ord r) is not redundant, even though it is a superclass of
  (UserOfRegs r CmmReg).  See Note [Recursive superclasses] in GHC.Tc.TyCl.Instance.

  Again this is specific to instance declarations.
-}


checkBadTelescope :: Implication -> TcS Bool
-- True <=> the skolems form a bad telescope
-- See Note [Checking telescopes] in GHC.Tc.Types.Constraint
checkBadTelescope (Implic { ic_info  = info
                          , ic_skols = skols })
  | checkTelescopeSkol info
  = do{ skols <- mapM TcS.zonkTyCoVarKind skols
      ; return (go emptyVarSet (reverse skols))}

  | otherwise
  = return False

  where
    go :: TyVarSet   -- skolems that appear *later* than the current ones
       -> [TcTyVar]  -- ordered skolems, in reverse order
       -> Bool       -- True <=> there is an out-of-order skolem
    go _ [] = False
    go later_skols (one_skol : earlier_skols)
      | tyCoVarsOfType (tyVarKind one_skol) `intersectsVarSet` later_skols
      = True
      | otherwise
      = go (later_skols `extendVarSet` one_skol) earlier_skols

warnRedundantGivens :: SkolemInfoAnon -> Bool
warnRedundantGivens (SigSkol ctxt _ _)
  = case ctxt of
       FunSigCtxt _ rrc -> reportRedundantConstraints rrc
       ExprSigCtxt rrc  -> reportRedundantConstraints rrc
       _                -> False

  -- To think about: do we want to report redundant givens for
  -- pattern synonyms, PatSynSigSkol? c.f #9953, comment:21.
warnRedundantGivens (InstSkol {}) = True
warnRedundantGivens _             = False

neededEvVars :: Implication -> TcS Implication
-- Find all the evidence variables that are "needed",
-- and delete dead evidence bindings
--   See Note [Tracking redundant constraints]
--   See Note [Delete dead Given evidence bindings]
--
--   - Start from initial_seeds (from nested implications)
--
--   - Add free vars of RHS of all Wanted evidence bindings
--     and coercion variables accumulated in tcvs (all Wanted)
--
--   - Generate 'needed', the needed set of EvVars, by doing transitive
--     closure through Given bindings
--     e.g.   Needed {a,b}
--            Given  a = sc_sel a2
--            Then a2 is needed too
--
--   - Prune out all Given bindings that are not needed
--
--   - From the 'needed' set, delete ev_bndrs, the binders of the
--     evidence bindings, to give the final needed variables
--
neededEvVars implic@(Implic { ic_given = givens
                            , ic_binds = ev_binds_var
                            , ic_wanted = WC { wc_impl = implics }
                            , ic_need_inner = old_needs })
 = do { ev_binds <- TcS.getTcEvBindsMap ev_binds_var
      ; tcvs     <- TcS.getTcEvTyCoVars ev_binds_var

      ; let seeds1        = foldr add_implic_seeds old_needs implics
            seeds2        = nonDetStrictFoldEvBindMap add_wanted seeds1 ev_binds
                            -- It's OK to use a non-deterministic fold here
                            -- because add_wanted is commutative
            seeds3        = seeds2 `unionVarSet` tcvs
            need_inner    = findNeededEvVars ev_binds seeds3
            live_ev_binds = filterEvBindMap (needed_ev_bind need_inner) ev_binds
            need_outer    = varSetMinusEvBindMap need_inner live_ev_binds
                            `delVarSetList` givens

      ; TcS.setTcEvBindsMap ev_binds_var live_ev_binds
           -- See Note [Delete dead Given evidence bindings]

      ; traceTcS "neededEvVars" $
        vcat [ text "old_needs:" <+> ppr old_needs
             , text "seeds3:" <+> ppr seeds3
             , text "tcvs:" <+> ppr tcvs
             , text "ev_binds:" <+> ppr ev_binds
             , text "live_ev_binds:" <+> ppr live_ev_binds ]

      ; return (implic { ic_need_inner = need_inner
                       , ic_need_outer = need_outer }) }
 where
   add_implic_seeds (Implic { ic_need_outer = needs }) acc
      = needs `unionVarSet` acc

   needed_ev_bind needed (EvBind { eb_lhs = ev_var
                                 , eb_info = info })
     | EvBindGiven{} <- info = ev_var `elemVarSet` needed
     | otherwise = True   -- Keep all wanted bindings

   add_wanted :: EvBind -> VarSet -> VarSet
   add_wanted (EvBind { eb_info = info, eb_rhs = rhs }) needs
     | EvBindGiven{} <- info = needs  -- Add the rhs vars of the Wanted bindings only
     | otherwise = evVarsOfTerm rhs `unionVarSet` needs

-------------------------------------------------
simplifyDelayedErrors :: Bag DelayedError -> TcS (Bag DelayedError)
-- Simplify any delayed errors: e.g. type and term holes
-- NB: At this point we have finished with all the simple
--     constraints; they are in wc_simple, not in the inert set.
--     So those Wanteds will not rewrite these delayed errors.
--     That's probably no bad thing.
--
--     However if we have [W] alpha ~ Maybe a, [W] alpha ~ Int
--     and _ : alpha, then we'll /unify/ alpha with the first of
--     the Wanteds we get, and thereby report (_ : Maybe a) or
--     (_ : Int) unpredictably, depending on which we happen to see
--     first.  Doesn't matter much; there is a type error anyhow.
--     T17139 is a case in point.
simplifyDelayedErrors = mapBagM simpl_err
  where
    simpl_err :: DelayedError -> TcS DelayedError
    simpl_err (DE_Hole hole) = DE_Hole <$> simpl_hole hole
    simpl_err err@(DE_NotConcrete {}) = return err

    simpl_hole :: Hole -> TcS Hole

     -- See Note [Do not simplify ConstraintHoles]
    simpl_hole h@(Hole { hole_sort = ConstraintHole }) = return h

     -- other wildcards should be simplified for printing
     -- we must do so here, and not in the error-message generation
     -- code, because we have all the givens already set up
    simpl_hole h@(Hole { hole_ty = ty, hole_loc = loc })
      = do { ty' <- rewriteType loc ty
           ; traceTcS "simpl_hole" (ppr ty $$ ppr ty')
           ; return (h { hole_ty = ty' }) }

{- Note [Delete dead Given evidence bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As a result of superclass expansion, we speculatively
generate evidence bindings for Givens. E.g.
   f :: (a ~ b) => a -> b -> Bool
   f x y = ...
We'll have
   [G] d1 :: (a~b)
and we'll speculatively generate the evidence binding
   [G] d2 :: (a ~# b) = sc_sel d

Now d2 is available for solving.  But it may not be needed!  Usually
such dead superclass selections will eventually be dropped as dead
code, but:

 * It won't always be dropped (#13032).  In the case of an
   unlifted-equality superclass like d2 above, we generate
       case heq_sc d1 of d2 -> ...
   and we can't (in general) drop that case expression in case
   d1 is bottom.  So it's technically unsound to have added it
   in the first place.

 * Simply generating all those extra superclasses can generate lots of
   code that has to be zonked, only to be discarded later.  Better not
   to generate it in the first place.

   Moreover, if we simplify this implication more than once
   (e.g. because we can't solve it completely on the first iteration
   of simpl_loop), we'll generate all the same bindings AGAIN!

Easy solution: take advantage of the work we are doing to track dead
(unused) Givens, and use it to prune the Given bindings too.  This is
all done by neededEvVars.

This led to a remarkable 25% overall compiler allocation decrease in
test T12227.

But we don't get to discard all redundant equality superclasses, alas;
see #15205.

Note [Do not simplify ConstraintHoles]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Before printing the inferred value for a type hole (a _ wildcard in
a partial type signature), we simplify it w.r.t. any Givens. This
makes for an easier-to-understand diagnostic for the user.

However, we do not wish to do this for extra-constraint holes. Here is
the example for why (partial-sigs/should_compile/T12844):

  bar :: _ => FooData rngs
  bar = foo

  data FooData rngs

  class Foo xs where foo :: (Head xs ~ '(r,r')) => FooData xs

  type family Head (xs :: [k]) where Head (x ': xs) = x

GHC correctly infers that the extra-constraints wildcard on `bar`
should be (Head rngs ~ '(r, r'), Foo rngs). It then adds this
constraint as a Given on the implication constraint for `bar`. (This
implication is emitted by emitResidualConstraints.) The Hole for the _
is stored within the implication's WantedConstraints.  When
simplifyHoles is called, that constraint is already assumed as a
Given. Simplifying with respect to it turns it into ('(r, r') ~ '(r,
r'), Foo rngs), which is disastrous.

Furthermore, there is no need to simplify here: extra-constraints wildcards
are filled in with the output of the solver, in chooseInferredQuantifiers
(choose_psig_context), so they are already simplified. (Contrast to normal
type holes, which are just bound to a meta-variable.) Avoiding the poor output
is simple: just don't simplify extra-constraints wildcards.

This is the only reason we need to track ConstraintHole separately
from TypeHole in HoleSort.

See also Note [Extra-constraint holes in partial type signatures]
in GHC.Tc.Gen.HsType.

Note [Tracking redundant constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
With Opt_WarnRedundantConstraints, GHC can report which
constraints of a type signature (or instance declaration) are
redundant, and can be omitted.  Here is an overview of how it
works.

This is all tested in typecheck/should_compile/T20602 (among
others).

----- What is a redundant constraint?

* The things that can be redundant are precisely the Given
  constraints of an implication.

* A constraint can be redundant in two different ways:
  a) It is not needed by the Wanted constraints covered by the
     implication E.g.
       f :: Eq a => a -> Bool
       f x = True  -- Equality not used
  b) It is implied by other givens.  E.g.
       f :: (Eq a, Ord a)     => blah   -- Eq a unnecessary
       g :: (Eq a, a~b, Eq b) => blah   -- Either Eq a or Eq b unnecessary

*  To find (a) we need to know which evidence bindings are 'wanted';
   hence the eb_is_given field on an EvBind.

*  To find (b), we use mkMinimalBySCs on the Givens to see if any
   are unnecessary.

----- How tracking works

(RC1) When two Givens are the same, we drop the evidence for the one
  that requires more superclass selectors. This is done
  according to 2(c) of Note [Replacement vs keeping] in GHC.Tc.Solver.InertSet.

(RC2) The ic_need fields of an Implic records in-scope (given) evidence
  variables bound by the context, that were needed to solve this
  implication (so far).  See the declaration of Implication.

(RC3) setImplicationStatus:
  When the constraint solver finishes solving all the wanteds in
  an implication, it sets its status to IC_Solved

  - The ics_dead field, of IC_Solved, records the subset of this
    implication's ic_given that are redundant (not needed).

  - We compute which evidence variables are needed by an implication
    in setImplicationStatus.  A variable is needed if
    a) it is free in the RHS of a Wanted EvBind,
    b) it is free in the RHS of an EvBind whose LHS is needed, or
    c) it is in the ics_need of a nested implication.

  - After computing which variables are needed, we then look at the
    remaining variables for internal redundancies. This is case (b)
    from above. This is also done in setImplicationStatus.
    Note that we only look for case (b) if case (a) shows up empty,
    as exemplified below.

  - We need to be careful not to discard an implication
    prematurely, even one that is fully solved, because we might
    thereby forget which variables it needs, and hence wrongly
    report a constraint as redundant.  But we can discard it once
    its free vars have been incorporated into its parent; or if it
    simply has no free vars. This careful discarding is also
    handled in setImplicationStatus.

(RC4) We do not want to report redundant constraints for implications
  that come from quantified constraints.  Example #23323:
     data T a
     instance Show (T a) where ...  -- No context!
     foo :: forall f c. (forall a. c a => Show (f a)) => Proxy c -> f Int -> Int
     bar = foo @T @Eq

  The call to `foo` gives us
    [W] d : (forall a. Eq a => Show (T a))
  To solve this, GHC.Tc.Solver.Solve.solveForAll makes an implication constraint:
    forall a. Eq a =>  [W] ds : Show (T a)
  and because of the degnerate instance for `Show (T a)`, we don't need the `Eq a`
  constraint.  But we don't want to report it as redundant!

* Examples:

    f, g, h :: (Eq a, Ord a) => a -> Bool
    f x = x == x
    g x = x > x
    h x = x == x && x > x

    All three will discover that they have two [G] Eq a constraints:
    one as given and one extracted from the Ord a constraint. They will
    both discard the latter, as noted above and in
    Note [Replacement vs keeping] in GHC.Tc.Solver.InertSet.

    The body of f uses the [G] Eq a, but not the [G] Ord a. It will
    report a redundant Ord a using the logic for case (a).

    The body of g uses the [G] Ord a, but not the [G] Eq a. It will
    report a redundant Eq a using the logic for case (a).

    The body of h uses both [G] Ord a and [G] Eq a. Case (a) will
    thus come up with nothing redundant. But then, the case (b)
    check will discover that Eq a is redundant and report this.

    If we did case (b) even when case (a) reports something, then
    we would report both constraints as redundant for f, which is
    terrible.

----- Reporting redundant constraints

* GHC.Tc.Errors does the actual warning, in warnRedundantConstraints.

* We don't report redundant givens for *every* implication; only
  for those which reply True to GHC.Tc.Solver.warnRedundantGivens:

   - For example, in a class declaration, the default method *can*
     use the class constraint, but it certainly doesn't *have* to,
     and we don't want to report an error there.  Ditto instance decls.

   - More subtly, in a function definition
       f :: (Ord a, Ord a, Ix a) => a -> a
       f x = rhs
     we do an ambiguity check on the type (which would find that one
     of the Ord a constraints was redundant), and then we check that
     the definition has that type (which might find that both are
     redundant).  We don't want to report the same error twice, so we
     disable it for the ambiguity check.  Hence using two different
     FunSigCtxts, one with the warn-redundant field set True, and the
     other set False in
        - GHC.Tc.Gen.Bind.tcSpecPrag
        - GHC.Tc.Gen.Bind.tcTySig

  This decision is taken in setImplicationStatus, rather than GHC.Tc.Errors
  so that we can discard implication constraints that we don't need.
  So ics_dead consists only of the *reportable* redundant givens.

----- Shortcomings

Shortcoming 1.  Consider

  j :: (Eq a, a ~ b) => a -> Bool
  j x = x == x

  k :: (Eq a, b ~ a) => a -> Bool
  k x = x == x

Currently (Nov 2021), j issues no warning, while k says that b ~ a
is redundant. This is because j uses the a ~ b constraint to rewrite
everything to be in terms of b, while k does none of that. This is
ridiculous, but I (Richard E) don't see a good fix.

Shortcoming 2.  Removing a redundant constraint can cause clients to fail to
compile, by making the function more polymoprhic. Consider (#16154)

  f :: (a ~ Bool) => a -> Int
  f x = 3

  g :: String -> Int
  g s = f (read s)

The constraint in f's signature is redundant; not used to typecheck
`f`.  And yet if you remove it, `g` won't compile, because there'll
be an ambiguous variable in `g`.
-}

type UnificationDone = Bool

noUnification, didUnification :: UnificationDone
noUnification  = False
didUnification = True

-- | Like 'defaultTyVar', but in the TcS monad.
defaultTyVarTcS :: TcTyVar -> TcS UnificationDone
defaultTyVarTcS the_tv
  | isTyVarTyVar the_tv
    -- TyVarTvs should only be unified with a tyvar
    -- never with a type; c.f. GHC.Tc.Utils.TcMType.defaultTyVar
    -- and Note [Inferring kinds for type declarations] in GHC.Tc.TyCl
  = return noUnification
  | isRuntimeRepVar the_tv
  = do { traceTcS "defaultTyVarTcS RuntimeRep" (ppr the_tv)
       ; unifyTyVar the_tv liftedRepTy
       ; return didUnification }
  | isLevityVar the_tv
  = do { traceTcS "defaultTyVarTcS Levity" (ppr the_tv)
       ; unifyTyVar the_tv liftedDataConTy
       ; return didUnification }
  | isMultiplicityVar the_tv
  = do { traceTcS "defaultTyVarTcS Multiplicity" (ppr the_tv)
       ; unifyTyVar the_tv ManyTy
       ; return didUnification }
  | otherwise
  = return noUnification  -- the common case

approximateWC :: Bool   -- See Wrinkle (W3) in Note [ApproximateWC]
              -> WantedConstraints
              -> Cts
-- Second return value is the depleted wc
-- Postcondition: Wanted Cts
-- See Note [ApproximateWC]
-- See Note [floatKindEqualities vs approximateWC]
approximateWC float_past_equalities wc
  = float_wc False emptyVarSet wc
  where
    float_wc :: Bool           -- True <=> there are enclosing equalities
             -> TcTyCoVarSet   -- Enclosing skolem binders
             -> WantedConstraints -> Cts
    float_wc encl_eqs trapping_tvs (WC { wc_simple = simples, wc_impl = implics })
      = filterBag (is_floatable encl_eqs trapping_tvs) simples `unionBags`
        concatMapBag (float_implic encl_eqs trapping_tvs) implics

    float_implic :: Bool -> TcTyCoVarSet -> Implication -> Cts
    float_implic encl_eqs trapping_tvs imp
      = float_wc new_encl_eqs new_trapping_tvs (ic_wanted imp)
      where
        new_trapping_tvs = trapping_tvs `extendVarSetList` ic_skols imp
        new_encl_eqs = encl_eqs || ic_given_eqs imp == MaybeGivenEqs

    is_floatable encl_eqs skol_tvs ct
       | isGivenCt ct                                = False
       | insolubleCt ct                              = False
       | tyCoVarsOfCt ct `intersectsVarSet` skol_tvs = False
       | otherwise
       = case classifyPredType (ctPred ct) of
           EqPred {}     -> float_past_equalities || not encl_eqs
                                  -- See Wrinkle (W1)
           ClassPred {}  -> True  -- See Wrinkle (W2)
           IrredPred {}  -> True  -- ..both in Note [ApproximateWC]
           ForAllPred {} -> False

{- Note [ApproximateWC]
~~~~~~~~~~~~~~~~~~~~~~~
approximateWC takes a constraint, typically arising from the RHS of a
let-binding whose type we are *inferring*, and extracts from it some
*simple* constraints that we might plausibly abstract over.  Of course
the top-level simple constraints are plausible, but we also float constraints
out from inside, if they are not captured by skolems.

The same function is used when doing type-class defaulting (see the call
to applyDefaultingRules) to extract constraints that might be defaulted.

Wrinkle (W1)
  When inferring most-general types (in simplifyInfer), we
  do *not* float an equality constraint if the implication binds
  equality constraints, because that defeats the OutsideIn story.
  Consider data T a where TInt :: T Int MkT :: T a

         f TInt = 3::Int

  We get the implication (a ~ Int => res ~ Int), where so far we've decided
     f :: T a -> res
  We don't want to float (res~Int) out because then we'll infer
     f :: T a -> Int
  which is only on of the possible types. (GHC 7.6 accidentally *did*
  float out of such implications, which meant it would happily infer
  non-principal types.)

Wrinkle (W2)
  We do allow /class/ constraints to float, even if
  the implication binds equalities.  This is a subtle point: see #23224.
  In principle, a class constraint might ultimately be satisfiable from
  a constraint bound by an implication (see #19106 for an example of this
  kind), but it's extremely obscure and I was unable to construct a
  concrete example.  In any case, in super-subtle cases where this might
  make a difference, you would be much better advised to simply write a
  type signature.

  I included IrredPred here too, for good measure.  In general,
  abstracting over more constraints does no harm.

Wrinkle (W3)
  In findDefaultableGroups we are not worried about the
  most-general type; and we /do/ want to float out of equalities
  (#12797).  Hence the boolean flag to approximateWC.

------ Historical note -----------
There used to be a second caveat, driven by #8155

   2. We do not float out an inner constraint that shares a type variable
      (transitively) with one that is trapped by a skolem.  Eg
          forall a.  F a ~ beta, Integral beta
      We don't want to float out (Integral beta).  Doing so would be bad
      when defaulting, because then we'll default beta:=Integer, and that
      makes the error message much worse; we'd get
          Can't solve  F a ~ Integer
      rather than
          Can't solve  Integral (F a)

      Moreover, floating out these "contaminated" constraints doesn't help
      when generalising either. If we generalise over (Integral b), we still
      can't solve the retained implication (forall a. F a ~ b).  Indeed,
      arguably that too would be a harder error to understand.

But this transitive closure stuff gives rise to a complex rule for
when defaulting actually happens, and one that was never documented.
Moreover (#12923), the more complex rule is sometimes NOT what
you want.  So I simply removed the extra code to implement the
contamination stuff.  There was zero effect on the testsuite (not even #8155).
------ End of historical note -----------

Note [DefaultTyVar]
~~~~~~~~~~~~~~~~~~~
defaultTyVar is used on any un-instantiated meta type variables to
default any RuntimeRep variables to LiftedRep.  This is important
to ensure that instance declarations match.  For example consider

     instance Show (a->b)
     foo x = show (\_ -> True)

Then we'll get a constraint (Show (p ->q)) where p has kind (TYPE r),
and that won't match the typeKind (*) in the instance decl.  See tests
tc217 and tc175.

We look only at touchable type variables. No further constraints
are going to affect these type variables, so it's time to do it by
hand.  However we aren't ready to default them fully to () or
whatever, because the type-class defaulting rules have yet to run.

An alternate implementation would be to emit a Wanted constraint setting
the RuntimeRep variable to LiftedRep, but this seems unnecessarily indirect.

Note [Promote _and_ default when inferring]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we are inferring a type, we simplify the constraint, and then use
approximateWC to produce a list of candidate constraints.  Then we MUST

  a) Promote any meta-tyvars that have been floated out by
     approximateWC, to restore invariant (WantedInv) described in
     Note [TcLevel invariants] in GHC.Tc.Utils.TcType.

  b) Default the kind of any meta-tyvars that are not mentioned in
     in the environment.

To see (b), suppose the constraint is (C ((a :: OpenKind) -> Int)), and we
have an instance (C ((x:*) -> Int)).  The instance doesn't match -- but it
should!  If we don't solve the constraint, we'll stupidly quantify over
(C (a->Int)) and, worse, in doing so skolemiseQuantifiedTyVar will quantify over
(b:*) instead of (a:OpenKind), which can lead to disaster; see #7332.
#7641 is a simpler example.

Note [Promoting unification variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we float an equality out of an implication we must "promote" free
unification variables of the equality, in order to maintain Invariant
(WantedInv) from Note [TcLevel invariants] in GHC.Tc.Types.TcType.

This is absolutely necessary. Consider the following example. We start
with two implications and a class with a functional dependency.

    class C x y | x -> y
    instance C [a] [a]

    (I1)      [untch=beta]forall b. 0 => F Int ~ [beta]
    (I2)      [untch=beta]forall c. 0 => F Int ~ [[alpha]] /\ C beta [c]

We float (F Int ~ [beta]) out of I1, and we float (F Int ~ [[alpha]]) out of I2.
They may react to yield that (beta := [alpha]) which can then be pushed inwards
the leftover of I2 to get (C [alpha] [a]) which, using the FunDep, will mean that
(alpha := a). In the end we will have the skolem 'b' escaping in the untouchable
beta! Concrete example is in indexed_types/should_fail/ExtraTcsUntch.hs:

    class C x y | x -> y where
     op :: x -> y -> ()

    instance C [a] [a]

    type family F a :: *

    h :: F Int -> ()
    h = undefined

    data TEx where
      TEx :: a -> TEx

    f (x::beta) =
        let g1 :: forall b. b -> ()
            g1 _ = h [x]
            g2 z = case z of TEx y -> (h [[undefined]], op x [y])
        in (g1 '3', g2 undefined)


*********************************************************************************
*                                                                               *
*                          Defaulting and disambiguation                        *
*                                                                               *
*********************************************************************************

Note [How type-class constraints are defaulted]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Type-class defaulting deals with the situation where we have unsolved
constraints like (Num alpha), where `alpha` is a unification variable.  We want
to pick a default for `alpha`, such as `alpha := Int` to resolve the ambiguity.

Type-class defaulting is guided by the `DefaultEnv`: see Note [Named default declarations]
in GHC.Tc.Gen.Default

The entry point for defaulting the unsolved constraints is `applyDefaultingRules`,
which depends on `disambigGroup`, which in turn depends on workhorse
`disambigProposalSequences`. The latter is also used by defaulting plugins through
`disambigMultiGroup` (see Note [Defaulting plugins] below).

The algorithm works as follows. Let S be the complete set of unsolved
constraints, and initialize Sx to an empty set of constraints. For every type
variable `v` that is free in S:

1. Define Cv = { Ci v | Ci v ∈ S }, the subset of S consisting of all constraints in S of
   form (Ci v), where Ci is a single-parameter type class.  (We do no defaulting for
   multi-parameter type classes.)

2. Define Dv, by extending Cv with the superclasses of every Ci in Cv

3. Define Ev, by filtering Dv to contain only classes with a default declaration.

4. For each Ci in Ev, if Ci has a non-empty default list in the `DefaultEnv`, find the first
   type T in the default list for Ci for which, for every (Ci v) in Cv, the constraint (Ci T)
  is soluble.

5. If there is precisely one type T in the resulting type set, resolve the ambiguity by adding
   a constraint (v~ Ti) constraint to a set Sx; otherwise report a static error.

Note [Defaulting plugins]
~~~~~~~~~~~~~~~~~~~~~~~~~
Defaulting plugins enable extending or overriding the defaulting
behaviour. In `applyDefaultingRules`, before the built-in defaulting
mechanism runs, the loaded defaulting plugins are passed the
`WantedConstraints` and get a chance to propose defaulting assignments
based on them.

Proposals are represented as `[DefaultingProposal]` with each proposal
consisting of a type variable to fill-in, the list of defaulting types to
try in order, and a set of constraints to check at each try. This is
the same representation (albeit in a nicely packaged-up data type) as
the candidates generated by the built-in defaulting mechanism, so the
actual trying of proposals is done by the same `disambigGroup` function.

Wrinkle (DP1): The role of `WantedConstraints`

  Plugins are passed `WantedConstraints` that can perhaps be
  progressed on by defaulting. But a defaulting plugin is not a solver
  plugin, its job is to provide defaulting proposals, i.e. mappings of
  type variable to types. How do plugins know which type variables
  they are supposed to default?

  The `WantedConstraints` passed to the defaulting plugin are zonked
  beforehand to ensure all remaining metavariables are unfilled. Thus,
  the `WantedConstraints` serve a dual purpose: they are both the
  constraints of the given context that can act as hints to the
  defaulting, as well as the containers of the type variables under
  consideration for defaulting.

Wrinkle (DP2): Interactions between defaulting mechanisms

  In the general case, we have multiple defaulting plugins loaded and
  there is also the built-in defaulting mechanism. In this case, we
  have to be careful to keep the `WantedConstraints` passed to the
  plugins up-to-date by zonking between successful defaulting
  rounds. Otherwise, two plugins might come up with a defaulting
  proposal for the same metavariable; if the first one is accepted by
  `disambigGroup` (thus the meta gets filled), the second proposal
  becomes invalid (see #23821 for an example).

-}

tryTypeClassDefaulting :: WantedConstraints -> TcS WantedConstraints
tryTypeClassDefaulting wc
  | isEmptyWC wc || insolubleWC wc -- See Note [Defaulting insolubles]
  = return wc
  | otherwise  -- See Note [When to do type-class defaulting]
  = do { something_happened <- applyDefaultingRules wc
                               -- See Note [Top-level Defaulting Plan]
       ; solveAgainIf something_happened wc }

applyDefaultingRules :: WantedConstraints -> TcS Bool
-- True <=> I did some defaulting, by unifying a meta-tyvar
-- Input WantedConstraints are not necessarily zonked
-- See Note [How type-class constraints are defaulted]

applyDefaultingRules wanteds
  | isEmptyWC wanteds
  = return False
  | otherwise
  = do { (default_env, extended_rules) <- getDefaultInfo
       ; wanteds                       <- TcS.zonkWC wanteds

       ; tcg_env <- TcS.getGblEnv
       ; let plugins = tcg_defaulting_plugins tcg_env
             default_tys = defaultList default_env
             -- see Note [Named default declarations] in GHC.Tc.Gen.Default

       -- Run any defaulting plugins
       -- See Note [Defaulting plugins] for an overview
       ; (wanteds, plugin_defaulted) <- if null plugins then return (wanteds, []) else
           do {
             ; traceTcS "defaultingPlugins {" (ppr wanteds)
             ; (wanteds, defaultedGroups) <- mapAccumLM run_defaulting_plugin wanteds plugins
             ; traceTcS "defaultingPlugins }" (ppr defaultedGroups)
             ; return (wanteds, defaultedGroups)
             }

       ; let groups = findDefaultableGroups (default_tys, extended_rules) wanteds

       ; traceTcS "applyDefaultingRules {" $
                  vcat [ text "wanteds =" <+> ppr wanteds
                       , text "groups  =" <+> ppr groups
                       , text "info    =" <+> ppr (default_tys, extended_rules) ]

       ; something_happeneds <- mapM (disambigGroup wanteds default_tys) groups

       ; traceTcS "applyDefaultingRules }" (ppr something_happeneds)

       ; return $ or something_happeneds || or plugin_defaulted }

    where
      run_defaulting_plugin wanteds p
          = do { groups <- runTcPluginTcS (p wanteds)
               ; defaultedGroups <-
                    filterM (\g -> disambigMultiGroup
                                   wanteds
                                   (deProposalCts g)
                                   (ProposalSequence (Proposal <$> deProposals g)))
                    groups
               ; traceTcS "defaultingPlugin " $ ppr defaultedGroups
               ; case defaultedGroups of
                 [] -> return (wanteds, False)
                 _  -> do
                     -- If a defaulting plugin solves any tyvars, some of the wanteds
                     -- will have filled-in metavars by now (see wrinkle DP2 of
                     -- Note [Defaulting plugins]). So we re-zonk to make sure later
                     -- defaulting doesn't try to solve the same metavars.
                     wanteds' <- TcS.zonkWC wanteds
                     return (wanteds', True) }

findDefaultableGroups
    :: ( [ClassDefaults]
       , Bool )            -- extended default rules
    -> WantedConstraints   -- Unsolved
    -> [(TyVar, [Ct])]
findDefaultableGroups (default_tys, extended_defaults) wanteds
  | null default_tys
  = []
  | otherwise
  = [ (tv, map fstOf3 group)
    | group'@((_,_,tv) :| _) <- unary_groups
    , let group = toList group'
    , defaultable_tyvar tv
    , defaultable_classes (map (classTyCon . sndOf3) group) ]
  where
    simples                = approximateWC True wanteds
    (unaries, non_unaries) = partitionWith find_unary (bagToList simples)
    unary_groups           = equivClasses cmp_tv unaries

    unary_groups :: [NonEmpty (Ct, Class, TcTyVar)] -- (C tv) constraints
    unaries      :: [(Ct, Class, TcTyVar)]          -- (C tv) constraints
    non_unaries  :: [Ct]                            -- and *other* constraints

    -- Finds unary type-class constraints
    -- But take account of polykinded classes like Typeable,
    -- which may look like (Typeable * (a:*))   (#8931)
    -- step (1) in Note [How type-class constraints are defaulted]
    find_unary :: Ct -> Either (Ct, Class, TyVar) Ct
    find_unary cc
        | Just (cls,tys)   <- getClassPredTys_maybe (ctPred cc)
        , [ty] <- filterOutInvisibleTypes (classTyCon cls) tys
              -- Ignore invisible arguments for this purpose
        , Just tv <- getTyVar_maybe ty
        , isMetaTyVar tv  -- We might have runtime-skolems in GHCi, and
                          -- we definitely don't want to try to assign to those!
        = Left (cc, cls, tv)
    find_unary cc = Right cc  -- Non unary or non dictionary

    bad_tvs :: TcTyCoVarSet  -- TyVars mentioned by non-unaries
    bad_tvs = mapUnionVarSet tyCoVarsOfCt non_unaries

    cmp_tv (_,_,tv1) (_,_,tv2) = tv1 `compare` tv2

    defaultable_tyvar :: TcTyVar -> Bool
    defaultable_tyvar tv
        = let b1 = isTyConableTyVar tv  -- Note [Avoiding spurious errors]
              b2 = not (tv `elemVarSet` bad_tvs)
          in b1 && (b2 || extended_defaults) -- Note [Multi-parameter defaults]

    -- Determines if any of the given type class constructors is in default_tys
    -- step (3) in Note [How type-class constraints are defaulted]
    defaultable_classes :: [TyCon] -> Bool
    defaultable_classes clss = not . null . intersect clss $ map cd_class default_tys

------------------------------

-- | 'Proposal's to be tried in sequence until the first one that succeeds
newtype ProposalSequence = ProposalSequence{getProposalSequence :: [Proposal]}

-- | An atomic set of proposed type assignments to try applying all at once
newtype Proposal = Proposal [(TcTyVar, Type)]

instance Outputable ProposalSequence where
  ppr (ProposalSequence proposals) = ppr proposals
instance Outputable Proposal where
  ppr (Proposal assignments) = ppr assignments

disambigGroup :: WantedConstraints -- ^ Original constraints, for diagnostic purposes
              -> [ClassDefaults]   -- ^ The default classes and types
              -> (TcTyVar, [Ct])   -- ^ All constraints sharing same type variable
              -> TcS Bool   -- True <=> something happened, reflected in ty_binds
disambigGroup orig_wanteds default_ctys (tv, wanteds)
  = disambigProposalSequences orig_wanteds wanteds proposalSequences allConsistent
  where
    proposalSequences = [ ProposalSequence [Proposal [(tv, ty)] | ty <- tys]
                        | ClassDefaults{cd_types = tys} <- defaultses ]
    allConsistent ((_, sub) :| subs) = all (eqSubAt tv sub . snd) subs
    defaultses =
      [ defaults | defaults@ClassDefaults{cd_class = cls} <- default_ctys
                 , any (isDictForClass cls) wanteds ]
    isDictForClass clcon ct = any ((clcon ==) . classTyCon . fst) (getClassPredTys_maybe $ ctPred ct)
    eqSubAt :: TcTyVar -> Subst -> Subst -> Bool
    eqSubAt tvar s1 s2 = or $ liftA2 tcEqType (lookupTyVar s1 tvar) (lookupTyVar s2 tvar)

-- See Note [How type-class constraints are defaulted]
disambigMultiGroup :: WantedConstraints    -- ^ Original constraints, for diagnostic purposes
                   -> [Ct]                 -- ^ check these are solved by defaulting
                   -> ProposalSequence     -- ^ defaulting type assignments to try
                   -> TcS Bool   -- True <=> something happened, reflected in ty_binds
disambigMultiGroup orig_wanteds wanteds proposalSequence
  = disambigProposalSequences orig_wanteds wanteds [proposalSequence] (const True)

disambigProposalSequences :: WantedConstraints   -- ^ Original constraints, for diagnostic purposes
                          -> [Ct]                -- ^ Check these are solved by defaulting
                          -> [ProposalSequence]  -- ^ The sequences of assignment proposals
                          -> (NonEmpty ([TcTyVar], Subst) -> Bool)
                                                 -- ^ Predicate for successful assignments
                          -> TcS Bool   -- True <=> something happened, reflected in ty_binds
disambigProposalSequences orig_wanteds wanteds proposalSequences allConsistent
  = do { traverse_ (traverse_ reportInvalidDefaultedTyVars . getProposalSequence) proposalSequences
       ; fake_ev_binds_var <- TcS.newTcEvBinds
       ; tclvl             <- TcS.getTcLevel
       -- Step (4) in Note [How type-class constraints are defaulted]
       ; successes <- fmap catMaybes $
                      nestImplicTcS fake_ev_binds_var (pushTcLevel tclvl) $
                      mapM firstSuccess proposalSequences
       ; traceTcS "disambigProposalSequences" (vcat [ ppr wanteds
                                                    , ppr proposalSequences
                                                    , ppr successes ])
       -- Step (5) in Note [How type-class constraints are defaulted]
       ; case successes of
           success@(tvs, subst) : rest
             | allConsistent (success :| rest)
             -> do { applyDefaultSubst tvs subst
                   ; let warn tv = mapM_ (warnDefaulting wanteds tv) (lookupTyVar subst tv)
                   ; wrapWarnTcS $ mapM_ warn tvs
                   ; traceTcS "disambigProposalSequences succeeded }" (ppr proposalSequences)
                   ; return True }
           _ ->
             do { traceTcS "disambigProposalSequences failed }" (ppr proposalSequences)
                ; return False } }
  where
    reportInvalidDefaultedTyVars :: Proposal -> TcS ()
    firstSuccess :: ProposalSequence -> TcS (Maybe ([TcTyVar], Subst))
    firstSuccess (ProposalSequence proposals)
      = getFirst <$> foldMapM (fmap First . tryDefaultGroup wanteds) proposals
    reportInvalidDefaultedTyVars proposal@(Proposal assignments)
      = do { let tvs = fst <$> assignments
             ; invalid_tvs <- filterOutM TcS.isUnfilledMetaTyVar tvs
             ; traverse_ (errInvalidDefaultedTyVar orig_wanteds proposal) (nonEmpty invalid_tvs) }

applyDefaultSubst :: [TcTyVar] -> Subst -> TcS ()
applyDefaultSubst tvs subst =
  do { deep_tvs <- filterM TcS.isUnfilledMetaTyVar $ nonDetEltsUniqSet $ closeOverKinds (mkVarSet tvs)
     ; forM_ deep_tvs $ \ tv -> mapM_ (unifyTyVar tv) (lookupVarEnv (getTvSubstEnv subst) tv)
     }

tryDefaultGroup :: [Ct]       -- ^ check these are solved by defaulting
                -> Proposal   -- ^ defaulting type assignments to try
                -> TcS (Maybe ([TcTyVar], Subst))  -- ^ successful substitutions, *not* reflected in ty_binds
tryDefaultGroup wanteds (Proposal assignments)
          | let (tvs, default_tys) = unzip assignments
          , Just subst <- tcMatchTyKis (mkTyVarTys tvs) default_tys
            -- Make sure the kinds match too; hence this call to tcMatchTyKi
            -- E.g. suppose the only constraint was (Typeable k (a::k))
            -- With the addition of polykinded defaulting we also want to reject
            -- ill-kinded defaulting attempts like (Eq []) or (Foldable Int) here.
          = do { lcl_env <- TcS.getLclEnv
               ; tc_lvl <- TcS.getTcLevel
               ; let loc = mkGivenLoc tc_lvl (getSkolemInfo unkSkol) (mkCtLocEnv lcl_env)
               -- Equality constraints are possible due to type defaulting plugins
               ; wanted_evs <- sequence [ newWantedNC loc rewriters pred'
                                        | wanted <- wanteds
                                        , CtWanted { ctev_pred = pred
                                                   , ctev_rewriters = rewriters }
                                            <- return (ctEvidence wanted)
                                        , let pred' = substTy subst pred ]
               ; residual_wc <- solveSimpleWanteds $ listToBag $ map mkNonCanonical wanted_evs
               ; return $ if isEmptyWC residual_wc then Just (tvs, subst) else Nothing }

          | otherwise
          = return Nothing

errInvalidDefaultedTyVar :: WantedConstraints -> Proposal -> NonEmpty TcTyVar -> TcS ()
errInvalidDefaultedTyVar wanteds (Proposal assignments) problematic_tvs
  = failTcS $ TcRnInvalidDefaultedTyVar tidy_wanteds tidy_assignments tidy_problems
  where
    proposal_tvs = concatMap (\(tv, ty) -> tv : tyCoVarsOfTypeList ty) assignments
    tidy_env = tidyFreeTyCoVars emptyTidyEnv $ proposal_tvs ++ NE.toList problematic_tvs
    tidy_wanteds = map (tidyCt tidy_env) $ flattenWC wanteds
    tidy_assignments = [(tidyTyCoVarOcc tidy_env tv, tidyType tidy_env ty) | (tv, ty) <- assignments]
    tidy_problems = fmap (tidyTyCoVarOcc tidy_env) problematic_tvs

    flattenWC :: WantedConstraints -> [Ct]
    flattenWC (WC { wc_simple = cts, wc_impl = impls })
      = ctsElts cts ++ concatMap (flattenWC . ic_wanted) impls

-- In interactive mode, or with -XExtendedDefaultRules,
-- we default Show a to Show () to avoid gratuitous errors on "show []"
isInteractiveClass :: Bool   -- -XOverloadedStrings?
                   -> Class -> Bool
isInteractiveClass ovl_strings cls
    = isNumClass ovl_strings cls || (classKey cls `elem` interactiveClassKeys)

    -- isNumClass adds IsString to the standard numeric classes,
    -- when -XOverloadedStrings is enabled
isNumClass :: Bool   -- -XOverloadedStrings?
           -> Class -> Bool
isNumClass ovl_strings cls
  = isNumericClass cls || (ovl_strings && (cls `hasKey` isStringClassKey))


{-
Note [Avoiding spurious errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When doing the unification for defaulting, we check for skolem
type variables, and simply don't default them.  For example:
   f = (*)      -- Monomorphic
   g :: Num a => a -> a
   g x = f x x
Here, we get a complaint when checking the type signature for g,
that g isn't polymorphic enough; but then we get another one when
dealing with the (Num a) context arising from f's definition;
we try to unify a with Int (to default it), but find that it's
already been unified with the rigid variable from g's type sig.

Note [Multi-parameter defaults]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
With -XExtendedDefaultRules, we default only based on single-variable
constraints, but do not exclude from defaulting any type variables which also
appear in multi-variable constraints. This means that the following will
default properly:

   default (Integer, Double)

   class A b (c :: Symbol) where
      a :: b -> Proxy c

   instance A Integer c where a _ = Proxy

   main = print (a 5 :: Proxy "5")

Note that if we change the above instance ("instance A Integer") to
"instance A Double", we get an error:

   No instance for (A Integer "5")

This is because the first defaulted type (Integer) has successfully satisfied
its single-parameter constraints (in this case Num).
-}

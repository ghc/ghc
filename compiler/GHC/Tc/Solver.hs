{-# LANGUAGE MultiWayIf, RecursiveDo, TupleSections #-}

module GHC.Tc.Solver(
       InferMode(..), simplifyInfer, findInferredDiff,
       growThetaTyVars,
       simplifyAmbiguityCheck,
       simplifyTop, simplifyTopImplic,
       simplifyInteractive,
       solveEqualities,
       pushLevelAndSolveEqualities, pushLevelAndSolveEqualitiesX,
       reportUnsolvedEqualities,
       simplifyWantedsTcM,
       tcCheckGivens,
       tcCheckWanteds,
       tcNormalise,
       approximateWC,    -- Exported for plugins to use

       captureTopConstraints, emitResidualConstraints,

       simplifyTopWanteds,

       promoteTyVarSet, simplifyAndEmitFlatConstraints

  ) where

import GHC.Prelude

import GHC.Tc.Errors
import GHC.Tc.Errors.Types
import GHC.Tc.Types.Evidence
import GHC.Tc.Solver.Solve   ( solveSimpleGivens, solveSimpleWanteds
                             , solveWanteds, simplifyWantedsTcM )
import GHC.Tc.Solver.Default ( tryDefaulting, tryDefaultingForAmbiguityCheck
                             , isInteractiveClass )
import GHC.Tc.Solver.Dict    ( makeSuperClasses )
import GHC.Tc.Solver.Rewrite ( rewriteType )
import GHC.Tc.Utils.Unify
import GHC.Tc.Utils.TcMType as TcM
import GHC.Tc.Utils.Monad   as TcM
import GHC.Tc.Zonk.TcType     as TcM
import GHC.Tc.Solver.InertSet
import GHC.Tc.Solver.Monad  as TcS
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.CtLoc( mkGivenLoc )
import GHC.Tc.Instance.FunDeps
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.TcType

import GHC.Core.Predicate
import GHC.Core.Type
import GHC.Core.Ppr
import GHC.Core.TyCon    ( TyConBinder )

import GHC.Types.Name
import GHC.Types.Id

import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Types.Basic
import GHC.Types.Error

import GHC.Driver.DynFlags( DynFlags, xopt )
import GHC.Driver.Flags( WarningFlag(..) )
import GHC.Utils.Panic
import GHC.Utils.Outputable
import GHC.Utils.Misc( filterOut )

import GHC.Data.Bag

import qualified GHC.LanguageExtensions as LangExt

import Control.Monad
import Data.List          ( partition )
import GHC.Data.Maybe     ( mapMaybe )

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
                         ; TcM.emitImplication implic
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

{- Note [Safe Haskell Overlapping Instances]
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
-}

------------------
simplifyAmbiguityCheck :: Type -> WantedConstraints -> TcM ()
simplifyAmbiguityCheck ty wc
  = do { traceTc "simplifyAmbiguityCheck {" $
         text "type = " <+> ppr ty $$ text "wanted = " <+> ppr wc

       ; (final_wc, _) <- runTcS $ do { wc1 <- solveWanteds wc
                                      ; tryDefaultingForAmbiguityCheck wc1 }

       ; discardResult (reportUnsolved final_wc)

       ; traceTc "End simplifyAmbiguityCheck }" empty }

------------------
simplifyInteractive :: WantedConstraints -> TcM (Bag EvBind)
simplifyInteractive wanteds
  = traceTc "simplifyInteractive" empty >>
    simplifyTop wanteds

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

simplifyInfer :: TopLevelFlag
              -> TcLevel               -- Used when generating the constraints
              -> InferMode
              -> [TcIdSigInst]         -- Any signatures (possibly partial)
              -> [(Name, TcTauType)]   -- Variables to be generalised,
                                       -- and their tau-types
              -> WantedConstraints
              -> TcM ([TcTyVar],    -- Quantify over these type variables
                      [EvVar],      -- ... and these constraints (fully zonked)
                      TcEvBinds,    -- ... binding these evidence variables
                      Bool)         -- True <=> the residual constraints are insoluble

simplifyInfer top_lvl rhs_tclvl infer_mode sigs name_taus wanteds
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
       -- NB: we must gather up all the bindings from doing this solving; hence
       -- (runTcSWithEvBinds ev_binds_var).  And note that since there are
       -- nested implications, calling solveWanteds will side-effect their
       -- evidence bindings, so we can't just revert to the input constraint.
       --
       -- See also Note [Inferring principal types]
       ; ev_binds_var <- TcM.newTcEvBinds
       ; psig_evs     <- newWanteds AnnOrigin psig_theta
       ; wanted_transformed
            <- runTcSWithEvBinds ev_binds_var $
               setTcLevelTcS rhs_tclvl        $
               solveWanteds (mkSimpleWC psig_evs `andWC` wanteds)
               -- setLevelTcS: we do setLevel /inside/ the runTcS, so that
               --              we initialise the InertSet inert_given_eq_lvl as far
               --              out as possible, maximising oppportunities to unify
               -- psig_evs : see Note [Add signature contexts as wanteds]

       -- Find quant_pred_candidates, the predicates that
       -- we'll consider quantifying over
       -- NB1: wanted_transformed does not include anything provable from
       --      the psig_theta; it's just the extra bit
       -- NB2: We do not do any defaulting when inferring a type, this can lead
       --      to less polymorphic types, see Note [Default while Inferring]
       ; wanted_transformed <- TcM.liftZonkM $ TcM.zonkWC wanted_transformed
       ; let definite_error = insolubleWC wanted_transformed
                              -- See Note [Quantification with errors]
             wanted_dq | definite_error = emptyWC
                       | otherwise      = wanted_transformed

       -- Decide what type variables and constraints to quantify
       -- NB: quant_pred_candidates is already fully zonked
       -- NB: bound_theta are constraints we want to quantify over,
       --     including the psig_theta, which we always quantify over
       -- NB: bound_theta are fully zonked
       -- rec {..}: see Note [Keeping SkolemInfo inside a SkolemTv]
       --           in GHC.Tc.Utils.TcType
       ; rec { (qtvs, bound_theta, co_vars) <- decideQuantification
                                                     top_lvl rhs_tclvl infer_mode
                                                     skol_info name_taus partial_sigs
                                                     wanted_dq

             ; bound_theta_vars <- mapM TcM.newEvVar bound_theta

             ; let full_theta = map idType bound_theta_vars
                   skol_info  = InferSkol [ (name, mkPhiTy full_theta ty)
                                          | (name, ty) <- name_taus ]
                 -- mkPhiTy: we don't add the quantified variables here, because
                 -- they are also bound in ic_skols and we want them to be tidied
                 -- uniformly.
       }


       -- Now emit the residual constraint
       ; emitResidualConstraints rhs_tclvl skol_info ev_binds_var
                                 co_vars qtvs bound_theta_vars
                                 wanted_transformed

         -- All done!
       ; traceTc "} simplifyInfer/produced residual implication for quantification" $
         vcat [ text "wanted_dq ="      <+> ppr wanted_dq
              , text "psig_theta ="     <+> ppr psig_theta
              , text "bound_theta ="    <+> pprCoreBinders bound_theta_vars
              , text "qtvs ="           <+> ppr qtvs
              , text "definite_error =" <+> ppr definite_error ]

       ; return ( qtvs, bound_theta_vars, TcEvBinds ev_binds_var, definite_error ) }
         -- NB: bound_theta_vars must be fully zonked
  where
    partial_sigs = filter isPartialSig sigs

--------------------
emitResidualConstraints :: TcLevel -> SkolemInfoAnon -> EvBindsVar
                        -> CoVarSet -> [TcTyVar] -> [EvVar]
                        -> WantedConstraints -> TcM ()
-- Emit the remaining constraints from the RHS.
emitResidualConstraints rhs_tclvl skol_info ev_binds_var
                        co_vars qtvs full_theta_vars wanteds
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
  :: TopLevelFlag
  -> TcLevel
  -> InferMode
  -> SkolemInfoAnon
  -> [(Name, TcTauType)]   -- Variables to be generalised
  -> [TcIdSigInst]         -- Partial type signatures (if any)
  -> WantedConstraints     -- Candidate theta; already zonked
  -> TcM ( [TcTyVar]       -- Quantify over these (skolems)
         , [PredType]      -- and this context (fully zonked)
         , CoVarSet)
-- See Note [Deciding quantification]
decideQuantification top_lvl rhs_tclvl infer_mode skol_info name_taus psigs wanted
  = do { -- Step 1: find the mono_tvs
       ; (candidates, co_vars)
             <- decideAndPromoteTyVars top_lvl rhs_tclvl infer_mode name_taus psigs wanted

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

       -- Take account of partial type signatures
       -- See Note [Constraints in partial type signatures]
       ; let min_psig_theta = mkMinimalBySCs id psig_theta
             min_theta      = pickQuantifiablePreds (mkVarSet qtvs) candidates
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

Note [Generalising top-level bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  class C a b | a -> b where ..
  f x = ...[W] C Int beta[1]...

When generalising `f`, closeWrtFunDeps will promote beta[1] to beta[0].
But we do NOT want to make a top level type
  f :: C Int beta[0] => blah
The danger is that beta[0] is defaulted to Any, and that then appears
in a user error message.  Even if the type `blah` mentions beta[0], /and/
there is a call that fixes beta[0] to (say) Bool, we'll end up with
[W] C Int Bool, which is insoluble.  Why insoluble? If there was an
   instance C Int Bool
then fundeps would have fixed beta:=Bool in the first place.

If the binding of `f` is nested, things are different: we can
definitely see all the calls.

For nested bindings, I think it just doesn't matter. No one cares what this
variable ends up being; it seems silly to halt compilation around it. (Like in
the length [] case.)
-}

decideAndPromoteTyVars :: TopLevelFlag -> TcLevel
                       -> InferMode
                       -> [(Name,TcType)]
                       -> [TcIdSigInst]
                       -> WantedConstraints
                       -> TcM ([PredType], CoVarSet)
-- See Note [decideAndPromoteTyVars]
decideAndPromoteTyVars top_lvl rhs_tclvl infer_mode name_taus psigs wanted
  = do { dflags <- getDynFlags

       -- If possible, we quantify over partial-sig qtvs, so they are
       -- not mono. Need to zonk them because they are meta-tyvar TyVarTvs
       ; (psig_qtvs, psig_theta, tau_tys) <- getSeedTys name_taus psigs

       ; let is_top_level = isTopLevel top_lvl  -- A syntactically top-level binding

             -- Step 1 of Note [decideAndPromoteTyVars]
             -- Get candidate constraints, decide which we can potentially quantify
             -- The `no_quant_tvs` are free in constraints we can't quantify.
             (can_quant_cts, no_quant_tvs) = approximateWCX False wanted
             can_quant = ctsPreds can_quant_cts
             can_quant_tvs = tyCoVarsOfTypes can_quant

             -- Step 2 of Note [decideAndPromoteTyVars]
             -- Apply the monomorphism restriction
             (post_mr_quant, mr_no_quant) = applyMR dflags infer_mode can_quant
             mr_no_quant_tvs              = tyCoVarsOfTypes mr_no_quant

             -- The co_var_tvs are tvs mentioned in the types of covars or
             -- coercion holes. We can't quantify over these covars, so we
             -- must include the variable in their types in the mono_tvs.
             -- E.g.  If we can't quantify over co :: k~Type, then we can't
             --       quantify over k either!  Hence closeOverKinds
             -- Recall that coVarsOfTypes also returns coercion holes
             co_vars    = coVarsOfTypes (mkTyVarTys psig_qtvs ++ psig_theta
                                         ++ tau_tys ++ post_mr_quant)
             co_var_tvs = closeOverKinds co_vars

             -- outer_tvs are mentioned in `wanted`, and belong to some outer level.
             -- We definitely can't quantify over them
             outer_tvs = outerLevelTyVars rhs_tclvl $
                         can_quant_tvs `unionVarSet` no_quant_tvs

             -- Step 3 of Note [decideAndPromoteTyVars], (a-c)
             -- Identify mono_tvs: the type variables that we must not quantify over
             -- At top level we are much less keen to create mono tyvars, to avoid
             -- spooky action at a distance.
             mono_tvs_without_mr
               | is_top_level = outer_tvs    -- See (DP2)
               | otherwise    = outer_tvs                    -- (a)
                                `unionVarSet` no_quant_tvs   -- (b)
                                `unionVarSet` co_var_tvs     -- (c)

             -- Step 3 of Note [decideAndPromoteTyVars], (d)
             mono_tvs_with_mr
               = -- Even at top level, we don't quantify over type variables
                 -- mentioned in constraints that the MR tells us not to quantify
                 -- See Note [decideAndPromoteTyVars] (DP2)
                 mono_tvs_without_mr `unionVarSet` mr_no_quant_tvs

             --------------------------------------------------------------------
             -- Step 4 of Note [decideAndPromoteTyVars]
             -- Use closeWrtFunDeps to find any other variables that are determined by mono_tvs
             add_determined tvs preds = closeWrtFunDeps preds tvs
                                        `delVarSetList` psig_qtvs
                 -- Why delVarSetList psig_qtvs?
                 -- If the user has explicitly asked for quantification, then that
                 -- request "wins" over the MR.
                 --
                 -- What if a psig variable is also free in the environment
                 -- (i.e. says "no" to isQuantifiableTv)? That's OK: explanation
                 -- in Step 2 of Note [Deciding quantification].

             mono_tvs_with_mr_det    = add_determined mono_tvs_with_mr    post_mr_quant
             mono_tvs_without_mr_det = add_determined mono_tvs_without_mr can_quant

             --------------------------------------------------------------------
             -- Step 5 of Note [decideAndPromoteTyVars]
             -- Do not quantify over any constraint mentioning a "newly-mono" tyvar.
             newly_mono_tvs = mono_tvs_with_mr_det `minusVarSet` mono_tvs_with_mr
             final_quant
               | is_top_level = filterOut (predMentions newly_mono_tvs) post_mr_quant
               | otherwise    = post_mr_quant

       --------------------------------------------------------------------
       -- Check if the Monomorphism Restriction has bitten
       ; warn_mr <- woptM Opt_WarnMonomorphism
       ; when (warn_mr && case infer_mode of { ApplyMR -> True; _ -> False}) $
         diagnosticTc (not (mono_tvs_with_mr_det `subVarSet` mono_tvs_without_mr_det)) $
              TcRnMonomorphicBindings (map fst name_taus)
             -- If there is a variable in mono_tvs, but not in mono_tvs_wo_mr
             -- then the MR has "bitten" and reduced polymorphism.

       --------------------------------------------------------------------
       -- Step 6: Promote the mono_tvs: see Note [Promote monomorphic tyvars]
       ; _ <- promoteTyVarSet mono_tvs_with_mr_det

       ; traceTc "decideAndPromoteTyVars" $ vcat
           [ text "rhs_tclvl =" <+> ppr rhs_tclvl
           , text "top =" <+> ppr is_top_level
           , text "infer_mode =" <+> ppr infer_mode
           , text "psigs =" <+> ppr psigs
           , text "psig_qtvs =" <+> ppr psig_qtvs
           , text "outer_tvs =" <+> ppr outer_tvs
           , text "mono_tvs_with_mr =" <+> ppr mono_tvs_with_mr
           , text "mono_tvs_without_mr =" <+> ppr mono_tvs_without_mr
           , text "mono_tvs_with_mr_det =" <+> ppr mono_tvs_with_mr_det
           , text "mono_tvs_without_mr_det =" <+> ppr mono_tvs_without_mr_det
           , text "newly_mono_tvs =" <+> ppr newly_mono_tvs
           , text "can_quant =" <+> ppr can_quant
           , text "post_mr_quant =" <+> ppr post_mr_quant
           , text "no_quant_tvs =" <+> ppr no_quant_tvs
           , text "mr_no_quant =" <+> ppr mr_no_quant
           , text "final_quant =" <+> ppr final_quant
           , text "co_vars =" <+> ppr co_vars ]

       ; return (final_quant, co_vars) }
          -- We return `co_vars` that appear free in the final quantified types
          -- we can't quantify over these, and we must make sure they are in scope

-------------------
applyMR :: DynFlags -> InferMode -> [PredType]
        -> ( [PredType]   -- Quantify over these
           , [PredType] ) -- But not over these
-- Split the candidates into ones we definitely
-- won't quantify, and ones that we might
applyMR _      NoRestrictions  cand = (cand, [])
applyMR _      ApplyMR         cand = ([], cand)
applyMR dflags EagerDefaulting cand = partition not_int_ct cand
  where
    ovl_strings = xopt LangExt.OverloadedStrings dflags

    -- not_int_ct returns True for a constraint we /can/ quantify
    -- For EagerDefaulting, do not quantify over
    -- over any interactive class constraint
    not_int_ct pred
      = case classifyPredType pred of
           ClassPred cls _ -> not (isInteractiveClass ovl_strings cls)
           _               -> True

-------------------
outerLevelTyVars :: TcLevel -> TcTyVarSet -> TcTyVarSet
-- Find just the tyvars that are bound outside rhs_tc_lvl
outerLevelTyVars rhs_tclvl tvs
  = filterVarSet is_outer_tv tvs
  where
    is_outer_tv tcv
     | isTcTyVar tcv  -- Might be a CoVar; change this when gather covars separately
     = rhs_tclvl `strictlyDeeperThan` tcTyVarLevel tcv
     | otherwise
     = False

{- Note [decideAndPromoteTyVars]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We are about to generalise a let-binding at "outer level" N, where we have
typechecked its RHS at "rhs level" N+1.  Each tyvar must be either
  (P) promoted
  (D) defaulted
  (Q) quantified
The function `decideAndPromoteTyVars` figures out (P), the type variables
mentioned in constraints should definitely not be quantified, and promotes them
to the outer level, namely N.

The plan

* Step 1.  Use `approximateWCX` to extract, from the RHS `WantedConstraints`,
  the PredTypes that we might quantify over; and also those that we can't.
  Example: suppose the `wanted` is this:
     (d1:Eq alpha, forall b. (F b ~ a) => (co:t1 ~ t2), (d:Show alpha))
  Then
     can_quant = [Eq alpha, Show alpha]
     no_quant  = (t1 ~ t2)
  We can't quantify over that (t1~t2) because of the enclosing equality (F b ~ a).

  We also choose never to quantify over some forms of equality constraints.
  Both this and the "given-equality" thing are described in
  Note [Quantifying over equality constraints] in GHC.Tc.Types.Constraint.

* Step 2. Further trim can_quant using the Monomorphism Restriction, yielding the
  further `mr_no_quant` predicates that we won't quantify over; plus `post_mr_quant`,
  which we can in principle quantify.

* Step 3. Identify the type variables we definitely won't quantify, because they are:
  a) From an outer level <=N anyway
  b) Mentioned in a constraint we /can't/ quantify.  See Wrinkle (DP1).
  c) Mentioned in the kind of a CoVar; we can't quantify over a CoVar,
     so we must not quantify over a type variable free in its kind
  d) Mentioned in a constraint that the MR says we should not quantify.

  There is a special case for top-level bindings: see Wrinkle (DP2).

* Step 4.  Close wrt functional dependencies and equalities.Example
  Example
           f x y = ...
              where z = x 3
  The body of z tries to unify the type of x (call it alpha[1]) with
  (beta[2] -> gamma[2]). This unification fails because alpha is untouchable, leaving
       [W] alpha[1] ~ (beta[2] -> gamma[2])
  We don't want to quantify over beta or gamma because they are fixed by alpha,
  which is monomorphic. Actual test case:   typecheck/should_compile/tc213

  Another example. Suppose we have
      class C a b | a -> b
  and a constraint ([W] C alpha beta), if we promote alpha we should promote beta.

  See also Note [growThetaTyVars vs closeWrtFunDeps]

* Step 5. Further restrict the quantifiable constraints `post_mr_quant` to ones
  that do not mention a "newly mono" tyvar. The "newly-mono" tyvars are the ones
  not free in the envt, nor forced to be promoted by the MR; but are determined
  (via fundeps) by them. Example:
           class C a b | a -> b
           [W] C Int beta[1],  tau = beta[1]->Int
  We promote beta[1] to beta[0] since it is determined by fundep, but we do not
  want to generate f :: (C Int beta[0]) => beta[0] -> Int Rather, we generate
  f :: beta[0] -> Int, but leave [W] C Int beta[0] in the residual constraints,
  which will probably cause a type error

  See Note [Do not quantify over constraints that determine a variable]

* Step 6: actually promote the type variables we don't want to quantify.
  We must do this: see Note [Promote monomorphic tyvars].

We also add a warning that signals when the MR "bites".

Wrinkles

(DP1) In step 3, why (b)?  Consider the example given in Step 1.  we can't
  quantify over the constraint (t1~t2).  But if we quantify over the /tyvars/ in
  t1 or t2, we may simply make that constraint insoluble (#25266 was an example).

(DP2) In Step 3, for top-level bindings, we do (a,d), but /not/ (b,c). Reason:
  see Note [The top-level Any principle].  At top level we are very reluctant to
  promote type variables.  But for bindings affected by the MR we have no choice
  but to promote.

  An example is in #26004.
      f w e = case e of
        T1 -> let y = not w in False
        T2 -> True
  When generalising `f` we have a constraint
      forall. (a ~ Bool) => alpha ~ Bool
  where our provisional type for `f` is `f :: T alpha -> blah`.
  In a /nested/ setting, we might simply not-generalise `f`, hoping to learn
  about `alpha` from f's call sites (test T5266b is an example).  But at top
  level, to avoid spooky action at a distance.

Note [The top-level Any principle]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Key principles:
  * we never want to show the programmer a type with `Any` in it.
  * avoid "spooky action at a distance" and silent defaulting

Most /top level/ bindings have a type signature, so none of this arises.  But
where a top-level binding lacks a signature, we don't want to infer a type like
    f :: alpha[0] -> Int
and then subsequently default alpha[0]:=Any.  Exposing `Any` to the user is bad
bad bad.  Better to report an error, which is what may well happen if we
quantify over alpha instead.

Moreover,
 * If (elsewhere in this module) we add a call to `f`, say (f True), then
   `f` will get the type `Bool -> Int`
 * If we add /another/ call, say (f 'x'), we will then get a type error.
 * If we have no calls, the final exported type of `f` may get set by
   defaulting, and might not be principal (#26004).

For /nested/ bindings, a monomorphic type like `f :: alpha[0] -> Int` is fine,
because we can see all the call sites of `f`, and they will probably fix
`alpha`.  In contrast, we can't see all of (or perhaps any of) the calls of
top-level (exported) functions, reducing the worries about "spooky action at a
distance".  This also moves in the direction of `MonoLocalBinds`, which we like.

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
Definitely not: see Note [The top-level Any principle]

What we really want (to catch the Zork example) is this:

   Quantify over the constraint only if all its free variables are
   (a) quantified, or
   (b) appears in the type of something in the environment (mono_tvs0).

To understand (b) consider

  class C a b where { op :: a -> b -> () }

  mr = 3                      -- mr :: alpha
  f1 x = op x mr              -- f1 :: forall b. b -> (), plus [W] C b alpha
  intify = mr + (4 :: Int)

In `f1` should we quantify over that `(C b alpha)`?  Answer: since `alpha` is
free in the type envt, yes we should.  After all, if we'd typechecked `intify`
first, we'd have set `alpha := Int`, and /then/ we'd certainly quantify.  The
delicate Zork situation applies when beta is completely unconstrained (not free
in the environment) -- except by the fundep.  Hence `newly_mono`.

Another way to put it: let's say `alpha` is in `outer_tvs`. It must be that
some variable `x` has `alpha` free in its type. If we are at top-level (and we
are, because nested decls don't go through this path all), then `x` must also
be at top-level. And, by induction, `x` will not have Any in its type when all
is said and done. The induction is well-founded because, if `x` is mutually
recursive with the definition at hand, then their constraints get processed
together (or `x` has a type signature, in which case the type doesn't have
`Any`). So the key thing is that we must not introduce a new top-level
unconstrained variable here.

However this regrettably-subtle reasoning is needed only for /top-level/
declarations.  For /nested/ decls we can see all the calls, so we'll instantiate
that quantifed `Zork a (Z [Char]) beta` constraint at call sites, and either
solve it or not (probably not).  We won't be left with a still-callable function
with Any in its type.  So for nested definitions we don't make this tricky test.

Historical note: we had a different, and more complicated test before, but it
was utterly wrong: #23199.

Note [Promote monomorphic tyvars]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

Note [Defaulting during simplifyInfer]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

-}

-------------------
defaultTyVarsAndSimplify :: TcLevel
                         -> [PredType]          -- Assumed zonked
                         -> TcM [PredType]      -- Guaranteed zonked
-- Default any tyvar free in the constraints;
-- and re-simplify in case the defaulting allows further simplification
-- See Note [Defaulting during simplifyInfer]
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
   :: SkolemInfoAnon
   -> [(Name,TcType)]   -- Annotated theta and (name,tau) pairs
   -> [TcIdSigInst]     -- Partial signatures
   -> [PredType]        -- Candidates, zonked
   -> TcM [TyVar]
-- Fix what tyvars we are going to quantify over, and quantify them
decideQuantifiedTyVars skol_info_anon name_taus psigs candidates
  = do {     -- Why psig_tys? We try to quantify over everything free in here
             -- See Note [Quantification and partial signatures]
             --     Wrinkles 2 and 3
         (psig_qtvs, psig_theta, tau_tys) <- getSeedTys name_taus psigs

       ; let psig_tys = mkTyVarTys psig_qtvs ++ psig_theta
             seed_tvs = tyCoVarsOfTypes (psig_tys ++ tau_tys)

               -- "Grow" those seeds to find ones reachable via 'candidates'
             -- See Note [growThetaTyVars vs closeWrtFunDeps]
             grown_tcvs = growThetaTyVars candidates seed_tvs

       -- Now we have to classify them into kind variables and type variables
       -- (sigh) just for the benefit of -XNoPolyKinds; see quantifyTyVars
       --
       -- The psig_tys are first in seed_tys, then candidates, then tau_tvs.
       -- This makes candidateQTyVarsOfTypes produces them in that order, so that the
        -- final qtvs quantifies in the same- order as the partial signatures do (#13524)
       ; dvs <- candidateQTyVarsOfTypes (psig_tys ++ candidates ++ tau_tys)
       ; let dvs_plus = weedOutCandidates (`dVarSetIntersectVarSet` grown_tcvs) dvs

       ; traceTc "decideQuantifiedTyVars" (vcat
           [ text "tau_tys =" <+> ppr tau_tys
           , text "candidates =" <+> ppr candidates
           , text "dvs =" <+> ppr dvs
           , text "tau_tys =" <+> ppr tau_tys
           , text "seed_tvs =" <+> ppr seed_tvs
           , text "grown_tcvs =" <+> ppr grown_tcvs
           , text "dvs =" <+> ppr dvs_plus])

       ; skol_info <- mkSkolemInfo skol_info_anon
       ; quantifyTyVars skol_info DefaultNonStandardTyVars dvs_plus }

------------------
getSeedTys :: [(Name,TcType)]    -- The type of each RHS in the group
           -> [TcIdSigInst]      -- Any partial type signatures
           -> TcM ( [TcTyVar]    -- Zonked partial-sig quantified tyvars
                  , ThetaType    -- Zonked partial signature thetas
                  , [TcType] )   -- Zonked tau-tys from the bindings
getSeedTys name_taus psigs
  = TcM.liftZonkM $
    do { psig_tv_tys <- mapM TcM.zonkTcTyVar [ tv | TISI{ sig_inst_skols = skols } <- psigs
                                                  , (_, Bndr tv _) <- skols ]
       ; psig_theta  <- mapM TcM.zonkTcType [ pred | TISI{ sig_inst_theta = theta } <- psigs
                                                   , pred <- theta ]
       ; tau_tys     <- mapM (TcM.zonkTcType . snd) name_taus
       ; return ( map getTyVar psig_tv_tys
                , psig_theta
                , tau_tys ) }

------------------
predMentions :: TcTyVarSet -> TcPredType -> Bool
predMentions qtvs pred = tyCoVarsOfType pred `intersectsVarSet` qtvs

-- | When inferring types, should we quantify over a given predicate?
-- See Note [pickQuantifiablePreds]
pickQuantifiablePreds
  :: TyVarSet           -- Quantifying over these
  -> TcThetaType        -- Proposed constraints to quantify
  -> TcThetaType        -- A subset that we can actually quantify
-- This function decides whether a particular constraint should be
-- quantified over, given the type variables that are being quantified
pickQuantifiablePreds qtvs theta
  = mkMinimalBySCs id $  -- See Note [Minimize by Superclasses]
    mapMaybe pick_me theta
  where
    pick_me pred
      = case classifyPredType pred of
          ClassPred cls _
            | isIPClass cls
            -> Just pred -- Pick, say, (?x::Int) whether or not it mentions qtvs
                         -- See Note [Inheriting implicit parameters]

          EqPred eq_rel ty1 ty2
            | predMentions qtvs pred
            , Just (cls, tys) <- boxEqPred eq_rel ty1 ty2
              -- boxEqPred: See Note [Lift equality constraints when quantifying]
            -> Just (mkClassPred cls tys)
            | otherwise
            -> Nothing

          _ | predMentions qtvs pred -> Just pred
            | otherwise              -> Nothing

------------------
growThetaTyVars :: ThetaType -> TyCoVarSet -> TyCoVarSet
-- See Note [growThetaTyVars vs closeWrtFunDeps]
growThetaTyVars theta tcvs
  | null theta = tcvs
  | otherwise  = transCloVarSet mk_next seed_tcvs
  where
    seed_tcvs = tcvs `unionVarSet` tyCoVarsOfTypes ips
    (ips, non_ips) = partition couldBeIPLike theta
                         -- See Note [Inheriting implicit parameters]

    mk_next :: VarSet -> VarSet -- Maps current set to newly-grown ones
    mk_next so_far = foldr (grow_one so_far) emptyVarSet non_ips
    grow_one so_far pred tcvs
       | pred_tcvs `intersectsVarSet` so_far = tcvs `unionVarSet` pred_tcvs
       | otherwise                           = tcvs
       where
         pred_tcvs = tyCoVarsOfType pred


{- Note [pickQuantifiablePreds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When pickQuantifiablePreds is called we have decided what type
variables to quantify over, `qtvs`. The only quesion is: which of the
unsolved candidate predicates should we quantify over?  Call them
`picked_theta`.

Note that will leave behind a residual implication
     forall qtvs. picked_theta => unsolved_constraints
For the members of unsolved_constraints that we select for picked_theta
it is easy to solve, by identity.  For the others we just hope that
we can solve them.

So which of the candidates should we pick to quantify over?  It's pretty easy:

* Never pick a constraint that doesn't mention any of the quantified
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

* Implicit parameters is an exception to the "no quantified vars"
  rule (see Note [Inheriting implicit parameters]) so we can't actually
  simply test this case first.

* Finally, we may need to "box" equality predicates: if we want to quantify
  over `a ~# b`, we actually quantify over the boxed version, `a ~ b`.
  See Note [Lift equality constraints when quantifying].

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
-}

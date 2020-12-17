{-# LANGUAGE CPP #-}

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
       tcCheckSatisfiability,
       tcNormalise,

       captureTopConstraints,

       simplifyTopWanteds,

       promoteTyVarSet, simplifyAndEmitFlatConstraints,

       -- For Rules we need these
       solveWanteds, solveWantedsAndDrop,
       approximateWC, runTcSDeriveds
  ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Data.Bag
import GHC.Core.Class ( Class, classKey, classTyCon )
import GHC.Driver.Session
import GHC.Tc.Utils.Instantiate
import GHC.Data.List.SetOps
import GHC.Types.Name
import GHC.Types.Id( idType )
import GHC.Utils.Outputable
import GHC.Builtin.Utils
import GHC.Builtin.Names
import GHC.Tc.Errors
import GHC.Tc.Types.Evidence
import GHC.Tc.Solver.Interact
import GHC.Tc.Solver.Canonical   ( makeSuperClasses, solveCallStack )
import GHC.Tc.Solver.Rewrite     ( rewriteType )
import GHC.Tc.Utils.Unify        ( buildTvImplication )
import GHC.Tc.Utils.TcMType as TcM
import GHC.Tc.Utils.Monad   as TcM
import GHC.Tc.Solver.Monad  as TcS
import GHC.Tc.Types.Constraint
import GHC.Core.Predicate
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.TcType
import GHC.Core.Type
import GHC.Builtin.Types ( liftedRepTy, manyDataConTy )
import GHC.Core.Unify    ( tcMatchTyKi )
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Types.Basic    ( IntWithInf, intGtLimit )
import GHC.Utils.Error    ( emptyMessages )
import qualified GHC.LanguageExtensions as LangExt

import Control.Monad
import Data.Foldable      ( toList )
import Data.List          ( partition )
import Data.List.NonEmpty ( NonEmpty(..) )

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
       ; MASSERT2( isEmptyBag empty_binds, ppr empty_binds )

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
       ; unless (isEmptyCts unsafe_ol) $ do {
           -- grab current error messages and clear, warnAllUnsolved will
           -- update error messages which we'll grab and then restore saved
           -- messages.
           ; errs_var  <- getErrsVar
           ; saved_msg <- TcM.readTcRef errs_var
           ; TcM.writeTcRef errs_var emptyMessages

           ; warnAllUnsolved $ emptyWC { wc_simple = unsafe_ol }

           ; whyUnsafe <- fst <$> TcM.readTcRef errs_var
           ; TcM.writeTcRef errs_var saved_msg
           ; recordUnsafeInfer whyUnsafe
           }
       ; traceTc "reportUnsolved (unsafe overlapping) }" empty

       ; return (evBindMapBinds binds1 `unionBags` binds2) }

pushLevelAndSolveEqualities :: SkolemInfo -> [TcTyVar] -> TcM a -> TcM a
-- Push level, and solve all resulting equalities
-- If there are any unsolved equalities, report them
-- and fail (in the monad)
--
-- Panics if we solve any non-equality constraints.  (In runTCSEqualities
-- we use an error thunk for the evidence bindings.)
pushLevelAndSolveEqualities skol_info skol_tvs thing_inside
  = do { (tclvl, wanted, res) <- pushLevelAndSolveEqualitiesX
                                      "pushLevelAndSolveEqualities" thing_inside
       ; reportUnsolvedEqualities skol_info skol_tvs tclvl wanted
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
  = do { -- Solve and zonk to esablish the
         -- preconditions for floatKindEqualities
         wanted <- runTcSEqualities (solveWanteds wanted)
       ; wanted <- TcM.zonkWC wanted

       ; traceTc "emitFlatConstraints {" (ppr wanted)
       ; case floatKindEqualities wanted of
           Nothing -> do { traceTc "emitFlatConstraints } failing" (ppr wanted)
                         ; emitConstraints wanted -- So they get reported!
                         ; failM }
           Just (simples, holes)
              -> do { _ <- promoteTyVarSet (tyCoVarsOfCts simples)
                    ; traceTc "emitFlatConstraints }" $
                      vcat [ text "simples:" <+> ppr simples
                           , text "holes:  " <+> ppr holes ]
                    ; emitHoles holes -- Holes don't need promotion
                    ; emitSimples simples } }

floatKindEqualities :: WantedConstraints -> Maybe (Bag Ct, Bag Hole)
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
    float_wc :: TcTyCoVarSet -> WantedConstraints -> Maybe (Bag Ct, Bag Hole)
    float_wc trapping_tvs (WC { wc_simple = simples
                              , wc_impl = implics
                              , wc_holes = holes })
      | all is_floatable simples
      = do { (inner_simples, inner_holes)
                <- flatMapBagPairM (float_implic trapping_tvs) implics
           ; return ( simples `unionBags` inner_simples
                    , holes `unionBags` inner_holes) }
      | otherwise
      = Nothing
      where
        is_floatable ct
           | insolubleEqCt ct = False
           | otherwise        = tyCoVarsOfCt ct `disjointVarSet` trapping_tvs

    float_implic :: TcTyCoVarSet -> Implication -> Maybe (Bag Ct, Bag Hole)
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

Another possiblity is that we might have something like
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
--
reportUnsolvedEqualities skol_info skol_tvs tclvl wanted
  | isEmptyWC wanted
  = return ()
  | otherwise
  = checkNoErrs $   -- Fail
    do { implic <- buildTvImplication skol_info skol_tvs tclvl wanted
       ; reportAllUnsolved (mkImplicWC (unitBag implic)) }


-- | Simplify top-level constraints, but without reporting any unsolved
-- constraints nor unsafe overlapping.
simplifyTopWanteds :: WantedConstraints -> TcS WantedConstraints
    -- See Note [Top-level Defaulting Plan]
simplifyTopWanteds wanteds
  = do { wc_first_go <- nestTcS (solveWantedsAndDrop wanteds)
                            -- This is where the main work happens
       ; dflags <- getDynFlags
       ; try_tyvar_defaulting dflags wc_first_go }
  where
    try_tyvar_defaulting :: DynFlags -> WantedConstraints -> TcS WantedConstraints
    try_tyvar_defaulting dflags wc
      | isEmptyWC wc
      = return wc
      | insolubleWC wc
      , gopt Opt_PrintExplicitRuntimeReps dflags -- See Note [Defaulting insolubles]
      = try_class_defaulting wc
      | otherwise
      = do { free_tvs <- TcS.zonkTyCoVarsAndFVList (tyCoVarsOfWCList wc)
           ; let meta_tvs = filter (isTyVar <&&> isMetaTyVar) free_tvs
                   -- zonkTyCoVarsAndFV: the wc_first_go is not yet zonked
                   -- filter isMetaTyVar: we might have runtime-skolems in GHCi,
                   -- and we definitely don't want to try to assign to those!
                   -- The isTyVar is needed to weed out coercion variables

           ; defaulted <- mapM defaultTyVarTcS meta_tvs   -- Has unification side effects
           ; if or defaulted
             then do { wc_residual <- nestTcS (solveWanteds wc)
                            -- See Note [Must simplify after defaulting]
                     ; try_class_defaulting wc_residual }
             else try_class_defaulting wc }     -- No defaulting took place

    try_class_defaulting :: WantedConstraints -> TcS WantedConstraints
    try_class_defaulting wc
      | isEmptyWC wc || insolubleWC wc -- See Note [Defaulting insolubles]
      = return wc
      | otherwise  -- See Note [When to do type-class defaulting]
      = do { something_happened <- applyDefaultingRules wc
                                   -- See Note [Top-level Defaulting Plan]
           ; if something_happened
             then do { wc_residual <- nestTcS (solveWantedsAndDrop wc)
                     ; try_class_defaulting wc_residual }
                  -- See Note [Overview of implicit CallStacks] in GHC.Tc.Types.Evidence
             else try_callstack_defaulting wc }

    try_callstack_defaulting :: WantedConstraints -> TcS WantedConstraints
    try_callstack_defaulting wc
      | isEmptyWC wc
      = return wc
      | otherwise
      = defaultCallStacks wc

-- | Default any remaining @CallStack@ constraints to empty @CallStack@s.
defaultCallStacks :: WantedConstraints -> TcS WantedConstraints
-- See Note [Overview of implicit CallStacks] in GHC.Tc.Types.Evidence
defaultCallStacks wanteds
  = do simples <- handle_simples (wc_simple wanteds)
       mb_implics <- mapBagM handle_implic (wc_impl wanteds)
       return (wanteds { wc_simple = simples
                       , wc_impl = catBagMaybes mb_implics })

  where

  handle_simples simples
    = catBagMaybes <$> mapBagM defaultCallStack simples

  handle_implic :: Implication -> TcS (Maybe Implication)
  -- The Maybe is because solving the CallStack constraint
  -- may well allow us to discard the implication entirely
  handle_implic implic
    | isSolvedStatus (ic_status implic)
    = return (Just implic)
    | otherwise
    = do { wanteds <- setEvBindsTcS (ic_binds implic) $
                      -- defaultCallStack sets a binding, so
                      -- we must set the correct binding group
                      defaultCallStacks (ic_wanted implic)
         ; setImplicationStatus (implic { ic_wanted = wanteds }) }

  defaultCallStack ct
    | ClassPred cls tys <- classifyPredType (ctPred ct)
    , Just {} <- isCallStackPred cls tys
    = do { solveCallStack (ctEvidence ct) EvCsEmpty
         ; return Nothing }

  defaultCallStack ct
    = return (Just ct)


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
Haskell inferrence to infer modules with unsafe overlaps as unsafe.

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

 2) `GHC.Tc.Solver.Interact.matchClassInst` -- This module drives the instance resolution
    / dictionary generation. The return type is `ClsInstResult`, which either
    says no instance matched, or one found, and if it was a safe or unsafe
    overlap.

 3) `GHC.Tc.Solver.Interact.doTopReactDict` -- Takes a dictionary / class constraint and
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

 6) `GHC.Tc.Utils.Monad.recordUnsafeInfer` -- Save the unsafe result and reason in an
      IORef called `tcg_safeInfer`.

 7) `GHC.Driver.Main.tcRnModule'` -- Reads `tcg_safeInfer` after type-checking, calling
    `GHC.Driver.Main.markUnsafeInfer` (passing the reason along) when safe-inferrence
    failed.

Note [No defaulting in the ambiguity check]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When simplifying constraints for the ambiguity check, we use
solveWantedsAndDrop, not simplifyTopWanteds, so that we do no defaulting.
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

Conclusion: we should do RuntimeRep-defaulting on insolubles only when the user does not
want to hear about RuntimeRep stuff -- that is, when -fprint-explicit-runtime-reps
is not set.
-}

------------------
simplifyAmbiguityCheck :: Type -> WantedConstraints -> TcM ()
simplifyAmbiguityCheck ty wanteds
  = do { traceTc "simplifyAmbiguityCheck {" (text "type = " <+> ppr ty $$ text "wanted = " <+> ppr wanteds)
       ; (final_wc, _) <- runTcS $ solveWantedsAndDrop wanteds
             -- NB: no defaulting!  See Note [No defaulting in the ambiguity check]

       ; traceTc "End simplifyAmbiguityCheck }" empty

       -- Normally report all errors; but with -XAllowAmbiguousTypes
       -- report only insoluble ones, since they represent genuinely
       -- inaccessible code
       ; allow_ambiguous <- xoptM LangExt.AllowAmbiguousTypes
       ; traceTc "reportUnsolved(ambig) {" empty
       ; unless (allow_ambiguous && not (insolubleWC final_wc))
                (discardResult (reportUnsolved final_wc))
       ; traceTc "reportUnsolved(ambig) }" empty

       ; return () }

------------------
simplifyInteractive :: WantedConstraints -> TcM (Bag EvBind)
simplifyInteractive wanteds
  = traceTc "simplifyInteractive" empty >>
    simplifyTop wanteds

------------------
simplifyDefault :: ThetaType    -- Wanted; has no type variables in it
                -> TcM ()       -- Succeeds if the constraint is soluble
simplifyDefault theta
  = do { traceTc "simplifyDefault" empty
       ; wanteds  <- newWanteds DefaultOrigin theta
       ; unsolved <- runTcSDeriveds (solveWantedsAndDrop (mkSimpleWC wanteds))
       ; reportAllUnsolved unsolved
       ; return () }

------------------
tcCheckSatisfiability :: InertSet -> Bag EvVar -> TcM (Maybe InertSet)
-- Return (Just new_inerts) if satisfiable, Nothing if definitely contradictory
tcCheckSatisfiability inerts given_ids = do
  (sat, new_inerts) <- runTcSInerts inerts $ do
    traceTcS "checkSatisfiability {" (ppr inerts <+> ppr given_ids)
    lcl_env <- TcS.getLclEnv
    let given_loc = mkGivenLoc topTcLevel UnkSkol lcl_env
    let given_cts = mkGivens given_loc (bagToList given_ids)
    -- See Note [Superclasses and satisfiability]
    solveSimpleGivens given_cts
    insols <- getInertInsols
    insols <- try_harder insols
    traceTcS "checkSatisfiability }" (ppr insols)
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

For stratightforard situations without type functions the try_harder
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

-}

-- | How should we choose which constraints to quantify over?
data InferMode = ApplyMR          -- ^ Apply the monomorphism restriction,
                                  -- never quantifying over any constraints
               | EagerDefaulting  -- ^ See Note [TcRnExprMode] in "GHC.Tc.Module",
                                  -- the :type +d case; this mode refuses
                                  -- to quantify over any defaultable constraint
               | NoRestrictions   -- ^ Quantify over any constraint that
                                  -- satisfies 'GHC.Tc.Utils.TcType.pickQuantifiablePreds'

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
       ; qtkvs <- quantifyTyVars dep_vars
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
       ; wanted_transformed_incl_derivs
            <- setTcLevel rhs_tclvl $
               runTcSWithEvBinds ev_binds_var $
               solveWanteds (mkSimpleWC psig_evs `andWC` wanteds)
               -- psig_evs : see Note [Add signature contexts as givens]

       -- Find quant_pred_candidates, the predicates that
       -- we'll consider quantifying over
       -- NB1: wanted_transformed does not include anything provable from
       --      the psig_theta; it's just the extra bit
       -- NB2: We do not do any defaulting when inferring a type, this can lead
       --      to less polymorphic types, see Note [Default while Inferring]
       ; wanted_transformed_incl_derivs <- TcM.zonkWC wanted_transformed_incl_derivs
       ; let definite_error = insolubleWC wanted_transformed_incl_derivs
                              -- See Note [Quantification with errors]
                              -- NB: must include derived errors in this test,
                              --     hence "incl_derivs"
             wanted_transformed = dropDerivedWC wanted_transformed_incl_derivs
             quant_pred_candidates
               | definite_error = []
               | otherwise      = ctsPreds (approximateWC False wanted_transformed)

       -- Decide what type variables and constraints to quantify
       -- NB: quant_pred_candidates is already fully zonked
       -- NB: bound_theta are constraints we want to quantify over,
       --     including the psig_theta, which we always quantify over
       -- NB: bound_theta are fully zonked
       ; (qtvs, bound_theta, co_vars) <- decideQuantification infer_mode rhs_tclvl
                                                     name_taus partial_sigs
                                                     quant_pred_candidates
       ; bound_theta_vars <- mapM TcM.newEvVar bound_theta

       -- Now emit the residual constraint
       ; emitResidualConstraints rhs_tclvl ev_binds_var
                                 name_taus co_vars qtvs bound_theta_vars
                                 wanted_transformed

         -- All done!
       ; traceTc "} simplifyInfer/produced residual implication for quantification" $
         vcat [ text "quant_pred_candidates =" <+> ppr quant_pred_candidates
              , text "psig_theta =" <+> ppr psig_theta
              , text "bound_theta =" <+> ppr bound_theta
              , text "qtvs ="       <+> ppr qtvs
              , text "definite_error =" <+> ppr definite_error ]

       ; return ( qtvs, bound_theta_vars, TcEvBinds ev_binds_var, definite_error ) }
         -- NB: bound_theta_vars must be fully zonked
  where
    partial_sigs = filter isPartialSig sigs

--------------------
emitResidualConstraints :: TcLevel -> EvBindsVar
                        -> [(Name, TcTauType)]
                        -> VarSet -> [TcTyVar] -> [EvVar]
                        -> WantedConstraints -> TcM ()
-- Emit the remaining constraints from the RHS.
emitResidualConstraints rhs_tclvl ev_binds_var
                        name_taus co_vars qtvs full_theta_vars wanteds
  | isEmptyWC wanteds
  = return ()

  | otherwise
  = do { wanted_simple <- TcM.zonkSimples (wc_simple wanteds)
       ; let (outer_simple, inner_simple) = partitionBag is_mono wanted_simple
             is_mono ct = isWantedCt ct && ctEvId ct `elemVarSet` co_vars
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
    skol_info = InferSkol [ (name, mkSigmaTy [] full_theta ty)
                          | (name, ty) <- name_taus ]
    -- We don't add the quantified variables here, because they are
    -- also bound in ic_skols and we want them to be tidied
    -- uniformly.

--------------------
ctsPreds :: Cts -> [PredType]
ctsPreds cts = [ ctEvPred ev | ct <- bagToList cts
                             , let ev = ctEvidence ct ]

findInferredDiff :: TcThetaType -> TcThetaType -> TcM TcThetaType
findInferredDiff annotated_theta inferred_theta
  = pushTcLevelM_ $
    do { lcl_env   <- TcM.getLclEnv
       ; given_ids <- mapM TcM.newEvVar annotated_theta
       ; wanteds   <- newWanteds AnnOrigin inferred_theta
       ; let given_loc = mkGivenLoc topTcLevel UnkSkol lcl_env
             given_cts = mkGivens given_loc given_ids

       ; residual <- runTcSDeriveds $
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
as extra Wanteds

************************************************************************
*                                                                      *
                Quantification
*                                                                      *
************************************************************************

Note [Deciding quantification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If the monomorphism restriction does not apply, then we quantify as follows:

* Step 1. Take the global tyvars, and "grow" them using the equality
  constraints
     E.g.  if x:alpha is in the environment, and alpha ~ [beta] (which can
          happen because alpha is untouchable here) then do not quantify over
          beta, because alpha fixes beta, and beta is effectively free in
          the environment too

  We also account for the monomorphism restriction; if it applies,
  add the free vars of all the constraints.

  Result is mono_tvs; we will not quantify over these.

* Step 2. Default any non-mono tyvars (i.e ones that are definitely
  not going to become further constrained), and re-simplify the
  candidate constraints.

  Motivation for re-simplification (#7857): imagine we have a
  constraint (C (a->b)), where 'a :: TYPE l1' and 'b :: TYPE l2' are
  not free in the envt, and instance forall (a::*) (b::*). (C a) => C
  (a -> b) The instance doesn't match while l1,l2 are polymorphic, but
  it will match when we default them to LiftedRep.

  This is all very tiresome.

* Step 3: decide which variables to quantify over, as follows:

  - Take the free vars of the tau-type (zonked_tau_tvs) and "grow"
    them using all the constraints.  These are tau_tvs_plus

  - Use quantifyTyVars to quantify over (tau_tvs_plus - mono_tvs), being
    careful to close over kinds, and to skolemise the quantified tyvars.
    (This actually unifies each quantifies meta-tyvar with a fresh skolem.)

  Result is qtvs.

* Step 4: Filter the constraints using pickQuantifiablePreds and the
  qtvs. We have to zonk the constraints first, so they "see" the
  freshly created skolems.

-}

decideQuantification
  :: InferMode
  -> TcLevel
  -> [(Name, TcTauType)]   -- Variables to be generalised
  -> [TcIdSigInst]         -- Partial type signatures (if any)
  -> [PredType]            -- Candidate theta; already zonked
  -> TcM ( [TcTyVar]       -- Quantify over these (skolems)
         , [PredType]      -- and this context (fully zonked)
         , VarSet)
-- See Note [Deciding quantification]
decideQuantification infer_mode rhs_tclvl name_taus psigs candidates
  = do { -- Step 1: find the mono_tvs
       ; (mono_tvs, candidates, co_vars) <- decideMonoTyVars infer_mode
                                              name_taus psigs candidates

       -- Step 2: default any non-mono tyvars, and re-simplify
       -- This step may do some unification, but result candidates is zonked
       ; candidates <- defaultTyVarsAndSimplify rhs_tclvl mono_tvs candidates

       -- Step 3: decide which kind/type variables to quantify over
       ; qtvs <- decideQuantifiedTyVars name_taus psigs candidates

       -- Step 4: choose which of the remaining candidate
       --         predicates to actually quantify over
       -- NB: decideQuantifiedTyVars turned some meta tyvars
       -- into quantified skolems, so we have to zonk again
       ; candidates <- TcM.zonkTcTypes candidates
       ; psig_theta <- TcM.zonkTcTypes (concatMap sig_inst_theta psigs)
       ; let quantifiable_candidates
               = pickQuantifiablePreds (mkVarSet qtvs) candidates

             theta = mkMinimalBySCs id $  -- See Note [Minimize by Superclasses]
                     psig_theta ++ quantifiable_candidates
             -- NB: add psig_theta back in here, even though it's already
             -- part of candidates, because we always want to quantify over
             -- psig_theta, and pickQuantifiableCandidates might have
             -- dropped some e.g. CallStack constraints.  c.f #14658
             --                   equalities (a ~ Bool)
             -- Remember, this is the theta for the residual constraint

       ; traceTc "decideQuantification"
           (vcat [ text "infer_mode:" <+> ppr infer_mode
                 , text "candidates:" <+> ppr candidates
                 , text "psig_theta:" <+> ppr psig_theta
                 , text "mono_tvs:"   <+> ppr mono_tvs
                 , text "co_vars:"    <+> ppr co_vars
                 , text "qtvs:"       <+> ppr qtvs
                 , text "theta:"      <+> ppr theta ])
       ; return (qtvs, theta, co_vars) }

------------------
decideMonoTyVars :: InferMode
                 -> [(Name,TcType)]
                 -> [TcIdSigInst]
                 -> [PredType]
                 -> TcM (TcTyCoVarSet, [PredType], CoVarSet)
-- Decide which tyvars and covars cannot be generalised:
--   (a) Free in the environment
--   (b) Mentioned in a constraint we can't generalise
--   (c) Connected by an equality to (a) or (b)
-- Also return CoVars that appear free in the final quantified types
--   we can't quantify over these, and we must make sure they are in scope
decideMonoTyVars infer_mode name_taus psigs candidates
  = do { (no_quant, maybe_quant) <- pick infer_mode candidates

       -- If possible, we quantify over partial-sig qtvs, so they are
       -- not mono. Need to zonk them because they are meta-tyvar TyVarTvs
       ; psig_qtvs <- mapM zonkTcTyVarToTyVar $ binderVars $
                      concatMap (map snd . sig_inst_skols) psigs

       ; psig_theta <- mapM TcM.zonkTcType $
                       concatMap sig_inst_theta psigs

       ; taus <- mapM (TcM.zonkTcType . snd) name_taus

       ; tc_lvl <- TcM.getTcLevel
       ; let psig_tys = mkTyVarTys psig_qtvs ++ psig_theta

             co_vars = coVarsOfTypes (psig_tys ++ taus)
             co_var_tvs = closeOverKinds co_vars
               -- The co_var_tvs are tvs mentioned in the types of covars or
               -- coercion holes. We can't quantify over these covars, so we
               -- must include the variable in their types in the mono_tvs.
               -- E.g.  If we can't quantify over co :: k~Type, then we can't
               --       quantify over k either!  Hence closeOverKinds

             mono_tvs0 = filterVarSet (not . isQuantifiableTv tc_lvl) $
                         tyCoVarsOfTypes candidates
               -- We need to grab all the non-quantifiable tyvars in the
               -- candidates so that we can grow this set to find other
               -- non-quantifiable tyvars. This can happen with something
               -- like
               --    f x y = ...
               --      where z = x 3
               -- The body of z tries to unify the type of x (call it alpha[1])
               -- with (beta[2] -> gamma[2]). This unification fails because
               -- alpha is untouchable. But we need to know not to quantify over
               -- beta or gamma, because they are in the equality constraint with
               -- alpha. Actual test case: typecheck/should_compile/tc213

             mono_tvs1 = mono_tvs0 `unionVarSet` co_var_tvs

             eq_constraints = filter isEqPrimPred candidates
             mono_tvs2      = growThetaTyVars eq_constraints mono_tvs1

             constrained_tvs = filterVarSet (isQuantifiableTv tc_lvl) $
                               (growThetaTyVars eq_constraints
                                               (tyCoVarsOfTypes no_quant)
                                `minusVarSet` mono_tvs2)
                               `delVarSetList` psig_qtvs
             -- constrained_tvs: the tyvars that we are not going to
             -- quantify solely because of the monomorphism restriction
             --
             -- (`minusVarSet` mono_tvs2`): a type variable is only
             --   "constrained" (so that the MR bites) if it is not
             --   free in the environment (#13785)
             --
             -- (`delVarSetList` psig_qtvs): if the user has explicitly
             --   asked for quantification, then that request "wins"
             --   over the MR.  Note: do /not/ delete psig_qtvs from
             --   mono_tvs1, because mono_tvs1 cannot under any circumstances
             --   be quantified (#14479); see
             --   Note [Quantification and partial signatures], Wrinkle 3, 4

             mono_tvs = mono_tvs2 `unionVarSet` constrained_tvs

           -- Warn about the monomorphism restriction
       ; warn_mono <- woptM Opt_WarnMonomorphism
       ; when (case infer_mode of { ApplyMR -> warn_mono; _ -> False}) $
         warnTc (Reason Opt_WarnMonomorphism)
                (constrained_tvs `intersectsVarSet` tyCoVarsOfTypes taus)
                mr_msg

       ; traceTc "decideMonoTyVars" $ vcat
           [ text "infer_mode =" <+> ppr infer_mode
           , text "mono_tvs0 =" <+> ppr mono_tvs0
           , text "no_quant =" <+> ppr no_quant
           , text "maybe_quant =" <+> ppr maybe_quant
           , text "eq_constraints =" <+> ppr eq_constraints
           , text "mono_tvs =" <+> ppr mono_tvs
           , text "co_vars =" <+> ppr co_vars ]

       ; return (mono_tvs, maybe_quant, co_vars) }
  where
    pick :: InferMode -> [PredType] -> TcM ([PredType], [PredType])
    -- Split the candidates into ones we definitely
    -- won't quantify, and ones that we might
    pick NoRestrictions  cand = return ([], cand)
    pick ApplyMR         cand = return (cand, [])
    pick EagerDefaulting cand = do { os <- xoptM LangExt.OverloadedStrings
                                   ; return (partition (is_int_ct os) cand) }

    -- For EagerDefaulting, do not quantify over
    -- over any interactive class constraint
    is_int_ct ovl_strings pred
      | Just (cls, _) <- getClassPredTys_maybe pred
      = isInteractiveClass ovl_strings cls
      | otherwise
      = False

    pp_bndrs = pprWithCommas (quotes . ppr . fst) name_taus
    mr_msg =
         hang (sep [ text "The Monomorphism Restriction applies to the binding"
                     <> plural name_taus
                   , text "for" <+> pp_bndrs ])
            2 (hsep [ text "Consider giving"
                    , text (if isSingleton name_taus then "it" else "them")
                    , text "a type signature"])

-------------------
defaultTyVarsAndSimplify :: TcLevel
                         -> TyCoVarSet
                         -> [PredType]          -- Assumed zonked
                         -> TcM [PredType]      -- Guaranteed zonked
-- Default any tyvar free in the constraints,
-- and re-simplify in case the defaulting allows further simplification
defaultTyVarsAndSimplify rhs_tclvl mono_tvs candidates
  = do {  -- Promote any tyvars that we cannot generalise
          -- See Note [Promote monomorphic tyvars]
       ; traceTc "decideMonoTyVars: promotion:" (ppr mono_tvs)
       ; any_promoted <- promoteTyVarSet mono_tvs

       -- Default any kind/levity vars
       ; DV {dv_kvs = cand_kvs, dv_tvs = cand_tvs}
                <- candidateQTyVarsOfTypes candidates
                -- any covars should already be handled by
                -- the logic in decideMonoTyVars, which looks at
                -- the constraints generated

       ; poly_kinds  <- xoptM LangExt.PolyKinds
       ; default_kvs <- mapM (default_one poly_kinds True)
                             (dVarSetElems cand_kvs)
       ; default_tvs <- mapM (default_one poly_kinds False)
                             (dVarSetElems (cand_tvs `minusDVarSet` cand_kvs))
       ; let some_default = or default_kvs || or default_tvs

       ; case () of
           _ | some_default -> simplify_cand candidates
             | any_promoted -> mapM TcM.zonkTcType candidates
             | otherwise    -> return candidates
       }
  where
    default_one poly_kinds is_kind_var tv
      | not (isMetaTyVar tv)
      = return False
      | tv `elemVarSet` mono_tvs
      = return False
      | otherwise
      = defaultTyVar (not poly_kinds && is_kind_var) tv

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
   :: [(Name,TcType)]   -- Annotated theta and (name,tau) pairs
   -> [TcIdSigInst]     -- Partial signatures
   -> [PredType]        -- Candidates, zonked
   -> TcM [TyVar]
-- Fix what tyvars we are going to quantify over, and quantify them
decideQuantifiedTyVars name_taus psigs candidates
  = do {     -- Why psig_tys? We try to quantify over everything free in here
             -- See Note [Quantification and partial signatures]
             --     Wrinkles 2 and 3
       ; psig_tv_tys <- mapM TcM.zonkTcTyVar [ tv | sig <- psigs
                                                  , (_,Bndr tv _) <- sig_inst_skols sig ]
       ; psig_theta <- mapM TcM.zonkTcType [ pred | sig <- psigs
                                                  , pred <- sig_inst_theta sig ]
       ; tau_tys  <- mapM (TcM.zonkTcType . snd) name_taus

       ; let -- Try to quantify over variables free in these types
             psig_tys = psig_tv_tys ++ psig_theta
             seed_tys = psig_tys ++ tau_tys

             -- Now "grow" those seeds to find ones reachable via 'candidates'
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

       ; quantifyTyVars dvs_plus }

------------------
growThetaTyVars :: ThetaType -> TyCoVarSet -> TyCoVarSet
-- See Note [Growing the tau-tvs using constraints]
growThetaTyVars theta tcvs
  | null theta = tcvs
  | otherwise  = transCloVarSet mk_next seed_tcvs
  where
    seed_tcvs = tcvs `unionVarSet` tyCoVarsOfTypes ips
    (ips, non_ips) = partition isIPLikePred theta
                         -- See Note [Inheriting implicit parameters] in GHC.Tc.Utils.TcType

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

Note [Quantification and partial signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When choosing type variables to quantify, the basic plan is to
quantify over all type variables that are
 * free in the tau_tvs, and
 * not forced to be monomorphic (mono_tvs),
   for example by being free in the environment.

However, in the case of a partial type signature, be doing inference
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
  left-over error of form (HoleCan (Maybe a ~ Bool)).  The error-reporting
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
  refrain from bogusly quantifying, in GHC.Tc.Solver.decideMonoTyVars.  We
  report the error later, in GHC.Tc.Gen.Bind.chooseInferredQuantifiers.

Note [Growing the tau-tvs using constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(growThetaTyVars insts tvs) is the result of extending the set
    of tyvars, tvs, using all conceivable links from pred

E.g. tvs = {a}, preds = {H [a] b, K (b,Int) c, Eq e}
Then growThetaTyVars preds tvs = {a,b,c}

Notice that
   growThetaTyVars is conservative       if v might be fixed by vs
                                         => v `elem` grow(vs,C)

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

NB that we must include *derived* errors in the check for insolubles.
Example:
    (a::*) ~ Int#
We get an insoluble derived error *~#, and we don't want to discard
it before doing the isInsolubleWC test!  (#8262)

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
-- Discards all Derived stuff in result
-- Postcondition: fully zonked
simplifyWantedsTcM wanted
  = do { traceTc "simplifyWantedsTcM {" (ppr wanted)
       ; (result, _) <- runTcS (solveWantedsAndDrop (mkSimpleWC wanted))
       ; result <- TcM.zonkWC result
       ; traceTc "simplifyWantedsTcM }" (ppr result)
       ; return result }

solveWantedsAndDrop :: WantedConstraints -> TcS WantedConstraints
-- Since solveWanteds returns the residual WantedConstraints,
-- it should always be called within a runTcS or something similar,
-- Result is not zonked
solveWantedsAndDrop wanted
  = do { wc <- solveWanteds wanted
       ; return (dropDerivedWC wc) }

solveWanteds :: WantedConstraints -> TcS WantedConstraints
-- so that the inert set doesn't mindlessly propagate.
-- NB: wc_simples may be wanted /or/ derived now
solveWanteds wc@(WC { wc_holes = holes })
  = do { cur_lvl <- TcS.getTcLevel
       ; traceTcS "solveWanteds {" $
         vcat [ text "Level =" <+> ppr cur_lvl
              , ppr wc ]

       ; dflags <- getDynFlags
       ; solved_wc <- simplify_loop 0 (solverIterations dflags) True wc

       ; holes' <- simplifyHoles holes
       ; let final_wc = solved_wc { wc_holes = holes' }

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
-- are iterating is that we have added some new Derived superclasses (from Wanteds)
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
                -- See Note [Rewrite insolubles] in GHC.Tc.Solver.Monad

       ; wc2 <- if not definitely_redo_implications  -- See Note [Superclass iteration]
                   && unifs1 == 0                    -- for this conditional
                   && isEmptyBag (wc_impl wc1)
                then return (wc { wc_simple = wc_simple wc1 })  -- Short cut
                else do { implics2 <- solveNestedImplications $
                                      implics `unionBags` (wc_impl wc1)
                        ; return (wc { wc_simple = wc_simple wc1
                                     , wc_impl = implics2 }) }

       ; unif_happened <- resetUnificationFlag
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
         addErrTcS (hang (text "solveWanteds: too many iterations"
                   <+> parens (text "limit =" <+> ppr limit))
                2 (vcat [ text "Unsolved:" <+> ppr wc
                        , text "Set limit with -fconstraint-solver-iterations=n; n=0 for no limit"
                  ]))
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
We will expand d's superclasses, giving [D] D Int beta, in the hope of geting
fundeps to unify beta.  Doing so is usually fruitless (no useful fundeps),
and if so it seems a pity to waste time iterating the implications (forall b. blah)
(If we add new Given superclasses it's a different matter: it's really worth looking
at the implications.)

Hence the definitely_redo_implications flag to simplify_loop.  It's usually
True, but False in the case where the only reason to iterate is new Derived
superclasses.  In that case we check whether the new Deriveds actually led to
any new unifications, and iterate the implications only if so.
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
  = do { inerts <- getTcSInerts
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
                        -- solveWanteds, *not* solveWantedsAndDrop, because
                        -- we want to retain derived equalities so we can float
                        -- them out in floatEqualities.

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
                        ; MASSERT2( tclvl == pushTcLevel cur_lvl , text "Cur lvl =" <+> ppr cur_lvl $$ text "Imp lvl =" <+> ppr tclvl ) }
    -}

----------------------
setImplicationStatus :: Implication -> TcS (Maybe Implication)
-- Finalise the implication returned from solveImplication:
--    * Set the ic_status field
--    * Trim the ic_wanted field to remove Derived constraints
-- Precondition: the ic_status field is not already IC_Solved
-- Return Nothing if we can discard the implication altogether
setImplicationStatus implic@(Implic { ic_status     = status
                                    , ic_info       = info
                                    , ic_wanted     = wc
                                    , ic_given      = givens })
 | ASSERT2( not (isSolvedStatus status ), ppr info )
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

      ; let dead_givens | warnRedundantGivens info
                        = filterOut (`elemVarSet` need_inner) givens
                        | otherwise = []   -- None to report

            discard_entire_implication  -- Can we discard the entire implication?
              =  null dead_givens           -- No warning from this implication
              && not bad_telescope
              && isEmptyWC pruned_wc        -- No live children
              && isEmptyVarSet need_outer   -- No needed vars to pass up to parent

            final_status
              | bad_telescope = IC_BadTelescope
              | otherwise     = IC_Solved { ics_dead = dead_givens }
            final_implic = implic { ic_status = final_status
                                  , ic_wanted = pruned_wc }

      ; traceTcS "setImplicationStatus(all-solved) }" $
        vcat [ text "discard:" <+> ppr discard_entire_implication
             , text "new_implic:" <+> ppr final_implic ]

      ; return $ if discard_entire_implication
                 then Nothing
                 else Just final_implic }
 where
   WC { wc_simple = simples, wc_impl = implics, wc_holes = holes } = wc

   pruned_simples = dropDerivedSimples simples
   pruned_implics = filterBag keep_me implics
   pruned_wc = WC { wc_simple = pruned_simples
                  , wc_impl   = pruned_implics
                  , wc_holes  = holes }   -- do not prune holes; these should be reported

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

warnRedundantGivens :: SkolemInfo -> Bool
warnRedundantGivens (SigSkol ctxt _ _)
  = case ctxt of
       FunSigCtxt _ warn_redundant -> warn_redundant
       ExprSigCtxt                 -> True
       _                           -> False

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
                                 , eb_is_given = is_given })
     | is_given  = ev_var `elemVarSet` needed
     | otherwise = True   -- Keep all wanted bindings

   add_wanted :: EvBind -> VarSet -> VarSet
   add_wanted (EvBind { eb_is_given = is_given, eb_rhs = rhs }) needs
     | is_given  = needs  -- Add the rhs vars of the Wanted bindings only
     | otherwise = evVarsOfTerm rhs `unionVarSet` needs

-------------------------------------------------
simplifyHoles :: Bag Hole -> TcS (Bag Hole)
simplifyHoles = mapBagM simpl_hole
  where
    simpl_hole :: Hole -> TcS Hole

     -- See Note [Do not simplify ConstraintHoles]
    simpl_hole h@(Hole { hole_sort = ConstraintHole }) = return h

     -- other wildcards should be simplified for printing
     -- we must do so here, and not in the error-message generation
     -- code, because we have all the givens already set up
    simpl_hole h@(Hole { hole_ty = ty, hole_loc = loc })
      = do { ty' <- rewriteType loc ty
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
   of simpl_looop), we'll generate all the same bindings AGAIN!

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
works:

----- What is a redundant constraint?

* The things that can be redundant are precisely the Given
  constraints of an implication.

* A constraint can be redundant in two different ways:
  a) It is implied by other givens.  E.g.
       f :: (Eq a, Ord a)     => blah   -- Eq a unnecessary
       g :: (Eq a, a~b, Eq b) => blah   -- Either Eq a or Eq b unnecessary
  b) It is not needed by the Wanted constraints covered by the
     implication E.g.
       f :: Eq a => a -> Bool
       f x = True  -- Equality not used

*  To find (a), when we have two Given constraints,
   we must be careful to drop the one that is a naked variable (if poss).
   So if we have
       f :: (Eq a, Ord a) => blah
   then we may find [G] sc_sel (d1::Ord a) :: Eq a
                    [G] d2 :: Eq a
   We want to discard d2 in favour of the superclass selection from
   the Ord dictionary.  This is done by GHC.Tc.Solver.Interact.solveOneFromTheOther
   See Note [Replacement vs keeping].

* To find (b) we need to know which evidence bindings are 'wanted';
  hence the eb_is_given field on an EvBind.

----- How tracking works

* The ic_need fields of an Implic records in-scope (given) evidence
  variables bound by the context, that were needed to solve this
  implication (so far).  See the declaration of Implication.

* When the constraint solver finishes solving all the wanteds in
  an implication, it sets its status to IC_Solved

  - The ics_dead field, of IC_Solved, records the subset of this
    implication's ic_given that are redundant (not needed).

* We compute which evidence variables are needed by an implication
  in setImplicationStatus.  A variable is needed if
    a) it is free in the RHS of a Wanted EvBind,
    b) it is free in the RHS of an EvBind whose LHS is needed,
    c) it is in the ics_need of a nested implication.

* We need to be careful not to discard an implication
  prematurely, even one that is fully solved, because we might
  thereby forget which variables it needs, and hence wrongly
  report a constraint as redundant.  But we can discard it once
  its free vars have been incorporated into its parent; or if it
  simply has no free vars. This careful discarding is also
  handled in setImplicationStatus.

----- Reporting redundant constraints

* GHC.Tc.Errors does the actual warning, in warnRedundantConstraints.

* We don't report redundant givens for *every* implication; only
  for those which reply True to GHC.Tc.Solver.warnRedundantGivens:

   - For example, in a class declaration, the default method *can*
     use the class constraint, but it certainly doesn't *have* to,
     and we don't want to report an error there.

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

Consider (see #9939)
    f2 :: (Eq a, Ord a) => a -> a -> Bool
    -- Ord a redundant, but Eq a is reported
    f2 x y = (x == y)

We report (Eq a) as redundant, whereas actually (Ord a) is.  But it's
really not easy to detect that!

-}

-- | Like 'defaultTyVar', but in the TcS monad.
defaultTyVarTcS :: TcTyVar -> TcS Bool
defaultTyVarTcS the_tv
  | isRuntimeRepVar the_tv
  , not (isTyVarTyVar the_tv)
    -- TyVarTvs should only be unified with a tyvar
    -- never with a type; c.f. GHC.Tc.Utils.TcMType.defaultTyVar
    -- and Note [Inferring kinds for type declarations] in GHC.Tc.TyCl
  = do { traceTcS "defaultTyVarTcS RuntimeRep" (ppr the_tv)
       ; unifyTyVar the_tv liftedRepTy
       ; return True }
  | isMultiplicityVar the_tv
  , not (isTyVarTyVar the_tv)  -- TyVarTvs should only be unified with a tyvar
                             -- never with a type; c.f. TcMType.defaultTyVar
                             -- See Note [Kind generalisation and SigTvs]
  = do { traceTcS "defaultTyVarTcS Multiplicity" (ppr the_tv)
       ; unifyTyVar the_tv manyDataConTy
       ; return True }
  | otherwise
  = return False  -- the common case

approximateWC :: Bool -> WantedConstraints -> Cts
-- Postcondition: Wanted or Derived Cts
-- See Note [ApproximateWC]
-- See Note [floatKindEqualities vs approximateWC]
approximateWC float_past_equalities wc
  = float_wc emptyVarSet wc
  where
    float_wc :: TcTyCoVarSet -> WantedConstraints -> Cts
    float_wc trapping_tvs (WC { wc_simple = simples, wc_impl = implics })
      = filterBag (is_floatable trapping_tvs) simples `unionBags`
        concatMapBag (float_implic trapping_tvs) implics
    float_implic :: TcTyCoVarSet -> Implication -> Cts
    float_implic trapping_tvs imp
      | float_past_equalities || ic_given_eqs imp /= MaybeGivenEqs
      = float_wc new_trapping_tvs (ic_wanted imp)
      | otherwise   -- Take care with equalities
      = emptyCts    -- See (1) under Note [ApproximateWC]
      where
        new_trapping_tvs = trapping_tvs `extendVarSetList` ic_skols imp

    is_floatable skol_tvs ct
       | isGivenCt ct     = False
       | insolubleEqCt ct = False
       | otherwise        = tyCoVarsOfCt ct `disjointVarSet` skol_tvs

{- Note [ApproximateWC]
~~~~~~~~~~~~~~~~~~~~~~~
approximateWC takes a constraint, typically arising from the RHS of a
let-binding whose type we are *inferring*, and extracts from it some
*simple* constraints that we might plausibly abstract over.  Of course
the top-level simple constraints are plausible, but we also float constraints
out from inside, if they are not captured by skolems.

The same function is used when doing type-class defaulting (see the call
to applyDefaultingRules) to extract constraints that might be defaulted.

There is one caveat:

1.  When inferring most-general types (in simplifyInfer), we do *not*
    float anything out if the implication binds equality constraints,
    because that defeats the OutsideIn story.  Consider
       data T a where
         TInt :: T Int
         MkT :: T a

       f TInt = 3::Int

    We get the implication (a ~ Int => res ~ Int), where so far we've decided
      f :: T a -> res
    We don't want to float (res~Int) out because then we'll infer
      f :: T a -> Int
    which is only on of the possible types. (GHC 7.6 accidentally *did*
    float out of such implications, which meant it would happily infer
    non-principal types.)

   HOWEVER (#12797) in findDefaultableGroups we are not worried about
   the most-general type; and we /do/ want to float out of equalities.
   Hence the boolean flag to approximateWC.

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
and that won't match the tcTypeKind (*) in the instance decl.  See tests
tc217 and tc175.

We look only at touchable type variables. No further constraints
are going to affect these type variables, so it's time to do it by
hand.  However we aren't ready to default them fully to () or
whatever, because the type-class defaulting rules have yet to run.

An alternate implementation would be to emit a derived constraint setting
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
-}

applyDefaultingRules :: WantedConstraints -> TcS Bool
-- True <=> I did some defaulting, by unifying a meta-tyvar
-- Input WantedConstraints are not necessarily zonked

applyDefaultingRules wanteds
  | isEmptyWC wanteds
  = return False
  | otherwise
  = do { info@(default_tys, _) <- getDefaultInfo
       ; wanteds               <- TcS.zonkWC wanteds

       ; let groups = findDefaultableGroups info wanteds

       ; traceTcS "applyDefaultingRules {" $
                  vcat [ text "wanteds =" <+> ppr wanteds
                       , text "groups  =" <+> ppr groups
                       , text "info    =" <+> ppr info ]

       ; something_happeneds <- mapM (disambigGroup default_tys) groups

       ; traceTcS "applyDefaultingRules }" (ppr something_happeneds)

       ; return (or something_happeneds) }

findDefaultableGroups
    :: ( [Type]
       , (Bool,Bool) )     -- (Overloaded strings, extended default rules)
    -> WantedConstraints   -- Unsolved (wanted or derived)
    -> [(TyVar, [Ct])]
findDefaultableGroups (default_tys, (ovl_strings, extended_defaults)) wanteds
  | null default_tys
  = []
  | otherwise
  = [ (tv, map fstOf3 group)
    | group'@((_,_,tv) :| _) <- unary_groups
    , let group = toList group'
    , defaultable_tyvar tv
    , defaultable_classes (map sndOf3 group) ]
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
    find_unary :: Ct -> Either (Ct, Class, TyVar) Ct
    find_unary cc
        | Just (cls,tys)   <- getClassPredTys_maybe (ctPred cc)
        , [ty] <- filterOutInvisibleTypes (classTyCon cls) tys
              -- Ignore invisible arguments for this purpose
        , Just tv <- tcGetTyVar_maybe ty
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

    defaultable_classes :: [Class] -> Bool
    defaultable_classes clss
        | extended_defaults = any (isInteractiveClass ovl_strings) clss
        | otherwise         = all is_std_class clss && (any (isNumClass ovl_strings) clss)

    -- is_std_class adds IsString to the standard numeric classes,
    -- when -foverloaded-strings is enabled
    is_std_class cls = isStandardClass cls ||
                       (ovl_strings && (cls `hasKey` isStringClassKey))

------------------------------
disambigGroup :: [Type]            -- The default types
              -> (TcTyVar, [Ct])   -- All classes of the form (C a)
                                   --  sharing same type variable
              -> TcS Bool   -- True <=> something happened, reflected in ty_binds

disambigGroup [] _
  = return False
disambigGroup (default_ty:default_tys) group@(the_tv, wanteds)
  = do { traceTcS "disambigGroup {" (vcat [ ppr default_ty, ppr the_tv, ppr wanteds ])
       ; fake_ev_binds_var <- TcS.newTcEvBinds
       ; tclvl             <- TcS.getTcLevel
       ; success <- nestImplicTcS fake_ev_binds_var (pushTcLevel tclvl) try_group

       ; if success then
             -- Success: record the type variable binding, and return
             do { unifyTyVar the_tv default_ty
                ; wrapWarnTcS $ warnDefaulting wanteds default_ty
                ; traceTcS "disambigGroup succeeded }" (ppr default_ty)
                ; return True }
         else
             -- Failure: try with the next type
             do { traceTcS "disambigGroup failed, will try other default types }"
                           (ppr default_ty)
                ; disambigGroup default_tys group } }
  where
    try_group
      | Just subst <- mb_subst
      = do { lcl_env <- TcS.getLclEnv
           ; tc_lvl <- TcS.getTcLevel
           ; let loc = mkGivenLoc tc_lvl UnkSkol lcl_env
           ; wanted_evs <- mapM (newWantedEvVarNC loc . substTy subst . ctPred)
                                wanteds
           ; fmap isEmptyWC $
             solveSimpleWanteds $ listToBag $
             map mkNonCanonical wanted_evs }

      | otherwise
      = return False

    the_ty   = mkTyVarTy the_tv
    mb_subst = tcMatchTyKi the_ty default_ty
      -- Make sure the kinds match too; hence this call to tcMatchTyKi
      -- E.g. suppose the only constraint was (Typeable k (a::k))
      -- With the addition of polykinded defaulting we also want to reject
      -- ill-kinded defaulting attempts like (Eq []) or (Foldable Int) here.

-- In interactive mode, or with -XExtendedDefaultRules,
-- we default Show a to Show () to avoid graututious errors on "show []"
isInteractiveClass :: Bool   -- -XOverloadedStrings?
                   -> Class -> Bool
isInteractiveClass ovl_strings cls
    = isNumClass ovl_strings cls || (classKey cls `elem` interactiveClassKeys)

    -- isNumClass adds IsString to the standard numeric classes,
    -- when -foverloaded-strings is enabled
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

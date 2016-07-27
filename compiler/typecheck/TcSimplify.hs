{-# LANGUAGE CPP #-}

module TcSimplify(
       simplifyInfer, InferMode(..),
       growThetaTyVars,
       simplifyAmbiguityCheck,
       simplifyDefault,
       simplifyTop, simplifyInteractive, solveEqualities,
       simplifyWantedsTcM,
       tcCheckSatisfiability,

       -- For Rules we need these
       solveWanteds, runTcSDeriveds
  ) where

#include "HsVersions.h"

import Bag
import Class         ( Class, classKey, classTyCon )
import DynFlags      ( WarningFlag ( Opt_WarnMonomorphism )
                     , WarnReason ( Reason )
                     , DynFlags( solverIterations ) )
import Inst
import ListSetOps
import Maybes
import Name
import Outputable
import PrelInfo
import PrelNames
import TcErrors
import TcEvidence
import TcInteract
import TcCanonical   ( makeSuperClasses )
import TcMType   as TcM
import TcRnMonad as TcM
import TcSMonad  as TcS
import TcType
import TrieMap       () -- DV: for now
import Type
import TysWiredIn    ( ptrRepLiftedTy )
import Unify         ( tcMatchTyKi )
import Util
import Var
import VarSet
import UniqFM
import BasicTypes    ( IntWithInf, intGtLimit )
import ErrUtils      ( emptyMessages )
import qualified GHC.LanguageExtensions as LangExt

import Control.Monad ( when, unless )
import Data.List     ( partition )

{-
*********************************************************************************
*                                                                               *
*                           External interface                                  *
*                                                                               *
*********************************************************************************
-}

simplifyTop :: WantedConstraints -> TcM (Bag EvBind)
-- Simplify top-level constraints
-- Usually these will be implications,
-- but when there is nothing to quantify we don't wrap
-- in a degenerate implication, so we do that here instead
simplifyTop wanteds
  = do { traceTc "simplifyTop {" $ text "wanted = " <+> ppr wanteds
       ; ((final_wc, unsafe_ol), binds1) <- runTcS $
            do { final_wc <- simpl_top wanteds
               ; unsafe_ol <- getSafeOverlapFailures
               ; return (final_wc, unsafe_ol) }
       ; traceTc "End simplifyTop }" empty

       ; traceTc "reportUnsolved {" empty
       ; binds2 <- reportUnsolved final_wc
       ; traceTc "reportUnsolved }" empty

       ; traceTc "reportUnsolved (unsafe overlapping) {" empty
       ; unless (isEmptyCts unsafe_ol) $ do {
           -- grab current error messages and clear, warnAllUnsolved will
           -- update error messages which we'll grab and then restore saved
           -- messages.
           ; errs_var  <- getErrsVar
           ; saved_msg <- TcM.readTcRef errs_var
           ; TcM.writeTcRef errs_var emptyMessages

           ; warnAllUnsolved $ WC { wc_simple = unsafe_ol
                                  , wc_insol = emptyCts
                                  , wc_impl = emptyBag }

           ; whyUnsafe <- fst <$> TcM.readTcRef errs_var
           ; TcM.writeTcRef errs_var saved_msg
           ; recordUnsafeInfer whyUnsafe
           }
       ; traceTc "reportUnsolved (unsafe overlapping) }" empty

       ; return (evBindMapBinds binds1 `unionBags` binds2) }

-- | Type-check a thing that emits only equality constraints, then
-- solve those constraints. Fails outright if there is trouble.
solveEqualities :: TcM a -> TcM a
solveEqualities thing_inside
  = checkNoErrs $  -- See Note [Fail fast on kind errors]
    do { (result, wanted) <- captureConstraints thing_inside
       ; traceTc "solveEqualities {" $ text "wanted = " <+> ppr wanted
       ; final_wc <- runTcSEqualities $ simpl_top wanted
       ; traceTc "End solveEqualities }" empty

       ; traceTc "reportAllUnsolved {" empty
       ; reportAllUnsolved final_wc
       ; traceTc "reportAllUnsolved }" empty
       ; return result }

simpl_top :: WantedConstraints -> TcS WantedConstraints
    -- See Note [Top-level Defaulting Plan]
simpl_top wanteds
  = do { wc_first_go <- nestTcS (solveWantedsAndDrop wanteds)
                            -- This is where the main work happens
       ; try_tyvar_defaulting wc_first_go }
  where
    try_tyvar_defaulting :: WantedConstraints -> TcS WantedConstraints
    try_tyvar_defaulting wc
      | isEmptyWC wc
      = return wc
      | otherwise
      = do { free_tvs <- TcS.zonkTyCoVarsAndFVList (tyCoVarsOfWCList wc)
           ; let meta_tvs = filter (isTyVar <&&> isMetaTyVar) free_tvs
                   -- zonkTyCoVarsAndFV: the wc_first_go is not yet zonked
                   -- filter isMetaTyVar: we might have runtime-skolems in GHCi,
                   -- and we definitely don't want to try to assign to those!
                   -- the isTyVar needs to weed out coercion variables

           ; defaulted <- mapM defaultTyVarTcS meta_tvs   -- Has unification side effects
           ; if or defaulted
             then do { wc_residual <- nestTcS (solveWanteds wc)
                            -- See Note [Must simplify after defaulting]
                     ; try_class_defaulting wc_residual }
             else try_class_defaulting wc }     -- No defaulting took place

    try_class_defaulting :: WantedConstraints -> TcS WantedConstraints
    try_class_defaulting wc
      | isEmptyWC wc
      = return wc
      | otherwise  -- See Note [When to do type-class defaulting]
      = do { something_happened <- applyDefaultingRules wc
                                   -- See Note [Top-level Defaulting Plan]
           ; if something_happened
             then do { wc_residual <- nestTcS (solveWantedsAndDrop wc)
                     ; try_class_defaulting wc_residual }
                  -- See Note [Overview of implicit CallStacks] in TcEvidence
             else try_callstack_defaulting wc }

    try_callstack_defaulting :: WantedConstraints -> TcS WantedConstraints
    try_callstack_defaulting wc
      | isEmptyWC wc
      = return wc
      | otherwise
      = defaultCallStacks wc

-- | Default any remaining @CallStack@ constraints to empty @CallStack@s.
defaultCallStacks :: WantedConstraints -> TcS WantedConstraints
-- See Note [Overview of implicit CallStacks] in TcEvidence
defaultCallStacks wanteds
  = do simples <- handle_simples (wc_simple wanteds)
       implics <- mapBagM handle_implic (wc_impl wanteds)
       return (wanteds { wc_simple = simples, wc_impl = implics })

  where

  handle_simples simples
    = catBagMaybes <$> mapBagM defaultCallStack simples

  handle_implic implic
    = do { wanteds <- setEvBindsTcS (ic_binds implic) $
                      -- defaultCallStack sets a binding, so
                      -- we must set the correct binding group
                      defaultCallStacks (ic_wanted implic)
         ; return (implic { ic_wanted = wanteds }) }

  defaultCallStack ct
    | Just _ <- isCallStackPred (ctPred ct)
    = do { solveCallStack (cc_ev ct) EvCsEmpty
         ; return Nothing }

  defaultCallStack ct
    = return (Just ct)


{- Note [Fail fast on kind errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
solveEqualities is used to solve kind equalities when kind-checking
user-written types. If solving fails we should fail outright, rather
than just accumulate an error message, for two reasons:

  * A kind-bogus type signature may cause a cascade of knock-on
    errors if we let it pass

  * More seriously, we don't have a convenient term-level place to add
    deferred bindings for unsolved kind-equality constraints, so we
    don't build evidence bindings (by usine reportAllUnsolved). That
    means that we'll be left with with a type that has coercion holes
    in it, something like
           <type> |> co-hole
    where co-hole is not filled in.  Eeek!  That un-filled-in
    hole actually causes GHC to crash with "fvProv falls into a hole"
    See Trac #11563, #11520, #11516, #11399

So it's important to use 'checkNoErrs' here!

Note [When to do type-class defaulting]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In GHC 7.6 and 7.8.2, we did type-class defaulting only if insolubleWC
was false, on the grounds that defaulting can't help solve insoluble
constraints.  But if we *don't* do defaulting we may report a whole
lot of errors that would be solved by defaulting; these errors are
quite spurious because fixing the single insoluble error means that
defaulting happens again, which makes all the other errors go away.
This is jolly confusing: Trac #9033.

So it seems better to always do type-class defaulting.

However, always doing defaulting does mean that we'll do it in
situations like this (Trac #5934):
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
errors, because it isn't an error!  Trac #7967 was due to this.

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

 2) `TcInteract.matchClassInst` -- This module drives the instance resolution
    / dictionary generation. The return type is `LookupInstResult`, which either
    says no instance matched, or one found, and if it was a safe or unsafe
    overlap.

 3) `TcInteract.doTopReactDict` -- Takes a dictionary / class constraint and
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

 4) `TcSimplify.simplifyTop`:
       * Call simpl_top, the top-level function for driving the simplifier for
         constraint resolution.

       * Once finished, call `getSafeOverlapFailures` to retrieve the
         list of overlapping instances that were successfully resolved,
         but unsafe. Remember, this is only applicable for generating warnings
         (`-Wunsafe`) or inferring a module unsafe. `-XSafe` and `-XTrustworthy`
         cause compilation failure by not resolving the unsafe constraint at all.

       * For unresolved constraints (all types), call `TcErrors.reportUnsolved`,
         while for resolved but unsafe overlapping dictionary constraints, call
         `TcErrors.warnAllUnsolved`. Both functions convert constraints into a
         warning message for the user.

       * In the case of `warnAllUnsolved` for resolved, but unsafe
         dictionary constraints, we collect the generated warning
         message (pop it) and call `TcRnMonad.recordUnsafeInfer` to
         mark the module we are compiling as unsafe, passing the
         warning message along as the reason.

 5) `TcErrors.*Unsolved` -- Generates error messages for constraints by
    actually calling `InstEnv.lookupInstEnv` again! Yes, confusing, but all we
    know is the constraint that is unresolved or unsafe. For dictionary, all we
    know is that we need a dictionary of type C, but not what instances are
    available and how they overlap. So we once again call `lookupInstEnv` to
    figure that out so we can generate a helpful error message.

 6) `TcRnMonad.recordUnsafeInfer` -- Save the unsafe result and reason in an
      IORef called `tcg_safeInfer`.

 7) `HscMain.tcRnModule'` -- Reads `tcg_safeInfer` after type-checking, calling
    `HscMain.markUnsafeInfer` (passing the reason along) when safe-inferrence
    failed.

Note [No defaulting in the ambiguity check]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When simplifying constraints for the ambiguity check, we use
solveWantedsAndDrop, not simpl_top, so that we do no defaulting.
Trac #11947 was an example:
   f :: Num a => Int -> Int
This is ambiguous of course, but we don't want to default the
(Num alpha) constraint to (Num Int)!  Doing so gives a defaulting
warning, but no error.
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
       ; tc_lvl <- TcM.getTcLevel
       ; unless (allow_ambiguous && not (insolubleWC tc_lvl final_wc))
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
       ; loc <- getCtLocM DefaultOrigin Nothing
       ; let wanted = [ CtDerived { ctev_pred = pred
                                  , ctev_loc  = loc }
                      | pred <- theta ]
       ; unsolved <- runTcSDeriveds (solveWanteds (mkSimpleWC wanted))
       ; traceTc "reportUnsolved {" empty
       ; reportAllUnsolved unsolved
       ; traceTc "reportUnsolved }" empty
       ; return () }

------------------
tcCheckSatisfiability :: Bag EvVar -> TcM Bool
-- Return True if satisfiable, False if definitely contradictory
tcCheckSatisfiability given_ids
  = do { lcl_env <- TcM.getLclEnv
       ; let given_loc = mkGivenLoc topTcLevel UnkSkol lcl_env
       ; (res, _ev_binds) <- runTcS $
             do { traceTcS "checkSatisfiability {" (ppr given_ids)
                ; let given_cts = mkGivens given_loc (bagToList given_ids)
                     -- See Note [Superclasses and satisfiability]
                ; insols <- solveSimpleGivens given_cts
                ; insols <- try_harder insols
                ; traceTcS "checkSatisfiability }" (ppr insols)
                ; return (isEmptyBag insols) }
       ; return res }
 where
    try_harder :: Cts -> TcS Cts
    -- Maybe we have to search up the superclass chain to find
    -- an unsatisfiable constraint.  Example: pmcheck/T3927b.
    -- At the moment we try just once
    try_harder insols
      | not (isEmptyBag insols)   -- We've found that it's definitely unsatisfiable
      = return insols             -- Hurrah -- stop now.
      | otherwise
      = do { pending_given <- getPendingScDicts
           ; new_given <- makeSuperClasses pending_given
           ; solveSimpleGivens new_given }

{- Note [Superclasses and satisfiability]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Expand superclasses before starting, because (Int ~ Bool), has
(Int ~~ Bool) as a superclass, which in turn has (Int ~N# Bool)
as a superclass, and it's the latter that is insoluble.  See
Note [The equality types story] in TysPrim.

If we fail to prove unsatisfiability we (arbitrarily) try just once to
find superclasses, using try_harder.  Reason: we might have a type
signature
   f :: F op (Implements push) => ..
where F is a type function.  This happened in Trac #3972.

We could do more than once but we'd have to have /some/ limit: in the
the recursive case, we would go on forever in the common case where
the constraints /are/ satisfiable (Trac #10592 comment:12!).

For stratightforard situations without type functions the try_harder
step does nothing.


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
   in TcBinds.tcPolyInfer

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
               | EagerDefaulting  -- ^ See Note [TcRnExprMode] in TcRnDriver,
                                  -- the :type +d case; this mode refuses
                                  -- to quantify over any defaultable constraint
               | NoRestrictions   -- ^ Quantify over any constraint that
                                  -- satisfies TcType.pickQuantifiablePreds

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
                      TcEvBinds)    -- ... binding these evidence variables
simplifyInfer rhs_tclvl infer_mode sigs name_taus wanteds
  | isEmptyWC wanteds
  = do { gbl_tvs <- tcGetGlobalTyCoVars
       ; dep_vars <- zonkTcTypesAndSplitDepVars (map snd name_taus)
       ; qtkvs <- quantifyZonkedTyVars gbl_tvs dep_vars
       ; traceTc "simplifyInfer: empty WC" (ppr name_taus $$ ppr qtkvs)
       ; return (qtkvs, [], emptyTcEvBinds) }

  | otherwise
  = do { traceTc "simplifyInfer {"  $ vcat
             [ text "sigs =" <+> ppr sigs
             , text "binds =" <+> ppr name_taus
             , text "rhs_tclvl =" <+> ppr rhs_tclvl
             , text "infer_mode =" <+> ppr infer_mode
             , text "(unzonked) wanted =" <+> ppr wanteds
             ]

       ; let partial_sigs = filter isPartialSig sigs
             psig_theta   = concatMap sig_inst_theta partial_sigs

       -- First do full-blown solving
       -- NB: we must gather up all the bindings from doing
       -- this solving; hence (runTcSWithEvBinds ev_binds_var).
       -- And note that since there are nested implications,
       -- calling solveWanteds will side-effect their evidence
       -- bindings, so we can't just revert to the input
       -- constraint.

       ; tc_lcl_env      <- TcM.getLclEnv
       ; ev_binds_var    <- TcM.newTcEvBinds
       ; psig_theta_vars <- mapM TcM.newEvVar psig_theta
       ; wanted_transformed_incl_derivs
            <- setTcLevel rhs_tclvl $
               runTcSWithEvBinds False (Just ev_binds_var) $
               do { let loc = mkGivenLoc rhs_tclvl UnkSkol tc_lcl_env
                        psig_givens = mkGivens loc psig_theta_vars
                  ; _ <- solveSimpleGivens psig_givens
                         -- See Note [Add signature contexts as givens]
                  ; solveWanteds wanteds }
       ; wanted_transformed_incl_derivs <- TcM.zonkWC wanted_transformed_incl_derivs

       -- Find quant_pred_candidates, the predicates that
       -- we'll consider quantifying over
       -- NB1: wanted_transformed does not include anything provable from
       --      the psig_theta; it's just the extra bit
       -- NB2: We do not do any defaulting when inferring a type, this can lead
       --      to less polymorphic types, see Note [Default while Inferring]

       ; let wanted_transformed = dropDerivedWC wanted_transformed_incl_derivs
       ; quant_pred_candidates   -- Fully zonked
           <- if insolubleWC rhs_tclvl wanted_transformed_incl_derivs
              then return []   -- See Note [Quantification with errors]
                               -- NB: must include derived errors in this test,
                               --     hence "incl_derivs"

              else do { let quant_cand = approximateWC wanted_transformed
                            meta_tvs   = filter isMetaTyVar $
                                         tyCoVarsOfCtsList quant_cand

                      ; gbl_tvs <- tcGetGlobalTyCoVars
                            -- Miminise quant_cand.  We are not interested in any evidence
                            -- produced, because we are going to simplify wanted_transformed
                            -- again later. All we want here are the predicates over which to
                            -- quantify.
                            --
                            -- If any meta-tyvar unifications take place (unlikely),
                            -- we'll pick that up later.

                      -- See Note [Promote _and_ default when inferring]
                      ; let def_tyvar tv
                              = when (not $ tv `elemVarSet` gbl_tvs) $
                                defaultTyVar tv
                      ; mapM_ def_tyvar meta_tvs
                      ; mapM_ (promoteTyVar rhs_tclvl) meta_tvs

                      ; WC { wc_simple = simples }
                           <- setTcLevel rhs_tclvl $
                              runTcSDeriveds       $
                              solveSimpleWanteds   $
                              mapBag toDerivedCt quant_cand
                                -- NB: we don't want evidence,
                                -- so use Derived constraints

                      ; simples <- TcM.zonkSimples simples

                      ; return [ ctEvPred ev | ct <- bagToList simples
                                             , let ev = ctEvidence ct ] }

       -- NB: quant_pred_candidates is already fully zonked

       -- Decide what type variables and constraints to quantify
       -- NB: bound_theta are constraints we want to quantify over,
       --     /apart from/ the psig_theta, which we always quantify over
       ; (qtvs, bound_theta) <- decideQuantification infer_mode name_taus psig_theta
                                                     quant_pred_candidates

         -- Promote any type variables that are free in the inferred type
         -- of the function:
         --    f :: forall qtvs. bound_theta => zonked_tau
         -- These variables now become free in the envt, and hence will show
         -- up whenever 'f' is called.  They may currently at rhs_tclvl, but
         -- they had better be unifiable at the outer_tclvl!
         -- Example:   envt mentions alpha[1]
         --            tau_ty = beta[2] -> beta[2]
         --            consraints = alpha ~ [beta]
         -- we don't quantify over beta (since it is fixed by envt)
         -- so we must promote it!  The inferred type is just
         --   f :: beta -> beta
       ; zonked_taus <- mapM (TcM.zonkTcType . snd) name_taus
              -- decideQuantification turned some meta tyvars into
              -- quantified skolems, so we have to zonk again

       ; let phi_tkvs = tyCoVarsOfTypes bound_theta  -- Already zonked
                        `unionVarSet` tyCoVarsOfTypes zonked_taus
             promote_tkvs = closeOverKinds phi_tkvs `delVarSetList` qtvs

       ; MASSERT2( closeOverKinds promote_tkvs `subVarSet` promote_tkvs
                 , ppr phi_tkvs $$
                   ppr (closeOverKinds phi_tkvs) $$
                   ppr promote_tkvs $$
                   ppr (closeOverKinds promote_tkvs) )
           -- we really don't want a type to be promoted when its kind isn't!

           -- promoteTyVar ignores coercion variables
       ; outer_tclvl <- TcM.getTcLevel
       ; mapM_ (promoteTyVar outer_tclvl) (nonDetEltsUFM promote_tkvs)
           -- It's OK to use nonDetEltsUFM here because promoteTyVar is
           -- commutative

           -- Emit an implication constraint for the
           -- remaining constraints from the RHS
           -- extra_qtvs: see Note [Quantification and partial signatures]
       ; bound_theta_vars <- mapM TcM.newEvVar bound_theta
       ; psig_theta_vars  <- mapM zonkId psig_theta_vars
       ; all_qtvs         <- add_psig_tvs qtvs
                             [ tv | sig <- partial_sigs
                                  , (_,tv) <- sig_inst_skols sig ]

       ; let full_theta      = psig_theta      ++ bound_theta
             full_theta_vars = psig_theta_vars ++ bound_theta_vars
             skol_info   = InferSkol [ (name, mkSigmaTy [] full_theta ty)
                                     | (name, ty) <- name_taus ]
                        -- Don't add the quantified variables here, because
                        -- they are also bound in ic_skols and we want them
                        -- to be tidied uniformly

             implic = Implic { ic_tclvl    = rhs_tclvl
                             , ic_skols    = all_qtvs
                             , ic_no_eqs   = False
                             , ic_given    = full_theta_vars
                             , ic_wanted   = wanted_transformed
                             , ic_status   = IC_Unsolved
                             , ic_binds    = Just ev_binds_var
                             , ic_info     = skol_info
                             , ic_env      = tc_lcl_env }
       ; emitImplication implic

         -- All done!
       ; traceTc "} simplifyInfer/produced residual implication for quantification" $
         vcat [ text "quant_pred_candidates =" <+> ppr quant_pred_candidates
              , text "promote_tvs=" <+> ppr promote_tkvs
              , text "psig_theta =" <+> ppr psig_theta
              , text "bound_theta =" <+> ppr bound_theta
              , text "full_theta =" <+> ppr full_theta
              , text "qtvs =" <+> ppr qtvs
              , text "implic =" <+> ppr implic ]

       ; return ( qtvs, full_theta_vars, TcEvBinds ev_binds_var ) }
  where
    add_psig_tvs qtvs [] = return qtvs
    add_psig_tvs qtvs (tv:tvs)
      = do { tv <- zonkTcTyVarToTyVar tv
           ; if tv `elem` qtvs
             then add_psig_tvs qtvs tvs
             else do { mb_tv <- zonkQuantifiedTyVar False tv
                     ; case mb_tv of
                         Nothing -> add_psig_tvs qtvs      tvs
                         Just tv -> add_psig_tvs (tv:qtvs) tvs } }

{- Note [Add signature contexts as givens]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this (Trac #11016):
  f2 :: (?x :: Int) => _
  f2 = ?x
or this
  f3 :: a ~ Bool => (a, _)
  f3 = (True, False)
or theis
  f4 :: (Ord a, _) => a -> Bool
  f4 x = x==x

We'll use plan InferGen because there are holes in the type.  But:
 * For f2 we want to have the (?x :: Int) constraint floating around
   so that the functional dependencies kick in.  Otherwise the
   occurrence of ?x on the RHS produces constraint (?x :: alpha), and
   we won't unify alpha:=Int.
 * For f3 we want the (a ~ Bool) available to solve the wanted (a ~ Bool)
   in the RHS
 * For f4 we want to use the (Ord a) in the signature to solve the Eq a
   constraint.

Solution: in simplifyInfer, just before simplifying the constraints
gathered from the RHS, add Given constraints for the context of any
type signatures.

************************************************************************
*                                                                      *
                Quantification
*                                                                      *
************************************************************************

Note [Deciding quantification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If the monomorphism restriction does not apply, then we quantify as follows:

  * Take the global tyvars, and "grow" them using the equality constraints
    E.g.  if x:alpha is in the environment, and alpha ~ [beta] (which can
          happen because alpha is untouchable here) then do not quantify over
          beta, because alpha fixes beta, and beta is effectively free in
          the environment too
    These are the mono_tvs

  * Take the free vars of the tau-type (zonked_tau_tvs) and "grow" them
    using all the constraints.  These are tau_tvs_plus

  * Use quantifyTyVars to quantify over (tau_tvs_plus - mono_tvs), being
    careful to close over kinds, and to skolemise the quantified tyvars.
    (This actually unifies each quantifies meta-tyvar with a fresh skolem.)
    Result is qtvs.

  * Filter the constraints using pickQuantifiablePreds and the qtvs.
    We have to zonk the constraints first, so they "see" the freshly
    created skolems.

If the MR does apply, mono_tvs includes all the constrained tyvars --
including all covars -- and the quantified constraints are empty/insoluble.

-}

decideQuantification
  :: InferMode
  -> [(Name, TcTauType)]   -- Variables to be generalised
  -> [PredType]            -- All annotated constraints from signatures
  -> [PredType]            -- Candidate theta
  -> TcM ( [TcTyVar]       -- Quantify over these (skolems)
         , [PredType] )    -- and this context (fully zonked)
-- See Note [Deciding quantification]
decideQuantification infer_mode name_taus psig_theta candidates
  = do { gbl_tvs <- tcGetGlobalTyCoVars
       ; zonked_taus <- mapM TcM.zonkTcType (psig_theta ++ taus)
                        -- psig_theta: see Note [Quantification and partial signatures]
       ; ovl_strings <- xoptM LangExt.OverloadedStrings
       ; let DV {dv_kvs = zkvs, dv_tvs = ztvs} = splitDepVarsOfTypes zonked_taus
             (gbl_cand, quant_cand)  -- gbl_cand   = do not quantify me
                = case infer_mode of   -- quant_cand = try to quantify me
                    ApplyMR         -> (candidates, [])
                    NoRestrictions  -> ([], candidates)
                    EagerDefaulting -> partition is_interactive_ct candidates
                      where
                        is_interactive_ct ct
                          | Just (cls, _) <- getClassPredTys_maybe ct
                          = isInteractiveClass ovl_strings cls
                          | otherwise
                          = False

             eq_constraints  = filter isEqPred quant_cand
             constrained_tvs = tyCoVarsOfTypes gbl_cand
             mono_tvs        = growThetaTyVars eq_constraints $
                               gbl_tvs `unionVarSet` constrained_tvs
             tau_tvs_plus    = growThetaTyVarsDSet quant_cand ztvs
             dvs_plus        = DV { dv_kvs = zkvs, dv_tvs = tau_tvs_plus }

       ; qtvs <- quantifyZonkedTyVars mono_tvs dvs_plus
          -- We don't grow the kvs, as there's no real need to. Recall
          -- that quantifyTyVars uses the separation between kvs and tvs
          -- only for defaulting, and we don't want (ever) to default a tv
          -- to *. So, don't grow the kvs.

       ; quant_cand <- TcM.zonkTcTypes quant_cand
                 -- quantifyTyVars turned some meta tyvars into
                 -- quantified skolems, so we have to zonk again

       ; let qtv_set   = mkVarSet qtvs
             theta     = pickQuantifiablePreds qtv_set quant_cand
             min_theta = mkMinimalBySCs theta
               -- See Note [Minimize by Superclasses]

           -- Warn about the monomorphism restriction
       ; warn_mono <- woptM Opt_WarnMonomorphism
       ; let mr_bites | ApplyMR <- infer_mode
                      = constrained_tvs `intersectsVarSet` tcDepVarSet dvs_plus
                      | otherwise
                      = False
       ; warnTc (Reason Opt_WarnMonomorphism) (warn_mono && mr_bites) $
         hang (text "The Monomorphism Restriction applies to the binding"
               <> plural bndrs <+> text "for" <+> pp_bndrs)
             2 (text "Consider giving a type signature for"
                <+> if isSingleton bndrs then pp_bndrs
                                         else text "these binders")

       ; traceTc "decideQuantification"
           (vcat [ text "infer_mode:"   <+> ppr infer_mode
                 , text "gbl_cand:"     <+> ppr gbl_cand
                 , text "quant_cand:"   <+> ppr quant_cand
                 , text "gbl_tvs:"      <+> ppr gbl_tvs
                 , text "mono_tvs:"     <+> ppr mono_tvs
                 , text "tau_tvs_plus:" <+> ppr tau_tvs_plus
                 , text "qtvs:"         <+> ppr qtvs
                 , text "min_theta:"    <+> ppr min_theta ])
       ; return (qtvs, min_theta) }
  where
    pp_bndrs = pprWithCommas (quotes . ppr) bndrs
    (bndrs, taus) = unzip name_taus

------------------
growThetaTyVars :: ThetaType -> TyCoVarSet -> TyVarSet
-- See Note [Growing the tau-tvs using constraints]
-- NB: only returns tyvars, never covars
growThetaTyVars theta tvs
  | null theta = tvs_only
  | otherwise  = filterVarSet isTyVar $
                 transCloVarSet mk_next seed_tvs
  where
    tvs_only = filterVarSet isTyVar tvs
    seed_tvs = tvs `unionVarSet` tyCoVarsOfTypes ips
    (ips, non_ips) = partition isIPPred theta
                         -- See Note [Inheriting implicit parameters] in TcType

    mk_next :: VarSet -> VarSet -- Maps current set to newly-grown ones
    mk_next so_far = foldr (grow_one so_far) emptyVarSet non_ips
    grow_one so_far pred tvs
       | pred_tvs `intersectsVarSet` so_far = tvs `unionVarSet` pred_tvs
       | otherwise                          = tvs
       where
         pred_tvs = tyCoVarsOfType pred

------------------
growThetaTyVarsDSet :: ThetaType -> DTyCoVarSet -> DTyVarSet
-- See Note [Growing the tau-tvs using constraints]
-- NB: only returns tyvars, never covars
-- It takes a deterministic set of TyCoVars and returns a deterministic set
-- of TyVars.
-- The implementation mirrors growThetaTyVars, the only difference is that
-- it avoids unionDVarSet and uses more efficient extendDVarSetList.
growThetaTyVarsDSet theta tvs
  | null theta = tvs_only
  | otherwise  = filterDVarSet isTyVar $
                 transCloDVarSet mk_next seed_tvs
  where
    tvs_only = filterDVarSet isTyVar tvs
    seed_tvs = tvs `extendDVarSetList` tyCoVarsOfTypesList ips
    (ips, non_ips) = partition isIPPred theta
                         -- See Note [Inheriting implicit parameters] in TcType

    mk_next :: DVarSet -> DVarSet -- Maps current set to newly-grown ones
    mk_next so_far = foldr (grow_one so_far) emptyDVarSet non_ips
    grow_one so_far pred tvs
       | any (`elemDVarSet` so_far) pred_tvs = tvs `extendDVarSetList` pred_tvs
       | otherwise                           = tvs
       where
         pred_tvs = tyCoVarsOfTypeList pred

{- Note [Quantification and partial signatures]
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
In both cases we use plan InferGen, and hence call simplifyInfer.
But those 'a' variables are skolems, and we should be sure to quantify
over them, for two reasons

* In the case of a type error
     f :: _ -> Maybe a
     f x = True && x
  The inferred type of 'f' is f :: Bool -> Bool, but there's a
  left-over error of form (HoleCan (Maybe a ~ Bool)).  The error-reporting
  machine expects to find a binding site for the skolem 'a', so we
  add it to the ic_skols of the residual implication.

  Note that we /only/ do this to the residual implication. We don't
  complicate the quantified type varialbes of 'f' for downstream code;
  it's just a device to make the error message generator know what to
  report.

* Consider the partial type signature
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
  which isn't ambiguous but is still very wrong.  That's why include
  psig_theta in the variables to quantify over, passed to
  decideQuantification.

Note [Quantifying over equality constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Should we quantify over an equality constraint (s ~ t)?  In general, we don't.
Doing so may simply postpone a type error from the function definition site to
its call site.  (At worst, imagine (Int ~ Bool)).

However, consider this
         forall a. (F [a] ~ Int) => blah
Should we quantify over the (F [a] ~ Int)?  Perhaps yes, because at the call
site we will know 'a', and perhaps we have instance  F [Bool] = Int.
So we *do* quantify over a type-family equality where the arguments mention
the quantified variables.

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
constraints, we abandon all attempts to find a context to quantify
over, and instead make the function fully-polymorphic in whatever
type we have found.  For two reasons
  a) Minimise downstream errors
  b) Avoid spurious errors from this function

But NB that we must include *derived* errors in the check. Example:
    (a::*) ~ Int#
We get an insoluble derived error *~#, and we don't want to discard
it before doing the isInsolubleWC test!  (Trac #8262)

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
    This note not longer applies; see the notes with Trac #4361.
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
-- Postcondition: fully zonked and unflattened constraints
simplifyWantedsTcM wanted
  = do { traceTc "simplifyWantedsTcM {" (ppr wanted)
       ; (result, _) <- runTcS (solveWantedsAndDrop $ mkSimpleWC wanted)
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
solveWanteds wc@(WC { wc_simple = simples, wc_insol = insols, wc_impl = implics })
  = do { traceTcS "solveWanteds {" (ppr wc)

         -- Try the simple bit, including insolubles. Solving insolubles a
         -- second time round is a bit of a waste; but the code is simple
         -- and the program is wrong anyway, and we don't run the danger
         -- of adding Derived insolubles twice; see
         -- TcSMonad Note [Do not add duplicate derived insolubles]
       ; wc1 <- solveSimpleWanteds simples
       ; (no_new_scs, wc1) <- expandSuperClasses wc1
       ; let WC { wc_simple = simples1, wc_insol = insols1, wc_impl = implics1 } = wc1

       ; (floated_eqs, implics2) <- solveNestedImplications (implics `unionBags` implics1)

       ; dflags <- getDynFlags
       ; final_wc <- simpl_loop 0 (solverIterations dflags) floated_eqs no_new_scs
                                (WC { wc_simple = simples1, wc_impl = implics2
                                    , wc_insol  = insols `unionBags` insols1 })

       ; bb <- TcS.getTcEvBindsMap
       ; traceTcS "solveWanteds }" $
                 vcat [ text "final wc =" <+> ppr final_wc
                      , text "current evbinds  =" <+> ppr (evBindMapBinds bb) ]

       ; return final_wc }

simpl_loop :: Int -> IntWithInf -> Cts -> Bool
           -> WantedConstraints
           -> TcS WantedConstraints
simpl_loop n limit floated_eqs no_new_scs
           wc@(WC { wc_simple = simples, wc_insol = insols, wc_impl = implics })
  | isEmptyBag floated_eqs && no_new_scs
  = return wc  -- Done!

  | n `intGtLimit` limit
  = do { -- Add an error (not a warning) if we blow the limit,
         -- Typically if we blow the limit we are going to report some other error
         -- (an unsolved constraint), and we don't want that error to suppress
         -- the iteration limit warning!
         addErrTcS (hang (text "solveWanteds: too many iterations"
                   <+> parens (text "limit =" <+> ppr limit))
                2 (vcat [ text "Unsolved:" <+> ppr wc
                        , ppUnless (isEmptyBag floated_eqs) $
                          text "Floated equalities:" <+> ppr floated_eqs
                        , ppUnless no_new_scs $
                          text "New superclasses found"
                        , text "Set limit with -fconstraint-solver-iterations=n; n=0 for no limit"
                  ]))
       ; return wc }

  | otherwise
  = do { let n_floated = lengthBag floated_eqs
       ; csTraceTcS $
         text "simpl_loop iteration=" <> int n
         <+> (parens $ hsep [ text "no new scs =" <+> ppr no_new_scs <> comma
                            , int n_floated <+> text "floated eqs" <> comma
                            , int (lengthBag simples) <+> text "simples to solve" ])

       -- solveSimples may make progress if either float_eqs hold
       ; (unifs1, wc1) <- reportUnifications $
                          solveSimpleWanteds (floated_eqs `unionBags` simples)
                               -- Put floated_eqs first so they get solved first
                               -- NB: the floated_eqs may include /derived/ equalities
                               --     arising from fundeps inside an implication

       ; (no_new_scs, wc1) <- expandSuperClasses wc1
       ; let WC { wc_simple = simples1, wc_insol = insols1, wc_impl = implics1 } = wc1

       -- We have already tried to solve the nested implications once
       -- Try again only if we have unified some meta-variables
       -- (which is a bit like adding more givens
       -- See Note [Cutting off simpl_loop]
       ; (floated_eqs2, implics2) <- if unifs1 == 0 && isEmptyBag implics1
                                     then return (emptyBag, implics)
                                     else solveNestedImplications (implics `unionBags` implics1)

       ; simpl_loop (n+1) limit floated_eqs2 no_new_scs
                    (WC { wc_simple = simples1, wc_impl = implics2
                        , wc_insol  = insols `unionBags` insols1 }) }

expandSuperClasses :: WantedConstraints -> TcS (Bool, WantedConstraints)
-- If there are any unsolved wanteds, expand one step of superclasses for
-- unsolved wanteds or givens
-- See Note [The superclass story] in TcCanonical
expandSuperClasses wc@(WC { wc_simple = unsolved, wc_insol = insols })
  | not (anyBag superClassesMightHelp unsolved)
  = return (True, wc)
  | otherwise
  = do { traceTcS "expandSuperClasses {" empty
       ; let (pending_wanted, unsolved') = mapAccumBagL get [] unsolved
             get acc ct = case isPendingScDict ct of
                            Just ct' -> (ct':acc, ct')
                            Nothing  -> (acc,     ct)
       ; pending_given <- getPendingScDicts
       ; if null pending_given && null pending_wanted
         then do { traceTcS "End expandSuperClasses no-op }" empty
                 ; return (True, wc) }
         else
    do { new_given  <- makeSuperClasses pending_given
       ; new_insols <- solveSimpleGivens new_given
       ; new_wanted <- makeSuperClasses pending_wanted
       ; traceTcS "End expandSuperClasses }"
                  (vcat [ text "Given:" <+> ppr pending_given
                        , text "Insols from given:" <+> ppr new_insols
                        , text "Wanted:" <+> ppr new_wanted ])
       ; return (False, wc { wc_simple = unsolved' `unionBags` listToBag new_wanted
                           , wc_insol = insols `unionBags` new_insols }) } }

solveNestedImplications :: Bag Implication
                        -> TcS (Cts, Bag Implication)
-- Precondition: the TcS inerts may contain unsolved simples which have
-- to be converted to givens before we go inside a nested implication.
solveNestedImplications implics
  | isEmptyBag implics
  = return (emptyBag, emptyBag)
  | otherwise
  = do { traceTcS "solveNestedImplications starting {" empty
       ; (floated_eqs_s, unsolved_implics) <- mapAndUnzipBagM solveImplication implics
       ; let floated_eqs = concatBag floated_eqs_s

       -- ... and we are back in the original TcS inerts
       -- Notice that the original includes the _insoluble_simples so it was safe to ignore
       -- them in the beginning of this function.
       ; traceTcS "solveNestedImplications end }" $
                  vcat [ text "all floated_eqs ="  <+> ppr floated_eqs
                       , text "unsolved_implics =" <+> ppr unsolved_implics ]

       ; return (floated_eqs, catBagMaybes unsolved_implics) }

solveImplication :: Implication    -- Wanted
                 -> TcS (Cts,      -- All wanted or derived floated equalities: var = type
                         Maybe Implication) -- Simplified implication (empty or singleton)
-- Precondition: The TcS monad contains an empty worklist and given-only inerts
-- which after trying to solve this implication we must restore to their original value
solveImplication imp@(Implic { ic_tclvl  = tclvl
                             , ic_binds  = m_ev_binds
                             , ic_skols  = skols
                             , ic_given  = given_ids
                             , ic_wanted = wanteds
                             , ic_info   = info
                             , ic_status = status
                             , ic_env    = env })
  | IC_Solved {} <- status
  = return (emptyCts, Just imp)  -- Do nothing

  | otherwise  -- Even for IC_Insoluble it is worth doing more work
               -- The insoluble stuff might be in one sub-implication
               -- and other unsolved goals in another; and we want to
               -- solve the latter as much as possible
  = do { inerts <- getTcSInerts
       ; traceTcS "solveImplication {" (ppr imp $$ text "Inerts" <+> ppr inerts)

         -- Solve the nested constraints
       ; ((no_given_eqs, given_insols, residual_wanted), used_tcvs)
             <- nestImplicTcS m_ev_binds (mkVarSet (skols ++ given_ids)) tclvl $
               do { let loc    = mkGivenLoc tclvl info env
                        givens = mkGivens loc given_ids
                  ; given_insols <- solveSimpleGivens givens

                  ; residual_wanted <- solveWanteds wanteds
                        -- solveWanteds, *not* solveWantedsAndDrop, because
                        -- we want to retain derived equalities so we can float
                        -- them out in floatEqualities

                  ; no_eqs <- getNoGivenEqs tclvl skols given_insols
                        -- Call getNoGivenEqs /after/ solveWanteds, because
                        -- solveWanteds can augment the givens, via expandSuperClasses,
                        -- to reveal given superclass equalities

                  ; return (no_eqs, given_insols, residual_wanted) }

       ; (floated_eqs, residual_wanted)
             <- floatEqualities skols no_given_eqs residual_wanted

       ; traceTcS "solveImplication 2"
           (ppr given_insols $$ ppr residual_wanted $$ ppr used_tcvs)
       ; let final_wanted = residual_wanted `addInsols` given_insols

       ; res_implic <- setImplicationStatus (imp { ic_no_eqs = no_given_eqs
                                                 , ic_wanted = final_wanted })
                                            used_tcvs

       ; evbinds <- TcS.getTcEvBindsMap
       ; traceTcS "solveImplication end }" $ vcat
             [ text "no_given_eqs =" <+> ppr no_given_eqs
             , text "floated_eqs =" <+> ppr floated_eqs
             , text "res_implic =" <+> ppr res_implic
             , text "implication evbinds = " <+> ppr (evBindMapBinds evbinds) ]

       ; return (floated_eqs, res_implic) }

----------------------
setImplicationStatus :: Implication -> TyCoVarSet  -- needed variables
                     -> TcS (Maybe Implication)
-- Finalise the implication returned from solveImplication:
--    * Set the ic_status field
--    * Trim the ic_wanted field to remove Derived constraints
-- Return Nothing if we can discard the implication altogether
setImplicationStatus implic@(Implic { ic_binds = m_ev_binds_var
                                    , ic_info = info
                                    , ic_tclvl  = tc_lvl
                                    , ic_wanted = wc
                                    , ic_given = givens })
                     used_tcvs
 | some_insoluble
 = return $ Just $
   implic { ic_status = IC_Insoluble
          , ic_wanted = wc { wc_simple = pruned_simples
                           , wc_insol  = pruned_insols } }

 | some_unsolved
 = return $ Just $
   implic { ic_status = IC_Unsolved
          , ic_wanted = wc { wc_simple = pruned_simples
                           , wc_insol  = pruned_insols } }

 | otherwise  -- Everything is solved; look at the implications
              -- See Note [Tracking redundant constraints]
 = do { ev_binds <- case m_ev_binds_var of
                      Just (EvBindsVar ref _) -> TcS.readTcRef ref
                      Nothing                 -> return emptyEvBindMap
      ; let all_needs = neededEvVars ev_binds
                                     (used_tcvs `unionVarSet` implic_needs)

            dead_givens | warnRedundantGivens info
                        = filterOut (`elemVarSet` all_needs) givens
                        | otherwise = []   -- None to report

            final_needs = all_needs `delVarSetList` givens

            discard_entire_implication  -- Can we discard the entire implication?
              =  null dead_givens           -- No warning from this implication
              && isEmptyBag pruned_implics  -- No live children
              && isEmptyVarSet final_needs  -- No needed vars to pass up to parent

            final_status = IC_Solved { ics_need = final_needs
                                     , ics_dead = dead_givens }
            final_implic = implic { ic_status = final_status
                                  , ic_wanted = wc { wc_simple = pruned_simples
                                                   , wc_insol  = pruned_insols
                                                   , wc_impl   = pruned_implics } }
               -- We can only prune the child implications (pruned_implics)
               -- in the IC_Solved status case, because only then we can
               -- accumulate their needed evidence variales into the
               -- IC_Solved final_status field of the parent implication.

      ; return $ if discard_entire_implication
                 then Nothing
                 else Just final_implic }
 where
   WC { wc_simple = simples, wc_impl = implics, wc_insol = insols } = wc

   some_insoluble = insolubleWC tc_lvl wc
   some_unsolved = not (isEmptyBag simples && isEmptyBag insols)
                 || isNothing mb_implic_needs

   pruned_simples = dropDerivedSimples simples
   pruned_insols  = dropDerivedInsols insols
   pruned_implics = filterBag need_to_keep_implic implics

   mb_implic_needs :: Maybe VarSet
        -- Just vs => all implics are IC_Solved, with 'vs' needed
        -- Nothing => at least one implic is not IC_Solved
   mb_implic_needs   = foldrBag add_implic (Just emptyVarSet) implics
   Just implic_needs = mb_implic_needs

   add_implic implic acc
      | Just vs_acc <- acc
      , IC_Solved { ics_need = vs } <- ic_status implic
      = Just (vs `unionVarSet` vs_acc)
      | otherwise = Nothing

   need_to_keep_implic ic
     | IC_Solved { ics_dead = [] } <- ic_status ic
           -- Fully solved, and no redundant givens to report
     , isEmptyBag (wc_impl (ic_wanted ic))
           -- And no children that might have things to report
     = False
     | otherwise
     = True

warnRedundantGivens :: SkolemInfo -> Bool
warnRedundantGivens (SigSkol ctxt _)
  = case ctxt of
       FunSigCtxt _ warn_redundant -> warn_redundant
       ExprSigCtxt                 -> True
       _                           -> False

  -- To think about: do we want to report redundant givens for
  -- pattern synonyms, PatSynSigSkol? c.f Trac #9953, comment:21.
warnRedundantGivens (InstSkol {}) = True
warnRedundantGivens _             = False

neededEvVars :: EvBindMap -> VarSet -> VarSet
-- Find all the evidence variables that are "needed",
--    and then delete all those bound by the evidence bindings
-- See note [Tracking redundant constraints]
neededEvVars ev_binds initial_seeds
 = needed `minusVarSet` bndrs
 where
   seeds  = foldEvBindMap add_wanted initial_seeds ev_binds
   needed = transCloVarSet also_needs seeds
   bndrs  = foldEvBindMap add_bndr emptyVarSet ev_binds

   add_wanted :: EvBind -> VarSet -> VarSet
   add_wanted (EvBind { eb_is_given = is_given, eb_rhs = rhs }) needs
     | is_given  = needs  -- Add the rhs vars of the Wanted bindings only
     | otherwise = evVarsOfTerm rhs `unionVarSet` needs

   also_needs :: VarSet -> VarSet
   also_needs needs
     = nonDetFoldUFM add emptyVarSet needs
     -- It's OK to use nonDetFoldUFM here because we immediately forget
     -- about the ordering by creating a set
     where
       add v needs
        | Just ev_bind <- lookupEvBind ev_binds v
        , EvBind { eb_is_given = is_given, eb_rhs = rhs } <- ev_bind
        , is_given
        = evVarsOfTerm rhs `unionVarSet` needs
        | otherwise
        = needs

   add_bndr :: EvBind -> VarSet -> VarSet
   add_bndr (EvBind { eb_lhs = v }) vs = extendVarSet vs v


{-
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
   the Ord dictionary.  This is done by TcInteract.solveOneFromTheOther
   See Note [Replacement vs keeping].

* To find (b) we need to know which evidence bindings are 'wanted';
  hence the eb_is_given field on an EvBind.

----- How tracking works

* When the constraint solver finishes solving all the wanteds in
  an implication, it sets its status to IC_Solved

  - The ics_dead field, of IC_Solved, records the subset of this implication's
    ic_given that are redundant (not needed).

  - The ics_need field of IC_Solved then records all the
    in-scope (given) evidence variables bound by the context, that
    were needed to solve this implication, including all its nested
    implications.  (We remove the ic_given of this implication from
    the set, of course.)

* We compute which evidence variables are needed by an implication
  in setImplicationStatus.  A variable is needed if
    a) it is free in the RHS of a Wanted EvBind,
    b) it is free in the RHS of an EvBind whose LHS is needed,
    c) it is in the ics_need of a nested implication.
    d) it is listed in the tcs_used_tcvs field of the nested TcSEnv

* We need to be careful not to discard an implication
  prematurely, even one that is fully solved, because we might
  thereby forget which variables it needs, and hence wrongly
  report a constraint as redundant.  But we can discard it once
  its free vars have been incorporated into its parent; or if it
  simply has no free vars. This careful discarding is also
  handled in setImplicationStatus.

----- Reporting redundant constraints

* TcErrors does the actual warning, in warnRedundantConstraints.

* We don't report redundant givens for *every* implication; only
  for those which reply True to TcSimplify.warnRedundantGivens:

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
        - TcBinds.tcSpecPrag
        - TcBinds.tcTySig

  This decision is taken in setImplicationStatus, rather than TcErrors
  so that we can discard implication constraints that we don't need.
  So ics_dead consists only of the *reportable* redundant givens.

----- Shortcomings

Consider (see Trac #9939)
    f2 :: (Eq a, Ord a) => a -> a -> Bool
    -- Ord a redundant, but Eq a is reported
    f2 x y = (x == y)

We report (Eq a) as redundant, whereas actually (Ord a) is.  But it's
really not easy to detect that!


Note [Cutting off simpl_loop]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It is very important not to iterate in simpl_loop unless there is a chance
of progress.  Trac #8474 is a classic example:

  * There's a deeply-nested chain of implication constraints.
       ?x:alpha => ?y1:beta1 => ... ?yn:betan => [W] ?x:Int

  * From the innermost one we get a [D] alpha ~ Int,
    but alpha is untouchable until we get out to the outermost one

  * We float [D] alpha~Int out (it is in floated_eqs), but since alpha
    is untouchable, the solveInteract in simpl_loop makes no progress

  * So there is no point in attempting to re-solve
       ?yn:betan => [W] ?x:Int
    via solveNestedImplications, because we'll just get the
    same [D] again

  * If we *do* re-solve, we'll get an ininite loop. It is cut off by
    the fixed bound of 10, but solving the next takes 10*10*...*10 (ie
    exponentially many) iterations!

Conclusion: we should call solveNestedImplications only if we did
some unifiction in solveSimpleWanteds; because that's the only way
we'll get more Givens (a unificaiton is like adding a Given) to
allow the implication to make progress.
-}

promoteTyVar :: TcLevel -> TcTyVar  -> TcM ()
-- When we float a constraint out of an implication we must restore
-- invariant (MetaTvInv) in Note [TcLevel and untouchable type variables] in TcType
-- See Note [Promoting unification variables]
promoteTyVar tclvl tv
  | isFloatedTouchableMetaTyVar tclvl tv
  = do { cloned_tv <- TcM.cloneMetaTyVar tv
       ; let rhs_tv = setMetaTyVarTcLevel cloned_tv tclvl
       ; TcM.writeMetaTyVar tv (mkTyVarTy rhs_tv) }
  | otherwise
  = return ()

promoteTyVarTcS :: TcLevel -> TcTyVar  -> TcS ()
-- When we float a constraint out of an implication we must restore
-- invariant (MetaTvInv) in Note [TcLevel and untouchable type variables] in TcType
-- See Note [Promoting unification variables]
-- We don't just call promoteTyVar because we want to use unifyTyVar,
-- not writeMetaTyVar
promoteTyVarTcS tclvl tv
  | isFloatedTouchableMetaTyVar tclvl tv
  = do { cloned_tv <- TcS.cloneMetaTyVar tv
       ; let rhs_tv = setMetaTyVarTcLevel cloned_tv tclvl
       ; unifyTyVar tv (mkTyVarTy rhs_tv) }
  | otherwise
  = return ()

-- | If the tyvar is a RuntimeRep var, set it to PtrRepLifted. Returns whether or
-- not this happened.
defaultTyVar :: TcTyVar -> TcM ()
-- Precondition: MetaTyVars only
-- See Note [DefaultTyVar]
defaultTyVar the_tv
  | isRuntimeRepVar the_tv
  = do { traceTc "defaultTyVar RuntimeRep" (ppr the_tv)
       ; writeMetaTyVar the_tv ptrRepLiftedTy }

  | otherwise = return ()    -- The common case

-- | Like 'defaultTyVar', but in the TcS monad.
defaultTyVarTcS :: TcTyVar -> TcS Bool
defaultTyVarTcS the_tv
  | isRuntimeRepVar the_tv
  = do { traceTcS "defaultTyVarTcS RuntimeRep" (ppr the_tv)
       ; unifyTyVar the_tv ptrRepLiftedTy
       ; return True }
  | otherwise
  = return False  -- the common case

approximateWC :: WantedConstraints -> Cts
-- Postcondition: Wanted or Derived Cts
-- See Note [ApproximateWC]
approximateWC wc
  = float_wc emptyVarSet wc
  where
    float_wc :: TcTyCoVarSet -> WantedConstraints -> Cts
    float_wc trapping_tvs (WC { wc_simple = simples, wc_impl = implics })
      = filterBag is_floatable simples `unionBags`
        do_bag (float_implic new_trapping_tvs) implics
      where
        is_floatable ct = tyCoVarsOfCt ct `disjointVarSet` new_trapping_tvs
        new_trapping_tvs = transCloVarSet grow trapping_tvs

        grow :: VarSet -> VarSet  -- Maps current trapped tyvars to newly-trapped ones
        grow so_far = foldrBag (grow_one so_far) emptyVarSet simples
        grow_one so_far ct tvs
          | ct_tvs `intersectsVarSet` so_far = tvs `unionVarSet` ct_tvs
          | otherwise                        = tvs
          where
            ct_tvs = tyCoVarsOfCt ct

    float_implic :: TcTyCoVarSet -> Implication -> Cts
    float_implic trapping_tvs imp
      | ic_no_eqs imp                 -- No equalities, so float
      = float_wc new_trapping_tvs (ic_wanted imp)
      | otherwise                     -- Don't float out of equalities
      = emptyCts                      -- See Note [ApproximateWC]
      where
        new_trapping_tvs = trapping_tvs `extendVarSetList` ic_skols imp
    do_bag :: (a -> Bag c) -> Bag a -> Bag c
    do_bag f = foldrBag (unionBags.f) emptyBag

{-
Note [ApproximateWC]
~~~~~~~~~~~~~~~~~~~~
approximateWC takes a constraint, typically arising from the RHS of a
let-binding whose type we are *inferring*, and extracts from it some
*simple* constraints that we might plausibly abstract over.  Of course
the top-level simple constraints are plausible, but we also float constraints
out from inside, if they are not captured by skolems.

The same function is used when doing type-class defaulting (see the call
to applyDefaultingRules) to extract constraints that that might be defaulted.

There are two caveats:

1.  We do *not* float anything out if the implication binds equality
    constraints, because that defeats the OutsideIn story.  Consider
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

Note [DefaultTyVar]
~~~~~~~~~~~~~~~~~~~
defaultTyVar is used on any un-instantiated meta type variables to
default any RuntimeRep variables to PtrRepLifted.  This is important
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

An alternate implementation would be to emit a derived constraint setting
the RuntimeRep variable to PtrRepLifted, but this seems unnecessarily indirect.

Note [Promote _and_ default when inferring]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we are inferring a type, we simplify the constraint, and then use
approximateWC to produce a list of candidate constraints.  Then we MUST

  a) Promote any meta-tyvars that have been floated out by
     approximateWC, to restore invariant (MetaTvInv) described in
     Note [TcLevel and untouchable type variables] in TcType.

  b) Default the kind of any meta-tyvars that are not mentioned in
     in the environment.

To see (b), suppose the constraint is (C ((a :: OpenKind) -> Int)), and we
have an instance (C ((x:*) -> Int)).  The instance doesn't match -- but it
should!  If we don't solve the constraint, we'll stupidly quantify over
(C (a->Int)) and, worse, in doing so zonkQuantifiedTyVar will quantify over
(b:*) instead of (a:OpenKind), which can lead to disaster; see Trac #7332.
Trac #7641 is a simpler example.

Note [Promoting unification variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we float an equality out of an implication we must "promote" free
unification variables of the equality, in order to maintain Invariant
(MetaTvInv) from Note [TcLevel and untouchable type variables] in TcType.  for the
leftover implication.

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


Note [Solving Family Equations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
After we are done with simplification we may be left with constraints of the form:
     [Wanted] F xis ~ beta
If 'beta' is a touchable unification variable not already bound in the TyBinds
then we'd like to create a binding for it, effectively "defaulting" it to be 'F xis'.

When is it ok to do so?
    1) 'beta' must not already be defaulted to something. Example:

           [Wanted] F Int  ~ beta   <~ Will default [beta := F Int]
           [Wanted] F Char ~ beta   <~ Already defaulted, can't default again. We
                                       have to report this as unsolved.

    2) However, we must still do an occurs check when defaulting (F xis ~ beta), to
       set [beta := F xis] only if beta is not among the free variables of xis.

    3) Notice that 'beta' can't be bound in ty binds already because we rewrite RHS
       of type family equations. See Inert Set invariants in TcInteract.

This solving is now happening during zonking, see Note [Unflattening while zonking]
in TcMType.


*********************************************************************************
*                                                                               *
*                          Floating equalities                                  *
*                                                                               *
*********************************************************************************

Note [Float Equalities out of Implications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For ordinary pattern matches (including existentials) we float
equalities out of implications, for instance:
     data T where
       MkT :: Eq a => a -> T
     f x y = case x of MkT _ -> (y::Int)
We get the implication constraint (x::T) (y::alpha):
     forall a. [untouchable=alpha] Eq a => alpha ~ Int
We want to float out the equality into a scope where alpha is no
longer untouchable, to solve the implication!

But we cannot float equalities out of implications whose givens may
yield or contain equalities:

      data T a where
        T1 :: T Int
        T2 :: T Bool
        T3 :: T a

      h :: T a -> a -> Int

      f x y = case x of
                T1 -> y::Int
                T2 -> y::Bool
                T3 -> h x y

We generate constraint, for (x::T alpha) and (y :: beta):
   [untouchables = beta] (alpha ~ Int => beta ~ Int)   -- From 1st branch
   [untouchables = beta] (alpha ~ Bool => beta ~ Bool) -- From 2nd branch
   (alpha ~ beta)                                      -- From 3rd branch

If we float the equality (beta ~ Int) outside of the first implication and
the equality (beta ~ Bool) out of the second we get an insoluble constraint.
But if we just leave them inside the implications, we unify alpha := beta and
solve everything.

Principle:
    We do not want to float equalities out which may
    need the given *evidence* to become soluble.

Consequence: classes with functional dependencies don't matter (since there is
no evidence for a fundep equality), but equality superclasses do matter (since
they carry evidence).
-}

floatEqualities :: [TcTyVar] -> Bool
                -> WantedConstraints
                -> TcS (Cts, WantedConstraints)
-- Main idea: see Note [Float Equalities out of Implications]
--
-- Precondition: the wc_simple of the incoming WantedConstraints are
--               fully zonked, so that we can see their free variables
--
-- Postcondition: The returned floated constraints (Cts) are only
--                Wanted or Derived
--
-- Also performs some unifications (via promoteTyVar), adding to
-- monadically-carried ty_binds. These will be used when processing
-- floated_eqs later
--
-- Subtleties: Note [Float equalities from under a skolem binding]
--             Note [Skolem escape]
floatEqualities skols no_given_eqs
                wanteds@(WC { wc_simple = simples })
  | not no_given_eqs  -- There are some given equalities, so don't float
  = return (emptyBag, wanteds)   -- Note [Float Equalities out of Implications]
  | otherwise
  = do { outer_tclvl <- TcS.getTcLevel
       ; mapM_ (promoteTyVarTcS outer_tclvl)
               (tyCoVarsOfCtsList float_eqs)
           -- See Note [Promoting unification variables]

       ; traceTcS "floatEqualities" (vcat [ text "Skols =" <+> ppr skols
                                          , text "Simples =" <+> ppr simples
                                          , text "Floated eqs =" <+> ppr float_eqs])
       ; return ( float_eqs
                , wanteds { wc_simple = remaining_simples } ) }
  where
    skol_set = mkVarSet skols
    (float_eqs, remaining_simples) = partitionBag (usefulToFloat skol_set) simples

usefulToFloat :: VarSet -> Ct -> Bool
usefulToFloat skol_set ct   -- The constraint is un-flattened and de-canonicalised
  = is_meta_var_eq pred &&
    (tyCoVarsOfType pred `disjointVarSet` skol_set)
  where
    pred = ctPred ct

      -- Float out alpha ~ ty, or ty ~ alpha
      -- which might be unified outside
      -- See Note [Which equalities to float]
    is_meta_var_eq pred
      | EqPred NomEq ty1 ty2 <- classifyPredType pred
      = case (tcGetTyVar_maybe ty1, tcGetTyVar_maybe ty2) of
          (Just tv1, _) -> float_tv_eq tv1 ty2
          (_, Just tv2) -> float_tv_eq tv2 ty1
          _             -> False
      | otherwise
      = False

    float_tv_eq tv1 ty2  -- See Note [Which equalities to float]
      =  isMetaTyVar tv1
      && (not (isSigTyVar tv1) || isTyVarTy ty2)

{- Note [Float equalities from under a skolem binding]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Which of the simple equalities can we float out?  Obviously, only
ones that don't mention the skolem-bound variables.  But that is
over-eager. Consider
   [2] forall a. F a beta[1] ~ gamma[2], G beta[1] gamma[2] ~ Int
The second constraint doesn't mention 'a'.  But if we float it,
we'll promote gamma[2] to gamma'[1].  Now suppose that we learn that
beta := Bool, and F a Bool = a, and G Bool _ = Int.  Then we'll
we left with the constraint
   [2] forall a. a ~ gamma'[1]
which is insoluble because gamma became untouchable.

Solution: float only constraints that stand a jolly good chance of
being soluble simply by being floated, namely ones of form
      a ~ ty
where 'a' is a currently-untouchable unification variable, but may
become touchable by being floated (perhaps by more than one level).

We had a very complicated rule previously, but this is nice and
simple.  (To see the notes, look at this Note in a version of
TcSimplify prior to Oct 2014).

Note [Which equalities to float]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Which equalities should we float?  We want to float ones where there
is a decent chance that floating outwards will allow unification to
happen.  In particular:

   Float out equalities of form (alpha ~ ty) or (ty ~ alpha), where

   * alpha is a meta-tyvar.

   * And 'alpha' is not a SigTv with 'ty' being a non-tyvar.  In that
     case, floating out won't help either, and it may affect grouping
     of error messages.

Note [Skolem escape]
~~~~~~~~~~~~~~~~~~~~
You might worry about skolem escape with all this floating.
For example, consider
    [2] forall a. (a ~ F beta[2] delta,
                   Maybe beta[2] ~ gamma[1])

The (Maybe beta ~ gamma) doesn't mention 'a', so we float it, and
solve with gamma := beta. But what if later delta:=Int, and
  F b Int = b.
Then we'd get a ~ beta[2], and solve to get beta:=a, and now the
skolem has escaped!

But it's ok: when we float (Maybe beta[2] ~ gamma[1]), we promote beta[2]
to beta[1], and that means the (a ~ beta[1]) will be stuck, as it should be.


*********************************************************************************
*                                                                               *
*                          Defaulting and disamgiguation                        *
*                                                                               *
*********************************************************************************
-}

applyDefaultingRules :: WantedConstraints -> TcS Bool
-- True <=> I did some defaulting, by unifying a meta-tyvar
-- Imput WantedConstraints are not necessarily zonked

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
    | group@((_,_,tv):_) <- unary_groups
    , defaultable_tyvar tv
    , defaultable_classes (map sndOf3 group) ]
  where
    simples                = approximateWC wanteds
    (unaries, non_unaries) = partitionWith find_unary (bagToList simples)
    unary_groups           = equivClasses cmp_tv unaries

    unary_groups :: [[(Ct, Class, TcTyVar)]]  -- (C tv) constraints
    unaries      ::  [(Ct, Class, TcTyVar)]   -- (C tv) constraints
    non_unaries  :: [Ct]                      -- and *other* constraints

        -- Finds unary type-class constraints
        -- But take account of polykinded classes like Typeable,
        -- which may look like (Typeable * (a:*))   (Trac #8931)
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

    defaultable_tyvar tv
        = let b1 = isTyConableTyVar tv  -- Note [Avoiding spurious errors]
              b2 = not (tv `elemVarSet` bad_tvs)
          in b1 && b2

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
       ; (success, _) <- nestImplicTcS (Just fake_ev_binds_var) emptyVarSet
                                       (pushTcLevel tclvl) try_group

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
           ; let loc = CtLoc { ctl_origin = GivenOrigin UnkSkol
                             , ctl_env    = lcl_env
                             , ctl_t_or_k = Nothing
                             , ctl_depth  = initialSubGoalDepth }
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
-}

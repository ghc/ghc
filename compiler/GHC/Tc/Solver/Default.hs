{-# LANGUAGE MultiWayIf #-}

module GHC.Tc.Solver.Default(
   tryDefaulting, tryUnsatisfiableGivens,
   isInteractiveClass, isNumClass
   ) where

import GHC.Prelude

import GHC.Tc.Errors
import GHC.Tc.Errors.Types
import GHC.Tc.Types.Evidence
import GHC.Tc.Solver.Solve   ( solveSimpleWanteds, setImplicationStatus )
import GHC.Tc.Solver.Dict    ( solveCallStack )
import GHC.Tc.Utils.Unify
import GHC.Tc.Utils.TcMType as TcM
import GHC.Tc.Utils.Monad   as TcM
import GHC.Tc.Zonk.TcType     as TcM
import GHC.Tc.Solver.Solve( solveWanteds )
import GHC.Tc.Solver.Monad  as TcS
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.CtLoc( mkGivenLoc )
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.TcType

import GHC.Core.Class
import GHC.Core.Reduction( Reduction, reductionCoercion )
import GHC.Core
import GHC.Core.DataCon
import GHC.Core.Make
import GHC.Core.Coercion( isReflCo, mkReflCo, mkSubCo )
import GHC.Core.Unify    ( tcMatchTyKis )
import GHC.Core.Predicate
import GHC.Core.Type
import GHC.Core.TyCon    ( TyCon )

import GHC.Types.DefaultEnv ( ClassDefaults (..), defaultList )
import GHC.Types.Unique.Set
import GHC.Types.Id

import GHC.Builtin.Utils
import GHC.Builtin.Names
import GHC.Builtin.Types

import GHC.Types.TyThing ( MonadThings(lookupId) )
import GHC.Types.Var
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Types.Id.Make  ( unboxedUnitExpr )

import GHC.Driver.DynFlags
import GHC.Unit.Module ( getModule )

import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Utils.Outputable

import GHC.Data.FastString
import GHC.Data.List.SetOps
import GHC.Data.Bag

import Control.Monad
import Control.Monad.Trans.Class        ( lift )
import Control.Monad.Trans.State.Strict ( StateT(runStateT), put )
import Data.Foldable      ( toList, traverse_ )
import Data.List          ( intersect )
import Data.List.NonEmpty ( NonEmpty(..), nonEmpty )
import qualified Data.List.NonEmpty as NE
import GHC.Data.Maybe     ( isJust, mapMaybe, catMaybes )
import Data.Monoid     ( First(..) )


{- Note [Top-level Defaulting Plan]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
-}


tryDefaulting :: WantedConstraints -> TcS WantedConstraints
-- This is the function that pulls all the defaulting strategies together
tryDefaulting wc
 = do { dflags <- getDynFlags
      ; traceTcS "tryDefaulting:before" (ppr wc)
      ; wc1 <- tryTyVarDefaulting dflags wc
      ; wc2 <- tryConstraintDefaulting wc1
      ; wc3 <- tryTypeClassDefaulting wc2
      ; wc4 <- tryUnsatisfiableGivens wc3
      ; traceTcS "tryDefaulting:after" (ppr wc4)
      ; return wc4 }

solveAgainIf :: Bool -> WantedConstraints -> TcS WantedConstraints
-- If the Bool is true, solve the wanted constraints again
-- See Note [Must simplify after defaulting]
solveAgainIf False wc = return wc
solveAgainIf True  wc = nestTcS (solveWanteds wc)


{- ******************************************************************************
*                                                                               *
                        tryTyVarDefaulting
*                                                                               *
****************************************************************************** -}

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


{- ******************************************************************************
*                                                                               *
                        tryUnsatisfiableGivens
*                                                                               *
****************************************************************************** -}

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


{- ******************************************************************************
*                                                                               *
                        tryConstraintDefaulting
*                                                                               *
****************************************************************************** -}

-- | A 'TcS' action which can may solve a `Ct`
type CtDefaultingStrategy = Ct -> TcS Bool
  -- True <=> I solved the constraint

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
      = do { simples'   <- mapMaybeBagM go_simple simples
           ; mb_implics <- mapMaybeBagM go_implic implics
           ; return (wc { wc_simple = simples', wc_impl = mb_implics }) }

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
    ( defaultCallStack :|
      defaultExceptionContext :
      defaultEquality :
      [] )

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
         -- EvCanonical: see Note [CallStack and ExceptionContext hack]
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
  | EqPred eq_rel ty1 ty2 <- classifyPredType (ctPred ct)
  = do { -- Remember: `ct` may not be zonked;
         -- see (DE3) in Note [Defaulting equalities]
         z_ty1 <- TcS.zonkTcType ty1
       ; z_ty2 <- TcS.zonkTcType ty2
       ; case eq_rel of
          { NomEq ->
       -- Now see if either LHS or RHS is a bare type variable
       -- You might think the type variable will only be on the LHS
       -- but with a type function we might get   F t1 ~ alpha
         case (getTyVar_maybe z_ty1, getTyVar_maybe z_ty2) of
           (Just z_tv1, _) -> try_default_tv z_tv1 z_ty2
           (_, Just z_tv2) -> try_default_tv z_tv2 z_ty1
           _               -> return False ;

          ; ReprEq
              -- See Note [Defaulting representational equalities]
              | CIrredCan (IrredCt { ir_reason }) <- ct
              , isInsolubleReason ir_reason
              -- Don't do this for definitely insoluble representational
              -- equalities such as Int ~R# Bool.
              -> return False
              | otherwise
              ->
       do { traceTcS "defaultEquality ReprEq {" $ vcat
              [ text "ct:" <+> ppr ct
              , text "z_ty1:" <+> ppr z_ty1
              , text "z_ty2:" <+> ppr z_ty2
              ]
            -- Promote this representational equality to a nominal equality.
            --
            -- This handles cases such as @IO alpha[tau] ~R# IO Int@
            -- by defaulting @alpha := Int@, which is useful in practice
            -- (see Note [Defaulting representational equalities]).
          ; (co, new_eqs, _unifs, _rw) <-
              wrapUnifierX (ctEvidence ct) Nominal $
              -- NB: nominal equality!
                \ uenv -> uType uenv z_ty1 z_ty2
            -- Only accept this solution if no new equalities are produced
            -- by the unifier.
            --
            -- See Note [Defaulting representational equalities].
          ; if null new_eqs
            then do { setEvBindIfWanted (ctEvidence ct) EvCanonical $
                       (evCoercion $ mkSubCo co)
                    ; return True }
            else return False
          } } }
  | otherwise
  = return False

  where
    try_default_tv lhs_tv rhs_ty
      | MetaTv { mtv_info = info } <- tcTyVarDetails lhs_tv
      , tyVarKind lhs_tv `tcEqType` typeKind rhs_ty
      , checkTopShape info rhs_ty
      -- Do not test for touchability of lhs_tv; that is the whole point!
      -- See (DE2) in Note [Defaulting equalities]
      = do { traceTcS "defaultEquality 1" (ppr lhs_tv $$ ppr rhs_ty)

           -- checkTyEqRhs: check that we can in fact unify lhs_tv := rhs_ty
           -- See Note [Defaulting equalities]
           ; let flags :: TyEqFlags TcM ()
                 flags = defaulting_TEFTask lhs_tv

           ; res :: PuResult () Reduction <- wrapTcS (checkTyEqRhs flags rhs_ty)

           ; case res of
               PuFail {}   -> cant_default_tv "checkTyEqRhs"
               PuOK _ redn -> assertPpr (isReflCo (reductionCoercion redn)) (ppr redn) $
                               -- With TEFA_Recurse we never get any reductions
                              default_tv }
      | otherwise
      = cant_default_tv "fall through"

      where
        cant_default_tv msg
          = do { traceTcS ("defaultEquality fails: " ++ msg) $
                 vcat [ ppr lhs_tv <+> char '~' <+>  ppr rhs_ty
                      , ppr (tyVarKind lhs_tv)
                      , ppr (typeKind rhs_ty) ]
               ; return False }

        -- All tests passed: do the unification
        default_tv
          = do { traceTcS "defaultEquality success:" (ppr rhs_ty)
               ; unifyTyVar lhs_tv rhs_ty  -- NB: unifyTyVar adds to the
                                           -- TcS unification counter
               ; setEvBindIfWanted (ctEvidence ct) EvCanonical $
                 evCoercion (mkReflCo Nominal rhs_ty)
               ; return True
               }


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
In top-level defaulting (as per Note [Top-level Defaulting Plan]), it makes
sense to try to default equality constraints, in addition to e.g. typeclass
defaulting: this doesn't threaten principal types (see DE1 below), but
allows GHC to accept strictly more programs.

This Note explains defaulting nominal equalities; see also
Note [Defaulting representational equalities] which describes
the defaulting of representational equalities.

Consider

  f :: forall a. (forall t. (F t ~ Int) => a -> Int) -> Int

  g :: Int
  g = f id

We'll typecheck

  id :: forall t. (F t ~ Int) => alpha[1] -> Int

where the `alpha[1]` comes from instantiating `f`. So we'll end up
with the implication constraint

   forall[2] t. (F t ~ Int) => alpha[1] ~ Int

and that can't be solved because `alpha` is untouchable under the
equality (F t ~ Int).

This is tiresome, and gave rise to user complaints: #25125 and #25029.
Moreover, in this case there is no good reason not to unify alpha:=Int.
Doing so solves the constraint, and since `alpha` is not otherwise
constrained, it does no harm.

In conclusion, for a Wanted equality constraint [W] lhs ~ rhs, if the only
reason for not unifying is that either lhs or rhs is an untouchable metavariable
then, in top-level defaulting, go ahead and unify.

In top-level defaulting, we already do several other somewhat-ad-hoc,
but terribly convenient, unifications. This is just one more.

Wrinkles:

(DE1) Note carefully that this does not threaten principal types.
  The original worry about unifying untouchable type variables was this:

     data T a where
       T1 :: T Bool
     f x = case x of T1 -> True

  Should we infer f :: T a -> Bool, or f :: T a -> a.  Both are valid, but
  neither is more general than the other.

(DE2) We still can't unify if there is a skolem-escape check, or an occurs check,
  or it it'd mean unifying a TyVarTv with a non-tyvar.  It's only the
  "untouchability test" that we lift.

(DE3) The contraint we are looking at may not be fully zonked; for example,
  an earlier defaulting might have affected it. So we zonk-on-the fly in
  `defaultEquality`.

(DE4) Promotion. Suppose we see  alpha[2] := Maybe beta[4].  We want to promote
  beta[4] to level 2 and unify alpha[2] := Maybe beta'[2].  This is done by
  checkTyEqRhs called in defaultEquality.

(DE5) Promotion. Suppose we see  alpha[2] := F beta[4], where F is a type
  family. Then we still want to promote beta to beta'[2], and unify. This is
  unusual: more commonly, we don't promote unification variables under a
  type family.  But here we want to.  (This mattered in #25251.)

  Hence the Bool flag on LC_Promote, and its use in `tef_unifying` in
  `defaultEquality`.

Note [Must simplify after defaulting]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We may have a deeply buried constraint
    (t:*) ~ (a:Open)
which we couldn't solve because of the kind incompatibility, and 'a' is free.
Then when we default 'a' we can solve the constraint.  And we want to do
that before starting in on type classes.  We MUST do it before reporting
errors, because it isn't an error!  #7967 was due to this.

Note [Defaulting representational equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we end up with [W] alpha ~#R Int, with no other constraints on alpha.
Then it makes sense to simply unify alpha := Int -- the alternative is to
reject the program due to an ambiguous metavariable alpha, so it makes sense
to unify and accept instead.

This is particularly convenient for users of `coerce`, as it lessens the
amount of type annotations required (see #21003). Consider for example:

  -- 'foldMap' defined using 'traverse'
  foldMapUsingTraverse :: forall t m a. (Traversable t, Monoid m) => (a -> m) -> t a -> m
  foldMapUsingTraverse = coerce $ traverse @t @(Const m)

  -- 'traverse_' defined using 'foldMap'
  traverse_UsingFoldMap :: forall f t a. (Foldable t, Applicative f) => (a -> f ()) -> t a -> f ()
  traverse_UsingFoldMap = coerce $ foldMap @t @(Ap f ())

Typechecking these functions results in unsolved Wanted constraints of the form
[W] alpha[tau] ~R# some_ty; accepting such programs by unifying
alpha := some_ty avoids the need for users to specify tiresome additional
type annotations, such as:

    foldMapUsingTraverse = coerce $ traverse @t @(Const m) @a
    traverse_UsingFoldMap = coerce $ foldMap @t @(Ap f ()) @a

Consider also the following example:

  -- 'sequence_', but for two nested 'Foldable' structures
  sequenceNested_ :: forall f1 f2. (Foldable f1, Foldable f2) => f1 (f2 (IO ())) -> IO ()
  sequenceNested_ = coerce $ sequence_ @( Compose f1 f2 )

Here, we end up with [W] mu[tau] beta[tau] ~#R IO (), and it similarly makes
sense to default mu := IO, beta := (). This avoids requiring the
user to provide additional type applications:

    sequenceNested_ = coerce $ sequence_ @( Compose f1 f2 ) @IO @()

The plan for defaulting a representational equality, say [W] ty1 ~R# ty2,
is thus as follows:

  1. attempt to unify ty1 ~# ty2 (at nominal role)
  2. a. if this succeeds without deferring any constraints, accept this solution
     b. otherwise, keep the original constraint.

(2b) ensures that we don't degrade all error messages by always turning unsolved
representational equalities into nominal ones; we only want to default a
representational equality when we can fully solve it.

Note that this does not threaten principle types. Recall that the original worry
(as per Note [Do not unify representational equalities]) was that we might have

    [W] alpha ~R# Int
    [W] alpha ~ Age

in which case unifying alpha := Int would be wrong, as the correct solution is
alpha := Age. This worry doesn't concern us in top-level defaulting, because
defaulting takes place after generalisation; it is fully monomorphic.

*********************************************************************************
*                                                                               *
*                Type-class defaulting
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
    simples  = approximateWC True wanteds
      -- True: for the purpose of defaulting we don't care
      --       about shape or enclosing equalities
      -- See (W3) in Note [ApproximateWC] in GHC.Tc.Types.Constraint

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

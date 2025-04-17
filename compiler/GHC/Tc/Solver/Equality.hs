{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}

module GHC.Tc.Solver.Equality(
     solveEquality
  ) where


import GHC.Prelude

import GHC.Tc.Solver.Irred( solveIrred )
import GHC.Tc.Solver.Dict( matchLocalInst, chooseInstance )
import GHC.Tc.Solver.Rewrite
import GHC.Tc.Solver.Monad
import GHC.Tc.Solver.InertSet
import GHC.Tc.Solver.Types( findFunEqsByTyCon )
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.CtLoc
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.Unify
import GHC.Tc.Utils.TcType
import GHC.Tc.Instance.Family ( tcTopNormaliseNewTypeTF_maybe )
import GHC.Tc.Instance.FunDeps( FunDepEqn(..) )
import qualified GHC.Tc.Utils.Monad    as TcM

import GHC.Core.Type
import GHC.Core.Predicate
import GHC.Core.Class
import GHC.Core.DataCon ( dataConName )
import GHC.Core.TyCon
import GHC.Core.TyCo.Rep   -- cleverly decomposes types, good for completeness checking
import GHC.Core.Coercion
import GHC.Core.Coercion.Axiom
import GHC.Core.Reduction
import GHC.Core.Unify( tcUnifyTyForInjectivity )
import GHC.Core.FamInstEnv ( FamInstEnvs, FamInst(..), apartnessCheck
                           , lookupFamInstEnvByTyCon )
import GHC.Core

import GHC.Types.Var
import GHC.Types.Var.Env
import GHC.Types.Var.Set( anyVarSet )
import GHC.Types.Name.Reader
import GHC.Types.Basic

import GHC.Builtin.Types.Literals ( tryInteractTopFam, tryInteractInertFam )

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc
import GHC.Utils.Monad

import GHC.Data.Pair
import GHC.Data.Bag
import Control.Monad
import Data.Maybe ( isJust, isNothing )
import Data.List  ( zip4 )

import qualified Data.Semigroup as S
import Data.Bifunctor ( bimap )
import Data.Void( Void )

{- *********************************************************************
*                                                                      *
*        Equalities
*                                                                      *
************************************************************************

Note [Canonicalising equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In order to canonicalise an equality, we look at the structure of the
two types at hand, looking for similarities. A difficulty is that the
types may look dissimilar before rewriting but similar after rewriting.
However, we don't just want to jump in and rewrite right away, because
this might be wasted effort. So, after looking for similarities and failing,
we rewrite and then try again. Of course, we don't want to loop, so we
track whether or not we've already rewritten.

It is conceivable to do a better job at tracking whether or not a type
is rewritten, but this is left as future work. (Mar '15)

Note [Decomposing FunTy]
~~~~~~~~~~~~~~~~~~~~~~~~
can_eq_nc' may attempt to decompose a FunTy that is un-zonked.  This
means that we may very well have a FunTy containing a type of some
unknown kind. For instance, we may have,

    FunTy (a :: k) Int

Where k is a unification variable. So the calls to splitRuntimeRep_maybe may
fail (returning Nothing).  In that case we'll fall through, zonk, and try again.
Zonking should fill the variable k, meaning that decomposition will succeed the
second time around.

Also note that we require the FunTyFlag to match.  This will stop
us decomposing
   (Int -> Bool)  ~  (Show a => blah)
It's as if we treat (->) and (=>) as different type constructors, which
indeed they are!
-}

solveEquality :: CtEvidence -> EqRel -> Type -> Type
              -> SolverStage Void
solveEquality ev eq_rel ty1 ty2
  = do { Pair ty1' ty2' <- zonkEqTypes ev eq_rel ty1 ty2
       ; mb_canon <- canonicaliseEquality ev eq_rel ty1' ty2'

       ; case mb_canon of {

            -- An IrredCt equality may be insoluble; but maybe not!
            -- E.g.  m a ~R# m b  is not canonical, but may be
            --       solved by a quantified constraint (T15290)
            -- See Note [Looking up primitive equalities in quantified constraints]
            Left irred_ct -> do { tryQCsIrredEqCt irred_ct
                                ; solveIrred irred_ct } ;

            Right eq_ct   -> do { tryInertEqs eq_ct
                                ; tryFunDeps  eq_rel eq_ct
                                ; tryQCsEqCt  eq_ct
                                ; simpleStage (updInertEqs eq_ct)
                                ; stopWithStage (eqCtEvidence eq_ct) "Kept inert EqCt" } } }

updInertEqs :: EqCt -> TcS ()
updInertEqs eq_ct
  = do { kickOutRewritable (KOAfterAdding (eqCtLHS eq_ct)) (eqCtFlavourRole eq_ct)
       ; tc_lvl <- getTcLevel
       ; updInertCans (addEqToCans tc_lvl eq_ct) }


{- *********************************************************************
*                                                                      *
*           zonkEqTypes
*                                                                      *
********************************************************************* -}

---------------------------------
-- | Compare types for equality, while zonking as necessary. Gives up
-- as soon as it finds that two types are not equal.
-- This is quite handy when some unification has made two
-- types in an inert Wanted to be equal. We can discover the equality without
-- rewriting, which is sometimes very expensive (in the case of type functions).
-- In particular, this function makes a ~20% improvement in test case
-- perf/compiler/T5030.
--
-- Returns either the (partially zonked) types in the case of
-- inequality, or the one type in the case of equality. canEqReflexive is
-- a good next step in the 'Right' case. Returning 'Left' is always safe.
--
-- NB: This does *not* look through type synonyms. In fact, it treats type
-- synonyms as rigid constructors. In the future, it might be convenient
-- to look at only those arguments of type synonyms that actually appear
-- in the synonym RHS. But we're not there yet.
zonkEqTypes :: CtEvidence -> EqRel -> TcType -> TcType -> SolverStage (Pair TcType)
zonkEqTypes ev eq_rel ty1 ty2
  = Stage $ do { res <- go ty1 ty2
               ; case res of
                    Left pair -> continueWith pair
                    Right ty  -> canEqReflexive ev eq_rel ty }
  where
    go :: TcType -> TcType -> TcS (Either (Pair TcType) TcType)
    go (TyVarTy tv1) (TyVarTy tv2) = tyvar_tyvar tv1 tv2
    go (TyVarTy tv1) ty2           = tyvar NotSwapped tv1 ty2
    go ty1 (TyVarTy tv2)           = tyvar IsSwapped  tv2 ty1

    -- We handle FunTys explicitly here despite the fact that they could also be
    -- treated as an application. Why? Well, for one it's cheaper to just look
    -- at two types (the argument and result types) than four (the argument,
    -- result, and their RuntimeReps). Also, we haven't completely zonked yet,
    -- so we may run into an unzonked type variable while trying to compute the
    -- RuntimeReps of the argument and result types. This can be observed in
    -- testcase tc269.
    go (FunTy mods1 arg1 res1) (FunTy mods2 arg2 res2)
      | (w1,af1) <- ftm_mods mods1, (w2,af2) <- ftm_mods mods2
      , af1 == af2
      , eqType w1 w2
      = do { res_a <- go arg1 arg2
           ; res_b <- go res1 res2
           ; return $ combine_rev (FunTy (mkFtMods w1 af1)) res_b res_a }

    go ty1@(FunTy {}) ty2 = bale_out ty1 ty2
    go ty1 ty2@(FunTy {}) = bale_out ty1 ty2

    go ty1 ty2
      | Just (tc1, tys1) <- splitTyConAppNoView_maybe ty1
      , Just (tc2, tys2) <- splitTyConAppNoView_maybe ty2
      = if tc1 == tc2 && tys1 `equalLength` tys2
          -- Crucial to check for equal-length args, because
          -- we cannot assume that the two args to 'go' have
          -- the same kind.  E.g go (Proxy *      (Maybe Int))
          --                        (Proxy (*->*) Maybe)
          -- We'll call (go (Maybe Int) Maybe)
          -- See #13083
        then tycon tc1 tys1 tys2
        else bale_out ty1 ty2

    go ty1 ty2
      | Just (ty1a, ty1b) <- tcSplitAppTyNoView_maybe ty1
      , Just (ty2a, ty2b) <- tcSplitAppTyNoView_maybe ty2
      = do { res_a <- go ty1a ty2a
           ; res_b <- go ty1b ty2b
           ; return $ combine_rev mkAppTy res_b res_a }

    go ty1@(LitTy lit1) (LitTy lit2)
      | lit1 == lit2
      = return (Right ty1)

    go ty1 ty2 = bale_out ty1 ty2
      -- We don't handle more complex forms here

    bale_out ty1 ty2 = return $ Left (Pair ty1 ty2)

    tyvar :: SwapFlag -> TcTyVar -> TcType
          -> TcS (Either (Pair TcType) TcType)
      -- Try to do as little as possible, as anything we do here is redundant
      -- with rewriting. In particular, no need to zonk kinds. That's why
      -- we don't use the already-defined zonking functions
    tyvar swapped tv ty
      = case tcTyVarDetails tv of
          MetaTv { mtv_ref = ref }
            -> do { cts <- readTcRef ref
                  ; case cts of
                      Flexi        -> give_up
                      Indirect ty' -> do { trace_indirect tv ty'
                                         ; unSwap swapped go ty' ty } }
          _ -> give_up
      where
        give_up = return $ Left $ unSwap swapped Pair (mkTyVarTy tv) ty

    tyvar_tyvar tv1 tv2
      | tv1 == tv2 = return (Right (mkTyVarTy tv1))
      | otherwise  = do { (ty1', progress1) <- quick_zonk tv1
                        ; (ty2', progress2) <- quick_zonk tv2
                        ; if progress1 || progress2
                          then go ty1' ty2'
                          else return $ Left (Pair (TyVarTy tv1) (TyVarTy tv2)) }

    trace_indirect tv ty
       = traceTcS "Following filled tyvar (zonk_eq_types)"
                  (ppr tv <+> equals <+> ppr ty)

    quick_zonk tv = case tcTyVarDetails tv of
      MetaTv { mtv_ref = ref }
        -> do { cts <- readTcRef ref
              ; case cts of
                  Flexi        -> return (TyVarTy tv, False)
                  Indirect ty' -> do { trace_indirect tv ty'
                                     ; return (ty', True) } }
      _ -> return (TyVarTy tv, False)

      -- This happens for type families, too. But recall that failure
      -- here just means to try harder, so it's OK if the type function
      -- isn't injective.
    tycon :: TyCon -> [TcType] -> [TcType]
          -> TcS (Either (Pair TcType) TcType)
    tycon tc tys1 tys2
      = do { results <- zipWithM go tys1 tys2
           ; return $ case combine_results results of
               Left tys  -> Left (mkTyConApp tc <$> tys)
               Right tys -> Right (mkTyConApp tc tys) }

    combine_results :: [Either (Pair TcType) TcType]
                    -> Either (Pair [TcType]) [TcType]
    combine_results = bimap (fmap reverse) reverse .
                      foldl' (combine_rev (:)) (Right [])

      -- combine (in reverse) a new result onto an already-combined result
    combine_rev :: (a -> b -> c)
                -> Either (Pair b) b
                -> Either (Pair a) a
                -> Either (Pair c) c
    combine_rev f (Left list) (Left elt) = Left (f <$> elt     <*> list)
    combine_rev f (Left list) (Right ty) = Left (f <$> pure ty <*> list)
    combine_rev f (Right tys) (Left elt) = Left (f <$> elt     <*> pure tys)
    combine_rev f (Right tys) (Right ty) = Right (f ty tys)


{- *********************************************************************
*                                                                      *
*           canonicaliseEquality
*                                                                      *
********************************************************************* -}

canonicaliseEquality
   :: CtEvidence -> EqRel
   -> Type -> Type     -- LHS and RHS
   -> SolverStage (Either IrredCt EqCt)
-- Nota Bene: `ty1` and `ty2` may be more-zonked than `ev`
-- This matters when calling mkSelCo, in canDecomposableTyConAppOK and
--   canDecomposableFunTy, when we need the precondition of mkSelCo to
--   hold.

canonicaliseEquality ev eq_rel ty1 ty2
  = Stage $ do { traceTcS "canonicaliseEquality" $
                 vcat [ ppr ev, ppr eq_rel, ppr ty1, ppr ty2 ]
               ; rdr_env   <- getGlobalRdrEnvTcS
               ; fam_insts <- getFamInstEnvs
               ; can_eq_nc False rdr_env fam_insts ev eq_rel ty1 ty1 ty2 ty2 }

can_eq_nc
   :: Bool           -- True => both input types are rewritten
   -> GlobalRdrEnv   -- needed to see which newtypes are in scope
   -> FamInstEnvs    -- needed to unwrap data instances
   -> CtEvidence
   -> EqRel
   -> Type -> Type    -- LHS, after and before type-synonym expansion, resp
   -> Type -> Type    -- RHS, after and before type-synonym expansion, resp
   -> TcS (StopOrContinue (Either IrredCt EqCt))

-- See Note [Unifying type synonyms] in GHC.Core.Unify
can_eq_nc _flat _rdr_env _envs ev eq_rel ty1@(TyConApp tc1 []) _ps_ty1 (TyConApp tc2 []) _ps_ty2
  | tc1 == tc2
  = canEqReflexive ev eq_rel ty1

-- Expand synonyms first; see Note [Type synonyms and canonicalization]
can_eq_nc rewritten rdr_env envs ev eq_rel ty1 ps_ty1 ty2 ps_ty2
  | Just ty1' <- coreView ty1 = can_eq_nc rewritten rdr_env envs ev eq_rel ty1' ps_ty1 ty2  ps_ty2
  | Just ty2' <- coreView ty2 = can_eq_nc rewritten rdr_env envs ev eq_rel ty1  ps_ty1 ty2' ps_ty2

-- need to check for reflexivity in the ReprEq case.
-- See Note [Eager reflexivity check]
-- Check only when rewritten because the zonk_eq_types check in canEqNC takes
-- care of the non-rewritten case.
can_eq_nc True _rdr_env _envs ev ReprEq ty1 _ ty2 _
  | ty1 `tcEqType` ty2
  = canEqReflexive ev ReprEq ty1

-- When working with ReprEq, unwrap newtypes.
-- See Note [Unwrap newtypes first]
-- This must be above the TyVarTy case, in order to guarantee (TyEq:N)
can_eq_nc _rewritten rdr_env envs ev eq_rel ty1 ps_ty1 ty2 ps_ty2
  | ReprEq <- eq_rel
  , Just stuff1 <- tcTopNormaliseNewTypeTF_maybe envs rdr_env ty1
  = can_eq_newtype_nc rdr_env envs ev NotSwapped ty1 stuff1 ty2 ps_ty2

  | ReprEq <- eq_rel
  , Just stuff2 <- tcTopNormaliseNewTypeTF_maybe envs rdr_env ty2
  = can_eq_newtype_nc rdr_env envs ev IsSwapped ty2 stuff2 ty1 ps_ty1

-- Then, get rid of casts
can_eq_nc rewritten rdr_env envs ev eq_rel (CastTy ty1 co1) _ ty2 ps_ty2
  | isNothing (canEqLHS_maybe ty2)  -- See (EIK3) in Note [Equalities with incompatible kinds]
  = canEqCast rewritten rdr_env envs ev eq_rel NotSwapped ty1 co1 ty2 ps_ty2
can_eq_nc rewritten rdr_env envs ev eq_rel ty1 ps_ty1 (CastTy ty2 co2) _
  | isNothing (canEqLHS_maybe ty1)  -- See (EIK3) in Note [Equalities with incompatible kinds]
  = canEqCast rewritten rdr_env envs ev eq_rel IsSwapped ty2 co2 ty1 ps_ty1

----------------------
-- Otherwise try to decompose
----------------------

-- Literals
can_eq_nc _rewritten _rdr_env _envs ev eq_rel ty1@(LitTy l1) _ (LitTy l2) _
 | l1 == l2
  = do { setEvBindIfWanted ev EvCanonical (evCoercion $ mkReflCo (eqRelRole eq_rel) ty1)
       ; stopWith ev "Equal LitTy" }

-- Decompose FunTy: (s -> t) and (c => t)
-- NB: don't decompose (Int -> blah) ~ (Show a => blah)
can_eq_nc _rewritten _rdr_env _envs ev eq_rel
           ty1@(FunTy { ft_mods = mods1, ft_arg = ty1a, ft_res = ty1b }) _ps_ty1
           ty2@(FunTy { ft_mods = mods2, ft_arg = ty2a, ft_res = ty2b }) _ps_ty2
  | (am1,af1) <- ftm_mods mods1, (am2,af2) <- ftm_mods mods2
  , af1 == af2  -- See Note [Decomposing FunTy]
  = canDecomposableFunTy ev eq_rel af1 (ty1,am1,ty1a,ty1b) (ty2,am2,ty2a,ty2b)

-- Decompose type constructor applications
-- NB: we have expanded type synonyms already
can_eq_nc rewritten _rdr_env _envs ev eq_rel ty1 _ ty2 _
  | Just (tc1, tys1) <- tcSplitTyConApp_maybe ty1
  , Just (tc2, tys2) <- tcSplitTyConApp_maybe ty2
   -- tcSplitTyConApp_maybe: we want to catch e.g. Maybe Int ~ (Int -> Int)
   -- here for better error messages rather than decomposing into AppTys;
   -- hence not using a direct match on TyConApp

  , not (isTypeFamilyTyCon tc1 || isTypeFamilyTyCon tc2)
    -- A type family at the top of LHS or RHS: we want to fall through
    -- to the canonical-LHS cases (look for canEqLHS_maybe)

  -- See (TC1) in Note [Canonicalising TyCon/TyCon equalities]
  , let role            = eqRelRole eq_rel
        both_generative = isGenerativeTyCon tc1 role && isGenerativeTyCon tc2 role
  , rewritten || both_generative
  = canTyConApp ev eq_rel both_generative (ty1,tc1,tys1) (ty2,tc2,tys2)

can_eq_nc _rewritten _rdr_env _envs ev eq_rel
           s1@ForAllTy{} _
           s2@ForAllTy{} _
  = can_eq_nc_forall ev eq_rel s1 s2

-- See Note [Canonicalising type applications] about why we require rewritten types
-- Use tcSplitAppTy, not matching on AppTy, to catch oversaturated type families
-- NB: Only decompose AppTy for nominal equality.
--     See Note [Decomposing AppTy equalities]
can_eq_nc True _rdr_env _envs ev NomEq ty1 _ ty2 _
  | Just (t1, s1) <- tcSplitAppTy_maybe ty1
  , Just (t2, s2) <- tcSplitAppTy_maybe ty2
  = can_eq_app ev t1 s1 t2 s2

-------------------
-- Can't decompose.
-------------------

-- No similarity in type structure detected. Rewrite and try again.
can_eq_nc False rdr_env envs ev eq_rel _ ps_ty1 _ ps_ty2
  = -- Rewrite the two types and try again
    do { (redn1@(Reduction _ xi1), rewriters1) <- rewrite ev ps_ty1
       ; (redn2@(Reduction _ xi2), rewriters2) <- rewrite ev ps_ty2
       ; new_ev <- rewriteEqEvidence (rewriters1 S.<> rewriters2) ev NotSwapped redn1 redn2
       ; traceTcS "can_eq_nc: go round again" (ppr new_ev $$ ppr xi1 $$ ppr xi2)
       ; can_eq_nc True rdr_env envs new_ev eq_rel xi1 xi1 xi2 xi2 }

----------------------------
-- Look for a canonical LHS.
-- Only rewritten types end up below here.
----------------------------

-- NB: pattern match on rewritten=True: we want only rewritten types sent to canEqLHS
-- This means we've rewritten any variables and reduced any type family redexes
-- See also Note [No top-level newtypes on RHS of representational equalities]

can_eq_nc True _rdr_env _envs ev eq_rel ty1 ps_ty1 ty2 ps_ty2
  | Just can_eq_lhs1 <- canEqLHS_maybe ty1
  = do { traceTcS "can_eq1" (ppr ty1 $$ ppr ty2)
       ; canEqCanLHS ev eq_rel NotSwapped can_eq_lhs1 ps_ty1 ty2 ps_ty2 }

  | Just can_eq_lhs2 <- canEqLHS_maybe ty2
  = do { traceTcS "can_eq2" (ppr ty1 $$ ppr ty2)
       ; canEqCanLHS ev eq_rel IsSwapped can_eq_lhs2 ps_ty2 ty1 ps_ty1 }

     -- If the type is TyConApp tc1 args1, then args1 really can't be less
     -- than tyConArity tc1. It could be *more* than tyConArity, but then we
     -- should have handled the case as an AppTy. That case only fires if
     -- _both_ sides of the equality are AppTy-like... but if one side is
     -- AppTy-like and the other isn't (and it also isn't a variable or
     -- saturated type family application, both of which are handled by
     -- can_eq_nc), we're in a failure mode and can just fall through.

----------------------------
-- Fall-through. Give up.
----------------------------

-- We've rewritten and the types don't match. Give up.
can_eq_nc True _rdr_env _envs ev eq_rel _ ps_ty1 _ ps_ty2
  = do { traceTcS "can_eq_nc catch-all case" (ppr ps_ty1 $$ ppr ps_ty2)
       ; case eq_rel of -- See Note [Unsolved equalities]
            ReprEq -> finishCanWithIrred ReprEqReason ev
            NomEq  -> finishCanWithIrred ShapeMismatchReason ev }
          -- No need to call canEqSoftFailure/canEqHardFailure because they
          -- rewrite, and the types involved here are already rewritten


{- Note [Unsolved equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have an unsolved equality like
  (a b ~R# Int)
that is not necessarily insoluble!  Maybe 'a' will turn out to be a newtype.
So we want to make it a potentially-soluble Irred not an insoluble one.
Missing this point is what caused #15431
-}

---------------------------------
can_eq_nc_forall :: CtEvidence -> EqRel
                 -> Type -> Type    -- LHS and RHS
                 -> TcS (StopOrContinue (Either IrredCt EqCt))
-- See Note [Solving forall equalities]

can_eq_nc_forall ev eq_rel s1 s2
 | CtWanted (WantedCt { ctev_dest = orig_dest }) <- ev
 = do { let (bndrs1, phi1, bndrs2, phi2) = split_foralls s1 s2
            flags1 = binderFlags bndrs1
            flags2 = binderFlags bndrs2

      ; if eq_rel == NomEq && not (all2 eqForAllVis flags1 flags2) -- Note [ForAllTy and type equality]
        then do { traceTcS "Forall failure: visibility-mismatch" $
                     vcat [ ppr s1, ppr s2, ppr bndrs1, ppr bndrs2
                          , ppr flags1, ppr flags2 ]
                ; canEqHardFailure ev s1 s2 }

        else do {
        traceTcS "Creating implication for polytype equality" (ppr ev)
      ; let free_tvs     = tyCoVarsOfTypes [s1,s2]
            empty_subst1 = mkEmptySubst $ mkInScopeSet free_tvs
      ; skol_info <- mkSkolemInfo (UnifyForAllSkol phi1)
      ; (subst1, skol_tvs) <- tcInstSkolTyVarsX skol_info empty_subst1 $
                              binderVars bndrs1

      ; let phi1' = substTy subst1 phi1

            -- Unify the kinds, extend the substitution
            go :: UnifyEnv -> [TcTyVar] -> Subst
               -> [TyVarBinder] -> [TyVarBinder] -> TcM.TcM TcCoercion
            go uenv (skol_tv:skol_tvs) subst2 (bndr1:bndrs1) (bndr2:bndrs2)
              = do { let tv2  = binderVar bndr2
                         vis1 = binderFlag bndr1
                         vis2 = binderFlag bndr2

                   -- Unify the kinds, at Nominal role
                   -- See (SF1) in Note [Solving forall equalities]
                   ; kind_co <- uType (uenv `setUEnvRole` Nominal)
                                      (tyVarKind skol_tv)
                                      (substTy subst2 (tyVarKind tv2))

                   ; let subst2' = extendTvSubstAndInScope subst2 tv2
                                       (mkCastTy (mkTyVarTy skol_tv) kind_co)
                         -- skol_tv is already in the in-scope set, but the
                         -- free vars of kind_co are not; hence "...AndInScope"
                   ; co <- go uenv skol_tvs subst2' bndrs1 bndrs2

                   ; return (mkNakedForAllCo skol_tv vis1 vis2 kind_co co)}
                     -- mkNaked.. because these types are not zonked, and the
                     -- assertions in mkForAllCo may fail without that zonking

            -- Done: unify phi1 ~ phi2
            go uenv [] subst2 bndrs1 bndrs2
              = assert (null bndrs1 && null bndrs2) $
                uType uenv phi1' (substTyUnchecked subst2 phi2)

            go _ _ _ _ _ = panic "can_eq_nc_forall"  -- case (s:ss) []

            init_subst2 = mkEmptySubst (substInScopeSet subst1)

      -- Generate the constraints that live in the body of the implication
      -- See (SF5) in Note [Solving forall equalities]
      ; (lvl, (all_co, wanteds)) <- pushLevelNoWorkList (ppr skol_info)   $
                                    unifyForAllBody ev (eqRelRole eq_rel) $ \uenv ->
                                    go uenv skol_tvs init_subst2 bndrs1 bndrs2

      ; emitTvImplicationTcS lvl (getSkolemInfo skol_info) skol_tvs wanteds

      ; setWantedEq orig_dest all_co
      ; stopWith ev "Deferred polytype equality" } }

 | otherwise
 = do { traceTcS "Omitting decomposition of given polytype equality" $
        pprEq s1 s2    -- See Note [Do not decompose Given polytype equalities]
      ; stopWith ev "Discard given polytype equality" }

 where
    split_foralls :: TcType -> TcType
                  -> ( [ForAllTyBinder], TcType
                     , [ForAllTyBinder], TcType)
    -- Split matching foralls; stop when the foralls don't match
    -- See #22537.  See (SF3) in Note [Solving forall equalities]
    -- Postcondition: the two lists of binders returned have the same length
    split_foralls s1 s2
      | Just (bndr1, s1') <- splitForAllForAllTyBinder_maybe s1
      , Just (bndr2, s2') <- splitForAllForAllTyBinder_maybe s2
      = let !(bndrs1, phi1, bndrs2, phi2) = split_foralls s1' s2'
        in (bndr1:bndrs1, phi1, bndr2:bndrs2, phi2)
    split_foralls s1 s2 = ([], s1, [], s2)

{- Note [Solving forall equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To solve an equality between foralls
   [W] (forall a. t1) ~ (forall b. t2)
the basic plan is simple: just create the implication constraint
   [W] forall a. { t1 ~ (t2[a/b]) }

The evidence we produce is a ForAllCo; see the typing rule for
ForAllCo in Note [ForAllCo] in GHC.Tc.TyCo.Rep.

There are lots of wrinkles of course:

(SF1) We must check the kinds match (at Nominal role).  So from
      [W] (forall (a:ka). t1) ~ (forall (b:kb). t2)
   we actually generate the implication
      [W] forall (a:ka). { ka ~N kb,  t1 ~ t2[a/b] }
   These kind equalities are generated by the `go` loop in `can_eq_nc_forall`.
   Why Nominal role? See the typing rule for ForAllCo.

(SF2) At Nominal role we must check that the visiblities match.
  For example
     [W] (forall a. a -> a) ~N  (forall b -> b -> b)
  should fail.  At /Representational/ role we allow this; see the
  typing rule for ForAllCo, mentioned above.

(SF3) Consider this (#22537)
     newtype P a = MkP (forall c. (a,c))
     [W] (forall a. P a) ~R (forall a b. (a,b))
  The number of foralls does not line up.  But if we just unwrap the outer
  forall a, we'll get
     [W] P a ~R forall b. (a,b)
  Now unwrap the newtype
     [W] (forall c. (a,c)) ~R (forall b. (a,b))
  and all is good.

  Conclusion: Don't fail if the number of foralls does not line up.  Instead,
  handle as many binders as are visibly apparent on both sides, and then keep
  going with unification. See `split_foralls` in `can_eq_nc_forall`.  In the
  above example, at Representational role, the unifier will proceed to unwrap
  the newtype on the RHS and we'll end up back in can_eq_nc_forall.

(SF4) Remember also that we might have forall z (a:z). blah
  so in that `go` loop, we must proceed one binder at a time (#13879)

(SF5) Rather than manually gather the constraints needed in the body of the
   implication, we use `uType`.  That way we can solve some of them on the fly,
   especially Refl ones.  We use the `unifyForAllBody` wrapper for `uType`,
   because we want to /gather/ the equality constraint (to put in the implication)
   rather than /emit/ them into the monad, as `wrapUnifierTcS` does.
-}

{- Note [Unwrap newtypes first]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also Note [Decomposing newtype equalities]

Consider
  newtype N m a = MkN (m a)
N will get a conservative, Nominal role for its second parameter 'a',
because it appears as an argument to the unknown 'm'. Now consider
  [W] N Maybe a  ~R#  N Maybe b

If we /decompose/, we'll get
  [W] a ~N# b

But if instead we /unwrap/ we'll get
  [W] Maybe a ~R# Maybe b
which in turn gives us
  [W] a ~R# b
which is easier to satisfy.

Conclusion: we must unwrap newtypes before decomposing them. This happens
in `can_eq_newtype_nc`

We did flirt with making the /rewriter/ expand newtypes, rather than
doing it in `can_eq_newtype_nc`.   But with recursive newtypes we want
to be super-careful about expanding!

   newtype A = MkA [A]   -- Recursive!

   f :: A -> [A]
   f = coerce

We have [W] A ~R# [A].  If we rewrite [A], it'll expand to
   [[[[[...]]]]]
and blow the reduction stack.  See Note [Newtypes can blow the stack]
in GHC.Tc.Solver.Rewrite.  But if we expand only the /top level/ of
both sides, we get
   [W] [A] ~R# [A]
which we can, just, solve by reflexivity.

So we simply unwrap, on-demand, at top level, in `can_eq_newtype_nc`.

This is all very delicate. There is a real risk of a loop in the type checker
with recursive newtypes -- but I think we're doomed to do *something*
delicate, as we're really trying to solve for equirecursive type
equality. Bottom line for users: recursive newtypes do not play well with type
inference for representational equality.  See also Section 5.3.1 and 5.3.4 of
"Safe Zero-cost Coercions for Haskell" (JFP 2016).

See also Note [Decomposing newtype equalities].

--- Historical side note ---

We flirted with doing /both/ unwrap-at-top-level /and/ rewrite-deeply;
see #22519.  But that didn't work: see discussion in #22924. Specifically
we got a loop with a minor variation:
   f2 :: a -> [A]
   f2 = coerce

Note [Eager reflexivity check]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have

  newtype X = MkX (Int -> X)

and

  [W] X ~R X

Naively, we would start unwrapping X and end up in a loop. Instead,
we do this eager reflexivity check. This is necessary only for representational
equality because the rewriter technology deals with the similar case
(recursive type families) for nominal equality.

Note that this check does not catch all cases, but it will catch the cases
we're most worried about, types like X above that are actually inhabited.

Here's another place where this reflexivity check is key:
Consider trying to prove (f a) ~R (f a). The AppTys in there can't
be decomposed, because representational equality isn't congruent with respect
to AppTy. So, when canonicalising the equality above, we get stuck and
would normally produce a CIrredCan. However, we really do want to
be able to solve (f a) ~R (f a). So, in the representational case only,
we do a reflexivity check.

(This would be sound in the nominal case, but unnecessary, and I [Richard
E.] am worried that it would slow down the common case.)

 Note [Newtypes can blow the stack]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have

  newtype X = MkX (Int -> X)
  newtype Y = MkY (Int -> Y)

and now wish to prove

  [W] X ~R Y

This Wanted will loop, expanding out the newtypes ever deeper looking
for a solid match or a solid discrepancy. Indeed, there is something
appropriate to this looping, because X and Y *do* have the same representation,
in the limit -- they're both (Fix ((->) Int)). However, no finitely-sized
coercion will ever witness it. This loop won't actually cause GHC to hang,
though, because we check our depth in `can_eq_newtype_nc`.
-}

------------------------
-- | We're able to unwrap a newtype. Update the bits accordingly.
can_eq_newtype_nc :: GlobalRdrEnv -> FamInstEnvs
                  -> CtEvidence           -- ^ :: ty1 ~ ty2
                  -> SwapFlag
                  -> TcType                                    -- ^ ty1
                  -> ((Bag GlobalRdrElt, TcCoercion), TcType)  -- ^ :: ty1 ~ ty1'
                  -> TcType               -- ^ ty2
                  -> TcType               -- ^ ty2, with type synonyms
                  -> TcS (StopOrContinue (Either IrredCt EqCt))
can_eq_newtype_nc rdr_env envs ev swapped ty1 ((gres, co1), ty1') ty2 ps_ty2
  = do { traceTcS "can_eq_newtype_nc" $
         vcat [ ppr ev, ppr swapped, ppr co1, ppr gres, ppr ty1', ppr ty2 ]

         -- Check for blowing our stack, and increase the depth
         -- See Note [Newtypes can blow the stack]
       ; let loc = ctEvLoc ev
             ev' = ev `setCtEvLoc` bumpCtLocDepth loc
       ; checkReductionDepth loc ty1

         -- Next, we record uses of newtype constructors, since coercing
         -- through newtypes is tantamount to using their constructors.
       ; recordUsedGREs gres

       ; let redn1 = mkReduction co1 ty1'

       ; new_ev <- rewriteEqEvidence emptyRewriterSet ev' swapped
                     redn1 (mkReflRedn Representational ps_ty2)

       ; can_eq_nc False rdr_env envs new_ev ReprEq ty1' ty1' ty2 ps_ty2 }

---------
-- ^ Decompose a type application.
-- All input types must be rewritten. See Note [Canonicalising type applications]
-- Nominal equality only!
can_eq_app :: CtEvidence       -- :: s1 t1 ~N s2 t2
           -> Xi -> Xi         -- s1 t1
           -> Xi -> Xi         -- s2 t2
           -> TcS (StopOrContinue (Either IrredCt EqCt))

-- AppTys only decompose for nominal equality, so this case just leads
-- to an irreducible constraint; see typecheck/should_compile/T10494
-- See Note [Decomposing AppTy equalities]
can_eq_app ev s1 t1 s2 t2
  | CtWanted (WantedCt { ctev_dest = dest }) <- ev
  = do { traceTcS "can_eq_app" (vcat [ text "s1:" <+> ppr s1, text "t1:" <+> ppr t1
                                     , text "s2:" <+> ppr s2, text "t2:" <+> ppr t2
                                     , text "vis:" <+> ppr (isNextArgVisible s1) ])
       ; (co,_,_) <- wrapUnifierTcS ev Nominal $ \uenv ->
            -- Unify arguments t1/t2 before function s1/s2, because
            -- the former have smaller kinds, and hence simpler error messages
            -- c.f. GHC.Tc.Utils.Unify.uType (go_app)
            do { let arg_env = updUEnvLoc uenv (adjustCtLoc (isNextArgVisible s1) False)
               ; co_t <- uType arg_env t1 t2
               ; co_s <- uType uenv s1 s2
               ; return (mkAppCo co_s co_t) }
       ; setWantedEq dest co
       ; stopWith ev "Decomposed [W] AppTy" }

    -- If there is a ForAll/(->) mismatch, the use of the Left coercion
    -- below is ill-typed, potentially leading to a panic in splitTyConApp
    -- Test case: typecheck/should_run/Typeable1
    -- We could also include this mismatch check above (for W and D), but it's slow
    -- and we'll get a better error message not doing it
  | s1k `mismatches` s2k
  = canEqHardFailure ev (s1 `mkAppTy` t1) (s2 `mkAppTy` t2)

  | CtGiven (GivenCt { ctev_evar = evar }) <- ev
  = do { let co   = mkCoVarCo evar
             co_s = mkLRCo CLeft  co
             co_t = mkLRCo CRight co
       ; evar_s <- newGivenEvVar loc ( mkTcEqPredLikeEv ev s1 s2
                                     , evCoercion co_s )
       ; evar_t <- newGivenEvVar loc ( mkTcEqPredLikeEv ev t1 t2
                                     , evCoercion co_t )
       ; emitWorkNC [CtGiven evar_t]
       ; startAgainWith (mkNonCanonical $ CtGiven evar_s) }

  where
    loc = ctEvLoc ev

    s1k = typeKind s1
    s2k = typeKind s2

    k1 `mismatches` k2
      =  isForAllTy k1 && not (isForAllTy k2)
      || not (isForAllTy k1) && isForAllTy k2

-----------------------
-- | Break apart an equality over a casted type
-- looking like   (ty1 |> co1) ~ ty2   (modulo a swap-flag)
canEqCast :: Bool         -- are both types rewritten?
          -> GlobalRdrEnv -> FamInstEnvs
          -> CtEvidence
          -> EqRel
          -> SwapFlag
          -> TcType -> Coercion   -- LHS (res. RHS), ty1 |> co1
          -> TcType -> TcType     -- RHS (res. LHS), ty2 both normal and pretty
          -> TcS (StopOrContinue (Either IrredCt EqCt))
canEqCast rewritten rdr_env envs ev eq_rel swapped ty1 co1 ty2 ps_ty2
  = do { traceTcS "Decomposing cast" (vcat [ ppr ev
                                           , ppr ty1 <+> text "|>" <+> ppr co1
                                           , ppr ps_ty2 ])
       ; new_ev <- rewriteEqEvidence emptyRewriterSet ev swapped
                      (mkGReflLeftRedn role ty1 co1)
                      (mkReflRedn role ps_ty2)
       ; can_eq_nc rewritten rdr_env envs new_ev eq_rel ty1 ty1 ty2 ps_ty2 }
  where
    role = eqRelRole eq_rel

------------------------
canTyConApp :: CtEvidence -> EqRel
            -> Bool  -- True <=> both TyCons are generative
            -> (Type,TyCon,[TcType])
            -> (Type,TyCon,[TcType])
            -> TcS (StopOrContinue (Either IrredCt EqCt))
-- See Note [Decomposing TyConApp equalities]
-- Neither tc1 nor tc2 is a saturated funTyCon, nor a type family
-- But they can be data families.
canTyConApp ev eq_rel both_generative (ty1,tc1,tys1) (ty2,tc2,tys2)
  | tc1 == tc2
  , tys1 `equalLength` tys2
  = do { inerts <- getInertSet
       ; if can_decompose inerts
         then canDecomposableTyConAppOK ev eq_rel tc1 (ty1,tys1) (ty2,tys2)
         else canEqSoftFailure ev eq_rel ty1 ty2 }

  -- See Note [Skolem abstract data] in GHC.Core.Tycon
  | tyConSkolem tc1 || tyConSkolem tc2
  = do { traceTcS "canTyConApp: skolem abstract" (ppr tc1 $$ ppr tc2)
       ; finishCanWithIrred AbstractTyConReason ev }

  | otherwise  -- Different TyCons
  = if both_generative -- See (TC2) and (TC3) in
                       -- Note [Canonicalising TyCon/TyCon equalities]
    then canEqHardFailure ev ty1 ty2
    else canEqSoftFailure ev eq_rel ty1 ty2
  where
     -- See Note [Decomposing TyConApp equalities]
     -- and Note [Decomposing newtype equalities]
    can_decompose inerts
      =  isInjectiveTyCon tc1 (eqRelRole eq_rel)
      || (assert (eq_rel == ReprEq) $
          -- assert: isInjectiveTyCon is always True for Nominal except
          --   for type synonyms/families, neither of which happen here
          -- Moreover isInjectiveTyCon is True for Representational
          --   for algebraic data types.  So we are down to newtypes
          --   and data families.
          ctEvFlavour ev == Wanted && noGivenNewtypeReprEqs tc1 inerts)
             -- See Note [Decomposing newtype equalities] (EX2)

{- Note [Canonicalising TyCon/TyCon equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

  type family TF a where TF Char = Bool
  data family DF a
  newtype instance DF Bool = MkDF Int

Suppose we are canonicalising [W] Int ~R# DF (TF a).  Then

(TC1) We might have an inert Given (a ~# Char), so if we rewrote the wanted
      (i.e. went around again in `can_eq_nc` with `rewritten`=True, we'd get
         [W] Int ~R# DF Bool
      and then the `tcTopNormaliseNewTypeTF_maybe` call would fire and
      we'd unwrap the newtype.  So we must do that "go round again" bit.
      Hence the complicated guard (rewritten || both_generative) in `can_eq_nc`.

(TC2) If we can't rewrite `a` yet, we'll finish with an unsolved
         [W] Int ~R# DF (TF a)
      in the inert set. But we must use canEqSoftFailure, not canEqHardFailure,
      because it might be solved "later" when we learn more about `a`.
      Hence the use of `both_generative` in `canTyConApp`.

(TC3) Here's another example:
         [G] Age ~R# Int
      where Age's constructor is not in scope. We don't want to report
      an "inaccessible code" error in the context of this Given!  So again
      we want `canEqSoftFailure`.

      For example, see typecheck/should_compile/T10493, repeated here:
        import Data.Ord (Down)  -- no constructor
        foo :: Coercible (Down Int) Int => Down Int -> Int
        foo = coerce

      That should compile, but only because we use canEqSoftFailure and
      not canEqHardFailure.

Note [Fast path when decomposing TyConApps]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we see (T s1 t1 ~ T s2 t2), then we can just decompose to
  (s1 ~ s2, t1 ~ t2)
and push those back into the work list.  But if
  s1 = K k1    s2 = K k2
then we will just decompose s1~s2, and it might be better to
do so on the spot.  An important special case is where s1=s2,
and we get just Refl.

So canDecomposableTyConAppOK uses wrapUnifierTcS etc to short-cut
that work.  See also Note [Work-list ordering].

Note [Decomposing TyConApp equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
        [G/W] T ty1 ~r T ty2
Can we decompose it, and replace it by
        [G/W] ty1 ~r' ty2
and if so what role is r'?  (In this Note, all the "~" are primitive
equalities "~#", but I have dropped the noisy "#" symbols.)  Lots of
background in the paper "Safe zero-cost coercions for Haskell".

This Note covers the topic for
  * Datatypes
  * Newtypes
  * Data families
For the rest:
  * Type synonyms: are always expanded
  * Type families: see Note [Decomposing type family applications]
  * AppTy:         see Note [Decomposing AppTy equalities].

---- Roles of the decomposed constraints ----
For a start, the role r' will always be defined like this:
  * If r=N then r' = N
  * If r=R then r' = role of T's first argument

For example:
   data TR a = MkTR a       -- Role of T's first arg is Representational
   data TN a = MkTN (F a)   -- Role of T's first arg is Nominal

The function tyConRolesX :: Role -> TyCon -> [Role] gets the argument
role r' for a TyCon T at role r.  E.g.
   tyConRolesX Nominal          TR = [Nominal]
   tyConRolesX Representational TR = [Representational]

---- Soundness and completeness ----
For Givens, for /soundness/ of decomposition we need, forall ty1,ty2:
    T ty1 ~r T ty2   ===>    ty1 ~r' ty2
Here "===>" means "implies".  That is, given evidence for (co1 : T ty1 ~r T co2)
we can produce evidence for (co2 : ty1 ~r' ty2).  But in the solver we
/replace/ co1 with co2 in the inert set, and we don't want to lose any proofs
thereby. So for /completeness/ of decomposition we also need the reverse:
    ty1 ~r' ty2   ===>    T ty1 ~r T ty2

For Wanteds, for /soundness/ of decomposition we need:
    ty1 ~r' ty2   ===>    T ty1 ~r T ty2
because if we do decompose we'll get evidence (co2 : ty1 ~r' ty2) and
from that we want to derive evidence (T co2 : T ty1 ~r T ty2).
For /completeness/ of decomposition we need the reverse implication too,
else we may decompose to a new proof obligation that is stronger than
the one we started with.  See Note [Decomposing newtype equalities].

---- Injectivity ----
When do these bi-implications hold? In one direction it is easy.
We /always/ have
    ty1 ~r'  ty2   ===>    T ty1 ~r T ty2
This is the CO_TYCONAPP rule of the paper (Fig 5); see also the
TyConAppCo case of GHC.Core.Lint.lintCoercion.

In the other direction, we have
    T ty1 ~r T ty2   ==>   ty1 ~r' ty2  if T is /injective at role r/
This is the very /definition/ of injectivity: injectivity means result
is the same => arguments are the same, modulo the role shift.
See comments on GHC.Core.TyCon.isInjectiveTyCon.  This is also
the CO_NTH rule in Fig 5 of the paper, except in the paper only
newtypes are non-injective at representation role, so the rule says "H
is not a newtype".

Injectivity is a bit subtle:
                 Nominal   Representational
   Datatype        YES        YES
   Newtype         YES        NO{1}
   Data family     YES        NO{2}

{1} Consider newtype N a = MkN (F a)   -- Arg has Nominal role
    Is it true that (N t1) ~R (N t2)   ==>   t1 ~N t2  ?
    No, absolutely not.  E.g.
       type instance F Int = Int; type instance F Bool = Char
       Then (N Int) ~R (N Bool), by unwrapping, but we don't want Int~Char!

    See Note [Decomposing newtype equalities]

{2} We must treat data families precisely like newtypes, because of the
    possibility of newtype instances. See also
    Note [Decomposing newtype equalities]. See #10534 and
    test case typecheck/should_fail/T10534.

---- Takeaway summary -----
For sound and complete decomposition, we simply need injectivity;
that is for isInjectiveTyCon to be true:

* At Nominal role, isInjectiveTyCon is True for all the TyCons we are
  considering in this Note: datatypes, newtypes, and data families.

* For Givens, injectivity is necessary for soundness; completeness has no
  side conditions.

* For Wanteds, soundness has no side conditions; but injectivity is needed
  for completeness. See Note [Decomposing newtype equalities]

This is implemented in `can_decompose` in `canTyConApp`; it looks at
injectivity, just as specified above.

Note [Work-list ordering]
~~~~~~~~~~~~~~~~~~~~~~~~~
Consider decomposing a TyCon equality

    (0) [W] T k_fresh (t1::k_fresh) ~ T k1 (t2::k1)

This gives rise to 2 equalities in the solver worklist

    (1) [W] k_fresh ~ k1
    (2) [W] t1::k_fresh ~ t2::k1

We would like to solve (1) before looking at (2), so that we don't end
up in the complexities of canEqLHSHetero.  To do this:

* `canDecomposableTyConAppOK` calls `uType` on the arguments
  /left-to-right/.  See the call to zipWith4M in that function.

* `uType` keeps the bag of emitted constraints in the same
  left-to-right order.  See the use of `snocBag` in `uType_defer`.

* `wrapUnifierTcS` adds the bag of deferred constraints from
  `do_unifications` to the work-list using `extendWorkListEqs`.

* `extendWorkListEqs` and `selectWorkItem` together arrange that the
  list of constraints given to `extendWorkListEqs` is processed in
  left-to-right order.

This is not a very big deal.  It reduces the number of solver steps
in the test RaeJobTalk from 1830 to 1815, a 1% reduction.  But still,
it doesn't cost anything either.

Note [Decomposing type family applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Supose we have
   [G/W]  (F ty1) ~r  (F ty2)
This is handled by the TyFamLHS/TyFamLHS case of canEqCanLHS2.

We never decompose to
   [G/W]  ty1 ~r' ty2

Instead

* For Givens we do nothing. Injective type families have no corresponding
  evidence of their injectivity, so we cannot decompose an
  injective-type-family Given.

* For Wanteds, for the Nominal role only, we emit new Wanteds rather like
  functional dependencies, for each injective argument position.

  E.g type family F a b   -- injective in first arg, but not second
      [W] (F s1 t1) ~N (F s2 t2)
  Emit new Wanteds
      [W] s1 ~N s2
  But retain the existing, unsolved constraint.

Note [Decomposing newtype equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This Note also applies to data families, which we treat like
newtype in case of 'newtype instance'.

As Note [Decomposing TyConApp equalities] describes, if N is injective
at role r, we can do this decomposition?
   [G/W] (N ty1) ~r (N ty2)    to     [G/W]  ty1 ~r' ty2

For a Given with r=R, the answer is a solid NO: newtypes are not injective at
representational role, and we must not decompose, or we lose soundness.
Example is wrinkle {1} in Note [Decomposing TyConApp equalities].

For a Wanted with r=R, since newtypes are not injective at representational
role, decomposition is sound, but we may lose completeness.  Nevertheless,
if the newtype is abstract (so can't be unwrapped) we can only solve
the equality by (a) using a Given or (b) decomposition.  If (a) is impossible
(e.g. no Givens) then (b) is safe albeit potentially incomplete.

There are two ways in which decomposing (N ty1) ~r (N ty2) could be incomplete:

* Incompleteness example (EX1): unwrap first
      newtype Nt a = MkNt (Id a)
      type family Id a where Id a = a

      [W] Nt Int ~R Nt Age

  Because of its use of a type family, Nt's parameter will get inferred to
  have a nominal role. Thus, decomposing the wanted will yield [W] Int ~N Age,
  which is unsatisfiable. Unwrapping, though, leads to a solution.

  CONCLUSION: always unwrap newtypes before attempting to decompose
  them.  This is done in can_eq_nc.  Of course, we can't unwrap if the data
  constructor isn't in scope.  See Note [Unwrap newtypes first].

* Incompleteness example (EX2): see #24887
      data family D a
      data instance D Int  = MkD1 (D Char)
      data instance D Bool = MkD2 (D Char)
  Now suppose we have
      [W] g1: D Int ~R# D a
      [W] g2: a ~# Bool
  If we solve g2 first, giving a:=Bool, then we can solve g1 easily:
      D Int ~R# D Char ~R# D Bool
  by newtype unwrapping.

  BUT: if we instead attempt to solve g1 first, we can unwrap the LHS (only)
  leaving     [W] D Char ~#R D Bool
  If we decompose now, we'll get (Char ~R# Bool), which is insoluble.

  CONCLUSION: prioritise nominal equalites in the work list.
  See Note [Prioritise equalities] in GHC.Tc.Solver.InertSet.

* Incompleteness example (EX3): available Givens
      newtype Nt a = Mk Bool         -- NB: a is not used in the RHS,
      type role Nt representational  -- but the user gives it an R role anyway

      [G] Nt t1 ~R Nt t2
      [W] Nt alpha ~R Nt beta

  We *don't* want to decompose to [W] alpha ~R beta, because it's possible
  that alpha and beta aren't representationally equal.  And if we figure
  out (elsewhere) that alpha:=t1 and beta:=t2, we can solve the Wanted
  from the Given.  This is somewhat similar to the question of overlapping
  Givens for class constraints: see Note [Instance and Given overlap] in
  GHC.Tc.Solver.Dict.

  CONCLUSION: don't decompose [W] N s ~R N t, if there are any Given
  equalities that could later solve it.

  But what precisely does it mean to say "any Given equalities that could
  later solve it"?

  In #22924 we had
     [G] f a ~R# a     [W] Const (f a) a ~R# Const a a
  where Const is an abstract newtype.  If we decomposed the newtype, we
  could solve.  Not-decomposing on the grounds that (f a ~R# a) might turn
  into (Const (f a) a ~R# Const a a) seems a bit silly.

  In #22331 we had
     [G] N a ~R# N b   [W] N b ~R# N a
  (where N is abstract so we can't unwrap). Here we really /don't/ want to
  decompose, because the /only/ way to solve the Wanted is from that Given
  (with a Sym).

  In #22519 we had
     [G] a <= b     [W] IO Age ~R# IO Int

  (where IO is abstract so we can't unwrap, and newtype Age = Int; and (<=)
  is a type-level comparison on Nats).  Here we /must/ decompose, despite the
  existence of an Irred Given, or we will simply be stuck.  (Side note: We
  flirted with deep-rewriting of newtypes (see discussion on #22519 and
  !9623) but that turned out not to solve #22924, and also makes type
  inference loop more often on recursive newtypes.)

  The currently-implemented compromise is this:

    we decompose [W] N s ~R# N t unless there is a [G] N s' ~ N t'

  that is, a Given Irred equality with both sides headed with N.
  See the call to noGivenNewtypeReprEqs in canTyConApp.

  This is not perfect.  In principle a Given like [G] (a b) ~ (c d), or
  even just [G] c, could later turn into N s ~ N t.  But since the free
  vars of a Given are skolems, or at least untouchable unification
  variables, this is extremely unlikely to happen.

  Another worry: there could, just, be a CDictCan with some
  un-expanded equality superclasses; but only in some very obscure
  recursive-superclass situations.

   Yet another approach (!) is desribed in
   Note [Decomposing newtypes a bit more aggressively].

Remember: decomposing Wanteds is always /sound/. This Note is
only about /completeness/.

Note [Decomposing newtypes a bit more aggressively]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
IMPORTANT: the ideas in this Note are *not* implemented. Instead, the
current approach is detailed in Note [Decomposing newtype equalities]
and Note [Unwrap newtypes first].
For more details about the ideas in this Note see
  * GHC propoosal: https://github.com/ghc-proposals/ghc-proposals/pull/549
  * issue #22441
  * discussion on !9282.

Consider [G] c, [W] (IO Int) ~R (IO Age)
where IO is abstract, and
   newtype Age = MkAge Int   -- Not abstract
With the above rules, if there any Given Irreds,
the Wanted is insoluble because we can't decompose it.  But in fact,
if we look at the defn of IO, roughly,
    newtype IO a = State# -> (State#, a)
we can see that decomposing [W] (IO Int) ~R (IO Age) to
    [W] Int ~R Age
definitely does not lose completeness. Why not? Because the role of
IO's argment is representational.  Hence:

  DecomposeNewtypeIdea:
     decompose [W] (N s1 .. sn) ~R (N t1 .. tn)
     if the roles of all N's arguments are representational

If N's arguments really /are/ representational this will not lose
completeness.  Here "really are representational" means "if you expand
all newtypes in N's RHS, we'd infer a representational role for each
of N's type variables in that expansion".  See Note [Role inference]
in GHC.Tc.TyCl.Utils.

But the user might /override/ a phantom role with an explicit role
annotation, and then we could (obscurely) get incompleteness.
Consider

   module A( silly, T ) where
     newtype T a = MkT Int
     type role T representational  -- Override phantom role

     silly :: Coercion (T Int) (T Bool)
     silly = Coercion  -- Typechecks by unwrapping the newtype

     data Coercion a b where  -- Actually defined in Data.Type.Coercion
       Coercion :: Coercible a b => Coercion a b

   module B where
     import A
     f :: T Int -> T Bool
     f = case silly of Coercion -> coerce

Here the `coerce` gives [W] (T Int) ~R (T Bool) which, if we decompose,
we'll get stuck with (Int ~R Bool).  Instead we want to use the
[G] (T Int) ~R (T Bool), which will be in the Irreds.

Summary: we could adopt (DecomposeNewtypeIdea), at the cost of a very
obscure incompleteness (above).  But no one is reporting a problem from
the lack of decompostion, so we'll just leave it for now.  This long
Note is just to record the thinking for our future selves.

Note [Decomposing AppTy equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For AppTy all the same questions arise as in
Note [Decomposing TyConApp equalities]. We have

    s1 ~r s2,  t1 ~N t2   ==>   s1 t1 ~r s2 t2       (rule CO_APP)
    s1 t1 ~N s2 t2        ==>   s1 ~N s2,  t1 ~N t2  (CO_LEFT, CO_RIGHT)

In the first of these, why do we need Nominal equality in (t1 ~N t2)?
See {2} below.

For sound and complete solving, we need both directions to decompose. So:
* At nominal role, all is well: we have both directions.
* At representational role, decomposition of Givens is unsound (see {1} below),
  and decomposition of Wanteds is incomplete.

Here is an example of the incompleteness for Wanteds:

    [G] g1 :: a ~R b
    [W] w1 :: Maybe b ~R alpha a
    [W] w2 :: alpha ~N Maybe

Suppose we see w1 before w2. If we decompose, using AppCo to prove w1, we get

    w1 := AppCo w3 w4
    [W] w3 :: Maybe ~R alpha
    [W] w4 :: b ~N a

Note that w4 is *nominal*. A nominal role here is necessary because AppCo
requires a nominal role on its second argument. (See {2} for an example of
why.) Now we are stuck, because w4 is insoluble. On the other hand, if we
see w2 first, setting alpha := Maybe, all is well, as we can decompose
Maybe b ~R Maybe a into b ~R a.

Another example:
    newtype Phant x = MkPhant Int
    [W] w1 :: Phant Int ~R alpha Bool
    [W] w2 :: alpha ~ Phant

If we see w1 first, decomposing would be disastrous, as we would then try
to solve Int ~ Bool. Instead, spotting w2 allows us to simplify w1 to become
    [W] w1' :: Phant Int ~R Phant Bool

which can then (assuming MkPhant is in scope) be simplified to Int ~R Int,
and all will be well. See also Note [Unwrap newtypes first].

Bottom line:
* Always decompose AppTy at nominal role: can_eq_app
* Never decompose AppTy at representational role (neither Given nor Wanted):
  the lack of an equation in can_eq_nc

Extra points
{1}  Decomposing a Given AppTy over a representational role is simply
     unsound. For example, if we have co1 :: Phant Int ~R a Bool (for
     the newtype Phant, above), then we surely don't want any relationship
     between Int and Bool, lest we also have co2 :: Phant ~ a around.

{2} The role on the AppCo coercion is a conservative choice, because we don't
    know the role signature of the function. For example, let's assume we could
    have a representational role on the second argument of AppCo. Then, consider

    data G a where    -- G will have a nominal role, as G is a GADT
      MkG :: G Int
    newtype Age = MkAge Int

    co1 :: G ~R a        -- by assumption
    co2 :: Age ~R Int    -- by newtype axiom
    co3 = AppCo co1 co2 :: G Age ~R a Int    -- by our broken AppCo

    and now co3 can be used to cast MkG to have type G Age, in violation of
    the way GADTs are supposed to work (which is to use nominal equality).
-}

canDecomposableTyConAppOK :: CtEvidence -> EqRel
                          -> TyCon
                          -> (Type,[TcType]) -> (Type,[TcType])
                          -> TcS (StopOrContinue a)
-- Precondition: tys1 and tys2 are the same finite length, hence "OK"
canDecomposableTyConAppOK ev eq_rel tc (ty1,tys1) (ty2,tys2)
  = assert (tys1 `equalLength` tys2) $
    do { traceTcS "canDecomposableTyConAppOK"
                  (ppr ev $$ ppr eq_rel $$ ppr tc $$ ppr tys1 $$ ppr tys2)
       ; case ev of
           CtWanted (WantedCt { ctev_dest = dest })
             -- new_locs and tc_roles are both infinite, so we are
             -- guaranteed that cos has the same length as tys1 and tys2
             -- See Note [Fast path when decomposing TyConApps]
             -> do { (co, _, _) <- wrapUnifierTcS ev role $ \uenv ->
                        do { cos <- zipWith4M (u_arg uenv) new_locs tc_roles tys1 tys2
                                    -- zipWith4M: see Note [Work-list ordering]
                                    -- in GHC.Tc.Solved.Equality
                           ; return (mkTyConAppCo role tc cos) }
                   ; setWantedEq dest co }

           CtGiven (GivenCt { ctev_evar = evar })
             | let pred_ty = mkEqPred eq_rel ty1 ty2
                   ev_co   = mkCoVarCo (setVarType evar pred_ty)
                   -- setVarType: satisfy Note [mkSelCo precondition] in Coercion.hs
                   -- Remember: ty1/ty2 may be more fully zonked than evar
                   --           See the call to canonicaliseEquality in solveEquality.
             -> emitNewGivens loc
                       [ (r, mkSelCo (SelTyCon i r) ev_co)
                       | (r, ty1, ty2, i) <- zip4 tc_roles tys1 tys2 [0..]
                       , r /= Phantom
                       , not (isCoercionTy ty1) && not (isCoercionTy ty2) ]

    ; stopWith ev "Decomposed TyConApp" }

  where
    loc  = ctEvLoc ev
    role = eqRelRole eq_rel

    u_arg uenv arg_loc arg_role = uType arg_env
       where
         arg_env = uenv `setUEnvRole` arg_role
                        `updUEnvLoc`  const arg_loc

    -- Infinite, to allow for over-saturated TyConApps
    tc_roles = tyConRoleListX role tc

      -- Add nuances to the location during decomposition:
      --  * if the argument is a kind argument, remember this, so that error
      --    messages say "kind", not "type". This is determined based on whether
      --    the corresponding tyConBinder is named (that is, dependent)
      --  * if the argument is invisible, note this as well, again by
      --    looking at the corresponding binder
      -- For oversaturated tycons, we need the (repeat loc) tail, which doesn't
      -- do either of these changes. (Forgetting to do so led to #16188)
      --
      -- NB: infinite in length
    new_locs = [ adjustCtLocTyConBinder bndr loc
               | bndr <- tyConBinders tc ]
               ++ repeat loc

canDecomposableFunTy :: CtEvidence -> EqRel -> FunTyFlag
                     -> (Type,Type,Type,Type)   -- (fun_ty,multiplicity,arg,res)
                     -> (Type,Type,Type,Type)   -- (fun_ty,multiplicity,arg,res)
                     -> TcS (StopOrContinue a)
canDecomposableFunTy ev eq_rel af f1@(ty1,m1,a1,r1) f2@(ty2,m2,a2,r2)
  = do { traceTcS "canDecomposableFunTy"
                  (ppr ev $$ ppr eq_rel $$ ppr f1 $$ ppr f2)
       ; case ev of
           CtWanted (WantedCt { ctev_dest = dest })
             -> do { (co, _, _) <- wrapUnifierTcS ev Nominal $ \ uenv ->
                        do { let mult_env = uenv `updUEnvLoc` toInvisibleLoc
                                                 `setUEnvRole` funRole role SelMult
                           ; mult <- uType mult_env m1 m2
                           ; arg  <- uType (uenv `setUEnvRole` funRole role SelArg) a1 a2
                           ; res  <- uType (uenv `setUEnvRole` funRole role SelRes) r1 r2
                           ; return (mkNakedFunCo role af mult arg res) }
                   ; setWantedEq dest co }

           CtGiven (GivenCt { ctev_evar = evar })
             | let pred_ty = mkEqPred eq_rel ty1 ty2
                   ev_co   = mkCoVarCo (setVarType evar pred_ty)
                   -- setVarType: satisfy Note [mkSelCo precondition] in Coercion.hs
                   -- Remember: ty1/ty2 may be more fully zonked than evar
                   --           See the call to canonicaliseEquality in solveEquality.
             -> emitNewGivens loc
                       [ (funRole role fs, mkSelCo (SelFun fs) ev_co)
                       | fs <- [SelMult, SelArg, SelRes] ]

    ; stopWith ev "Decomposed TyConApp" }

  where
    loc  = ctEvLoc ev
    role = eqRelRole eq_rel

-- | Call canEqSoftFailure when canonicalizing an equality fails, but if the
-- equality is representational, there is some hope for the future.
canEqSoftFailure :: CtEvidence -> EqRel -> TcType -> TcType
                 -> TcS (StopOrContinue (Either IrredCt a))
canEqSoftFailure ev NomEq ty1 ty2
  = canEqHardFailure ev ty1 ty2
canEqSoftFailure ev ReprEq ty1 ty2
  = do { (redn1, rewriters1) <- rewrite ev ty1
       ; (redn2, rewriters2) <- rewrite ev ty2
            -- We must rewrite the types before putting them in the
            -- inert set, so that we are sure to kick them out when
            -- new equalities become available
       ; traceTcS "canEqSoftFailure with ReprEq" $
         vcat [ ppr ev, ppr redn1, ppr redn2 ]
       ; new_ev <- rewriteEqEvidence (rewriters1 S.<> rewriters2) ev NotSwapped redn1 redn2
       ; finishCanWithIrred ReprEqReason new_ev }

-- | Call when canonicalizing an equality fails with utterly no hope.
canEqHardFailure :: CtEvidence -> TcType -> TcType
                 -> TcS (StopOrContinue (Either IrredCt a))
-- See Note [Make sure that insolubles are fully rewritten]
canEqHardFailure ev ty1 ty2
  = do { traceTcS "canEqHardFailure" (ppr ty1 $$ ppr ty2)
       ; (redn1, rewriters1) <- rewriteForErrors ev ty1
       ; (redn2, rewriters2) <- rewriteForErrors ev ty2
       ; new_ev <- rewriteEqEvidence (rewriters1 S.<> rewriters2) ev NotSwapped redn1 redn2
       ; finishCanWithIrred ShapeMismatchReason new_ev }

{-
Note [Canonicalising type applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given (s1 t1) ~ ty2, how should we proceed?
The simple thing is to see if ty2 is of form (s2 t2), and
decompose.

However, over-eager decomposition gives bad error messages
for things like
   a b ~ Maybe c
   e f ~ p -> q
Suppose (in the first example) we already know a~Array.  Then if we
decompose the application eagerly, yielding
   a ~ Maybe
   b ~ c
we get an error        "Can't match Array ~ Maybe",
but we'd prefer to get "Can't match Array b ~ Maybe c".

So instead can_eq_wanted_app rewrites the LHS and RHS, in the hope of
replacing (a b) by (Array b), before using try_decompose_app to
decompose it.

Note [Make sure that insolubles are fully rewritten]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When an equality fails, we still want to rewrite the equality
all the way down, so that it accurately reflects
 (a) the mutable reference substitution in force at start of solving
 (b) any ty-binds in force at this point in solving
See Note [Rewrite insolubles] in GHC.Tc.Solver.InertSet.
And if we don't do this there is a bad danger that
GHC.Tc.Solver.applyTyVarDefaulting will find a variable
that has in fact been substituted.

Note [Do not decompose Given polytype equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider [G] (forall a. t1 ~ forall a. t2).  Can we decompose this?
No -- what would the evidence look like?  So instead we simply discard
this given evidence.

Note [No top-level newtypes on RHS of representational equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we're in this situation:

 work item:  [W] c1 : a ~R b
     inert:  [G] c2 : b ~R Id a

where
  newtype Id a = Id a

We want to make sure canEqCanLHS sees [W] a ~R a, after b is rewritten
and the Id newtype is unwrapped. This is assured by requiring only rewritten
types in canEqCanLHS *and* having the newtype-unwrapping check above
the tyvar check in can_eq_nc.

Note that this only applies to saturated applications of newtype TyCons, as
we can't rewrite an unsaturated application. See for example T22310, where
we ended up with:

  newtype Compose f g a = ...

  [W] t[tau] ~# Compose Foo Bar

Note [Put touchable variables on the left]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Ticket #10009, a very nasty example:

    f :: (UnF (F b) ~ b) => F b -> ()

    g :: forall a. (UnF (F a) ~ a) => a -> ()
    g _ = f (undefined :: F a)

For g we get [G]  g1 : UnF (F a) ~ a
             [W] w1 : UnF (F beta) ~ beta
             [W] w2 : F a ~ F beta

g1 is canonical (CEqCan). It is oriented as above because a is not touchable.
See canEqTyVarFunEq.

w1 is similarly canonical, though the occurs-check in canEqTyVarFunEq is key
here.

w2 is canonical. But which way should it be oriented? As written, we'll be
stuck. When w2 is added to the inert set, nothing gets kicked out: g1 is
a Given (and Wanteds don't rewrite Givens), and w2 doesn't mention the LHS
of w2. We'll thus lose.

But if w2 is swapped around, to

    [W] w3 : F beta ~ F a

then we'll kick w1 out of the inert set (it mentions the LHS of w3). We then
rewrite w1 to

    [W] w4 : UnF (F a) ~ beta

and then, using g1, to

    [W] w5 : a ~ beta

at which point we can unify and go on to glory. (This rewriting actually
happens all at once, in the call to rewrite during canonicalisation.)

But what about the new LHS makes it better? It mentions a variable (beta)
that can appear in a Wanted -- a touchable metavariable never appears
in a Given. On the other hand, the original LHS mentioned only variables
that appear in Givens. We thus choose to put variables that can appear
in Wanteds on the left.

Ticket #12526 is another good example of this in action.

-}

---------------------
canEqCanLHS :: CtEvidence            -- ev :: lhs ~ rhs
            -> EqRel -> SwapFlag
            -> CanEqLHS              -- lhs (or, if swapped, rhs)
            -> TcType                -- lhs: pretty lhs, already rewritten
            -> TcType -> TcType      -- rhs: already rewritten
            -> TcS (StopOrContinue (Either IrredCt EqCt))
canEqCanLHS ev eq_rel swapped lhs1 ps_xi1 xi2 ps_xi2
  | k1 `tcEqType` k2
  = canEqCanLHSHomo ev eq_rel swapped lhs1 ps_xi1 xi2 ps_xi2

  | otherwise
  = canEqCanLHSHetero ev eq_rel swapped lhs1 ps_xi1 k1 xi2 ps_xi2 k2

  where
    k1 = canEqLHSKind lhs1
    k2 = typeKind xi2


{-
Note [Kind Equality Orientation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
While in theory [W] x ~ y and [W] y ~ x ought to give us the same behaviour, in
practice it does not.  See Note [Fundeps with instances, and equality
orientation] where this is discussed at length.  As a rule of thumb: we keep
the newest unification variables on the left of the equality.  See also
Note [Improvement orientation].

In particular, `canEqCanLHSHetero` produces the following constraint equalities

[X] (xi1 :: ki1) ~ (xi2 :: ki2)
  -->  [X] kco :: ki1 ~ ki2
       [X] co : xi1 :: ki1 ~ (xi2 |> sym kco) :: ki1

Note that the types in the LHS of the new constraints are the ones that were on the LHS of
the original constraint.

--- Historical note ---
We prevously used to flip the kco to avoid using a sym in the cast

[X] (xi1 :: ki1) ~ (xi2 :: ki2)
  -->  [X] kco :: ki2 ~ ki1
       [X] co : xi1 :: ki1 ~ (xi2 |> kco) :: ki1

But this sent solver in an infinite loop (see #19415).
-- End of historical note --
-}

canEqCanLHSHetero :: CtEvidence         -- :: (xi1 :: ki1) ~ (xi2 :: ki2)
                                        --    (or reversed if SwapFlag=IsSwapped)
                  -> EqRel -> SwapFlag
                  -> CanEqLHS -> TcType -- xi1
                  -> TcKind             -- ki1
                  -> TcType -> TcType   -- xi2
                  -> TcKind             -- ki2
                  -> TcS (StopOrContinue (Either IrredCt EqCt))
canEqCanLHSHetero ev eq_rel swapped lhs1 ps_xi1 ki1 xi2 ps_xi2 ki2
-- See Note [Equalities with incompatible kinds]
-- See Note [Kind Equality Orientation]

-- NB: preserve left-to-right orientation!! See wrinkle (W2) in
-- Note [Fundeps with instances, and equality orientation] in GHC.Tc.Solver.Dict
--    NotSwapped:
--        ev      :: (lhs1:ki1) ~r# (xi2:ki2)
--        kind_co :: k11 ~# ki2               -- Same orientation as ev
--        type_ev :: lhs1 ~r# (xi2 |> sym kind_co)
--    Swapped
--        ev      :: (xi2:ki2) ~r# (lhs1:ki1)
--        kind_co :: ki2 ~# ki1               -- Same orientation as ev
--        type_ev :: (xi2 |> kind_co) ~r# lhs1

  = do { (kind_co, rewriters, unifs_happened) <- mk_kind_eq   -- :: ki1 ~N ki2
       ; if unifs_happened
              -- Unifications happened, so start again to do the zonking
              -- Otherwise we might put something in the inert set that isn't inert
         then startAgainWith (mkNonCanonical ev)
         else
    do { let lhs_redn = mkReflRedn role ps_xi1
             rhs_redn = mkGReflRightRedn role xi2 mb_sym_kind_co
             mb_sym_kind_co = case swapped of
                                NotSwapped -> mkSymCo kind_co
                                IsSwapped  -> kind_co

       ; traceTcS "Hetero equality gives rise to kind equality"
           (ppr swapped $$
            ppr kind_co <+> dcolon <+> sep [ ppr ki1, text "~#", ppr ki2 ])
       ; type_ev <- rewriteEqEvidence rewriters ev swapped lhs_redn rhs_redn

       ; let new_xi2 = mkCastTy ps_xi2 mb_sym_kind_co
       ; canEqCanLHSHomo type_ev eq_rel NotSwapped lhs1 ps_xi1 new_xi2 new_xi2 }}

  where
    mk_kind_eq :: TcS (CoercionN, RewriterSet, Bool)
    -- Returned kind_co has kind (k1 ~ k2) if NotSwapped, (k2 ~ k1) if Swapped
    -- Returned Bool = True if unifications happened, so we should retry
    mk_kind_eq = case ev of
      CtGiven (GivenCt { ctev_evar = evar, ctev_loc = loc })
        -> do { let kind_co  = mkKindCo (mkCoVarCo evar)
                    pred_ty  = unSwap swapped mkNomEqPred ki1 ki2
                    kind_loc = mkKindEqLoc xi1 xi2 loc
              ; kind_ev <- newGivenEvVar kind_loc (pred_ty, evCoercion kind_co)
              ; emitWorkNC [CtGiven kind_ev]
              ; return (givenCtEvCoercion kind_ev, emptyRewriterSet, False) }

      CtWanted {}
        -> do { (kind_co, cts, unifs) <- wrapUnifierTcS ev Nominal $ \uenv ->
                                         let uenv' = updUEnvLoc uenv (mkKindEqLoc xi1 xi2)
                                         in unSwap swapped (uType uenv') ki1 ki2
              ; return (kind_co, rewriterSetFromCts cts, not (null unifs)) }

    xi1  = canEqLHSType lhs1
    role = eqRelRole eq_rel

canEqCanLHSHomo :: CtEvidence          -- lhs ~ rhs
                                       -- or, if swapped: rhs ~ lhs
                -> EqRel -> SwapFlag
                -> CanEqLHS -> TcType  -- lhs, pretty lhs
                -> TcType   -> TcType  -- rhs, pretty rhs
                -> TcS (StopOrContinue (Either IrredCt EqCt))
-- Guaranteed that typeKind lhs == typeKind rhs
canEqCanLHSHomo ev eq_rel swapped lhs1 ps_xi1 xi2 ps_xi2
  | (xi2', mco) <- split_cast_ty xi2
  , Just lhs2 <- canEqLHS_maybe xi2'
  = canEqCanLHS2 ev eq_rel swapped lhs1 ps_xi1 lhs2 (ps_xi2 `mkCastTyMCo` mkSymMCo mco) mco

  | otherwise
  = canEqCanLHSFinish ev eq_rel swapped lhs1 ps_xi2

  where
    split_cast_ty (CastTy ty co) = (ty, MCo co)
    split_cast_ty other          = (other, MRefl)

-- This function deals with the case that both LHS and RHS are potential
-- CanEqLHSs.
canEqCanLHS2 :: CtEvidence              -- lhs ~ (rhs |> mco)
                                        -- or, if swapped: (rhs |> mco) ~ lhs
             -> EqRel -> SwapFlag
             -> CanEqLHS                -- lhs (or, if swapped, rhs)
             -> TcType                  -- pretty lhs
             -> CanEqLHS                -- rhs
             -> TcType                  -- pretty rhs
             -> MCoercion               -- :: kind(rhs) ~N kind(lhs)
             -> TcS (StopOrContinue (Either IrredCt EqCt))
canEqCanLHS2 ev eq_rel swapped lhs1 ps_xi1 lhs2 ps_xi2 mco
  | lhs1 `eqCanEqLHS` lhs2
    -- It must be the case that mco is reflexive
  = canEqReflexive ev eq_rel lhs1_ty

  | TyVarLHS tv1 <- lhs1
  , TyVarLHS tv2 <- lhs2
  = -- See Note [TyVar/TyVar orientation] in GHC.Tc.Utils.Unify
    do { traceTcS "canEqLHS2 swapOver" (ppr tv1 $$ ppr tv2 $$ ppr swapped)
       ; if swapOverTyVars (isGiven ev) tv1 tv2
         then finish_with_swapping
         else finish_without_swapping }

  | TyVarLHS {} <- lhs1
  , TyFamLHS {} <- lhs2
  = if put_tyvar_on_lhs
    then finish_without_swapping
    else finish_with_swapping

  | TyFamLHS {} <- lhs1
  , TyVarLHS {} <- lhs2
  = if put_tyvar_on_lhs
    then finish_with_swapping
    else finish_without_swapping

  | TyFamLHS _fun_tc1 fun_args1 <- lhs1
  , TyFamLHS _fun_tc2 fun_args2 <- lhs2
  -- See Note [Decomposing type family applications]
  = do { traceTcS "canEqCanLHS2 two type families" (ppr lhs1 $$ ppr lhs2)
       ; tclvl <- getTcLevel
       ; let tvs1 = tyCoVarsOfTypes fun_args1
             tvs2 = tyCoVarsOfTypes fun_args2

             -- See Note [Orienting TyFamLHS/TyFamLHS]
             swap_for_size = typesSize fun_args2 > typesSize fun_args1

             -- See Note [Orienting TyFamLHS/TyFamLHS]
             meta_tv_lhs = anyVarSet (isTouchableMetaTyVar tclvl) tvs1
             meta_tv_rhs = anyVarSet (isTouchableMetaTyVar tclvl) tvs2
             swap_for_rewriting = meta_tv_rhs && not meta_tv_lhs
                                  -- See Note [Put touchable variables on the left]
                                  -- This second check is just to avoid unfruitful swapping

         -- It's important that we don't flip-flop (#T24134)
         -- So swap_for_rewriting "wins", and we only try swap_for_size
         -- if swap_for_rewriting doesn't care either way
       ; if swap_for_rewriting || (meta_tv_lhs == meta_tv_rhs && swap_for_size)
         then finish_with_swapping
         else finish_without_swapping }
  where
    sym_mco = mkSymMCo mco
    role    = eqRelRole eq_rel
    lhs1_ty  = canEqLHSType lhs1
    lhs2_ty  = canEqLHSType lhs2

    finish_without_swapping
      = canEqCanLHSFinish ev eq_rel swapped lhs1 (ps_xi2 `mkCastTyMCo` mco)

    -- Swapping. We have   ev : lhs1 ~ lhs2 |> co
    -- We swap to      new_ev : lhs2 ~ lhs1 |> sym co
    --                     ev = grefl1 ; sym new_ev ; grefl2
    --      where grefl1 : lhs1 ~ lhs1 |> sym co
    --            grefl2 : lhs2 ~ lhs2 |> co
    finish_with_swapping
      = do { let lhs1_redn = mkGReflRightMRedn role lhs1_ty sym_mco
                 lhs2_redn = mkGReflLeftMRedn  role lhs2_ty mco
           ; new_ev <-rewriteEqEvidence emptyRewriterSet ev swapped lhs1_redn lhs2_redn
           ; canEqCanLHSFinish new_ev eq_rel IsSwapped lhs2 (ps_xi1 `mkCastTyMCo` sym_mco) }

    put_tyvar_on_lhs = isWanted ev && eq_rel == NomEq
    -- See Note [Orienting TyVarLHS/TyFamLHS]
    -- Same conditions as for canEqCanLHSFinish_try_unification
    -- which we are setting ourselves up for here

{- Note [Orienting TyVarLHS/TyFamLHS]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
What if one side is a TyVarLHS and the other is a TyFamLHS, (a ~ F tys)?
Which to put on the left?  Answer:

* If there is no chance of unifying, put the type family on the left,
  (F tys ~ a), because it's generally better to rewrite away function
  calls.  See `put_tyvar_on_lhs` in canEqCanLHS2; and
  Note [Orienting TyVarLHS/TyFamLHS]

* But if there /is/ a chance of unifying, put the tyvar on the left,
  (a ~ F tys), as this may be our only shot to unify. Again see
  `put_tyvar_on_lhs`.

* But if we /fail/ to unify then flip back to (F tys ~ a) because it's
  generally better to rewrite away function calls. See the call to
  `swapAndFinish` in `canEqCanLHSFinish_try_unification`

  It's important to flip back. Consider
    [W] F alpha ~ alpha
    [W] F alpha ~ beta
    [W] G alpha beta ~ Int   ( where we have type instance G a a = a )
  If we end up with a stuck alpha ~ F alpha, we won't be able to solve this.
  Test case: indexed-types/should_compile/CEqCanOccursCheck

Note [Orienting TyFamLHS/TyFamLHS]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have a TyFamLHS on both sides, we choose how to orient it.

* swap_for_size.  If we have
      S a ~ F (G (H (Maybe a)))
  then we swap so that we tend to rewrite the bigger type (F (G (H (Maybe a))))
  into the smaller one (S a).  This same test tends to avoid occurs-check
  errors.  E.g.
      S g ~ F (G (S g))
  Here (S g) occurs on the RHS, so this is not canonical.  But if we swap it
  around, it /is/ canonical
      F (G (S g)) ~ S g

* swap_for_rewriting: put touchable meta-tyvars on the left:
  see Note [Put touchable variables on the left]
-}

-- The RHS here is either not CanEqLHS, or it's one that we
-- want to rewrite the LHS to (as per e.g. swapOverTyVars)
canEqCanLHSFinish, canEqCanLHSFinish_try_unification,
                   canEqCanLHSFinish_no_unification
    :: CtEvidence           -- (lhs ~ rhs) or if swapped (rhs ~ lhs)
    -> EqRel -> SwapFlag
    -> CanEqLHS             -- lhs
    -> TcType               -- rhs
    -> TcS (StopOrContinue (Either IrredCt EqCt))
    -- RHS is fully rewritten, but with type synonyms
    --   preserved as much as possible
    -- Guaranteed preconditions that
    --    (TyEq:K)  handled in canEqCanLHSHomo
    --    (TyEq:N)  checked in can_eq_nc
    --    (TyEq:TV) handled in canEqCanLHS2

---------------------------
canEqCanLHSFinish ev eq_rel swapped lhs rhs
  = do { traceTcS "canEqCanLHSFinish" $
         vcat [ text "ev:" <+> ppr ev
              , text "swapped:" <+> ppr swapped
              , text "lhs:" <+> ppr lhs
              , text "rhs:" <+> ppr rhs ]

         -- Assertion: (TyEq:K) is already satisfied
       ; massert (canEqLHSKind lhs `eqType` typeKind rhs)

         -- Assertion: (TyEq:N) is already satisfied (if applicable)
       ; assertPprM ty_eq_N_OK $
           vcat [ text "CanEqCanLHSFinish: (TyEq:N) not satisfied"
                , text "rhs:" <+> ppr rhs ]

       ; canEqCanLHSFinish_try_unification ev eq_rel swapped lhs rhs }

  where
    -- This is about (TyEq:N): check that we don't have a saturated application
    -- of a newtype TyCon at the top level of the RHS, if the constructor
    -- of the newtype is in scope.
    ty_eq_N_OK :: TcS Bool
    ty_eq_N_OK
      | ReprEq <- eq_rel
      , Just (tc, tc_args) <- splitTyConApp_maybe rhs
      , Just con <- newTyConDataCon_maybe tc
      -- #22310: only a problem if the newtype TyCon is saturated.
      , tc_args `lengthAtLeast` tyConArity tc
      -- #21010: only a problem if the newtype constructor is in scope.
      = do { rdr_env <- getGlobalRdrEnvTcS
           ; let con_in_scope = isJust $ lookupGRE_Name rdr_env (dataConName con)
           ; return $ not con_in_scope }
      | otherwise
      = return True

-----------------------
canEqCanLHSFinish_try_unification ev eq_rel swapped lhs rhs
  -- Try unification; for Wanted, Nominal equalities with a meta-tyvar on the LHS
  | isWanted ev      -- See Note [Do not unify Givens]
  , NomEq <- eq_rel  -- See Note [Do not unify representational equalities]
  , TyVarLHS tv <- lhs
  = do { given_eq_lvl <- getInnermostGivenEqLevel
       ; if not (touchabilityAndShapeTest given_eq_lvl tv rhs)
         then if | Just can_rhs <- canTyFamEqLHS_maybe rhs
                 -> swapAndFinish ev eq_rel swapped (mkTyVarTy tv) can_rhs
                    -- See Note [Orienting TyVarLHS/TyFamLHS]

                 | otherwise
                 -> canEqCanLHSFinish_no_unification ev eq_rel swapped lhs rhs
         else

    -- We have a touchable unification variable on the left
    do { check_result <- checkTouchableTyVarEq ev tv rhs
       ; case check_result of {
            PuFail reason
              | Just can_rhs <- canTyFamEqLHS_maybe rhs
              -> swapAndFinish ev eq_rel swapped (mkTyVarTy tv) can_rhs
                -- Swap back: see Note [Orienting TyVarLHS/TyFamLHS]

              | reason `cterHasOnlyProblems` do_not_prevent_rewriting
              -> canEqCanLHSFinish_no_unification ev eq_rel swapped lhs rhs

              | otherwise
              -> tryIrredInstead reason ev eq_rel swapped lhs rhs ;

            PuOK _ rhs_redn ->

    -- Success: we can solve by unification
    do { -- In the common case where rhs_redn is Refl, we don't need to rewrite
         -- the evidence, even if swapped=IsSwapped.   Suppose the original was
         --     [W] co : Int ~ alpha
         -- We unify alpha := Int, and set co := <Int>.  No need to
         -- swap to   co = sym co'
         --           co' = <Int>
         new_ev <- if isReflCo (reductionCoercion rhs_redn)
                   then return ev
                   else rewriteEqEvidence emptyRewriterSet ev swapped
                            (mkReflRedn Nominal (mkTyVarTy tv)) rhs_redn

       ; let tv_ty     = mkTyVarTy tv
             final_rhs = reductionReducedType rhs_redn

       ; traceTcS "Sneaky unification:" $
         vcat [text "Unifies:" <+> ppr tv <+> text ":=" <+> ppr final_rhs,
               text "Coercion:" <+> pprEq tv_ty final_rhs,
               text "Left Kind is:" <+> ppr (typeKind tv_ty),
               text "Right Kind is:" <+> ppr (typeKind final_rhs) ]

       -- Update the unification variable itself
       ; unifyTyVar tv final_rhs

       -- Provide Refl evidence for the constraint
       -- Ignore 'swapped' because it's Refl!
       ; setEvBindIfWanted new_ev EvCanonical $
         evCoercion (mkNomReflCo final_rhs)

       -- Kick out any constraints that can now be rewritten
       ; kickOutAfterUnification [tv]

       ; return (Stop new_ev (text "Solved by unification")) }}}}

  -- Otherwise unification is off the table
  | otherwise
  = canEqCanLHSFinish_no_unification ev eq_rel swapped lhs rhs

  where
    -- Some problems prevent /unification/ but not /rewriting/
    -- Skolem-escape: if we have [W] alpha[2] ~ Maybe b[3]
    --    we can't unify (skolem-escape); but it /is/ canonical,
    --    and hence we /can/ use it for rewriting
    -- Concrete-ness:  alpha[conc] ~ b[sk]
    --    We can use it to rewrite; we still have to solve the original
    do_not_prevent_rewriting :: CheckTyEqResult
    do_not_prevent_rewriting = cteProblem cteSkolemEscape S.<>
                               cteProblem cteConcrete

---------------------------
-- Unification is off the table
canEqCanLHSFinish_no_unification ev eq_rel swapped lhs rhs
  = do { -- Do checkTypeEq to guarantee (TyEq:OC), (TyEq:F)
         -- Must do the occurs check even on tyvar/tyvar equalities,
         -- in case have  x ~ (y :: ..x...); this is #12593.
       ; check_result <- checkTypeEq ev eq_rel lhs rhs

       ; let lhs_ty = canEqLHSType lhs
       ; case check_result of
            PuFail reason

              -- If we had F a ~ G (F a), which gives an occurs check,
              -- then swap it to G (F a) ~ F a, which does not
              -- However `swap_for_size` above will orient it with (G (F a)) on
              -- the left anwyway.  `swap_for_rewriting` "wins", but that doesn't
              -- matter: in the occurs check case swap_for_rewriting will be moot.
              -- TL;DR: the next four lines of code are redundant
              -- I'm leaving them here in case they become relevant again
--              | TyFamLHS {} <- lhs
--              , Just can_rhs <- canTyFamEqLHS_maybe rhs
--              , reason `cterHasOnlyProblem` cteSolubleOccurs
--              -> swapAndFinish ev eq_rel swapped lhs_ty can_rhs
--              | otherwise

              -> tryIrredInstead reason ev eq_rel swapped lhs rhs

            PuOK _ rhs_redn
              -> do { new_ev <- rewriteEqEvidence emptyRewriterSet ev swapped
                                   (mkReflRedn (eqRelRole eq_rel) lhs_ty)
                                   rhs_redn

                    -- Important: even if the coercion is Refl,
                    --   * new_ev has reductionReducedType on the RHS
                    --   * eq_rhs is set to reductionReducedType
                    -- See Note [Forgetful synonyms in checkTyConApp] in GHC.Tc.Utils.Unify
                    ; continueWith $ Right $
                      EqCt { eq_ev  = new_ev, eq_eq_rel = eq_rel
                           , eq_lhs = lhs
                           , eq_rhs = reductionReducedType rhs_redn } } }

----------------------
swapAndFinish :: CtEvidence -> EqRel -> SwapFlag
              -> TcType -> CanEqLHS      -- ty ~ F tys
              -> TcS (StopOrContinue (Either unused EqCt))
-- We have an equality alpha ~ F tys, that we can't unify e.g because 'tys'
-- mentions alpha, it would not be a canonical constraint as-is.
-- We want to flip it to (F tys ~ a), whereupon it is canonical
swapAndFinish ev eq_rel swapped lhs_ty can_rhs
  = do { let role = eqRelRole eq_rel
       ; new_ev <- rewriteEqEvidence emptyRewriterSet ev (flipSwap swapped)
                       (mkReflRedn role (canEqLHSType can_rhs))
                       (mkReflRedn role lhs_ty)
       ; continueWith $ Right $
         EqCt { eq_ev  = new_ev, eq_eq_rel = eq_rel
              , eq_lhs = can_rhs, eq_rhs = lhs_ty } }

----------------------
tryIrredInstead :: CheckTyEqResult -> CtEvidence
                -> EqRel -> SwapFlag -> CanEqLHS -> TcType
                -> TcS (StopOrContinue (Either IrredCt unused))
-- We have a non-canonical equality
-- We still swap it if 'swapped' says so, so that it is oriented
-- in the direction that the error-reporting machinery
-- expects it; e.g.  (m ~ t m) rather than (t m ~ m)
-- This is not very important, and only affects error reporting.
tryIrredInstead reason ev eq_rel swapped lhs rhs
  = do { traceTcS "cantMakeCanonical" (ppr reason $$ ppr lhs $$ ppr rhs)
       ; let role = eqRelRole eq_rel
       ; new_ev <- rewriteEqEvidence emptyRewriterSet ev swapped
                       (mkReflRedn role (canEqLHSType lhs))
                       (mkReflRedn role rhs)
       ; finishCanWithIrred (NonCanonicalReason reason) new_ev }

finishCanWithIrred :: CtIrredReason -> CtEvidence
                   -> TcS (StopOrContinue (Either IrredCt a))
finishCanWithIrred reason ev
  = do { -- Abort fast if we have any insoluble Wanted constraints,
         -- and the TcS abort-if-insoluble flag is on.
         when (isInsolubleReason reason) tryEarlyAbortTcS

       ; continueWith $ Left $ IrredCt { ir_ev = ev, ir_reason = reason } }

-----------------------
-- | Solve a reflexive equality constraint
canEqReflexive :: CtEvidence    -- ty ~ ty
               -> EqRel
               -> TcType        -- ty
               -> TcS (StopOrContinue a)   -- always Stop
canEqReflexive ev eq_rel ty
  = do { setEvBindIfWanted ev EvCanonical $
         evCoercion (mkReflCo (eqRelRole eq_rel) ty)
       ; stopWith ev "Solved by reflexivity" }

{- Note [Equalities with incompatible kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
What do we do when we have an equality

  (tv :: k1) ~ (rhs :: k2)

where k1 and k2 differ? Easy: we create a coercion that relates k1 and
k2 and use this to cast. To wit, from

  [X] (tv :: k1) ~ (rhs :: k2)

(where [X] is [G] or [W]), we go to

  [X] co :: k1 ~ k2
  [X] (tv :: k1) ~ ((rhs |> sym co) :: k1)

Wrinkles:

(EIK1) When X is W, the new type-level wanted is effectively rewritten by the
     kind-level one. We thus include the kind-level wanted in the RewriterSet
     for the type-level one. See Note [Wanteds rewrite Wanteds] in GHC.Tc.Types.Constraint.
     This is done in canEqCanLHSHetero.

(EIK2) Suppose we have [W] (a::Type) ~ (b::Type->Type). The above rewrite will produce
        [W] w  : a ~ (b |> kw)
        [W] kw : Type ~ (Type->Type)

     But we do /not/ want to regard `w` as canonical, and use it for rewriting
     other constraints: `kw` is insoluble, and replacing something of kind
     `Type` with something of kind `Type->Type` (even wrapped in an insouluble
     cast) does not help, and doing so turns out to lead to much worse error
     messages.  (In particular, if 'a' is a unification variable, we might
     unify, losing the tracking info that it depends on solving `kw`.)

     Conclusion: if a RHS contains a coercion hole arising from fixing a hetero-kinded
     equality, treat the equality (`w` in this case) as non-canonical, so that
       * It will not be used for unification
       * It will not be used for rewriting
     Instead, it lands in the inert_irreds in the inert set, awaiting solution of
     that `kw`.

     (EIK2a) We must later indeed unify if/when the kind-level wanted, `kw` gets
     solved. This is done in kickOutAfterFillingCoercionHole, which kicks out
     all equalities whose RHS mentions the filled-in coercion hole.  Note that
     it looks for type family equalities, too, because of the use of unifyTest
     in canEqTyVarFunEq.

     (EIK2b) What if the RHS mentions /other/ coercion holes?  How can that happen?  The
     main way is like this. Assume F :: forall k. k -> Type
        [W] kw : k  ~ Type
        [W] w  : a ~ F k t
     We can rewrite `w` with `kw` like this:
        [W] w' : a ~ F Type (t |> kw)
     The cast on the second argument of `F` is necessary to keep the appliation well-kinded.
     There is nothing special here; no reason not treat w' as canonical, and use it for
     rewriting. Indeed tests JuanLopez only typechecks if we do.  So we'd like to treat
     this kind of equality as canonical.

     Hence the ch_hetero_kind field in CoercionHole: it is True of constraints
     created by `canEqCanLHSHetero` to fix up hetero-kinded equalities; and False otherwise:

     * An equality constraint is non-canonical if it mentions a hetero-kind
       CoercionHole on the RHS.  See the `hasCoercionHoleCo` test in GHC.Tc.Utils.checkCo.

     * Hetero-kind CoercionHoles are created when the parent's CtOrigin is
       KindEqOrigin: see GHC.Tc.Utils.TcMType.newCoercionHole and friends.  We
       set this origin, via `mkKindLoc`, in `mk_kind_eq` in `canEqCanLHSHetero`.

(EIK3) Suppose we have [W] (a :: k1) ~ (rhs :: k2). We duly follow the
     algorithm detailed here, producing [W] co :: k1 ~ k2, and adding
     [W] (a :: k1) ~ ((rhs |> sym co) :: k1) to the irreducibles. Some time
     later, we solve co, and fill in co's coercion hole. This kicks out
     the irreducible as described in (2).

     But now, during canonicalization, we see the cast and remove it, in
     canEqCast. By the time we get into canEqCanLHS, the equality is
     heterogeneous again, and the process repeats.

     To avoid this, we don't strip casts off a type if the other type in the
     equality is a CanEqLHS (the scenario above can happen with a type
     family, too. testcase: typecheck/should_compile/T13822).

     And this is an improvement regardless: because tyvars can, generally,
     unify with casted types, there's no reason to go through the work of
     stripping off the cast when the cast appears opposite a tyvar. This is
     implemented in the cast case of can_eq_nc.

Historical note:

We used to do this via emitting a Derived kind equality and then parking
the heterogeneous equality as irreducible. But this new approach is much
more direct. And it doesn't produce duplicate Deriveds (as the old one did).

Note [Type synonyms and canonicalization]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We treat type synonym applications as xi types, that is, they do not
count as type function applications.  However, we do need to be a bit
careful with type synonyms: like type functions they may not be
generative or injective.  However, unlike type functions, they are
parametric, so there is no problem in expanding them whenever we see
them, since we do not need to know anything about their arguments in
order to expand them; this is what justifies not having to treat them
as specially as type function applications.  The thing that causes
some subtleties is that we prefer to leave type synonym applications
*unexpanded* whenever possible, in order to generate better error
messages.

If we encounter an equality constraint with type synonym applications
on both sides, or a type synonym application on one side and some sort
of type application on the other, we simply must expand out the type
synonyms in order to continue decomposing the equality constraint into
primitive equality constraints.  For example, suppose we have

  type F a = [Int]

and we encounter the equality

  F a ~ [b]

In order to continue we must expand F a into [Int], giving us the
equality

  [Int] ~ [b]

which we can then decompose into the more primitive equality
constraint

  Int ~ b.

However, if we encounter an equality constraint with a type synonym
application on one side and a variable on the other side, we should
NOT (necessarily) expand the type synonym, since for the purpose of
good error messages we want to leave type synonyms unexpanded as much
as possible.  Hence the ps_xi1, ps_xi2 argument passed to canEqCanLHS.

Note [Type equality cycles]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this situation (from indexed-types/should_compile/GivenLoop):

  instance C (Maybe b)
  *[G] a ~ Maybe (F a)
  [W] C a

or (typecheck/should_compile/T19682b):

  instance C (a -> b)
  *[W] alpha ~ (Arg alpha -> Res alpha)
  [W] C alpha

or (typecheck/should_compile/T21515):

  type family Code a
  *[G] Code a ~ '[ '[ Head (Head (Code a)) ] ]
  [W] Code a ~ '[ '[ alpha ] ]

In order to solve the final Wanted, we must use the starred constraint
for rewriting. But note that all starred constraints have occurs-check failures,
and so we can't straightforwardly add these to the inert set and
use them for rewriting. (NB: A rigid type constructor is at the
top of all RHSs, preventing reorienting in canEqTyVarFunEq in the tyvar
cases.)

The key idea is to replace the outermost type family applications in the RHS of
the starred constraints with a fresh variable, which we'll call a cycle-breaker
variable, or cbv. Then, relate the cbv back with the original type family
application via new equality constraints. Our situations thus become:

  instance C (Maybe b)
  [G] a ~ Maybe cbv
  [G] F a ~ cbv
  [W] C a

or

  instance C (a -> b)
  [W] alpha ~ (cbv1 -> cbv2)
  [W] Arg alpha ~ cbv1
  [W] Res alpha ~ cbv2
  [W] C alpha

or

  [G] Code a ~ '[ '[ cbv ] ]
  [G] Head (Head (Code a)) ~ cbv
  [W] Code a ~ '[ '[ alpha ] ]

This transformation (creating the new types and emitting new equality
constraints) is done by the `FamAppBreaker` field of `TEFA_Break`, which
in turn lives in the `tef_fam_app` field of `TyEqFlags`.  And that in
turn controls the behaviour of the workhorse: GHC.Tc.Utils.Unify.checkTyEqRhs.

The details depend on whether we're working with a Given or a Wanted.

Given
-----
We emit a new Given, [G] F a ~ cbv, equating the type family application
to our new cbv. This is actually done by `break_given` in
`GHC.Tc.Solver.Monad.checkTypeEq`.

Note its orientation: The type family ends up on the left; see
Note [Orienting TyFamLHS/TyFamLHS]. No special treatment for
CycleBreakerTvs is necessary. This scenario is now easily soluble, by using
the first Given to rewrite the Wanted, which can now be solved.

(The first Given actually also rewrites the second one, giving
[G] F (Maybe cbv) ~ cbv, but this causes no trouble.)

Of course, we don't want our fresh variables leaking into e.g. error
messages.  So we fill in the metavariables with their original type family
applications after we're done running the solver (in nestImplicTcS and
runTcSWithEvBinds).  This is done by `restoreTyVarCycles`, which uses the
`inert_cycle_breakers` field in InertSet, which contains the pairings
invented in `break_given`.

That is, we transform
  [G] g : lhs ~ ...(F lhs)...
to
  [G] (Refl lhs) : F lhs ~ cbv      -- CEqCan
  [G] g          : lhs ~ ...cbv...  -- CEqCan

Note that
* `cbv` is a fresh cycle breaker variable.
* `cbv` is a meta-tyvar, but it is completely untouchable.
* We track the cycle-breaker variables in inert_cycle_breakers in InertSet
* We eventually fill in the cycle-breakers, with `cbv := F lhs`.
  No one else fills in CycleBreakerTvs!
* The evidence for the new `F lhs ~ cbv` constraint is Refl, because we know
  this fill-in is ultimately going to happen.
* In `inert_cycle_breakers`, we remember the (cbv, F lhs) pair; that is, we
  remember the /original/ type.  The [G] F lhs ~ cbv constraint may be rewritten
  by other givens (eg if we have another [G] lhs ~ (b,c)), but at the end we
  still fill in with cbv := F lhs
* This fill-in is done when solving is complete, by restoreTyVarCycles
  in nestImplicTcS and runTcSWithEvBinds.

Wanted
------
First, we do not cycle-break unless the LHS is a unifiable type variable
See Note [Don't cycle-break Wanteds when not unifying] in GHC.Tc.Solver.Monad.

OK, so suppose the LHS is a unifiable type variable.  The fresh cycle-breaker
variables here must actually be normal, touchable metavariables. That is, they
are TauTvs. Nothing at all unusual. Repeating the example from above, we have

  *[W] alpha ~ (Arg alpha -> Res alpha)

and we turn this into

  *[W] alpha ~ (cbv1 -> cbv2)
  [W] Arg alpha ~ cbv1
  [W] Res alpha ~ cbv2

where cbv1 and cbv2 are fresh TauTvs.  This is actually done by `break_wanted`
in `GHC.Tc.Solver.Monad.checkTouchableTyVarEq`.

Why TauTvs? See [Why TauTvs] below.

Critically, we emit the two new constraints (the last two above)
directly instead of calling wrapUnifierTcS. (Otherwise, we'd end up
unifying cbv1 and cbv2 immediately, achieving nothing.)  Next, we
unify alpha := cbv1 -> cbv2, having eliminated the occurs check. This
unification happens immediately following a successful call to
checkTouchableTyVarEq, in canEqCanLHSFinish_try_unification.

Now, we're here (including further context from our original example,
from the top of the Note):

  instance C (a -> b)
  [W] Arg (cbv1 -> cbv2) ~ cbv1
  [W] Res (cbv1 -> cbv2) ~ cbv2
  [W] C (cbv1 -> cbv2)

The first two W constraints reduce to reflexivity and are discarded,
and the last is easily soluble.

[Why TauTvs]:
Let's look at another example (typecheck/should_compile/T19682) where we need
to unify the cbvs:

  class    (AllEqF xs ys, SameShapeAs xs ys) => AllEq xs ys
  instance (AllEqF xs ys, SameShapeAs xs ys) => AllEq xs ys

  type family SameShapeAs xs ys :: Constraint where
    SameShapeAs '[] ys      = (ys ~ '[])
    SameShapeAs (x : xs) ys = (ys ~ (Head ys : Tail ys))

  type family AllEqF xs ys :: Constraint where
    AllEqF '[]      '[]      = ()
    AllEqF (x : xs) (y : ys) = (x ~ y, AllEq xs ys)

  [W] alpha ~ (Head alpha : Tail alpha)
  [W] AllEqF '[Bool] alpha

Without the logic detailed in this Note, we're stuck here, as AllEqF cannot
reduce and alpha cannot unify. Let's instead apply our cycle-breaker approach,
just as described above. We thus invent cbv1 and cbv2 and unify
alpha := cbv1 -> cbv2, yielding (after zonking)

  [W] Head (cbv1 : cbv2) ~ cbv1
  [W] Tail (cbv1 : cbv2) ~ cbv2
  [W] AllEqF '[Bool] (cbv1 : cbv2)

The first two W constraints simplify to reflexivity and are discarded.
But the last reduces:

  [W] Bool ~ cbv1
  [W] AllEq '[] cbv2

The first of these is solved by unification: cbv1 := Bool. The second
is solved by the instance for AllEq to become

  [W] AllEqF '[] cbv2
  [W] SameShapeAs '[] cbv2

While the first of these is stuck, the second makes progress, to lead to

  [W] AllEqF '[] cbv2
  [W] cbv2 ~ '[]

This second constraint is solved by unification: cbv2 := '[]. We now
have

  [W] AllEqF '[] '[]

which reduces to

  [W] ()

which is trivially satisfiable. Hooray!

Note that we need to unify the cbvs here; if we did not, there would be
no way to solve those constraints. That's why the cycle-breakers are
ordinary TauTvs.

How all this is implemented
---------------------------
We implement all this via the `TEFA_Break` constructor of `TyEqFamApp`,
itself stored in the `tef_fam_app` field of `TyEqFlags`, which controls
the behaviour of `GHC.Tc.Utils.Unify.checkTyEqRhs`.  The `TEFA_Break`
stuff happens when `checkTyEqRhs` encounters a family application.

We try the cycle-breaking trick:
* For Wanteds, when there is a touchable unification variable on the left
* For Givens, regardless of the LHS

EXCEPT that, in both cases, as `GHC.Tc.Solver.Monad.mkTEFA_Break` shows, we
don't use this trick:

* When the constraint we are looking at was itself created by cycle-breaking;
  see Detail (7) below.

* For representational equalities, as there is no concrete use case where it is
  helpful (unlike for nominal equalities).

  Furthermore, because function applications can be CanEqLHSs, but newtype
  applications cannot, the disparities between the cases are enough that it
  would be effortful to expand the idea to representational equalities. A quick
  attempt, with
      data family N a b
      f :: (Coercible a (N a b), Coercible (N a b) b) => a -> b
      f = coerce
  failed with "Could not match 'b' with 'b'." Further work is held off
  until when we have a concrete incentive to explore this dark corner.

More details:

 (1) We don't look under foralls, at all, in `checkTyEqRhs`.  There might be
     a cyclic occurrence underneath, in a case like
          [G] lhs ~ forall b. ... lhs ....
     but it doesn't matter because we will classify the constraint as Irred,
     so it will not be used for rewriting.

     Earlier versions required an extra, post-breaking, check.  Skipping this
     check causes typecheck/should_fail/GivenForallLoop and polykinds/T18451 to
     loop.  But now it is all simpler, with no need for a second check.

 (2) Historical Note: our goal here is to avoid loops in rewriting. We can thus
     skip looking in coercions, as we don't rewrite in coercions in the
     algorithm in GHC.Solver.Rewrite.  This doesn't seem relevant any more.
     We cycle break to make the constraint canonical.

 (3) As we cycle-break as described in this Note, we can build ill-kinded
     types. For example, if we have Proxy (F a) b, where (b :: F a), then
     replacing this with Proxy cbv b is ill-kinded. However, we will later
     set cbv := F a, and so the zonked type will be well-kinded again.
     The temporary ill-kinded type hurts no one, and avoiding this would
     be quite painfully difficult.

     Specifically, this detail does not contravene the Purely Kinded Type Invariant
     (Note [The Purely Kinded Type Invariant (PKTI)] in GHC.Tc.Gen.HsType).
     The PKTI says that we can call typeKind on any type, without failure.
     It would be violated if we, say, replaced a kind (a -> b) with a kind c,
     because an arrow kind might be consulted in piResultTys. Here, we are
     replacing one opaque type like (F a b c) with another, cbv (opaque in
     that we never assume anything about its structure, like that it has a
     result type or a RuntimeRep argument).

 (4) The evidence for the produced Givens is all just reflexive, because we
     will eventually set the cycle-breaker variable to be the type family, and
     then, after the zonk, all will be well. See also the notes at the end of
     the Given section of this Note.

 (5) The implementation in `checkTyEqRhs` is efficient because it only replaces
     a type family application with a type variable, if that particular
     appplication is implicated in the occurs check.  For example:
         [W] alpha ~ Maybe (F alpha, G beta)
     We'll end up calling GHC.Tc.Utils.Unify.checkFamApp
       * On `F alpha`, which fail and calls the cycle-breaker in TEFA_Break
       * On `G beta`, which succeeds no problem.

     However, we make no attempt to detect cases like a ~ (F a, F a) and use the
     same tyvar to replace F a. The constraint solver will common them up later!
     (Cf. Note [Apartness and type families] in GHC.Core.Unify, which goes to
     this extra effort.) However, this is really a very small corner case.  The
     investment to craft a clever, performant solution seems unworthwhile.

 (6) We often get the predicate associated with a constraint from its evidence
     with ctPred. We thus must not only make sure the generated CEqCan's fields
     have the updated RHS type (that is, the one produced by replacing type
     family applications with fresh variables), but we must also update the
     evidence itself. This is done by the call to rewriteEqEvidence in
     canEqCanLHSFinish.

 (7) We don't wish to apply this magic on the equalities created
     by this very same process. Consider this, from
     typecheck/should_compile/ContextStack2:

       type instance TF (a, b) = (TF a, TF b)
       t :: (a ~ TF (a, Int)) => ...

       [G] a ~ TF (a, Int)

     The RHS reduces, so we get

       [G] a ~ (TF a, TF Int)

     We then break cycles, to get

       [G] g1 :: a ~ (cbv1, cbv2)
       [G] g2 :: TF a ~ cbv1
       [G] g3 :: TF Int ~ cbv2

     g1 gets added to the inert set, as written. But then g2 becomes
     the work item. g1 rewrites g2 to become

       [G] TF (cbv1, cbv2) ~ cbv1

     which then uses the type instance to become

       [G] (TF cbv1, TF cbv2) ~ cbv1

     which looks remarkably like the Given we started with. If left unchecked,
     this will end up breaking cycles again, looping ad infinitum (and
     resulting in a context-stack reduction error, not an outright loop). The
     solution is easy: don't break cycles on an equality generated by breaking
     cycles. Instead, we mark this final Given as a CIrredCan with a
     NonCanonicalReason with the soluble occurs-check bit set (only).

     We track these equalities by giving them a special CtOrigin,
     CycleBreakerOrigin. This works for both Givens and Wanteds, as we need the
     logic in the W case for e.g. typecheck/should_fail/T17139.  Because this
     logic needs to work for Wanteds, too, we cannot simply look for a
     CycleBreakerTv on the left: Wanteds don't use them.


**********************************************************************
*                                                                    *
                   Rewriting evidence
*                                                                    *
**********************************************************************
-}

rewriteEqEvidence :: RewriterSet        -- New rewriters
                                        -- See GHC.Tc.Types.Constraint
                                        -- Note [Wanteds rewrite Wanteds]
                  -> CtEvidence         -- Old evidence :: olhs ~ orhs (not swapped)
                                        --              or orhs ~ olhs (swapped)
                  -> SwapFlag
                  -> Reduction          -- lhs_co :: olhs ~ nlhs
                  -> Reduction          -- rhs_co :: orhs ~ nrhs
                  -> TcS CtEvidence     -- Of type nlhs ~ nrhs
-- With reductions (Reduction lhs_co nlhs) (Reduction rhs_co nrhs),
-- rewriteEqEvidence yields, for a given equality (Given g olhs orhs):
-- If not swapped
--      g1 : nlhs ~ nrhs = sym lhs_co ; g ; rhs_co
-- If swapped
--      g1 : nlhs ~ nrhs = sym lhs_co ; Sym g ; rhs_co
--
-- For a wanted equality (Wanted w), we do the dual thing:
-- New  w1 : nlhs ~ nrhs
-- If not swapped
--      w : olhs ~ orhs = lhs_co ; w1 ; sym rhs_co
-- If swapped
--      w : orhs ~ olhs = rhs_co ; sym w1 ; sym lhs_co
--
-- It's all a form of rewriteEvidence, specialised for equalities
rewriteEqEvidence new_rewriters old_ev swapped (Reduction lhs_co nlhs) (Reduction rhs_co nrhs)
  | NotSwapped <- swapped
  , isReflCo lhs_co      -- See Note [Rewriting with Refl]
  , isReflCo rhs_co
  = return (setCtEvPredType old_ev new_pred)

  | CtGiven (GivenCt { ctev_evar = old_evar }) <- old_ev
  = do { let new_tm = evCoercion ( mkSymCo lhs_co
                                  `mkTransCo` maybeSymCo swapped (mkCoVarCo old_evar)
                                  `mkTransCo` rhs_co)
       ; CtGiven <$> newGivenEvVar loc (new_pred, new_tm) }

  | CtWanted (WantedCt { ctev_dest = dest, ctev_rewriters = rewriters }) <- old_ev
  = do { let rewriters' = rewriters S.<> new_rewriters
       ; (new_ev, hole_co) <- newWantedEq loc rewriters' (ctEvRewriteRole old_ev) nlhs nrhs
       ; let co = maybeSymCo swapped $
                  lhs_co `mkTransCo` hole_co `mkTransCo` mkSymCo rhs_co
       ; setWantedEq dest co
       ; traceTcS "rewriteEqEvidence" (vcat [ ppr old_ev
                                            , ppr nlhs
                                            , ppr nrhs
                                            , ppr co
                                            , ppr new_rewriters ])
       ; return $ CtWanted new_ev }

  where
    new_pred = mkTcEqPredLikeEv old_ev nlhs nrhs
    loc      = ctEvLoc old_ev

{-
**********************************************************************
*                                                                    *
                   tryInertEqs
*                                                                    *
**********************************************************************

Note [Combining equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
   Inert:     g1 :: a ~ t
   Work item: g2 :: a ~ t

Then we can simply solve g2 from g1, thus g2 := g1.  Easy!
But it's not so simple:

* If t is a type variable, the equalties might be oriented differently:
      e.g. (g1 :: a~b) and (g2 :: b~a)
  So we look both ways round.  Hence the SwapFlag result to
  inertsCanDischarge.

* We can only do g2 := g1 if g1 can discharge g2; that depends on
  (a) the role and (b) the flavour.  E.g. a representational equality
  cannot discharge a nominal one; a Wanted cannot discharge a Given.
  The predicate is eqCanRewriteFR.

* Visibility. Suppose  S :: forall k. k -> Type, and consider unifying
      S @Type (a::Type)  ~   S @(Type->Type) (b::Type->Type)
  From the first argument we get (Type ~ Type->Type); from the second
  argument we get (a ~ b) which in turn gives (Type ~ Type->Type).
  See typecheck/should_fail/T16204c.

  That first argument is invisible in the source program (aside from
  visible type application), so we'd much prefer to get the error from
  the second. We track visibility in the uo_visible field of a TypeEqOrigin.
  We use this to prioritise visible errors (see GHC.Tc.Errors.tryReporters,
  the partition on isVisibleOrigin).

  So when combining two otherwise-identical equalites, we want to
  keep the visible one, and discharge the invisible one.  Hence the
  call to strictly_more_visible.
-}

tryInertEqs :: EqCt -> SolverStage ()
tryInertEqs work_item@(EqCt { eq_ev = ev, eq_eq_rel = eq_rel })
  = Stage $
    do { inerts <- getInertCans
       ; if | Just (ev_i, swapped) <- inertsCanDischarge inerts work_item
            -> do { setEvBindIfWanted ev EvCanonical $
                    evCoercion (maybeSymCo swapped $
                                downgradeRole (eqRelRole eq_rel)
                                              (ctEvRewriteRole ev_i)
                                              (ctEvCoercion ev_i))
                  ; stopWith ev "Solved from inert" }

            | otherwise
            -> continueWith () }

inertsCanDischarge :: InertCans -> EqCt
                   -> Maybe ( CtEvidence  -- The evidence for the inert
                            , SwapFlag )  -- Whether we need mkSymCo
inertsCanDischarge inerts (EqCt { eq_lhs = lhs_w, eq_rhs = rhs_w
                                , eq_ev = ev_w, eq_eq_rel = eq_rel })
  | (ev_i : _) <- [ ev_i | EqCt { eq_ev = ev_i, eq_rhs = rhs_i
                                , eq_eq_rel = eq_rel }
                                  <- findEq inerts lhs_w
                         , rhs_i `tcEqType` rhs_w
                         , inert_beats_wanted ev_i eq_rel ]
  =  -- Inert:     a ~ ty
     -- Work item: a ~ ty
    Just (ev_i, NotSwapped)

  | Just rhs_lhs <- canEqLHS_maybe rhs_w
  , (ev_i : _) <- [ ev_i | EqCt { eq_ev = ev_i, eq_rhs = rhs_i
                                , eq_eq_rel = eq_rel }
                             <- findEq inerts rhs_lhs
                         , rhs_i `tcEqType` canEqLHSType lhs_w
                         , inert_beats_wanted ev_i eq_rel ]
  =  -- Inert:     a ~ b
     -- Work item: b ~ a
     Just (ev_i, IsSwapped)

  where
    loc_w  = ctEvLoc ev_w
    flav_w = ctEvFlavour ev_w
    fr_w   = (flav_w, eq_rel)

    inert_beats_wanted ev_i eq_rel
      = -- eqCanRewriteFR:        see second bullet of Note [Combining equalities]
        -- strictly_more_visible: see last bullet of Note [Combining equalities]
        fr_i `eqCanRewriteFR` fr_w
        && not ((loc_w `strictly_more_visible` ctEvLoc ev_i)
                 && (fr_w `eqCanRewriteFR` fr_i))
      where
        fr_i = (ctEvFlavour ev_i, eq_rel)

    -- See Note [Combining equalities], final bullet
    strictly_more_visible loc1 loc2
       = not (isVisibleOrigin (ctLocOrigin loc2)) &&
         isVisibleOrigin (ctLocOrigin loc1)

inertsCanDischarge _ _ = Nothing



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

Note [Do not unify representational equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider   [W] alpha ~R# b
where alpha is touchable. Should we unify alpha := b?

Certainly not!  Unifying forces alpha and be to be the same; but they
only need to be representationally equal types.

For example, we might have another constraint [W] alpha ~# N b
where
  newtype N b = MkN b
and we want to get alpha := N b.

See also #15144, which was caused by unifying a representational
equality.

Note that it does however make sense to perform such unifications, as a last
resort, when doing top-level defaulting.
See Note [Defaulting representational equalities].

Note [Solve by unification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we solve
   alpha[n] ~ ty
by unification, there are two cases to consider

* TouchableSameLevel: if the ambient level is 'n', then
  we can simply update alpha := ty, and do nothing else

* TouchableOuterLevel free_metas n: if the ambient level is greater than
  'n' (the level of alpha), in addition to setting alpha := ty we must
  do two other things:

  1. Promote all the free meta-vars of 'ty' to level n.  After all,
     alpha[n] is at level n, and so if we set, say,
          alpha[n] := Maybe beta[m],
     we must ensure that when unifying beta we do skolem-escape checks
     etc relevant to level n.  Simple way to do that: promote beta to
     level n.

  2. Set the Unification Level Flag to record that a level-n unification has
     taken place. See Note [The Unification Level Flag] in GHC.Tc.Solver.Monad

NB: TouchableSameLevel is just an optimisation for TouchableOuterLevel. Promotion
would be a no-op, and setting the unification flag unnecessarily would just
make the solver iterate more often.  (We don't need to iterate when unifying
at the ambient level because of the kick-out mechanism.)
-}


{-********************************************************************
*                                                                    *
          Final wrap-up for equalities
*                                                                    *
********************************************************************-}

{- Note [Looking up primitive equalities in quantified constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For equalities (a ~# b) look up (a ~ b), and then do a superclass
selection. This avoids having to support quantified constraints whose
kind is not Constraint, such as (forall a. F a ~# b)

See
 * Note [Evidence for quantified constraints] in GHC.Core.Predicate
 * Note [Equality superclasses in quantified constraints]
   in GHC.Tc.Solver.Dict
-}

--------------------
tryQCsIrredEqCt :: IrredCt -> SolverStage ()
tryQCsIrredEqCt irred@(IrredCt { ir_ev = ev })
  | EqPred eq_rel t1 t2 <- classifyPredType (ctEvPred ev)
  = lookup_eq_in_qcis (CIrredCan irred) eq_rel t1 t2

  | otherwise  -- All the calls come from in this module, where we deal only with
               -- equalities, so ctEvPred ev) must be an equality. Indeed, we could
               -- pass eq_rel, t1, t2 as arguments, to avoid this can't-happen case,
               -- but it's not a hot path, and this is simple and robust
  = pprPanic "solveIrredEquality" (ppr ev)

--------------------
tryQCsEqCt :: EqCt -> SolverStage ()
tryQCsEqCt work_item@(EqCt { eq_lhs = lhs, eq_rhs = rhs, eq_eq_rel = eq_rel })
  = lookup_eq_in_qcis (CEqCan work_item) eq_rel (canEqLHSType lhs) rhs

--------------------
lookup_eq_in_qcis :: Ct -> EqRel -> TcType -> TcType -> SolverStage ()
-- The "final QCI check" checks to see if we have
--    [W] t1 ~# t2
-- and a Given quantified contraint like (forall a b. blah => a ~ b)
-- Why?  See Note [Looking up primitive equalities in quantified constraints]
-- See also GHC.Tc.Solver.Dict
-- Note [Equality superclasses in quantified constraints]
lookup_eq_in_qcis work_ct eq_rel lhs rhs
  = Stage $
    do { ev_binds_var <- getTcEvBindsVar
       ; ics <- getInertCans
       ; if isWanted ev                       -- Never look up Givens in quantified constraints
         && not (null (inert_insts ics))      -- Shortcut common case
         && not (isCoEvBindsVar ev_binds_var) -- See Note [Instances in no-evidence implications]
         then try_for_qci
         else continueWith () }
  where
    ev  = ctEvidence work_ct
    loc = ctEvLoc ev
    role = eqRelRole eq_rel

    try_for_qci  -- First try looking for (lhs ~ rhs)
       | Just (cls, tys) <- boxEqPred eq_rel lhs rhs
       = do { res <- matchLocalInst (mkClassPred cls tys) loc
            ; traceTcS "lookup_irred_in_qcis:1" (ppr (mkClassPred cls tys))
            ; case res of
                OneInst { cir_mk_ev = mk_ev }
                  -> chooseInstance ev (res { cir_mk_ev = mk_eq_ev cls tys mk_ev })
                _ -> try_swapping }
       | otherwise
       = continueWith ()

    try_swapping  -- Now try looking for (rhs ~ lhs)  (see #23333)
       | Just (cls, tys) <- boxEqPred eq_rel rhs lhs
       = do { res <- matchLocalInst (mkClassPred cls tys) loc
            ; traceTcS "lookup_irred_in_qcis:2" (ppr (mkClassPred cls tys))
            ; case res of
                OneInst { cir_mk_ev = mk_ev }
                  -> do { ev' <- rewriteEqEvidence emptyRewriterSet ev IsSwapped
                                      (mkReflRedn role rhs) (mkReflRedn role lhs)
                        ; chooseInstance ev' (res { cir_mk_ev = mk_eq_ev cls tys mk_ev }) }
                _ -> do { traceTcS "lookup_irred_in_qcis:3" (ppr work_ct)
                        ; continueWith () }}
       | otherwise
       = continueWith ()

    mk_eq_ev cls tys mk_ev evs
      | sc_id : rest <- classSCSelIds cls  -- Just one superclass for this
      = assert (null rest) $ case (mk_ev evs) of
          EvExpr e -> EvExpr (Var sc_id `mkTyApps` tys `App` e)
          ev       -> pprPanic "mk_eq_ev" (ppr ev)
      | otherwise = pprPanic "finishEqCt" (ppr work_ct)

{- Note [Instances in no-evidence implications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In #15290 we had
  [G] forall p q. Coercible p q => Coercible (m p) (m q))   -- Quantified
  [W] forall <no-ev> a. m (Int, IntStateT m a)
                          ~R#
                        m (Int, StateT Int m a)

The Given is an ordinary quantified constraint; the Wanted is an implication
equality that arises from
  [W] (forall a. t1) ~R# (forall a. t2)

But because the (t1 ~R# t2) is solved "inside a type" (under that forall a)
we can't generate any term evidence.  So we can't actually use that
lovely quantified constraint.  Alas!

This test arranges to ignore the instance-based solution under these
(rare) circumstances.   It's sad, but I  really don't see what else we can do.
-}


{-
**********************************************************************
*                                                                    *
    Functional dependencies for type families
*                                                                    *
**********************************************************************

Note [Reverse order of fundep equations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this scenario (from dependent/should_fail/T13135_simple):

  type Sig :: Type -> Type
  data Sig a = SigFun a (Sig a)

  type SmartFun :: forall (t :: Type). Sig t -> Type
  type family SmartFun sig = r | r -> sig where
    SmartFun @Type (SigFun @Type a sig) = a -> SmartFun @Type sig

  [W] SmartFun @kappa sigma ~ (Int -> Bool)

The injectivity of SmartFun allows us to produce two new equalities:

  [W] w1 :: Type ~ kappa
  [W] w2 :: SigFun @Type Int beta ~ sigma

for some fresh (beta :: SigType). The second Wanted here is actually
heterogeneous: the LHS has type Sig Type while the RHS has type Sig kappa.
Of course, if we solve the first wanted first, the second becomes homogeneous.

When looking for injectivity-inspired equalities, we work left-to-right,
producing the two equalities in the order written above. However, these
equalities are then passed into wrapUnifierTcS, which will fail, adding these
to the work list. However, crucially, the work list operates like a *stack*.
So, because we add w1 and then w2, we process w2 first. This is silly: solving
w1 would unlock w2. So we make sure to add equalities to the work
list in left-to-right order, which requires a few key calls to 'reverse'.

This treatment is also used for class-based functional dependencies, although
we do not have a program yet known to exhibit a loop there. It just seems
like the right thing to do.

When this was originally conceived, it was necessary to avoid a loop in T13135.
That loop is now avoided by continuing with the kind equality (not the type
equality) in canEqCanLHSHetero (see Note [Equalities with incompatible kinds]).
However, the idea of working left-to-right still seems worthwhile, and so the calls
to 'reverse' remain.

Note [Improvement orientation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also Note [Fundeps with instances, and equality orientation], which describes
the Exact Same Problem, with the same solution, but for functional dependencies.

A very delicate point is the orientation of equalities
arising from injectivity improvement (#12522).  Suppose we have
  type family F x = t | t -> x
  type instance F (a, Int) = (Int, G a)
where G is injective; and wanted constraints

  [W] F (alpha, beta) ~ (Int, <some type>)

The injectivity will give rise to constraints

  [W] gamma1 ~ alpha
  [W] Int ~ beta

The fresh unification variable gamma1 comes from the fact that we
can only do "partial improvement" here; see Section 5.2 of
"Injective type families for Haskell" (HS'15).

Now, it's very important to orient the equations this way round,
so that the fresh unification variable will be eliminated in
favour of alpha.  If we instead had
   [W] alpha ~ gamma1
then we would unify alpha := gamma1; and kick out the wanted
constraint.  But when we substitute it back in, it'd look like
   [W] F (gamma1, beta) ~ fuv
and exactly the same thing would happen again!  Infinite loop.

This all seems fragile, and it might seem more robust to avoid
introducing gamma1 in the first place, in the case where the
actual argument (alpha, beta) partly matches the improvement
template.  But that's a bit tricky, esp when we remember that the
kinds much match too; so it's easier to let the normal machinery
handle it.  Instead we are careful to orient the new
equality with the template on the left.  Delicate, but it works.

-}

--------------------
tryFunDeps :: EqRel -> EqCt -> SolverStage ()
tryFunDeps eq_rel work_item@(EqCt { eq_lhs = lhs, eq_ev = ev })
  | NomEq <- eq_rel
  , TyFamLHS tc args <- lhs
  = Stage $
    do { inerts <- getInertCans
       ; imp1 <- improveLocalFunEqs inerts tc args work_item
       ; imp2 <- improveTopFunEqs tc args work_item
       ; if (imp1 || imp2)
         then startAgainWith (mkNonCanonical ev)
         else continueWith () }
  | otherwise
  = nopStage ()

--------------------
improveTopFunEqs :: TyCon -> [TcType] -> EqCt -> TcS Bool
-- TyCon is definitely a type family
-- See Note [FunDep and implicit parameter reactions]
improveTopFunEqs fam_tc args (EqCt { eq_ev = ev, eq_rhs = rhs_ty })
  | isGiven ev = improveGivenTopFunEqs  fam_tc args ev rhs_ty
  | otherwise  = improveWantedTopFunEqs fam_tc args ev rhs_ty

improveGivenTopFunEqs :: TyCon -> [TcType] -> CtEvidence -> Xi -> TcS Bool
-- TyCon is definitely a type family
-- Work-item is a Given
improveGivenTopFunEqs fam_tc args ev rhs_ty
  | Just ops <- isBuiltInSynFamTyCon_maybe fam_tc
  = do { traceTcS "improveGivenTopFunEqs" (ppr fam_tc <+> ppr args $$ ppr ev $$ ppr rhs_ty)
       ; emitNewGivens (ctEvLoc ev) $
           [ (Nominal, new_co)
           | (ax, _) <- tryInteractTopFam ops fam_tc args rhs_ty
           , let new_co = mkAxiomCo ax [given_co] ]
       ; return False }  -- False: no unifications
  | otherwise
  = return False
  where
    given_co :: Coercion = ctEvCoercion ev

improveWantedTopFunEqs :: TyCon -> [TcType] -> CtEvidence -> Xi -> TcS Bool
-- TyCon is definitely a type family
-- Work-item is a Wanted
improveWantedTopFunEqs fam_tc args ev rhs_ty
  = do { eqns <- improve_wanted_top_fun_eqs fam_tc args rhs_ty
       ; traceTcS "improveTopFunEqs" (vcat [ text "lhs:" <+> ppr fam_tc <+> ppr args
                                           , text "rhs:" <+> ppr rhs_ty
                                           , text "eqns:" <+> ppr eqns ])
       ; unifyFunDeps ev Nominal $ \uenv ->
         uPairsTcM (bump_depth uenv) (reverse eqns) }
         -- Missing that `reverse` causes T13135 and T13135_simple to loop.
         -- See Note [Reverse order of fundep equations]

  where
    bump_depth env = env { u_loc = bumpCtLocDepth (u_loc env) }
        -- ToDo: this location is wrong; it should be FunDepOrigin2
        -- See #14778

improve_wanted_top_fun_eqs :: TyCon -> [TcType] -> Xi
                           -> TcS [TypeEqn]
-- TyCon is definitely a type family
improve_wanted_top_fun_eqs fam_tc lhs_tys rhs_ty
  | Just ops <- isBuiltInSynFamTyCon_maybe fam_tc
  = return (map snd $ tryInteractTopFam ops fam_tc lhs_tys rhs_ty)

  -- See Note [Type inference for type families with injectivity]
  | Injective inj_args <- tyConInjectivityInfo fam_tc
  = do { fam_envs <- getFamInstEnvs
       ; top_eqns <- improve_injective_wanted_top fam_envs inj_args fam_tc lhs_tys rhs_ty
       ; let local_eqns = improve_injective_wanted_famfam  inj_args fam_tc lhs_tys rhs_ty
       ; traceTcS "improve_wanted_top_fun_eqs" $
         vcat [ ppr fam_tc, text "local_eqns" <+> ppr local_eqns, text "top_eqns" <+> ppr top_eqns ]
       ; return (local_eqns ++ top_eqns) }

  | otherwise  -- No injectivity
  = return []

improve_injective_wanted_top :: FamInstEnvs -> [Bool] -> TyCon -> [TcType] -> Xi -> TcS [TypeEqn]
-- Interact with top-level instance declarations
improve_injective_wanted_top fam_envs inj_args fam_tc lhs_tys rhs_ty
  = concatMapM do_one branches
  where
    branches :: [CoAxBranch]
    branches | isOpenTypeFamilyTyCon fam_tc
             , let fam_insts = lookupFamInstEnvByTyCon fam_envs fam_tc
             = concatMap (fromBranches . coAxiomBranches . fi_axiom) fam_insts

             | Just ax <- isClosedSynFamilyTyConWithAxiom_maybe fam_tc
             = fromBranches (coAxiomBranches ax)

             | otherwise
             = []

    do_one :: CoAxBranch -> TcS [TypeEqn]
    do_one branch@(CoAxBranch { cab_tvs = branch_tvs, cab_lhs = branch_lhs_tys, cab_rhs = branch_rhs })
      | let in_scope1 = in_scope `extendInScopeSetList` branch_tvs
      , Just subst <- tcUnifyTyForInjectivity False in_scope1 branch_rhs rhs_ty
      = do { let inSubst tv = tv `elemVarEnv` getTvSubstEnv subst
                 unsubstTvs = filterOut inSubst branch_tvs
                 -- The order of unsubstTvs is important; it must be
                 -- in telescope order e.g. (k:*) (a:k)

           ; subst1 <- instFlexiX subst unsubstTvs
                -- If the current substitution bind [k -> *], and
                -- one of the un-substituted tyvars is (a::k), we'd better
                -- be sure to apply the current substitution to a's kind.
                -- Hence instFlexiX.   #13135 was an example.

           ; traceTcS "improve_inj_top" $
             vcat [ text "branch_rhs" <+> ppr branch_rhs
                  , text "rhs_ty" <+> ppr rhs_ty
                  , text "subst" <+> ppr subst
                  , text "subst1" <+> ppr subst1 ]
           ; if apartnessCheck (substTys subst1 branch_lhs_tys) branch
             then do { traceTcS "improv_inj_top1" (ppr branch_lhs_tys)
                     ; return (mkInjectivityEqns inj_args (map (substTy subst1) branch_lhs_tys) lhs_tys) }
                  -- NB: The fresh unification variables (from unsubstTvs) are on the left
                  --     See Note [Improvement orientation]
             else do { traceTcS "improve_inj_top2" empty; return []  } }
      | otherwise
      = do { traceTcS "improve_inj_top:fail" (ppr branch_rhs $$ ppr rhs_ty $$ ppr in_scope $$ ppr branch_tvs)
           ; return [] }

    in_scope = mkInScopeSet (tyCoVarsOfType rhs_ty)


improve_injective_wanted_famfam :: [Bool] -> TyCon -> [TcType] -> Xi -> [TypeEqn]
-- Interact with itself, specifically  F s1 s2 ~ F t1 t2
improve_injective_wanted_famfam inj_args fam_tc lhs_tys rhs_ty
  | Just (tc, rhs_tys) <- tcSplitTyConApp_maybe rhs_ty
  , tc == fam_tc
  = mkInjectivityEqns inj_args lhs_tys rhs_tys
  | otherwise
  = []

mkInjectivityEqns :: [Bool] -> [TcType] -> [TcType] -> [TypeEqn]
-- When F s1 s2 s3 ~ F t1 t2 t3, and F has injectivity info [True,False,True]
-- return the equations [Pair s1 t1, Pair s3 t3]
mkInjectivityEqns inj_args lhs_args rhs_args
  = [ Pair lhs_arg rhs_arg | (True, lhs_arg, rhs_arg) <- zip3 inj_args lhs_args rhs_args ]

---------------------------------------------
improveLocalFunEqs :: InertCans
                   -> TyCon -> [TcType] -> EqCt   -- F args ~ rhs
                   -> TcS Bool
-- Emit equalities from interaction between two equalities
improveLocalFunEqs inerts fam_tc args (EqCt { eq_ev = work_ev, eq_rhs = rhs })
  | isGiven work_ev = improveGivenLocalFunEqs  funeqs_for_tc fam_tc args work_ev rhs
  | otherwise       = improveWantedLocalFunEqs funeqs_for_tc fam_tc args work_ev rhs
  where
    funeqs = inert_funeqs inerts
    funeqs_for_tc :: [EqCt]   -- Mixture of Given and Wanted
    funeqs_for_tc = [ funeq_ct | equal_ct_list <- findFunEqsByTyCon funeqs fam_tc
                               , funeq_ct <- equal_ct_list
                               , NomEq == eq_eq_rel funeq_ct ]
                                  -- Representational equalities don't interact
                                  -- with type family dependencies


improveGivenLocalFunEqs :: [EqCt]    -- Inert items, mixture of Given and Wanted
                        -> TyCon -> [TcType] -> CtEvidence -> Xi  -- Work item (Given)
                        -> TcS Bool  -- Always False (no unifications)
-- Emit equalities from interaction between two Given type-family equalities
--    e.g.    (x+y1~z, x+y2~z) => (y1 ~ y2)
improveGivenLocalFunEqs funeqs_for_tc fam_tc work_args work_ev work_rhs
  | Just ops <- isBuiltInSynFamTyCon_maybe fam_tc
  = do { mapM_ (do_one ops) funeqs_for_tc
       ; return False }     -- False: no unifications
  | otherwise
  = return False
  where
    given_co :: Coercion = ctEvCoercion work_ev

    do_one :: BuiltInSynFamily -> EqCt -> TcS ()
    -- Used only work-item is Given
    do_one ops EqCt { eq_ev  = inert_ev, eq_lhs = inert_lhs, eq_rhs = inert_rhs }
      | isGiven inert_ev                    -- Given/Given interaction
      , TyFamLHS _ inert_args <- inert_lhs  -- Inert item is F inert_args ~ inert_rhs
      , work_rhs `tcEqType` inert_rhs       -- Both RHSs are the same
      , -- So we have work_ev  : F work_args  ~ rhs
        --            inert_ev : F inert_args ~ rhs
        let pairs :: [(CoAxiomRule, TypeEqn)]
            pairs = tryInteractInertFam ops fam_tc work_args inert_args
      , not (null pairs)
      = do { traceTcS "improveGivenLocalFunEqs" (vcat[ ppr fam_tc <+> ppr work_args
                                                     , text "work_ev" <+>  ppr work_ev
                                                     , text "inert_ev" <+> ppr inert_ev
                                                     , ppr work_rhs
                                                     , ppr pairs ])
           ; emitNewGivens (ctEvLoc inert_ev) (map mk_ax_co pairs) }
             -- This CtLoc for the new Givens doesn't reflect the
             -- fact that it's a combination of Givens, but I don't
             -- this that matters.
      where
        inert_co = ctEvCoercion inert_ev
        mk_ax_co (ax,_) = (Nominal, mkAxiomCo ax [combined_co])
          where
            combined_co = given_co `mkTransCo` mkSymCo inert_co
            -- given_co :: F work_args  ~ rhs
            -- inert_co :: F inert_args ~ rhs
            -- the_co :: F work_args ~ F inert_args

    do_one _  _ = return ()

improveWantedLocalFunEqs
    :: [EqCt]     -- Inert items (Given and Wanted)
    -> TyCon -> [TcType] -> CtEvidence -> Xi  -- Work item (Wanted)
    -> TcS Bool   -- True <=> some unifications
-- Emit improvement equalities for a Wanted constraint, by comparing
-- the current work item with inert CFunEqs (boh Given and Wanted)
-- E.g.   x + y ~ z,   x + y' ~ z   =>   [W] y ~ y'
--
-- See Note [FunDep and implicit parameter reactions]
improveWantedLocalFunEqs funeqs_for_tc fam_tc args work_ev rhs
  | null improvement_eqns
  = return False
  | otherwise
  = do { traceTcS "interactFunEq improvements: " $
                   vcat [ text "Eqns:" <+> ppr improvement_eqns
                        , text "Candidates:" <+> ppr funeqs_for_tc ]
       ; emitFunDepWanteds work_ev improvement_eqns }
  where
    work_loc      = ctEvLoc work_ev
    work_pred     = ctEvPred work_ev
    fam_inj_info  = tyConInjectivityInfo fam_tc

    --------------------
    improvement_eqns :: [FunDepEqn (CtLoc, RewriterSet)]
    improvement_eqns
      | Just ops <- isBuiltInSynFamTyCon_maybe fam_tc
      =    -- Try built-in families, notably for arithmethic
        concatMap (do_one_built_in ops rhs) funeqs_for_tc

      | Injective injective_args <- fam_inj_info
      =    -- Try improvement from type families with injectivity annotations
        concatMap (do_one_injective injective_args rhs) funeqs_for_tc

      | otherwise
      = []

    --------------------
    do_one_built_in ops rhs (EqCt { eq_lhs = TyFamLHS _ iargs, eq_rhs = irhs, eq_ev = inert_ev })
      | irhs `tcEqType` rhs
      = mk_fd_eqns inert_ev (map snd $ tryInteractInertFam ops fam_tc args iargs)
      | otherwise
      = []
    do_one_built_in _ _ _ = pprPanic "interactFunEq 1" (ppr fam_tc) -- TyVarLHS

    --------------------
    -- See Note [Type inference for type families with injectivity]
    do_one_injective inj_args rhs (EqCt { eq_lhs = TyFamLHS _ inert_args
                                        , eq_rhs = irhs, eq_ev = inert_ev })
      | rhs `tcEqType` irhs
      = mk_fd_eqns inert_ev $ mkInjectivityEqns inj_args args inert_args
      | otherwise
      = []

    do_one_injective _ _ _ = pprPanic "interactFunEq 2" (ppr fam_tc)  -- TyVarLHS

    --------------------
    mk_fd_eqns :: CtEvidence -> [TypeEqn] -> [FunDepEqn (CtLoc, RewriterSet)]
    mk_fd_eqns inert_ev eqns
      | null eqns  = []
      | otherwise  = [ FDEqn { fd_qtvs = [], fd_eqs = eqns
                             , fd_loc   = (loc, inert_rewriters) } ]
      where
        initial_loc  -- start with the location of the Wanted involved
          | isGiven work_ev = inert_loc
          | otherwise       = work_loc
        eqn_orig        = InjTFOrigin1 work_pred  (ctLocOrigin work_loc)  (ctLocSpan work_loc)
                                       inert_pred (ctLocOrigin inert_loc) (ctLocSpan inert_loc)
        eqn_loc         = setCtLocOrigin initial_loc eqn_orig
        inert_pred      = ctEvPred inert_ev
        inert_loc       = ctEvLoc inert_ev
        inert_rewriters = ctEvRewriters inert_ev
        loc = eqn_loc { ctl_depth = ctl_depth inert_loc `maxSubGoalDepth`
                                    ctl_depth work_loc }

{- Note [Type inference for type families with injectivity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have a type family with an injectivity annotation:
    type family F a b = r | r -> b

Then if we have an equality like F s1 t1 ~ F s2 t2,
we can use the injectivity to get a new Wanted constraint on
the injective argument
  [W] t1 ~ t2

That in turn can help GHC solve constraints that would otherwise require
guessing.  For example, consider the ambiguity check for
   f :: F Int b -> Int
We get the constraint
   [W] F Int b ~ F Int beta
where beta is a unification variable.  Injectivity lets us pick beta ~ b.

Injectivity information is also used at the call sites. For example:
   g = f True
gives rise to
   [W] F Int b ~ Bool
from which we can derive b.  This requires looking at the defining equations of
a type family, ie. finding equation with a matching RHS (Bool in this example)
and inferring values of type variables (b in this example) from the LHS patterns
of the matching equation.  For closed type families we have to perform
additional apartness check for the selected equation to check that the selected
is guaranteed to fire for given LHS arguments.

These new constraints are Wanted constraints, but we will not use the evidence.
We could go further and offer evidence from decomposing injective type-function
applications, but that would require new evidence forms, and an extension to
FC, so we don't do that right now (Dec 14).

We generate these Wanteds in three places, depending on how we notice the
injectivity.

1. When we have a [W] F tys1 ~ F tys2. This is handled in canEqCanLHS2, and
described in Note [Decomposing type family applications] in GHC.Tc.Solver.Equality

2. When we have [W] F tys1 ~ T and [W] F tys2 ~ T. Note that neither of these
constraints rewrites the other, as they have different LHSs. This is done
in improveLocalFunEqs, called during the interactWithInertsStage.

3. When we have [W] F tys ~ T and an equation for F that looks like F tys' = T.
This is done in improve_top_fun_eqs, called from the top-level reactions stage.

See also Note [Injective type families] in GHC.Core.TyCon

Note [Cache-caused loops]
~~~~~~~~~~~~~~~~~~~~~~~~~
It is very dangerous to cache a rewritten wanted family equation as 'solved' in our
solved cache (which is the default behaviour or xCtEvidence), because the interaction
may not be contributing towards a solution. Here is an example:

Initial inert set:
  [W] g1 : F a ~ beta1
Work item:
  [W] g2 : F a ~ beta2
The work item will react with the inert yielding the _same_ inert set plus:
    (i)   Will set g2 := g1 `cast` g3
    (ii)  Will add to our solved cache that [S] g2 : F a ~ beta2
    (iii) Will emit [W] g3 : beta1 ~ beta2
Now, the g3 work item will be spontaneously solved to [G] g3 : beta1 ~ beta2
and then it will react the item in the inert ([W] g1 : F a ~ beta1). So it
will set
      g1 := g ; sym g3
and what is g? Well it would ideally be a new goal of type (F a ~ beta2) but
remember that we have this in our solved cache, and it is ... g2! In short we
created the evidence loop:

        g2 := g1 ; g3
        g3 := refl
        g1 := g2 ; sym g3

To avoid this situation we do not cache as solved any workitems (or inert)
which did not really made a 'step' towards proving some goal. Solved's are
just an optimization so we don't lose anything in terms of completeness of
solving.
-}

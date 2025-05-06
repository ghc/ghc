-- | Tidying types and coercions for printing in error messages.
module GHC.Core.TyCo.Tidy
  (
        -- * Tidying type related things up for printing
        tidyType, tidyTypes,
        tidyCo,   tidyCos,
        tidyTopType,

        tidyOpenType,  tidyOpenTypes,
        tidyOpenTypeX, tidyOpenTypesX,
        tidyFreeTyCoVars, tidyFreeTyCoVarX, tidyFreeTyCoVarsX,

        tidyAvoiding,
        tidyVarBndr, tidyVarBndrs, avoidNameClashes,
        tidyForAllTyBinder, tidyForAllTyBinders,
        tidyTyCoVarOcc
  ) where

import GHC.Prelude
import GHC.Data.FastString

import GHC.Core.Predicate( scopedSort )
import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.FVs
import GHC.Types.Name hiding (varName)
import GHC.Types.Var
import GHC.Types.Var.Env
import GHC.Utils.Misc (strictMap)

import Data.List (mapAccumL)

{- **********************************************************************

                  TidyType

********************************************************************** -}

{- Note [Tidying open types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When tidying some open types [t1,..,tn], we find their free vars, and tidy them first.

But (tricky point) we restrict the occ_env part of inner_env to just the /free/
vars of [t1..tn], so that we don't gratuitously rename the /bound/ variables.

Example: assume the TidyEnv
      ({"a1","b"} , [a_4 :-> a1, b_7 :-> b])
and call tidyOpenTypes on
      [a_1, forall a_2. Maybe (a_2,a_4), forall b. (b,a_1)]
All the a's have the same OccName, but different uniques.

The TidyOccEnv binding for "b" relates b_7, which doesn't appear free in the
these types at all, so we don't want that to mess up the tidying for the
(forall b...).

So we proceed as follows:
  1. Find the free vars.
     In our example:the free vars are a_1 and a_4:

  2. Use tidyFreeTyCoVars to tidy them (workhorse: `tidyFreeCoVarX`)
     In our example:
      * a_4 already has a tidy form, a1, so don't change that
      * a_1 gets tidied to a2

  3. Trim the TidyOccEnv to OccNames of the tidied free vars (`trimTidyEnv`)
     In our example "a1" and "a2"

  4. Now tidy the types.  In our example we get
      [a2, forall a3. Maybe (a3,a1), forall b. (b, a2)]

Note [Tidying is idempotent]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Key invariant: tidyFreeTyCoVars is idempotent, at least if you start with
an empty TidyEnv. This is important because:

  * The typechecker error message processing carefully tidies types, using
    global knowledge; see for example calls to `tidyCt` in GHC.Tc.Errors.

  * Then the type pretty-printer, GHC.Core.TyCo.Ppr.pprType tidies the type
    again, because that's important for pretty-printing types in general.

But the second tidying is a no-op if the first step has happened, because
all the free vars will have distinct OccNames, so no renaming needs to happen.

Note [tidyAvoiding]
~~~~~~~~~~~~~~~~~~~
Consider tidying this unsolved constraint in GHC.Tc.Errors.report_unsolved.
    C a_33, (forall a. Eq a => D a)
Here a_33 is a free unification variable.  If we firs tidy [a_33 :-> "a"]
then we have no choice but to tidy the `forall a` to something else. But it
is confusing (sometimes very confusing) to gratuitously rename skolems in
this way -- see #24868.  So it is better to :

  * Find the /bound/ skolems (just `a` in this case)
  * Initialise the TidyOccEnv to avoid using "a"
  * Now tidy the free a_33 to, say, "a1"
  * Delete "a" from the TidyOccEnv

This is done by `tidyAvoiding`.

The last step is very important; if we leave "a" in the TidyOccEnv, when
we get to the (forall a. blah) we'll rename `a` to "a2", avoiding "a".
-}

-- | This tidies up a type for printing in an error message, or in
-- an interface file.
--
-- It doesn't change the uniques at all, just the print names.
tidyVarBndrs :: TidyEnv -> [TyCoVar] -> (TidyEnv, [TyCoVar])
tidyVarBndrs tidy_env tvs
  = mapAccumL tidyVarBndr (avoidNameClashes tvs tidy_env) tvs

tidyVarBndr :: TidyEnv -> TyCoVar -> (TidyEnv, TyCoVar)
tidyVarBndr tidy_env@(occ_env, subst) var
  = case tidyOccName occ_env (getHelpfulOccName var) of
      (occ_env', occ') -> ((occ_env', subst'), var')
        where
          subst' = extendVarEnv subst var var'
          var'   = updateVarType (tidyType tidy_env) (setVarName var name')
          name'  = tidyNameOcc name occ'
          name   = varName var

avoidNameClashes :: [TyCoVar] -> TidyEnv -> TidyEnv
-- Seed the occ_env with clashes among the names, see
-- Note [Tidying multiple names at once] in GHC.Types.Name.Occurrence
avoidNameClashes tvs (occ_env, subst)
  = (avoidClashesOccEnv occ_env occs, subst)
  where
    occs = map getHelpfulOccName tvs

getHelpfulOccName :: TyCoVar -> OccName
-- A TcTyVar with a System Name is probably a
-- unification variable; when we tidy them we give them a trailing
-- "0" (or 1 etc) so that they don't take precedence for the
-- un-modified name. Plus, indicating a unification variable in
-- this way is a helpful clue for users
getHelpfulOccName tv
  | isSystemName name, isTcTyVar tv
  = mkTyVarOccFS (occNameFS occ `appendFS` fsLit "0")
  | otherwise
  = occ
  where
   name = varName tv
   occ  = getOccName name

tidyForAllTyBinder :: TidyEnv -> VarBndr TyCoVar vis
                  -> (TidyEnv, VarBndr TyCoVar vis)
tidyForAllTyBinder tidy_env (Bndr tv vis)
  = (tidy_env', Bndr tv' vis)
  where
    (tidy_env', tv') = tidyVarBndr tidy_env tv

tidyForAllTyBinders :: TidyEnv -> [VarBndr TyCoVar vis]
                   -> (TidyEnv, [VarBndr TyCoVar vis])
tidyForAllTyBinders tidy_env tvbs
  = mapAccumL tidyForAllTyBinder
              (avoidNameClashes (binderVars tvbs) tidy_env) tvbs

---------------
tidyFreeTyCoVars :: TidyEnv -> [TyCoVar] -> TidyEnv
-- ^ Add the free 'TyVar's to the env in tidy form,
-- so that we can tidy the type they are free in
-- Precondition: input free vars are closed over kinds and
-- This function does a scopedSort, so that tidied variables
-- have tidied kinds.
-- See Note [Tidying is idempotent]
tidyFreeTyCoVars tidy_env tyvars = fst (tidyFreeTyCoVarsX tidy_env tyvars)

---------------
tidyFreeTyCoVarsX :: TidyEnv -> [TyCoVar] -> (TidyEnv, [TyCoVar])
-- Precondition: input free vars are closed over kinds and
-- This function does a scopedSort, so that tidied variables
-- have tidied kinds.
-- See Note [Tidying is idempotent]
tidyFreeTyCoVarsX env tyvars = mapAccumL tidyFreeTyCoVarX env $
                               scopedSort tyvars

---------------
tidyFreeTyCoVarX :: TidyEnv -> TyCoVar -> (TidyEnv, TyCoVar)
-- ^ Treat a new 'TyCoVar' as a binder, and give it a fresh tidy name
-- using the environment if one has not already been allocated. See
-- also 'tidyVarBndr'
-- See Note [Tidying is idempotent]
tidyFreeTyCoVarX env@(_, subst) tyvar
  = case lookupVarEnv subst tyvar of
        Just tyvar' -> (env, tyvar')           -- Already substituted
        Nothing     -> tidyVarBndr env tyvar  -- Treat it as a binder

---------------
tidyTyCoVarOcc :: TidyEnv -> TyCoVar -> TyCoVar
tidyTyCoVarOcc env@(_, subst) tcv
  = case lookupVarEnv subst tcv of
        Nothing   -> updateVarType (tidyType env) tcv
        Just tcv' -> tcv'

---------------

{-
Note [Strictness in tidyType and friends]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Since the result of tidying will be inserted into the HPT, a potentially
long-lived structure, we generally want to avoid pieces of the old AST
being retained by the thunks produced by tidying.

For this reason we take great care to ensure that all pieces of the tidied AST
are evaluated strictly.  So you will see lots of strict applications ($!) and
uses of `strictMap` in `tidyType`, `tidyTypes` and `tidyCo`.

In the case of tidying of lists (e.g. lists of arguments) we prefer to use
`strictMap f xs` rather than `seqList (map f xs)` as the latter will
unnecessarily allocate a thunk, which will then be almost-immediately
evaluated, for each list element.

Making `tidyType` strict has a rather large effect on performance: see #14738.
Sometimes as much as a 5% reduction in allocation.
-}

-- | Tidy a list of Types
--
-- See Note [Strictness in tidyType and friends]
tidyTypes :: TidyEnv -> [Type] -> [Type]
tidyTypes env tys = strictMap (tidyType env) tys

---------------


-- | Tidy a Type
--
-- See Note [Strictness in tidyType and friends]
tidyType :: TidyEnv -> Type -> Type
tidyType _   t@(LitTy {})           = t -- Preserve sharing
tidyType env (TyVarTy tv)           = TyVarTy $! tidyTyCoVarOcc env tv
tidyType _   t@(TyConApp _ [])      = t -- Preserve sharing if possible
tidyType env (TyConApp tycon tys)   = TyConApp tycon $! tidyTypes env tys
tidyType env (AppTy fun arg)        = (AppTy $! (tidyType env fun)) $! (tidyType env arg)
tidyType env (CastTy ty co)         = (CastTy $! tidyType env ty) $! (tidyCo env co)
tidyType env (CoercionTy co)        = CoercionTy $! (tidyCo env co)
tidyType env ty@(FunTy _ w arg res) = let { !w'   = tidyType env w
                                          ; !arg' = tidyType env arg
                                          ; !res' = tidyType env res }
                                      in ty { ft_mult = w', ft_arg = arg', ft_res = res' }
tidyType env (ty@(ForAllTy{}))      = tidyForAllType env ty


tidyForAllType :: TidyEnv -> Type -> Type
tidyForAllType env ty
   = (mkForAllTys' $! (zip tcvs' vis)) $! tidyType body_env body_ty
  where
    (tcvs, vis, body_ty) = splitForAllTyCoVars' ty
    (body_env, tcvs') = tidyVarBndrs env tcvs

-- The following two functions differ from mkForAllTys and splitForAllTyCoVars in that
-- they expect/preserve the ForAllTyFlag argument. These belong to "GHC.Core.Type", but
-- how should they be named?
mkForAllTys' :: [(TyCoVar, ForAllTyFlag)] -> Type -> Type
mkForAllTys' tvvs ty = foldr strictMkForAllTy ty tvvs
  where
    strictMkForAllTy (tv,vis) ty = (ForAllTy $! ((Bndr $! tv) $! vis)) $! ty

splitForAllTyCoVars' :: Type -> ([TyCoVar], [ForAllTyFlag], Type)
splitForAllTyCoVars' ty = go ty [] []
  where
    go (ForAllTy (Bndr tv vis) ty) tvs viss = go ty (tv:tvs) (vis:viss)
    go ty                          tvs viss = (reverse tvs, reverse viss, ty)


---------------
tidyAvoiding :: [OccName]
             -> (TidyEnv -> a -> TidyEnv)
             -> a -> TidyEnv
-- Initialise an empty TidyEnv with some bound vars to avoid,
-- run the do_tidy function, and then remove the bound vars again.
-- See Note [tidyAvoiding]
tidyAvoiding bound_var_avoids do_tidy thing
  = (occs' `delTidyOccEnvList` bound_var_avoids, vars')
  where
    (occs', vars') = do_tidy init_tidy_env thing
    init_tidy_env  = mkEmptyTidyEnv (initTidyOccEnv bound_var_avoids)

---------------
trimTidyEnv :: TidyEnv -> [TyCoVar] -> TidyEnv
trimTidyEnv (occ_env, var_env) tcvs
  = (trimTidyOccEnv occ_env (map getOccName tcvs), var_env)

---------------
-- | Grabs the free type variables, tidies them
-- and then uses 'tidyType' to work over the type itself
tidyOpenTypesX :: TidyEnv -> [Type] -> (TidyEnv, [Type])
-- See Note [Tidying open types]
tidyOpenTypesX env tys
  = (env1, tidyTypes inner_env tys)
  where
    free_tcvs :: [TyCoVar] -- Closed over kinds
    free_tcvs          = tyCoVarsOfTypesList tys
    (env1, free_tcvs') = tidyFreeTyCoVarsX env free_tcvs
    inner_env          = trimTidyEnv env1 free_tcvs'

---------------
tidyOpenTypeX :: TidyEnv -> Type -> (TidyEnv, Type)
-- See Note [Tidying open types]
tidyOpenTypeX env ty
  = (env1, tidyType inner_env ty)
  where
    free_tcvs          = tyCoVarsOfTypeList ty
    (env1, free_tcvs') = tidyFreeTyCoVarsX env free_tcvs
    inner_env          = trimTidyEnv env1 free_tcvs'

---------------
tidyOpenTypes :: TidyEnv -> [Type] -> [Type]
tidyOpenTypes env ty = snd (tidyOpenTypesX env ty)

tidyOpenType :: TidyEnv -> Type -> Type
tidyOpenType env ty = snd (tidyOpenTypeX env ty)

---------------
-- | Calls 'tidyType' on a top-level type (i.e. with an empty tidying environment)
tidyTopType :: Type -> Type
tidyTopType ty = tidyType emptyTidyEnv ty

---------------

-- | Tidy a Coercion
--
-- See Note [Strictness in tidyType and friends]
tidyCo :: TidyEnv -> Coercion -> Coercion
tidyCo env co
  = go co
  where
    go_mco MRefl    = MRefl
    go_mco (MCo co) = MCo $! go co

    go (Refl ty)             = Refl $! tidyType env ty
    go (GRefl r ty mco)      = (GRefl r $! tidyType env ty) $! go_mco mco
    go (TyConAppCo r tc cos) = TyConAppCo r tc $! strictMap go cos
    go (AppCo co1 co2)       = (AppCo $! go co1) $! go co2
    go (ForAllCo tv visL visR h co)
      = ((((ForAllCo $! tvp) $! visL) $! visR) $! (go h)) $! (tidyCo envp co)
      where (envp, tvp) = tidyVarBndr env tv
            -- the case above duplicates a bit of work in tidying h and the kind
            -- of tv. But the alternative is to use coercionKind, which seems worse.
    go (FunCo r afl afr w co1 co2) = ((FunCo r afl afr $! go w) $! go co1) $! go co2
    go (CoVarCo cv)          = CoVarCo $! go_cv cv
    go (HoleCo h)            = HoleCo $! go_hole h
    go (AxiomCo ax cos)      = AxiomCo ax $ strictMap go cos
    go (UnivCo prov role t1 t2 cos)
                             = ((UnivCo prov role
                                $! tidyType env t1)
                                $! tidyType env t2)
                                $! strictMap go cos
    go (SymCo co)            = SymCo $! go co
    go (TransCo co1 co2)     = (TransCo $! go co1) $! go co2
    go (SelCo d co)          = SelCo d $! go co
    go (LRCo lr co)          = LRCo lr $! go co
    go (InstCo co ty)        = (InstCo $! go co) $! go ty
    go (KindCo co)           = KindCo $! go co
    go (SubCo co)            = SubCo $! go co

    go_cv cv = tidyTyCoVarOcc env cv

    go_hole (CoercionHole cv r) = (CoercionHole $! go_cv cv) r
    -- Tidy even the holes; tidied types should have tidied kinds

tidyCos :: TidyEnv -> [Coercion] -> [Coercion]
tidyCos env = strictMap (tidyCo env)

{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}

-- | Tidying types and coercions for printing in error messages.
module GHC.Core.TyCo.Tidy
  (
        -- * Tidying type related things up for printing
        tidyType,      tidyTypes,
        tidyOpenType,  tidyOpenTypes,
        tidyVarBndr, tidyVarBndrs, tidyFreeTyCoVars, avoidNameClashes,
        tidyOpenTyCoVar, tidyOpenTyCoVars,
        tidyTyCoVarOcc,
        tidyTopType,
        tidyCo, tidyCos,
        tidyDCo,
        tidyTyCoVarBinder, tidyTyCoVarBinders
  ) where

import GHC.Prelude
import GHC.Data.Maybe ( orElse )

import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.FVs (tyCoVarsOfTypesWellScoped, tyCoVarsOfTypeList)

import GHC.Types.Name hiding (varName)
import GHC.Types.Var
import GHC.Types.Var.Env
import GHC.Utils.Misc (strictMap)

import Data.List (mapAccumL)

{-
%************************************************************************
%*                                                                      *
\subsection{TidyType}
%*                                                                      *
%************************************************************************
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
  = mkTyVarOcc (occNameString occ ++ "0")
  | otherwise
  = occ
  where
   name = varName tv
   occ  = getOccName name

tidyTyCoVarBinder :: TidyEnv -> VarBndr TyCoVar vis
                  -> (TidyEnv, VarBndr TyCoVar vis)
tidyTyCoVarBinder tidy_env (Bndr tv vis)
  = (tidy_env', Bndr tv' vis)
  where
    (tidy_env', tv') = tidyVarBndr tidy_env tv

tidyTyCoVarBinders :: TidyEnv -> [VarBndr TyCoVar vis]
                   -> (TidyEnv, [VarBndr TyCoVar vis])
tidyTyCoVarBinders tidy_env tvbs
  = mapAccumL tidyTyCoVarBinder
              (avoidNameClashes (binderVars tvbs) tidy_env) tvbs

---------------
tidyFreeTyCoVars :: TidyEnv -> [TyCoVar] -> TidyEnv
-- ^ Add the free 'TyVar's to the env in tidy form,
-- so that we can tidy the type they are free in
tidyFreeTyCoVars tidy_env tyvars
  = fst (tidyOpenTyCoVars tidy_env tyvars)

---------------
tidyOpenTyCoVars :: TidyEnv -> [TyCoVar] -> (TidyEnv, [TyCoVar])
tidyOpenTyCoVars env tyvars = mapAccumL tidyOpenTyCoVar env tyvars

---------------
tidyOpenTyCoVar :: TidyEnv -> TyCoVar -> (TidyEnv, TyCoVar)
-- ^ Treat a new 'TyCoVar' as a binder, and give it a fresh tidy name
-- using the environment if one has not already been allocated. See
-- also 'tidyVarBndr'
tidyOpenTyCoVar env@(_, subst) tyvar
  = case lookupVarEnv subst tyvar of
        Just tyvar' -> (env, tyvar')              -- Already substituted
        Nothing     ->
          let env' = tidyFreeTyCoVars env (tyCoVarsOfTypeList (tyVarKind tyvar))
          in tidyVarBndr env' tyvar  -- Treat it as a binder

---------------
tidyTyCoVarOcc :: TidyEnv -> TyCoVar -> TyCoVar
tidyTyCoVarOcc env@(_, subst) tv
  = case lookupVarEnv subst tv of
        Nothing  -> updateVarType (tidyType env) tv
        Just tv' -> tv'

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
tidyType _   t@(LitTy {})          = t -- Preserve sharing
tidyType env (TyVarTy tv)          = TyVarTy $! tidyTyCoVarOcc env tv
tidyType _   t@(TyConApp _ [])     = t -- Preserve sharing if possible
tidyType env (TyConApp tycon tys)  = TyConApp tycon $! tidyTypes env tys
tidyType env (AppTy fun arg)       = (AppTy $! (tidyType env fun)) $! (tidyType env arg)
tidyType env ty@(FunTy _ w arg res)  = let { !w'   = tidyType env w
                                           ; !arg' = tidyType env arg
                                           ; !res' = tidyType env res }
                                       in ty { ft_mult = w', ft_arg = arg', ft_res = res' }
tidyType env (ty@(ForAllTy{}))     = (mkForAllTys' $! (zip tvs' vis)) $! tidyType env' body_ty
  where
    (tvs, vis, body_ty) = splitForAllTyCoVars' ty
    (env', tvs') = tidyVarBndrs env tvs
tidyType env (CastTy ty co)       = (CastTy $! tidyType env ty) $! (tidyCo env co)
tidyType env (CoercionTy co)      = CoercionTy $! (tidyCo env co)


-- The following two functions differ from mkForAllTys and splitForAllTyCoVars in that
-- they expect/preserve the ArgFlag argument. These belong to "GHC.Core.Type", but
-- how should they be named?
mkForAllTys' :: [(TyCoVar, ArgFlag)] -> Type -> Type
mkForAllTys' tvvs ty = foldr strictMkForAllTy ty tvvs
  where
    strictMkForAllTy (tv,vis) ty = (ForAllTy $! ((Bndr $! tv) $! vis)) $! ty

splitForAllTyCoVars' :: Type -> ([TyCoVar], [ArgFlag], Type)
splitForAllTyCoVars' ty = go ty [] []
  where
    go (ForAllTy (Bndr tv vis) ty) tvs viss = go ty (tv:tvs) (vis:viss)
    go ty                          tvs viss = (reverse tvs, reverse viss, ty)


---------------
-- | Grabs the free type variables, tidies them
-- and then uses 'tidyType' to work over the type itself
tidyOpenTypes :: TidyEnv -> [Type] -> (TidyEnv, [Type])
tidyOpenTypes env tys
  = (env', tidyTypes (trimmed_occ_env, var_env) tys)
  where
    (env'@(_, var_env), tvs') = tidyOpenTyCoVars env $
                                tyCoVarsOfTypesWellScoped tys
    trimmed_occ_env = initTidyOccEnv (map getOccName tvs')
      -- The idea here was that we restrict the new TidyEnv to the
      -- _free_ vars of the types, so that we don't gratuitously rename
      -- the _bound_ variables of the types.

---------------
tidyOpenType :: TidyEnv -> Type -> (TidyEnv, Type)
tidyOpenType env ty = let (env', [ty']) = tidyOpenTypes env [ty] in
                      (env', ty')

---------------
-- | Calls 'tidyType' on a top-level type (i.e. with an empty tidying environment)
tidyTopType :: Type -> Type
tidyTopType ty = tidyType emptyTidyEnv ty

---------------

-- | Tidy a Coercion
--
-- See Note [Strictness in tidyType and friends]
tidyCo :: TidyEnv -> Coercion -> Coercion
tidyCo = fst . tidyCoDCo

tidyDCo :: TidyEnv -> DCoercion -> DCoercion
tidyDCo = snd . tidyCoDCo

tidyCoDCo :: TidyEnv -> (Coercion -> Coercion, DCoercion -> DCoercion)
tidyCoDCo env@(_, subst) = (go, go_dco)
  where
    go_mco MRefl    = MRefl
    go_mco (MCo co) = MCo $! go co

    go (Refl ty)             = Refl $! tidyType env ty
    go (GRefl r ty mco)      = (GRefl r $! tidyType env ty) $! go_mco mco
    go (TyConAppCo r tc cos) = TyConAppCo r tc $! strictMap go cos
    go (AppCo co1 co2)       = (AppCo $! go co1) $! go co2
    go (ForAllCo tv h co)    = ((ForAllCo $! tvp) $! (go h)) $! (tidyCo envp co)
                               where (envp, tvp) = tidyVarBndr env tv
            -- the case above duplicates a bit of work in tidying h and the kind
            -- of tv. But the alternative is to use coercionKind, which seems worse.
    go (FunCo r w co1 co2)   = ((FunCo r $! go w) $! go co1) $! go co2
    go (CoVarCo cv)          = CoVarCo $! go_cv cv
    go (HoleCo h)            = HoleCo h
    go (AxiomInstCo con ind cos) = AxiomInstCo con ind $! strictMap go cos
    go (HydrateDCo r t1 dco rty) = ((HydrateDCo r $! tidyType env t1) $! go_dco dco) $! tidyType env rty
    go (UnivCo p r t1 t2)    = (((UnivCo $! (go_prov go p)) $! r) $!
                                tidyType env t1) $! tidyType env t2
    go (SymCo co)            = SymCo $! go co
    go (TransCo co1 co2)     = (TransCo $! go co1) $! go co2
    go (NthCo r d co)        = NthCo r d $! go co
    go (LRCo lr co)          = LRCo lr $! go co
    go (InstCo co ty)        = (InstCo $! go co) $! go ty
    go (KindCo co)           = KindCo $! go co
    go (SubCo co)            = SubCo $! go co
    go (AxiomRuleCo ax cos)  = AxiomRuleCo ax $ strictMap go cos

    go_dco ReflDCo                = ReflDCo
    go_dco (GReflRightDCo co)     = GReflRightDCo $! go co
    go_dco (GReflLeftDCo  co)     = GReflLeftDCo  $! go co
    go_dco (TyConAppDCo cos)      = TyConAppDCo $! strictMap go_dco cos
    go_dco (AppDCo co1 co2)       = (AppDCo $! go_dco co1) $! go_dco co2
    go_dco (ForAllDCo tv h co)    = ((ForAllDCo $! tvp) $! (go_dco h)) $! tidyDCo envp co
                               where (envp, tvp) = tidyVarBndr env tv
            -- the case above duplicates a bit of work in tidying h and the kind
            -- of tv. But the alternative is to use coercionKind, which seems worse.
    go_dco (CoVarDCo cv)          = CoVarDCo $! go_cv cv
    go_dco dco@AxiomInstDCo{} = dco
    go_dco dco@StepsDCo{}     = dco
    go_dco (TransDCo co1 co2) = (TransDCo $! go_dco co1) $! go_dco co2
    go_dco (DehydrateCo co)   = DehydrateCo $! go co
    go_dco (SubDCo dco)       = SubDCo $! go_dco dco
    go_dco (UnivDCo prov rhs) = (UnivDCo $! go_prov go_dco prov) $! tidyType env rhs

    go_prov :: (co -> co) -> UnivCoProvenance co -> UnivCoProvenance co
    go_prov do_tidy (PhantomProv co)    = PhantomProv $! do_tidy co
    go_prov do_tidy (ProofIrrelProv co) = ProofIrrelProv $! do_tidy co
    go_prov _       p@(PluginProv _)    = p
    go_prov _       p@(CorePrepProv _)  = p

    go_cv cv = lookupVarEnv subst cv `orElse` cv

tidyCos :: TidyEnv -> [Coercion] -> [Coercion]
tidyCos env = strictMap (tidyCo env)

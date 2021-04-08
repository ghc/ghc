{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}

-- | Tidying types and coercions for printing in error messages.
module GHC.Core.TyCo.Tidy
  (
        -- * Tidying type related things up for printing
        tidyType,      tidyTypes,
        tidyOpenType,  tidyOpenTypes,
        tidyOpenKind,
        tidyVarBndr, tidyVarBndrs, tidyFreeTyCoVars, avoidNameClashes,
        tidyOpenTyCoVar, tidyOpenTyCoVars,
        tidyTyCoVarOcc,
        tidyTopType,
        tidyKind,
        tidyCo,
        tidyTyCoVarBinder, tidyTyCoVarBinders
  ) where

import GHC.Prelude

import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.FVs (tyCoVarsOfTypesWellScoped, tyCoVarsOfTypeList)

import GHC.Types.Name hiding (varName)
import GHC.Types.Var
import GHC.Types.Var.Env
import GHC.Utils.Update as Upd

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
tidyVarBndrs tidy_env tvs = case tidy_tycovars_upd (avoidNameClashes tvs tidy_env) tvs of
  (# env', upd_tvs #) -> (env', runUpd upd_tvs)

tidy_tycovars_upd :: TidyEnv -> [TyCoVar] -> (# TidyEnv, Upd [TyCoVar] #)
tidy_tycovars_upd tidy_env tvs = updateListAccumL tidy_tycovar_upd tidy_env tvs

tidyVarBndr :: TidyEnv -> TyCoVar -> (TidyEnv, TyCoVar)
tidyVarBndr tidy_env var = case tidy_tycovar_upd tidy_env var of
  (# tidy_env', var' #) -> (tidy_env', runUpd var' )

tidy_tycovar_upd :: TidyEnv -> TyCoVar -> (# TidyEnv, Upd TyCoVar #)
tidy_tycovar_upd tidy_env var = (# tidy_env', upd_var #)
  where
    (occ_env, subst) = tidy_env
    tidy_env'        = (occ_env', subst')
    subst'           = extendVarEnv subst var (runUpd upd_var)

    -- tidy the helpful OccName and update the occ only when
    --    helpful_occ /= current_occ
    --  or
    --    tidy_occ /= helpful_occ
    current_occ = getOccName (varName var)
    helpful_occ = getHelpfulOccName var
    !(# occ_env', tidy_occ #) = tidy_OccName_upd occ_env (runUpd helpful_occ)
    upd_occ = rebuild current_occ const (# tidy_occ, helpful_occ #)

    -- tidy the Name with the given OccName and finally rebuild the Var
    upd_name = tidyNameOcc_upd (varName var) upd_occ
    upd_var  = rebuild var
                       (\name ty -> (setVarName var name) { varType = ty })
                       (# upd_name, tidy_type_upd tidy_env (varType var) #)

avoidNameClashes :: [TyCoVar] -> TidyEnv -> TidyEnv
-- Seed the occ_env with clashes among the names, see
-- Note [Tidying multiple names at once] in GHC.Types.Names.OccName
avoidNameClashes tvs (occ_env, subst)
  = (avoidClashesOccEnv occ_env occs, subst)
  where
    occs = map (\tv -> runUpd (getHelpfulOccName tv)) tvs

getHelpfulOccName :: TyCoVar -> Upd OccName
-- A TcTyVar with a System Name is probably a
-- unification variable; when we tidy them we give them a trailing
-- "0" (or 1 etc) so that they don't take precedence for the
-- un-modified name. Plus, indicating a unification variable in
-- this way is a helpful clue for users
getHelpfulOccName tv
  | isSystemName name, isTcTyVar tv
  = Update (mkTyVarOcc (occNameString occ ++ "0"))
  | otherwise
  = NoUpdate occ
  where
   !name = varName tv
   occ   = getOccName name

tidyTyCoVarBinder :: TidyEnv -> VarBndr TyCoVar vis
                  -> (TidyEnv, VarBndr TyCoVar vis)
tidyTyCoVarBinder tidy_env (Bndr tv vis)
  = (tidy_env', Bndr tv' vis)
  where
    (tidy_env', tv') = tidyVarBndr tidy_env tv

tidyTyCoVarBinders :: TidyEnv -> [VarBndr TyCoVar vis]
                   -> (TidyEnv, [VarBndr TyCoVar vis])
tidyTyCoVarBinders tidy_env tvbs = go tidy_env' tvbs
  where
    tidy_env' = avoidNameClashes (binderVars tvbs) tidy_env
    go env []       = (env, [])
    go env (tv:tvs) = (env'', tv':tvs')
      where
        !(!env',  !tv')  = tidyTyCoVarBinder env tv
        !(!env'', !tvs') = go env' tvs

---------------
tidyFreeTyCoVars :: TidyEnv -> [TyCoVar] -> TidyEnv
-- ^ Add the free 'TyVar's to the env in tidy form,
-- so that we can tidy the type they are free in
tidyFreeTyCoVars tidy_env tyvars
  = fst (tidyOpenTyCoVars tidy_env tyvars)

---------------
tidyOpenTyCoVars :: TidyEnv -> [TyCoVar] -> (TidyEnv, [TyCoVar])
tidyOpenTyCoVars = go
  where
    go env []       = (env, [])
    go env (tv:tvs) = (env'', tv':tvs')
      where
        !(!env',  !tv')  = tidyOpenTyCoVar env tv
        !(!env'', !tvs') = go env' tvs

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

tidy_TyCoVar_upd :: TidyEnv -> TyCoVar -> Upd TyCoVar
tidy_TyCoVar_upd env@(_, subst) tv
  = case lookupVarEnv subst tv of
        Nothing  -> updateVarType_upd (tidy_type_upd env) tv
        Just tv' -> Update tv'

-- FIXME: adapted from GHC.Types.Var
updateVarType_upd :: (Type -> Upd Type) -> Var -> Upd Var
updateVarType_upd upd var = rebuild var (\t -> var { varType = t}) (# upd (varType var) #)
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
tidyTypes env tys = Upd.update (tidy_types_upd env) tys

tidy_types_upd :: TidyEnv -> [Type] -> Upd [Type]
tidy_types_upd env tys = Upd.updateList (tidy_type_upd env) tys

---------------


-- | Tidy a Type
--
-- See Note [Strictness in tidyType and friends]
tidyType :: TidyEnv -> Type -> Type
tidyType env ty = Upd.update (tidy_type_upd env) ty

tidy_type_upd :: TidyEnv -> Type -> Upd Type
tidy_type_upd = go
  where
    go env t = case t of
      LitTy _             -> NoUpdate t
      TyVarTy tv          -> rebuild t TyVarTy (# tidy_TyCoVar_upd env tv #)
      TyConApp _ []       -> NoUpdate t
      TyConApp tycon tys  -> rebuild t TyConApp (# NoUpdate tycon, tidy_types_upd env tys #)
      AppTy fun arg       -> rebuild t AppTy (# go env fun, go env arg #)
      FunTy af w arg res  -> rebuild t (FunTy af) (# go env w, go env arg, go env res #)
      CastTy ty co        -> rebuild t CastTy  (# go env ty, tidy_co_upd env co #)
      CoercionTy co       -> rebuild t CoercionTy (# tidy_co_upd env co #)
      ForAllTy{}          -> go_forall env' t
            where
              -- get all the successive ForAllTys' TyCoVars and rename them at
              -- once. See Note [Tidying multiple names at once] in
              -- GHC.Types.Names.OccName
              all_tvs = get_forall_tvs t
              env'    = avoidNameClashes all_tvs env

    go_forall env t = case t of
      ForAllTy bndr@(Bndr tv vis) ty
        -> rebuild t ForAllTy (# upd_bndr, go_forall env' ty #)
            where
              !upd_bndr = rebuild bndr Bndr (# upd_tv, NoUpdate vis #)
              !(# !env', !upd_tv #) = tidy_tycovar_upd env tv
      _ -> go env t

    -- get successive ForAllTy TyCoVars
    get_forall_tvs = \case
      ForAllTy (Bndr tv _) ty -> tv : get_forall_tvs ty
      _                       -> []


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
tidyOpenKind :: TidyEnv -> Kind -> (TidyEnv, Kind)
tidyOpenKind = tidyOpenType

tidyKind :: TidyEnv -> Kind -> Kind
tidyKind = tidyType

----------------

-- | Tidy a Coercion
--
-- See Note [Strictness in tidyType and friends]
tidyCo :: TidyEnv -> Coercion -> Coercion
tidyCo env co = Upd.update (tidy_co_upd env) co

tidy_co_upd :: TidyEnv -> Coercion -> Upd Coercion
tidy_co_upd = go
  where
    go_mco env c = case c of
      MRefl    -> NoUpdate c
      MCo co   -> rebuild c MCo (# go env co #)

    go env@(_, subst) c = case c of
      Refl ty                 -> rebuild c Refl (# tidy_type_upd env ty #)
      GRefl r ty mco          -> rebuild c GRefl (# NoUpdate r, tidy_type_upd env ty, go_mco env mco #)
      TyConAppCo r tc cos     -> rebuild c TyConAppCo (# NoUpdate r, NoUpdate tc, Upd.updateList (go env) cos #)
      AppCo co1 co2           -> rebuild c AppCo (# go env co1, go env co2 #)
      ForAllCo tv h co        -> rebuild c ForAllCo (# tvp, go env h, tidy_co_upd envp co #)
                               where !(# envp, tvp #) = tidy_tycovar_upd env tv
            -- the case above duplicates a bit of work in tidying h and the kind
            -- of tv. But the alternative is to use coercionKind, which seems worse.
      FunCo r w co1 co2       -> rebuild c FunCo (# NoUpdate r, go env w, go env co1, go env co2 #)
      CoVarCo cv              -> case lookupVarEnv subst cv of
                                  Nothing  -> NoUpdate c
                                  Just cv' -> Update (CoVarCo cv')
      HoleCo _                -> NoUpdate c
      AxiomInstCo con ind cos -> rebuild c AxiomInstCo (# NoUpdate con, NoUpdate ind, Upd.updateList (go env) cos #)
      UnivCo p r t1 t2        -> rebuild c UnivCo (# go_prov env p, NoUpdate r, tidy_type_upd env t1, tidy_type_upd env t2 #)
      SymCo co                -> rebuild c SymCo (# go env co #)
      TransCo co1 co2         -> rebuild c TransCo (# go env co1, go env co2 #)
      NthCo r d co            -> rebuild c NthCo (# NoUpdate r, NoUpdate d, go env co #)
      LRCo lr co              -> rebuild c LRCo (# NoUpdate lr, go env co #)
      InstCo co ty            -> rebuild c InstCo (# go env co, go env ty #)
      KindCo co               -> rebuild c KindCo (# go env co #)
      SubCo co                -> rebuild c SubCo (# go env co #)
      AxiomRuleCo ax cos      -> rebuild c AxiomRuleCo (# NoUpdate ax, Upd.updateList (go env) cos #)

    go_prov env p = case p of
      PhantomProv co          -> rebuild p PhantomProv (# go env co #)
      ProofIrrelProv co       -> rebuild p ProofIrrelProv (# go env co #)
      PluginProv _            -> NoUpdate p

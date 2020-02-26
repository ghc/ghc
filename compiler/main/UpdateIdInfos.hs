{-# LANGUAGE CPP, BangPatterns, Strict, RecordWildCards #-}

module UpdateIdInfos
  ( updateModDetailsIdInfos
  ) where

import GhcPrelude

import GHC.Core
import GHC.Driver.Session
import GHC.Driver.Types
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Core.InstEnv
import GHC.Types.Name.Env
import GHC.Types.Name.Set
import Util
import GHC.Types.Var
import Outputable
import GHC.StgToCmm.Types (CgInfos (..))

#include "HsVersions.h"

-- | Update CafInfos and LFInfos of all occurences (in rules, unfoldings, class instances)
updateModDetailsIdInfos
  :: DynFlags
  -> CgInfos
  -> ModDetails -- ^ ModDetails to update
  -> ModDetails

updateModDetailsIdInfos dflags _ mod_details
  | gopt Opt_OmitInterfacePragmas dflags
  = mod_details

updateModDetailsIdInfos _ cg_infos mod_details =
  let
    ModDetails{ md_types = type_env -- for unfoldings
              , md_insts = insts
              , md_rules = rules
              } = mod_details

    -- type TypeEnv = NameEnv TyThing
    ~type_env' = mapNameEnv (updateTyThingCafInfos type_env' cg_infos) type_env
    -- Not strict!

    !insts' = strictMap (updateInstIdInfos type_env' cg_infos) insts
    !rules' = strictMap (updateRuleIdInfos type_env') rules
  in
    mod_details{ md_types = type_env'
               , md_insts = insts'
               , md_rules = rules'
               }

--------------------------------------------------------------------------------
-- Rules
--------------------------------------------------------------------------------

updateRuleIdInfos :: TypeEnv -> CoreRule -> CoreRule
updateRuleIdInfos _ rule@BuiltinRule{} = rule
updateRuleIdInfos type_env Rule{ .. } = Rule { ru_rhs = updateGlobalIds type_env ru_rhs, .. }

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

updateInstIdInfos :: TypeEnv -> CgInfos -> ClsInst -> ClsInst
updateInstIdInfos type_env cg_infos =
    updateClsInstDFun (updateIdUnfolding type_env . updateIdInfo cg_infos)

--------------------------------------------------------------------------------
-- TyThings
--------------------------------------------------------------------------------

updateTyThingCafInfos :: TypeEnv -> CgInfos -> TyThing -> TyThing

updateTyThingCafInfos type_env cg_infos (AnId id) =
    AnId (updateIdUnfolding type_env (updateIdInfo cg_infos id))

updateTyThingCafInfos _ _ other = other -- AConLike, ATyCon, ACoAxiom

--------------------------------------------------------------------------------
-- Unfoldings
--------------------------------------------------------------------------------

updateIdUnfolding :: TypeEnv -> Id -> Id
updateIdUnfolding type_env id =
    case idUnfolding id of
      CoreUnfolding{ .. } ->
        setIdUnfolding id CoreUnfolding{ uf_tmpl = updateGlobalIds type_env uf_tmpl, .. }
      DFunUnfolding{ .. } ->
        setIdUnfolding id DFunUnfolding{ df_args = map (updateGlobalIds type_env) df_args, .. }
      _ -> id

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

updateIdInfo :: CgInfos -> Id -> Id
updateIdInfo CgInfos{ cgNonCafs = non_cafs, cgLFInfos = lf_infos } id =
    let
      not_caffy = elemNameSet (idName id) non_cafs
      mb_lf_info = lookupNameEnv lf_infos (idName id)

      id1 = if not_caffy then setIdCafInfo id NoCafRefs else id
      id2 = case mb_lf_info of
              Nothing -> id1
              Just lf_info -> setIdLFInfo id1 lf_info
    in
      id2

--------------------------------------------------------------------------------

updateGlobalIds :: NameEnv TyThing -> CoreExpr -> CoreExpr
-- Update occurrences of GlobalIds as directed by 'env'
-- The 'env' maps a GlobalId to a version with accurate CAF info
-- (and in due course perhaps other back-end-related info)
updateGlobalIds env e = go env e
  where
    go_id :: NameEnv TyThing -> Id -> Id
    go_id env var =
      case lookupNameEnv env (varName var) of
        Nothing -> var
        Just (AnId id) -> id
        Just other -> pprPanic "UpdateCafInfos.updateGlobalIds" $
          text "Found a non-Id for Id Name" <+> ppr (varName var) $$
          nest 4 (text "Id:" <+> ppr var $$
                  text "TyThing:" <+> ppr other)

    go :: NameEnv TyThing -> CoreExpr -> CoreExpr
    go env (Var v) = Var (go_id env v)
    go _ e@Lit{} = e
    go env (App e1 e2) = App (go env e1) (go env e2)
    go env (Lam b e) = assertNotInNameEnv env [b] (Lam b (go env e))
    go env (Let bs e) = Let (go_binds env bs) (go env e)
    go env (Case e b ty alts) =
        assertNotInNameEnv env [b] (Case (go env e) b ty (map go_alt alts))
      where
         go_alt (k,bs,e) = assertNotInNameEnv env bs (k, bs, go env e)
    go env (Cast e c) = Cast (go env e) c
    go env (Tick t e) = Tick t (go env e)
    go _ e@Type{} = e
    go _ e@Coercion{} = e

    go_binds :: NameEnv TyThing -> CoreBind -> CoreBind
    go_binds env (NonRec b e) =
      assertNotInNameEnv env [b] (NonRec b (go env e))
    go_binds env (Rec prs) =
      assertNotInNameEnv env (map fst prs) (Rec (mapSnd (go env) prs))

-- In `updateGlobaLIds` Names of local binders should not shadow Name of
-- globals. This assertion is to check that.
assertNotInNameEnv :: NameEnv a -> [Id] -> b -> b
assertNotInNameEnv env ids x = ASSERT(not (any (\id -> elemNameEnv (idName id) env) ids)) x

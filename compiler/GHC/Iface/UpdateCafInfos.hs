{-# LANGUAGE CPP, BangPatterns, Strict, RecordWildCards #-}

module GHC.Iface.UpdateCafInfos
  ( updateModDetailsCafInfos
  ) where

import GHC.Prelude

import GHC.Core
import GHC.Driver.Session
import GHC.Driver.Types
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Core.InstEnv
import GHC.Types.Name.Env
import GHC.Types.Name.Set
import GHC.Utils.Misc
import GHC.Types.Var
import GHC.Utils.Outputable

#include "HsVersions.h"

-- | Update CafInfos of all occurences (in rules, unfoldings, class instances)
updateModDetailsCafInfos
  :: DynFlags
  -> NonCaffySet -- ^ Non-CAFFY names in the module. Names not in this set are CAFFY.
  -> ModDetails -- ^ ModDetails to update
  -> ModDetails

updateModDetailsCafInfos dflags _ mod_details
  | gopt Opt_OmitInterfacePragmas dflags
  = mod_details

updateModDetailsCafInfos _ (NonCaffySet non_cafs) mod_details =
  {- pprTrace "updateModDetailsCafInfos" (text "non_cafs:" <+> ppr non_cafs) $ -}
  let
    ModDetails{ md_types = type_env -- for unfoldings
              , md_insts = insts
              , md_rules = rules
              } = mod_details

    -- type TypeEnv = NameEnv TyThing
    ~type_env' = mapNameEnv (updateTyThingCafInfos type_env' non_cafs) type_env
    -- Not strict!

    !insts' = strictMap (updateInstCafInfos type_env' non_cafs) insts
    !rules' = strictMap (updateRuleCafInfos type_env') rules
  in
    mod_details{ md_types = type_env'
               , md_insts = insts'
               , md_rules = rules'
               }

--------------------------------------------------------------------------------
-- Rules
--------------------------------------------------------------------------------

updateRuleCafInfos :: TypeEnv -> CoreRule -> CoreRule
updateRuleCafInfos _ rule@BuiltinRule{} = rule
updateRuleCafInfos type_env Rule{ .. } = Rule { ru_rhs = updateGlobalIds type_env ru_rhs, .. }

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

updateInstCafInfos :: TypeEnv -> NameSet -> ClsInst -> ClsInst
updateInstCafInfos type_env non_cafs =
    updateClsInstDFun (updateIdUnfolding type_env . updateIdCafInfo non_cafs)

--------------------------------------------------------------------------------
-- TyThings
--------------------------------------------------------------------------------

updateTyThingCafInfos :: TypeEnv -> NameSet -> TyThing -> TyThing

updateTyThingCafInfos type_env non_cafs (AnId id) =
    AnId (updateIdUnfolding type_env (updateIdCafInfo non_cafs id))

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

updateIdCafInfo :: NameSet -> Id -> Id
updateIdCafInfo non_cafs id
  | idName id `elemNameSet` non_cafs
  = -- pprTrace "updateIdCafInfo" (text "Marking" <+> ppr id <+> parens (ppr (idName id)) <+> text "as non-CAFFY") $
    id `setIdCafInfo` NoCafRefs
  | otherwise
  = id

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
        Just other -> pprPanic "GHC.Iface.UpdateCafInfos.updateGlobalIds" $
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

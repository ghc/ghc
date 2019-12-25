{-# LANGUAGE BangPatterns, Strict #-}

module UpdateCafInfos
  ( updateModDetailsCafInfos
  ) where

import GhcPrelude

import CoreSyn
import HscTypes
import Id
import IdInfo
import InstEnv
import NameEnv
import NameSet
import Util
-- import Outputable

-- | Update CafInfos of all occurences (in rules, unfoldings, class instances)
updateModDetailsCafInfos
  :: NameSet -- ^ Non-CAFFY names in the module. Names not in this set are CAFFY.
  -> ModDetails -- ^ ModDetails to update
  -> ModDetails
updateModDetailsCafInfos non_cafs mod_details =
  {- pprTrace "updateModDetailsCafInfos" (text "non_cafs:" <+> ppr non_cafs) $ -}
  let
    ModDetails{ md_types = type_env -- for unfoldings
              , md_insts = insts
              , md_rules = rules
              } = mod_details

    -- type TypeEnv = NameEnv TyThing
    !type_env' = mapNameEnv (updateTyThingCafInfos non_cafs) type_env
    !insts' = strictMap (updateInstCafInfos non_cafs) insts
    !rules' = strictMap (updateRuleCafInfos non_cafs) rules
  in
    mod_details{ md_types = type_env'
               , md_insts = insts'
               , md_rules = rules'
               }

--------------------------------------------------------------------------------
-- Rules
--------------------------------------------------------------------------------

updateRuleCafInfos :: NameSet -> CoreRule -> CoreRule
updateRuleCafInfos _ rule@BuiltinRule{} = rule
updateRuleCafInfos non_cafs rule@Rule{ ru_rhs = rhs } = rule{ ru_rhs = updateExprCafInfos non_cafs rhs }

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

updateInstCafInfos :: NameSet -> ClsInst -> ClsInst
updateInstCafInfos non_cafs = updateClsInstDFun (updateIdCafInfo non_cafs)

--------------------------------------------------------------------------------
-- TyThings
--------------------------------------------------------------------------------

updateTyThingCafInfos :: NameSet -> TyThing -> TyThing
updateTyThingCafInfos non_cafs (AnId id) = AnId (updateIdCafInfo non_cafs id)
updateTyThingCafInfos _ other = other -- AConLike, ATyCon, ACoAxiom

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

-- TODO: How to avoid losing sharing here?
updateExprCafInfos :: NameSet -> CoreExpr -> CoreExpr
updateExprCafInfos non_cafs = go
  where
    go_id = updateIdCafInfo non_cafs

    go :: CoreExpr -> CoreExpr
    go (Var v) = Var (go_id v)
    go e@Lit{} = e
    go (App e1 e2) = App (go e1) (go e2)
    go (Lam b e) = Lam (go_id b) (go e)
    go (Let bs e) = Let (go_bs bs) (go e)
    go (Case e b ty alts) = Case (go e) (go_id b) ty (strictMap go_alt alts)
    go (Cast e co) = Cast (go e) co
    go (Tick t e) = Tick t (go e)
    go e@Type{} = e
    go e@Coercion{} = e

    go_bs (NonRec b e) = NonRec (go_id b) (go e)
    go_bs (Rec bs) = Rec (strictMap (\(b, e) -> (go_id b, go e)) bs)

    go_alt (con, bs, e) = (con, strictMap go_id bs, go e)

updateIdCafInfo :: NameSet -> Id -> Id
updateIdCafInfo non_cafs id
  | idName id `elemNameSet` non_cafs
  = -- pprTrace "updateIdCafInfo" (text "Marking" <+> ppr id <+> parens (ppr (idName id)) <+> text "as non-CAFFY") $
    id `setIdCafInfo` NoCafRefs
  | otherwise
  = id

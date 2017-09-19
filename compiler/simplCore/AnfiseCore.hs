-- | Convert Core into A-normal form (ANF).
module AnfiseCore ( anfiseProgram ) where

import BasicTypes
import Type
import Id
import VarEnv
import CoreUtils
import CoreSyn
import FastString
import Unique

import Data.Bifunctor
import Data.Either
import Control.Monad.Trans.State

anfiseProgram :: CoreProgram -> CoreProgram
anfiseProgram top_binds = map goTopLvl top_binds
  where
    goTopLvl (NonRec v e) = NonRec v (go in_scope_toplvl e)
    goTopLvl (Rec pairs) = Rec (map (second (go in_scope_toplvl)) pairs)

    in_scope_toplvl = emptyInScopeSet `extendInScopeSetList` bindersOfBinds top_binds

    go :: InScopeSet -> CoreExpr -> CoreExpr
    go _ e@(Var{})       = e
    go _ e@(Lit {})      = e
    go _ e@(Type {})     = e
    go _ e@(Coercion {}) = e

    go in_scope e@(App e1 e2)
      | Var f_id <- f
      , isJoinId f_id
      = dont_bind
      | otherwise
      = let bound_args :: [Either CoreExpr (Id, CoreExpr)]
            bound_args = evalState (mapM bind_arg args) in_scope
              where
                bind_arg :: CoreExpr -> State InScopeSet (Either CoreExpr (Id, CoreExpr))
                bind_arg arg
                  | not should_bind = return $ Left arg
                  | otherwise = do
                      bndr <- mkAnfId ty
                      nowInScope bndr
                      return $ Right (bndr, arg)
                  where
                    ty = exprType arg
                    should_bind = isValArg arg && not (isUnliftedType ty) && not (exprIsTrivial arg)
            binds = map (uncurry NonRec) (rights bound_args)
            to_arg = either id (Var . fst)
        in mkLets binds $ mkApps f (map to_arg bound_args)
      where
        (f, args) = collectArgs e
        dont_bind = App (go in_scope e1) (go in_scope e2)
    go in_scope (Lam v e')  = Lam v (go in_scope' e')
      where in_scope' = in_scope `extendInScopeSet` v
    go in_scope (Case scrut bndr ty alts)
        = Case (go in_scope scrut) bndr ty (map (goAlt in_scope') alts)
      where in_scope' = in_scope `extendInScopeSet` bndr
    go in_scope (Cast e' c) = Cast (go in_scope e') c
    go in_scope (Tick t e') = Tick t (go in_scope e')
    go in_scope (Let bind body) = goBind in_scope bind (go in_scope' body)
      where in_scope' = in_scope `extendInScopeSetList` bindersOf bind

    goAlt :: InScopeSet -> CoreAlt -> CoreAlt
    goAlt in_scope (dc, pats, rhs) = (dc, pats, go in_scope' rhs)
      where in_scope' = in_scope `extendInScopeSetList` pats

    goBind :: InScopeSet -> CoreBind -> (CoreExpr -> CoreExpr)
    goBind in_scope (NonRec v rhs) = Let (NonRec v (go in_scope rhs))
    goBind in_scope (Rec pairs) = Let (Rec pairs')
      where pairs' = map (second (go in_scope')) pairs
            in_scope' = in_scope `extendInScopeSetList` bindersOf (Rec pairs)

nowInScope :: Id -> State InScopeSet ()
nowInScope id = modify (`extendInScopeSet` id)

mkAnfId :: Type -> State InScopeSet Id
mkAnfId ty = do
    in_scope <- get
    return $ uniqAway in_scope id_tmpl
  where
    id_tmpl = mkSysLocal (fsLit "anf") initExitJoinUnique ty
              `setIdOccInfo` occ_info
    occ_info =
      OneOcc { occ_in_lam = insideLam
             , occ_one_br = oneBranch
             , occ_int_cxt = False
             , occ_tail = NoTailCallInfo
             }

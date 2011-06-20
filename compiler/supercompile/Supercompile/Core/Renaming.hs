{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Supercompile.Core.Renaming (
    module Supercompile.Core.Renaming,
    InScopeSet, emptyInScopeSet, mkInScopeSet
  ) where

import Supercompile.Core.FreeVars
import Supercompile.Core.Syntax

import Supercompile.Utilities

import CoreSubst
import Coercion (CvSubstEnv, isCoVar, mkCoVarCo)
import qualified CoreSyn as CoreSyn (CoreExpr, Expr(Var))
import Type     (mkTyVarTy)
import Var      (CoVar, TyVar, isTyVar)
import VarEnv


-- We are going to use GHC's substitution type in a rather stylised way, and only
-- ever substitute variables for variables. The reasons for this are twofold:
--
--  1. Particularly since we are in ANF, doing any other sort of substitution is unnecessary
--
--  2. We have our own syntax data type, and we don't want to build a GHC syntax tree just
--     for insertion into the Subst if we can help it!
--
-- Unfortunately, in order to make this work with the coercionful operational semantics
-- we will sometimes need to substitute coerced variables for variables. An example would be
-- when reducing:
--
--  (\x. e) |> gam y
--
-- Where
--
--  gam = (F Int -> F Int ~ Bool -> Bool)
--
-- We need to reduce to something like:
--
--  e[(y |> sym (nth 1 gam))/x] |> (nth 2 gam)
--
-- We deal with this problem in the evaluator by introducing an intermediate let binding for
-- such redexes.

type Renaming = (IdSubstEnv, TvSubstEnv, CvSubstEnv)

joinSubst :: InScopeSet -> Renaming -> Subst
joinSubst iss (id_subst, tv_subst, co_subst) = mkSubst iss tv_subst co_subst id_subst

splitSubst :: Subst -> (InScopeSet, Renaming)
splitSubst (Subst iss id_subst tv_subst co_subst) = (iss, (id_subst, tv_subst, co_subst))


emptyRenaming :: Renaming
emptyRenaming = (emptyVarEnv, emptyVarEnv, emptyVarEnv)

mkIdentityRenaming :: FreeVars -> Renaming
mkIdentityRenaming fvs = (mkVarEnv [(x, CoreSyn.Var x) | x <- id_list], mkVarEnv [(x, mkTyVarTy x) | x <- tv_list], mkVarEnv [(x, mkCoVarCo x) | x <- co_list])
  where (tv_list, coid_list) = partition isTyVar (varSetElems fvs)
        (co_list, id_list)   = partition isCoVar coid_list

mkRenaming :: [(Var, Var)] -> Renaming
mkRenaming xxs' = (mkVarEnv [(x, CoreSyn.Var x') | (x, x') <- idids'], mkVarEnv [(x, mkTyVarTy x') | (x, x') <- tvtvs'], mkVarEnv [(x, mkCoVarCo x') | (x, x') <- cocos'])
  where (tvtvs', yys')   = partition (isTyVar . fst) xxs'
        (cocos', idids') = partition (isCoVar . fst) yys'

varToCoreSyn :: Var -> CoreSyn.CoreExpr
varToCoreSyn x' = CoreSyn.Var x'

coreSynToVar :: CoreSyn.CoreExpr -> Var
coreSynToVar (CoreSyn.Var x') = x'
coreSynToVar e                = panic "renome" (ppr e)

insertRenaming :: Renaming -> Var -> Var -> Renaming
insertRenaming (id_subst, tv_subst, co_subst) x x' = (extendVarEnv id_subst x (varToCoreSyn x'), tv_subst, co_subst)

insertRenamings :: Renaming -> [(Var, Var)] -> Renaming
insertRenamings (id_subst, tv_subst, co_subst) xxs = (extendVarEnvList id_subst $ map (second varToCoreSyn) xxs, tv_subst, co_subst)

insertTypeSubst :: Renaming -> TyVar -> Type -> Renaming
insertTypeSubst (id_subst, tv_subst, co_subst) x ty' = (id_subst, extendVarEnv tv_subst x ty', co_subst)

insertTypeSubsts :: Renaming -> [(TyVar, Type)] -> Renaming
insertTypeSubsts (id_subst, tv_subst, co_subst) xtys = (id_subst, extendVarEnvList tv_subst xtys, co_subst)

insertCoercionSubst :: Renaming -> CoVar -> Coercion -> Renaming
insertCoercionSubst (id_subst, tv_subst, co_subst) x co' = (id_subst, tv_subst, extendVarEnv co_subst x co')

rename :: Renaming -> Var -> Out Var
rename rn = coreSynToVar . lookupIdSubst (text "rename") (joinSubst emptyInScopeSet rn)


type In a = (Renaming, a)
type Out a = a


inFreeVars :: (a -> FreeVars) -> In a -> FreeVars
inFreeVars thing_fvs (rn, thing) = renameFreeVars rn (thing_fvs thing)

renameFreeVars :: Renaming -> FreeVars -> FreeVars
renameFreeVars rn = mapVarSet (rename rn)

renameType :: InScopeSet -> Renaming -> Type -> Type
renameType iss rn = substTy (joinSubst iss rn)

renameCoercion :: InScopeSet -> Renaming -> Coercion -> Coercion
renameCoercion iss rn = substCo (joinSubst iss rn)


renameIn :: (Renaming -> a -> a) -> In a -> a
renameIn f (rn, x) = f rn x

renameBinders, renameNonRecBinders :: InScopeSet -> Renaming -> [Var] -> (InScopeSet, Renaming, [Var])
renameBinders       = renameBinders' substRecBndrs
renameNonRecBinders = renameBinders' substBndrs

renameBinders' :: (Subst -> [Var] -> (Subst, [Var]))
               -> InScopeSet -> Renaming -> [Var] -> (InScopeSet, Renaming, [Var])
renameBinders' subst_bndrs iss rn xs = (iss', rn', xs')
  where (subst', xs') = subst_bndrs (joinSubst iss rn) xs
        (iss', rn') = splitSubst subst'

renameBounds :: InScopeSet -> Renaming -> [(Var, a)] -> (InScopeSet, Renaming, [(Var, In a)])
renameBounds iss rn xes = (iss', rn', xs' `zip` map ((,) rn') es)
  where (xs, es) = unzip xes
        (iss', rn', xs') = renameBinders iss rn xs


(renameTerm,                renameAlts,                renameValue,                renameValue')                = mkRename (\f rn (I e) -> I (f rn e))
(renameFVedTerm,            renameFVedAlts,            renameFVedValue,            renameFVedValue')            = mkRename (\f rn (FVed fvs e) -> FVed (renameFreeVars rn fvs) (f rn e))
(renameTaggedTerm,          renameTaggedAlts,          renameTaggedValue,          renameTaggedValue')          = mkRename (\f rn (Tagged tg e) -> Tagged tg (f rn e))
(renameTaggedSizedFVedTerm, renameTaggedSizedFVedAlts, renameTaggedSizedFVedValue, renameTaggedSizedFVedValue') = mkRename (\f rn (Comp (Tagged tg (Comp (Sized sz (FVed fvs e))))) -> Comp (Tagged tg (Comp (Sized sz (FVed (renameFreeVars rn fvs) (f rn e))))))

{-# INLINE mkRename #-}
mkRename :: (forall a. (Renaming -> a -> a) -> Renaming -> ann a -> ann a)
         -> (InScopeSet -> Renaming -> ann (TermF ann)  -> ann (TermF ann),
             InScopeSet -> Renaming -> [AltF ann]       -> [AltF ann],
             InScopeSet -> Renaming -> ann (ValueF ann) -> ann (ValueF ann),
             InScopeSet -> Renaming -> ValueF ann       -> ValueF ann)
mkRename rec = (term, alternatives, value, value')
  where
    term ids rn = rec (term' ids) rn
    term' ids rn e = case e of
      Var x -> Var (rename rn x)
      Value v -> Value (value' ids rn v)
      TyApp e ty -> TyApp (term ids rn e) (renameType ids rn ty)
      App e x -> App (term ids rn e) (rename rn x)
      PrimOp pop es -> PrimOp pop (map (term ids rn) es)
      Case e x ty alts -> Case (term ids rn e) x' (renameType ids rn ty) (alternatives ids' rn' alts)
        where (ids', rn', [x']) = renameNonRecBinders ids rn [x]
      LetRec xes e -> LetRec (map (second (renameIn (term ids'))) xes') (term ids' rn' e)
        where (ids', rn', xes') = renameBounds ids rn xes
      Cast e co -> Cast (term ids rn e) (renameCoercion ids rn co)
    
    value ids rn = rec (value' ids) rn
    value' ids rn v = case v of
      Indirect x -> Indirect (rename rn x)
      TyLambda x e -> TyLambda x' (term ids' rn' e)
        where (ids', rn', [x']) = renameNonRecBinders ids rn [x]
      Lambda x e -> Lambda x' (term ids' rn' e)
        where (ids', rn', [x']) = renameNonRecBinders ids rn [x]
      Data dc tys xs -> Data dc (map (renameType ids rn) tys) (map (rename rn) xs)
      Literal l -> Literal l
    
    alternatives ids rn = map (alternative ids rn)
    
    alternative ids rn (alt_con, alt_e) = (alt_con', term ids' rn' alt_e)
        where (ids', rn', alt_con') = renameAltCon ids rn alt_con

renameAltCon :: InScopeSet -> Renaming -> AltCon -> (InScopeSet, Renaming, AltCon)
renameAltCon ids rn_alt alt_con = case alt_con of
    DataAlt alt_dc alt_xs -> third3 (DataAlt alt_dc) $ renameNonRecBinders ids rn_alt alt_xs
    LiteralAlt _          -> (ids, rn_alt, alt_con)
    DefaultAlt            -> (ids, rn_alt, alt_con)

{-# LANGUAGE Rank2Types #-}
module Supercompile.Core.Renaming (
    module Supercompile.Core.Renaming,
    mkInScopeSet
  ) where

import Supercompile.Core.FreeVars
import Supercompile.Core.Syntax

import Supercompile.Utilities

import qualified Data.Set as S

import CoreSubst
import qualified CoreSyn as CoreSyn (Expr(Var))
import Type (mkTyVarTy)
import Var  (isTyCoVar)
import VarEnv


-- We are going to use GHC's substitution type in a rather stylised way, and only
-- ever substitute variables for variables. The reasons for this are twofold:
--
--  1. Particularly since we are in ANF, doing any other sort of substitution is unnecessary
--
--  2. We have our own syntax data type, and we don't want to build a GHC syntax tree just
--     for insertion into the Subst if we can help it!
--
-- Furthermore, we don't use the InScopeSet stored in the Subst.
type Renaming = (IdSubstEnv, TvSubstEnv)

joinSubst :: InScopeSet -> Renaming -> Subst
joinSubst iss (id_subst, tv_subst) = mkSubst iss id_subst tv_subst

trivialSubst :: Renaming -> Subst
trivialSubst = joinSubst emptyInScopeSet

splitSubst :: Subst -> (InScopeSet, Renaming)
splitSubst (Subst iss id_subst tv_subst) = (iss, (id_subst, tv_subst))


mkIdentityRenaming :: FreeVars -> Renaming
mkIdentityRenaming fvs = (mkVarEnv [(x, CoreSyn.Var x) | x <- id_list], mkVarEnv [(x, mkTyVarTy x) | x <- tv_list])
  where (tv_list, id_list) = partition isTyCoVar (S.toList fvs)

rename :: Renaming -> Var -> Out Var
rename rn x = case lookupIdSubst (text "rename") (trivialSubst rn) x of CoreSyn.Var x' -> x'


type In a = (Renaming, a)
type Out a = a


renameFreeVars :: Renaming -> FreeVars -> FreeVars
renameFreeVars rn = S.map (rename rn)


renameIn :: (Renaming -> a -> a) -> In a -> a
renameIn f (rn, x) = f rn x

renameBinders, renameNonRecBinders :: InScopeSet -> Renaming -> [In Var] -> (InScopeSet, Renaming, [Out Var])
renameBinders       = renameBinders' substRecBndrs
renameNonRecBinders = renameBinders' substBndrs

renameBinders' :: (Subst -> [In Var] -> (Subst, [Out Var]))
               -> InScopeSet -> Renaming -> [In Var] -> (InScopeSet, Renaming, [Out Var])
renameBinders' subst_bndrs iss rn xs = (iss', rn', xs')
  where (subst', xs') = subst_bndrs (joinSubst iss rn) xs
        (iss', rn') = splitSubst subst'

renameBounds :: InScopeSet -> Renaming -> [(In Var, a)] -> (InScopeSet, Renaming, [(Out Var, a)])
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
      TyApp e ty -> TyApp (term ids rn e) (typ ids rn ty)
      App e x -> App (term ids rn e) (rename rn x)
      PrimOp pop es -> PrimOp pop (map (term ids rn) es)
      Case e x ty alts -> Case (term ids rn e) (rename rn x) (typ ids rn ty) (alternatives ids rn alts)
        where (ids', rn', x') = rename
      LetRec xes e -> LetRec (map (second (renameIn (term ids'))) xes') (term ids' rn' e)
        where (ids', rn', xes') = renameBounds renameBinders ids rn xes
    
    value ids rn = rec (value' ids) rn
    value' ids rn v = case v of
      Indirect x -> Indirect (rename rn x)
      TyLambda x v -> TyLambda x' (value ids' rn' v)
        where (ids', rn', x') = renameNonRecBinders ids rn [x]
      Lambda x e -> Lambda x' (term ids' rn' e)
        where (ids', rn', x') = renameNonRecBinders ids rn [x]
      Data dc xs -> Data dc (map (rename rn) xs)
      Literal l -> Literal l
    
    alternatives ids rn = map (alternative ids rn)
    
    alternative ids rn (alt_con, alt_e) = (alt_con', term ids' rn' alt_e)
        where (ids', rn', alt_con') = renameAltCon ids rn alt_con
    
    typ ids rn = substTy (joinSubst ids rn)

renameAltCon :: InScopeSet -> Renaming -> AltCon -> (InScopeSet, Renaming, AltCon)
renameAltCon ids rn_alt alt_con = case alt_con of
    DataAlt alt_dc alt_xs -> third3 (DataAlt alt_dc) $ renameNonRecBinders ids rn_alt alt_xs
    LiteralAlt _          -> (ids, rn_alt, alt_con)
    DefaultAlt            -> (ids, rn_alt, alt_con)

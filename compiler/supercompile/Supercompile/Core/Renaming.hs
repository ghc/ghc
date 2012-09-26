{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Supercompile.Core.Renaming (
    -- | Renamings
    Renaming, emptyRenaming,
    mkInScopeIdentityRenaming, mkIdentityRenaming, mkTyVarRenaming, mkRenaming,
    InScopeSet, emptyInScopeSet, mkInScopeSet,
    
    -- | PreRenamings
    PreRenaming, invertRenaming, composeRenamings,
    restrictRenaming,

    -- | Extending the renaming
    insertVarRenaming,
    insertIdRenaming, insertIdRenamings,
    insertTypeSubst, insertTypeSubsts,
    insertCoercionSubst, insertCoercionSubsts,
    
    -- | Querying the renaming
    renameId, lookupTyVarSubst, lookupCoVarSubst,
    
    -- | Things with associated renamings
    In, Out,

    -- | Renaming variables occurrences and binding sites
    inFreeVars, renameFreeVars, renameIn,
    renameType, renameCoercion,
    renameBinders, renameNonRecBinder, renameNonRecBinders,
    renameBounds, renameNonRecBound,
    
    -- | Renaming actual bits of syntax
    renameValueG, renameAltCon,
    renameTerm,                renameAlts,                renameValue,                renameValue',
    renameFVedTerm,            renameFVedAlts,            renameFVedValue,            renameFVedValue',
    renameTaggedTerm,          renameTaggedAlts,          renameTaggedValue,          renameTaggedValue',
    renameTaggedSizedFVedTerm, renameTaggedSizedFVedAlts, renameTaggedSizedFVedValue, renameTaggedSizedFVedValue'
  ) where

import Supercompile.Core.FreeVars
import Supercompile.Core.Syntax

import Supercompile.Utilities

import CoreSubst
import OptCoercion (optCoercion)
import Coercion    (CvSubst(..), CvSubstEnv, isCoVar, mkCoVarCo, getCoVar_maybe)
import qualified CoreSyn as CoreSyn (CoreExpr, Expr(Var))
import Type        (mkTyVarTy, getTyVar_maybe)
import Id          (mkSysLocal)
import Var         (Id, TyVar, CoVar, isTyVar, mkTyVar, varType, isGlobalId, varUnique)
import OccName     (occNameFS)
import Name        (getOccName, mkSysTvName)
import FastString  (FastString)
import UniqFM      (ufmToList)
import VarEnv

import Control.Monad.Fix (mfix)

import qualified Data.Map as M


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

-- GHC's binder-renaming stuff does this awful thing where a var->var renaming
-- will always be added to the InScopeSet (which is really an InScopeMap) but
-- will only be added to the IdSubstEnv *if the unique changes*.
--
-- This is a problem for us because we only store the Renaming with each In thing,
-- not the full Subst. So we might lose some renamings recorded only in the InScopeSet.
--
-- The solution is either:
--  1) Rewrite the rest of the supercompiler so it stores a Subst with each binding.
--     Given the behaviour of GHCs binder-renamer, this is probably cleaner (and matches
--     what the GHC does), but I'm not really interested in doing that work right now.
--
--     It also means you have te be very careful to join together InScopeSets if you
--     pull one of those Subst-paired things down into a strictly deeper context. This
--     is easy to get wrong.
--
--  2) Ensure that we always extend the IdSubstEnv, regardless of whether the unique changed.
--     This is the solution I've adopted, and it is implemented here in splitSubst:
splitSubst :: Subst -> [(Var, Var)] -> (InScopeSet, Renaming)
splitSubst (Subst iss id_subst tv_subst co_subst) extend
  = (iss, foldVarlikes (\f -> foldr (\x_x' -> f (fst x_x') x_x')) extend
                       (\(x, x') -> first3  (\id_subst -> extendVarEnv id_subst x (mkIdExpr x')))
                       (\(a, a') -> second3 (\tv_subst -> extendVarEnv tv_subst a (mkTyVarTy a')))
                       (\(q, q') -> third3  (\co_subst -> extendVarEnv co_subst q (mkCoVarCo q')))
                       (id_subst, tv_subst, co_subst))

-- NB: this used to return a triple of lists, but I introduced this version due to profiling
-- results that indicated a caller (renameFreeVars) was causing 2% of all allocations. It turns
-- out that I managed to achieve deforestation in all of the callers by rewriting them to use this
-- version instead.
{-# INLINE foldVarlikes #-}
foldVarlikes :: ((Var -> a -> b -> b) -> b -> f_a -> b)
             -> f_a
             -> (a -> b -> b) -- Id continuation
             -> (a -> b -> b) -- TyVar continuation
             -> (a -> b -> b) -- CoVar continuation
             -> b -> b
foldVarlikes fold as id tv co acc = fold go acc as
  where go x a res | isTyVar x = tv a res
                   | isCoVar x = co a res
                   | otherwise = id a res

emptyRenaming :: Renaming
emptyRenaming = (emptyVarEnv, emptyVarEnv, emptyVarEnv)

mkIdentityRenaming :: FreeVars -> Renaming
mkIdentityRenaming fvs = foldVarlikes (\f -> foldVarSet (\x -> f x x)) fvs
                                      (\x -> first3  (\id_subst -> extendVarEnv id_subst x (mkIdExpr x)))
                                      (\a -> second3 (\tv_subst -> extendVarEnv tv_subst a (mkTyVarTy a)))
                                      (\q -> third3  (\co_subst -> extendVarEnv co_subst q (mkCoVarCo q)))
                                      (emptyVarEnv, emptyVarEnv, emptyVarEnv)

mkInScopeIdentityRenaming :: InScopeSet -> Renaming
mkInScopeIdentityRenaming = mkIdentityRenaming . getInScopeVars

mkTyVarRenaming :: [(TyVar, Type)] -> Renaming
mkTyVarRenaming aas = (emptyVarEnv, mkVarEnv aas, emptyVarEnv)

mkRenaming :: M.Map Var Var -> Renaming
mkRenaming rn = foldVarlikes (\f -> M.foldWithKey (\x x' -> f x (x, x'))) rn
                             (\(x, x') -> first3  (\id_subst -> extendVarEnv id_subst x (mkIdExpr x')))
                             (\(a, a') -> second3 (\tv_subst -> extendVarEnv tv_subst a (mkTyVarTy a')))
                             (\(q, q') -> third3  (\co_subst -> extendVarEnv co_subst q (mkCoVarCo q')))
                             (emptyVarEnv, emptyVarEnv, emptyVarEnv)

type PreRenaming = (VarEnv Id, VarEnv TyVar, VarEnv CoVar)

-- NB: the output Vars in the range of the mappings are dodgy and should really only be used for
-- their Uniques. I turn them into full Ids mostly for convenience.
--
-- NB: the InScopeSet should be that of the *domain* of the renaming (I think!)
--
-- NB: I used to return a real *Renaming* as the result, but that wasn't very convenient for the MSG caller:
--  1. It hides the fact that looking up a CoVar/TyVar always yields a variable
--  2. It doesn't let us easily test if a variable is actually present in the domain of the inverted renaming
invertRenaming :: InScopeSet -> Renaming -> Maybe PreRenaming
invertRenaming ids (id_subst, tv_subst, co_subst)
  = mfix $ \rn -> let -- FIXME: this inversion relies on something of a hack because the domain of the mapping is not stored (only its Unique)
                      -- Furthermore, we want to carefully rename the *types* (and extra info, if we actually preserved any) as well when doing
                      -- this inversion so that the renaming {a |-> b, y |-> x :: b} is inverted to {b |-> a, x |-> y :: a}
                      invertVarEnv :: (FastString -> Unique -> Type -> Var)
                                   -> VarEnv Var -> Maybe (VarEnv Var)
                      invertVarEnv mk env
                        | distinct (varEnvElts env) = Just (mkVarEnv [ (x, if isGlobalId x && u == varUnique x
                                                                            then x -- So we don't replace global Ids with new local Ids!
                                                                            else mk (occNameFS (getOccName x)) u (renameType ids (mkRenaming' rn) (varType x)))
                                                                     | (u, x) <- ufmToList env])
                        | otherwise                 = Nothing
                  in liftM3 (,,) (traverse getId_maybe    id_subst >>= invertVarEnv  mkSysLocal)
                                 (traverse getTyVar_maybe tv_subst >>= invertVarEnv (\fs uniq -> mkTyVar (mkSysTvName uniq fs)))
                                 (traverse getCoVar_maybe co_subst >>= invertVarEnv mkSysLocal)
  where
    mkRenaming' :: PreRenaming -> Renaming
    mkRenaming' (xxs, aas, qqs) = (mapVarEnv mkIdExpr  xxs,
                                   mapVarEnv mkTyVarTy aas,
                                   mapVarEnv mkCoVarCo qqs)

composeRenamings :: PreRenaming -> Renaming -> Renaming
composeRenamings (id_subst1, tv_subst1, co_subst1) rn2
  = (mapVarEnv (mkIdExpr . renameId rn2) id_subst1,
     mapVarEnv (lookupTyVarSubst    rn2) tv_subst1,
     mapVarEnv (lookupCoVarSubst    rn2) co_subst1)

restrictRenaming :: Renaming -> VarSet -> Renaming
restrictRenaming (id_subst, tv_subst, co_subst) fvs = (id_subst `restrictVarEnv` fvs, tv_subst `restrictVarEnv` fvs, co_subst `restrictVarEnv` fvs)

mkIdExpr :: Id -> CoreSyn.CoreExpr
mkIdExpr = CoreSyn.Var

getId_maybe :: CoreSyn.CoreExpr -> Maybe Id
getId_maybe (CoreSyn.Var x') = Just x'
getId_maybe _                = Nothing

coreSynToVar :: CoreSyn.CoreExpr -> Var
coreSynToVar = fromMaybe (panic "renameId" empty) . getId_maybe

insertVarRenaming :: Renaming -> Var -> Out Var -> Renaming
insertVarRenaming rn x y
  | isTyVar x = insertTypeSubst     rn x (mkTyVarTy y)
  | isCoVar x = insertCoercionSubst rn x (mkCoVarCo y)
  | otherwise = insertIdRenaming    rn x y

insertIdRenaming :: Renaming -> Id -> Out Id -> Renaming
insertIdRenaming (id_subst, tv_subst, co_subst) x x'
  = (extendVarEnv id_subst x (mkIdExpr x'), tv_subst, co_subst)

insertIdRenamings :: Renaming -> [(Id, Out Id)] -> Renaming
insertIdRenamings = foldr (\(x, x') rn -> insertIdRenaming rn x x')

insertTypeSubst :: Renaming -> TyVar -> Out Type -> Renaming
insertTypeSubst (id_subst, tv_subst, co_subst) x ty' = (id_subst, extendVarEnv tv_subst x ty', co_subst)

insertTypeSubsts :: Renaming -> [(TyVar, Out Type)] -> Renaming
insertTypeSubsts (id_subst, tv_subst, co_subst) xtys = (id_subst, extendVarEnvList tv_subst xtys, co_subst)

insertCoercionSubst :: Renaming -> CoVar -> Out Coercion -> Renaming
insertCoercionSubst (id_subst, tv_subst, co_subst) x co' = (id_subst, tv_subst, extendVarEnv co_subst x co')

insertCoercionSubsts :: Renaming -> [(CoVar, Out Coercion)] -> Renaming
insertCoercionSubsts (id_subst, tv_subst, co_subst) xcos = (id_subst, tv_subst, extendVarEnvList co_subst xcos)

-- NB: these three function can supply emptyInScopeSet because of what I do in splitSubst

renameId :: Renaming -> Id -> Out Id
renameId rn = coreSynToVar . lookupIdSubst (text "renameId") (joinSubst emptyInScopeSet rn)

lookupTyVarSubst :: Renaming -> TyVar -> Out Type
lookupTyVarSubst rn = lookupTvSubst (joinSubst emptyInScopeSet rn)

lookupCoVarSubst :: Renaming -> CoVar -> Out Coercion
lookupCoVarSubst rn = lookupCvSubst (joinSubst emptyInScopeSet rn)


type In a = (Renaming, a)
type Out a = a


inFreeVars :: (a -> FreeVars) -> In a -> FreeVars
inFreeVars thing_fvs (rn, thing) = renameFreeVars rn (thing_fvs thing)

renameFreeVars :: Renaming -> FreeVars -> FreeVars
renameFreeVars rn fvs = foldVarlikes (\f -> foldVarSet (\x -> f x x)) fvs
                                     (\x -> flip extendVarSet (renameId rn x))
                                     (\a -> unionVarSet (tyVarsOfType (lookupTyVarSubst rn a)))
                                     (\q -> unionVarSet (tyCoVarsOfCo (lookupCoVarSubst rn q)))
                                     emptyVarSet

renameType :: InScopeSet -> Renaming -> Type -> Type
renameType iss rn = substTy (joinSubst iss rn)

renameCoercion :: InScopeSet -> Renaming -> Coercion -> NormalCo
renameCoercion iss (_, tv_subst, co_subst) = optCoercion (CvSubst iss tv_subst co_subst)


renameIn :: (Renaming -> a -> a) -> In a -> a
renameIn f (rn, x) = f rn x


renameBinders :: InScopeSet -> Renaming -> [Var] -> (InScopeSet, Renaming, [Var])
renameBinders iss rn xs = (iss', rn', xs')
  where (subst', xs') = substRecBndrs (joinSubst iss rn) xs
        (iss', rn') = splitSubst subst' (xs `zip` xs')

renameNonRecBinder :: InScopeSet -> Renaming -> Var -> (InScopeSet, Renaming, Var)
renameNonRecBinder iss rn x = (iss', rn', x')
  where (subst', x') = substBndr (joinSubst iss rn) x
        (iss', rn') = splitSubst subst' [(x, x')]

renameNonRecBinders :: InScopeSet -> Renaming -> [Var] -> (InScopeSet, Renaming, [Var])
renameNonRecBinders iss rn xs = (iss', rn', xs')
  where (subst', xs') = substBndrs (joinSubst iss rn) xs
        (iss', rn') = splitSubst subst' (xs `zip` xs')


renameBounds :: InScopeSet -> Renaming -> [(Var, a)] -> (InScopeSet, Renaming, [(Var, In a)])
renameBounds iss rn xes = (iss', rn', xs' `zip` map ((,) rn') es)
  where (xs, es) = unzip xes
        (iss', rn', xs') = renameBinders iss rn xs

renameNonRecBound :: InScopeSet -> Renaming -> (Var, a) -> (InScopeSet, Renaming, (Var, In a))
renameNonRecBound iss rn (x, e) = (iss', rn', (x', (rn, e)))
  where (iss', rn', x') = renameNonRecBinder iss rn x


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
      Var x -> Var (renameId rn x)
      Value v -> Value (value' ids rn v)
      TyApp e ty -> TyApp (term ids rn e) (renameType ids rn ty)
      CoApp e co -> CoApp (term ids rn e) (renameCoercion ids rn co)
      App e x -> App (term ids rn e) (renameId rn x)
      PrimOp pop tys es -> PrimOp pop (map (renameType ids rn) tys) (map (term ids rn) es)
      Case e x ty alts -> Case (term ids rn e) x' (renameType ids rn ty) (alternatives ids' rn' alts)
        where (ids', rn', x') = renameNonRecBinder ids rn x
      Let x e1 e2 -> Let x' (renameIn (term ids) in_e1) (term ids' rn' e2)
        where (ids', rn', (x', in_e1)) = renameNonRecBound ids rn (x, e1)
      LetRec xes e -> LetRec (map (second (renameIn (term ids'))) xes') (term ids' rn' e)
        where (ids', rn', xes') = renameBounds ids rn xes
      Cast e co -> Cast (term ids rn e) (renameCoercion ids rn co)
    
    value ids rn = rec (value' ids) rn
    value' ids rn v = renameValueG term ids rn v
    
    alternatives ids rn = map (alternative ids rn)
    
    alternative ids rn (alt_con, alt_e) = (alt_con', term ids' rn' alt_e)
        where (ids', rn', alt_con') = renameAltCon ids rn alt_con

renameValueG :: (InScopeSet -> Renaming -> a -> b)
             -> InScopeSet -> Renaming -> ValueG a -> ValueG b
renameValueG term ids rn v = case v of
      TyLambda x e -> TyLambda x' (term ids' rn' e)
        where (ids', rn', x') = renameNonRecBinder ids rn x
      Lambda x e -> Lambda x' (term ids' rn' e)
        where (ids', rn', x') = renameNonRecBinder ids rn x
      Data dc tys cos xs -> Data dc (map (renameType ids rn) tys) (map (renameCoercion ids rn) cos) (map (renameId rn) xs)
      Literal l -> Literal l
      Coercion co -> Coercion (renameCoercion ids rn co)

renameAltCon :: InScopeSet -> Renaming -> AltCon -> (InScopeSet, Renaming, AltCon)
renameAltCon ids rn_alt alt_con = case alt_con of
    DataAlt alt_dc alt_as alt_qs alt_xs -> third3 (DataAlt alt_dc alt_as' alt_qs') $ renameNonRecBinders ids1 rn_alt1 alt_xs
      where (ids0, rn_alt0, alt_as') = renameNonRecBinders ids rn_alt alt_as
            (ids1, rn_alt1, alt_qs') = renameNonRecBinders ids0 rn_alt0 alt_qs
    LiteralAlt _                        -> (ids, rn_alt, alt_con)
    DefaultAlt                          -> (ids, rn_alt, alt_con)

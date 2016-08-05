{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Utility functions on @Core@ syntax
-}

{-# LANGUAGE CPP #-}
module CoreSubst (
        -- * Main data types
        Subst(..), -- Implementation exported for supercompiler's Renaming.hs only
        TvSubstEnv, IdSubstEnv, InScopeSet,

        -- ** Substituting into expressions and related types
        deShadowBinds, substSpec, substRulesForImportedIds,
        substTy, substCo, substExpr, substExprSC, substBind, substBindSC,
        substUnfolding, substUnfoldingSC,
        lookupIdSubst, lookupTCvSubst, substIdOcc,
        substTickish, substDVarSet,

        -- ** Operations on substitutions
        emptySubst, mkEmptySubst, mkSubst, mkOpenSubst, substInScope, isEmptySubst,
        extendIdSubst, extendIdSubstList, extendTCvSubst, extendTvSubstList,
        extendSubst, extendSubstList, extendSubstWithVar, zapSubstEnv,
        addInScopeSet, extendInScope, extendInScopeList, extendInScopeIds,
        isInScope, setInScope,
        delBndr, delBndrs,

        -- ** Substituting and cloning binders
        substBndr, substBndrs, substRecBndrs,
        cloneBndr, cloneBndrs, cloneIdBndr, cloneIdBndrs, cloneRecIdBndrs,

        -- ** Simple expression optimiser
        simpleOptPgm, simpleOptExpr, simpleOptExprWith,
        exprIsConApp_maybe, exprIsLiteral_maybe, exprIsLambda_maybe,
    ) where

#include "HsVersions.h"

import CoreSyn
import CoreFVs
import CoreSeq
import CoreUtils
import Literal  ( Literal(MachStr) )
import qualified Data.ByteString as BS
import OccurAnal( occurAnalyseExpr, occurAnalysePgm )

import qualified Type
import qualified Coercion

        -- We are defining local versions
import Type     hiding ( substTy, extendTvSubst, extendCvSubst, extendTvSubstList
                       , isInScope, substTyVarBndr, cloneTyVarBndr )
import Coercion hiding ( substCo, substCoVarBndr )

import TyCon       ( tyConArity )
import DataCon
import PrelNames
import OptCoercion ( optCoercion )
import PprCore     ( pprCoreBindings, pprRules )
import Module      ( Module )
import VarSet
import VarEnv
import Id
import Name     ( Name )
import Var
import IdInfo
import UniqSupply
import Maybes
import ErrUtils
import DynFlags
import BasicTypes ( isAlwaysActive )
import Util
import Pair
import Outputable
import PprCore          ()              -- Instances
import FastString

import Data.List

import TysWiredIn

{-
************************************************************************
*                                                                      *
\subsection{Substitutions}
*                                                                      *
************************************************************************
-}

-- | A substitution environment, containing 'Id', 'TyVar', and 'CoVar'
-- substitutions.
--
-- Some invariants apply to how you use the substitution:
--
-- 1. #in_scope_invariant# The in-scope set contains at least those 'Id's and 'TyVar's that will be in scope /after/
-- applying the substitution to a term. Precisely, the in-scope set must be a superset of the free vars of the
-- substitution range that might possibly clash with locally-bound variables in the thing being substituted in.
--
-- 2. #apply_once# You may apply the substitution only /once/
--
-- There are various ways of setting up the in-scope set such that the first of these invariants hold:
--
-- * Arrange that the in-scope set really is all the things in scope
--
-- * Arrange that it's the free vars of the range of the substitution
--
-- * Make it empty, if you know that all the free vars of the substitution are fresh, and hence can't possibly clash
data Subst
  = Subst InScopeSet  -- Variables in in scope (both Ids and TyVars) /after/
                      -- applying the substitution
          IdSubstEnv  -- Substitution from NcIds to CoreExprs
          TvSubstEnv  -- Substitution from TyVars to Types
          CvSubstEnv  -- Substitution from CoVars to Coercions

        -- INVARIANT 1: See #in_scope_invariant#
        -- This is what lets us deal with name capture properly
        -- It's a hard invariant to check...
        --
        -- INVARIANT 2: The substitution is apply-once; see Note [Apply once] with
        --              Types.TvSubstEnv
        --
        -- INVARIANT 3: See Note [Extending the Subst]

{-
Note [Extending the Subst]
~~~~~~~~~~~~~~~~~~~~~~~~~~
For a core Subst, which binds Ids as well, we make a different choice for Ids
than we do for TyVars.

For TyVars, see Note [Extending the TCvSubst] with Type.TvSubstEnv

For Ids, we have a different invariant
        The IdSubstEnv is extended *only* when the Unique on an Id changes
        Otherwise, we just extend the InScopeSet

In consequence:

* If all subst envs are empty, substExpr would be a
  no-op, so substExprSC ("short cut") does nothing.

  However, substExpr still goes ahead and substitutes.  Reason: we may
  want to replace existing Ids with new ones from the in-scope set, to
  avoid space leaks.

* In substIdBndr, we extend the IdSubstEnv only when the unique changes

* If the CvSubstEnv, TvSubstEnv and IdSubstEnv are all empty,
  substExpr does nothing (Note that the above rule for substIdBndr
  maintains this property.  If the incoming envts are both empty, then
  substituting the type and IdInfo can't change anything.)

* In lookupIdSubst, we *must* look up the Id in the in-scope set, because
  it may contain non-trivial changes.  Example:
        (/\a. \x:a. ...x...) Int
  We extend the TvSubstEnv with [a |-> Int]; but x's unique does not change
  so we only extend the in-scope set.  Then we must look up in the in-scope
  set when we find the occurrence of x.

* The requirement to look up the Id in the in-scope set means that we
  must NOT take no-op short cut when the IdSubst is empty.
  We must still look up every Id in the in-scope set.

* (However, we don't need to do so for expressions found in the IdSubst
  itself, whose range is assumed to be correct wrt the in-scope set.)

Why do we make a different choice for the IdSubstEnv than the
TvSubstEnv and CvSubstEnv?

* For Ids, we change the IdInfo all the time (e.g. deleting the
  unfolding), and adding it back later, so using the TyVar convention
  would entail extending the substitution almost all the time

* The simplifier wants to look up in the in-scope set anyway, in case it
  can see a better unfolding from an enclosing case expression

* For TyVars, only coercion variables can possibly change, and they are
  easy to spot
-}

-- | An environment for substituting for 'Id's
type IdSubstEnv = IdEnv CoreExpr   -- Domain is NcIds, i.e. not coercions

----------------------------
isEmptySubst :: Subst -> Bool
isEmptySubst (Subst _ id_env tv_env cv_env)
  = isEmptyVarEnv id_env && isEmptyVarEnv tv_env && isEmptyVarEnv cv_env

emptySubst :: Subst
emptySubst = Subst emptyInScopeSet emptyVarEnv emptyVarEnv emptyVarEnv

mkEmptySubst :: InScopeSet -> Subst
mkEmptySubst in_scope = Subst in_scope emptyVarEnv emptyVarEnv emptyVarEnv

mkSubst :: InScopeSet -> TvSubstEnv -> CvSubstEnv -> IdSubstEnv -> Subst
mkSubst in_scope tvs cvs ids = Subst in_scope ids tvs cvs

-- | Find the in-scope set: see "CoreSubst#in_scope_invariant"
substInScope :: Subst -> InScopeSet
substInScope (Subst in_scope _ _ _) = in_scope

-- | Remove all substitutions for 'Id's and 'Var's that might have been built up
-- while preserving the in-scope set
zapSubstEnv :: Subst -> Subst
zapSubstEnv (Subst in_scope _ _ _) = Subst in_scope emptyVarEnv emptyVarEnv emptyVarEnv

-- | Add a substitution for an 'Id' to the 'Subst': you must ensure that the in-scope set is
-- such that the "CoreSubst#in_scope_invariant" is true after extending the substitution like this
extendIdSubst :: Subst -> Id -> CoreExpr -> Subst
-- ToDo: add an ASSERT that fvs(subst-result) is already in the in-scope set
extendIdSubst (Subst in_scope ids tvs cvs) v r
  = ASSERT2( isNonCoVarId v, ppr v $$ ppr r )
    Subst in_scope (extendVarEnv ids v r) tvs cvs

-- | Adds multiple 'Id' substitutions to the 'Subst': see also 'extendIdSubst'
extendIdSubstList :: Subst -> [(Id, CoreExpr)] -> Subst
extendIdSubstList (Subst in_scope ids tvs cvs) prs
  = ASSERT( all (isNonCoVarId . fst) prs )
    Subst in_scope (extendVarEnvList ids prs) tvs cvs

-- | Add a substitution for a 'TyVar' to the 'Subst'
-- The 'TyVar' *must* be a real TyVar, and not a CoVar
-- You must ensure that the in-scope set is such that
-- the "CoreSubst#in_scope_invariant" is true after extending
-- the substitution like this.
extendTvSubst :: Subst -> TyVar -> Type -> Subst
extendTvSubst (Subst in_scope ids tvs cvs) tv ty
  = ASSERT( isTyVar tv )
    Subst in_scope ids (extendVarEnv tvs tv ty) cvs

-- | Adds multiple 'TyVar' substitutions to the 'Subst': see also 'extendTvSubst'
extendTvSubstList :: Subst -> [(TyVar,Type)] -> Subst
extendTvSubstList subst vrs
  = foldl' extend subst vrs
  where
    extend subst (v, r) = extendTvSubst subst v r

-- | Add a substitution from a 'CoVar' to a 'Coercion' to the 'Subst': you must ensure that the in-scope set is
-- such that the "CoreSubst#in_scope_invariant" is true after extending the substitution like this
extendCvSubst :: Subst -> CoVar -> Coercion -> Subst
extendCvSubst (Subst in_scope ids tvs cvs) v r
  = ASSERT( isCoVar v )
    Subst in_scope ids tvs (extendVarEnv cvs v r)

-- | Add a substitution appropriate to the thing being substituted
--   (whether an expression, type, or coercion). See also
--   'extendIdSubst', 'extendTvSubst', 'extendCvSubst'
extendSubst :: Subst -> Var -> CoreArg -> Subst
extendSubst subst var arg
  = case arg of
      Type ty     -> ASSERT( isTyVar var ) extendTvSubst subst var ty
      Coercion co -> ASSERT( isCoVar var ) extendCvSubst subst var co
      _           -> ASSERT( isId    var ) extendIdSubst subst var arg

extendSubstWithVar :: Subst -> Var -> Var -> Subst
extendSubstWithVar subst v1 v2
  | isTyVar v1 = ASSERT( isTyVar v2 ) extendTvSubst subst v1 (mkTyVarTy v2)
  | isCoVar v1 = ASSERT( isCoVar v2 ) extendCvSubst subst v1 (mkCoVarCo v2)
  | otherwise  = ASSERT( isId    v2 ) extendIdSubst subst v1 (Var v2)

-- | Add a substitution as appropriate to each of the terms being
--   substituted (whether expressions, types, or coercions). See also
--   'extendSubst'.
extendSubstList :: Subst -> [(Var,CoreArg)] -> Subst
extendSubstList subst []              = subst
extendSubstList subst ((var,rhs):prs) = extendSubstList (extendSubst subst var rhs) prs

-- | Find the substitution for an 'Id' in the 'Subst'
lookupIdSubst :: SDoc -> Subst -> Id -> CoreExpr
lookupIdSubst doc (Subst in_scope ids _ _) v
  | not (isLocalId v) = Var v
  | Just e  <- lookupVarEnv ids       v = e
  | Just v' <- lookupInScope in_scope v = Var v'
        -- Vital! See Note [Extending the Subst]
  | otherwise = WARN( True, text "CoreSubst.lookupIdSubst" <+> doc <+> ppr v
                            $$ ppr in_scope)
                Var v

-- | Find the substitution for a 'TyVar' in the 'Subst'
lookupTCvSubst :: Subst -> TyVar -> Type
lookupTCvSubst (Subst _ _ tvs cvs) v
  | isTyVar v
  = lookupVarEnv tvs v `orElse` Type.mkTyVarTy v
  | otherwise
  = mkCoercionTy $ lookupVarEnv cvs v `orElse` mkCoVarCo v

delBndr :: Subst -> Var -> Subst
delBndr (Subst in_scope ids tvs cvs) v
  | isCoVar v = Subst in_scope ids tvs (delVarEnv cvs v)
  | isTyVar v = Subst in_scope ids (delVarEnv tvs v) cvs
  | otherwise = Subst in_scope (delVarEnv ids v) tvs cvs

delBndrs :: Subst -> [Var] -> Subst
delBndrs (Subst in_scope ids tvs cvs) vs
  = Subst in_scope (delVarEnvList ids vs) (delVarEnvList tvs vs) (delVarEnvList cvs vs)
      -- Easiest thing is just delete all from all!

-- | Simultaneously substitute for a bunch of variables
--   No left-right shadowing
--   ie the substitution for   (\x \y. e) a1 a2
--      so neither x nor y scope over a1 a2
mkOpenSubst :: InScopeSet -> [(Var,CoreArg)] -> Subst
mkOpenSubst in_scope pairs = Subst in_scope
                                   (mkVarEnv [(id,e)  | (id, e) <- pairs, isId id])
                                   (mkVarEnv [(tv,ty) | (tv, Type ty) <- pairs])
                                   (mkVarEnv [(v,co)  | (v, Coercion co) <- pairs])

------------------------------
isInScope :: Var -> Subst -> Bool
isInScope v (Subst in_scope _ _ _) = v `elemInScopeSet` in_scope

-- | Add the 'Var' to the in-scope set, but do not remove
-- any existing substitutions for it
addInScopeSet :: Subst -> VarSet -> Subst
addInScopeSet (Subst in_scope ids tvs cvs) vs
  = Subst (in_scope `extendInScopeSetSet` vs) ids tvs cvs

-- | Add the 'Var' to the in-scope set: as a side effect,
-- and remove any existing substitutions for it
extendInScope :: Subst -> Var -> Subst
extendInScope (Subst in_scope ids tvs cvs) v
  = Subst (in_scope `extendInScopeSet` v)
          (ids `delVarEnv` v) (tvs `delVarEnv` v) (cvs `delVarEnv` v)

-- | Add the 'Var's to the in-scope set: see also 'extendInScope'
extendInScopeList :: Subst -> [Var] -> Subst
extendInScopeList (Subst in_scope ids tvs cvs) vs
  = Subst (in_scope `extendInScopeSetList` vs)
          (ids `delVarEnvList` vs) (tvs `delVarEnvList` vs) (cvs `delVarEnvList` vs)

-- | Optimized version of 'extendInScopeList' that can be used if you are certain
-- all the things being added are 'Id's and hence none are 'TyVar's or 'CoVar's
extendInScopeIds :: Subst -> [Id] -> Subst
extendInScopeIds (Subst in_scope ids tvs cvs) vs
  = Subst (in_scope `extendInScopeSetList` vs)
          (ids `delVarEnvList` vs) tvs cvs

setInScope :: Subst -> InScopeSet -> Subst
setInScope (Subst _ ids tvs cvs) in_scope = Subst in_scope ids tvs cvs

-- Pretty printing, for debugging only

instance Outputable Subst where
  ppr (Subst in_scope ids tvs cvs)
        =  text "<InScope =" <+> in_scope_doc
        $$ text " IdSubst   =" <+> ppr ids
        $$ text " TvSubst   =" <+> ppr tvs
        $$ text " CvSubst   =" <+> ppr cvs
         <> char '>'
    where
    in_scope_doc = pprVarSet (getInScopeVars in_scope) (braces . fsep . map ppr)

{-
************************************************************************
*                                                                      *
        Substituting expressions
*                                                                      *
************************************************************************
-}

-- | Apply a substitution to an entire 'CoreExpr'. Remember, you may only
-- apply the substitution /once/: see "CoreSubst#apply_once"
--
-- Do *not* attempt to short-cut in the case of an empty substitution!
-- See Note [Extending the Subst]
substExprSC :: SDoc -> Subst -> CoreExpr -> CoreExpr
substExprSC doc subst orig_expr
  | isEmptySubst subst = orig_expr
  | otherwise          = -- pprTrace "enter subst-expr" (doc $$ ppr orig_expr) $
                         subst_expr doc subst orig_expr

substExpr :: SDoc -> Subst -> CoreExpr -> CoreExpr
substExpr doc subst orig_expr = subst_expr doc subst orig_expr

subst_expr :: SDoc -> Subst -> CoreExpr -> CoreExpr
subst_expr doc subst expr
  = go expr
  where
    go (Var v)         = lookupIdSubst (doc $$ text "subst_expr") subst v
    go (Type ty)       = Type (substTy subst ty)
    go (Coercion co)   = Coercion (substCo subst co)
    go (Lit lit)       = Lit lit
    go (App fun arg)   = App (go fun) (go arg)
    go (Tick tickish e) = mkTick (substTickish subst tickish) (go e)
    go (Cast e co)     = Cast (go e) (substCo subst co)
       -- Do not optimise even identity coercions
       -- Reason: substitution applies to the LHS of RULES, and
       --         if you "optimise" an identity coercion, you may
       --         lose a binder. We optimise the LHS of rules at
       --         construction time

    go (Lam bndr body) = Lam bndr' (subst_expr doc subst' body)
                       where
                         (subst', bndr') = substBndr subst bndr

    go (Let bind body) = Let bind' (subst_expr doc subst' body)
                       where
                         (subst', bind') = substBind subst bind

    go (Case scrut bndr ty alts) = Case (go scrut) bndr' (substTy subst ty) (map (go_alt subst') alts)
                                 where
                                 (subst', bndr') = substBndr subst bndr

    go_alt subst (con, bndrs, rhs) = (con, bndrs', subst_expr doc subst' rhs)
                                 where
                                   (subst', bndrs') = substBndrs subst bndrs

-- | Apply a substitution to an entire 'CoreBind', additionally returning an updated 'Subst'
-- that should be used by subsequent substitutions.
substBind, substBindSC :: Subst -> CoreBind -> (Subst, CoreBind)

substBindSC subst bind    -- Short-cut if the substitution is empty
  | not (isEmptySubst subst)
  = substBind subst bind
  | otherwise
  = case bind of
       NonRec bndr rhs -> (subst', NonRec bndr' rhs)
          where
            (subst', bndr') = substBndr subst bndr
       Rec pairs -> (subst', Rec (bndrs' `zip` rhss'))
          where
            (bndrs, rhss)    = unzip pairs
            (subst', bndrs') = substRecBndrs subst bndrs
            rhss' | isEmptySubst subst'
                  = rhss
                  | otherwise
                  = map (subst_expr (text "substBindSC") subst') rhss

substBind subst (NonRec bndr rhs)
  = (subst', NonRec bndr' (subst_expr (text "substBind") subst rhs))
  where
    (subst', bndr') = substBndr subst bndr

substBind subst (Rec pairs)
   = (subst', Rec (bndrs' `zip` rhss'))
   where
       (bndrs, rhss)    = unzip pairs
       (subst', bndrs') = substRecBndrs subst bndrs
       rhss' = map (subst_expr (text "substBind") subst') rhss

-- | De-shadowing the program is sometimes a useful pre-pass. It can be done simply
-- by running over the bindings with an empty substitution, because substitution
-- returns a result that has no-shadowing guaranteed.
--
-- (Actually, within a single /type/ there might still be shadowing, because
-- 'substTy' is a no-op for the empty substitution, but that's probably OK.)
--
-- [Aug 09] This function is not used in GHC at the moment, but seems so
--          short and simple that I'm going to leave it here
deShadowBinds :: CoreProgram -> CoreProgram
deShadowBinds binds = snd (mapAccumL substBind emptySubst binds)

{-
************************************************************************
*                                                                      *
        Substituting binders
*                                                                      *
************************************************************************

Remember that substBndr and friends are used when doing expression
substitution only.  Their only business is substitution, so they
preserve all IdInfo (suitably substituted).  For example, we *want* to
preserve occ info in rules.
-}

-- | Substitutes a 'Var' for another one according to the 'Subst' given, returning
-- the result and an updated 'Subst' that should be used by subsequent substitutions.
-- 'IdInfo' is preserved by this process, although it is substituted into appropriately.
substBndr :: Subst -> Var -> (Subst, Var)
substBndr subst bndr
  | isTyVar bndr  = substTyVarBndr subst bndr
  | isCoVar bndr  = substCoVarBndr subst bndr
  | otherwise     = substIdBndr (text "var-bndr") subst subst bndr

-- | Applies 'substBndr' to a number of 'Var's, accumulating a new 'Subst' left-to-right
substBndrs :: Subst -> [Var] -> (Subst, [Var])
substBndrs subst bndrs = mapAccumL substBndr subst bndrs

-- | Substitute in a mutually recursive group of 'Id's
substRecBndrs :: Subst -> [Id] -> (Subst, [Id])
substRecBndrs subst bndrs
  = (new_subst, new_bndrs)
  where         -- Here's the reason we need to pass rec_subst to subst_id
    (new_subst, new_bndrs) = mapAccumL (substIdBndr (text "rec-bndr") new_subst) subst bndrs

substIdBndr :: SDoc
            -> Subst            -- ^ Substitution to use for the IdInfo
            -> Subst -> Id      -- ^ Substitution and Id to transform
            -> (Subst, Id)      -- ^ Transformed pair
                                -- NB: unfolding may be zapped

substIdBndr _doc rec_subst subst@(Subst in_scope env tvs cvs) old_id
  = -- pprTrace "substIdBndr" (doc $$ ppr old_id $$ ppr in_scope) $
    (Subst (in_scope `extendInScopeSet` new_id) new_env tvs cvs, new_id)
  where
    id1 = uniqAway in_scope old_id      -- id1 is cloned if necessary
    id2 | no_type_change = id1
        | otherwise      = setIdType id1 (substTy subst old_ty)

    old_ty = idType old_id
    no_type_change = (isEmptyVarEnv tvs && isEmptyVarEnv cvs) ||
                     isEmptyVarSet (tyCoVarsOfType old_ty)

        -- new_id has the right IdInfo
        -- The lazy-set is because we're in a loop here, with
        -- rec_subst, when dealing with a mutually-recursive group
    new_id = maybeModifyIdInfo mb_new_info id2
    mb_new_info = substIdInfo rec_subst id2 (idInfo id2)
        -- NB: unfolding info may be zapped

        -- Extend the substitution if the unique has changed
        -- See the notes with substTyVarBndr for the delVarEnv
    new_env | no_change = delVarEnv env old_id
            | otherwise = extendVarEnv env old_id (Var new_id)

    no_change = id1 == old_id
        -- See Note [Extending the Subst]
        -- it's /not/ necessary to check mb_new_info and no_type_change

{-
Now a variant that unconditionally allocates a new unique.
It also unconditionally zaps the OccInfo.
-}

-- | Very similar to 'substBndr', but it always allocates a new 'Unique' for
-- each variable in its output.  It substitutes the IdInfo though.
cloneIdBndr :: Subst -> UniqSupply -> Id -> (Subst, Id)
cloneIdBndr subst us old_id
  = clone_id subst subst (old_id, uniqFromSupply us)

-- | Applies 'cloneIdBndr' to a number of 'Id's, accumulating a final
-- substitution from left to right
cloneIdBndrs :: Subst -> UniqSupply -> [Id] -> (Subst, [Id])
cloneIdBndrs subst us ids
  = mapAccumL (clone_id subst) subst (ids `zip` uniqsFromSupply us)

cloneBndrs :: Subst -> UniqSupply -> [Var] -> (Subst, [Var])
-- Works for all kinds of variables (typically case binders)
-- not just Ids
cloneBndrs subst us vs
  = mapAccumL (\subst (v, u) -> cloneBndr subst u v) subst (vs `zip` uniqsFromSupply us)

cloneBndr :: Subst -> Unique -> Var -> (Subst, Var)
cloneBndr subst uniq v
  | isTyVar v = cloneTyVarBndr subst v uniq
  | otherwise = clone_id subst subst (v,uniq)  -- Works for coercion variables too

-- | Clone a mutually recursive group of 'Id's
cloneRecIdBndrs :: Subst -> UniqSupply -> [Id] -> (Subst, [Id])
cloneRecIdBndrs subst us ids
  = (subst', ids')
  where
    (subst', ids') = mapAccumL (clone_id subst') subst
                               (ids `zip` uniqsFromSupply us)

-- Just like substIdBndr, except that it always makes a new unique
-- It is given the unique to use
clone_id    :: Subst                    -- Substitution for the IdInfo
            -> Subst -> (Id, Unique)    -- Substitution and Id to transform
            -> (Subst, Id)              -- Transformed pair

clone_id rec_subst subst@(Subst in_scope idvs tvs cvs) (old_id, uniq)
  = (Subst (in_scope `extendInScopeSet` new_id) new_idvs tvs new_cvs, new_id)
  where
    id1     = setVarUnique old_id uniq
    id2     = substIdType subst id1
    new_id  = maybeModifyIdInfo (substIdInfo rec_subst id2 (idInfo old_id)) id2
    (new_idvs, new_cvs) | isCoVar old_id = (idvs, extendVarEnv cvs old_id (mkCoVarCo new_id))
                        | otherwise      = (extendVarEnv idvs old_id (Var new_id), cvs)

{-
************************************************************************
*                                                                      *
                Types and Coercions
*                                                                      *
************************************************************************

For types and coercions we just call the corresponding functions in
Type and Coercion, but we have to repackage the substitution, from a
Subst to a TCvSubst.
-}

substTyVarBndr :: Subst -> TyVar -> (Subst, TyVar)
substTyVarBndr (Subst in_scope id_env tv_env cv_env) tv
  = case Type.substTyVarBndr (TCvSubst in_scope tv_env cv_env) tv of
        (TCvSubst in_scope' tv_env' cv_env', tv')
           -> (Subst in_scope' id_env tv_env' cv_env', tv')

cloneTyVarBndr :: Subst -> TyVar -> Unique -> (Subst, TyVar)
cloneTyVarBndr (Subst in_scope id_env tv_env cv_env) tv uniq
  = case Type.cloneTyVarBndr (TCvSubst in_scope tv_env cv_env) tv uniq of
        (TCvSubst in_scope' tv_env' cv_env', tv')
           -> (Subst in_scope' id_env tv_env' cv_env', tv')

substCoVarBndr :: Subst -> TyVar -> (Subst, TyVar)
substCoVarBndr (Subst in_scope id_env tv_env cv_env) cv
  = case Coercion.substCoVarBndr (TCvSubst in_scope tv_env cv_env) cv of
        (TCvSubst in_scope' tv_env' cv_env', cv')
           -> (Subst in_scope' id_env tv_env' cv_env', cv')

-- | See 'Type.substTy'
substTy :: Subst -> Type -> Type
substTy subst ty = Type.substTyUnchecked (getTCvSubst subst) ty

getTCvSubst :: Subst -> TCvSubst
getTCvSubst (Subst in_scope _ tenv cenv) = TCvSubst in_scope tenv cenv

-- | See 'Coercion.substCo'
substCo :: Subst -> Coercion -> Coercion
substCo subst co = Coercion.substCo (getTCvSubst subst) co

{-
************************************************************************
*                                                                      *
\section{IdInfo substitution}
*                                                                      *
************************************************************************
-}

substIdType :: Subst -> Id -> Id
substIdType subst@(Subst _ _ tv_env cv_env) id
  | (isEmptyVarEnv tv_env && isEmptyVarEnv cv_env) || isEmptyVarSet (tyCoVarsOfType old_ty) = id
  | otherwise   = setIdType id (substTy subst old_ty)
                -- The tyCoVarsOfType is cheaper than it looks
                -- because we cache the free tyvars of the type
                -- in a Note in the id's type itself
  where
    old_ty = idType id

------------------
-- | Substitute into some 'IdInfo' with regard to the supplied new 'Id'.
substIdInfo :: Subst -> Id -> IdInfo -> Maybe IdInfo
substIdInfo subst new_id info
  | nothing_to_do = Nothing
  | otherwise     = Just (info `setRuleInfo`      substSpec subst new_id old_rules
                               `setUnfoldingInfo` substUnfolding subst old_unf)
  where
    old_rules     = ruleInfo info
    old_unf       = unfoldingInfo info
    nothing_to_do = isEmptyRuleInfo old_rules && isClosedUnfolding old_unf


------------------
-- | Substitutes for the 'Id's within an unfolding
substUnfolding, substUnfoldingSC :: Subst -> Unfolding -> Unfolding
        -- Seq'ing on the returned Unfolding is enough to cause
        -- all the substitutions to happen completely

substUnfoldingSC subst unf       -- Short-cut version
  | isEmptySubst subst = unf
  | otherwise          = substUnfolding subst unf

substUnfolding subst df@(DFunUnfolding { df_bndrs = bndrs, df_args = args })
  = df { df_bndrs = bndrs', df_args = args' }
  where
    (subst',bndrs') = substBndrs subst bndrs
    args'           = map (substExpr (text "subst-unf:dfun") subst') args

substUnfolding subst unf@(CoreUnfolding { uf_tmpl = tmpl, uf_src = src })
        -- Retain an InlineRule!
  | not (isStableSource src)  -- Zap an unstable unfolding, to save substitution work
  = NoUnfolding
  | otherwise                 -- But keep a stable one!
  = seqExpr new_tmpl `seq`
    unf { uf_tmpl = new_tmpl }
  where
    new_tmpl = substExpr (text "subst-unf") subst tmpl

substUnfolding _ unf = unf      -- NoUnfolding, OtherCon

------------------
substIdOcc :: Subst -> Id -> Id
-- These Ids should not be substituted to non-Ids
substIdOcc subst v = case lookupIdSubst (text "substIdOcc") subst v of
                        Var v' -> v'
                        other  -> pprPanic "substIdOcc" (vcat [ppr v <+> ppr other, ppr subst])

------------------
-- | Substitutes for the 'Id's within the 'WorkerInfo' given the new function 'Id'
substSpec :: Subst -> Id -> RuleInfo -> RuleInfo
substSpec subst new_id (RuleInfo rules rhs_fvs)
  = seqRuleInfo new_spec `seq` new_spec
  where
    subst_ru_fn = const (idName new_id)
    new_spec = RuleInfo (map (substRule subst subst_ru_fn) rules)
                        (substDVarSet subst rhs_fvs)

------------------
substRulesForImportedIds :: Subst -> [CoreRule] -> [CoreRule]
substRulesForImportedIds subst rules
  = map (substRule subst not_needed) rules
  where
    not_needed name = pprPanic "substRulesForImportedIds" (ppr name)

------------------
substRule :: Subst -> (Name -> Name) -> CoreRule -> CoreRule

-- The subst_ru_fn argument is applied to substitute the ru_fn field
-- of the rule:
--    - Rules for *imported* Ids never change ru_fn
--    - Rules for *local* Ids are in the IdInfo for that Id,
--      and the ru_fn field is simply replaced by the new name
--      of the Id
substRule _ _ rule@(BuiltinRule {}) = rule
substRule subst subst_ru_fn rule@(Rule { ru_bndrs = bndrs, ru_args = args
                                       , ru_fn = fn_name, ru_rhs = rhs
                                       , ru_local = is_local })
  = rule { ru_bndrs = bndrs'
         , ru_fn    = if is_local
                        then subst_ru_fn fn_name
                        else fn_name
         , ru_args  = map (substExpr doc subst') args
         , ru_rhs   = substExpr (text "foo") subst' rhs }
           -- Do NOT optimise the RHS (previously we did simplOptExpr here)
           -- See Note [Substitute lazily]
  where
    doc = text "subst-rule" <+> ppr fn_name
    (subst', bndrs') = substBndrs subst bndrs

------------------
substVects :: Subst -> [CoreVect] -> [CoreVect]
substVects subst = map (substVect subst)

------------------
substVect :: Subst -> CoreVect -> CoreVect
substVect subst  (Vect v rhs)        = Vect v (simpleOptExprWith subst rhs)
substVect _subst vd@(NoVect _)       = vd
substVect _subst vd@(VectType _ _ _) = vd
substVect _subst vd@(VectClass _)    = vd
substVect _subst vd@(VectInst _)     = vd

------------------
substDVarSet :: Subst -> DVarSet -> DVarSet
substDVarSet subst fvs
  = mkDVarSet $ fst $ foldr (subst_fv subst) ([], emptyVarSet) $ dVarSetElems fvs
  where
  subst_fv subst fv acc
     | isId fv = expr_fvs (lookupIdSubst (text "substDVarSet") subst fv) isLocalVar emptyVarSet $! acc
     | otherwise = tyCoFVsOfType (lookupTCvSubst subst fv) (const True) emptyVarSet $! acc

------------------
substTickish :: Subst -> Tickish Id -> Tickish Id
substTickish subst (Breakpoint n ids)
   = Breakpoint n (map do_one ids)
 where
    do_one = getIdFromTrivialExpr . lookupIdSubst (text "subst_tickish") subst
substTickish _subst other = other

{- Note [Substitute lazily]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
The functions that substitute over IdInfo must be pretty lazy, because
they are knot-tied by substRecBndrs.

One case in point was Trac #10627 in which a rule for a function 'f'
referred to 'f' (at a differnet type) on the RHS.  But instead of just
substituting in the rhs of the rule, we were calling simpleOptExpr, which
looked at the idInfo for 'f'; result <<loop>>.

In any case we don't need to optimise the RHS of rules, or unfoldings,
because the simplifier will do that.


Note [substTickish]
~~~~~~~~~~~~~~~~~~~~~~
A Breakpoint contains a list of Ids.  What happens if we ever want to
substitute an expression for one of these Ids?

First, we ensure that we only ever substitute trivial expressions for
these Ids, by marking them as NoOccInfo in the occurrence analyser.
Then, when substituting for the Id, we unwrap any type applications
and abstractions to get back to an Id, with getIdFromTrivialExpr.

Second, we have to ensure that we never try to substitute a literal
for an Id in a breakpoint.  We ensure this by never storing an Id with
an unlifted type in a Breakpoint - see Coverage.mkTickish.
Breakpoints can't handle free variables with unlifted types anyway.
-}

{-
Note [Worker inlining]
~~~~~~~~~~~~~~~~~~~~~~
A worker can get sustituted away entirely.
        - it might be trivial
        - it might simply be very small
We do not treat an InlWrapper as an 'occurrence' in the occurrence
analyser, so it's possible that the worker is not even in scope any more.

In all all these cases we simply drop the special case, returning to
InlVanilla.  The WARN is just so I can see if it happens a lot.


************************************************************************
*                                                                      *
        The Very Simple Optimiser
*                                                                      *
************************************************************************

Note [Getting the map/coerce RULE to work]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We wish to allow the "map/coerce" RULE to fire:

  {-# RULES "map/coerce" map coerce = coerce #-}

The naive core produced for this is

  forall a b (dict :: Coercible * a b).
    map @a @b (coerce @a @b @dict) = coerce @[a] @[b] @dict'

  where dict' :: Coercible [a] [b]
        dict' = ...

This matches literal uses of `map coerce` in code, but that's not what we
want. We want it to match, say, `map MkAge` (where newtype Age = MkAge Int)
too. Some of this is addressed by compulsorily unfolding coerce on the LHS,
yielding

  forall a b (dict :: Coercible * a b).
    map @a @b (\(x :: a) -> case dict of
      MkCoercible (co :: a ~R# b) -> x |> co) = ...

Getting better. But this isn't exactly what gets produced. This is because
Coercible essentially has ~R# as a superclass, and superclasses get eagerly
extracted during solving. So we get this:

  forall a b (dict :: Coercible * a b).
    case Coercible_SCSel @* @a @b dict of
      _ [Dead] -> map @a @b (\(x :: a) -> case dict of
                               MkCoercible (co :: a ~R# b) -> x |> co) = ...

Unfortunately, this still abstracts over a Coercible dictionary. We really
want it to abstract over the ~R# evidence. So, we have Desugar.unfold_coerce,
which transforms the above to (see also Note [Desugaring coerce as cast] in
Desugar)

  forall a b (co :: a ~R# b).
    let dict = MkCoercible @* @a @b co in
    case Coercible_SCSel @* @a @b dict of
      _ [Dead] -> map @a @b (\(x :: a) -> case dict of
         MkCoercible (co :: a ~R# b) -> x |> co) = let dict = ... in ...

Now, we need simpleOptExpr to fix this up. It does so by taking three
separate actions:
  1. Inline certain non-recursive bindings. The choice whether to inline
     is made in maybe_substitute. Note the rather specific check for
     MkCoercible in there.

  2. Stripping case expressions like the Coercible_SCSel one.
     See the `Case` case of simple_opt_expr's `go` function.

  3. Look for case expressions that unpack something that was
     just packed and inline them. This is also done in simple_opt_expr's
     `go` function.

This is all a fair amount of special-purpose hackery, but it's for
a good cause. And it won't hurt other RULES and such that it comes across.

-}

simpleOptExpr :: CoreExpr -> CoreExpr
-- Do simple optimisation on an expression
-- The optimisation is very straightforward: just
-- inline non-recursive bindings that are used only once,
-- or where the RHS is trivial
--
-- We also inline bindings that bind a Eq# box: see
-- See Note [Getting the map/coerce RULE to work].
--
-- The result is NOT guaranteed occurrence-analysed, because
-- in  (let x = y in ....) we substitute for x; so y's occ-info
-- may change radically

simpleOptExpr expr
  = -- pprTrace "simpleOptExpr" (ppr init_subst $$ ppr expr)
    simpleOptExprWith init_subst expr
  where
    init_subst = mkEmptySubst (mkInScopeSet (exprFreeVars expr))
        -- It's potentially important to make a proper in-scope set
        -- Consider  let x = ..y.. in \y. ...x...
        -- Then we should remember to clone y before substituting
        -- for x.  It's very unlikely to occur, because we probably
        -- won't *be* substituting for x if it occurs inside a
        -- lambda.
        --
        -- It's a bit painful to call exprFreeVars, because it makes
        -- three passes instead of two (occ-anal, and go)

simpleOptExprWith :: Subst -> InExpr -> OutExpr
simpleOptExprWith subst expr = simple_opt_expr subst (occurAnalyseExpr expr)

----------------------
simpleOptPgm :: DynFlags -> Module
             -> CoreProgram -> [CoreRule] -> [CoreVect]
             -> IO (CoreProgram, [CoreRule], [CoreVect])
simpleOptPgm dflags this_mod binds rules vects
  = do { dumpIfSet_dyn dflags Opt_D_dump_occur_anal "Occurrence analysis"
                       (pprCoreBindings occ_anald_binds $$ pprRules rules );

       ; return (reverse binds', substRulesForImportedIds subst' rules, substVects subst' vects) }
  where
    occ_anald_binds  = occurAnalysePgm this_mod (\_ -> False) {- No rules active -}
                                       rules vects emptyVarEnv binds
    (subst', binds') = foldl do_one (emptySubst, []) occ_anald_binds

    do_one (subst, binds') bind
      = case simple_opt_bind subst bind of
          (subst', Nothing)    -> (subst', binds')
          (subst', Just bind') -> (subst', bind':binds')

----------------------
type InVar   = Var
type OutVar  = Var
type InId    = Id
type OutId   = Id
type InExpr  = CoreExpr
type OutExpr = CoreExpr

-- In these functions the substitution maps InVar -> OutExpr

----------------------
simple_opt_expr :: Subst -> InExpr -> OutExpr
simple_opt_expr subst expr
  = go expr
  where
    in_scope_env = (substInScope subst, simpleUnfoldingFun)

    go (Var v)          = lookupIdSubst (text "simpleOptExpr") subst v
    go (App e1 e2)      = simple_app subst e1 [go e2]
    go (Type ty)        = Type     (substTy subst ty)
    go (Coercion co)    = Coercion (optCoercion (getTCvSubst subst) co)
    go (Lit lit)        = Lit lit
    go (Tick tickish e) = mkTick (substTickish subst tickish) (go e)
    go (Cast e co)      | isReflCo co' = go e
                        | otherwise    = Cast (go e) co'
                        where
                          co' = optCoercion (getTCvSubst subst) co

    go (Let bind body) = case simple_opt_bind subst bind of
                           (subst', Nothing)   -> simple_opt_expr subst' body
                           (subst', Just bind) -> Let bind (simple_opt_expr subst' body)

    go lam@(Lam {})     = go_lam [] subst lam
    go (Case e b ty as)
       -- See Note [Getting the map/coerce RULE to work]
      | isDeadBinder b
      , Just (con, _tys, es) <- exprIsConApp_maybe in_scope_env e'
      , Just (altcon, bs, rhs) <- findAlt (DataAlt con) as
      = case altcon of
          DEFAULT -> go rhs
          _       -> mkLets (catMaybes mb_binds) $ simple_opt_expr subst' rhs
            where (subst', mb_binds) = mapAccumL simple_opt_out_bind subst
                                                 (zipEqual "simpleOptExpr" bs es)

         -- Note [Getting the map/coerce RULE to work]
      | isDeadBinder b
      , [(DEFAULT, _, rhs)] <- as
      , isCoercionType (varType b)
      , (Var fun, _args) <- collectArgs e
      , fun `hasKey` coercibleSCSelIdKey
         -- without this last check, we get #11230
      = go rhs

      | otherwise
      = Case e' b' (substTy subst ty)
                   (map (go_alt subst') as)
        where
          e' = go e
          (subst', b') = subst_opt_bndr subst b

    ----------------------
    go_alt subst (con, bndrs, rhs)
      = (con, bndrs', simple_opt_expr subst' rhs)
      where
        (subst', bndrs') = subst_opt_bndrs subst bndrs

    ----------------------
    -- go_lam tries eta reduction
    go_lam bs' subst (Lam b e)
       = go_lam (b':bs') subst' e
       where
         (subst', b') = subst_opt_bndr subst b
    go_lam bs' subst e
       | Just etad_e <- tryEtaReduce bs e' = etad_e
       | otherwise                         = mkLams bs e'
       where
         bs = reverse bs'
         e' = simple_opt_expr subst e

----------------------
-- simple_app collects arguments for beta reduction
simple_app :: Subst -> InExpr -> [OutExpr] -> CoreExpr
simple_app subst (App e1 e2) as
  = simple_app subst e1 (simple_opt_expr subst e2 : as)
simple_app subst (Lam b e) (a:as)
  = case maybe_substitute subst b a of
      Just ext_subst -> simple_app ext_subst e as
      Nothing        -> Let (NonRec b2 a) (simple_app subst' e as)
  where
    (subst', b') = subst_opt_bndr subst b
    b2 = add_info subst' b b'
simple_app subst (Var v) as
  | isCompulsoryUnfolding (idUnfolding v)
  , isAlwaysActive (idInlineActivation v)
  -- See Note [Unfold compulsory unfoldings in LHSs]
  =  simple_app subst (unfoldingTemplate (idUnfolding v)) as
simple_app subst (Tick t e) as
  -- Okay to do "(Tick t e) x ==> Tick t (e x)"?
  | t `tickishScopesLike` SoftScope
  = mkTick t $ simple_app subst e as
simple_app subst e as
  = foldl App (simple_opt_expr subst e) as

----------------------
simple_opt_bind,simple_opt_bind' :: Subst -> CoreBind -> (Subst, Maybe CoreBind)
simple_opt_bind s b               -- Can add trace stuff here
  = simple_opt_bind' s b

simple_opt_bind' subst (Rec prs)
  = (subst'', res_bind)
  where
    res_bind            = Just (Rec (reverse rev_prs'))
    (subst', bndrs')    = subst_opt_bndrs subst (map fst prs)
    (subst'', rev_prs') = foldl do_pr (subst', []) (prs `zip` bndrs')
    do_pr (subst, prs) ((b,r), b')
       = case maybe_substitute subst b r2 of
           Just subst' -> (subst', prs)
           Nothing     -> (subst,  (b2,r2):prs)
       where
         b2 = add_info subst b b'
         r2 = simple_opt_expr subst r

simple_opt_bind' subst (NonRec b r)
  = simple_opt_out_bind subst (b, simple_opt_expr subst r)

----------------------
simple_opt_out_bind :: Subst -> (InVar, OutExpr) -> (Subst, Maybe CoreBind)
simple_opt_out_bind subst (b, r')
  | Just ext_subst <- maybe_substitute subst b r'
  = (ext_subst, Nothing)
  | otherwise
  = (subst', Just (NonRec b2 r'))
  where
    (subst', b') = subst_opt_bndr subst b
    b2 = add_info subst' b b'

----------------------
maybe_substitute :: Subst -> InVar -> OutExpr -> Maybe Subst
    -- (maybe_substitute subst in_var out_rhs)
    --   either extends subst with (in_var -> out_rhs)
    --   or     returns Nothing
maybe_substitute subst b r
  | Type ty <- r        -- let a::* = TYPE ty in <body>
  = ASSERT( isTyVar b )
    Just (extendTvSubst subst b ty)

  | Coercion co <- r
  = ASSERT( isCoVar b )
    Just (extendCvSubst subst b co)

  | isId b              -- let x = e in <body>
  , not (isCoVar b)     -- See Note [Do not inline CoVars unconditionally]
                        -- in SimplUtils
  , safe_to_inline (idOccInfo b)
  , isAlwaysActive (idInlineActivation b)       -- Note [Inline prag in simplOpt]
  , not (isStableUnfolding (idUnfolding b))
  , not (isExportedId b)
  , not (isUnliftedType (idType b)) || exprOkForSpeculation r
  = Just (extendIdSubst subst b r)

  | otherwise
  = Nothing
  where
        -- Unconditionally safe to inline
    safe_to_inline :: OccInfo -> Bool
    safe_to_inline (IAmALoopBreaker {})     = False
    safe_to_inline IAmDead                  = True
    safe_to_inline (OneOcc in_lam one_br _) = (not in_lam && one_br) || trivial
    safe_to_inline NoOccInfo                = trivial

    trivial | exprIsTrivial r = True
            | (Var fun, args) <- collectArgs r
            , Just dc <- isDataConWorkId_maybe fun
            , dc `hasKey` heqDataConKey || dc `hasKey` coercibleDataConKey
            , all exprIsTrivial args = True
                     -- See Note [Getting the map/coerce RULE to work]
            | otherwise = False

----------------------
subst_opt_bndr :: Subst -> InVar -> (Subst, OutVar)
subst_opt_bndr subst bndr
  | isTyVar bndr  = substTyVarBndr subst bndr
  | isCoVar bndr  = substCoVarBndr subst bndr
  | otherwise     = subst_opt_id_bndr subst bndr

subst_opt_id_bndr :: Subst -> InId -> (Subst, OutId)
-- Nuke all fragile IdInfo, unfolding, and RULES;
--    it gets added back later by add_info
-- Rather like SimplEnv.substIdBndr
--
-- It's important to zap fragile OccInfo (which CoreSubst.substIdBndr
-- carefully does not do) because simplOptExpr invalidates it

subst_opt_id_bndr subst@(Subst in_scope id_subst tv_subst cv_subst) old_id
  = (Subst new_in_scope new_id_subst tv_subst cv_subst, new_id)
  where
    id1    = uniqAway in_scope old_id
    id2    = setIdType id1 (substTy subst (idType old_id))
    new_id = zapFragileIdInfo id2       -- Zaps rules, worker-info, unfolding
                                        -- and fragile OccInfo
    new_in_scope = in_scope `extendInScopeSet` new_id

        -- Extend the substitution if the unique has changed,
        -- or there's some useful occurrence information
        -- See the notes with substTyVarBndr for the delSubstEnv
    new_id_subst | new_id /= old_id
                 = extendVarEnv id_subst old_id (Var new_id)
                 | otherwise
                 = delVarEnv id_subst old_id

----------------------
subst_opt_bndrs :: Subst -> [InVar] -> (Subst, [OutVar])
subst_opt_bndrs subst bndrs
  = mapAccumL subst_opt_bndr subst bndrs

----------------------
add_info :: Subst -> InVar -> OutVar -> OutVar
add_info subst old_bndr new_bndr
 | isTyVar old_bndr = new_bndr
 | otherwise        = maybeModifyIdInfo mb_new_info new_bndr
 where mb_new_info = substIdInfo subst new_bndr (idInfo old_bndr)

simpleUnfoldingFun :: IdUnfoldingFun
simpleUnfoldingFun id
  | isAlwaysActive (idInlineActivation id) = idUnfolding id
  | otherwise                              = noUnfolding

{-
Note [Inline prag in simplOpt]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If there's an INLINE/NOINLINE pragma that restricts the phase in
which the binder can be inlined, we don't inline here; after all,
we don't know what phase we're in.  Here's an example

  foo :: Int -> Int -> Int
  {-# INLINE foo #-}
  foo m n = inner m
     where
       {-# INLINE [1] inner #-}
       inner m = m+n

  bar :: Int -> Int
  bar n = foo n 1

When inlining 'foo' in 'bar' we want the let-binding for 'inner'
to remain visible until Phase 1

Note [Unfold compulsory unfoldings in LHSs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When the user writes `RULES map coerce = coerce` as a rule, the rule
will only ever match if simpleOptExpr replaces coerce by its unfolding
on the LHS, because that is the core that the rule matching engine
will find. So do that for everything that has a compulsory
unfolding. Also see Note [Desugaring coerce as cast] in Desugar.

However, we don't want to inline 'seq', which happens to also have a
compulsory unfolding, so we only do this unfolding only for things
that are always-active.  See Note [User-defined RULES for seq] in MkId.


************************************************************************
*                                                                      *
         exprIsConApp_maybe
*                                                                      *
************************************************************************

Note [exprIsConApp_maybe]
~~~~~~~~~~~~~~~~~~~~~~~~~
exprIsConApp_maybe is a very important function.  There are two principal
uses:
  * case e of { .... }
  * cls_op e, where cls_op is a class operation

In both cases you want to know if e is of form (C e1..en) where C is
a data constructor.

However e might not *look* as if


Note [exprIsConApp_maybe on literal strings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See #9400.

Conceptually, a string literal "abc" is just ('a':'b':'c':[]), but in Core
they are represented as unpackCString# "abc"# by MkCore.mkStringExprFS, or
unpackCStringUtf8# when the literal contains multi-byte UTF8 characters.

For optimizations we want to be able to treat it as a list, so they can be
decomposed when used in a case-statement. exprIsConApp_maybe detects those
calls to unpackCString# and returns:

Just (':', [Char], ['a', unpackCString# "bc"]).

We need to be careful about UTF8 strings here. ""# contains a ByteString, so
we must parse it back into a FastString to split off the first character.
That way we can treat unpackCString# and unpackCStringUtf8# in the same way.
-}

data ConCont = CC [CoreExpr] Coercion
                  -- Substitution already applied

-- | Returns @Just (dc, [t1..tk], [x1..xn])@ if the argument expression is
-- a *saturated* constructor application of the form @dc t1..tk x1 .. xn@,
-- where t1..tk are the *universally-qantified* type args of 'dc'
exprIsConApp_maybe :: InScopeEnv -> CoreExpr -> Maybe (DataCon, [Type], [CoreExpr])
exprIsConApp_maybe (in_scope, id_unf) expr
  = go (Left in_scope) expr (CC [] (mkRepReflCo (exprType expr)))
  where
    go :: Either InScopeSet Subst
       -> CoreExpr -> ConCont
       -> Maybe (DataCon, [Type], [CoreExpr])
    go subst (Tick t expr) cont
       | not (tickishIsCode t) = go subst expr cont
    go subst (Cast expr co1) (CC [] co2)
       = go subst expr (CC [] (subst_co subst co1 `mkTransCo` co2))
    go subst (App fun arg) (CC args co)
       = go subst fun (CC (subst_arg subst arg : args) co)
    go subst (Lam var body) (CC (arg:args) co)
       | exprIsTrivial arg          -- Don't duplicate stuff!
       = go (extend subst var arg) body (CC args co)
    go (Right sub) (Var v) cont
       = go (Left (substInScope sub))
            (lookupIdSubst (text "exprIsConApp" <+> ppr expr) sub v)
            cont

    go (Left in_scope) (Var fun) cont@(CC args co)

        | Just con <- isDataConWorkId_maybe fun
        , count isValArg args == idArity fun
        = dealWithCoercion co con args

        -- Look through dictionary functions; see Note [Unfolding DFuns]
        | DFunUnfolding { df_bndrs = bndrs, df_con = con, df_args = dfun_args } <- unfolding
        , bndrs `equalLength` args    -- See Note [DFun arity check]
        , let subst = mkOpenSubst in_scope (bndrs `zip` args)
        = dealWithCoercion co con (map (substExpr (text "exprIsConApp1") subst) dfun_args)

        -- Look through unfoldings, but only arity-zero one;
        -- if arity > 0 we are effectively inlining a function call,
        -- and that is the business of callSiteInline.
        -- In practice, without this test, most of the "hits" were
        -- CPR'd workers getting inlined back into their wrappers,
        | idArity fun == 0
        , Just rhs <- expandUnfolding_maybe unfolding
        , let in_scope' = extendInScopeSetSet in_scope (exprFreeVars rhs)
        = go (Left in_scope') rhs cont

        | (fun `hasKey` unpackCStringIdKey)
         || (fun `hasKey` unpackCStringUtf8IdKey)
        , [Lit (MachStr str)] <- args
        = dealWithStringLiteral fun str co
        where
          unfolding = id_unf fun

    go _ _ _ = Nothing

    ----------------------------
    -- Operations on the (Either InScopeSet CoreSubst)
    -- The Left case is wildly dominant
    subst_co (Left {}) co = co
    subst_co (Right s) co = CoreSubst.substCo s co

    subst_arg (Left {}) e = e
    subst_arg (Right s) e = substExpr (text "exprIsConApp2") s e

    extend (Left in_scope) v e = Right (extendSubst (mkEmptySubst in_scope) v e)
    extend (Right s)       v e = Right (extendSubst s v e)

-- See Note [exprIsConApp_maybe on literal strings]
dealWithStringLiteral :: Var -> BS.ByteString -> Coercion
                      -> Maybe (DataCon, [Type], [CoreExpr])

-- This is not possible with user-supplied empty literals, MkCore.mkStringExprFS
-- turns those into [] automatically, but just in case something else in GHC
-- generates a string literal directly.
dealWithStringLiteral _   str co
  | BS.null str
  = dealWithCoercion co nilDataCon [Type charTy]

dealWithStringLiteral fun str co
  = let strFS = mkFastStringByteString str

        char = mkConApp charDataCon [mkCharLit (headFS strFS)]
        charTail = fastStringToByteString (tailFS strFS)

        -- In singleton strings, just add [] instead of unpackCstring# ""#.
        rest = if BS.null charTail
                 then mkConApp nilDataCon [Type charTy]
                 else App (Var fun)
                          (Lit (MachStr charTail))

    in dealWithCoercion co consDataCon [Type charTy, char, rest]

dealWithCoercion :: Coercion -> DataCon -> [CoreExpr]
                 -> Maybe (DataCon, [Type], [CoreExpr])
dealWithCoercion co dc dc_args
  | isReflCo co || from_ty `eqType` to_ty  -- try cheap test first
  , let (univ_ty_args, rest_args) = splitAtList (dataConUnivTyVars dc) dc_args
  = Just (dc, map exprToType univ_ty_args, rest_args)

  | Just (to_tc, to_tc_arg_tys) <- splitTyConApp_maybe to_ty
  , to_tc == dataConTyCon dc
        -- These two tests can fail; we might see
        --      (C x y) `cast` (g :: T a ~ S [a]),
        -- where S is a type function.  In fact, exprIsConApp
        -- will probably not be called in such circumstances,
        -- but there't nothing wrong with it

  =     -- Here we do the KPush reduction rule as described in "Down with kinds"
        -- The transformation applies iff we have
        --      (C e1 ... en) `cast` co
        -- where co :: (T t1 .. tn) ~ to_ty
        -- The left-hand one must be a T, because exprIsConApp returned True
        -- but the right-hand one might not be.  (Though it usually will.)
    let
        tc_arity       = tyConArity to_tc
        dc_univ_tyvars = dataConUnivTyVars dc
        dc_ex_tyvars   = dataConExTyVars dc
        arg_tys        = dataConRepArgTys dc

        non_univ_args  = dropList dc_univ_tyvars dc_args
        (ex_args, val_args) = splitAtList dc_ex_tyvars non_univ_args

        -- Make the "Psi" from the paper
        omegas = decomposeCo tc_arity co
        (psi_subst, to_ex_arg_tys)
          = liftCoSubstWithEx Representational
                              dc_univ_tyvars
                              omegas
                              dc_ex_tyvars
                              (map exprToType ex_args)

          -- Cast the value arguments (which include dictionaries)
        new_val_args = zipWith cast_arg arg_tys val_args
        cast_arg arg_ty arg = mkCast arg (psi_subst arg_ty)

        to_ex_args = map Type to_ex_arg_tys

        dump_doc = vcat [ppr dc,      ppr dc_univ_tyvars, ppr dc_ex_tyvars,
                         ppr arg_tys, ppr dc_args,
                         ppr ex_args, ppr val_args, ppr co, ppr from_ty, ppr to_ty, ppr to_tc ]
    in
    ASSERT2( eqType from_ty (mkTyConApp to_tc (map exprToType $ takeList dc_univ_tyvars dc_args)), dump_doc )
    ASSERT2( equalLength val_args arg_tys, dump_doc )
    Just (dc, to_tc_arg_tys, to_ex_args ++ new_val_args)

  | otherwise
  = Nothing

  where
    Pair from_ty to_ty = coercionKind co


{-
Note [Unfolding DFuns]
~~~~~~~~~~~~~~~~~~~~~~
DFuns look like

  df :: forall a b. (Eq a, Eq b) -> Eq (a,b)
  df a b d_a d_b = MkEqD (a,b) ($c1 a b d_a d_b)
                               ($c2 a b d_a d_b)

So to split it up we just need to apply the ops $c1, $c2 etc
to the very same args as the dfun.  It takes a little more work
to compute the type arguments to the dictionary constructor.

Note [DFun arity check]
~~~~~~~~~~~~~~~~~~~~~~~
Here we check that the total number of supplied arguments (inclding
type args) matches what the dfun is expecting.  This may be *less*
than the ordinary arity of the dfun: see Note [DFun unfoldings] in CoreSyn
-}

exprIsLiteral_maybe :: InScopeEnv -> CoreExpr -> Maybe Literal
-- Same deal as exprIsConApp_maybe, but much simpler
-- Nevertheless we do need to look through unfoldings for
-- Integer literals, which are vigorously hoisted to top level
-- and not subsequently inlined
exprIsLiteral_maybe env@(_, id_unf) e
  = case e of
      Lit l     -> Just l
      Tick _ e' -> exprIsLiteral_maybe env e' -- dubious?
      Var v     | Just rhs <- expandUnfolding_maybe (id_unf v)
                -> exprIsLiteral_maybe env rhs
      _         -> Nothing

{-
Note [exprIsLambda_maybe]
~~~~~~~~~~~~~~~~~~~~~~~~~~
exprIsLambda_maybe will, given an expression `e`, try to turn it into the form
`Lam v e'` (returned as `Just (v,e')`). Besides using lambdas, it looks through
casts (using the Push rule), and it unfolds function calls if the unfolding
has a greater arity than arguments are present.

Currently, it is used in Rules.match, and is required to make
"map coerce = coerce" match.
-}

exprIsLambda_maybe :: InScopeEnv -> CoreExpr
                      -> Maybe (Var, CoreExpr,[Tickish Id])
    -- See Note [exprIsLambda_maybe]

-- The simple case: It is a lambda already
exprIsLambda_maybe _ (Lam x e)
    = Just (x, e, [])

-- Still straightforward: Ticks that we can float out of the way
exprIsLambda_maybe (in_scope_set, id_unf) (Tick t e)
    | tickishFloatable t
    , Just (x, e, ts) <- exprIsLambda_maybe (in_scope_set, id_unf) e
    = Just (x, e, t:ts)

-- Also possible: A casted lambda. Push the coercion inside
exprIsLambda_maybe (in_scope_set, id_unf) (Cast casted_e co)
    | Just (x, e,ts) <- exprIsLambda_maybe (in_scope_set, id_unf) casted_e
    -- Only do value lambdas.
    -- this implies that x is not in scope in gamma (makes this code simpler)
    , not (isTyVar x) && not (isCoVar x)
    , ASSERT( not $ x `elemVarSet` tyCoVarsOfCo co) True
    , Just (x',e') <- pushCoercionIntoLambda in_scope_set x e co
    , let res = Just (x',e',ts)
    = --pprTrace "exprIsLambda_maybe:Cast" (vcat [ppr casted_e,ppr co,ppr res)])
      res

-- Another attempt: See if we find a partial unfolding
exprIsLambda_maybe (in_scope_set, id_unf) e
    | (Var f, as, ts) <- collectArgsTicks tickishFloatable e
    , idArity f > count isValArg as
    -- Make sure there is hope to get a lambda
    , Just rhs <- expandUnfolding_maybe (id_unf f)
    -- Optimize, for beta-reduction
    , let e' =  simpleOptExprWith (mkEmptySubst in_scope_set) (rhs `mkApps` as)
    -- Recurse, because of possible casts
    , Just (x', e'', ts') <- exprIsLambda_maybe (in_scope_set, id_unf) e'
    , let res = Just (x', e'', ts++ts')
    = -- pprTrace "exprIsLambda_maybe:Unfold" (vcat [ppr e, ppr (x',e'')])
      res

exprIsLambda_maybe _ _e
    = -- pprTrace "exprIsLambda_maybe:Fail" (vcat [ppr _e])
      Nothing


pushCoercionIntoLambda
    :: InScopeSet -> Var -> CoreExpr -> Coercion -> Maybe (Var, CoreExpr)
pushCoercionIntoLambda in_scope x e co
    -- This implements the Push rule from the paper on coercions
    -- Compare with simplCast in Simplify
    | ASSERT(not (isTyVar x) && not (isCoVar x)) True
    , Pair s1s2 t1t2 <- coercionKind co
    , Just (_s1,_s2) <- splitFunTy_maybe s1s2
    , Just (t1,_t2) <- splitFunTy_maybe t1t2
    = let [co1, co2] = decomposeCo 2 co
          -- Should we optimize the coercions here?
          -- Otherwise they might not match too well
          x' = x `setIdType` t1
          in_scope' = in_scope `extendInScopeSet` x'
          subst = extendIdSubst (mkEmptySubst in_scope')
                                x
                                (mkCast (Var x') co1)
      in Just (x', subst_expr (text "pushCoercionIntoLambda") subst e `mkCast` co2)
    | otherwise
    = pprTrace "exprIsLambda_maybe: Unexpected lambda in case" (ppr (Lam x e))
      Nothing

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
        substTickish, substDVarSet, substIdInfo,

        -- ** Operations on substitutions
        emptySubst, mkEmptySubst, mkSubst, mkOpenSubst, substInScope, isEmptySubst,
        extendIdSubst, extendIdSubstList, extendTCvSubst, extendTvSubstList,
        extendSubst, extendSubstList, extendSubstWithVar, zapSubstEnv,
        addInScopeSet, extendInScope, extendInScopeList, extendInScopeIds,
        isInScope, setInScope, getTCvSubst, extendTvSubst, extendCvSubst,
        delBndr, delBndrs,

        -- ** Substituting and cloning binders
        substBndr, substBndrs, substRecBndrs, substTyVarBndr, substCoVarBndr,
        cloneBndr, cloneBndrs, cloneIdBndr, cloneIdBndrs, cloneRecIdBndrs,

    ) where

#include "HsVersions.h"


import GhcPrelude

import CoreSyn
import CoreFVs
import CoreSeq
import CoreUtils
import qualified Type
import qualified Coercion

        -- We are defining local versions
import Type     hiding ( substTy, extendTvSubst, extendCvSubst, extendTvSubstList
                       , isInScope, substTyVarBndr, cloneTyVarBndr )
import Coercion hiding ( substCo, substCoVarBndr )

import PrelNames
import VarSet
import VarEnv
import Id
import Name     ( Name )
import Var
import IdInfo
import UniqSupply
import Maybes
import Util
import Outputable
import Data.List



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
-- 1. Note [The substitution invariant] in TyCoSubst
--
-- 2. Note [Substitutions apply only once] in TyCoSubst
data Subst
  = Subst InScopeSet  -- Variables in in scope (both Ids and TyVars) /after/
                      -- applying the substitution
          IdSubstEnv  -- Substitution from NcIds to CoreExprs
          TvSubstEnv  -- Substitution from TyVars to Types
          CvSubstEnv  -- Substitution from CoVars to Coercions

        -- INVARIANT 1: See TyCoSubst Note [The substitution invariant]
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

For TyVars, see Note [Extending the TCvSubst] in TyCoSubst.

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

-- | Find the in-scope set: see TyCoSubst Note [The substitution invariant]
substInScope :: Subst -> InScopeSet
substInScope (Subst in_scope _ _ _) = in_scope

-- | Remove all substitutions for 'Id's and 'Var's that might have been built up
-- while preserving the in-scope set
zapSubstEnv :: Subst -> Subst
zapSubstEnv (Subst in_scope _ _ _) = Subst in_scope emptyVarEnv emptyVarEnv emptyVarEnv

-- | Add a substitution for an 'Id' to the 'Subst': you must ensure that the in-scope set is
-- such that TyCoSubst Note [The substitution invariant]
-- holds after extending the substitution like this
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
-- TyCoSubst Note [The substitution invariant] holds
-- after extending the substitution like this.
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

-- | Add a substitution from a 'CoVar' to a 'Coercion' to the 'Subst':
-- you must ensure that the in-scope set satisfies
-- TyCoSubst Note [The substitution invariant]
-- after extending the substitution like this
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
-- apply the substitution /once/:
-- See Note [Substitutions apply only once] in TyCoSubst
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
                     noFreeVarsOfType old_ty

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
substCo :: HasCallStack => Subst -> Coercion -> Coercion
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
  | (isEmptyVarEnv tv_env && isEmptyVarEnv cv_env) || noFreeVarsOfType old_ty = id
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
    nothing_to_do = isEmptyRuleInfo old_rules && not (isFragileUnfolding old_unf)

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
         , ru_args  = map (stripTicksE (const True) . substExpr doc subst') args
           -- See Note [Rule templates are devoid of ticks] in Rules.
         , ru_rhs   = substExpr (text "foo") subst' rhs }
           -- Do NOT optimise the RHS (previously we did simplOptExpr here)
           -- See Note [Substitute lazily]
  where
    doc = text "subst-rule" <+> ppr fn_name
    (subst', bndrs') = substBndrs subst bndrs

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

One case in point was #10627 in which a rule for a function 'f'
referred to 'f' (at a different type) on the RHS.  But instead of just
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
-}


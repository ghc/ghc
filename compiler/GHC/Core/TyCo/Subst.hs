{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1998
Type and Coercion - friends' interface
-}



-- | Substitution into types and coercions.
module GHC.Core.TyCo.Subst
  (
        -- * Substitutions
        Subst(..), TvSubstEnv, CvSubstEnv, IdSubstEnv,
        emptyIdSubstEnv, emptyTvSubstEnv, emptyCvSubstEnv, composeTCvSubst,
        emptySubst, mkEmptySubst, isEmptyTCvSubst, isEmptySubst,
        mkTCvSubst, mkTvSubst, mkCvSubst, mkIdSubst,
        getTvSubstEnv, getIdSubstEnv,
        getCvSubstEnv, getSubstInScope, setInScope, getSubstRangeTyCoFVs,
        isInScope, elemSubst, notElemSubst, zapSubst,
        extendSubstInScope, extendSubstInScopeList, extendSubstInScopeSet,
        extendTCvSubst, extendTCvSubstWithClone,
        extendCvSubst, extendCvSubstWithClone,
        extendTvSubst, extendTvSubstWithClone,
        extendTvSubstList, extendTvSubstAndInScope,
        extendCvSubstAndInScope,
        extendTCvSubstList,
        unionSubst, zipTyEnv, zipCoEnv,
        zipTvSubst, zipCvSubst,
        zipTCvSubst,
        mkTvSubstPrs,

        substTyWith, substTyWithCoVars, substTysWith, substTysWithCoVars,
        substCoWith,
        substTy, substTyAddInScope, substScaledTy,
        substTyUnchecked, substTysUnchecked, substScaledTysUnchecked, substThetaUnchecked,
        substTyWithUnchecked, substScaledTyUnchecked,
        substCoUnchecked, substCoWithUnchecked,
        substTyWithInScope,
        substTys, substScaledTys, substTheta,
        lookupTyVar,
        substCo, substCos, substCoVar, substCoVars, lookupCoVar,
        cloneTyVarBndr, cloneTyVarBndrs,
        substVarBndr, substVarBndrs,
        substTyVarBndr, substTyVarBndrs,
        substCoVarBndr, substDCoVarSet,
        substTyVar, substTyVars, substTyVarToTyVar,
        substTyCoVars,
        substTyCoBndr, substForAllCoBndr,
        substVarBndrUsing, substForAllCoBndrUsing,
        checkValidSubst, isValidTCvSubst,
  ) where

import GHC.Prelude

import {-# SOURCE #-} GHC.Core.Type
   ( mkCastTy, mkAppTy, isCoercionTy, mkTyConApp, getTyVar_maybe )
import {-# SOURCE #-} GHC.Core.Coercion
   ( mkCoVarCo, mkKindCo, mkSelCo, mkTransCo
   , mkNomReflCo, mkSubCo, mkSymCo
   , mkFunCo2, mkForAllCo, mkUnivCo
   , mkAxiomCo, mkAppCo, mkGReflCo
   , mkInstCo, mkLRCo, mkTyConAppCo
   , mkCoercionType
   , coercionKind, coercionLKind, coVarTypesRole )
import {-# SOURCE #-} GHC.Core.TyCo.Ppr ( pprTyVar )
import {-# SOURCE #-} GHC.Core.Ppr ( ) -- instance Outputable CoreExpr
import {-# SOURCE #-} GHC.Core ( CoreExpr )

import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.FVs

import GHC.Types.Basic( SwapFlag(..), isSwapped, pickSwap, notSwapped )
import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Types.Var.Env

import GHC.Data.Pair
import GHC.Utils.Constants (debugIsOn)
import GHC.Utils.Misc
import GHC.Types.Unique.Supply
import GHC.Types.Unique
import GHC.Types.Unique.FM
import GHC.Types.Unique.Set
import GHC.Utils.Outputable
import GHC.Utils.Panic

import Data.List (mapAccumL)

{-
%************************************************************************
%*                                                                      *
                        Substitutions
      Data type defined here to avoid unnecessary mutual recursion
%*                                                                      *
%************************************************************************
-}

-- | Type & coercion & id substitution
--
-- The "Subst" data type defined in this module contains substitution
-- for tyvar, covar and id. However, operations on IdSubstEnv (mapping
-- from "Id" to "CoreExpr") that require the definition of the "Expr"
-- data type are defined in GHC.Core.Subst to avoid circular module
-- dependency.
data Subst
  = Subst InScopeSet  -- Variables in scope (both Ids and TyVars) /after/
                      -- applying the substitution
          IdSubstEnv  -- Substitution from NcIds to CoreExprs
          TvSubstEnv  -- Substitution from TyVars to Types
          CvSubstEnv  -- Substitution from CoVars to Coercions

        -- INVARIANT 1: See Note [The substitution invariant]
        -- This is what lets us deal with name capture properly
        --
        -- INVARIANT 2: The substitution is apply-once;
        --              see Note [Substitutions apply only once]
        --
        -- INVARIANT 3: See Note [Extending the IdSubstEnv] in "GHC.Core.Subst"
        -- and Note [Extending the TvSubstEnv and CvSubstEnv]
        --
        -- INVARIANT 4: See Note [Substituting types, coercions, and expressions]

-- | A substitution of 'Expr's for non-coercion 'Id's
type IdSubstEnv = IdEnv CoreExpr   -- Domain is NonCoVarIds, i.e. not coercions

-- | A substitution of 'Type's for 'TyVar's
--                 and 'Kind's for 'KindVar's
type TvSubstEnv = TyVarEnv Type
  -- NB: A TvSubstEnv is used
  --   both inside a TCvSubst (with the apply-once invariant
  --        discussed in Note [Substitutions apply only once],
  --   and  also independently in the middle of matching,
  --        and unification (see Types.Unify).
  -- So you have to look at the context to know if it's idempotent or
  -- apply-once or whatever

-- | A substitution of 'Coercion's for 'CoVar's
type CvSubstEnv = CoVarEnv Coercion

{- Note [The substitution invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When calling (substTy subst ty) it should be the case that
the in-scope set in the substitution is a superset of both:

  (SIa) The free vars of the range of the substitution
  (SIb) The free vars of ty minus the domain of the substitution

* Reason for (SIa). Consider
      substTy [a :-> Maybe b] (forall b. b->a)
  we must rename the forall b, to get
      forall b2. b2 -> Maybe b
  Making 'b' part of the in-scope set forces this renaming to
  take place.

* Reason for (SIb). Consider
     substTy [a :-> Maybe b] (forall b. (a,b,x))
  Then if we use the in-scope set {b}, satisfying (SIa), there is
  a danger we will rename the forall'd variable to 'x' by mistake,
  getting this:
      forall x. (Maybe b, x, x)
  Breaking (SIb) caused the bug from #11371.

Note: if the free vars of the range of the substitution are freshly created,
then the problems of (SIa) can't happen, and so it would be sound to
ignore (SIa).

Note [Substitutions apply only once]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We use TCvSubsts to instantiate things, and we might instantiate
        forall a b. ty
with the types
        [a, b], or [b, a].
So the substitution might go [a->b, b->a].  A similar situation arises in Core
when we find a beta redex like
        (/\ a /\ b -> e) b a
Then we also end up with a substitution that permutes type variables. Other
variations happen to; for example [a -> (a, b)].

        ********************************************************
        *** So a substitution must be applied precisely once ***
        ********************************************************

A TCvSubst is not idempotent, but, unlike the non-idempotent substitution
we use during unifications, it must not be repeatedly applied.

Note [Extending the TvSubstEnv and CvSubstEnv]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See #tcvsubst_invariant# for the invariants that must hold.

This invariant allows a short-cut when the subst envs are empty:
if the TvSubstEnv and CvSubstEnv are empty --- i.e. (isEmptyTCvSubst subst)
holds --- then (substTy subst ty) does nothing.

For example, consider:
        (/\a. /\b:(a~Int). ...b..) Int
We substitute Int for 'a'.  The Unique of 'b' does not change, but
nevertheless we add 'b' to the TvSubstEnv, because b's kind does change

This invariant has several crucial consequences:

* In substVarBndr, we need extend the TvSubstEnv
        - if the unique has changed
        - or if the kind has changed

* In substTyVar, we do not need to consult the in-scope set;
  the TvSubstEnv is enough

* In substTy, substTheta, we can short-circuit when the TvSubstEnv is empty

Note [Substituting types, coercions, and expressions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Types and coercions are mutually recursive, and either may have variables
"belonging" to the other. Thus, every time we wish to substitute in a
type, we may also need to substitute in a coercion, and vice versa.
Likewise, expressions may contain type variables or coercion variables.
However, we use different constructors for constructing expression variables,
coercion variables, and type variables, so we carry three VarEnvs for each
variable type. Note that it would be possible to use the CoercionTy constructor
and the Type constructor to combine these environments, but that seems like a
false economy.

Note that the domain of the VarEnvs must be respected, despite the fact that
TyVar, Id, and CoVar are all type synonyms of the Var type. For example,
TvSubstEnv should *never* map a CoVar (built with the Id constructor)
and the CvSubstEnv should *never* map a TyVar. Furthermore, the range
of the TvSubstEnv should *never* include a type headed with
CoercionTy.
-}

emptyIdSubstEnv :: IdSubstEnv
emptyIdSubstEnv = emptyVarEnv

emptyTvSubstEnv :: TvSubstEnv
emptyTvSubstEnv = emptyVarEnv

emptyCvSubstEnv :: CvSubstEnv
emptyCvSubstEnv = emptyVarEnv

-- | Composes two substitutions, applying the second one provided first,
-- like in function composition. This function leaves IdSubstEnv untouched
-- because IdSubstEnv is not used during substitution for types.
composeTCvSubst :: Subst -> Subst -> Subst
composeTCvSubst subst1@(Subst is1 ids1 tenv1 cenv1) (Subst is2 _ tenv2 cenv2)
  = Subst is3 ids1 tenv3 cenv3
  where
    is3 = is1 `unionInScope` is2
    tenv3 = tenv1 `plusVarEnv` mapVarEnv (substTy extended_subst1) tenv2
    cenv3 = cenv1 `plusVarEnv` mapVarEnv (substCo extended_subst1) cenv2

    -- Make sure the in-scope set in the first substitution is wide enough to
    -- cover the free variables in the range of the second substitution before
    -- applying it (#22235).
    extended_subst1 = subst1 `setInScope` is3

emptySubst :: Subst
emptySubst = Subst emptyInScopeSet emptyVarEnv emptyVarEnv emptyVarEnv

mkEmptySubst :: InScopeSet -> Subst
mkEmptySubst in_scope = Subst in_scope emptyVarEnv emptyVarEnv emptyVarEnv

isEmptySubst :: Subst -> Bool
isEmptySubst (Subst _ id_env tv_env cv_env)
  = isEmptyVarEnv id_env && isEmptyVarEnv tv_env && isEmptyVarEnv cv_env

-- | Checks whether the tyvar and covar environments are empty.
-- This function should be used over 'isEmptySubst' when substituting
-- for types, because types currently do not contain expressions; we can
-- safely disregard the expression environment when deciding whether
-- to skip a substitution. Using 'isEmptyTCvSubst' gives us a non-trivial
-- performance boost (up to 70% less allocation for T18223)
isEmptyTCvSubst :: Subst -> Bool
isEmptyTCvSubst (Subst _ _ tv_env cv_env)
  = isEmptyVarEnv tv_env && isEmptyVarEnv cv_env

mkTCvSubst :: InScopeSet -> TvSubstEnv -> CvSubstEnv -> Subst
mkTCvSubst in_scope tvs cvs = Subst in_scope emptyIdSubstEnv tvs cvs

mkIdSubst :: InScopeSet -> IdSubstEnv -> Subst
mkIdSubst in_scope ids = Subst in_scope ids emptyTvSubstEnv emptyCvSubstEnv

mkTvSubst :: InScopeSet -> TvSubstEnv -> Subst
-- ^ Make a TCvSubst with specified tyvar subst and empty covar subst
mkTvSubst in_scope tenv = Subst in_scope emptyIdSubstEnv tenv emptyCvSubstEnv

mkCvSubst :: InScopeSet -> CvSubstEnv -> Subst
-- ^ Make a TCvSubst with specified covar subst and empty tyvar subst
mkCvSubst in_scope cenv = Subst in_scope emptyIdSubstEnv emptyTvSubstEnv cenv

getIdSubstEnv :: Subst -> IdSubstEnv
getIdSubstEnv (Subst _ ids _ _) = ids

getTvSubstEnv :: Subst -> TvSubstEnv
getTvSubstEnv (Subst _ _ tenv _) = tenv

getCvSubstEnv :: Subst -> CvSubstEnv
getCvSubstEnv (Subst _ _ _ cenv) = cenv

-- | Find the in-scope set: see Note [The substitution invariant]
getSubstInScope :: Subst -> InScopeSet
getSubstInScope (Subst in_scope _ _ _) = in_scope

setInScope :: Subst -> InScopeSet -> Subst
setInScope (Subst _ ids tvs cvs) in_scope = Subst in_scope ids tvs cvs

-- | Returns the free variables of the types in the range of a substitution as
-- a non-deterministic set.
getSubstRangeTyCoFVs :: Subst -> VarSet
getSubstRangeTyCoFVs (Subst _ _ tenv cenv)
  = tenvFVs `unionVarSet` cenvFVs
  where
    tenvFVs = shallowTyCoVarsOfTyVarEnv tenv
    cenvFVs = shallowTyCoVarsOfCoVarEnv cenv

isInScope :: Var -> Subst -> Bool
isInScope v (Subst in_scope _ _ _) = v `elemInScopeSet` in_scope

elemSubst :: Var -> Subst -> Bool
elemSubst v (Subst _ ids tenv cenv)
  | isTyVar v
  = v `elemVarEnv` tenv
  | isCoVar v
  = v `elemVarEnv` cenv
  | otherwise
  = v `elemVarEnv` ids

notElemSubst :: Var -> Subst -> Bool
notElemSubst v = not . elemSubst v

-- | Remove all substitutions that might have been built up
-- while preserving the in-scope set
-- originally called zapSubstEnv
zapSubst :: Subst -> Subst
zapSubst (Subst in_scope _ _ _) = Subst in_scope emptyVarEnv emptyVarEnv emptyVarEnv

-- | Add the 'Var' to the in-scope set
extendSubstInScope :: Subst -> Var -> Subst
extendSubstInScope (Subst in_scope ids tvs cvs) v
  = Subst (in_scope `extendInScopeSet` v)
          ids tvs cvs

-- | Add the 'Var's to the in-scope set: see also 'extendInScope'
extendSubstInScopeList :: Subst -> [Var] -> Subst
extendSubstInScopeList (Subst in_scope ids tvs cvs) vs
  = Subst (in_scope `extendInScopeSetList` vs)
          ids tvs cvs

-- | Add the 'Var's to the in-scope set: see also 'extendInScope'
extendSubstInScopeSet :: Subst -> VarSet -> Subst
extendSubstInScopeSet (Subst in_scope ids tvs cvs) vs
  = Subst (in_scope `extendInScopeSetSet` vs)
          ids tvs cvs

extendTCvSubst :: Subst -> TyCoVar -> Type -> Subst
extendTCvSubst subst v ty
  | isTyVar v
  = extendTvSubst subst v ty
  | CoercionTy co <- ty
  = extendCvSubst subst v co
  | otherwise
  = pprPanic "extendTCvSubst" (ppr v <+> text "|->" <+> ppr ty)

extendTCvSubstWithClone :: Subst -> TyCoVar -> TyCoVar -> Subst
extendTCvSubstWithClone subst tcv
  | isTyVar tcv = extendTvSubstWithClone subst tcv
  | otherwise   = extendCvSubstWithClone subst tcv

-- | Add a substitution for a 'TyVar' to the 'Subst'
-- The 'TyVar' *must* be a real TyVar, and not a CoVar
-- You must ensure that the in-scope set is such that
-- Note [The substitution invariant] holds
-- after extending the substitution like this.
extendTvSubst :: Subst -> TyVar -> Type -> Subst
extendTvSubst (Subst in_scope ids tvs cvs) tv ty
  = assert (isTyVar tv) $
    Subst in_scope ids (extendVarEnv tvs tv ty) cvs

extendTvSubstWithClone :: Subst -> TyVar -> TyVar -> Subst
-- Adds a new tv -> tv mapping, /and/ extends the in-scope set with the clone
-- Does not look in the kind of the new variable;
--   those variables should be in scope already
extendTvSubstWithClone (Subst in_scope idenv tenv cenv) tv tv'
  = Subst (extendInScopeSet in_scope tv')
          idenv
          (extendVarEnv tenv tv (mkTyVarTy tv'))
          cenv

-- | Add a substitution from a 'CoVar' to a 'Coercion' to the 'Subst':
-- you must ensure that the in-scope set satisfies
-- Note [The substitution invariant]
-- after extending the substitution like this
extendCvSubst :: Subst -> CoVar -> Coercion -> Subst
extendCvSubst (Subst in_scope ids tvs cvs) v r
  = assert (isCoVar v) $
    Subst in_scope ids tvs (extendVarEnv cvs v r)

extendCvSubstWithClone :: Subst -> CoVar -> CoVar -> Subst
extendCvSubstWithClone (Subst in_scope ids tenv cenv) cv cv'
  = Subst (extendInScopeSetSet in_scope new_in_scope)
             ids
             tenv
             (extendVarEnv cenv cv (mkCoVarCo cv'))
  where
    new_in_scope = tyCoVarsOfType (varType cv') `extendVarSet` cv'

extendTvSubstAndInScope :: Subst -> TyVar -> Type -> Subst
-- Also extends the in-scope set
extendTvSubstAndInScope (Subst in_scope ids tenv cenv) tv ty
  = Subst (in_scope `extendInScopeSetSet` tyCoVarsOfType ty)
             ids
             (extendVarEnv tenv tv ty)
             cenv

extendCvSubstAndInScope :: Subst -> CoVar -> Coercion -> Subst
-- Also extends the in-scope set
extendCvSubstAndInScope (Subst in_scope ids tenv cenv) cv co
  = Subst (in_scope `extendInScopeSetSet` tyCoVarsOfCo co)
             ids
             tenv
             (extendVarEnv cenv cv co)

-- | Adds multiple 'TyVar' substitutions to the 'Subst': see also 'extendTvSubst'
extendTvSubstList :: Subst -> [(TyVar,Type)] -> Subst
extendTvSubstList subst vrs
  = foldl' extend subst vrs
  where
    extend subst (v, r) = extendTvSubst subst v r

extendTCvSubstList :: Subst -> [Var] -> [Type] -> Subst
extendTCvSubstList subst tvs tys
  = foldl2 extendTCvSubst subst tvs tys

unionSubst :: Subst -> Subst -> Subst
-- Works when the ranges are disjoint
unionSubst (Subst in_scope1 ids1 tenv1 cenv1) (Subst in_scope2 ids2 tenv2 cenv2)
  = assert (ids1  `disjointVarEnv` ids2
         && tenv1 `disjointVarEnv` tenv2
         && cenv1 `disjointVarEnv` cenv2 )
    Subst (in_scope1 `unionInScope` in_scope2)
           (ids1      `plusVarEnv`   ids2)
           (tenv1     `plusVarEnv`   tenv2)
           (cenv1     `plusVarEnv`   cenv2)

-- | Generates the in-scope set for the 'Subst' from the types in the incoming
-- environment. No CoVars or Ids, please!
zipTvSubst :: HasDebugCallStack => [TyVar] -> [Type] -> Subst
zipTvSubst tvs tys
  = mkTvSubst (mkInScopeSet (shallowTyCoVarsOfTypes tys)) tenv
  where
    tenv = zipTyEnv tvs tys

-- | Generates the in-scope set for the 'Subst' from the types in the incoming
-- environment.  No TyVars, please!
zipCvSubst :: HasDebugCallStack => [CoVar] -> [Coercion] -> Subst
zipCvSubst cvs cos
  = mkCvSubst (mkInScopeSet (shallowTyCoVarsOfCos cos)) cenv
  where
    cenv = zipCoEnv cvs cos


zipTCvSubst :: HasDebugCallStack => [TyCoVar] -> [Type] -> Subst
zipTCvSubst tcvs tys
  = zip_tcvsubst tcvs tys $
    mkEmptySubst $ mkInScopeSet $ shallowTyCoVarsOfTypes tys
  where zip_tcvsubst :: [TyCoVar] -> [Type] -> Subst -> Subst
        zip_tcvsubst (tv:tvs) (ty:tys) subst
          = zip_tcvsubst tvs tys (extendTCvSubst subst tv ty)
        zip_tcvsubst [] [] subst = subst -- empty case
        zip_tcvsubst _  _  _     = pprPanic "zipTCvSubst: length mismatch"
                                   (ppr tcvs <+> ppr tys)

-- | Generates the in-scope set for the 'TCvSubst' from the types in the
-- incoming environment. No CoVars, please! The InScopeSet is just a thunk
--  so with a bit of luck it'll never be evaluated
mkTvSubstPrs :: [(TyVar, Type)] -> Subst
mkTvSubstPrs []  = emptySubst
mkTvSubstPrs prs =
    assertPpr onlyTyVarsAndNoCoercionTy (text "prs" <+> ppr prs) $
    mkTvSubst in_scope tenv
  where tenv = mkVarEnv prs
        in_scope = mkInScopeSet $ shallowTyCoVarsOfTypes $ map snd prs
        onlyTyVarsAndNoCoercionTy =
          and [ isTyVar tv && not (isCoercionTy ty)
              | (tv, ty) <- prs ]

-- | The InScopeSet is just a thunk so with a bit of luck it'll never be evaluated
zipTyEnv :: HasDebugCallStack => [TyVar] -> [Type] -> TvSubstEnv
zipTyEnv tyvars tys
  | debugIsOn
  , not (all isTyVar tyvars && (tyvars `equalLength` tys))
  = pprPanic "zipTyEnv" (ppr tyvars $$ ppr tys)
  | otherwise
  = assert (all (not . isCoercionTy) tys )
    zipToUFM tyvars tys
        -- There used to be a special case for when
        --      ty == TyVarTy tv
        -- (a not-uncommon case) in which case the substitution was dropped.
        -- But the type-tidier changes the print-name of a type variable without
        -- changing the unique, and that led to a bug.   Why?  Pre-tidying, we had
        -- a type {Foo t}, where Foo is a one-method class.  So Foo is really a newtype.
        -- And it happened that t was the type variable of the class.  Post-tiding,
        -- it got turned into {Foo t2}.  The ext-core printer expanded this using
        -- sourceTypeRep, but that said "Oh, t == t2" because they have the same unique,
        -- and so generated a rep type mentioning t not t2.
        --
        -- Simplest fix is to nuke the "optimisation"

zipCoEnv :: HasDebugCallStack => [CoVar] -> [Coercion] -> CvSubstEnv
zipCoEnv cvs cos
  | debugIsOn
  , not (all isCoVar cvs)
  = pprPanic "zipCoEnv" (ppr cvs <+> ppr cos)
  | otherwise
  = mkVarEnv (zipEqual "zipCoEnv" cvs cos)

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
%************************************************************************
%*                                                                      *
                Performing type or kind substitutions
%*                                                                      *
%************************************************************************

Note [Sym and ForAllCo]
~~~~~~~~~~~~~~~~~~~~~~~
In OptCoercion, we try to push "sym" out to the leaves of a coercion. But,
how do we push sym into a ForAllCo? It's a little ugly.

Ignoring visibility, here is the typing rule
(see Note [ForAllCo] in GHC.Core.TyCo.Rep).

h : k1 ~# k2
(tv : k1) |- g : ty1 ~# ty2
----------------------------
ForAllCo tv h g : (ForAllTy (tv : k1) ty1) ~#
                  (ForAllTy (tv : k2) (ty2[tv |-> tv |> sym h]))

Here is what we want:

ForAllCo tv h' g' : (ForAllTy (tv : k2) (ty2[tv |-> tv |> sym h])) ~#
                    (ForAllTy (tv : k1) ty1)


Because the kinds of the type variables to the right of the colon are the kinds
coerced by h', we know (h' : k2 ~# k1). Thus, (h' = sym h).

Now, we can rewrite ty1 to be (ty1[tv |-> tv |> sym h' |> h']). We thus want

ForAllCo tv h' g' :
  (ForAllTy (tv : k2) (ty2[tv |-> tv |> h'])) ~#
  (ForAllTy (tv : k1) (ty1[tv |-> tv |> h'][tv |-> tv |> sym h']))

We thus see that we want

g' : ty2[tv |-> tv |> h'] ~# ty1[tv |-> tv |> h']

and thus g' = sym (g[tv |-> tv |> h']).

Putting it all together, we get this:

sym (ForAllCo tv h g)
==>
ForAllCo tv (sym h) (sym g[tv |-> tv |> sym h])

Note [Substituting in a coercion hole]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It seems highly suspicious to be substituting in a coercion that still
has coercion holes. Yet, this can happen in a situation like this:

  f :: forall k. k :~: Type -> ()
  f Refl = let x :: forall (a :: k). [a] -> ...
               x = ...

When we check x's type signature, we require that k ~ Type. We indeed
know this due to the Refl pattern match, but the eager unifier can't
make use of givens. So, when we're done looking at x's type, a coercion
hole will remain. Then, when we're checking x's definition, we skolemise
x's type (in order to, e.g., bring the scoped type variable `a` into scope).
This requires performing a substitution for the fresh skolem variables.

This substitution needs to affect the kind of the coercion hole, too --
otherwise, the kind will have an out-of-scope variable in it. More problematically
in practice (we won't actually notice the out-of-scope variable ever), skolems
in the kind might have too high a level, triggering a failure to uphold the
invariant that no free variables in a type have a higher level than the
ambient level in the type checker. In the event of having free variables in the
hole's kind, I'm pretty sure we'll always have an erroneous program, so we
don't need to worry what will happen when the hole gets filled in. After all,
a hole relating a locally-bound type variable will be unable to be solved. This
is why it's OK not to look through the IORef of a coercion hole during
substitution.

-}

-- | Type substitution, see 'zipTvSubst'
substTyWith :: HasDebugCallStack => [TyVar] -> [Type] -> Type -> Type
-- Works only if the domain of the substitution is a
-- superset of the type being substituted into
substTyWith tvs tys = {-#SCC "substTyWith" #-}
                      assert (tvs `equalLength` tys )
                      substTy (zipTvSubst tvs tys)

-- | Type substitution, see 'zipTvSubst'. Disables sanity checks.
-- The problems that the sanity checks in substTy catch are described in
-- Note [The substitution invariant].
-- The goal of #11371 is to migrate all the calls of substTyUnchecked to
-- substTy and remove this function. Please don't use in new code.
substTyWithUnchecked :: [TyVar] -> [Type] -> Type -> Type
substTyWithUnchecked tvs tys
  = assert (tvs `equalLength` tys )
    substTyUnchecked (zipTvSubst tvs tys)

-- | Substitute tyvars within a type using a known 'InScopeSet'.
-- Pre-condition: the 'in_scope' set should satisfy Note [The substitution
-- invariant]; specifically it should include the free vars of 'tys',
-- and of 'ty' minus the domain of the subst.
substTyWithInScope :: HasDebugCallStack => InScopeSet -> [TyVar] -> [Type] -> Type -> Type
substTyWithInScope in_scope tvs tys ty =
  assert (tvs `equalLength` tys )
  substTy (mkTvSubst in_scope tenv) ty
  where tenv = zipTyEnv tvs tys

-- | Coercion substitution, see 'zipTvSubst'
substCoWith :: HasDebugCallStack => [TyVar] -> [Type] -> Coercion -> Coercion
substCoWith tvs tys = assert (tvs `equalLength` tys )
                      substCo (zipTvSubst tvs tys)

-- | Coercion substitution, see 'zipTvSubst'. Disables sanity checks.
-- The problems that the sanity checks in substCo catch are described in
-- Note [The substitution invariant].
-- The goal of #11371 is to migrate all the calls of substCoUnchecked to
-- substCo and remove this function. Please don't use in new code.
substCoWithUnchecked :: [TyVar] -> [Type] -> Coercion -> Coercion
substCoWithUnchecked tvs tys
  = assert (tvs `equalLength` tys )
    substCoUnchecked (zipTvSubst tvs tys)



-- | Substitute covars within a type
substTyWithCoVars :: [CoVar] -> [Coercion] -> Type -> Type
substTyWithCoVars cvs cos = substTy (zipCvSubst cvs cos)

-- | Type substitution, see 'zipTvSubst'
substTysWith :: HasDebugCallStack => [TyVar] -> [Type] -> [Type] -> [Type]
substTysWith tvs tys = assert (tvs `equalLength` tys )
                       substTys (zipTvSubst tvs tys)

-- | Type substitution, see 'zipTvSubst'
substTysWithCoVars :: HasDebugCallStack => [CoVar] -> [Coercion] -> [Type] -> [Type]
substTysWithCoVars cvs cos = assert (cvs `equalLength` cos )
                             substTys (zipCvSubst cvs cos)

-- | Substitute within a 'Type' after adding the free variables of the type
-- to the in-scope set. This is useful for the case when the free variables
-- aren't already in the in-scope set or easily available.
-- See also Note [The substitution invariant].
substTyAddInScope :: HasDebugCallStack => Subst -> Type -> Type
substTyAddInScope subst ty =
  substTy (extendSubstInScopeSet subst $ tyCoVarsOfType ty) ty

-- | When calling `substTy` it should be the case that the in-scope set in
-- the substitution is a superset of the free vars of the range of the
-- substitution.
-- See also Note [The substitution invariant].
-- TODO: take into account ids and rename as isValidSubst
isValidTCvSubst :: Subst -> Bool
isValidTCvSubst (Subst in_scope _ tenv cenv) =
  (tenvFVs `varSetInScope` in_scope) &&
  (cenvFVs `varSetInScope` in_scope)
  where
  tenvFVs = shallowTyCoVarsOfTyVarEnv tenv
  cenvFVs = shallowTyCoVarsOfCoVarEnv cenv

-- | This checks if the substitution satisfies the invariant from
-- Note [The substitution invariant].
checkValidSubst :: HasDebugCallStack => Subst -> [Type] -> [Coercion] -> a -> a
checkValidSubst subst@(Subst in_scope _ tenv cenv) tys cos a
  = assertPpr (isValidTCvSubst subst)
              (text "in_scope" <+> ppr in_scope $$
               text "tenv" <+> ppr tenv $$
               text "tenvFVs" <+> ppr (shallowTyCoVarsOfTyVarEnv tenv) $$
               text "cenv" <+> ppr cenv $$
               text "cenvFVs" <+> ppr (shallowTyCoVarsOfCoVarEnv cenv) $$
               text "tys" <+> ppr tys $$
               text "cos" <+> ppr cos) $
    assertPpr tysCosFVsInScope
              (text "in_scope" <+> ppr in_scope $$
               text "tenv" <+> ppr tenv $$
               text "cenv" <+> ppr cenv $$
               text "tys" <+> ppr tys $$
               text "cos" <+> ppr cos $$
               text "needInScope" <+> ppr needInScope)
    a
  where
  substDomain = nonDetKeysUFM tenv ++ nonDetKeysUFM cenv
    -- It's OK to use nonDetKeysUFM here, because we only use this list to
    -- remove some elements from a set
  needInScope = (shallowTyCoVarsOfTypes tys `unionVarSet`
                 shallowTyCoVarsOfCos cos)
                `delListFromUniqSet_Directly` substDomain
  tysCosFVsInScope = needInScope `varSetInScope` in_scope


-- | Substitute within a 'Type'
-- The substitution has to satisfy the invariants described in
-- Note [The substitution invariant].
substTy :: HasDebugCallStack => Subst -> Type  -> Type
substTy subst ty
  | isEmptyTCvSubst subst = ty
  | otherwise             = checkValidSubst subst [ty] [] $
                            subst_ty subst ty

-- | Substitute within a 'Type' disabling the sanity checks.
-- The problems that the sanity checks in substTy catch are described in
-- Note [The substitution invariant].
-- The goal of #11371 is to migrate all the calls of substTyUnchecked to
-- substTy and remove this function. Please don't use in new code.
substTyUnchecked :: Subst -> Type -> Type
substTyUnchecked subst ty
  | isEmptyTCvSubst subst = ty
  | otherwise             = subst_ty subst ty

substScaledTy :: HasDebugCallStack => Subst -> Scaled Type -> Scaled Type
substScaledTy subst scaled_ty = mapScaledType (substTy subst) scaled_ty

substScaledTyUnchecked :: HasDebugCallStack => Subst -> Scaled Type -> Scaled Type
substScaledTyUnchecked subst scaled_ty = mapScaledType (substTyUnchecked subst) scaled_ty

-- | Substitute within several 'Type's
-- The substitution has to satisfy the invariants described in
-- Note [The substitution invariant].
substTys :: HasDebugCallStack => Subst -> [Type] -> [Type]
substTys subst tys
  | isEmptyTCvSubst subst = tys
  | otherwise = checkValidSubst subst tys [] $ map (subst_ty subst) tys

substScaledTys :: HasDebugCallStack => Subst -> [Scaled Type] -> [Scaled Type]
substScaledTys subst scaled_tys
  | isEmptyTCvSubst subst = scaled_tys
  | otherwise = checkValidSubst subst (map scaledMult scaled_tys ++ map scaledThing scaled_tys) [] $
                map (mapScaledType (subst_ty subst)) scaled_tys

-- | Substitute within several 'Type's disabling the sanity checks.
-- The problems that the sanity checks in substTys catch are described in
-- Note [The substitution invariant].
-- The goal of #11371 is to migrate all the calls of substTysUnchecked to
-- substTys and remove this function. Please don't use in new code.
substTysUnchecked :: Subst -> [Type] -> [Type]
substTysUnchecked subst tys
                 | isEmptyTCvSubst subst = tys
                 | otherwise             = map (subst_ty subst) tys

substScaledTysUnchecked :: Subst -> [Scaled Type] -> [Scaled Type]
substScaledTysUnchecked subst tys
                 | isEmptyTCvSubst subst = tys
                 | otherwise             = map (mapScaledType (subst_ty subst)) tys

-- | Substitute within a 'ThetaType'
-- The substitution has to satisfy the invariants described in
-- Note [The substitution invariant].
substTheta :: HasDebugCallStack => Subst -> ThetaType -> ThetaType
substTheta = substTys

-- | Substitute within a 'ThetaType' disabling the sanity checks.
-- The problems that the sanity checks in substTys catch are described in
-- Note [The substitution invariant].
-- The goal of #11371 is to migrate all the calls of substThetaUnchecked to
-- substTheta and remove this function. Please don't use in new code.
substThetaUnchecked :: Subst -> ThetaType -> ThetaType
substThetaUnchecked = substTysUnchecked


subst_ty :: Subst -> Type -> Type
-- subst_ty is the main workhorse for type substitution
--
-- Note that the in_scope set is poked only if we hit a forall
-- so it may often never be fully computed
subst_ty subst ty
   = go ty
  where
    go (TyVarTy tv)      = substTyVar subst tv
    go (AppTy fun arg)   = (mkAppTy $! (go fun)) $! (go arg)
                -- The mkAppTy smart constructor is important
                -- we might be replacing (a Int), represented with App
                -- by [Int], represented with TyConApp
    go ty@(TyConApp tc []) = tc `seq` ty  -- avoid allocation in this common case
    go (TyConApp tc tys) = (mkTyConApp $! tc) $! strictMap go tys
                               -- NB: mkTyConApp, not TyConApp.
                               -- mkTyConApp has optimizations.
                               -- See Note [Using synonyms to compress types]
                               -- in GHC.Core.Type
    go ty@(FunTy { ft_mult = mult, ft_arg = arg, ft_res = res })
      = let !mult' = go mult
            !arg' = go arg
            !res' = go res
        in ty { ft_mult = mult', ft_arg = arg', ft_res = res' }
    go (ForAllTy (Bndr tv vis) ty)
                         = case substVarBndrUnchecked subst tv of
                             (subst', tv') ->
                               (ForAllTy $! ((Bndr $! tv') vis)) $!
                                            (subst_ty subst' ty)
    go (LitTy n)         = LitTy $! n
    go (CastTy ty co)    = (mkCastTy $! (go ty)) $! (subst_co subst co)
    go (CoercionTy co)   = CoercionTy $! (subst_co subst co)

substTyVar :: Subst -> TyVar -> Type
substTyVar (Subst _ _ tenv _) tv
  = assert (isTyVar tv) $
    case lookupVarEnv tenv tv of
      Just ty -> ty
      Nothing -> TyVarTy tv

substTyVarToTyVar :: HasDebugCallStack => Subst -> TyVar -> TyVar
-- Apply the substitution, expecting the result to be a TyVarTy
substTyVarToTyVar (Subst _ _ tenv _) tv
  = assert (isTyVar tv) $
    case lookupVarEnv tenv tv of
      Just ty -> case getTyVar_maybe ty of
                    Just tv -> tv
                    Nothing -> pprPanic "substTyVarToTyVar" (ppr tv $$ ppr ty)
      Nothing -> tv

substTyVars :: Subst -> [TyVar] -> [Type]
substTyVars subst = map $ substTyVar subst

substTyCoVars :: Subst -> [TyCoVar] -> [Type]
substTyCoVars subst = map $ substTyCoVar subst

substTyCoVar :: Subst -> TyCoVar -> Type
substTyCoVar subst tv
  | isTyVar tv = substTyVar subst tv
  | otherwise = CoercionTy $ substCoVar subst tv

lookupTyVar :: Subst -> TyVar  -> Maybe Type
        -- See Note [Extending the TvSubstEnv and CvSubstEnv]
lookupTyVar (Subst _ _ tenv _) tv
  = assert (isTyVar tv )
    lookupVarEnv tenv tv

-- | Substitute within a 'Coercion'
-- The substitution has to satisfy the invariants described in
-- Note [The substitution invariant].
substCo :: HasDebugCallStack => Subst -> Coercion -> Coercion
substCo subst co
  | isEmptyTCvSubst subst = co
  | otherwise = checkValidSubst subst [] [co] $ subst_co subst co

-- | Substitute within a 'Coercion' disabling sanity checks.
-- The problems that the sanity checks in substCo catch are described in
-- Note [The substitution invariant].
-- The goal of #11371 is to migrate all the calls of substCoUnchecked to
-- substCo and remove this function. Please don't use in new code.
substCoUnchecked :: Subst -> Coercion -> Coercion
substCoUnchecked subst co
  | isEmptyTCvSubst subst = co
  | otherwise = subst_co subst co

-- | Substitute within several 'Coercion's
-- The substitution has to satisfy the invariants described in
-- Note [The substitution invariant].
substCos :: HasDebugCallStack => Subst -> [Coercion] -> [Coercion]
substCos subst cos
  | isEmptyTCvSubst subst = cos
  | otherwise = checkValidSubst subst [] cos $ map (subst_co subst) cos

subst_co :: Subst -> Coercion -> Coercion
subst_co subst co
  = go co
  where
    go_ty :: Type -> Type
    go_ty = subst_ty subst

    go_mco :: MCoercion -> MCoercion
    go_mco MRefl    = MRefl
    go_mco (MCo co) = MCo (go co)

    go :: Coercion -> Coercion
    go (Refl ty)             = mkNomReflCo $! (go_ty ty)
    go (GRefl r ty mco)      = (mkGReflCo r $! (go_ty ty)) $! (go_mco mco)
    go (TyConAppCo r tc args)= mkTyConAppCo r tc $! go_cos args
    go (AxiomCo con cos)     = mkAxiomCo con $! go_cos cos
    go (AppCo co arg)        = (mkAppCo $! go co) $! go arg
    go (ForAllCo tv visL visR kind_co co)
      = case substForAllCoBndrUnchecked subst tv kind_co of
         (subst', tv', kind_co') ->
          ((mkForAllCo $! tv') visL visR $! kind_co') $! subst_co subst' co
    go (FunCo r afl afr w co1 co2)   = ((mkFunCo2 r afl afr $! go w) $! go co1) $! go co2
    go (CoVarCo cv)          = substCoVar subst cv
    go (UnivCo { uco_prov = p, uco_role = r
               , uco_lty = t1, uco_rty = t2, uco_deps = deps })
                             = ((((mkUnivCo $! p) $! go_cos deps) $! r) $!
                                  (go_ty t1)) $! (go_ty t2)
    go (SymCo co)            = mkSymCo $! (go co)
    go (TransCo co1 co2)     = (mkTransCo $! (go co1)) $! (go co2)
    go (SelCo d co)          = mkSelCo d $! (go co)
    go (LRCo lr co)          = mkLRCo lr $! (go co)
    go (InstCo co arg)       = (mkInstCo $! (go co)) $! go arg
    go (KindCo co)           = mkKindCo $! (go co)
    go (SubCo co)            = mkSubCo $! (go co)
    go (HoleCo h)            = HoleCo $! go_hole h

    go_cos cos = let cos' = map go cos
                 in cos' `seqList` cos'

    -- See Note [Substituting in a coercion hole]
    go_hole h@(CoercionHole { ch_co_var = cv })
      = h { ch_co_var = updateVarType go_ty cv }

-- | Perform a substitution within a 'DVarSet' of free variables,
-- returning the shallow free coercion variables.
substDCoVarSet :: Subst -> DCoVarSet -> DCoVarSet
substDCoVarSet subst cvs = coVarsOfCosDSet $ map (substCoVar subst) $
                           dVarSetElems cvs

substForAllCoBndr :: Subst -> TyCoVar -> KindCoercion
                  -> (Subst, TyCoVar, Coercion)
substForAllCoBndr subst
  = substForAllCoBndrUsing NotSwapped (substCo subst) subst

-- | Like 'substForAllCoBndr', but disables sanity checks.
-- The problems that the sanity checks in substCo catch are described in
-- Note [The substitution invariant].
-- The goal of #11371 is to migrate all the calls of substCoUnchecked to
-- substCo and remove this function. Please don't use in new code.
substForAllCoBndrUnchecked :: Subst -> TyCoVar -> KindCoercion
                           -> (Subst, TyCoVar, Coercion)
substForAllCoBndrUnchecked subst
  = substForAllCoBndrUsing NotSwapped (substCoUnchecked subst) subst

-- See Note [Sym and ForAllCo]
substForAllCoBndrUsing :: SwapFlag  -- Apply sym to binder?
                       -> (Coercion -> Coercion)  -- transformation to kind co
                       -> Subst -> TyCoVar -> KindCoercion
                       -> (Subst, TyCoVar, KindCoercion)
substForAllCoBndrUsing sym sco subst old_var
  | isTyVar old_var = substForAllCoTyVarBndrUsing sym sco subst old_var
  | otherwise       = substForAllCoCoVarBndrUsing sym sco subst old_var

substForAllCoTyVarBndrUsing :: SwapFlag  -- Apply sym to binder?
                            -> (Coercion -> Coercion)  -- transformation to kind co
                            -> Subst -> TyVar -> KindCoercion
                            -> (Subst, TyVar, KindCoercion)
substForAllCoTyVarBndrUsing sym sco (Subst in_scope idenv tenv cenv) old_var old_kind_co
  = assert (isTyVar old_var )
    ( Subst (in_scope `extendInScopeSet` new_var) idenv new_env cenv
    , new_var, new_kind_co )
  where
    new_env | no_change, notSwapped sym
            = delVarEnv tenv old_var
            | isSwapped sym
            = extendVarEnv tenv old_var $
              TyVarTy new_var `CastTy` new_kind_co
            | otherwise
            = extendVarEnv tenv old_var (TyVarTy new_var)

    no_kind_change = noFreeVarsOfCo old_kind_co
    no_change = no_kind_change && (new_var == old_var)

    new_kind_co | no_kind_change = old_kind_co
                | otherwise      = sco old_kind_co

    new_ki1 = coercionLKind new_kind_co
    -- We could do substitution to (tyVarKind old_var). We don't do so because
    -- we already substituted new_kind_co, which contains the kind information
    -- we want. We don't want to do substitution once more. Also, in most cases,
    -- new_kind_co is a Refl, in which case coercionKind is really fast.

    new_var  = uniqAway in_scope (setTyVarKind old_var new_ki1)

substForAllCoCoVarBndrUsing :: SwapFlag  -- Apply sym to binder?
                            -> (Coercion -> Coercion)  -- transformation to kind co
                            -> Subst -> CoVar -> KindCoercion
                            -> (Subst, CoVar, KindCoercion)
substForAllCoCoVarBndrUsing sym sco (Subst in_scope idenv tenv cenv)
                            old_var old_kind_co
  = assert (isCoVar old_var )
    ( Subst (in_scope `extendInScopeSet` new_var) idenv tenv new_cenv
    , new_var, new_kind_co )
  where
    new_cenv | no_change, notSwapped sym
             = delVarEnv cenv old_var
             | otherwise
             = extendVarEnv cenv old_var (mkCoVarCo new_var)

    no_kind_change = noFreeVarsOfCo old_kind_co
    no_change = no_kind_change && (new_var == old_var)

    new_kind_co | no_kind_change = old_kind_co
                | otherwise      = sco old_kind_co

    Pair h1 h2 = coercionKind new_kind_co

    new_var       = uniqAway in_scope $ mkCoVar (varName old_var) new_var_type
    new_var_type  = pickSwap sym h1 h2

substCoVar :: Subst -> CoVar -> Coercion
substCoVar (Subst _ _ _ cenv) cv
  = case lookupVarEnv cenv cv of
      Just co -> co
      Nothing -> CoVarCo cv

substCoVars :: Subst -> [CoVar] -> [Coercion]
substCoVars subst cvs = map (substCoVar subst) cvs

lookupCoVar :: Subst -> Var -> Maybe Coercion
lookupCoVar (Subst _ _ _ cenv) v = lookupVarEnv cenv v

substTyVarBndr :: HasDebugCallStack => Subst -> TyVar -> (Subst, TyVar)
substTyVarBndr = substTyVarBndrUsing substTy

substTyVarBndrs :: HasDebugCallStack => Subst -> [TyVar] -> (Subst, [TyVar])
substTyVarBndrs = mapAccumL substTyVarBndr

substVarBndr :: HasDebugCallStack => Subst -> TyCoVar -> (Subst, TyCoVar)
substVarBndr = substVarBndrUsing substTy

substVarBndrs :: HasDebugCallStack => Subst -> [TyCoVar] -> (Subst, [TyCoVar])
substVarBndrs = mapAccumL substVarBndr

substCoVarBndr :: HasDebugCallStack => Subst -> CoVar -> (Subst, CoVar)
substCoVarBndr = substCoVarBndrUsing substTy

-- | Like 'substVarBndr', but disables sanity checks.
-- The problems that the sanity checks in substTy catch are described in
-- Note [The substitution invariant].
-- The goal of #11371 is to migrate all the calls of substTyUnchecked to
-- substTy and remove this function. Please don't use in new code.
substVarBndrUnchecked :: Subst -> TyCoVar -> (Subst, TyCoVar)
substVarBndrUnchecked = substVarBndrUsing substTyUnchecked

substVarBndrUsing :: (Subst -> Type -> Type)
                  -> Subst -> TyCoVar -> (Subst, TyCoVar)
substVarBndrUsing subst_fn subst v
  | isTyVar v = substTyVarBndrUsing subst_fn subst v
  | otherwise = substCoVarBndrUsing subst_fn subst v

-- | Substitute a tyvar in a binding position, returning an
-- extended subst and a new tyvar.
-- Use the supplied function to substitute in the kind
substTyVarBndrUsing
  :: (Subst -> Type -> Type)  -- ^ Use this to substitute in the kind
  -> Subst -> TyVar -> (Subst, TyVar)
substTyVarBndrUsing subst_fn subst@(Subst in_scope idenv tenv cenv) old_var
  = assertPpr _no_capture (pprTyVar old_var $$ pprTyVar new_var $$ ppr subst) $
    assert (isTyVar old_var )
    (Subst (in_scope `extendInScopeSet` new_var) idenv new_env cenv, new_var)
  where
    new_env | no_change = delVarEnv tenv old_var
            | otherwise = extendVarEnv tenv old_var (TyVarTy new_var)

    _no_capture = not (new_var `elemVarSet` shallowTyCoVarsOfTyVarEnv tenv)
    -- Assertion check that we are not capturing something in the substitution

    old_ki = tyVarKind old_var
    no_kind_change = noFreeVarsOfType old_ki -- verify that kind is closed
    no_change = no_kind_change && (new_var == old_var)
        -- no_change means that the new_var is identical in
        -- all respects to the old_var (same unique, same kind)
        -- See Note [Extending the TvSubstEnv and CvSubstEnv]
        --
        -- In that case we don't need to extend the substitution
        -- to map old to new.  But instead we must zap any
        -- current substitution for the variable. For example:
        --      (\x.e) with id_subst = [x |-> e']
        -- Here we must simply zap the substitution for x

    new_var | no_kind_change = uniqAway in_scope old_var
            | otherwise = uniqAway in_scope $
                          setTyVarKind old_var (subst_fn subst old_ki)
        -- The uniqAway part makes sure the new variable is not already in scope

-- | Substitute a covar in a binding position, returning an
-- extended subst and a new covar.
-- Use the supplied function to substitute in the kind
substCoVarBndrUsing
  :: (Subst -> Type -> Type)
  -> Subst -> CoVar -> (Subst, CoVar)
substCoVarBndrUsing subst_fn subst@(Subst in_scope idenv tenv cenv) old_var
  = assert (isCoVar old_var)
    (Subst (in_scope `extendInScopeSet` new_var) idenv tenv new_cenv, new_var)
  where
    new_co         = mkCoVarCo new_var
    no_kind_change = noFreeVarsOfTypes [t1, t2]
    no_change      = new_var == old_var && no_kind_change

    new_cenv | no_change = delVarEnv cenv old_var
             | otherwise = extendVarEnv cenv old_var new_co

    new_var = uniqAway in_scope subst_old_var
    subst_old_var = mkCoVar (varName old_var) new_var_type

    (t1, t2, role) = coVarTypesRole old_var
    t1' = subst_fn subst t1
    t2' = subst_fn subst t2
    new_var_type = mkCoercionType role t1' t2'
                  -- It's important to do the substitution for coercions,
                  -- because they can have free type variables

cloneTyVarBndr :: Subst -> TyVar -> Unique -> (Subst, TyVar)
cloneTyVarBndr subst@(Subst in_scope id_env tv_env cv_env) tv uniq
  = assertPpr (isTyVar tv) (ppr tv)   -- I think it's only called on TyVars
    ( Subst (extendInScopeSet in_scope tv')
            id_env
            (extendVarEnv tv_env tv (mkTyVarTy tv'))
            cv_env
    , tv')
  where
    old_ki = tyVarKind tv
    no_kind_change = noFreeVarsOfType old_ki -- verify that kind is closed

    tv1 | no_kind_change = tv
        | otherwise      = setTyVarKind tv (substTy subst old_ki)

    tv' = setVarUnique tv1 uniq

cloneTyVarBndrs :: Subst -> [TyVar] -> UniqSupply -> (Subst, [TyVar])
cloneTyVarBndrs subst []     _usupply = (subst, [])
cloneTyVarBndrs subst (t:ts)  usupply = (subst'', tv:tvs)
  where
    (uniq, usupply') = takeUniqFromSupply usupply
    (subst' , tv )   = cloneTyVarBndr subst t uniq
    (subst'', tvs)   = cloneTyVarBndrs subst' ts usupply'

substTyCoBndr :: Subst -> PiTyBinder -> (Subst, PiTyBinder)
substTyCoBndr subst (Anon ty af)          = (subst, Anon (substScaledTy subst ty) af)
substTyCoBndr subst (Named (Bndr tv vis)) = (subst', Named (Bndr tv' vis))
                                          where
                                            (subst', tv') = substVarBndr subst tv

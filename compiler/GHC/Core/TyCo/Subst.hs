{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1998
Type and Coercion - friends' interface
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- | Substitution into types and coercions.
module GHC.Core.TyCo.Subst
  (
        -- * Substitutions
        TCvSubst(..), TvSubstEnv, CvSubstEnv,
        emptyTvSubstEnv, emptyCvSubstEnv, composeTCvSubstEnv, composeTCvSubst,
        emptyTCvSubst, mkEmptyTCvSubst, isEmptyTCvSubst,
        mkTCvSubst, mkTvSubst, mkCvSubst,
        getTvSubstEnv,
        getCvSubstEnv, getTCvInScope, getTCvSubstRangeFVs,
        isInScope, notElemTCvSubst,
        setTvSubstEnv, setCvSubstEnv, zapTCvSubst,
        extendTCvInScope, extendTCvInScopeList, extendTCvInScopeSet,
        extendTCvSubst, extendTCvSubstWithClone,
        extendCvSubst, extendCvSubstWithClone,
        extendTvSubst, extendTvSubstBinderAndInScope, extendTvSubstWithClone,
        extendTvSubstList, extendTvSubstAndInScope,
        extendTCvSubstList,
        unionTCvSubst, zipTyEnv, zipCoEnv,
        zipTvSubst, zipCvSubst,
        zipTCvSubst,
        mkTvSubstPrs,

        substTyWith, substTyWithCoVars, substTysWith, substTysWithCoVars,
        substCoWith,
        substTy, substTyAddInScope,
        substTyUnchecked, substTysUnchecked, substThetaUnchecked,
        substTyWithUnchecked,
        substCoUnchecked, substCoWithUnchecked,
        substTyWithInScope,
        substTys, substTheta,
        lookupTyVar,
        substCo, substCos, substCoVar, substCoVars, lookupCoVar,
        cloneTyVarBndr, cloneTyVarBndrs,
        substVarBndr, substVarBndrs,
        substTyVarBndr, substTyVarBndrs,
        substCoVarBndr,
        substTyVar, substTyVars, substTyCoVars,
        substForAllCoBndr,
        substVarBndrUsing, substForAllCoBndrUsing,
        checkValidSubst, isValidTCvSubst,
  ) where

#include "HsVersions.h"

import GhcPrelude

import {-# SOURCE #-} GHC.Core.Type
   ( mkCastTy, mkAppTy, isCoercionTy )
import {-# SOURCE #-} GHC.Core.Coercion
   ( mkCoVarCo, mkKindCo, mkNthCo, mkTransCo
   , mkNomReflCo, mkSubCo, mkSymCo
   , mkFunCo, mkForAllCo, mkUnivCo
   , mkAxiomInstCo, mkAppCo, mkGReflCo
   , mkInstCo, mkLRCo, mkTyConAppCo
   , mkCoercionType
   , coercionKind, coercionLKind, coVarKindsTypesRole )

import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.FVs
import GHC.Core.TyCo.Ppr

import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Types.Var.Env

import Pair
import Util
import GHC.Types.Unique.Supply
import GHC.Types.Unique
import GHC.Types.Unique.FM
import GHC.Types.Unique.Set
import Outputable

import Data.List (mapAccumL)

{-
%************************************************************************
%*                                                                      *
                        Substitutions
      Data type defined here to avoid unnecessary mutual recursion
%*                                                                      *
%************************************************************************
-}

-- | Type & coercion substitution
--
-- #tcvsubst_invariant#
-- The following invariants must hold of a 'TCvSubst':
--
-- 1. The in-scope set is needed /only/ to
-- guide the generation of fresh uniques
--
-- 2. In particular, the /kind/ of the type variables in
-- the in-scope set is not relevant
--
-- 3. The substitution is only applied ONCE! This is because
-- in general such application will not reach a fixed point.
data TCvSubst
  = TCvSubst InScopeSet -- The in-scope type and kind variables
             TvSubstEnv -- Substitutes both type and kind variables
             CvSubstEnv -- Substitutes coercion variables
        -- See Note [Substitutions apply only once]
        -- and Note [Extending the TvSubstEnv]
        -- and Note [Substituting types and coercions]
        -- and Note [The substitution invariant]

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

The same rules apply to other substitutions (notably GHC.Core.Subst.Subst)

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

Note [Extending the TvSubstEnv]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

Note [Substituting types and coercions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Types and coercions are mutually recursive, and either may have variables
"belonging" to the other. Thus, every time we wish to substitute in a
type, we may also need to substitute in a coercion, and vice versa.
However, the constructor used to create type variables is distinct from
that of coercion variables, so we carry two VarEnvs in a TCvSubst. Note
that it would be possible to use the CoercionTy constructor to combine
these environments, but that seems like a false economy.

Note that the TvSubstEnv should *never* map a CoVar (built with the Id
constructor) and the CvSubstEnv should *never* map a TyVar. Furthermore,
the range of the TvSubstEnv should *never* include a type headed with
CoercionTy.
-}

emptyTvSubstEnv :: TvSubstEnv
emptyTvSubstEnv = emptyVarEnv

emptyCvSubstEnv :: CvSubstEnv
emptyCvSubstEnv = emptyVarEnv

composeTCvSubstEnv :: InScopeSet
                   -> (TvSubstEnv, CvSubstEnv)
                   -> (TvSubstEnv, CvSubstEnv)
                   -> (TvSubstEnv, CvSubstEnv)
-- ^ @(compose env1 env2)(x)@ is @env1(env2(x))@; i.e. apply @env2@ then @env1@.
-- It assumes that both are idempotent.
-- Typically, @env1@ is the refinement to a base substitution @env2@
composeTCvSubstEnv in_scope (tenv1, cenv1) (tenv2, cenv2)
  = ( tenv1 `plusVarEnv` mapVarEnv (substTy subst1) tenv2
    , cenv1 `plusVarEnv` mapVarEnv (substCo subst1) cenv2 )
        -- First apply env1 to the range of env2
        -- Then combine the two, making sure that env1 loses if
        -- both bind the same variable; that's why env1 is the
        --  *left* argument to plusVarEnv, because the right arg wins
  where
    subst1 = TCvSubst in_scope tenv1 cenv1

-- | Composes two substitutions, applying the second one provided first,
-- like in function composition.
composeTCvSubst :: TCvSubst -> TCvSubst -> TCvSubst
composeTCvSubst (TCvSubst is1 tenv1 cenv1) (TCvSubst is2 tenv2 cenv2)
  = TCvSubst is3 tenv3 cenv3
  where
    is3 = is1 `unionInScope` is2
    (tenv3, cenv3) = composeTCvSubstEnv is3 (tenv1, cenv1) (tenv2, cenv2)

emptyTCvSubst :: TCvSubst
emptyTCvSubst = TCvSubst emptyInScopeSet emptyTvSubstEnv emptyCvSubstEnv

mkEmptyTCvSubst :: InScopeSet -> TCvSubst
mkEmptyTCvSubst is = TCvSubst is emptyTvSubstEnv emptyCvSubstEnv

isEmptyTCvSubst :: TCvSubst -> Bool
         -- See Note [Extending the TvSubstEnv]
isEmptyTCvSubst (TCvSubst _ tenv cenv) = isEmptyVarEnv tenv && isEmptyVarEnv cenv

mkTCvSubst :: InScopeSet -> (TvSubstEnv, CvSubstEnv) -> TCvSubst
mkTCvSubst in_scope (tenv, cenv) = TCvSubst in_scope tenv cenv

mkTvSubst :: InScopeSet -> TvSubstEnv -> TCvSubst
-- ^ Make a TCvSubst with specified tyvar subst and empty covar subst
mkTvSubst in_scope tenv = TCvSubst in_scope tenv emptyCvSubstEnv

mkCvSubst :: InScopeSet -> CvSubstEnv -> TCvSubst
-- ^ Make a TCvSubst with specified covar subst and empty tyvar subst
mkCvSubst in_scope cenv = TCvSubst in_scope emptyTvSubstEnv cenv

getTvSubstEnv :: TCvSubst -> TvSubstEnv
getTvSubstEnv (TCvSubst _ env _) = env

getCvSubstEnv :: TCvSubst -> CvSubstEnv
getCvSubstEnv (TCvSubst _ _ env) = env

getTCvInScope :: TCvSubst -> InScopeSet
getTCvInScope (TCvSubst in_scope _ _) = in_scope

-- | Returns the free variables of the types in the range of a substitution as
-- a non-deterministic set.
getTCvSubstRangeFVs :: TCvSubst -> VarSet
getTCvSubstRangeFVs (TCvSubst _ tenv cenv)
    = unionVarSet tenvFVs cenvFVs
  where
    tenvFVs = shallowTyCoVarsOfTyVarEnv tenv
    cenvFVs = shallowTyCoVarsOfCoVarEnv cenv

isInScope :: Var -> TCvSubst -> Bool
isInScope v (TCvSubst in_scope _ _) = v `elemInScopeSet` in_scope

notElemTCvSubst :: Var -> TCvSubst -> Bool
notElemTCvSubst v (TCvSubst _ tenv cenv)
  | isTyVar v
  = not (v `elemVarEnv` tenv)
  | otherwise
  = not (v `elemVarEnv` cenv)

setTvSubstEnv :: TCvSubst -> TvSubstEnv -> TCvSubst
setTvSubstEnv (TCvSubst in_scope _ cenv) tenv = TCvSubst in_scope tenv cenv

setCvSubstEnv :: TCvSubst -> CvSubstEnv -> TCvSubst
setCvSubstEnv (TCvSubst in_scope tenv _) cenv = TCvSubst in_scope tenv cenv

zapTCvSubst :: TCvSubst -> TCvSubst
zapTCvSubst (TCvSubst in_scope _ _) = TCvSubst in_scope emptyVarEnv emptyVarEnv

extendTCvInScope :: TCvSubst -> Var -> TCvSubst
extendTCvInScope (TCvSubst in_scope tenv cenv) var
  = TCvSubst (extendInScopeSet in_scope var) tenv cenv

extendTCvInScopeList :: TCvSubst -> [Var] -> TCvSubst
extendTCvInScopeList (TCvSubst in_scope tenv cenv) vars
  = TCvSubst (extendInScopeSetList in_scope vars) tenv cenv

extendTCvInScopeSet :: TCvSubst -> VarSet -> TCvSubst
extendTCvInScopeSet (TCvSubst in_scope tenv cenv) vars
  = TCvSubst (extendInScopeSetSet in_scope vars) tenv cenv

extendTCvSubst :: TCvSubst -> TyCoVar -> Type -> TCvSubst
extendTCvSubst subst v ty
  | isTyVar v
  = extendTvSubst subst v ty
  | CoercionTy co <- ty
  = extendCvSubst subst v co
  | otherwise
  = pprPanic "extendTCvSubst" (ppr v <+> text "|->" <+> ppr ty)

extendTCvSubstWithClone :: TCvSubst -> TyCoVar -> TyCoVar -> TCvSubst
extendTCvSubstWithClone subst tcv
  | isTyVar tcv = extendTvSubstWithClone subst tcv
  | otherwise   = extendCvSubstWithClone subst tcv

extendTvSubst :: TCvSubst -> TyVar -> Type -> TCvSubst
extendTvSubst (TCvSubst in_scope tenv cenv) tv ty
  = TCvSubst in_scope (extendVarEnv tenv tv ty) cenv

extendTvSubstBinderAndInScope :: TCvSubst -> TyCoBinder -> Type -> TCvSubst
extendTvSubstBinderAndInScope subst (Named (Bndr v _)) ty
  = ASSERT( isTyVar v )
    extendTvSubstAndInScope subst v ty
extendTvSubstBinderAndInScope subst (Anon {}) _
  = subst

extendTvSubstWithClone :: TCvSubst -> TyVar -> TyVar -> TCvSubst
-- Adds a new tv -> tv mapping, /and/ extends the in-scope set
extendTvSubstWithClone (TCvSubst in_scope tenv cenv) tv tv'
  = TCvSubst (extendInScopeSetSet in_scope new_in_scope)
             (extendVarEnv tenv tv (mkTyVarTy tv'))
             cenv
  where
    new_in_scope = tyCoVarsOfType (tyVarKind tv') `extendVarSet` tv'

extendCvSubst :: TCvSubst -> CoVar -> Coercion -> TCvSubst
extendCvSubst (TCvSubst in_scope tenv cenv) v co
  = TCvSubst in_scope tenv (extendVarEnv cenv v co)

extendCvSubstWithClone :: TCvSubst -> CoVar -> CoVar -> TCvSubst
extendCvSubstWithClone (TCvSubst in_scope tenv cenv) cv cv'
  = TCvSubst (extendInScopeSetSet in_scope new_in_scope)
             tenv
             (extendVarEnv cenv cv (mkCoVarCo cv'))
  where
    new_in_scope = tyCoVarsOfType (varType cv') `extendVarSet` cv'

extendTvSubstAndInScope :: TCvSubst -> TyVar -> Type -> TCvSubst
-- Also extends the in-scope set
extendTvSubstAndInScope (TCvSubst in_scope tenv cenv) tv ty
  = TCvSubst (in_scope `extendInScopeSetSet` tyCoVarsOfType ty)
             (extendVarEnv tenv tv ty)
             cenv

extendTvSubstList :: TCvSubst -> [Var] -> [Type] -> TCvSubst
extendTvSubstList subst tvs tys
  = foldl2 extendTvSubst subst tvs tys

extendTCvSubstList :: TCvSubst -> [Var] -> [Type] -> TCvSubst
extendTCvSubstList subst tvs tys
  = foldl2 extendTCvSubst subst tvs tys

unionTCvSubst :: TCvSubst -> TCvSubst -> TCvSubst
-- Works when the ranges are disjoint
unionTCvSubst (TCvSubst in_scope1 tenv1 cenv1) (TCvSubst in_scope2 tenv2 cenv2)
  = ASSERT( tenv1 `disjointVarEnv` tenv2
         && cenv1 `disjointVarEnv` cenv2 )
    TCvSubst (in_scope1 `unionInScope` in_scope2)
             (tenv1     `plusVarEnv`   tenv2)
             (cenv1     `plusVarEnv`   cenv2)

-- mkTvSubstPrs and zipTvSubst generate the in-scope set from
-- the types given; but it's just a thunk so with a bit of luck
-- it'll never be evaluated

-- | Generates the in-scope set for the 'TCvSubst' from the types in the incoming
-- environment. No CoVars, please!
zipTvSubst :: HasDebugCallStack => [TyVar] -> [Type] -> TCvSubst
zipTvSubst tvs tys
  = mkTvSubst (mkInScopeSet (shallowTyCoVarsOfTypes tys)) tenv
  where
    tenv = zipTyEnv tvs tys

-- | Generates the in-scope set for the 'TCvSubst' from the types in the incoming
-- environment.  No TyVars, please!
zipCvSubst :: HasDebugCallStack => [CoVar] -> [Coercion] -> TCvSubst
zipCvSubst cvs cos
  = TCvSubst (mkInScopeSet (shallowTyCoVarsOfCos cos)) emptyTvSubstEnv cenv
  where
    cenv = zipCoEnv cvs cos

zipTCvSubst :: HasDebugCallStack => [TyCoVar] -> [Type] -> TCvSubst
zipTCvSubst tcvs tys
  = zip_tcvsubst tcvs tys $
    mkEmptyTCvSubst $ mkInScopeSet $ shallowTyCoVarsOfTypes tys
  where zip_tcvsubst :: [TyCoVar] -> [Type] -> TCvSubst -> TCvSubst
        zip_tcvsubst (tv:tvs) (ty:tys) subst
          = zip_tcvsubst tvs tys (extendTCvSubst subst tv ty)
        zip_tcvsubst [] [] subst = subst -- empty case
        zip_tcvsubst _  _  _     = pprPanic "zipTCvSubst: length mismatch"
                                            (ppr tcvs <+> ppr tys)

-- | Generates the in-scope set for the 'TCvSubst' from the types in the
-- incoming environment. No CoVars, please!
mkTvSubstPrs :: [(TyVar, Type)] -> TCvSubst
mkTvSubstPrs prs =
    ASSERT2( onlyTyVarsAndNoCoercionTy, text "prs" <+> ppr prs )
    mkTvSubst in_scope tenv
  where tenv = mkVarEnv prs
        in_scope = mkInScopeSet $ shallowTyCoVarsOfTypes $ map snd prs
        onlyTyVarsAndNoCoercionTy =
          and [ isTyVar tv && not (isCoercionTy ty)
              | (tv, ty) <- prs ]

zipTyEnv :: HasDebugCallStack => [TyVar] -> [Type] -> TvSubstEnv
zipTyEnv tyvars tys
  | debugIsOn
  , not (all isTyVar tyvars)
  = pprPanic "zipTyEnv" (ppr tyvars <+> ppr tys)
  | otherwise
  = ASSERT( all (not . isCoercionTy) tys )
    mkVarEnv (zipEqual "zipTyEnv" tyvars tys)
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

instance Outputable TCvSubst where
  ppr (TCvSubst ins tenv cenv)
    = brackets $ sep[ text "TCvSubst",
                      nest 2 (text "In scope:" <+> ppr ins),
                      nest 2 (text "Type env:" <+> ppr tenv),
                      nest 2 (text "Co env:" <+> ppr cenv) ]

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

Here is the typing rule:

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
substTyWith :: HasCallStack => [TyVar] -> [Type] -> Type -> Type
-- Works only if the domain of the substitution is a
-- superset of the type being substituted into
substTyWith tvs tys = {-#SCC "substTyWith" #-}
                      ASSERT( tvs `equalLength` tys )
                      substTy (zipTvSubst tvs tys)

-- | Type substitution, see 'zipTvSubst'. Disables sanity checks.
-- The problems that the sanity checks in substTy catch are described in
-- Note [The substitution invariant].
-- The goal of #11371 is to migrate all the calls of substTyUnchecked to
-- substTy and remove this function. Please don't use in new code.
substTyWithUnchecked :: [TyVar] -> [Type] -> Type -> Type
substTyWithUnchecked tvs tys
  = ASSERT( tvs `equalLength` tys )
    substTyUnchecked (zipTvSubst tvs tys)

-- | Substitute tyvars within a type using a known 'InScopeSet'.
-- Pre-condition: the 'in_scope' set should satisfy Note [The substitution
-- invariant]; specifically it should include the free vars of 'tys',
-- and of 'ty' minus the domain of the subst.
substTyWithInScope :: InScopeSet -> [TyVar] -> [Type] -> Type -> Type
substTyWithInScope in_scope tvs tys ty =
  ASSERT( tvs `equalLength` tys )
  substTy (mkTvSubst in_scope tenv) ty
  where tenv = zipTyEnv tvs tys

-- | Coercion substitution, see 'zipTvSubst'
substCoWith :: HasCallStack => [TyVar] -> [Type] -> Coercion -> Coercion
substCoWith tvs tys = ASSERT( tvs `equalLength` tys )
                      substCo (zipTvSubst tvs tys)

-- | Coercion substitution, see 'zipTvSubst'. Disables sanity checks.
-- The problems that the sanity checks in substCo catch are described in
-- Note [The substitution invariant].
-- The goal of #11371 is to migrate all the calls of substCoUnchecked to
-- substCo and remove this function. Please don't use in new code.
substCoWithUnchecked :: [TyVar] -> [Type] -> Coercion -> Coercion
substCoWithUnchecked tvs tys
  = ASSERT( tvs `equalLength` tys )
    substCoUnchecked (zipTvSubst tvs tys)



-- | Substitute covars within a type
substTyWithCoVars :: [CoVar] -> [Coercion] -> Type -> Type
substTyWithCoVars cvs cos = substTy (zipCvSubst cvs cos)

-- | Type substitution, see 'zipTvSubst'
substTysWith :: [TyVar] -> [Type] -> [Type] -> [Type]
substTysWith tvs tys = ASSERT( tvs `equalLength` tys )
                       substTys (zipTvSubst tvs tys)

-- | Type substitution, see 'zipTvSubst'
substTysWithCoVars :: [CoVar] -> [Coercion] -> [Type] -> [Type]
substTysWithCoVars cvs cos = ASSERT( cvs `equalLength` cos )
                             substTys (zipCvSubst cvs cos)

-- | Substitute within a 'Type' after adding the free variables of the type
-- to the in-scope set. This is useful for the case when the free variables
-- aren't already in the in-scope set or easily available.
-- See also Note [The substitution invariant].
substTyAddInScope :: TCvSubst -> Type -> Type
substTyAddInScope subst ty =
  substTy (extendTCvInScopeSet subst $ tyCoVarsOfType ty) ty

-- | When calling `substTy` it should be the case that the in-scope set in
-- the substitution is a superset of the free vars of the range of the
-- substitution.
-- See also Note [The substitution invariant].
isValidTCvSubst :: TCvSubst -> Bool
isValidTCvSubst (TCvSubst in_scope tenv cenv) =
  (tenvFVs `varSetInScope` in_scope) &&
  (cenvFVs `varSetInScope` in_scope)
  where
  tenvFVs = shallowTyCoVarsOfTyVarEnv tenv
  cenvFVs = shallowTyCoVarsOfCoVarEnv cenv

-- | This checks if the substitution satisfies the invariant from
-- Note [The substitution invariant].
checkValidSubst :: HasCallStack => TCvSubst -> [Type] -> [Coercion] -> a -> a
checkValidSubst subst@(TCvSubst in_scope tenv cenv) tys cos a
  = ASSERT2( isValidTCvSubst subst,
             text "in_scope" <+> ppr in_scope $$
             text "tenv" <+> ppr tenv $$
             text "tenvFVs" <+> ppr (shallowTyCoVarsOfTyVarEnv tenv) $$
             text "cenv" <+> ppr cenv $$
             text "cenvFVs" <+> ppr (shallowTyCoVarsOfCoVarEnv cenv) $$
             text "tys" <+> ppr tys $$
             text "cos" <+> ppr cos )
    ASSERT2( tysCosFVsInScope,
             text "in_scope" <+> ppr in_scope $$
             text "tenv" <+> ppr tenv $$
             text "cenv" <+> ppr cenv $$
             text "tys" <+> ppr tys $$
             text "cos" <+> ppr cos $$
             text "needInScope" <+> ppr needInScope )
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
substTy :: HasCallStack => TCvSubst -> Type  -> Type
substTy subst ty
  | isEmptyTCvSubst subst = ty
  | otherwise             = checkValidSubst subst [ty] [] $
                            subst_ty subst ty

-- | Substitute within a 'Type' disabling the sanity checks.
-- The problems that the sanity checks in substTy catch are described in
-- Note [The substitution invariant].
-- The goal of #11371 is to migrate all the calls of substTyUnchecked to
-- substTy and remove this function. Please don't use in new code.
substTyUnchecked :: TCvSubst -> Type -> Type
substTyUnchecked subst ty
                 | isEmptyTCvSubst subst = ty
                 | otherwise             = subst_ty subst ty

-- | Substitute within several 'Type's
-- The substitution has to satisfy the invariants described in
-- Note [The substitution invariant].
substTys :: HasCallStack => TCvSubst -> [Type] -> [Type]
substTys subst tys
  | isEmptyTCvSubst subst = tys
  | otherwise = checkValidSubst subst tys [] $ map (subst_ty subst) tys

-- | Substitute within several 'Type's disabling the sanity checks.
-- The problems that the sanity checks in substTys catch are described in
-- Note [The substitution invariant].
-- The goal of #11371 is to migrate all the calls of substTysUnchecked to
-- substTys and remove this function. Please don't use in new code.
substTysUnchecked :: TCvSubst -> [Type] -> [Type]
substTysUnchecked subst tys
                 | isEmptyTCvSubst subst = tys
                 | otherwise             = map (subst_ty subst) tys

-- | Substitute within a 'ThetaType'
-- The substitution has to satisfy the invariants described in
-- Note [The substitution invariant].
substTheta :: HasCallStack => TCvSubst -> ThetaType -> ThetaType
substTheta = substTys

-- | Substitute within a 'ThetaType' disabling the sanity checks.
-- The problems that the sanity checks in substTys catch are described in
-- Note [The substitution invariant].
-- The goal of #11371 is to migrate all the calls of substThetaUnchecked to
-- substTheta and remove this function. Please don't use in new code.
substThetaUnchecked :: TCvSubst -> ThetaType -> ThetaType
substThetaUnchecked = substTysUnchecked


subst_ty :: TCvSubst -> Type -> Type
-- subst_ty is the main workhorse for type substitution
--
-- Note that the in_scope set is poked only if we hit a forall
-- so it may often never be fully computed
subst_ty subst ty
   = go ty
  where
    go (TyVarTy tv)      = substTyVar subst tv
    go (AppTy fun arg)   = mkAppTy (go fun) $! (go arg)
                -- The mkAppTy smart constructor is important
                -- we might be replacing (a Int), represented with App
                -- by [Int], represented with TyConApp
    go (TyConApp tc tys) = let args = map go tys
                           in  args `seqList` TyConApp tc args
    go ty@(FunTy { ft_arg = arg, ft_res = res })
      = let !arg' = go arg
            !res' = go res
        in ty { ft_arg = arg', ft_res = res' }
    go (ForAllTy (Bndr tv vis) ty)
                         = case substVarBndrUnchecked subst tv of
                             (subst', tv') ->
                               (ForAllTy $! ((Bndr $! tv') vis)) $!
                                            (subst_ty subst' ty)
    go (LitTy n)         = LitTy $! n
    go (CastTy ty co)    = (mkCastTy $! (go ty)) $! (subst_co subst co)
    go (CoercionTy co)   = CoercionTy $! (subst_co subst co)

substTyVar :: TCvSubst -> TyVar -> Type
substTyVar (TCvSubst _ tenv _) tv
  = ASSERT( isTyVar tv )
    case lookupVarEnv tenv tv of
      Just ty -> ty
      Nothing -> TyVarTy tv

substTyVars :: TCvSubst -> [TyVar] -> [Type]
substTyVars subst = map $ substTyVar subst

substTyCoVars :: TCvSubst -> [TyCoVar] -> [Type]
substTyCoVars subst = map $ substTyCoVar subst

substTyCoVar :: TCvSubst -> TyCoVar -> Type
substTyCoVar subst tv
  | isTyVar tv = substTyVar subst tv
  | otherwise = CoercionTy $ substCoVar subst tv

lookupTyVar :: TCvSubst -> TyVar  -> Maybe Type
        -- See Note [Extending the TCvSubst]
lookupTyVar (TCvSubst _ tenv _) tv
  = ASSERT( isTyVar tv )
    lookupVarEnv tenv tv

-- | Substitute within a 'Coercion'
-- The substitution has to satisfy the invariants described in
-- Note [The substitution invariant].
substCo :: HasCallStack => TCvSubst -> Coercion -> Coercion
substCo subst co
  | isEmptyTCvSubst subst = co
  | otherwise = checkValidSubst subst [] [co] $ subst_co subst co

-- | Substitute within a 'Coercion' disabling sanity checks.
-- The problems that the sanity checks in substCo catch are described in
-- Note [The substitution invariant].
-- The goal of #11371 is to migrate all the calls of substCoUnchecked to
-- substCo and remove this function. Please don't use in new code.
substCoUnchecked :: TCvSubst -> Coercion -> Coercion
substCoUnchecked subst co
  | isEmptyTCvSubst subst = co
  | otherwise = subst_co subst co

-- | Substitute within several 'Coercion's
-- The substitution has to satisfy the invariants described in
-- Note [The substitution invariant].
substCos :: HasCallStack => TCvSubst -> [Coercion] -> [Coercion]
substCos subst cos
  | isEmptyTCvSubst subst = cos
  | otherwise = checkValidSubst subst [] cos $ map (subst_co subst) cos

subst_co :: TCvSubst -> Coercion -> Coercion
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
    go (TyConAppCo r tc args)= let args' = map go args
                               in  args' `seqList` mkTyConAppCo r tc args'
    go (AppCo co arg)        = (mkAppCo $! go co) $! go arg
    go (ForAllCo tv kind_co co)
      = case substForAllCoBndrUnchecked subst tv kind_co of
         (subst', tv', kind_co') ->
          ((mkForAllCo $! tv') $! kind_co') $! subst_co subst' co
    go (FunCo r co1 co2)     = (mkFunCo r $! go co1) $! go co2
    go (CoVarCo cv)          = substCoVar subst cv
    go (AxiomInstCo con ind cos) = mkAxiomInstCo con ind $! map go cos
    go (UnivCo p r t1 t2)    = (((mkUnivCo $! go_prov p) $! r) $!
                                (go_ty t1)) $! (go_ty t2)
    go (SymCo co)            = mkSymCo $! (go co)
    go (TransCo co1 co2)     = (mkTransCo $! (go co1)) $! (go co2)
    go (NthCo r d co)        = mkNthCo r d $! (go co)
    go (LRCo lr co)          = mkLRCo lr $! (go co)
    go (InstCo co arg)       = (mkInstCo $! (go co)) $! go arg
    go (KindCo co)           = mkKindCo $! (go co)
    go (SubCo co)            = mkSubCo $! (go co)
    go (AxiomRuleCo c cs)    = let cs1 = map go cs
                                in cs1 `seqList` AxiomRuleCo c cs1
    go (HoleCo h)            = HoleCo $! go_hole h

    go_prov (PhantomProv kco)    = PhantomProv (go kco)
    go_prov (ProofIrrelProv kco) = ProofIrrelProv (go kco)
    go_prov p@(PluginProv _)     = p

    -- See Note [Substituting in a coercion hole]
    go_hole h@(CoercionHole { ch_co_var = cv })
      = h { ch_co_var = updateVarType go_ty cv }

substForAllCoBndr :: TCvSubst -> TyCoVar -> KindCoercion
                  -> (TCvSubst, TyCoVar, Coercion)
substForAllCoBndr subst
  = substForAllCoBndrUsing False (substCo subst) subst

-- | Like 'substForAllCoBndr', but disables sanity checks.
-- The problems that the sanity checks in substCo catch are described in
-- Note [The substitution invariant].
-- The goal of #11371 is to migrate all the calls of substCoUnchecked to
-- substCo and remove this function. Please don't use in new code.
substForAllCoBndrUnchecked :: TCvSubst -> TyCoVar -> KindCoercion
                           -> (TCvSubst, TyCoVar, Coercion)
substForAllCoBndrUnchecked subst
  = substForAllCoBndrUsing False (substCoUnchecked subst) subst

-- See Note [Sym and ForAllCo]
substForAllCoBndrUsing :: Bool  -- apply sym to binder?
                       -> (Coercion -> Coercion)  -- transformation to kind co
                       -> TCvSubst -> TyCoVar -> KindCoercion
                       -> (TCvSubst, TyCoVar, KindCoercion)
substForAllCoBndrUsing sym sco subst old_var
  | isTyVar old_var = substForAllCoTyVarBndrUsing sym sco subst old_var
  | otherwise       = substForAllCoCoVarBndrUsing sym sco subst old_var

substForAllCoTyVarBndrUsing :: Bool  -- apply sym to binder?
                            -> (Coercion -> Coercion)  -- transformation to kind co
                            -> TCvSubst -> TyVar -> KindCoercion
                            -> (TCvSubst, TyVar, KindCoercion)
substForAllCoTyVarBndrUsing sym sco (TCvSubst in_scope tenv cenv) old_var old_kind_co
  = ASSERT( isTyVar old_var )
    ( TCvSubst (in_scope `extendInScopeSet` new_var) new_env cenv
    , new_var, new_kind_co )
  where
    new_env | no_change && not sym = delVarEnv tenv old_var
            | sym       = extendVarEnv tenv old_var $
                          TyVarTy new_var `CastTy` new_kind_co
            | otherwise = extendVarEnv tenv old_var (TyVarTy new_var)

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

substForAllCoCoVarBndrUsing :: Bool  -- apply sym to binder?
                            -> (Coercion -> Coercion)  -- transformation to kind co
                            -> TCvSubst -> CoVar -> KindCoercion
                            -> (TCvSubst, CoVar, KindCoercion)
substForAllCoCoVarBndrUsing sym sco (TCvSubst in_scope tenv cenv)
                            old_var old_kind_co
  = ASSERT( isCoVar old_var )
    ( TCvSubst (in_scope `extendInScopeSet` new_var) tenv new_cenv
    , new_var, new_kind_co )
  where
    new_cenv | no_change && not sym = delVarEnv cenv old_var
             | otherwise = extendVarEnv cenv old_var (mkCoVarCo new_var)

    no_kind_change = noFreeVarsOfCo old_kind_co
    no_change = no_kind_change && (new_var == old_var)

    new_kind_co | no_kind_change = old_kind_co
                | otherwise      = sco old_kind_co

    Pair h1 h2 = coercionKind new_kind_co

    new_var       = uniqAway in_scope $ mkCoVar (varName old_var) new_var_type
    new_var_type  | sym       = h2
                  | otherwise = h1

substCoVar :: TCvSubst -> CoVar -> Coercion
substCoVar (TCvSubst _ _ cenv) cv
  = case lookupVarEnv cenv cv of
      Just co -> co
      Nothing -> CoVarCo cv

substCoVars :: TCvSubst -> [CoVar] -> [Coercion]
substCoVars subst cvs = map (substCoVar subst) cvs

lookupCoVar :: TCvSubst -> Var -> Maybe Coercion
lookupCoVar (TCvSubst _ _ cenv) v = lookupVarEnv cenv v

substTyVarBndr :: HasCallStack => TCvSubst -> TyVar -> (TCvSubst, TyVar)
substTyVarBndr = substTyVarBndrUsing substTy

substTyVarBndrs :: HasCallStack => TCvSubst -> [TyVar] -> (TCvSubst, [TyVar])
substTyVarBndrs = mapAccumL substTyVarBndr

substVarBndr :: HasCallStack => TCvSubst -> TyCoVar -> (TCvSubst, TyCoVar)
substVarBndr = substVarBndrUsing substTy

substVarBndrs :: HasCallStack => TCvSubst -> [TyCoVar] -> (TCvSubst, [TyCoVar])
substVarBndrs = mapAccumL substVarBndr

substCoVarBndr :: HasCallStack => TCvSubst -> CoVar -> (TCvSubst, CoVar)
substCoVarBndr = substCoVarBndrUsing substTy

-- | Like 'substVarBndr', but disables sanity checks.
-- The problems that the sanity checks in substTy catch are described in
-- Note [The substitution invariant].
-- The goal of #11371 is to migrate all the calls of substTyUnchecked to
-- substTy and remove this function. Please don't use in new code.
substVarBndrUnchecked :: TCvSubst -> TyCoVar -> (TCvSubst, TyCoVar)
substVarBndrUnchecked = substVarBndrUsing substTyUnchecked

substVarBndrUsing :: (TCvSubst -> Type -> Type)
                  -> TCvSubst -> TyCoVar -> (TCvSubst, TyCoVar)
substVarBndrUsing subst_fn subst v
  | isTyVar v = substTyVarBndrUsing subst_fn subst v
  | otherwise = substCoVarBndrUsing subst_fn subst v

-- | Substitute a tyvar in a binding position, returning an
-- extended subst and a new tyvar.
-- Use the supplied function to substitute in the kind
substTyVarBndrUsing
  :: (TCvSubst -> Type -> Type)  -- ^ Use this to substitute in the kind
  -> TCvSubst -> TyVar -> (TCvSubst, TyVar)
substTyVarBndrUsing subst_fn subst@(TCvSubst in_scope tenv cenv) old_var
  = ASSERT2( _no_capture, pprTyVar old_var $$ pprTyVar new_var $$ ppr subst )
    ASSERT( isTyVar old_var )
    (TCvSubst (in_scope `extendInScopeSet` new_var) new_env cenv, new_var)
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
        -- See Note [Extending the TCvSubst]
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
  :: (TCvSubst -> Type -> Type)
  -> TCvSubst -> CoVar -> (TCvSubst, CoVar)
substCoVarBndrUsing subst_fn subst@(TCvSubst in_scope tenv cenv) old_var
  = ASSERT( isCoVar old_var )
    (TCvSubst (in_scope `extendInScopeSet` new_var) tenv new_cenv, new_var)
  where
    new_co         = mkCoVarCo new_var
    no_kind_change = noFreeVarsOfTypes [t1, t2]
    no_change      = new_var == old_var && no_kind_change

    new_cenv | no_change = delVarEnv cenv old_var
             | otherwise = extendVarEnv cenv old_var new_co

    new_var = uniqAway in_scope subst_old_var
    subst_old_var = mkCoVar (varName old_var) new_var_type

    (_, _, t1, t2, role) = coVarKindsTypesRole old_var
    t1' = subst_fn subst t1
    t2' = subst_fn subst t2
    new_var_type = mkCoercionType role t1' t2'
                  -- It's important to do the substitution for coercions,
                  -- because they can have free type variables

cloneTyVarBndr :: TCvSubst -> TyVar -> Unique -> (TCvSubst, TyVar)
cloneTyVarBndr subst@(TCvSubst in_scope tv_env cv_env) tv uniq
  = ASSERT2( isTyVar tv, ppr tv )   -- I think it's only called on TyVars
    (TCvSubst (extendInScopeSet in_scope tv')
              (extendVarEnv tv_env tv (mkTyVarTy tv')) cv_env, tv')
  where
    old_ki = tyVarKind tv
    no_kind_change = noFreeVarsOfType old_ki -- verify that kind is closed

    tv1 | no_kind_change = tv
        | otherwise      = setTyVarKind tv (substTy subst old_ki)

    tv' = setVarUnique tv1 uniq

cloneTyVarBndrs :: TCvSubst -> [TyVar] -> UniqSupply -> (TCvSubst, [TyVar])
cloneTyVarBndrs subst []     _usupply = (subst, [])
cloneTyVarBndrs subst (t:ts)  usupply = (subst'', tv:tvs)
  where
    (uniq, usupply') = takeUniqFromSupply usupply
    (subst' , tv )   = cloneTyVarBndr subst t uniq
    (subst'', tvs)   = cloneTyVarBndrs subst' ts usupply'

module TyCoFVs
  (
        tyCoVarsOfType, tyCoVarsOfTypeDSet, tyCoVarsOfTypes, tyCoVarsOfTypesDSet,
        tyCoFVsBndr, tyCoFVsVarBndr, tyCoFVsVarBndrs,
        tyCoFVsOfType, tyCoVarsOfTypeList,
        tyCoFVsOfTypes, tyCoVarsOfTypesList,
        tyCoVarsOfTypesSet, tyCoVarsOfCosSet,
        coVarsOfType, coVarsOfTypes,
        coVarsOfCo, coVarsOfCos,
        tyCoVarsOfCo, tyCoVarsOfCos,
        tyCoVarsOfCoDSet,
        tyCoFVsOfCo, tyCoFVsOfCos,
        tyCoVarsOfCoList, tyCoVarsOfProv,
        almostDevoidCoVarOfCo,
        injectiveVarsOfType, tyConAppNeedsKindSig,

        noFreeVarsOfType, noFreeVarsOfTypes, noFreeVarsOfCo,

        mkTyCoInScopeSet,

        -- * Welll-scoped free variables
        scopedSort, tyCoVarsOfTypeWellScoped,
        tyCoVarsOfTypesWellScoped,
  ) where

import GhcPrelude

import {-# SOURCE #-} Type (coreView)

import TyCoRep
import TyCon
import Var
import FV

import UniqFM
import VarSet
import VarEnv
import Util
import Panic

{-
%************************************************************************
%*                                                                      *
                 Free variables of types and coercions
%*                                                                      *
%************************************************************************
-}

{- Note [Free variables of types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The family of functions tyCoVarsOfType, tyCoVarsOfTypes etc, returns
a VarSet that is closed over the types of its variables.  More precisely,
  if    S = tyCoVarsOfType( t )
  and   (a:k) is in S
  then  tyCoVarsOftype( k ) is a subset of S

Example: The tyCoVars of this ((a:* -> k) Int) is {a, k}.

We could /not/ close over the kinds of the variable occurrences, and
instead do so at call sites, but it seems that we always want to do
so, so it's easiest to do it here.

It turns out that getting the free variables of types is performance critical,
so we profiled several versions, exploring different implementation strategies.

1. Baseline version: uses FV naively. Essentially:

   tyCoVarsOfType ty = fvVarSet $ tyCoFVsOfType ty

   This is not nice, because FV introduces some overhead to implement
   determinism, and throught its "interesting var" function, neither of which
   we need here, so they are a complete waste.

2. UnionVarSet version: instead of reusing the FV-based code, we simply used
   VarSets directly, trying to avoid the overhead of FV. E.g.:

   -- FV version:
   tyCoFVsOfType (AppTy fun arg)    a b c = (tyCoFVsOfType fun `unionFV` tyCoFVsOfType arg) a b c

   -- UnionVarSet version:
   tyCoVarsOfType (AppTy fun arg)    = (tyCoVarsOfType fun `unionVarSet` tyCoVarsOfType arg)

   This looks deceptively similar, but while FV internally builds a list- and
   set-generating function, the VarSet functions manipulate sets directly, and
   the latter peforms a lot worse than the naive FV version.

3. Accumulator-style VarSet version: this is what we use now. We do use VarSet
   as our data structure, but delegate the actual work to a new
   ty_co_vars_of_...  family of functions, which use accumulator style and the
   "in-scope set" filter found in the internals of FV, but without the
   determinism overhead.

See #14880.

Note [Closing over free variable kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tyCoVarsOfType and tyCoFVsOfType, while traversing a type, will also close over
free variable kinds. In previous GHC versions, this happened naively: whenever
we would encounter an occurrence of a free type variable, we would close over
its kind. This, however is wrong for two reasons (see #14880):

1. Efficiency. If we have Proxy (a::k) -> Proxy (a::k) -> Proxy (a::k), then
   we don't want to have to traverse k more than once.

2. Correctness. Imagine we have forall k. b -> k, where b has
   kind k, for some k bound in an outer scope. If we look at b's kind inside
   the forall, we'll collect that k is free and then remove k from the set of
   free variables. This is plain wrong. We must instead compute that b is free
   and then conclude that b's kind is free.

An obvious first approach is to move the closing-over-kinds from the
occurrences of a type variable to after finding the free vars - however, this
turns out to introduce performance regressions, and isn't even entirely
correct.

In fact, it isn't even important *when* we close over kinds; what matters is
that we handle each type var exactly once, and that we do it in the right
context.

So the next approach we tried was to use the "in-scope set" part of FV or the
equivalent argument in the accumulator-style `ty_co_vars_of_type` function, to
say "don't bother with variables we have already closed over". This should work
fine in theory, but the code is complicated and doesn't perform well.

But there is a simpler way, which is implemented here. Consider the two points
above:

1. Efficiency: we now have an accumulator, so the second time we encounter 'a',
   we'll ignore it, certainly not looking at its kind - this is why
   pre-checking set membership before inserting ends up not only being faster,
   but also being correct.

2. Correctness: we have an "in-scope set" (I think we should call it it a
  "bound-var set"), specifying variables that are bound by a forall in the type
  we are traversing; we simply ignore these variables, certainly not looking at
  their kind.

So now consider:

    forall k. b -> k

where b :: k->Type is free; but of course, it's a different k! When looking at
b -> k we'll have k in the bound-var set. So we'll ignore the k. But suppose
this is our first encounter with b; we want the free vars of its kind. But we
want to behave as if we took the free vars of its kind at the end; that is,
with no bound vars in scope.

So the solution is easy. The old code was this:

  ty_co_vars_of_type (TyVarTy v) is acc
    | v `elemVarSet` is  = acc
    | v `elemVarSet` acc = acc
    | otherwise          = ty_co_vars_of_type (tyVarKind v) is (extendVarSet acc v)

Now all we need to do is take the free vars of tyVarKind v *with an empty
bound-var set*, thus:

ty_co_vars_of_type (TyVarTy v) is acc
  | v `elemVarSet` is  = acc
  | v `elemVarSet` acc = acc
  | otherwise          = ty_co_vars_of_type (tyVarKind v) emptyVarSet (extendVarSet acc v)
                                                          ^^^^^^^^^^^

And that's it.

-}

tyCoVarsOfType :: Type -> TyCoVarSet
-- See Note [Free variables of types]
tyCoVarsOfType ty = ty_co_vars_of_type ty emptyVarSet emptyVarSet

tyCoVarsOfTypes :: [Type] -> TyCoVarSet
tyCoVarsOfTypes tys = ty_co_vars_of_types tys emptyVarSet emptyVarSet

ty_co_vars_of_type :: Type -> TyCoVarSet -> TyCoVarSet -> TyCoVarSet
ty_co_vars_of_type (TyVarTy v) is acc
  | v `elemVarSet` is  = acc
  | v `elemVarSet` acc = acc
  | otherwise          = ty_co_vars_of_type (tyVarKind v)
                            emptyVarSet  -- See Note [Closing over free variable kinds]
                            (extendVarSet acc v)

ty_co_vars_of_type (TyConApp _ tys)   is acc = ty_co_vars_of_types tys is acc
ty_co_vars_of_type (LitTy {})         _  acc = acc
ty_co_vars_of_type (AppTy fun arg)    is acc = ty_co_vars_of_type fun is (ty_co_vars_of_type arg is acc)
ty_co_vars_of_type (FunTy _ arg res)  is acc = ty_co_vars_of_type arg is (ty_co_vars_of_type res is acc)
ty_co_vars_of_type (ForAllTy (Bndr tv _) ty) is acc = ty_co_vars_of_type (varType tv) is $
                                                      ty_co_vars_of_type ty (extendVarSet is tv) acc
ty_co_vars_of_type (CastTy ty co)     is acc = ty_co_vars_of_type ty is (ty_co_vars_of_co co is acc)
ty_co_vars_of_type (CoercionTy co)    is acc = ty_co_vars_of_co co is acc

ty_co_vars_of_types :: [Type] -> TyCoVarSet -> TyCoVarSet -> TyCoVarSet
ty_co_vars_of_types []       _  acc = acc
ty_co_vars_of_types (ty:tys) is acc = ty_co_vars_of_type ty is (ty_co_vars_of_types tys is acc)

tyCoVarsOfCo :: Coercion -> TyCoVarSet
-- See Note [Free variables of types]
tyCoVarsOfCo co = ty_co_vars_of_co co emptyVarSet emptyVarSet

tyCoVarsOfCos :: [Coercion] -> TyCoVarSet
tyCoVarsOfCos cos = ty_co_vars_of_cos cos emptyVarSet emptyVarSet


ty_co_vars_of_co :: Coercion -> TyCoVarSet -> TyCoVarSet -> TyCoVarSet
ty_co_vars_of_co (Refl ty)            is acc = ty_co_vars_of_type ty is acc
ty_co_vars_of_co (GRefl _ ty mco)     is acc = ty_co_vars_of_type ty is $
                                               ty_co_vars_of_mco mco is acc
ty_co_vars_of_co (TyConAppCo _ _ cos) is acc = ty_co_vars_of_cos cos is acc
ty_co_vars_of_co (AppCo co arg)       is acc = ty_co_vars_of_co co is $
                                               ty_co_vars_of_co arg is acc
ty_co_vars_of_co (ForAllCo tv kind_co co) is acc = ty_co_vars_of_co kind_co is $
                                                   ty_co_vars_of_co co (extendVarSet is tv) acc
ty_co_vars_of_co (FunCo _ co1 co2)    is acc = ty_co_vars_of_co co1 is $
                                               ty_co_vars_of_co co2 is acc
ty_co_vars_of_co (CoVarCo v)          is acc = ty_co_vars_of_co_var v is acc
ty_co_vars_of_co (HoleCo h)           is acc = ty_co_vars_of_co_var (coHoleCoVar h) is acc
    -- See Note [CoercionHoles and coercion free variables]
ty_co_vars_of_co (AxiomInstCo _ _ cos) is acc = ty_co_vars_of_cos cos is acc
ty_co_vars_of_co (UnivCo p _ t1 t2)    is acc = ty_co_vars_of_prov p is $
                                                ty_co_vars_of_type t1 is $
                                                ty_co_vars_of_type t2 is acc
ty_co_vars_of_co (SymCo co)          is acc = ty_co_vars_of_co co is acc
ty_co_vars_of_co (TransCo co1 co2)   is acc = ty_co_vars_of_co co1 is $
                                              ty_co_vars_of_co co2 is acc
ty_co_vars_of_co (NthCo _ _ co)      is acc = ty_co_vars_of_co co is acc
ty_co_vars_of_co (LRCo _ co)         is acc = ty_co_vars_of_co co is acc
ty_co_vars_of_co (InstCo co arg)     is acc = ty_co_vars_of_co co is $
                                              ty_co_vars_of_co arg is acc
ty_co_vars_of_co (KindCo co)         is acc = ty_co_vars_of_co co is acc
ty_co_vars_of_co (SubCo co)          is acc = ty_co_vars_of_co co is acc
ty_co_vars_of_co (AxiomRuleCo _ cs)  is acc = ty_co_vars_of_cos cs is acc

ty_co_vars_of_mco :: MCoercion -> TyCoVarSet -> TyCoVarSet -> TyCoVarSet
ty_co_vars_of_mco MRefl    _is acc = acc
ty_co_vars_of_mco (MCo co) is  acc = ty_co_vars_of_co co is acc

ty_co_vars_of_co_var :: CoVar -> TyCoVarSet -> TyCoVarSet -> TyCoVarSet
ty_co_vars_of_co_var v is acc
  | v `elemVarSet` is  = acc
  | v `elemVarSet` acc = acc
  | otherwise          = ty_co_vars_of_type (varType v)
                            emptyVarSet  -- See Note [Closing over free variable kinds]
                            (extendVarSet acc v)

ty_co_vars_of_cos :: [Coercion] -> TyCoVarSet -> TyCoVarSet -> TyCoVarSet
ty_co_vars_of_cos []       _  acc = acc
ty_co_vars_of_cos (co:cos) is acc = ty_co_vars_of_co co is (ty_co_vars_of_cos cos is acc)

tyCoVarsOfProv :: UnivCoProvenance -> TyCoVarSet
tyCoVarsOfProv prov = ty_co_vars_of_prov prov emptyVarSet emptyVarSet

ty_co_vars_of_prov :: UnivCoProvenance -> TyCoVarSet -> TyCoVarSet -> TyCoVarSet
ty_co_vars_of_prov (PhantomProv co)    is acc = ty_co_vars_of_co co is acc
ty_co_vars_of_prov (ProofIrrelProv co) is acc = ty_co_vars_of_co co is acc
ty_co_vars_of_prov UnsafeCoerceProv    _  acc = acc
ty_co_vars_of_prov (PluginProv _)      _  acc = acc

-- | Generates an in-scope set from the free variables in a list of types
-- and a list of coercions
mkTyCoInScopeSet :: [Type] -> [Coercion] -> InScopeSet
mkTyCoInScopeSet tys cos
  = mkInScopeSet (ty_co_vars_of_types tys emptyVarSet $
                  ty_co_vars_of_cos   cos emptyVarSet emptyVarSet)

-- | `tyCoFVsOfType` that returns free variables of a type in a deterministic
-- set. For explanation of why using `VarSet` is not deterministic see
-- Note [Deterministic FV] in FV.
tyCoVarsOfTypeDSet :: Type -> DTyCoVarSet
-- See Note [Free variables of types]
tyCoVarsOfTypeDSet ty = fvDVarSet $ tyCoFVsOfType ty

-- | `tyCoFVsOfType` that returns free variables of a type in deterministic
-- order. For explanation of why using `VarSet` is not deterministic see
-- Note [Deterministic FV] in FV.
tyCoVarsOfTypeList :: Type -> [TyCoVar]
-- See Note [Free variables of types]
tyCoVarsOfTypeList ty = fvVarList $ tyCoFVsOfType ty

-- | Returns free variables of types, including kind variables as
-- a non-deterministic set. For type synonyms it does /not/ expand the
-- synonym.
tyCoVarsOfTypesSet :: TyVarEnv Type -> TyCoVarSet
-- See Note [Free variables of types]
tyCoVarsOfTypesSet tys = tyCoVarsOfTypes $ nonDetEltsUFM tys
  -- It's OK to use nonDetEltsUFM here because we immediately forget the
  -- ordering by returning a set

-- | Returns free variables of types, including kind variables as
-- a deterministic set. For type synonyms it does /not/ expand the
-- synonym.
tyCoVarsOfTypesDSet :: [Type] -> DTyCoVarSet
-- See Note [Free variables of types]
tyCoVarsOfTypesDSet tys = fvDVarSet $ tyCoFVsOfTypes tys

-- | Returns free variables of types, including kind variables as
-- a deterministically ordered list. For type synonyms it does /not/ expand the
-- synonym.
tyCoVarsOfTypesList :: [Type] -> [TyCoVar]
-- See Note [Free variables of types]
tyCoVarsOfTypesList tys = fvVarList $ tyCoFVsOfTypes tys

-- | The worker for `tyCoFVsOfType` and `tyCoFVsOfTypeList`.
-- The previous implementation used `unionVarSet` which is O(n+m) and can
-- make the function quadratic.
-- It's exported, so that it can be composed with
-- other functions that compute free variables.
-- See Note [FV naming conventions] in FV.
--
-- Eta-expanded because that makes it run faster (apparently)
-- See Note [FV eta expansion] in FV for explanation.
tyCoFVsOfType :: Type -> FV
-- See Note [Free variables of types]
tyCoFVsOfType (TyVarTy v)        f bound_vars (acc_list, acc_set)
  | not (f v) = (acc_list, acc_set)
  | v `elemVarSet` bound_vars = (acc_list, acc_set)
  | v `elemVarSet` acc_set = (acc_list, acc_set)
  | otherwise = tyCoFVsOfType (tyVarKind v) f
                               emptyVarSet   -- See Note [Closing over free variable kinds]
                               (v:acc_list, extendVarSet acc_set v)
tyCoFVsOfType (TyConApp _ tys)   f bound_vars acc = tyCoFVsOfTypes tys f bound_vars acc
tyCoFVsOfType (LitTy {})         f bound_vars acc = emptyFV f bound_vars acc
tyCoFVsOfType (AppTy fun arg)    f bound_vars acc = (tyCoFVsOfType fun `unionFV` tyCoFVsOfType arg) f bound_vars acc
tyCoFVsOfType (FunTy _ arg res)  f bound_vars acc = (tyCoFVsOfType arg `unionFV` tyCoFVsOfType res) f bound_vars acc
tyCoFVsOfType (ForAllTy bndr ty) f bound_vars acc = tyCoFVsBndr bndr (tyCoFVsOfType ty)  f bound_vars acc
tyCoFVsOfType (CastTy ty co)     f bound_vars acc = (tyCoFVsOfType ty `unionFV` tyCoFVsOfCo co) f bound_vars acc
tyCoFVsOfType (CoercionTy co)    f bound_vars acc = tyCoFVsOfCo co f bound_vars acc

tyCoFVsBndr :: TyCoVarBinder -> FV -> FV
-- Free vars of (forall b. <thing with fvs>)
tyCoFVsBndr (Bndr tv _) fvs = tyCoFVsVarBndr tv fvs

tyCoFVsVarBndrs :: [Var] -> FV -> FV
tyCoFVsVarBndrs vars fvs = foldr tyCoFVsVarBndr fvs vars

tyCoFVsVarBndr :: Var -> FV -> FV
tyCoFVsVarBndr var fvs
  = tyCoFVsOfType (varType var)   -- Free vars of its type/kind
    `unionFV` delFV var fvs       -- Delete it from the thing-inside

tyCoFVsOfTypes :: [Type] -> FV
-- See Note [Free variables of types]
tyCoFVsOfTypes (ty:tys) fv_cand in_scope acc = (tyCoFVsOfType ty `unionFV` tyCoFVsOfTypes tys) fv_cand in_scope acc
tyCoFVsOfTypes []       fv_cand in_scope acc = emptyFV fv_cand in_scope acc

-- | Get a deterministic set of the vars free in a coercion
tyCoVarsOfCoDSet :: Coercion -> DTyCoVarSet
-- See Note [Free variables of types]
tyCoVarsOfCoDSet co = fvDVarSet $ tyCoFVsOfCo co

tyCoVarsOfCoList :: Coercion -> [TyCoVar]
-- See Note [Free variables of types]
tyCoVarsOfCoList co = fvVarList $ tyCoFVsOfCo co

tyCoFVsOfMCo :: MCoercion -> FV
tyCoFVsOfMCo MRefl    = emptyFV
tyCoFVsOfMCo (MCo co) = tyCoFVsOfCo co

tyCoVarsOfCosSet :: CoVarEnv Coercion -> TyCoVarSet
tyCoVarsOfCosSet cos = tyCoVarsOfCos $ nonDetEltsUFM cos
  -- It's OK to use nonDetEltsUFM here because we immediately forget the
  -- ordering by returning a set

tyCoFVsOfCo :: Coercion -> FV
-- Extracts type and coercion variables from a coercion
-- See Note [Free variables of types]
tyCoFVsOfCo (Refl ty) fv_cand in_scope acc
  = tyCoFVsOfType ty fv_cand in_scope acc
tyCoFVsOfCo (GRefl _ ty mco) fv_cand in_scope acc
  = (tyCoFVsOfType ty `unionFV` tyCoFVsOfMCo mco) fv_cand in_scope acc
tyCoFVsOfCo (TyConAppCo _ _ cos) fv_cand in_scope acc = tyCoFVsOfCos cos fv_cand in_scope acc
tyCoFVsOfCo (AppCo co arg) fv_cand in_scope acc
  = (tyCoFVsOfCo co `unionFV` tyCoFVsOfCo arg) fv_cand in_scope acc
tyCoFVsOfCo (ForAllCo tv kind_co co) fv_cand in_scope acc
  = (tyCoFVsVarBndr tv (tyCoFVsOfCo co) `unionFV` tyCoFVsOfCo kind_co) fv_cand in_scope acc
tyCoFVsOfCo (FunCo _ co1 co2)    fv_cand in_scope acc
  = (tyCoFVsOfCo co1 `unionFV` tyCoFVsOfCo co2) fv_cand in_scope acc
tyCoFVsOfCo (CoVarCo v) fv_cand in_scope acc
  = tyCoFVsOfCoVar v fv_cand in_scope acc
tyCoFVsOfCo (HoleCo h) fv_cand in_scope acc
  = tyCoFVsOfCoVar (coHoleCoVar h) fv_cand in_scope acc
    -- See Note [CoercionHoles and coercion free variables]
tyCoFVsOfCo (AxiomInstCo _ _ cos) fv_cand in_scope acc = tyCoFVsOfCos cos fv_cand in_scope acc
tyCoFVsOfCo (UnivCo p _ t1 t2) fv_cand in_scope acc
  = (tyCoFVsOfProv p `unionFV` tyCoFVsOfType t1
                     `unionFV` tyCoFVsOfType t2) fv_cand in_scope acc
tyCoFVsOfCo (SymCo co)          fv_cand in_scope acc = tyCoFVsOfCo co fv_cand in_scope acc
tyCoFVsOfCo (TransCo co1 co2)   fv_cand in_scope acc = (tyCoFVsOfCo co1 `unionFV` tyCoFVsOfCo co2) fv_cand in_scope acc
tyCoFVsOfCo (NthCo _ _ co)      fv_cand in_scope acc = tyCoFVsOfCo co fv_cand in_scope acc
tyCoFVsOfCo (LRCo _ co)         fv_cand in_scope acc = tyCoFVsOfCo co fv_cand in_scope acc
tyCoFVsOfCo (InstCo co arg)     fv_cand in_scope acc = (tyCoFVsOfCo co `unionFV` tyCoFVsOfCo arg) fv_cand in_scope acc
tyCoFVsOfCo (KindCo co)         fv_cand in_scope acc = tyCoFVsOfCo co fv_cand in_scope acc
tyCoFVsOfCo (SubCo co)          fv_cand in_scope acc = tyCoFVsOfCo co fv_cand in_scope acc
tyCoFVsOfCo (AxiomRuleCo _ cs)  fv_cand in_scope acc = tyCoFVsOfCos cs fv_cand in_scope acc

tyCoFVsOfCoVar :: CoVar -> FV
tyCoFVsOfCoVar v fv_cand in_scope acc
  = (unitFV v `unionFV` tyCoFVsOfType (varType v)) fv_cand in_scope acc

tyCoFVsOfProv :: UnivCoProvenance -> FV
tyCoFVsOfProv UnsafeCoerceProv    fv_cand in_scope acc = emptyFV fv_cand in_scope acc
tyCoFVsOfProv (PhantomProv co)    fv_cand in_scope acc = tyCoFVsOfCo co fv_cand in_scope acc
tyCoFVsOfProv (ProofIrrelProv co) fv_cand in_scope acc = tyCoFVsOfCo co fv_cand in_scope acc
tyCoFVsOfProv (PluginProv _)      fv_cand in_scope acc = emptyFV fv_cand in_scope acc

tyCoFVsOfCos :: [Coercion] -> FV
tyCoFVsOfCos []       fv_cand in_scope acc = emptyFV fv_cand in_scope acc
tyCoFVsOfCos (co:cos) fv_cand in_scope acc = (tyCoFVsOfCo co `unionFV` tyCoFVsOfCos cos) fv_cand in_scope acc


------------- Extracting the CoVars of a type or coercion -----------

{-

Note [CoVarsOfX and the InterestingVarFun]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The coVarsOfType, coVarsOfTypes, coVarsOfCo, and coVarsOfCos functions are
implemented in terms of the respective FV equivalents (tyCoFVsOf...), rather
than the VarSet-based flavors (tyCoVarsOf...), despite the performance
considerations outlined in Note [Free variables of types].

This is because FV includes the InterestingVarFun, which is useful here,
because we can cleverly use it to restrict our calculations to CoVars - this
is what getCoVarSet achieves.

See #14880.

-}

getCoVarSet :: FV -> CoVarSet
getCoVarSet fv = snd (fv isCoVar emptyVarSet ([], emptyVarSet))

coVarsOfType :: Type -> CoVarSet
coVarsOfType ty = getCoVarSet (tyCoFVsOfType ty)

coVarsOfTypes :: [Type] -> TyCoVarSet
coVarsOfTypes tys = getCoVarSet (tyCoFVsOfTypes tys)

coVarsOfCo :: Coercion -> CoVarSet
coVarsOfCo co = getCoVarSet (tyCoFVsOfCo co)

coVarsOfCos :: [Coercion] -> CoVarSet
coVarsOfCos cos = getCoVarSet (tyCoFVsOfCos cos)

----- Whether a covar is /Almost Devoid/ in a type or coercion ----

-- | Given a covar and a coercion, returns True if covar is almost devoid in
-- the coercion. That is, covar can only appear in Refl and GRefl.
-- See last wrinkle in Note [Unused coercion variable in ForAllCo] in Coercion
almostDevoidCoVarOfCo :: CoVar -> Coercion -> Bool
almostDevoidCoVarOfCo cv co =
  almost_devoid_co_var_of_co co cv

almost_devoid_co_var_of_co :: Coercion -> CoVar -> Bool
almost_devoid_co_var_of_co (Refl {}) _ = True   -- covar is allowed in Refl and
almost_devoid_co_var_of_co (GRefl {}) _ = True  -- GRefl, so we don't look into
                                                -- the coercions
almost_devoid_co_var_of_co (TyConAppCo _ _ cos) cv
  = almost_devoid_co_var_of_cos cos cv
almost_devoid_co_var_of_co (AppCo co arg) cv
  = almost_devoid_co_var_of_co co cv
  && almost_devoid_co_var_of_co arg cv
almost_devoid_co_var_of_co (ForAllCo v kind_co co) cv
  = almost_devoid_co_var_of_co kind_co cv
  && (v == cv || almost_devoid_co_var_of_co co cv)
almost_devoid_co_var_of_co (FunCo _ co1 co2) cv
  = almost_devoid_co_var_of_co co1 cv
  && almost_devoid_co_var_of_co co2 cv
almost_devoid_co_var_of_co (CoVarCo v) cv = v /= cv
almost_devoid_co_var_of_co (HoleCo h)  cv = (coHoleCoVar h) /= cv
almost_devoid_co_var_of_co (AxiomInstCo _ _ cos) cv
  = almost_devoid_co_var_of_cos cos cv
almost_devoid_co_var_of_co (UnivCo p _ t1 t2) cv
  = almost_devoid_co_var_of_prov p cv
  && almost_devoid_co_var_of_type t1 cv
  && almost_devoid_co_var_of_type t2 cv
almost_devoid_co_var_of_co (SymCo co) cv
  = almost_devoid_co_var_of_co co cv
almost_devoid_co_var_of_co (TransCo co1 co2) cv
  = almost_devoid_co_var_of_co co1 cv
  && almost_devoid_co_var_of_co co2 cv
almost_devoid_co_var_of_co (NthCo _ _ co) cv
  = almost_devoid_co_var_of_co co cv
almost_devoid_co_var_of_co (LRCo _ co) cv
  = almost_devoid_co_var_of_co co cv
almost_devoid_co_var_of_co (InstCo co arg) cv
  = almost_devoid_co_var_of_co co cv
  && almost_devoid_co_var_of_co arg cv
almost_devoid_co_var_of_co (KindCo co) cv
  = almost_devoid_co_var_of_co co cv
almost_devoid_co_var_of_co (SubCo co) cv
  = almost_devoid_co_var_of_co co cv
almost_devoid_co_var_of_co (AxiomRuleCo _ cs) cv
  = almost_devoid_co_var_of_cos cs cv

almost_devoid_co_var_of_cos :: [Coercion] -> CoVar -> Bool
almost_devoid_co_var_of_cos [] _ = True
almost_devoid_co_var_of_cos (co:cos) cv
  = almost_devoid_co_var_of_co co cv
  && almost_devoid_co_var_of_cos cos cv

almost_devoid_co_var_of_prov :: UnivCoProvenance -> CoVar -> Bool
almost_devoid_co_var_of_prov (PhantomProv co) cv
  = almost_devoid_co_var_of_co co cv
almost_devoid_co_var_of_prov (ProofIrrelProv co) cv
  = almost_devoid_co_var_of_co co cv
almost_devoid_co_var_of_prov UnsafeCoerceProv _ = True
almost_devoid_co_var_of_prov (PluginProv _) _ = True

almost_devoid_co_var_of_type :: Type -> CoVar -> Bool
almost_devoid_co_var_of_type (TyVarTy _) _ = True
almost_devoid_co_var_of_type (TyConApp _ tys) cv
  = almost_devoid_co_var_of_types tys cv
almost_devoid_co_var_of_type (LitTy {}) _ = True
almost_devoid_co_var_of_type (AppTy fun arg) cv
  = almost_devoid_co_var_of_type fun cv
  && almost_devoid_co_var_of_type arg cv
almost_devoid_co_var_of_type (FunTy _ arg res) cv
  = almost_devoid_co_var_of_type arg cv
  && almost_devoid_co_var_of_type res cv
almost_devoid_co_var_of_type (ForAllTy (Bndr v _) ty) cv
  = almost_devoid_co_var_of_type (varType v) cv
  && (v == cv || almost_devoid_co_var_of_type ty cv)
almost_devoid_co_var_of_type (CastTy ty co) cv
  = almost_devoid_co_var_of_type ty cv
  && almost_devoid_co_var_of_co co cv
almost_devoid_co_var_of_type (CoercionTy co) cv
  = almost_devoid_co_var_of_co co cv

almost_devoid_co_var_of_types :: [Type] -> CoVar -> Bool
almost_devoid_co_var_of_types [] _ = True
almost_devoid_co_var_of_types (ty:tys) cv
  = almost_devoid_co_var_of_type ty cv
  && almost_devoid_co_var_of_types tys cv

------------- Injective free vars -----------------

-- | Returns the free variables of a 'Type' that are in injective positions.
-- For example, if @F@ is a non-injective type family, then:
--
-- @
-- injectiveTyVarsOf( Either c (Maybe (a, F b c)) ) = {a,c}
-- @
--
-- If @'injectiveVarsOfType' ty = itvs@, then knowing @ty@ fixes @itvs@.
-- More formally, if
-- @a@ is in @'injectiveVarsOfType' ty@
-- and  @S1(ty) ~ S2(ty)@,
-- then @S1(a)  ~ S2(a)@,
-- where @S1@ and @S2@ are arbitrary substitutions.
--
-- See @Note [When does a tycon application need an explicit kind signature?]@.
injectiveVarsOfType :: Type -> FV
injectiveVarsOfType = go
  where
    go ty                 | Just ty' <- coreView ty
                          = go ty'
    go (TyVarTy v)        = unitFV v `unionFV` go (tyVarKind v)
    go (AppTy f a)        = go f `unionFV` go a
    go (FunTy _ ty1 ty2)  = go ty1 `unionFV` go ty2
    go (TyConApp tc tys)  =
      case tyConInjectivityInfo tc of
        NotInjective  -> emptyFV
        Injective inj -> mapUnionFV go $
                         filterByList (inj ++ repeat True) tys
                         -- Oversaturated arguments to a tycon are
                         -- always injective, hence the repeat True
    go (ForAllTy tvb ty) = tyCoFVsBndr tvb $ go ty
    go LitTy{}           = emptyFV
    go (CastTy ty _)     = go ty
    go CoercionTy{}      = emptyFV

------------- No free vars -----------------

-- | Returns True if this type has no free variables. Should be the same as
-- isEmptyVarSet . tyCoVarsOfType, but faster in the non-forall case.
noFreeVarsOfType :: Type -> Bool
noFreeVarsOfType (TyVarTy _)      = False
noFreeVarsOfType (AppTy t1 t2)    = noFreeVarsOfType t1 && noFreeVarsOfType t2
noFreeVarsOfType (TyConApp _ tys) = all noFreeVarsOfType tys
noFreeVarsOfType ty@(ForAllTy {}) = isEmptyVarSet (tyCoVarsOfType ty)
noFreeVarsOfType (FunTy _ t1 t2)  = noFreeVarsOfType t1 && noFreeVarsOfType t2
noFreeVarsOfType (LitTy _)        = True
noFreeVarsOfType (CastTy ty co)   = noFreeVarsOfType ty && noFreeVarsOfCo co
noFreeVarsOfType (CoercionTy co)  = noFreeVarsOfCo co

noFreeVarsOfMCo :: MCoercion -> Bool
noFreeVarsOfMCo MRefl    = True
noFreeVarsOfMCo (MCo co) = noFreeVarsOfCo co

noFreeVarsOfTypes :: [Type] -> Bool
noFreeVarsOfTypes = all noFreeVarsOfType

-- | Returns True if this coercion has no free variables. Should be the same as
-- isEmptyVarSet . tyCoVarsOfCo, but faster in the non-forall case.
noFreeVarsOfCo :: Coercion -> Bool
noFreeVarsOfCo (Refl ty)              = noFreeVarsOfType ty
noFreeVarsOfCo (GRefl _ ty co)        = noFreeVarsOfType ty && noFreeVarsOfMCo co
noFreeVarsOfCo (TyConAppCo _ _ args)  = all noFreeVarsOfCo args
noFreeVarsOfCo (AppCo c1 c2)          = noFreeVarsOfCo c1 && noFreeVarsOfCo c2
noFreeVarsOfCo co@(ForAllCo {})       = isEmptyVarSet (tyCoVarsOfCo co)
noFreeVarsOfCo (FunCo _ c1 c2)        = noFreeVarsOfCo c1 && noFreeVarsOfCo c2
noFreeVarsOfCo (CoVarCo _)            = False
noFreeVarsOfCo (HoleCo {})            = True    -- I'm unsure; probably never happens
noFreeVarsOfCo (AxiomInstCo _ _ args) = all noFreeVarsOfCo args
noFreeVarsOfCo (UnivCo p _ t1 t2)     = noFreeVarsOfProv p &&
                                        noFreeVarsOfType t1 &&
                                        noFreeVarsOfType t2
noFreeVarsOfCo (SymCo co)             = noFreeVarsOfCo co
noFreeVarsOfCo (TransCo co1 co2)      = noFreeVarsOfCo co1 && noFreeVarsOfCo co2
noFreeVarsOfCo (NthCo _ _ co)         = noFreeVarsOfCo co
noFreeVarsOfCo (LRCo _ co)            = noFreeVarsOfCo co
noFreeVarsOfCo (InstCo co1 co2)       = noFreeVarsOfCo co1 && noFreeVarsOfCo co2
noFreeVarsOfCo (KindCo co)            = noFreeVarsOfCo co
noFreeVarsOfCo (SubCo co)             = noFreeVarsOfCo co
noFreeVarsOfCo (AxiomRuleCo _ cs)     = all noFreeVarsOfCo cs

-- | Returns True if this UnivCoProv has no free variables. Should be the same as
-- isEmptyVarSet . tyCoVarsOfProv, but faster in the non-forall case.
noFreeVarsOfProv :: UnivCoProvenance -> Bool
noFreeVarsOfProv UnsafeCoerceProv    = True
noFreeVarsOfProv (PhantomProv co)    = noFreeVarsOfCo co
noFreeVarsOfProv (ProofIrrelProv co) = noFreeVarsOfCo co
noFreeVarsOfProv (PluginProv {})     = True


-- | Does a 'TyCon' (that is applied to some number of arguments) need to be
-- ascribed with an explicit kind signature to resolve ambiguity if rendered as
-- a source-syntax type?
-- (See @Note [When does a tycon application need an explicit kind signature?]@
-- for a full explanation of what this function checks for.)

-- Morally, this function ought to belong in TyCon.hs, not TyCoRep.hs, but
-- accomplishing this requires a fair deal of futzing aruond with .hs-boot
-- files.
tyConAppNeedsKindSig
  :: Bool  -- ^ Should specified binders count towards injective positions in
           --   the kind of the TyCon? (If you're using visible kind
           --   applications, then you want True here.
  -> TyCon
  -> Int   -- ^ The number of args the 'TyCon' is applied to.
  -> Bool  -- ^ Does @T t_1 ... t_n@ need a kind signature? (Where @n@ is the
           --   number of arguments)
tyConAppNeedsKindSig spec_inj_pos tc n_args
  | LT <- listLengthCmp tc_binders n_args
  = False
  | otherwise
  = let (dropped_binders, remaining_binders)
          = splitAt n_args tc_binders
        result_kind  = mkTyConKind remaining_binders tc_res_kind
        result_vars  = tyCoVarsOfType result_kind
        dropped_vars = fvVarSet $
                       mapUnionFV injective_vars_of_binder dropped_binders

    in not (subVarSet result_vars dropped_vars)
  where
    tc_binders  = tyConBinders tc
    tc_res_kind = tyConResKind tc

    -- Returns the variables that would be fixed by knowing a TyConBinder. See
    -- Note [When does a tycon application need an explicit kind signature?]
    -- for a more detailed explanation of what this function does.
    injective_vars_of_binder :: TyConBinder -> FV
    injective_vars_of_binder (Bndr tv vis) =
      case vis of
        AnonTCB VisArg -> injectiveVarsOfType (varType tv)
        NamedTCB argf  | source_of_injectivity argf
                       -> unitFV tv `unionFV` injectiveVarsOfType (varType tv)
        _              -> emptyFV

    source_of_injectivity Required  = True
    source_of_injectivity Specified = spec_inj_pos
    source_of_injectivity Inferred  = False

{-
Note [When does a tycon application need an explicit kind signature?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are a couple of places in GHC where we convert Core Types into forms that
more closely resemble user-written syntax. These include:

1. Template Haskell Type reification (see, for instance, TcSplice.reify_tc_app)
2. Converting Types to LHsTypes (in HsUtils.typeToLHsType, or in Haddock)

This conversion presents a challenge: how do we ensure that the resulting type
has enough kind information so as not to be ambiguous? To better motivate this
question, consider the following Core type:

  -- Foo :: Type -> Type
  type Foo = Proxy Type

There is nothing ambiguous about the RHS of Foo in Core. But if we were to,
say, reify it into a TH Type, then it's tempting to just drop the invisible
Type argument and simply return `Proxy`. But now we've lost crucial kind
information: we don't know if we're dealing with `Proxy Type` or `Proxy Bool`
or `Proxy Int` or something else! We've inadvertently introduced ambiguity.

Unlike in other situations in GHC, we can't just turn on
-fprint-explicit-kinds, as we need to produce something which has the same
structure as a source-syntax type. Moreover, we can't rely on visible kind
application, since the first kind argument to Proxy is inferred, not specified.
Our solution is to annotate certain tycons with their kinds whenever they
appear in applied form in order to resolve the ambiguity. For instance, we
would reify the RHS of Foo like so:

  type Foo = (Proxy :: Type -> Type)

We need to devise an algorithm that determines precisely which tycons need
these explicit kind signatures. We certainly don't want to annotate _every_
tycon with a kind signature, or else we might end up with horribly bloated
types like the following:

  (Either :: Type -> Type -> Type) (Int :: Type) (Char :: Type)

We only want to annotate tycons that absolutely require kind signatures in
order to resolve some sort of ambiguity, and nothing more.

Suppose we have a tycon application (T ty_1 ... ty_n). Why might this type
require a kind signature? It might require it when we need to fill in any of
T's omitted arguments. By "omitted argument", we mean one that is dropped when
reifying ty_1 ... ty_n. Sometimes, the omitted arguments are inferred and
specified arguments (e.g., TH reification in TcSplice), and sometimes the
omitted arguments are only the inferred ones (e.g., in HsUtils.typeToLHsType,
which reifies specified arguments through visible kind application).
Regardless, the key idea is that _some_ arguments are going to be omitted after
reification, and the only mechanism we have at our disposal for filling them in
is through explicit kind signatures.

What do we mean by "fill in"? Let's consider this small example:

  T :: forall {k}. Type -> (k -> Type) -> k

Moreover, we have this application of T:

  T @{j} Int aty

When we reify this type, we omit the inferred argument @{j}. Is it fixed by the
other (non-inferred) arguments? Yes! If we know the kind of (aty :: blah), then
we'll generate an equality constraint (kappa -> Type) and, assuming we can
solve it, that will fix `kappa`. (Here, `kappa` is the unification variable
that we instantiate `k` with.)

Therefore, for any application of a tycon T to some arguments, the Question We
Must Answer is:

* Given the first n arguments of T, do the kinds of the non-omitted arguments
  fill in the omitted arguments?

(This is still a bit hand-wavey, but we'll refine this question incrementally
as we explain more of the machinery underlying this process.)

Answering this question is precisely the role that the `injectiveVarsOfType`
and `injective_vars_of_binder` functions exist to serve. If an omitted argument
`a` appears in the set returned by `injectiveVarsOfType ty`, then knowing
`ty` determines (i.e., fills in) `a`. (More on `injective_vars_of_binder` in a
bit.)

More formally, if
`a` is in `injectiveVarsOfType ty`
and  S1(ty) ~ S2(ty),
then S1(a)  ~ S2(a),
where S1 and S2 are arbitrary substitutions.

For example, is `F` is a non-injective type family, then

  injectiveVarsOfType(Either c (Maybe (a, F b c))) = {a, c}

Now that we know what this function does, here is a second attempt at the
Question We Must Answer:

* Given the first n arguments of T (ty_1 ... ty_n), consider the binders
  of T that are instantiated by non-omitted arguments. Do the injective
  variables of these binders fill in the remainder of T's kind?

Alright, we're getting closer. Next, we need to clarify what the injective
variables of a tycon binder are. This the role that the
`injective_vars_of_binder` function serves. Here is what this function does for
each form of tycon binder:

* Anonymous binders are injective positions. For example, in the promoted data
  constructor '(:):

    '(:) :: forall a. a -> [a] -> [a]

  The second and third tyvar binders (of kinds `a` and `[a]`) are both
  anonymous, so if we had '(:) 'True '[], then the kinds of 'True and
  '[] would contribute to the kind of '(:) 'True '[]. Therefore,
  injective_vars_of_binder(_ :: a) = injectiveVarsOfType(a) = {a}.
  (Similarly, injective_vars_of_binder(_ :: [a]) = {a}.)
* Named binders:
  - Inferred binders are never injective positions. For example, in this data
    type:

      data Proxy a
      Proxy :: forall {k}. k -> Type

    If we had Proxy 'True, then the kind of 'True would not contribute to the
    kind of Proxy 'True. Therefore,
    injective_vars_of_binder(forall {k}. ...) = {}.
  - Required binders are injective positions. For example, in this data type:

      data Wurble k (a :: k) :: k
      Wurble :: forall k -> k -> k

  The first tyvar binder (of kind `forall k`) has required visibility, so if
  we had Wurble (Maybe a) Nothing, then the kind of Maybe a would
  contribute to the kind of Wurble (Maybe a) Nothing. Hence,
  injective_vars_of_binder(forall a -> ...) = {a}.
  - Specified binders /might/ be injective positions, depending on how you
    approach things. Continuing the '(:) example:

      '(:) :: forall a. a -> [a] -> [a]

    Normally, the (forall a. ...) tyvar binder wouldn't contribute to the kind
    of '(:) 'True '[], since it's not explicitly instantiated by the user. But
    if visible kind application is enabled, then this is possible, since the
    user can write '(:) @Bool 'True '[]. (In that case,
    injective_vars_of_binder(forall a. ...) = {a}.)

    There are some situations where using visible kind application is appropriate
    (e.g., HsUtils.typeToLHsType) and others where it is not (e.g., TH
    reification), so the `injective_vars_of_binder` function is parametrized by
    a Bool which decides if specified binders should be counted towards
    injective positions or not.

Now that we've defined injective_vars_of_binder, we can refine the Question We
Must Answer once more:

* Given the first n arguments of T (ty_1 ... ty_n), consider the binders
  of T that are instantiated by non-omitted arguments. For each such binder
  b_i, take the union of all injective_vars_of_binder(b_i). Is this set a
  superset of the free variables of the remainder of T's kind?

If the answer to this question is "no", then (T ty_1 ... ty_n) needs an
explicit kind signature, since T's kind has kind variables leftover that
aren't fixed by the non-omitted arguments.

One last sticking point: what does "the remainder of T's kind" mean? You might
be tempted to think that it corresponds to all of the arguments in the kind of
T that would normally be instantiated by omitted arguments. But this isn't
quite right, strictly speaking. Consider the following (silly) example:

  S :: forall {k}. Type -> Type

And suppose we have this application of S:

  S Int Bool

The Int argument would be omitted, and
injective_vars_of_binder(_ :: Type) = {}. This is not a superset of {k}, which
might suggest that (S Bool) needs an explicit kind signature. But
(S Bool :: Type) doesn't actually fix `k`! This is because the kind signature
only affects the /result/ of the application, not all of the individual
arguments. So adding a kind signature here won't make a difference. Therefore,
the fourth (and final) iteration of the Question We Must Answer is:

* Given the first n arguments of T (ty_1 ... ty_n), consider the binders
  of T that are instantiated by non-omitted arguments. For each such binder
  b_i, take the union of all injective_vars_of_binder(b_i). Is this set a
  superset of the free variables of the kind of (T ty_1 ... ty_n)?

Phew, that was a lot of work!

How can be sure that this is correct? That is, how can we be sure that in the
event that we leave off a kind annotation, that one could infer the kind of the
tycon application from its arguments? It's essentially a proof by induction: if
we can infer the kinds of every subtree of a type, then the whole tycon
application will have an inferrable kind--unless, of course, the remainder of
the tycon application's kind has uninstantiated kind variables.

What happens if T is oversaturated? That is, if T's kind has fewer than n
arguments, in the case that the concrete application instantiates a result
kind variable with an arrow kind? If we run out of arguments, we do not attach
a kind annotation. This should be a rare case, indeed. Here is an example:

   data T1 :: k1 -> k2 -> *
   data T2 :: k1 -> k2 -> *

   type family G (a :: k) :: k
   type instance G T1 = T2

   type instance F Char = (G T1 Bool :: (* -> *) -> *)   -- F from above

Here G's kind is (forall k. k -> k), and the desugared RHS of that last
instance of F is (G (* -> (* -> *) -> *) (T1 * (* -> *)) Bool). According to
the algorithm above, there are 3 arguments to G so we should peel off 3
arguments in G's kind. But G's kind has only two arguments. This is the
rare special case, and we choose not to annotate the application of G with
a kind signature. After all, we needn't do this, since that instance would
be reified as:

   type instance F Char = G (T1 :: * -> (* -> *) -> *) Bool

So the kind of G isn't ambiguous anymore due to the explicit kind annotation
on its argument. See #8953 and test th/T8953.
-}


{-
%************************************************************************
%*                                                                      *
         Well-scoped tyvars
*                                                                      *
************************************************************************

Note [ScopedSort]
~~~~~~~~~~~~~~~~~
Consider

  foo :: Proxy a -> Proxy (b :: k) -> Proxy (a :: k2) -> ()

This function type is implicitly generalised over [a, b, k, k2]. These
variables will be Specified; that is, they will be available for visible
type application. This is because they are written in the type signature
by the user.

However, we must ask: what order will they appear in? In cases without
dependency, this is easy: we just use the lexical left-to-right ordering
of first occurrence. With dependency, we cannot get off the hook so
easily.

We thus state:

 * These variables appear in the order as given by ScopedSort, where
   the input to ScopedSort is the left-to-right order of first occurrence.

Note that this applies only to *implicit* quantification, without a
`forall`. If the user writes a `forall`, then we just use the order given.

ScopedSort is defined thusly (as proposed in #15743):
  * Work left-to-right through the input list, with a cursor.
  * If variable v at the cursor is depended on by any earlier variable w,
    move v immediately before the leftmost such w.

INVARIANT: The prefix of variables before the cursor form a valid telescope.

Note that ScopedSort makes sense only after type inference is done and all
types/kinds are fully settled and zonked.

-}

-- | Do a topological sort on a list of tyvars,
--   so that binders occur before occurrences
-- E.g. given  [ a::k, k::*, b::k ]
-- it'll return a well-scoped list [ k::*, a::k, b::k ]
--
-- This is a deterministic sorting operation
-- (that is, doesn't depend on Uniques).
--
-- It is also meant to be stable: that is, variables should not
-- be reordered unnecessarily. This is specified in Note [ScopedSort]
-- See also Note [Ordering of implicit variables] in RnTypes

scopedSort :: [TyCoVar] -> [TyCoVar]
scopedSort = go [] []
  where
    go :: [TyCoVar] -- already sorted, in reverse order
       -> [TyCoVarSet] -- each set contains all the variables which must be placed
                       -- before the tv corresponding to the set; they are accumulations
                       -- of the fvs in the sorted tvs' kinds

                       -- This list is in 1-to-1 correspondence with the sorted tyvars
                       -- INVARIANT:
                       --   all (\tl -> all (`subVarSet` head tl) (tail tl)) (tails fv_list)
                       -- That is, each set in the list is a superset of all later sets.

       -> [TyCoVar] -- yet to be sorted
       -> [TyCoVar]
    go acc _fv_list [] = reverse acc
    go acc  fv_list (tv:tvs)
      = go acc' fv_list' tvs
      where
        (acc', fv_list') = insert tv acc fv_list

    insert :: TyCoVar       -- var to insert
           -> [TyCoVar]     -- sorted list, in reverse order
           -> [TyCoVarSet]  -- list of fvs, as above
           -> ([TyCoVar], [TyCoVarSet])   -- augmented lists
    insert tv []     []         = ([tv], [tyCoVarsOfType (tyVarKind tv)])
    insert tv (a:as) (fvs:fvss)
      | tv `elemVarSet` fvs
      , (as', fvss') <- insert tv as fvss
      = (a:as', fvs `unionVarSet` fv_tv : fvss')

      | otherwise
      = (tv:a:as, fvs `unionVarSet` fv_tv : fvs : fvss)
      where
        fv_tv = tyCoVarsOfType (tyVarKind tv)

       -- lists not in correspondence
    insert _ _ _ = panic "scopedSort"

-- | Get the free vars of a type in scoped order
tyCoVarsOfTypeWellScoped :: Type -> [TyVar]
tyCoVarsOfTypeWellScoped = scopedSort . tyCoVarsOfTypeList

-- | Get the free vars of types in scoped order
tyCoVarsOfTypesWellScoped :: [Type] -> [TyVar]
tyCoVarsOfTypesWellScoped = scopedSort . tyCoVarsOfTypesList




module GHC.Core.TyCo.FVs
  (     shallowTyCoVarsOfType, shallowTyCoVarsOfTypes,
        tyCoVarsOfType,        tyCoVarsOfTypes,
        tyCoVarsOfTypeDSet, tyCoVarsOfTypesDSet,

        tyCoFVsBndr, tyCoFVsVarBndr, tyCoFVsVarBndrs,
        tyCoFVsOfType, tyCoVarsOfTypeList,
        tyCoFVsOfTypes, tyCoVarsOfTypesList,
        deepTcvFolder,

        shallowTyCoVarsOfTyVarEnv, shallowTyCoVarsOfCoVarEnv,

        shallowTyCoVarsOfCo, shallowTyCoVarsOfCos,
        tyCoVarsOfCo, tyCoVarsOfCos, tyCoVarsOfMCo,
        coVarsOfType, coVarsOfTypes,
        coVarsOfCo, coVarsOfCos,
        tyCoVarsOfCoDSet,
        tyCoFVsOfCo, tyCoFVsOfCos,
        tyCoVarsOfCoList,

        almostDevoidCoVarOfCo,

        -- Injective free vars
        injectiveVarsOfType, injectiveVarsOfTypes,
        invisibleVarsOfType, invisibleVarsOfTypes,

        -- Any and No Free vars
        anyFreeVarsOfType, anyFreeVarsOfTypes, anyFreeVarsOfCo,
        noFreeVarsOfType, noFreeVarsOfTypes, noFreeVarsOfCo,

        -- * Well-scoped free variables
        scopedSort, tyCoVarsOfTypeWellScoped,
        tyCoVarsOfTypesWellScoped,

        -- * Closing over kinds
        closeOverKindsDSet, closeOverKindsList,
        closeOverKinds,

        -- * Raw materials
        Endo(..), runTyCoVars
  ) where

import GHC.Prelude

import {-# SOURCE #-} GHC.Core.Type (coreView, partitionInvisibleTypes)

import Data.Monoid as DM ( Endo(..), Any(..) )
import GHC.Core.TyCo.Rep
import GHC.Core.TyCon
import GHC.Types.Var
import GHC.Utils.FV

import GHC.Types.Unique.FM
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Utils.Misc
import GHC.Utils.Panic

{-
%************************************************************************
%*                                                                      *
                 Free variables of types and coercions
%*                                                                      *
%************************************************************************
-}

{- Note [Shallow and deep free variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Definitions

* Shallow free variables of a type: the variables
  affected by substitution. Specifically, the (TyVarTy tv)
  and (CoVar cv) that appear
    - In the type and coercions appearing in the type
    - In shallow free variables of the kind of a Forall binder
  but NOT in the kind of the /occurrences/ of a type variable.

* Deep free variables of a type: shallow free variables, plus
  the deep free variables of the kinds of those variables.
  That is,  deepFVs( t ) = closeOverKinds( shallowFVs( t ) )

Examples:

  Type                     Shallow     Deep
  ---------------------------------
  (a : (k:Type))           {a}        {a,k}
  forall (a:(k:Type)). a   {k}        {k}
  (a:k->Type) (b:k)        {a,b}      {a,b,k}
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
   determinism, and through its "interesting var" function, neither of which
   we need here, so they are a complete waste.

2. UnionVarSet version: instead of reusing the FV-based code, we simply used
   VarSets directly, trying to avoid the overhead of FV. E.g.:

   -- FV version:
   tyCoFVsOfType (AppTy fun arg)    a b c = (tyCoFVsOfType fun `unionFV` tyCoFVsOfType arg) a b c

   -- UnionVarSet version:
   tyCoVarsOfType (AppTy fun arg)    = (tyCoVarsOfType fun `unionVarSet` tyCoVarsOfType arg)

   This looks deceptively similar, but while FV internally builds a list- and
   set-generating function, the VarSet functions manipulate sets directly, and
   the latter performs a lot worse than the naive FV version.

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

And that's it. This works because a variable is either bound or free. If it is bound,
then we won't look at it at all. If it is free, then all the variables free in its
kind are free -- regardless of whether some local variable has the same Unique.
So if we're looking at a variable occurrence at all, then all variables in its
kind are free.
-}

{- *********************************************************************
*                                                                      *
          Endo for free variables
*                                                                      *
********************************************************************* -}

{- Note [Acumulating parameter free variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We can use foldType to build an accumulating-parameter version of a
free-var finder, thus:

    fvs :: Type -> TyCoVarSet
    fvs ty = appEndo (foldType folder ty) emptyVarSet

Recall that
    foldType :: TyCoFolder env a -> env -> Type -> a

    newtype Endo a = Endo (a -> a)   -- In Data.Monoid
    instance Monoid a => Monoid (Endo a) where
       (Endo f) `mappend` (Endo g) = Endo (f.g)

    appEndo :: Endo a -> a -> a
    appEndo (Endo f) x = f x

So `mappend` for Endos is just function composition.

It's very important that, after optimisation, we end up with
* an arity-three function
* that is strict in the accumulator

   fvs env (TyVarTy v) acc
      | v `elemVarSet` env = acc
      | v `elemVarSet` acc = acc
      | otherwise          = acc `extendVarSet` v
   fvs env (AppTy t1 t2)   = fvs env t1 (fvs env t2 acc)
   ...

The "strict in the accumulator" part is to ensure that in the
AppTy equation we don't build a thunk for (fvs env t2 acc).

The optimiser does do all this, but not very robustly. It depends
critially on the basic arity-2 function not being exported, so that
all its calls are visibly to three arguments. This analysis is
done by the Call Arity pass.

TL;DR: check this regularly!
-}

runTyCoVars :: Endo TyCoVarSet -> TyCoVarSet
{-# INLINE runTyCoVars #-}
runTyCoVars f = appEndo f emptyVarSet

{- *********************************************************************
*                                                                      *
          Deep free variables
          See Note [Shallow and deep free variables]
*                                                                      *
********************************************************************* -}

tyCoVarsOfType :: Type -> TyCoVarSet
tyCoVarsOfType ty = runTyCoVars (deep_ty ty)
-- Alternative:
--   tyCoVarsOfType ty = closeOverKinds (shallowTyCoVarsOfType ty)

tyCoVarsOfTypes :: [Type] -> TyCoVarSet
tyCoVarsOfTypes tys = runTyCoVars (deep_tys tys)
-- Alternative:
--   tyCoVarsOfTypes tys = closeOverKinds (shallowTyCoVarsOfTypes tys)

tyCoVarsOfCo :: Coercion -> TyCoVarSet
-- See Note [Free variables of Coercions]
tyCoVarsOfCo co = runTyCoVars (deep_co co)

tyCoVarsOfMCo :: MCoercion -> TyCoVarSet
tyCoVarsOfMCo MRefl    = emptyVarSet
tyCoVarsOfMCo (MCo co) = tyCoVarsOfCo co

tyCoVarsOfCos :: [Coercion] -> TyCoVarSet
tyCoVarsOfCos cos = runTyCoVars (deep_cos cos)

deep_ty  :: Type       -> Endo TyCoVarSet
deep_tys :: [Type]     -> Endo TyCoVarSet
deep_co  :: Coercion   -> Endo TyCoVarSet
deep_cos :: [Coercion] -> Endo TyCoVarSet
(deep_ty, deep_tys, deep_co, deep_cos) = foldTyCo deepTcvFolder emptyVarSet

deepTcvFolder :: TyCoFolder TyCoVarSet (Endo TyCoVarSet)
deepTcvFolder = TyCoFolder { tcf_view = noView
                           , tcf_tyvar = do_tcv, tcf_covar = do_tcv
                           , tcf_hole  = do_hole, tcf_tycobinder = do_bndr }
  where
    do_tcv is v = Endo do_it
      where
        do_it acc | v `elemVarSet` is  = acc
                  | v `elemVarSet` acc = acc
                  | otherwise          = appEndo (deep_ty (varType v)) $
                                         acc `extendVarSet` v

    do_bndr is tcv _ = extendVarSet is tcv
    do_hole is hole  = do_tcv is (coHoleCoVar hole)
                       -- See Note [CoercionHoles and coercion free variables]
                       -- in GHC.Core.TyCo.Rep

{- *********************************************************************
*                                                                      *
          Shallow free variables
          See Note [Shallow and deep free variables]
*                                                                      *
********************************************************************* -}


shallowTyCoVarsOfType :: Type -> TyCoVarSet
-- See Note [Free variables of types]
shallowTyCoVarsOfType ty = runTyCoVars (shallow_ty ty)

shallowTyCoVarsOfTypes :: [Type] -> TyCoVarSet
shallowTyCoVarsOfTypes tys = runTyCoVars (shallow_tys tys)

shallowTyCoVarsOfCo :: Coercion -> TyCoVarSet
shallowTyCoVarsOfCo co = runTyCoVars (shallow_co co)

shallowTyCoVarsOfCos :: [Coercion] -> TyCoVarSet
shallowTyCoVarsOfCos cos = runTyCoVars (shallow_cos cos)

-- | Returns free variables of types, including kind variables as
-- a non-deterministic set. For type synonyms it does /not/ expand the
-- synonym.
shallowTyCoVarsOfTyVarEnv :: TyVarEnv Type -> TyCoVarSet
-- See Note [Free variables of types]
shallowTyCoVarsOfTyVarEnv tys = shallowTyCoVarsOfTypes (nonDetEltsUFM tys)
  -- It's OK to use nonDetEltsUFM here because we immediately
  -- forget the ordering by returning a set

shallowTyCoVarsOfCoVarEnv :: CoVarEnv Coercion -> TyCoVarSet
shallowTyCoVarsOfCoVarEnv cos = shallowTyCoVarsOfCos (nonDetEltsUFM cos)
  -- It's OK to use nonDetEltsUFM here because we immediately
  -- forget the ordering by returning a set

shallow_ty  :: Type       -> Endo TyCoVarSet
shallow_tys :: [Type]     -> Endo TyCoVarSet
shallow_co  :: Coercion   -> Endo TyCoVarSet
shallow_cos :: [Coercion] -> Endo TyCoVarSet
(shallow_ty, shallow_tys, shallow_co, shallow_cos) = foldTyCo shallowTcvFolder emptyVarSet

shallowTcvFolder :: TyCoFolder TyCoVarSet (Endo TyCoVarSet)
shallowTcvFolder = TyCoFolder { tcf_view = noView
                              , tcf_tyvar = do_tcv, tcf_covar = do_tcv
                              , tcf_hole  = do_hole, tcf_tycobinder = do_bndr }
  where
    do_tcv is v = Endo do_it
      where
        do_it acc | v `elemVarSet` is  = acc
                  | v `elemVarSet` acc = acc
                  | otherwise          = acc `extendVarSet` v

    do_bndr is tcv _ = extendVarSet is tcv
    do_hole _ _  = mempty   -- Ignore coercion holes


{- *********************************************************************
*                                                                      *
          Free coercion variables
*                                                                      *
********************************************************************* -}


{- Note [Finding free coercion variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Here we are only interested in the free /coercion/ variables.
We can achieve this through a slightly different TyCo folder.

Notice that we look deeply, into kinds.

See #14880.
-}

-- See Note [Finding free coercion variables]
coVarsOfType  :: Type       -> CoVarSet
coVarsOfTypes :: [Type]     -> CoVarSet
coVarsOfCo    :: Coercion   -> CoVarSet
coVarsOfCos   :: [Coercion] -> CoVarSet

coVarsOfType  ty  = runTyCoVars (deep_cv_ty ty)
coVarsOfTypes tys = runTyCoVars (deep_cv_tys tys)
coVarsOfCo    co  = runTyCoVars (deep_cv_co co)
coVarsOfCos   cos = runTyCoVars (deep_cv_cos cos)

deep_cv_ty  :: Type       -> Endo CoVarSet
deep_cv_tys :: [Type]     -> Endo CoVarSet
deep_cv_co  :: Coercion   -> Endo CoVarSet
deep_cv_cos :: [Coercion] -> Endo CoVarSet
(deep_cv_ty, deep_cv_tys, deep_cv_co, deep_cv_cos) = foldTyCo deepCoVarFolder emptyVarSet

deepCoVarFolder :: TyCoFolder TyCoVarSet (Endo CoVarSet)
deepCoVarFolder = TyCoFolder { tcf_view = noView
                             , tcf_tyvar = do_tyvar, tcf_covar = do_covar
                             , tcf_hole  = do_hole, tcf_tycobinder = do_bndr }
  where
    do_tyvar _ _  = mempty
      -- This do_tyvar means we won't see any CoVars in this
      -- TyVar's kind.   This may be wrong; but it's the way it's
      -- always been.  And its awkward to change, because
      -- the tyvar won't end up in the accumulator, so
      -- we'd look repeatedly.  Blargh.

    do_covar is v = Endo do_it
      where
        do_it acc | v `elemVarSet` is  = acc
                  | v `elemVarSet` acc = acc
                  | otherwise          = appEndo (deep_cv_ty (varType v)) $
                                         acc `extendVarSet` v

    do_bndr is tcv _ = extendVarSet is tcv
    do_hole is hole  = do_covar is (coHoleCoVar hole)
                       -- See Note [CoercionHoles and coercion free variables]
                       -- in GHC.Core.TyCo.Rep

{- *********************************************************************
*                                                                      *
          Closing over kinds
*                                                                      *
********************************************************************* -}

------------- Closing over kinds -----------------

closeOverKinds :: TyCoVarSet -> TyCoVarSet
-- For each element of the input set,
-- add the deep free variables of its kind
closeOverKinds vs = nonDetStrictFoldVarSet do_one vs vs
  where
    do_one v acc = appEndo (deep_ty (varType v)) acc

{- --------------- Alternative version 1 (using FV) ------------
closeOverKinds = fvVarSet . closeOverKindsFV . nonDetEltsUniqSet
-}

{- ---------------- Alternative version 2 -------------

-- | Add the kind variables free in the kinds of the tyvars in the given set.
-- Returns a non-deterministic set.
closeOverKinds :: TyCoVarSet -> TyCoVarSet
closeOverKinds vs
   = go vs vs
  where
    go :: VarSet   -- Work list
       -> VarSet   -- Accumulator, always a superset of wl
       -> VarSet
    go wl acc
      | isEmptyVarSet wl = acc
      | otherwise        = go wl_kvs (acc `unionVarSet` wl_kvs)
      where
        k v inner_acc = ty_co_vars_of_type (varType v) acc inner_acc
        wl_kvs = nonDetFoldVarSet k emptyVarSet wl
        -- wl_kvs = union of shallow free vars of the kinds of wl
        --          but don't bother to collect vars in acc

-}

{- ---------------- Alternative version 3 -------------
-- | Add the kind variables free in the kinds of the tyvars in the given set.
-- Returns a non-deterministic set.
closeOverKinds :: TyVarSet -> TyVarSet
closeOverKinds vs = close_over_kinds vs emptyVarSet


close_over_kinds :: TyVarSet  -- Work list
                 -> TyVarSet  -- Accumulator
                 -> TyVarSet
-- Precondition: in any call (close_over_kinds wl acc)
--  for every tv in acc, the shallow kind-vars of tv
--  are either in the work list wl, or in acc
-- Postcondition: result is the deep free vars of (wl `union` acc)
close_over_kinds wl acc
  = nonDetFoldVarSet do_one acc wl
  where
    do_one :: Var -> TyVarSet -> TyVarSet
    -- (do_one v acc) adds v and its deep free-vars to acc
    do_one v acc | v `elemVarSet` acc
                 = acc
                 | otherwise
                 = close_over_kinds (shallowTyCoVarsOfType (varType v)) $
                   acc `extendVarSet` v
-}


{- *********************************************************************
*                                                                      *
          The FV versions return deterministic results
*                                                                      *
********************************************************************* -}

-- | Given a list of tyvars returns a deterministic FV computation that
-- returns the given tyvars with the kind variables free in the kinds of the
-- given tyvars.
closeOverKindsFV :: [TyVar] -> FV
closeOverKindsFV tvs =
  mapUnionFV (tyCoFVsOfType . tyVarKind) tvs `unionFV` mkFVs tvs

-- | Add the kind variables free in the kinds of the tyvars in the given set.
-- Returns a deterministically ordered list.
closeOverKindsList :: [TyVar] -> [TyVar]
closeOverKindsList tvs = fvVarList $ closeOverKindsFV tvs

-- | Add the kind variables free in the kinds of the tyvars in the given set.
-- Returns a deterministic set.
closeOverKindsDSet :: DTyVarSet -> DTyVarSet
closeOverKindsDSet = fvDVarSet . closeOverKindsFV . dVarSetElems

-- | `tyCoFVsOfType` that returns free variables of a type in a deterministic
-- set. For explanation of why using `VarSet` is not deterministic see
-- Note [Deterministic FV] in "GHC.Utils.FV".
tyCoVarsOfTypeDSet :: Type -> DTyCoVarSet
-- See Note [Free variables of types]
tyCoVarsOfTypeDSet ty = fvDVarSet $ tyCoFVsOfType ty

-- | `tyCoFVsOfType` that returns free variables of a type in deterministic
-- order. For explanation of why using `VarSet` is not deterministic see
-- Note [Deterministic FV] in "GHC.Utils.FV".
tyCoVarsOfTypeList :: Type -> [TyCoVar]
-- See Note [Free variables of types]
tyCoVarsOfTypeList ty = fvVarList $ tyCoFVsOfType ty

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
-- See Note [FV naming conventions] in "GHC.Utils.FV".
--
-- Eta-expanded because that makes it run faster (apparently)
-- See Note [FV eta expansion] in "GHC.Utils.FV" for explanation.
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
tyCoFVsOfType (FunTy _ w arg res)  f bound_vars acc = (tyCoFVsOfType w `unionFV` tyCoFVsOfType arg `unionFV` tyCoFVsOfType res) f bound_vars acc
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
tyCoFVsOfCo (FunCo _ w co1 co2)    fv_cand in_scope acc
  = (tyCoFVsOfCo co1 `unionFV` tyCoFVsOfCo co2 `unionFV` tyCoFVsOfCo w) fv_cand in_scope acc
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
tyCoFVsOfProv (PhantomProv co)    fv_cand in_scope acc = tyCoFVsOfCo co fv_cand in_scope acc
tyCoFVsOfProv (ProofIrrelProv co) fv_cand in_scope acc = tyCoFVsOfCo co fv_cand in_scope acc
tyCoFVsOfProv (PluginProv _)      fv_cand in_scope acc = emptyFV fv_cand in_scope acc
tyCoFVsOfProv (CorePrepProv _)    fv_cand in_scope acc = emptyFV fv_cand in_scope acc

tyCoFVsOfCos :: [Coercion] -> FV
tyCoFVsOfCos []       fv_cand in_scope acc = emptyFV fv_cand in_scope acc
tyCoFVsOfCos (co:cos) fv_cand in_scope acc = (tyCoFVsOfCo co `unionFV` tyCoFVsOfCos cos) fv_cand in_scope acc


----- Whether a covar is /Almost Devoid/ in a type or coercion ----

-- | Given a covar and a coercion, returns True if covar is almost devoid in
-- the coercion. That is, covar can only appear in Refl and GRefl.
-- See last wrinkle in Note [Unused coercion variable in ForAllCo] in "GHC.Core.Coercion"
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
almost_devoid_co_var_of_co (FunCo _ w co1 co2) cv
  = almost_devoid_co_var_of_co w cv
  && almost_devoid_co_var_of_co co1 cv
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
almost_devoid_co_var_of_prov (PluginProv _)   _ = True
almost_devoid_co_var_of_prov (CorePrepProv _) _ = True

almost_devoid_co_var_of_type :: Type -> CoVar -> Bool
almost_devoid_co_var_of_type (TyVarTy _) _ = True
almost_devoid_co_var_of_type (TyConApp _ tys) cv
  = almost_devoid_co_var_of_types tys cv
almost_devoid_co_var_of_type (LitTy {}) _ = True
almost_devoid_co_var_of_type (AppTy fun arg) cv
  = almost_devoid_co_var_of_type fun cv
  && almost_devoid_co_var_of_type arg cv
almost_devoid_co_var_of_type (FunTy _ w arg res) cv
  = almost_devoid_co_var_of_type w cv
  && almost_devoid_co_var_of_type arg cv
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



{- *********************************************************************
*                                                                      *
                 Injective free vars
*                                                                      *
********************************************************************* -}

-- | Returns the free variables of a 'Type' that are in injective positions.
-- Specifically, it finds the free variables while:
--
-- * Expanding type synonyms
--
-- * Ignoring the coercion in @(ty |> co)@
--
-- * Ignoring the non-injective fields of a 'TyConApp'
--
--
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
injectiveVarsOfType :: Bool   -- ^ Should we look under injective type families?
                              -- See Note [Coverage condition for injective type families]
                              -- in "GHC.Tc.Instance.Family".
                    -> Type -> FV
injectiveVarsOfType look_under_tfs = go
  where
    go ty                  | Just ty' <- coreView ty
                           = go ty'
    go (TyVarTy v)         = unitFV v `unionFV` go (tyVarKind v)
    go (AppTy f a)         = go f `unionFV` go a
    go (FunTy _ w ty1 ty2) = go w `unionFV` go ty1 `unionFV` go ty2
    go (TyConApp tc tys)   =
      case tyConInjectivityInfo tc of
        Injective inj
          |  look_under_tfs || not (isTypeFamilyTyCon tc)
          -> mapUnionFV go $
             filterByList (inj ++ repeat True) tys
                         -- Oversaturated arguments to a tycon are
                         -- always injective, hence the repeat True
        _ -> emptyFV
    go (ForAllTy (Bndr tv _) ty) = go (tyVarKind tv) `unionFV` delFV tv (go ty)
    go LitTy{}                   = emptyFV
    go (CastTy ty _)             = go ty
    go CoercionTy{}              = emptyFV

-- | Returns the free variables of a 'Type' that are in injective positions.
-- Specifically, it finds the free variables while:
--
-- * Expanding type synonyms
--
-- * Ignoring the coercion in @(ty |> co)@
--
-- * Ignoring the non-injective fields of a 'TyConApp'
--
-- See @Note [When does a tycon application need an explicit kind signature?]@.
injectiveVarsOfTypes :: Bool -- ^ look under injective type families?
                             -- See Note [Coverage condition for injective type families]
                             -- in "GHC.Tc.Instance.Family".
                     -> [Type] -> FV
injectiveVarsOfTypes look_under_tfs = mapUnionFV (injectiveVarsOfType look_under_tfs)


{- *********************************************************************
*                                                                      *
                 Invisible vars
*                                                                      *
********************************************************************* -}


-- | Returns the set of variables that are used invisibly anywhere within
-- the given type. A variable will be included even if it is used both visibly
-- and invisibly. An invisible use site includes:
--   * In the kind of a variable
--   * In the kind of a bound variable in a forall
--   * In a coercion
--   * In a Specified or Inferred argument to a function
-- See Note [VarBndrs, TyCoVarBinders, TyConBinders, and visibility] in "GHC.Core.TyCo.Rep"
invisibleVarsOfType :: Type -> FV
invisibleVarsOfType = go
  where
    go ty                 | Just ty' <- coreView ty
                          = go ty'
    go (TyVarTy v)        = go (tyVarKind v)
    go (AppTy f a)        = go f `unionFV` go a
    go (FunTy _ w ty1 ty2) = go w `unionFV` go ty1 `unionFV` go ty2
    go (TyConApp tc tys)  = tyCoFVsOfTypes invisibles `unionFV`
                            invisibleVarsOfTypes visibles
      where (invisibles, visibles) = partitionInvisibleTypes tc tys
    go (ForAllTy tvb ty)  = tyCoFVsBndr tvb $ go ty
    go LitTy{}            = emptyFV
    go (CastTy ty co)     = tyCoFVsOfCo co `unionFV` go ty
    go (CoercionTy co)    = tyCoFVsOfCo co

-- | Like 'invisibleVarsOfType', but for many types.
invisibleVarsOfTypes :: [Type] -> FV
invisibleVarsOfTypes = mapUnionFV invisibleVarsOfType


{- *********************************************************************
*                                                                      *
                 Any free vars
*                                                                      *
********************************************************************* -}

{-# INLINE afvFolder #-}   -- so that specialization to (const True) works
afvFolder :: (TyCoVar -> Bool) -> TyCoFolder TyCoVarSet DM.Any
afvFolder check_fv = TyCoFolder { tcf_view = noView
                                , tcf_tyvar = do_tcv, tcf_covar = do_tcv
                                , tcf_hole = do_hole, tcf_tycobinder = do_bndr }
  where
    do_tcv is tv = Any (not (tv `elemVarSet` is) && check_fv tv)
    do_hole _ _  = Any False    -- I'm unsure; probably never happens
    do_bndr is tv _ = is `extendVarSet` tv

anyFreeVarsOfType :: (TyCoVar -> Bool) -> Type -> Bool
anyFreeVarsOfType check_fv ty = DM.getAny (f ty)
  where (f, _, _, _) = foldTyCo (afvFolder check_fv) emptyVarSet

anyFreeVarsOfTypes :: (TyCoVar -> Bool) -> [Type] -> Bool
anyFreeVarsOfTypes check_fv tys = DM.getAny (f tys)
  where (_, f, _, _) = foldTyCo (afvFolder check_fv) emptyVarSet

anyFreeVarsOfCo :: (TyCoVar -> Bool) -> Coercion -> Bool
anyFreeVarsOfCo check_fv co = DM.getAny (f co)
  where (_, _, f, _) = foldTyCo (afvFolder check_fv) emptyVarSet

noFreeVarsOfType :: Type -> Bool
noFreeVarsOfType ty = not $ DM.getAny (f ty)
  where (f, _, _, _) = foldTyCo (afvFolder (const True)) emptyVarSet

noFreeVarsOfTypes :: [Type] -> Bool
noFreeVarsOfTypes tys = not $ DM.getAny (f tys)
  where (_, f, _, _) = foldTyCo (afvFolder (const True)) emptyVarSet

noFreeVarsOfCo :: Coercion -> Bool
noFreeVarsOfCo co = not $ DM.getAny (f co)
  where (_, _, f, _) = foldTyCo (afvFolder (const True)) emptyVarSet


{- *********************************************************************
*                                                                      *
                 scopedSort
*                                                                      *
********************************************************************* -}

{- Note [ScopedSort]
~~~~~~~~~~~~~~~~~~~~
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
-- See also Note [Ordering of implicit variables] in "GHC.Rename.HsType"

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

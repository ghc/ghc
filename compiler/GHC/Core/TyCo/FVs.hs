{-# LANGUAGE MultiWayIf, PatternSynonyms #-}

module GHC.Core.TyCo.FVs
  (     -- FVRes and friends
        FVRes( runFV, FVRes ),
        addBndrFVRes, addBndrsFVRes, addBndrSelectiveFVRes, addBndrsSelectiveFVRes,
        mapUnionFVRes, shallowUnitFVRes, deepUnitFVRes,
        BoundVars, VarSetFVRes, DVarSetFVRes, SelectiveFVRes,
        TyCoFVRes, DTyCoFVRes,
        runFVTop, runFVAcc, runTyCoVars, runTyCoVarsDSet,
        runFVSelective, runFVSelectiveList, runFVSelectiveSet,
        InterestingVarFun,

        -- Shallow
        shallowTyCoVarsOfType, shallowTyCoVarsOfTypes,
        shallowTyCoVarsOfCo, shallowTyCoVarsOfCos,
        shallowTyCoVarsOfTyVarEnv, shallowTyCoVarsOfCoVarEnv,

        -- Deep
        tyCoVarsOfType, tyCoVarsOfTypes, tyCoVarsOfTypesList,
        tyCoVarsOfThings,
        tyCoVarsOfCo, tyCoVarsOfCos, tyCoVarsOfMCo,
        deepTcvFolder, deepTypeFV, deepCoFV,

        -- Deep, deterministic
        tyCoVarsOfTypeDSet, tyCoVarsOfTypesDSet, tyCoVarsOfTypeList,
        tyCoVarsOfCoDSet, tyCoVarsOfCoList,
        tyCoVarsOfThingsDSet,
        detTyCoVarsOfType, detTyCoVarsOfTypes, detTyCoVarsOfCo,

        -- Selective
        someTyCoVarsOfType, someTyCoVarsOfTypes,

        -- CoVars only
        coVarsOfType, coVarsOfTypes,
        coVarsOfCo, coVarsOfCos,
        coVarsOfCoDSet, coVarsOfCosDSet,

        -- Shallow, deterministic, composable
        tyCoFVsOfType, tyCoFVsOfCo,

        -- Almost devoid
        almostDevoidCoVarOfCo,

        -- Injective free vars
        injectiveVarsOfType, injectiveVarsOfTypes, isInjectiveInType,
        invisibleVarsOfType, invisibleVarsOfTypes,

        -- Any and No Free vars
        anyFreeVarsOfType, anyFreeVarsOfTypes, anyFreeVarsOfCo,
        noFreeVarsOfType, noFreeVarsOfTypes, noFreeVarsOfCo,

        -- * Free type constructors
        tyConsOfType, tyConsOfTypes,

        -- * Free vars with visible/invisible separate
        visVarsOfTypes, visVarsOfType,

        -- * Occurrence-check expansion
        occCheckExpand,

        -- * Closing over kinds
        closeOverKindsDSet,
        closeOverKinds,
  ) where

import GHC.Prelude

import {-# SOURCE #-} GHC.Core.Type( partitionInvisibleTypes, coreView, rewriterView )

import GHC.Builtin.Types.Prim( funTyFlagTyCon )

import Data.Monoid as DM ( Any(..) )
import GHC.Core.TyCo.Rep
import GHC.Core.TyCon
import GHC.Core.Coercion.Axiom( CoAxiomRule(..), BuiltInFamRewrite(..), coAxiomTyCon )

import GHC.Types.Var
import GHC.Types.Unique.FM
import GHC.Types.Unique.Set

import GHC.Types.Var.Set
import GHC.Types.Var.Env

import GHC.Utils.Misc
import GHC.Utils.EndoOS

import GHC.Data.Pair
import GHC.Exts (oneShot)

import Data.Semigroup

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

Note [Free vars and synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When finding free variables we generally do not expand synonyms.  So given
   type T a = Int
the type (T [b]) will return `b` as a free variable, even though expanding the
synonym would get rid of it.  Expanding synonyms might lead to types that look
ill-scoped; an alternative we have not explored.

But see `occCheckExpand` in this module for a function that does, selectively,
expand synonyms to reduce free-var occurences.

Note [CoercionHoles and coercion free variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Generally, we do not treat a CoercionHole as a free variable of a coercion;
see `tyCoVarsOfType` and friends.

But there is an exception. When finding the free /coercion/ variables of a type,
in `coVarsOfType`, we /do/ treat a CoercionHole as a free variable.  Why?
The sole reason is in Note [Emitting the residual implication in simplifyInfer]
in GHC.Tc.Solver.  Yuk.  This is not pretty.
-}

{- *********************************************************************
*                                                                      *
          Endo for free variables
*                                                                      *
********************************************************************* -}

{- Note [Accumulating parameter free variables]
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
critically on the basic arity-2 function not being exported, so that
all its calls are visibly to three arguments. This analysis is
done by the Call Arity pass.

TL;DR: check this regularly!
-}


{- *********************************************************************
*                                                                      *
          Free-var result type
*                                                                      *
********************************************************************* -}

type InterestingVarFun = Var -> Bool

newtype FVRes env acc = FVRes' { runFV :: env -> acc }
  -- Caries an environment (typically empty, or a set of in-scope variables)
  -- and a composable accumulator

pattern FVRes :: (env -> acc) -> FVRes env acc
pattern FVRes f <- FVRes' f
      where
        FVRes f = FVRes' (oneShot f)
         -- oneShot: this is the core of the one-shot trick!
         -- Note [The one-shot state monad trick] in  GHC.Utils.Monad.

instance Semigroup a => Semigroup (FVRes env a) where
  f1 <>  f2 = FVRes (\env -> runFV f1 env <> runFV f2 env)

instance Monoid a => Monoid (FVRes env a) where
  mempty  = FVRes (\_ -> mempty)

addBndrFV :: (env -> env) -> FVRes env a -> FVRes env a
{-# INLINE addBndrFV #-}
addBndrFV upd f = FVRes (\bvs -> runFV f $! upd bvs)
    -- Strict application to avoid making a thunk

addBndrFVRes :: TyCoVar -> FVRes BoundVars a -> FVRes BoundVars a
addBndrFVRes tcv = addBndrFV (\bvs -> extendVarSet bvs tcv)

addBndrsFVRes :: [Var] -> FVRes BoundVars a -> FVRes BoundVars a
addBndrsFVRes tcvs = addBndrFV (\bvs -> extendVarSetList bvs tcvs)

addBndrSelectiveFVRes :: TyCoVar -> FVRes (f, BoundVars) a -> FVRes (f, BoundVars) a
addBndrSelectiveFVRes tcv
  = addBndrFV (\(f,bvs) -> let !bvs' = extendVarSet bvs tcv
                               -- Strict let to avoid thunks
                           in (f,bvs'))
addBndrsSelectiveFVRes :: [Var] -> FVRes (f, BoundVars) a -> FVRes (f, BoundVars) a
addBndrsSelectiveFVRes bs
  = addBndrFV (\(f,bvs) -> let !bvs' = extendVarSetList bvs bs
                               -- Strict let to avoid thunks
                           in (f,bvs'))

mapUnionFVRes :: (Foldable t, Monoid acc)
          => (a -> FVRes env acc) -> t a -> FVRes env acc
{-# INLINE mapUnionFVRes #-}
mapUnionFVRes f xs = foldr (mappend . f) mempty xs


type BoundVars = TyCoVarSet


type VarSetFVRes    = FVRes BoundVars (EndoOS TyCoVarSet)
type DVarSetFVRes   = FVRes BoundVars (EndoOS DTyCoVarSet)
type SelectiveFVRes = FVRes (InterestingVarFun, BoundVars) (EndoOS DVarSet)
-- VarSetFVRes:    collects a VarSet
-- DVarSetFVRes:   collects a DVarSet (deterministic)
-- SelectiveFVRes: selectively collects a DVarSet

type TyCoFVRes  = VarSetFVRes
type DTyCoFVRes = DVarSetFVRes

runFVTop :: FVRes BoundVars a -> a
{-# INLINE runFVTop #-}
runFVTop f = runFV f emptyVarSet

runFVAcc :: FVRes BoundVars (EndoOS a) -> a -> a
{-# INLINE runFVAcc #-}
runFVAcc f = runEndoOS (runFVTop f)

runTyCoVars :: TyCoFVRes -> TyCoVarSet
{-# INLINE runTyCoVars #-}
runTyCoVars f = runFVAcc f emptyVarSet

runTyCoVarsDSet :: DTyCoFVRes -> DTyCoVarSet
{-# INLINE runTyCoVarsDSet #-}
runTyCoVarsDSet f = runFVAcc f emptyDVarSet

runFVSelective :: InterestingVarFun -> SelectiveFVRes -> DVarSet
runFVSelective interesting f
  = runEndoOS (runFV f (interesting, emptyVarSet)) emptyDVarSet

runFVSelectiveList :: InterestingVarFun -> SelectiveFVRes -> [Var]
runFVSelectiveList interesting f = dVarSetElems (runFVSelective interesting f)

runFVSelectiveSet :: InterestingVarFun -> SelectiveFVRes -> VarSet
runFVSelectiveSet interesting f = dVarSetToVarSet (runFVSelective interesting f)


{- *********************************************************************
*                                                                      *
          Deep free variables
          See Note [Shallow and deep free variables]
*                                                                      *
********************************************************************* -}

tyCoVarsOfType :: Type -> TyCoVarSet
-- The "deep" TyCoVars of the the type
tyCoVarsOfType ty = runTyCoVars (deepTypeFV ty)
-- Alternative:
--   tyCoVarsOfType ty = closeOverKinds (shallowTyCoVarsOfType ty)

tyCoVarsOfTypes :: [Type] -> TyCoVarSet
-- The "deep" TyCoVars of the the type
tyCoVarsOfTypes tys = runTyCoVars (deepTypesFV tys)
-- Alternative:
--   tyCoVarsOfTypes tys = closeOverKinds (shallowTyCoVarsOfTypes tys)

tyCoVarsOfCo :: Coercion -> TyCoVarSet
-- The "deep" TyCoVars of the the coercion
-- See Note [Free variables of types]
tyCoVarsOfCo co = runTyCoVars (deepCoFV co)

tyCoVarsOfMCo :: MCoercion -> TyCoVarSet
tyCoVarsOfMCo MRefl    = emptyVarSet
tyCoVarsOfMCo (MCo co) = tyCoVarsOfCo co

tyCoVarsOfCos :: [Coercion] -> TyCoVarSet
tyCoVarsOfCos cos = runTyCoVars (deepCosFV cos)

tyCoVarsOfThings :: Foldable t => (a -> Type) -> t a -> TyCoVarSet
-- Works over a collection of things from which we can extract a type
-- See Note [Free variables of types]
tyCoVarsOfThings get_ty things
  = runTyCoVars $ mapUnionFVRes (deepTypeFV . get_ty) things

deepTypeFV  :: Type       -> TyCoFVRes
deepTypesFV :: [Type]     -> TyCoFVRes
deepCoFV    :: Coercion   -> TyCoFVRes
deepCosFV   :: [Coercion] -> TyCoFVRes
(deepTypeFV, deepTypesFV, deepCoFV, deepCosFV) = foldTyCo deepTcvFolder

deepTcvFolder :: TyCoFolder TyCoFVRes
-- It's important that we use a one-shot EndoOS, to ensure that all
-- the free-variable finders are eta-expanded.  Lacking the one-shot-ness
-- led to some big slow downs.  See Note [The one-shot state monad trick]
-- in GHC.Utils.Monad
deepTcvFolder = TyCoFolder { tcf_view = noView  -- See Note [Free vars and synonyms]
                           , tcf_tyvar = do_tcv, tcf_covar = do_tcv
                           , tcf_hole  = do_hole
                           , tcf_tycobinder = addBndrFVRes }
  where
    do_tcv :: TyVar -> TyCoFVRes
    do_tcv = deepUnitFVRes deepTypeFV

    do_hole :: CoercionHole -> TyCoFVRes
    do_hole hole = deepTypeFV (varType (coHoleCoVar hole))
                     -- We don't collect the CoercionHole itself, but we /do/
                     -- need to collect the free variables of its /kind/
                     -- See Note [CoercionHoles and coercion free variables]

deepUnitFVRes :: (Type -> TyCoFVRes) -> TyCoVar -> TyCoFVRes
-- Deal with a single TyCoVar
-- Takes a function to find free vars of the kind
deepUnitFVRes fvs_of_kind v
  = FVRes (\bvs -> EndoOS (do_it bvs))
  where
    do_it :: BoundVars -> TyCoVarSet -> TyCoVarSet
    do_it bvs acc | v `elemVarSet` bvs = acc
                  | v `elemVarSet` acc = acc
                  | otherwise          = runFVAcc (fvs_of_kind (varType v)) $
                                         acc `extendVarSet` v

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

shallow_ty  :: Type       -> TyCoFVRes
shallow_tys :: [Type]     -> TyCoFVRes
shallow_co  :: Coercion   -> TyCoFVRes
shallow_cos :: [Coercion] -> TyCoFVRes
(shallow_ty, shallow_tys, shallow_co, shallow_cos)
   = foldTyCo shallowTcvFolder

shallowTcvFolder :: TyCoFolder TyCoFVRes
shallowTcvFolder = TyCoFolder { tcf_view = noView  -- See Note [Free vars and synonyms]
                              , tcf_tyvar = do_tcv, tcf_covar = do_tcv
                              , tcf_hole  = do_hole
                              , tcf_tycobinder = addBndrFVRes }
  where
    do_tcv = shallowUnitFVRes
    do_hole _  = mempty   -- Ignore coercion holes

shallowUnitFVRes :: TyCoVar -> TyCoFVRes
shallowUnitFVRes v
  = FVRes (\bvs -> EndoOS (do_it bvs))
  where
    do_it bvs acc | v `elemVarSet` bvs = acc
                  | v `elemVarSet` acc = acc
                  | otherwise          = acc `extendVarSet` v


{- *********************************************************************
*                                                                      *
          Deterministic, deep free vars
*                                                                      *
********************************************************************* -}

-- | `tyCoVarsOfTypeDSet` that returns free variables of a type in a deterministic
-- set. For explanation of why using `VarSet` is not deterministic see
-- Note [Deterministic FV] in "GHC.Utils.FV".
tyCoVarsOfTypeDSet :: Type -> DTyCoVarSet
-- See Note [Free variables of types]
tyCoVarsOfTypeDSet ty = runTyCoVarsDSet (detTyCoVarsOfType ty)

-- | Returns free variables of types, including kind variables as
-- a deterministic set. For type synonyms it does /not/ expand the
-- synonym.
tyCoVarsOfTypesDSet :: [Type] -> DTyCoVarSet
-- See Note [Free variables of types]
tyCoVarsOfTypesDSet tys = runTyCoVarsDSet (detTyCoVarsOfTypes tys)

tyCoVarsOfThingsDSet :: Foldable t => (a -> Type) -> t a -> DTyCoVarSet
-- Works over a collection of things from which we can extract a type
-- See Note [Free variables of types]
tyCoVarsOfThingsDSet get_ty things
  = runTyCoVarsDSet (mapUnionFVRes (detTyCoVarsOfType . get_ty) things)

-- | `tyCoVarsOfTypeList` returns free variables of a type in deterministic
-- order. For explanation of why using `VarSet` is not deterministic see
-- Note [Deterministic FV] in "GHC.Utils.FV".
tyCoVarsOfTypeList :: Type -> [TyCoVar]
-- See Note [Free variables of types]
tyCoVarsOfTypeList ty = dVarSetElems $ tyCoVarsOfTypeDSet ty

tyCoVarsOfCoDSet :: Coercion -> DTyCoVarSet
-- See Note [Free variables of types]
tyCoVarsOfCoDSet ty = runTyCoVarsDSet (detTyCoVarsOfCo ty)

tyCoVarsOfCoList :: Coercion -> [TyCoVar]
-- See Note [Free variables of types]
tyCoVarsOfCoList ty = dVarSetElems $ tyCoVarsOfCoDSet ty

-- | Returns free variables of types, including kind variables as
-- a deterministically ordered list. For type synonyms it does /not/ expand the
-- synonym.
tyCoVarsOfTypesList :: [Type] -> [TyCoVar]
-- See Note [Free variables of types]
tyCoVarsOfTypesList tys = dVarSetElems $ tyCoVarsOfTypesDSet tys

detTyCoVarsOfType  :: Type   -> DTyCoFVRes
detTyCoVarsOfTypes :: [Type] -> DTyCoFVRes
detTyCoVarsOfCo    :: Coercion -> DTyCoFVRes
(detTyCoVarsOfType, detTyCoVarsOfTypes, detTyCoVarsOfCo, _)
  = foldTyCo deepDetTcvFolder

deepDetTcvFolder :: TyCoFolder DTyCoFVRes
-- This one returns a /deterministic/ list
-- See `deepTcvFolder` for the general pattern
deepDetTcvFolder
  = TyCoFolder { tcf_view = noView
               , tcf_tyvar = do_tcv, tcf_covar = do_tcv
               , tcf_hole  = do_hole
               , tcf_tycobinder = addBndrFVRes }
  where
    do_tcv = deepDetUnitFVRes detTyCoVarsOfType
    do_hole hole = detTyCoVarsOfType (varType (coHoleCoVar hole))

deepDetUnitFVRes :: (Type -> DTyCoFVRes) -> TyCoVar -> DTyCoFVRes
-- Deal with a single TyCoVar
-- Takes a function to find free vars of the kind
deepDetUnitFVRes fvs_of_kind v
  = FVRes (\bvs -> EndoOS (do_it bvs))
  where
    do_it :: BoundVars -> DTyCoVarSet -> DTyCoVarSet
    do_it bvs acc | v `elemVarSet` bvs  = acc
                  | v `elemDVarSet` acc = acc
                  | otherwise           = runFVAcc (fvs_of_kind (varType v)) $
                                          acc `extendDVarSet` v

{- *********************************************************************
*                                                                      *
          Selective, shallow, deterministic free vars
*                                                                      *
********************************************************************* -}

someTyCoVarsOfType :: (TyCoVar -> Bool) -> Type -> [TyCoVar]
someTyCoVarsOfType interesting
  = runFVSelectiveList interesting . tyCoFVsOfType

someTyCoVarsOfTypes :: (TyCoVar -> Bool) -> [Type] -> [TyCoVar]
someTyCoVarsOfTypes interesting
  = runFVSelectiveList interesting . mapUnionFVRes tyCoFVsOfType

tyCoFVsOfType :: Type -> SelectiveFVRes
tyCoFVsOfCo   :: Coercion -> SelectiveFVRes
-- Returns shallow free vars
-- See Note [Free variables of types]
(tyCoFVsOfType, _, tyCoFVsOfCo, _) = foldTyCo selectiveTcvFolder

selectiveTcvFolder :: TyCoFolder SelectiveFVRes
-- This one takes an `InterestingVarFun`, and returns shallow free vars
-- See `shallowTcvFolder` for the general pattern
selectiveTcvFolder
  = TyCoFolder { tcf_view  = noView  -- See Note [Free vars and synonyms]
               , tcf_tyvar = do_tcv, tcf_covar = do_tcv
               , tcf_hole  = do_hole
               , tcf_tycobinder = addBndrSelectiveFVRes }
  where
    do_tcv v = FVRes (\bvs -> EndoOS (do_it bvs))
      where
        do_it (is_interesting,bvs) acc
          | not (is_interesting v) = acc  -- The "selective" bit
          | v `elemVarSet` bvs     = acc
          | v `elemDVarSet` acc    = acc
          | otherwise              = acc `extendDVarSet` v

    do_hole hole = tyCoFVsOfType (varType (coHoleCoVar hole))


{- *********************************************************************
*                                                                      *
          Free coercion variables
*                                                                      *
********************************************************************* -}

{- Note [Finding free coercion variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Here we are only interested in the free /coercion/ variables.
We can achieve this through a slightly different TyCo folder.

Notice that

* We look deeply, into kinds.

* We /include/ CoercionHoles. Why?  Specifically because of #14584,
  Note [Emitting the residual implication in simplifyInfer] in GHC.Tc.Solver

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

type CoVarFVRes  = FVRes BoundVars (EndoOS CoVarSet)

deep_cv_ty  :: Type       -> CoVarFVRes
deep_cv_tys :: [Type]     -> CoVarFVRes
deep_cv_co  :: Coercion   -> CoVarFVRes
deep_cv_cos :: [Coercion] -> CoVarFVRes
(deep_cv_ty, deep_cv_tys, deep_cv_co, deep_cv_cos) = foldTyCo deepCoVarFolder

deepCoVarFolder :: TyCoFolder CoVarFVRes
deepCoVarFolder = TyCoFolder { tcf_view = noView
                             , tcf_tyvar = do_tyvar, tcf_covar = do_covar
                             , tcf_hole  = do_hole
                             , tcf_tycobinder = addBndrFVRes }
  where
    do_tyvar _  = mempty
      -- This do_tyvar means we won't see any CoVars in this
      -- TyVar's kind.   This may be wrong; but it's the way it's
      -- always been.  And its awkward to change, because
      -- the tyvar won't end up in the accumulator, so
      -- we'd look repeatedly.  Blargh.

    do_covar = deepUnitFVRes deep_cv_ty

    do_hole hole  = do_covar (coHoleCoVar hole)
      -- We /do/ treat a CoercionHole as a free variable
      -- See Note [CoercionHoles and coercion free variables]

-------------- Deterministic versions ------------------
type DCoVarFVRes  = FVRes BoundVars (EndoOS DCoVarSet)

coVarsOfCoDSet :: Coercion -> DCoVarSet
coVarsOfCoDSet co = runTyCoVarsDSet (det_co co)

coVarsOfCosDSet :: [Coercion] -> DCoVarSet
coVarsOfCosDSet cos = runTyCoVarsDSet (det_cos cos)

det_ty  :: Type       -> DCoVarFVRes
det_co  :: Coercion   -> DCoVarFVRes
det_cos :: [Coercion] -> DCoVarFVRes
(det_ty, _, det_co, det_cos) = foldTyCo deepDetCoVarFolder

deepDetCoVarFolder :: TyCoFolder DCoVarFVRes
-- Follows deepCoVarFolders, but returns a /deterministic/ set
deepDetCoVarFolder = TyCoFolder { tcf_view = noView
                                , tcf_tyvar = do_tyvar
                                , tcf_covar = do_covar
                                , tcf_hole  = do_hole
                                , tcf_tycobinder = addBndrFVRes }
  where
    do_tyvar _  = mempty

    do_covar :: CoVar -> DCoVarFVRes
    do_covar = deepDetUnitFVRes det_ty

    do_hole hole  = do_covar (coHoleCoVar hole)


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
    do_one v = runFVAcc (deepTypeFV (varType v))

-- | Add the kind variables free in the kinds of the tyvars in the given set.
-- Returns a deterministic set.
closeOverKindsDSet :: DTyVarSet -> DTyVarSet
closeOverKindsDSet vs = nonDetStrictFoldDVarSet do_one vs vs
  where
    do_one v = runFVAcc (detTyCoVarsOfType (varType v))

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




{-
%************************************************************************
%*                                                                      *
        almostDevoidCoVarOfCo
%*                                                                      *
%************************************************************************

-}

----- Whether a covar is /Almost Devoid/ in a type or coercion ----

-- | Given a covar and a coercion, returns True if covar is almost devoid in
-- the coercion. That is, covar can only appear in Refl and GRefl.
-- See (FC6) in Note [ForAllCo] in "GHC.Core.TyCo.Rep"
almostDevoidCoVarOfCo :: CoVar -> Coercion -> Bool
almostDevoidCoVarOfCo cv co =
  almost_devoid_co_var_of_co co cv

almost_devoid_co_var_of_mco :: MCoercion -> CoVar -> Bool
almost_devoid_co_var_of_mco MRefl    _  = True
almost_devoid_co_var_of_mco (MCo co) cv = almost_devoid_co_var_of_co co cv

almost_devoid_co_var_of_co :: Coercion -> CoVar -> Bool
almost_devoid_co_var_of_co (Refl {}) _ = True   -- covar is allowed in Refl and
almost_devoid_co_var_of_co (GRefl {}) _ = True  -- GRefl, so we don't look into
                                                -- the coercions
almost_devoid_co_var_of_co (TyConAppCo _ _ cos) cv
  = almost_devoid_co_var_of_cos cos cv
almost_devoid_co_var_of_co (AppCo co arg) cv
  = almost_devoid_co_var_of_co co cv
  && almost_devoid_co_var_of_co arg cv
almost_devoid_co_var_of_co (ForAllCo { fco_tcv = v, fco_kind = kind_co, fco_body = co }) cv
  = almost_devoid_co_var_of_mco kind_co cv
  && (v == cv || almost_devoid_co_var_of_co co cv)
almost_devoid_co_var_of_co (FunCo { fco_mult = w, fco_arg = co1, fco_res = co2 }) cv
  =  almost_devoid_co_var_of_co w   cv
  && almost_devoid_co_var_of_co co1 cv
  && almost_devoid_co_var_of_co co2 cv
almost_devoid_co_var_of_co (CoVarCo v) cv = v /= cv
almost_devoid_co_var_of_co (HoleCo h)  cv = (coHoleCoVar h) /= cv
almost_devoid_co_var_of_co (AxiomCo _ cs) cv
  = almost_devoid_co_var_of_cos cs cv
almost_devoid_co_var_of_co (UnivCo { uco_lty = t1, uco_rty = t2, uco_deps = deps }) cv
  =  almost_devoid_co_var_of_cos deps cv
  && almost_devoid_co_var_of_type t1 cv
  && almost_devoid_co_var_of_type t2 cv
almost_devoid_co_var_of_co (SymCo co) cv
  = almost_devoid_co_var_of_co co cv
almost_devoid_co_var_of_co (TransCo co1 co2) cv
  = almost_devoid_co_var_of_co co1 cv
  && almost_devoid_co_var_of_co co2 cv
almost_devoid_co_var_of_co (SelCo _ co) cv
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

almost_devoid_co_var_of_cos :: [Coercion] -> CoVar -> Bool
almost_devoid_co_var_of_cos [] _ = True
almost_devoid_co_var_of_cos (co:cos) cv
  = almost_devoid_co_var_of_co co cv
  && almost_devoid_co_var_of_cos cos cv

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



{-
%************************************************************************
%*                                                                      *
        Free tyvars, but with visible/invisible info
%*                                                                      *
%************************************************************************

-}
-- | Retrieve the free variables in this type, splitting them based
-- on whether they are used visibly or invisibly. Invisible ones come
-- first.
visVarsOfType :: Type -> Pair TyCoVarSet
visVarsOfType orig_ty = Pair invis_vars vis_vars
  where
    Pair invis_vars1 vis_vars = go orig_ty
    invis_vars = invis_vars1 `minusVarSet` vis_vars

    go (TyVarTy tv)      = Pair (tyCoVarsOfType $ tyVarKind tv) (unitVarSet tv)
    go (AppTy t1 t2)     = go t1 `mappend` go t2
    go (TyConApp tc tys) = go_tc tc tys
    go (FunTy _ w t1 t2) = go w `mappend` go t1 `mappend` go t2
    go (ForAllTy (Bndr tv _) ty)
      = ((`delVarSet` tv) <$> go ty) `mappend`
        (invisible (tyCoVarsOfType $ varType tv))
    go (LitTy {}) = mempty
    go (CastTy ty co) = go ty `mappend` invisible (tyCoVarsOfCo co)
    go (CoercionTy co) = invisible $ tyCoVarsOfCo co

    invisible vs = Pair vs emptyVarSet

    go_tc tc tys = let (invis, vis) = partitionInvisibleTypes tc tys in
                   invisible (tyCoVarsOfTypes invis) `mappend` foldMap go vis

visVarsOfTypes :: [Type] -> Pair TyCoVarSet
visVarsOfTypes = foldMap visVarsOfType


{- *********************************************************************
*                                                                      *
                 Injective free vars
*                                                                      *
********************************************************************* -}

isInjectiveInType :: TyVar -> Type -> Bool
-- True <=> tv /definitely/ appears injectively in ty
-- A bit more efficient that (tv `elemVarSet` injectiveTyVarsOfType ty)
-- Ignore occurrence in coercions, and even in injective positions of
-- type families.
isInjectiveInType tv ty
  = go ty
  where
    go ty | Just ty' <- rewriterView ty = go ty'
    go (TyVarTy tv')                    = tv' == tv
    go (AppTy f a)                      = go f || go a
    go (FunTy _ w ty1 ty2)              = go w || go ty1 || go ty2
    go (TyConApp tc tys)                = go_tc tc tys
    go (ForAllTy (Bndr tv' _) ty)       = go (tyVarKind tv')
                                          || (tv /= tv' && go ty)
    go LitTy{}                          = False
    go (CastTy ty _)                    = go ty
    go CoercionTy{}                     = False

    go_tc tc tys | isTypeFamilyTyCon tc = False
                 | otherwise            = any go tys

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
                    -> Type -> VarSet
injectiveVarsOfType look_under_tfs ty = runTyCoVars (inj_vars_of_type look_under_tfs ty)

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
                     -> [Type] -> VarSet
injectiveVarsOfTypes look_under_tfs tys
  = runTyCoVars $ mapUnionFVRes (inj_vars_of_type look_under_tfs) tys

inj_vars_of_type :: Bool -> Type -> TyCoFVRes
inj_vars_of_type look_under_tfs = go
  where
    go ty | Just ty' <- rewriterView ty = go ty'
    go (TyVarTy v)                      = deepUnitFVRes go v
    go (AppTy f a)                      = go f `mappend` go a
    go (FunTy _ w ty1 ty2)              = go w `mappend` go ty1 `mappend` go ty2
    go (TyConApp tc tys)                = go_tc tc tys
    go (ForAllTy (Bndr tv _) ty)        = go (tyVarKind tv) `mappend`
                                          addBndrFVRes tv (go ty)
    go LitTy{}                          = mempty
    go (CastTy ty _)                    = go ty
    go CoercionTy{}                     = mempty

    go_tc tc tys
      | isTypeFamilyTyCon tc
      = if | look_under_tfs
           , Injective flags <- tyConInjectivityInfo tc
           -> mapUnionFVRes go $
              filterByList (flags ++ repeat True) tys
                         -- Oversaturated arguments to a tycon are
                         -- always injective, hence the repeat True
           | otherwise   -- No injectivity info for this type family
           -> mempty

      | otherwise        -- Data type, injective in all positions
      = mapUnionFVRes go tys



{- *********************************************************************
*                                                                      *
                 Invisible vars
*                                                                      *
********************************************************************* -}


-- | Returns the set of variables that are used invisibly anywhere within
-- the given type. A variable will be included even if it is used both visibly
-- and invisibly. An "invisible" use site includes:
--   * In the kind of a variable
--   * In the kind of a bound variable in a forall
--   * In a coercion
--   * In a Specified or Inferred argument to a function
-- See Note [VarBndrs, ForAllTyBinders, TyConBinders, and visibility] in "GHC.Core.TyCo.Rep"
invisibleVarsOfType :: Type -> VarSet
invisibleVarsOfType = go
  where
    go ty                  | Just ty' <- coreView ty
                           = go ty'
    go (TyVarTy v)         = go (tyVarKind v)
    go (AppTy f a)         = go f `unionVarSet` go a
    go (FunTy _ w ty1 ty2) = go ty1 `unionVarSet` go w `unionVarSet` go ty2
                             -- As per #23764, order is: arg, mult, res.
    go (TyConApp tc tys)   = tyCoVarsOfTypes invisibles `unionVarSet`
                             invisibleVarsOfTypes visibles
      where (invisibles, visibles) = partitionInvisibleTypes tc tys
    go (ForAllTy (Bndr var _) ty)
      = tyCoVarsOfType (varType var) `unionVarSet` (go ty `delVarSet` var)
    go LitTy{}             = emptyVarSet
    go (CastTy ty co)      = tyCoVarsOfCo co `unionVarSet` go ty
    go (CoercionTy co)     = tyCoVarsOfCo co

-- | Like 'invisibleVarsOfType', but for many types.
invisibleVarsOfTypes :: [Type] -> VarSet
invisibleVarsOfTypes = foldr (unionVarSet . invisibleVarsOfType) emptyVarSet


{- *********************************************************************
*                                                                      *
                 Any free vars
*                                                                      *
********************************************************************* -}

{-# INLINE afvFolder #-}   -- so that specialization to (const True) works
afvFolder :: (TyCoVar -> Bool) -> TyCoFolder (FVRes TyCoVarSet DM.Any)
-- 'afvFolder' is short for "any-free-var folder", good for checking
-- if any free var of a type satisfies a predicate `check_fv`
afvFolder check_fv = TyCoFolder { tcf_view = noView  -- See Note [Free vars and synonyms]
                                , tcf_tyvar = do_tcv, tcf_covar = do_tcv
                                , tcf_hole = do_hole
                                , tcf_tycobinder = addBndrFVRes }
  where
    do_tcv tv = FVRes $ \ bvs ->
                Any (not (tv `elemVarSet` bvs) && check_fv tv)
    do_hole _ = mempty    -- I'm unsure; probably never happens

anyFreeVarsOfType :: (TyCoVar -> Bool) -> Type -> Bool
anyFreeVarsOfType check_fv ty = DM.getAny (runFVTop (f ty))
  where (f, _, _, _) = foldTyCo (afvFolder check_fv)

anyFreeVarsOfTypes :: (TyCoVar -> Bool) -> [Type] -> Bool
anyFreeVarsOfTypes check_fv tys = DM.getAny (runFVTop (f tys))
  where (_, f, _, _) = foldTyCo (afvFolder check_fv)

anyFreeVarsOfCo :: (TyCoVar -> Bool) -> Coercion -> Bool
anyFreeVarsOfCo check_fv co = DM.getAny (runFVTop (f co))
  where (_, _, f, _) = foldTyCo (afvFolder check_fv)

noFreeVarsOfType :: Type -> Bool
noFreeVarsOfType ty = not $ DM.getAny (runFVTop (f ty))
  where (f, _, _, _) = foldTyCo (afvFolder (const True))

noFreeVarsOfTypes :: [Type] -> Bool
noFreeVarsOfTypes tys = not $ DM.getAny (runFVTop (f tys))
  where (_, f, _, _) = foldTyCo (afvFolder (const True))

noFreeVarsOfCo :: Coercion -> Bool
noFreeVarsOfCo co = not $ DM.getAny (runFVTop (f co))
  where (_, _, f, _) = foldTyCo (afvFolder (const True))


{-
************************************************************************
*                                                                      *
            Free type constructors
*                                                                      *
************************************************************************
-}

{- Note [tyConsOfType]
~~~~~~~~~~~~~~~~~~~~~~
It is slightly odd to find the TyCons of a type.  Especially since, via a type
family reduction or axiom, a type that doesn't mention T might start to mention T.

This function is used in only three places:
* In GHC.Tc.Validity.validDerivPred, when identifying "exotic" predicates.
* In GHC.Tc.Errors.Ppr.pprTcSolverReportMsg, when trying to print a helpful
  error about overlapping instances
* In utils/dump-decls/Main.hs, an ill-documented module.

None seem critical. Currently tyConsOfType looks inside coercions, but perhaps
it doesn't even need to do that.
-}

-- | All type constructors occurring in the type; looking through type
--   synonyms, but not newtypes.
--  When it finds a Class, it returns the class TyCon.
tyConsOfType :: Type -> UniqSet TyCon
tyConsOfType ty
  = go ty
  where
     go :: Type -> UniqSet TyCon  -- The UniqSet does duplicate elim
     go ty | Just ty' <- coreView ty = go ty'
     go (TyVarTy {})                = emptyUniqSet
     go (LitTy {})                  = emptyUniqSet
     go (TyConApp tc tys)           = go_tc tc `unionUniqSets` tyConsOfTypes tys
     go (AppTy a b)                 = go a `unionUniqSets` go b
     go (FunTy af w a b)            = go w `unionUniqSets`
                                      go a `unionUniqSets` go b
                                      `unionUniqSets` go_tc (funTyFlagTyCon af)
     go (ForAllTy (Bndr tv _) ty)   = go ty `unionUniqSets` go (varType tv)
     go (CastTy ty co)              = go ty `unionUniqSets` go_co co
     go (CoercionTy co)             = go_co co

     go_co (Refl ty)               = go ty
     go_co (GRefl _ ty mco)        = go ty `unionUniqSets` go_mco mco
     go_co (TyConAppCo _ tc args)  = go_tc tc `unionUniqSets` go_cos args
     go_co (AppCo co arg)          = go_co co `unionUniqSets` go_co arg
     go_co (ForAllCo { fco_kind = kind_co, fco_body = co })
                                   = go_mco kind_co `unionUniqSets` go_co co
     go_co (FunCo { fco_mult = m, fco_arg = a, fco_res = r })
                                   = go_co m `unionUniqSets` go_co a `unionUniqSets` go_co r
     go_co (AxiomCo ax args)       = go_ax ax `unionUniqSets` go_cos args
     go_co (UnivCo { uco_lty = t1, uco_rty = t2, uco_deps = cos })
                                   = go t1 `unionUniqSets` go t2 `unionUniqSets` go_cos cos
     go_co (CoVarCo {})            = emptyUniqSet
     go_co (HoleCo {})             = emptyUniqSet
     go_co (SymCo co)              = go_co co
     go_co (TransCo co1 co2)       = go_co co1 `unionUniqSets` go_co co2
     go_co (SelCo _ co)            = go_co co
     go_co (LRCo _ co)             = go_co co
     go_co (InstCo co arg)         = go_co co `unionUniqSets` go_co arg
     go_co (KindCo co)             = go_co co
     go_co (SubCo co)              = go_co co

     go_mco MRefl    = emptyUniqSet
     go_mco (MCo co) = go_co co

     go_cos cos   = foldr (unionUniqSets . go_co)  emptyUniqSet cos

     go_tc tc = unitUniqSet tc

     go_ax (UnbranchedAxiom ax) = go_tc $ coAxiomTyCon ax
     go_ax (BranchedAxiom ax _) = go_tc $ coAxiomTyCon ax
     go_ax (BuiltInFamRew  bif) = go_tc $ bifrw_fam_tc bif
     go_ax (BuiltInFamInj {})   = emptyUniqSet  -- A free-floating axiom

tyConsOfTypes :: [Type] -> UniqSet TyCon
tyConsOfTypes tys = foldr (unionUniqSets . tyConsOfType) emptyUniqSet tys

{- **********************************************************************
*                                                                       *
           Occurs check expansion
%*                                                                      *
%********************************************************************* -}

{- Note [Occurs check expansion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(occurCheckExpand tv xi) expands synonyms in xi just enough to get rid
of occurrences of tv outside type function arguments, if that is
possible; otherwise, it returns Nothing.

For example, suppose we have
  type F a b = [a]
Then
  occCheckExpand b (F Int b) = Just [Int]
but
  occCheckExpand a (F a Int) = Nothing

We don't promise to do the absolute minimum amount of expanding
necessary, but we try not to do expansions we don't need to.  We
prefer doing inner expansions first.  For example,
  type F a b = (a, Int, a, [a])
  type G b   = Char
We have
  occCheckExpand b (F (G b)) = Just (F Char)
even though we could also expand F to get rid of b.

Note [Occurrence checking: look inside kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we are considering unifying
   (alpha :: *)  ~  Int -> (beta :: alpha -> alpha)
This may be an error (what is that alpha doing inside beta's kind?),
but we must not make the mistake of actually unifying or we'll
build an infinite data structure.  So when looking for occurrences
of alpha in the rhs, we must look in the kinds of type variables
that occur there.

occCheckExpand tries to expand type synonyms to remove
unnecessary occurrences of a variable, and thereby get past an
occurs-check failure.  This is good; but
     we can't do it in the /kind/ of a variable /occurrence/

For example #18451 built an infinite type:
    type Const a b = a
    data SameKind :: k -> k -> Type
    type T (k :: Const Type a) = forall (b :: k). SameKind a b

We have
  b :: k
  k :: Const Type a
  a :: k   (must be same as b)

So if we aren't careful, a's kind mentions a, which is bad.
And expanding an /occurrence/ of 'a' doesn't help, because the
/binding site/ is the master copy and all the occurrences should
match it.

Here's a related example:
   f :: forall a b (c :: Const Type b). Proxy '[a, c]

The list means that 'a' gets the same kind as 'c'; but that
kind mentions 'b', so the binders are out of order.

Bottom line: in occCheckExpand, do not expand inside the kinds
of occurrences.  See bad_var_occ in occCheckExpand.  And
see #18451 for more debate.
-}

occCheckExpand :: [Var] -> Type -> Maybe Type
-- See Note [Occurs check expansion]
-- We may have needed to do some type synonym unfolding in order to
-- get rid of the variable (or forall), so we also return the unfolded
-- version of the type, which is guaranteed to be syntactically free
-- of the given type variable.  If the type is already syntactically
-- free of the variable, then the same type is returned.
occCheckExpand vs_to_avoid ty
  | null vs_to_avoid  -- Efficient shortcut
  = Just ty           -- Can happen, eg. GHC.Core.Utils.mkSingleAltCase

  | otherwise
  = go (mkVarSet vs_to_avoid, emptyVarEnv) ty
  where
    go :: (VarSet, VarEnv TyCoVar) -> Type -> Maybe Type
          -- The VarSet is the set of variables we are trying to avoid
          -- The VarEnv carries mappings necessary
          -- because of kind expansion
    go (as, env) ty@(TyVarTy tv)
      | Just tv' <- lookupVarEnv env tv = return (mkTyVarTy tv')
      | bad_var_occ as tv               = Nothing
      | otherwise                       = return ty

    go _   ty@(LitTy {}) = return ty
    go cxt (AppTy ty1 ty2) = do { ty1' <- go cxt ty1
                                ; ty2' <- go cxt ty2
                                ; return (AppTy ty1' ty2') }
    go cxt ty@(FunTy _ w ty1 ty2)
       = do { w'   <- go cxt w
            ; ty1' <- go cxt ty1
            ; ty2' <- go cxt ty2
            ; return (ty { ft_mult = w', ft_arg = ty1', ft_res = ty2' }) }
    go cxt@(as, env) (ForAllTy (Bndr tv vis) body_ty)
       = do { ki' <- go cxt (varType tv)
            ; let tv'  = setVarType tv ki'
                  env' = extendVarEnv env tv tv'
                  as'  = as `delVarSet` tv
            ; body' <- go (as', env') body_ty
            ; return (ForAllTy (Bndr tv' vis) body') }

    -- For a type constructor application, first try expanding away the
    -- offending variable from the arguments.  If that doesn't work, next
    -- see if the type constructor is a type synonym, and if so, expand
    -- it and try again.
    go cxt ty@(TyConApp tc tys)
      = case mapM (go cxt) tys of
          Just tys' -> return (TyConApp tc tys')
          Nothing | Just ty' <- coreView ty -> go cxt ty'
                  | otherwise               -> Nothing
                      -- Failing that, try to expand a synonym

    go cxt (CastTy ty co) =  do { ty' <- go cxt ty
                                ; co' <- go_co cxt co
                                ; return (CastTy ty' co') }
    go cxt (CoercionTy co) = do { co' <- go_co cxt co
                                ; return (CoercionTy co') }

    ------------------
    bad_var_occ :: VarSet -> Var -> Bool
    -- Works for TyVar and CoVar
    -- See Note [Occurrence checking: look inside kinds]
    bad_var_occ vs_to_avoid v
       =  v                          `elemVarSet`       vs_to_avoid
       || tyCoVarsOfType (varType v) `intersectsVarSet` vs_to_avoid

    ------------------
    go_mco _   MRefl = return MRefl
    go_mco ctx (MCo co) = MCo <$> go_co ctx co

    ------------------
    go_co cxt (Refl ty)                 = do { ty' <- go cxt ty
                                             ; return (Refl ty') }
    go_co cxt (GRefl r ty mco)          = do { mco' <- go_mco cxt mco
                                             ; ty' <- go cxt ty
                                             ; return (GRefl r ty' mco') }
      -- Note: Coercions do not contain type synonyms
    go_co cxt (TyConAppCo r tc args)    = do { args' <- mapM (go_co cxt) args
                                             ; return (TyConAppCo r tc args') }
    go_co cxt (AppCo co arg)            = do { co' <- go_co cxt co
                                             ; arg' <- go_co cxt arg
                                             ; return (AppCo co' arg') }
    go_co cxt (SymCo co)                = do { co' <- go_co cxt co
                                             ; return (SymCo co') }
    go_co cxt (TransCo co1 co2)         = do { co1' <- go_co cxt co1
                                             ; co2' <- go_co cxt co2
                                             ; return (TransCo co1' co2') }
    go_co cxt (SelCo n co)              = do { co' <- go_co cxt co
                                             ; return (SelCo n co') }
    go_co cxt (LRCo lr co)              = do { co' <- go_co cxt co
                                             ; return (LRCo lr co') }
    go_co cxt (InstCo co arg)           = do { co' <- go_co cxt co
                                             ; arg' <- go_co cxt arg
                                             ; return (InstCo co' arg') }
    go_co cxt (KindCo co)               = do { co' <- go_co cxt co
                                             ; return (KindCo co') }
    go_co cxt (SubCo co)                = do { co' <- go_co cxt co
                                             ; return (SubCo co') }

    go_co cxt@(as, env) co@(ForAllCo { fco_tcv = tcv, fco_kind = kind_co, fco_body = body_co })
      = do { ki' <- go cxt (varType tcv)
           ; let tcv' = setVarType tcv ki'
                 env' = extendVarEnv env tcv tcv'
                 as'  = as `delVarSet` tcv
           ; kind_co' <- go_mco cxt kind_co
           ; body' <- go_co (as', env') body_co
           ; return (co { fco_tcv = tcv', fco_kind = kind_co', fco_body = body' }) }

    go_co cxt co@(FunCo { fco_mult = w, fco_arg = co1 ,fco_res = co2 })
      = do { co1' <- go_co cxt co1
           ; co2' <- go_co cxt co2
           ; w' <- go_co cxt w
           ; return (co { fco_mult = w', fco_arg = co1', fco_res = co2' })}

    go_co (as,env) co@(CoVarCo c)
      | Just c' <- lookupVarEnv env c   = return (CoVarCo c')
      | bad_var_occ as c                = Nothing
      | otherwise                       = return co

    go_co (as,_) co@(HoleCo h)
      | bad_var_occ as (ch_co_var h)    = Nothing
      | otherwise                       = return co

    go_co cxt (AxiomCo ax cs)           = do { cs' <- mapM (go_co cxt) cs
                                             ; return (AxiomCo ax cs') }
    go_co cxt co@(UnivCo { uco_lty = ty1, uco_rty = ty2, uco_deps = cos })
      = do { ty1' <- go cxt ty1
           ; ty2' <- go cxt ty2
           ; cos' <- mapM (go_co cxt) cos
           ; return (co { uco_lty = ty1', uco_rty = ty2', uco_deps = cos' }) }

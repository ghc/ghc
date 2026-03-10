{-# LANGUAGE MultiWayIf #-}

module GHC.Core.TyCo.FVs
  (     -- Shallow
        shallowTyCoVarsOfType, shallowTyCoVarsOfTypes,
        shallowTyCoVarsOfCo, shallowTyCoVarsOfCos,

        -- Deep
        tyCoVarsOfType, tyCoVarsOfTypes, tyCoVarsOfTypesList,
        tyCoVarsOfThings,
        tyCoVarsOfCo, tyCoVarsOfCos, tyCoVarsOfMCo,
        tyCoVarsOfTyVarEnv, tyCoVarsOfCoVarEnv,
        deepTcvFolder, deepTypeFV, deepTypesFV, deepCoFV,

        -- Deep, deterministic
        tyCoVarsOfTypeDSet, tyCoVarsOfTypesDSet, tyCoVarsOfTypeList,
        tyCoVarsOfCoDSet, tyCoVarsOfCoList,
        tyCoVarsOfThingsDSet,
        deepDetTypeFV, deepDetTypesFV, deepDetCoFV,

        -- Selective
        someTyCoVarsOfType, someTyCoVarsOfTypes,

        -- CoVars only
        coVarsOfType, coVarsOfTypes,
        coVarsOfCo, coVarsOfCos,
        coVarsOfCoDSet, coVarsOfCosDSet,

        -- Shallow, deterministic, composable
        shallowSelTypeFV, shallowSelCoFV,

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
import GHC.Types.Var.FV
import GHC.Types.Unique.FM
import GHC.Types.Unique.Set

import GHC.Types.Var.Set
import GHC.Types.Var.Env

import GHC.Utils.Misc
import GHC.Utils.EndoOS

import GHC.Data.Pair

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


Note [Computing deep free variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tyCoVarsOfType computes the /deep/ free variables of a type; that is, if
`a::k` is in the result, then so are the free vars of `k`.  We say that the
resulting set is "closed over kinds".

But we must take care (see #14880):

1. Efficiency. If we have Proxy (a::ki) -> Proxy (a::ki) -> Proxy (a::ki), then
   we don't want to have to traverse ki more than once.

2. Correctness. Imagine we have forall k. (b::k) -> k, where b has
   kind k, for some k bound in an /outer/ scope. If we look at b's kind inside
   the forall, we'll collect that k is free and then remove k from the set of
   free variables. This is plain wrong. We must instead compute that b is free
   and then conclude that b's kind is free.

   BUT: there is no worry here any more because of Invariant (NoTypeShadowing).
        in GHC.Core.   In the example, the `forall k` shadows the `k` in
        b's kind, which is now illegal and checked by Lint.

An obvious first approach is to compute the /shallow/ free variables of the type,
and /then/ close over kinds.   But that turns out not to be very efficient.
Fortunately, there is a simpler way, which works with the accumulating
free-var story described in (FV1) of Note [Finding free variables] in
GHC.Types.Var.FV.  At an occurrence of a variable (a::k)

* Check if `a` is a locally-bound var; if so, ignore it.

* Check if `a` is already in the accumulator; if so, ignore it because we have
  deal with its kind already. Also pre-checking set membership before inserting
  allocates less than just inserting, because the no-op case of insertion does
  allocation.

* Otherwise add `a` to the accumulator,
  AND add on the free vars of its kind `k`.
  BUT in this latter step, start with an empty BoundVars set.

The "start with an empty BoundVars set" is implemented in `deepUnitFV`.  It's
not /necessary/ to zap the BoundVars set, because of Invariant (NoTypeShadowing).
But it's a tiny bit more efficient because the BoundVars set is smaller.

Side note: the free-variable binder would still work even without (NoTypeShadowing).
Consider:
    forall k. b -> k
where b :: k->Type is free; but of course, it's a different k! When looking at
b -> k we'll have k in the bound-var set. So we'll ignore the k. But suppose
this is our first encounter with b; we want the free vars of its kind. But we
want to behave as if we took the free vars of its kind at the end; that is,
with no bound vars in scope.

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
-- See Note [Computing deep free variables]
tyCoVarsOfCo co = runTyCoVars (deepCoFV co)

tyCoVarsOfMCo :: MCoercion -> TyCoVarSet
tyCoVarsOfMCo MRefl    = emptyVarSet
tyCoVarsOfMCo (MCo co) = tyCoVarsOfCo co

tyCoVarsOfCos :: [Coercion] -> TyCoVarSet
tyCoVarsOfCos cos = runTyCoVars (deepCosFV cos)

tyCoVarsOfThings :: Foldable t => (a -> Type) -> t a -> TyCoVarSet
-- Works over a collection of things from which we can extract a type
-- See Note [Computing deep free variables]
tyCoVarsOfThings get_ty things
  = runTyCoVars $ mapUnionFV (deepTypeFV . get_ty) things

-- | Returns free variables of types, including kind variables as
-- a non-deterministic set. For type synonyms it does /not/ expand the
-- synonym.
tyCoVarsOfTyVarEnv :: TyVarEnv Type -> TyCoVarSet
-- See Note [Shallow and deep free variables]of types]
tyCoVarsOfTyVarEnv tys = tyCoVarsOfTypes (nonDetEltsUFM tys)
  -- It's OK to use nonDetEltsUFM here because we immediately
  -- forget the ordering by returning a set

tyCoVarsOfCoVarEnv :: CoVarEnv Coercion -> TyCoVarSet
tyCoVarsOfCoVarEnv cos = tyCoVarsOfCos (nonDetEltsUFM cos)
  -- It's OK to use nonDetEltsUFM here because we immediately
  -- forget the ordering by returning a set

deepTypeFV  :: Type       -> TyCoFV
deepTypesFV :: [Type]     -> TyCoFV
deepCoFV    :: Coercion   -> TyCoFV
deepCosFV   :: [Coercion] -> TyCoFV
(deepTypeFV, deepTypesFV, deepCoFV, deepCosFV) = foldTyCo deepTcvFolder

deepTcvFolder :: TyCoFolder TyCoFV
-- It's important that we use a one-shot EndoOS, to ensure that all
-- the free-variable finders are eta-expanded.  Lacking the one-shot-ness
-- led to some big slow downs.  See Note [The one-shot state monad trick]
-- in GHC.Utils.Monad
deepTcvFolder = TyCoFolder { tcf_view = noView  -- See Note [Free vars and synonyms]
                           , tcf_tyvar = do_tcv, tcf_covar = do_tcv
                           , tcf_hole  = do_hole
                           , tcf_tycobinder = addBndrFV }
  where
    do_tcv :: TyVar -> TyCoFV
    do_tcv = deepUnitFV deepTypeFV

    do_hole :: CoercionHole -> TyCoFV
    do_hole hole = deepTypeFV (varType (coHoleCoVar hole))
                     -- We don't collect the CoercionHole itself, but we /do/
                     -- need to collect the free variables of its /kind/
                     -- See Note [CoercionHoles and coercion free variables]

deepUnitFV :: (Type -> TyCoFV) -> TyCoVar -> TyCoFV
-- Deal with a single TyCoVar
-- Takes a function to find free vars of the kind
-- See Note [Computing deep free variables]
deepUnitFV fvs_of_kind v
  = MkFV (\bvs -> EndoOS (do_it bvs))
  where
    do_it :: BoundVars -> TyCoVarSet -> TyCoVarSet
    do_it bvs acc | v `elemVarSet` bvs = acc
                  | v `elemVarSet` acc = acc
                  | otherwise          = runFVAcc (fvs_of_kind (varType v)) acc
                                         `extendVarSet` v
                  -- Left-to-right: add the kind variables to the
                  --                accumulator before v itself

{- *********************************************************************
*                                                                      *
          Shallow free variables
          See Note [Shallow and deep free variables]
*                                                                      *
********************************************************************* -}

shallowTyCoVarsOfType :: Type -> TyCoVarSet
-- See Note [Shallow and deep free variables]
shallowTyCoVarsOfType ty = runTyCoVars (shallowTypeFV ty)

shallowTyCoVarsOfTypes :: [Type] -> TyCoVarSet
shallowTyCoVarsOfTypes tys = runTyCoVars (shallowTypesFV tys)

shallowTyCoVarsOfCo :: Coercion -> TyCoVarSet
shallowTyCoVarsOfCo co = runTyCoVars (shallowCoFV co)

shallowTyCoVarsOfCos :: [Coercion] -> TyCoVarSet
shallowTyCoVarsOfCos cos = runTyCoVars (shallowCosFV cos)

shallowTypeFV  :: Type       -> TyCoFV
shallowTypesFV :: [Type]     -> TyCoFV
shallowCoFV    :: Coercion   -> TyCoFV
shallowCosFV   :: [Coercion] -> TyCoFV
(shallowTypeFV, shallowTypesFV, shallowCoFV, shallowCosFV)
   = foldTyCo shallowTcvFolder

shallowTcvFolder :: TyCoFolder TyCoFV
shallowTcvFolder = TyCoFolder { tcf_view = noView  -- See Note [Free vars and synonyms]
                              , tcf_tyvar = do_tcv, tcf_covar = do_tcv
                              , tcf_hole  = do_hole
                              , tcf_tycobinder = addBndrFV }
  where
    do_tcv = shallowUnitFV
    do_hole _  = mempty   -- Ignore coercion holes

shallowUnitFV :: TyCoVar -> TyCoFV
shallowUnitFV v
  = MkFV (\bvs -> EndoOS (do_it bvs))
  where
    do_it bvs acc | v `elemVarSet` bvs = acc
                  | v `elemVarSet` acc = acc
                  | otherwise          = acc `extendVarSet` v


{- *********************************************************************
*                                                                      *
          Deterministic, deep free vars
*                                                                      *
********************************************************************* -}

-- | `tyCoVarsOfTypeDSet` that returns deep free variables of a type in a
-- deterministic-- set. For explanation of why using `VarSet` is not deterministic
-- see Note [Deterministic FV] in "GHC.TYpes.Var.FV".
tyCoVarsOfTypeDSet :: Type -> DTyCoVarSet
-- See Note [Computing deep free variables]
tyCoVarsOfTypeDSet ty = runTyCoVarsDSet (deepDetTypeFV ty)

-- | Returns free variables of types, including kind variables as
-- a deterministic set. For type synonyms it does /not/ expand the
-- synonym.
tyCoVarsOfTypesDSet :: [Type] -> DTyCoVarSet
-- See Note [Computing deep free variables]
tyCoVarsOfTypesDSet tys = runTyCoVarsDSet (deepDetTypesFV tys)

tyCoVarsOfThingsDSet :: Foldable t => (a -> Type) -> t a -> DTyCoVarSet
-- Works over a collection of things from which we can extract a type
-- See Note [Computing deep free variables]
tyCoVarsOfThingsDSet get_ty things
  = runTyCoVarsDSet (mapUnionFV (deepDetTypeFV . get_ty) things)

-- | `tyCoVarsOfTypeList` returns free variables of a type in deterministic
-- order. For explanation of why using `VarSet` is not deterministic see
-- Note [Deterministic FV] in "GHC.Types.Var.FV".
tyCoVarsOfTypeList :: Type -> [TyCoVar]
-- See Note [Computing deep free variables]
tyCoVarsOfTypeList ty = dVarSetElems $ tyCoVarsOfTypeDSet ty

tyCoVarsOfCoDSet :: Coercion -> DTyCoVarSet
-- See Note [Computing deep free variables]
tyCoVarsOfCoDSet ty = runTyCoVarsDSet (deepDetCoFV ty)

tyCoVarsOfCoList :: Coercion -> [TyCoVar]
-- See Note [Computing deep free variables]
tyCoVarsOfCoList ty = dVarSetElems $ tyCoVarsOfCoDSet ty

-- | Returns free variables of types, including kind variables as
-- a deterministically ordered list. For type synonyms it does /not/ expand the
-- synonym.
tyCoVarsOfTypesList :: [Type] -> [TyCoVar]
-- See Note [Computing deep free variables]
tyCoVarsOfTypesList tys = dVarSetElems $ tyCoVarsOfTypesDSet tys

deepDetTypeFV  :: Type   -> DTyCoFV
deepDetTypesFV :: [Type] -> DTyCoFV
deepDetCoFV    :: Coercion -> DTyCoFV
(deepDetTypeFV, deepDetTypesFV, deepDetCoFV, _) = foldTyCo deepDetTcvFolder

deepDetTcvFolder :: TyCoFolder DTyCoFV
-- This one returns a /deterministic/ list
-- See `deepTcvFolder` for the general pattern
deepDetTcvFolder
  = TyCoFolder { tcf_view = noView
               , tcf_tyvar = do_tcv, tcf_covar = do_tcv
               , tcf_hole  = do_hole
               , tcf_tycobinder = addBndrFV }
  where
    do_tcv = deepDetUnitFV deepDetTypeFV
    do_hole hole = deepDetTypeFV (varType (coHoleCoVar hole))

deepDetUnitFV :: (Type -> DTyCoFV) -> TyCoVar -> DTyCoFV
-- Deal with a single TyCoVar
-- Takes a function to find free vars of the kind
-- See Note [Computing deep free variables]
deepDetUnitFV fvs_of_kind v
  = MkFV (\bvs -> EndoOS (do_it bvs))
  where
    do_it :: BoundVars -> DTyCoVarSet -> DTyCoVarSet
    do_it bvs acc | v `elemVarSet` bvs  = acc
                  | v `elemDVarSet` acc = acc
                  | otherwise           = runFVAcc (fvs_of_kind (varType v)) acc
                                          `extendDVarSet` v
                  -- Left-to-right: add the kind variables to the
                  --                accumulator before v itself

{- *********************************************************************
*                                                                      *
          Selective, shallow, deterministic free vars
*                                                                      *
********************************************************************* -}

someTyCoVarsOfType :: (TyCoVar -> Bool) -> Type -> [TyCoVar]
someTyCoVarsOfType interesting
  = runFVSelectiveList interesting . shallowSelTypeFV

someTyCoVarsOfTypes :: (TyCoVar -> Bool) -> [Type] -> [TyCoVar]
someTyCoVarsOfTypes interesting
  = runFVSelectiveList interesting . mapUnionFV shallowSelTypeFV

shallowSelTypeFV :: Type -> SelectiveDFV
shallowSelCoFV   :: Coercion -> SelectiveDFV
-- Returns shallow free vars
-- See Note [Shallow and deep free variables]
(shallowSelTypeFV, _, shallowSelCoFV, _) = foldTyCo selectiveTcvFolder

selectiveTcvFolder :: TyCoFolder SelectiveDFV
-- This one takes an `InterestingVarFun`, and returns shallow free vars
-- See `shallowTcvFolder` for the general pattern
selectiveTcvFolder
  = TyCoFolder { tcf_view  = noView  -- See Note [Free vars and synonyms]
               , tcf_tyvar = do_tcv, tcf_covar = do_tcv
               , tcf_hole  = do_hole
               , tcf_tycobinder = addBndrSelectiveFV }
  where
    do_tcv v = MkFV (\bvs -> EndoOS (do_it bvs))
      where
        do_it (is_interesting,bvs) acc
          | not (is_interesting v) = acc  -- The "selective" bit
          | v `elemVarSet` bvs     = acc
          | v `elemDVarSet` acc    = acc
          | otherwise              = acc `extendDVarSet` v

    do_hole hole = shallowSelTypeFV (varType (coHoleCoVar hole))


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

coVarsOfType  ty  = runTyCoVars (deepCoVarTypeFV ty)
coVarsOfTypes tys = runTyCoVars (deepCoVarTypesFV tys)
coVarsOfCo    co  = runTyCoVars (deepCoVarCoFV co)
coVarsOfCos   cos = runTyCoVars (deepCoVarCosFV cos)

type CoVarFV  = FV BoundVars (EndoOS CoVarSet)

deepCoVarTypeFV  :: Type       -> CoVarFV
deepCoVarTypesFV :: [Type]     -> CoVarFV
deepCoVarCoFV  :: Coercion   -> CoVarFV
deepCoVarCosFV :: [Coercion] -> CoVarFV
(deepCoVarTypeFV, deepCoVarTypesFV, deepCoVarCoFV, deepCoVarCosFV) = foldTyCo deepCoVarFolder

deepCoVarFolder :: TyCoFolder CoVarFV
deepCoVarFolder = TyCoFolder { tcf_view = noView
                             , tcf_tyvar = do_tyvar, tcf_covar = do_covar
                             , tcf_hole  = do_hole
                             , tcf_tycobinder = addBndrFV }
  where
    do_tyvar _  = mempty
      -- This do_tyvar means we won't see any CoVars in this
      -- TyVar's kind.   This may be wrong; but it's the way it's
      -- always been.  And its awkward to change, because
      -- the tyvar won't end up in the accumulator, so
      -- we'd look repeatedly.  Blargh.

    do_covar = deepUnitFV deepCoVarTypeFV

    do_hole hole  = do_covar (coHoleCoVar hole)
      -- We /do/ treat a CoercionHole as a free variable
      -- See Note [CoercionHoles and coercion free variables]

-------------- Deterministic versions ------------------
type DCoVarFV  = FV BoundVars (EndoOS DCoVarSet)

coVarsOfCoDSet :: Coercion -> DCoVarSet
coVarsOfCoDSet co = runTyCoVarsDSet (det_co co)

coVarsOfCosDSet :: [Coercion] -> DCoVarSet
coVarsOfCosDSet cos = runTyCoVarsDSet (det_cos cos)

det_ty  :: Type       -> DCoVarFV
det_co  :: Coercion   -> DCoVarFV
det_cos :: [Coercion] -> DCoVarFV
(det_ty, _, det_co, det_cos) = foldTyCo deepDetCoVarFolder

deepDetCoVarFolder :: TyCoFolder DCoVarFV
-- Follows deepCoVarFolders, but returns a /deterministic/ set
deepDetCoVarFolder = TyCoFolder { tcf_view = noView
                                , tcf_tyvar = do_tyvar
                                , tcf_covar = do_covar
                                , tcf_hole  = do_hole
                                , tcf_tycobinder = addBndrFV }
  where
    do_tyvar _  = mempty

    do_covar :: CoVar -> DCoVarFV
    do_covar = deepDetUnitFV det_ty

    do_hole hole  = do_covar (coHoleCoVar hole)


{- *********************************************************************
*                                                                      *
          Closing over kinds
*                                                                      *
********************************************************************* -}

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
    do_one v = runFVAcc (deepDetTypeFV (varType v))


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
  = runTyCoVars $ mapUnionFV (inj_vars_of_type look_under_tfs) tys

inj_vars_of_type :: Bool -> Type -> TyCoFV
inj_vars_of_type look_under_tfs = go
  where
    go ty | Just ty' <- rewriterView ty = go ty'
    go (TyVarTy v)                      = deepUnitFV go v
    go (AppTy f a)                      = go f `mappend` go a
    go (FunTy _ w ty1 ty2)              = go w `mappend` go ty1 `mappend` go ty2
    go (TyConApp tc tys)                = go_tc tc tys
    go (ForAllTy (Bndr tv _) ty)        = go (tyVarKind tv) `mappend`
                                          addBndrFV tv (go ty)
    go LitTy{}                          = mempty
    go (CastTy ty _)                    = go ty
    go CoercionTy{}                     = mempty

    go_tc tc tys
      | isTypeFamilyTyCon tc
      = if | look_under_tfs
           , Injective flags <- tyConInjectivityInfo tc
           -> mapUnionFV go $
              filterByList (flags ++ repeat True) tys
                         -- Oversaturated arguments to a tycon are
                         -- always injective, hence the repeat True
           | otherwise   -- No injectivity info for this type family
           -> mempty

      | otherwise        -- Data type, injective in all positions
      = mapUnionFV go tys



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
afvFolder :: (TyCoVar -> Bool) -> TyCoFolder (FV TyCoVarSet DM.Any)
-- 'afvFolder' is short for "any-free-var folder", good for checking
-- if any free var of a type satisfies a predicate `check_fv`
afvFolder check_fv = TyCoFolder { tcf_view = noView  -- See Note [Free vars and synonyms]
                                , tcf_tyvar = do_tcv, tcf_covar = do_tcv
                                , tcf_hole = do_hole
                                , tcf_tycobinder = addBndrFV }
  where
    do_tcv tv = MkFV $ \ bvs ->
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

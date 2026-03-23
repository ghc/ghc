{-# LANGUAGE PatternSynonyms #-}

-- | Utilities for efficiently and deterministically computing free variables.
module GHC.Types.Var.FV (
        FV( runFV, MkFV ),
        BoundVars, VarSetFV, DVarSetFV, SelectiveDFV,
        TyCoFV, DTyCoFV,
        runFVTop, runFVAcc, runTyCoVars, runTyCoVarsDSet,
        runFVSelective, runFVSelectiveList, runFVSelectiveSet,
        InterestingVarFun,

        addBndrFV, addBndrsFV, addBndrSelectiveFV, addBndrsSelectiveFV,
        emptyFV,   -- or `mempty`
        unionFV,   -- or `mappend`
        mapUnionFV
    ) where

import GHC.Prelude

import GHC.Types.Var
import GHC.Types.Var.Set

import GHC.Utils.EndoOS

import GHC.Exts( oneShot )
import Data.Semigroup

{- Note [Finding free variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We have lots of different free-variable finders:
  * Shallow or deep: do we include the kind of a variable occurrence
  * Deterministic or non-deterministic: what kind of set is collected
  * Selective or not: selective means there is a predicate that says
    which variables are of interest

The type (FV env set) is the result-type of a free-variable finder, e.g.
    deepTypeFV :: Type -> FV BoundVars (EndoOS TyCoVarSet)

Here
  * `env` is an environment, which records the locally-bound variables
    (which are not of interest since they aren't free) and optionally
    the selective predicate.

  * `set` is the accumulating result set

The type (FV BoundVars (EndoOS TyCoVarSet) is roughly

        BoundVars -> TyCoVarSet -> TyCoVarSet

So `deepTypeFV ty` is a function that takes an environment and a set-
valued accumulator, and extends the set with the free vars of `ty`.
See (FV1) for why we use an accumulating parameter here.

However, we want to take the union of two FV values, thus
      deepTypeFV t1 `mappend` deepTypeFV t2
We do this by making `FV` an instance of `Monoid` and `Semigroup`
and using `mappend`.  So we use a newtype wrapper.

Some important wrinkles for efficiency:

(FV1) Accumulating parameter for the free variables.  We could return a
  VarSet, and do lots of `unionVarSet`. But it's quite a bit more efficient
  to use an accumulating parameter and add free variables one at a time.

  We implement this using a (GHC-specific variant of) the standard `Endo`
  type.  The `mappend` for `EndoOS` is just function composition; see
  GHC.Utils.EndoOS.  The "OS" stands for "one shot"; see GHC.Utils.EndoOS.

  This is more efficient for two reasons:
  * Doing a tree of `unionVarSet` calls is asymptotically inefficient.
  * For "deep" free variables, if we have an accumulator we can see if we
    have encountered this variable before; if so, we don't need to look at its
    kind.  See Note [Computing deep free variables] in GHC.Core.TyCo.FVs

(FV2) Eta expansion.  It's very important that, after optimisation, we end up
  with an arity-3 function. Let's consider:

     tyCoVarsOfType (AppTy a b) = tyCoVarsOfType a `mappend` tyCoVarsOfType b

  If we aren't careful, after inlining `mappend`, we'll end up with

    tyCoVarsOfType = \ty ->
      case ty of
        AppTy a b -> \env acc ->
          tyCoVarsOfType a env (tyCoVarsOfType env acc)

  which has to create a lambda, entirely unnecessarily. On the other hand if it
  is eta-expanded, we get this:

    tyCoVarsOfType = \ty env acc->
      case ty of
        AppTy a b -> tyCoVarsOfType a env (tyCoVarsOfType env acc)

  We achieve this using the "one-shot trick" described in
  Note [The one-shot state monad trick] in  GHC.Utils.Monad, both for the
  `env` parameter (see `oneShot` call in this module), and the `acc`
  accumulator (see `oneShot` call in GHC.Utils.EndoOS).

  See also #11146.

(FV3) Avoiding thunks.  The `mappend` for `Endo` is just function composition,
   but we don't want to get code like
     tyCoVarsOfType (AppTy a b) env acc = tyCoVarsOfType a env $ tyCoVarsOfType b env acc
   because that argument is a thunk.  We want to be strict in the accumulator, so we get
     tyCoVarsOfType (AppTy a b) env acc = tyCoVarsOfType env a $! tyCoVarsOfType b env acc
   The strictness is done by the (<>) method in `EndoOS`.

(FV4) Order.  When computing order-deterministic free vars, we'd like to return the
   variables in left-to-right order.  Thus, the free vars of (a -> b -> b) should
   be [a,b] not [b,a].  This matters for quantifying variables in a predictable
   way.

   We achieve this by
   * Combining sub-expressions in the natural left-to-right way.  e.g.
        tyCoVarsOfType (AppTy a b) = tyCoVarsOfType a `unionFV` tyCoVarsOfType b
   * In the EndoOS instance, make (f <> g) do (g.f), that is compose "backwards"

Note [Deterministic FV]
~~~~~~~~~~~~~~~~~~~~~~~
When computing free variables, the order in which you get them affects
the results of floating and specialization. If you use UniqFM to collect
them and then turn that into a list, you get them in nondeterministic
order as described in Note [Deterministic UniqFM] in GHC.Types.Unique.DFM.

So we instead collect them in a `DVarSet` (deterministic VarSet).
-}


{- *********************************************************************
*                                                                      *
          Free-var result type
*                                                                      *
********************************************************************* -}

type InterestingVarFun = Var -> Bool

type BoundVars = TyCoVarSet

type VarSetFV     = FV BoundVars (EndoOS TyCoVarSet)
type DVarSetFV    = FV BoundVars (EndoOS DTyCoVarSet)
type SelectiveDFV = FV (InterestingVarFun, BoundVars) (EndoOS DVarSet)
-- VarSetFV:     collects a VarSet
-- DVarSetFV:    collects a DVarSet (deterministic)
-- SelectiveDFV: selectively collects a DVarSet
-- Why EndoOS? See (FV1) in Note [Finding free variables]

type TyCoFV  = VarSetFV
type DTyCoFV = DVarSetFV

newtype FV env acc = MkFV' { runFV :: env -> acc }
  -- Caries an environment (typically empty, or a set of in-scope variables)
  -- and a composable accumulator.
  --
  -- NB: `acc` is usually (EndoOS something) but not always;
  --     see for example GHC.Core.TyCo.FVs.afvFolder

pattern MkFV :: (env -> acc) -> FV env acc
{-# COMPLETE MkFV #-}
pattern MkFV f <- MkFV' f
      where
        MkFV f = MkFV' (oneShot f)
         -- oneShot: this is the core of the one-shot trick!
         -- Note [The one-shot state monad trick] in  GHC.Utils.Monad.

instance Semigroup a => Semigroup (FV env a) where
  (<>) = unionFV

instance Monoid a => Monoid (FV env a) where
  mempty  = emptyFV

emptyFV :: Monoid a => FV env a
emptyFV = MkFV (\_ -> mempty)

unionFV :: Semigroup a => FV env a -> FV env a -> FV env a
unionFV (MkFV' f1) (MkFV' f2) = MkFV (\env -> f1 env <> f2 env)

upd_bndrs_fv :: (env -> env) -> FV env a -> FV env a
{-# INLINE addBndrFV #-}
upd_bndrs_fv upd f = MkFV (\bvs -> runFV f $! upd bvs)
    -- Strict application to avoid making a thunk

addBndrFV :: TyCoVar -> FV BoundVars a -> FV BoundVars a
addBndrFV tcv = upd_bndrs_fv (\bvs -> extendVarSet bvs tcv)

addBndrsFV :: [Var] -> FV BoundVars a -> FV BoundVars a
addBndrsFV tcvs = upd_bndrs_fv (\bvs -> extendVarSetList bvs tcvs)

addBndrSelectiveFV :: TyCoVar -> FV (f, BoundVars) a -> FV (f, BoundVars) a
addBndrSelectiveFV tcv
  = upd_bndrs_fv (\(f,bvs) -> let !bvs' = extendVarSet bvs tcv
                                  -- Strict let to avoid thunks
                              in (f,bvs'))

addBndrsSelectiveFV :: [Var] -> FV (f, BoundVars) a -> FV (f, BoundVars) a
addBndrsSelectiveFV bs
  = upd_bndrs_fv (\(f,bvs) -> let !bvs' = extendVarSetList bvs bs
                                  -- Strict let to avoid thunks
                              in (f,bvs'))

mapUnionFV :: (Foldable t, Monoid acc)
          => (a -> FV env acc) -> t a -> FV env acc
{-# INLINE mapUnionFV #-}
mapUnionFV f xs = foldr (mappend . f) mempty xs


runFVTop :: FV BoundVars a -> a
{-# INLINE runFVTop #-}
runFVTop f = runFV f (emptyVarSet :: BoundVars)

runFVAcc :: FV BoundVars (EndoOS a) -> a -> a
{-# INLINE runFVAcc #-}
runFVAcc f = runEndoOS (runFVTop f)

runTyCoVars :: TyCoFV -> TyCoVarSet
{-# INLINE runTyCoVars #-}
runTyCoVars f = runFVAcc f emptyVarSet

runTyCoVarsDSet :: DTyCoFV -> DTyCoVarSet
{-# INLINE runTyCoVarsDSet #-}
runTyCoVarsDSet f = runFVAcc f emptyDVarSet

runFVSelective :: InterestingVarFun -> SelectiveDFV -> DVarSet
runFVSelective interesting f
  = runEndoOS (runFV f (interesting, emptyVarSet)) emptyDVarSet

runFVSelectiveList :: InterestingVarFun -> SelectiveDFV -> [Var]
runFVSelectiveList interesting f = dVarSetElems (runFVSelective interesting f)

runFVSelectiveSet :: InterestingVarFun -> SelectiveDFV -> VarSet
runFVSelectiveSet interesting f = dVarSetToVarSet (runFVSelective interesting f)

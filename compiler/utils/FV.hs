{-# OPTIONS_GHC -ddump-to-file -ddump-simpl #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BangPatterns #-}

module FV
  ( -- | An abstraction over free variable computations
    FVM(..)
    -- * Are there any free variables at all?
  , AnyFVs, noFVs
    -- * Deterministic free variable computation
  , FV(..)
  , fvVarListVarSet
  , fvVarList
  , fvDVarSet
  , fvVarSet
  , filterFV
  , InterestingVarFun
  , emptyFV
  , delFV
  , delFVs
  , unionFV, mapUnionFV, mkFVs
    -- * Non-deterministic free variable computation
  , NonDetFV
  , nonDetFVSet
    -- * Non-deterministic free coercion variable computation
  , NonDetCoFV
  , nonDetCoFVSet
  ) where

import GhcPrelude

import FVM

import {-# SOURCE #-} TyCoRep (coHoleCoVar)
import {-# SOURCE #-} TyCoFVs (typeFVs)
import Var
import VarSet

import Data.Semigroup (Semigroup((<>)))


--------------------------------------------------------------------------------
-- Checking for empty free variable sets
--------------------------------------------------------------------------------

-- | A free variables traversal that checks whether the free variable set is empty.
newtype AnyFVs = AnyFVs (VarSet -> Bool)

instance Monoid AnyFVs where
  mempty = AnyFVs $ const False
  {-# INLINE mempty #-}

instance Semigroup AnyFVs where
  AnyFVs f <> AnyFVs g = AnyFVs $ \in_scope -> f in_scope || g in_scope
  {-# INLINE (<>) #-}

instance FVM AnyFVs where
  coholeFV _hole = mempty
  unitFV v = AnyFVs $ \in_scope -> not (v `elemVarSet` in_scope)
  bindVar tv (AnyFVs f) = AnyFVs $ \in_scope -> f (extendVarSet in_scope tv)

  {-# INLINE coholeFV #-}
  {-# INLINE unitFV #-}
  {-# INLINE bindVar #-}

runAnyFVs :: AnyFVs -> Bool
runAnyFVs (AnyFVs f) = f emptyVarSet

noFVs :: AnyFVs -> Bool
noFVs = not . runAnyFVs


--------------------------------------------------------------------------------
-- Non-deterministic free variable sets
--------------------------------------------------------------------------------

-- | A free variables traversal that produces a non-deterministic 'TyCoVarSet'.
newtype NonDetFV = NonDetFV { runNonDetFV :: TyCoVarSet -> TyCoVarSet -> TyCoVarSet }

instance Monoid NonDetFV where
  mempty = NonDetFV $ \_ acc -> acc
  {-# INLINE mempty #-}

instance Semigroup NonDetFV where
  NonDetFV f <> NonDetFV g = NonDetFV $ \is acc -> g is (f is acc)
  {-# INLINE (<>) #-}

instance FVM NonDetFV where
  coholeFV hole = unitFV $ coHoleCoVar hole
  unitFV v = NonDetFV $ \is acc ->
    if | v `elemVarSet` is  -> acc
       | v `elemVarSet` acc -> acc
       | otherwise          -> runNonDetFV (typeFVs (varType v)) emptyVarSet (extendVarSet acc v)
  bindVar v (NonDetFV f) = NonDetFV $ \is acc -> f (extendVarSet is v) acc

  {-# INLINE coholeFV #-}
  {-# INLINE unitFV #-}
  {-# INLINE bindVar #-}

nonDetFVSet :: NonDetFV -> TyCoVarSet
nonDetFVSet (NonDetFV f) = f emptyVarSet emptyVarSet


--------------------------------------------------------------------------------
-- Non-deterministic free coercion variable sets
--------------------------------------------------------------------------------

-- | A free variables (restricted to coercion variables) traversal that
-- produces a non-deterministic 'CoVarSet'.
newtype NonDetCoFV = NonDetCoFV { runNonDetCoFV :: CoVarSet -> CoVarSet -> CoVarSet }

instance Monoid NonDetCoFV where
  mempty = NonDetCoFV $ \_ acc -> acc
  {-# INLINE mempty #-}

instance Semigroup NonDetCoFV where
  NonDetCoFV f <> NonDetCoFV g = NonDetCoFV $ \is acc -> g is (f is acc)
  {-# INLINE (<>) #-}

instance FVM NonDetCoFV where
  coholeFV hole = unitFV $ coHoleCoVar hole
  unitFV v = NonDetCoFV $ \is acc ->
    if | not (isCoVar v)    -> acc
       | v `elemVarSet` is  -> acc
       | v `elemVarSet` acc -> acc
       | otherwise          -> runNonDetCoFV (typeFVs (varType v)) emptyVarSet (extendVarSet acc v)
  bindVar v (NonDetCoFV f) = NonDetCoFV $ \is acc -> f (extendVarSet is v) acc

  {-# INLINE coholeFV #-}
  {-# INLINE unitFV #-}
  {-# INLINE bindVar #-}

nonDetCoFVSet :: NonDetCoFV -> CoVarSet
nonDetCoFVSet (NonDetCoFV f) = f emptyVarSet emptyVarSet


--------------------------------------------------------------------------------
-- Deterministic free variable sets
--------------------------------------------------------------------------------

type InterestingVarFun = Var -> Bool

-- | A free variables traversal that produces a deterministic 'DVarSet
newtype FV = FV { runFV :: InterestingVarFun -> TyCoVarSet -> ([Var], VarSet) -> ([Var], VarSet) }

instance Monoid FV where
  mempty = FV $ \_ _ acc -> acc
  {-# INLINE mempty #-}

instance Semigroup FV where
  FV f <> FV g = FV $ \fv_cand in_scope acc -> f fv_cand in_scope $! g fv_cand in_scope $! acc
  {-# INLINE (<>) #-}

whenIsInteresting :: Var -> FV -> FV
whenIsInteresting var (FV f) = FV g
  where
    g fv_cand in_scope acc@(_have, have_set)
      | var `elemVarSet` in_scope  = acc
      | var `elemVarSet` have_set  = acc
      | fv_cand var                = f fv_cand in_scope acc
      | otherwise                  = acc

instance FVM FV where
  coholeFV hole = unitFV $ coHoleCoVar hole
  unitFV var = whenIsInteresting var $ add_fv var <> typeFVs (varType var)
    where
      add_fv :: Var -> FV
      add_fv var = FV $ \_fv_cand _in_scope (have, have_set) ->
        (var : have, extendVarSet have_set var)
  bindVar tv (FV f) = FV $ \fv_cand in_scope acc ->
    f fv_cand (extendVarSet in_scope tv) acc

  {-# INLINE coholeFV #-}
  {-# INLINE unitFV #-}
  {-# INLINE bindVar #-}

fvVarListVarSet :: FV ->  ([Var], VarSet)
fvVarListVarSet (FV fv) = fv (const True) emptyVarSet ([], emptyVarSet)

fvVarList :: FV -> [Var]
fvVarList = fst . fvVarListVarSet

fvDVarSet :: FV -> DVarSet
fvDVarSet fv = mkDVarSet $ fst $ fvVarListVarSet fv

fvVarSet :: FV -> VarSet
fvVarSet = snd . fvVarListVarSet

-- | Filter a free variable computation.
filterFV :: InterestingVarFun -> FV -> FV
filterFV fv_cand2 (FV fv) = FV $ \fv_cand1 in_scope acc ->
  fv (\v -> fv_cand1 v && fv_cand2 v) in_scope acc
{-# INLINE filterFV #-}

-- | Return no free variables.
emptyFV :: FV
emptyFV = mempty
{-# INLINE emptyFV #-}

-- | Mark the variable as not free by putting it in scope.
delFV :: Var -> FV -> FV
delFV = bindVar
{-# INLINE delFV #-}

-- | Mark many free variables as not free.
delFVs :: VarSet -> FV -> FV
delFVs vars (FV fv) = FV $ \fv_cand !in_scope acc ->
  fv fv_cand (in_scope `unionVarSet` vars) acc
{-# INLINE delFVs #-}

mapUnionFV :: (a -> FV) -> [a] -> FV
mapUnionFV = foldMap

unionFV :: FV -> FV -> FV
unionFV = (<>)

mkFVs :: [Var] -> FV
mkFVs = foldMap unitFV

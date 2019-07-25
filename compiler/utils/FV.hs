{-# OPTIONS_GHC -ddump-to-file -ddump-simpl #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BangPatterns #-}

module FV
  ( -- | An abstraction over free variable computations
    FVM(..)
    -- * Are there any free variables at all?
  , AnyFVs, noFVs
    -- * Deterministic free variable computation
  , FV
  , fvVarListVarSet
  , fvVarList
  , fvDVarSet
  , fvVarSet
  , filterFV
  , emptyFV
  , delFV
  , delFVs
    -- * Non-deterministic free variable computation
  , NonDetFV
  , runNonDetFV
  ) where

import GhcPrelude
import Data.Semigroup (Semigroup((<>)))

import {-# SOURCE #-} TyCoRep (Type, Coercion, CoercionHole, coHoleCoVar)
import Var
import VarSet

class Monoid m => FVM m where
  coholeFV  :: CoercionHole -> m
  unitFV   :: Var -> m
  tycoVarsFV :: DVarSet -> m
  bindVar :: Var -> m -> m


newtype AnyFVs = AnyFVs (VarSet -> Bool)

instance Monoid AnyFVs where
  mempty = AnyFVs $ const False
  {-# INLINE mempty #-}

instance Semigroup AnyFVs where
  AnyFVs f <> AnyFVs g = AnyFVs $ \in_scope -> f in_scope || g in_scope
  {-# INLINE (<>) #-}

instance FVM AnyFVs where
  coholeFV hole = mempty
  unitFV v = AnyFVs $ \in_scope -> not (v `elemVarSet` in_scope)
  tycoVarsFV fvs = AnyFVs $ \in_scope -> not $ isEmptyVarSet $ dVarSetToVarSet fvs `minusVarSet` in_scope
  bindVar tv (AnyFVs f) = AnyFVs $ \in_scope -> f (extendVarSet in_scope tv)

  {-# INLINE coholeFV #-}
  {-# INLINE unitFV #-}
  {-# INLINE bindVar #-}

runAnyFVs :: AnyFVs -> Bool
runAnyFVs (AnyFVs f) = f emptyVarSet

noFVs :: AnyFVs -> Bool
noFVs = not . runAnyFVs


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
  tycoVarsFV fvs = NonDetFV $ \is acc -> acc `unionVarSet` (dVarSetToVarSet fvs `minusVarSet` is)
  bindVar v (NonDetFV f) = NonDetFV $ \is acc -> f (extendVarSet v is) acc

  {-# INLINE coholeFV #-}
  {-# INLINE unitFV #-}
  {-# INLINE bindVar #-}

type InterestingVarFun = Var -> Bool

newtype FV = FV { runFV :: InterestingVarFun -> TyCoVarSet -> ([Var], VarSet) -> ([Var], VarSet) }

instance Monoid FV where
  mempty = FV $ \_ acc -> acc
  {-# INLINE mempty #-}

instance Semigroup FV where
  FV f <> FV g = FV $ \fv_cand in_scope acc -> f fv_cand in_scope $! g fv_cand in_scope $! acc
  {-# INLINE (<>) #-}

instance FVM FV where
  coholeFV hole = unitFV $ coHoleCoVar hole
  unitFV v = FV $ \fv_cand in_scope acc@(have, haveSet) ->
    if | v `elemVarSet` in_scope  -> acc
       | v `elemVarSet` haveSet   -> acc
       | fv_cand var              -> (var: have, extendVarSet haveSet var)
       | otherwise                -> acc
  tycoVarsFV fvs = foldMap tyvar (dvarSetElems fvs) -- can we do better than this?
  bindVar tv (FV f) = FV $ \fv_cand in_scope acc ->
    f fv_cand (extendVarSet tv in_scope) acc

  {-# INLINE coholeFV #-}
  {-# INLINE unitFV #-}
  {-# INLINE tycoVarsFV #-}
  {-# INLINE bindVar #-}

fvVarListVarSet :: FV ->  ([Var], VarSet)
fvVarListVarSet (FV fv) = fv (const True) emptyVarSet ([], emptyVarSet)

fvVarList :: FV -> [Var]
fvVarList = fst . fvVarListVarSet

fvDVarSet :: FV -> DVarSet
fvDVarSet (FV fv) = mkDVarSet $ fst $ fvVarListVarSet fv

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

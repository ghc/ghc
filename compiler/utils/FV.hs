{-# OPTIONS_GHC -ddump-to-file -ddump-simpl #-}

module FV2 where

import GhcPrelude

import Var
import VarSet

class Monoid m => FVM m where
  coholeFV  :: CoercionHole -> m
  tyvarFV   :: TyVar -> m
  covarFV   :: CoVar -> m
  tycoVarsFV :: DVarSet -> m
  bindTyvar :: TyVar -> m -> m

typeFVs :: FVM m => Type -> m
typeFVs (TyVarTy v) = tyvarFV v
typeFVs (TyConApp _ tys)   = foldMap typeFVs tys
typeFVs (LitTy {})         = mempty
typeFVs (AppTy fun arg)    = typeFVs fun <> typeFVs arg
typeFVs (FunTy _ arg res)  = typeFVs arg <> typeFVs res
typeFVs (ForAllTy (Bndr tv _) ty) = typeFVs (varType tv) <> bindTyvar tv (typeFVs ty)
typeFVs (CastTy ty co)     = typeFVsty <> coFVs co
typeFVs (CoercionTy co)    = ty_co_vars_of_co co is acc
{-# SPECIALISE typeFVs :: Type -> FV #-}
{-# SPECIALISE typeFVs :: Type -> NonDetFV #-}

typesFVs :: FVM m => [Type] -> m
typesFVs = foldMap typeFVs
{-# SPECIALISE typesFVs :: [Type] -> FV #-}
{-# SPECIALISE typesFVs :: [Type] -> NonDetFV #-}

coFVs :: FVM m => Coercion -> m
coFVs (Refl ty) = typeFVs ty
coFVs (GRefl _ ty mco) = typeFVs ty <> mcoFVs mco
coFVs (TyConAppCo_ _ cos) = foldMap coFVs cos
coFVs (AppCo co arg) = coFVs co <> coFVs arg
coFVs (ForAllCo tv kind_co co) = coFVs kind_co <> bindTyvar tv (coFVs co)
coFVs (FunCo _ co1 co2) = coFVs co1 <> coFVs co2
coFVs (HoleCo hole) = coholeFV hole
coFVs (AxiomInstCo _ _ cos) = foldMap coFVs cos
coFVs (UnivCo p _ t1 t2) = provFVs p <> typeFVs t1 <> typeFVs t2
coFVs (SymCo co) = coFVs co
coFVs (TransCo co1 co2) = coFVs co1 <> coFVs co2
coFVs (NthCo _ _ co) = coFVs co
coFVs (LRCo _ co) = coFVs co
coFVs (InstCo co arg) = coFVs co <> coFVs arg
coFVs (KindCo co) = coFVs co
coFVs (SubCo co) = coFVs co
coFVs (AxiomRuleCo _ cos) = foldMap coFVs co
{-# SPECIALISE coFVs :: Coercion -> FV #-}
{-# SPECIALISE coFVs :: Coercion -> NonDetFV #-}

cosFVs :: FVM m => [Coercion] -> m
cosFVs = foldMap coFVs
{-# SPECIALISE cosFVs :: [Coercion] -> FV #-}
{-# SPECIALISE cosFVs :: [Coercion] -> NonDetFV #-}

mcoFVs :: FVM m => MCoercion -> m
mcoFVs MRefl = mempty
mcoFVs (MCo co) = coFVs co
{-# SPECIALISE mcoFVs :: MCoercion -> FV #-}
{-# SPECIALISE mcoFVs :: MCoercion -> NonDetFV #-}

provFVs :: FVM m => UnivCoProvenance -> m
provFVs (PhantomProv co)    = coFVs co
provFVs (ProofIrrelProv co) = coFVs co
provFVs UnsafeCoerceProv    = mempty
provFVs (PluginProv _)      = mempty
provFVs (ZappedProv fvs)    = tycoVarsFV fvs
provFVs (TcZappedProv fvs coholes)
                            = foldMap (tyvarFV . coHoleCoVar) coholes <> tyscoVarsFV fvs
{-# SPECIALISE provFVs :: UnivCoProvenance -> FV #-}
{-# SPECIALISE provFVs :: UnivCoProvenance -> NonDetFV #-}


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
  tycovarsFV fvs = AnyFVs $ \in_scope -> not $ nullVarSet $ dVarSetToVarSet fvs `minusVarSet` in_scope
  bindTyVar tv (AnyFVs f) = AnyFVs $ \in_scope -> f (extendVarSet tv in_scope)

  {-# INLINE coholeFV #-}
  {-# INLINE tyvarFV #-}
  {-# INLINE covarFV #-}
  {-# INLINE bindTyvar #-}

runAnyFVs :: AnyFVs -> Bool
runAnyFVs (AnyFVs f) = f emptyVarSet


newtype NonDetFV = NonDetFV { runNonDetFV :: TyCoVarSet -> TyCoVarSet -> TyCoVarSet }

instance Monoid NonDetFV where
  mempty = NonDetFV $ \_ acc -> acc
  {-# INLINE mempty #-}

instance Semigroup NonDetFV where
  NonDetFV f <> NonDetFV g = NonDetFV $ \is acc -> g is (f is acc)
  {-# INLINE (<>) #-}

addNDFV :: Var -> NonDetFV
addNDFV v = NonDetFV $ \is acc ->
    if | v `elemVarSet` is  -> acc
       | v `elemVarSet` acc -> acc
       | otherwise       acc -> runNonDetFV (typeFVs (varType v)) emptyVarSet (extendVarSet acc v)

instance FVM NonDetFV where
  coholeFV hole = covarFV $ coHoleCoVar hole
  tyvarFV tv = addNDFV tv
  covarFV cv = addNDFV cv
  tycovarsFV fvs = NonDetFV $ \is acc -> acc `unionVarSet` (dVarSetToVarSet fvs `minusVarSet` is)
  bindTyVar tv (NonDetFV f) = NonDetFV $ \is acc -> f (extendVarSet tv is) acc

  {-# INLINE coholeFV #-}
  {-# INLINE tyvarFV #-}
  {-# INLINE covarFV #-}
  {-# INLINE bindTyvar #-}

type InterestingVarFun = Var -> Bool

newtype FV = FV { runFV :: InterestingVarFun -> TyCoVarSet -> ([Var], VarSet) -> ([Var], VarSet) }

instance Monoid FV where
  mempty = FV $ \_ acc -> acc
  {-# INLINE mempty #-}

instance Semigroup FV where
  FV f <> FV g = FV $ \fv_cand in_scope acc -> f fv_cand in_scope $! g fv_cand in_scope $! acc
  {-# INLINE (<>) #-}

instance FVM FV where
  coholeFV hole = covarFV $ coHoleCoVar hole
  tyvarFV tv = addFV tv
  covarFV cv = addFV cv
  tycoVarsFV fvs = foldMap tyvar (dvarSetElems fvs) -- can we do better than this?
  bindTyVar tv (FV f) = FV $ \fv_cand in_scope acc ->
    f fv_cand (extendVarSet tv in_scope) acc ``

  {-# INLINE coholeFV #-}
  {-# INLINE tyvarFV #-}
  {-# INLINE covarFV #-}
  {-# INLINE bindTyvar #-}

addFV :: Var -> FV
addFV v = FV $ \fv_cand in_scope acc@(have, haveSet) ->
    if | v `elemVarSet` in_scope  -> acc
       | v `elemVarSet` haveSet   -> acc
       | fv_cand var              -> (var: have, extendVarSet haveSet var)
       | otherwise                -> acc
{-# INLINE addFV #-}

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

unitFV :: Id -> FV
unitFV = tyvarFV
{-# INLINE unitFV #-}

-- | Return no free variables.
emptyFV :: FV
emptyFV = mempty
{-# INLINE emptyFV #-}

-- | Mark the variable as not free by putting it in scope.
delFV :: Var -> FV -> FV
delFV = bindTyVar
{-# INLINE delFV #-}

-- | Mark many free variables as not free.
delFVs :: VarSet -> FV -> FV
delFVs vars (FV fv) = FV $ \fv_cand !in_scope acc ->
  fv fv_cand (in_scope `unionVarSet` vars) acc
{-# INLINE delFVs #-}

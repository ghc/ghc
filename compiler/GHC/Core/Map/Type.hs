{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module GHC.Core.Map.Type (
     -- * Re-export generic interface
   TrieMap(..),

     -- * Maps over 'Type's
   TypeMap, emptyTypeMap, extendTypeMap, lookupTypeMap, foldTypeMap,
   LooseTypeMap,
   -- ** With explicit scoping
   CmEnv, lookupCME, extendTypeMapWithScope, lookupTypeMapWithScope,
   mkDeBruijnContext, extendCME, extendCMEs, emptyCME,

   -- * Utilities for use by friends only
   TypeMapG, CoercionMapG,

   DeBruijn(..), deBruijnize,

   BndrMap, xtBndr, lkBndr,
   VarMap, xtVar, lkVar, lkDFreeVar, xtDFreeVar,

   xtDNamed, lkDNamed

   ) where

-- This module is separate from GHC.Core.Map.Expr to avoid a module loop
-- between GHC.Core.Unify (which depends on this module) and GHC.Core

import GHC.Prelude

import GHC.Core.Type
import GHC.Core.Coercion
import GHC.Core.TyCo.Rep
import GHC.Data.TrieMap

import GHC.Data.FastString
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Var
import GHC.Types.Var.Env
import GHC.Types.Unique.FM
import GHC.Utils.Outputable

import GHC.Data.Maybe
import GHC.Utils.Panic

import qualified Data.Map    as Map
import qualified Data.IntMap as IntMap

import Control.Monad ( (>=>) )

-- NB: Be careful about RULES and type families (#5821).  So we should make sure
-- to specify @Key TypeMapX@ (and not @DeBruijn Type@, the reduced form)

{-# SPECIALIZE lkG :: Key TypeMapX     -> TypeMapG a     -> Maybe a #-}
{-# SPECIALIZE lkG :: Key CoercionMapX -> CoercionMapG a -> Maybe a #-}

{-# SPECIALIZE xtG :: Key TypeMapX     -> XT a -> TypeMapG a -> TypeMapG a #-}
{-# SPECIALIZE xtG :: Key CoercionMapX -> XT a -> CoercionMapG a -> CoercionMapG a #-}

{-# SPECIALIZE mapG :: (a -> b) -> TypeMapG a     -> TypeMapG b #-}
{-# SPECIALIZE mapG :: (a -> b) -> CoercionMapG a -> CoercionMapG b #-}

{-# SPECIALIZE fdG :: (a -> b -> b) -> TypeMapG a     -> b -> b #-}
{-# SPECIALIZE fdG :: (a -> b -> b) -> CoercionMapG a -> b -> b #-}

{-
************************************************************************
*                                                                      *
                   Coercions
*                                                                      *
************************************************************************
-}

-- We should really never care about the contents of a coercion. Instead,
-- just look up the coercion's type.
newtype CoercionMap a = CoercionMap (CoercionMapG a)

instance TrieMap CoercionMap where
   type Key CoercionMap = Coercion
   emptyTM                     = CoercionMap emptyTM
   lookupTM k  (CoercionMap m) = lookupTM (deBruijnize k) m
   alterTM k f (CoercionMap m) = CoercionMap (alterTM (deBruijnize k) f m)
   foldTM k    (CoercionMap m) = foldTM k m
   mapTM f     (CoercionMap m) = CoercionMap (mapTM f m)
   filterTM f  (CoercionMap m) = CoercionMap (filterTM f m)

type CoercionMapG = GenMap CoercionMapX
newtype CoercionMapX a = CoercionMapX (TypeMapX a)

instance TrieMap CoercionMapX where
  type Key CoercionMapX = DeBruijn Coercion
  emptyTM = CoercionMapX emptyTM
  lookupTM = lkC
  alterTM  = xtC
  foldTM f (CoercionMapX core_tm) = foldTM f core_tm
  mapTM f (CoercionMapX core_tm)  = CoercionMapX (mapTM f core_tm)
  filterTM f (CoercionMapX core_tm) = CoercionMapX (filterTM f core_tm)

instance Eq (DeBruijn Coercion) where
  D env1 co1 == D env2 co2
    = D env1 (coercionType co1) ==
      D env2 (coercionType co2)

lkC :: DeBruijn Coercion -> CoercionMapX a -> Maybe a
lkC (D env co) (CoercionMapX core_tm) = lkT (D env $ coercionType co)
                                        core_tm

xtC :: DeBruijn Coercion -> XT a -> CoercionMapX a -> CoercionMapX a
xtC (D env co) f (CoercionMapX m)
  = CoercionMapX (xtT (D env $ coercionType co) f m)

{-
************************************************************************
*                                                                      *
                   Types
*                                                                      *
************************************************************************
-}

-- | @TypeMapG a@ is a map from @DeBruijn Type@ to @a@.  The extended
-- key makes it suitable for recursive traversal, since it can track binders,
-- but it is strictly internal to this module.  If you are including a 'TypeMap'
-- inside another 'TrieMap', this is the type you want. Note that this
-- lookup does not do a kind-check. Thus, all keys in this map must have
-- the same kind. Also note that this map respects the distinction between
-- @Type@ and @Constraint@, despite the fact that they are equivalent type
-- synonyms in Core.
type TypeMapG = GenMap TypeMapX

-- | @TypeMapX a@ is the base map from @DeBruijn Type@ to @a@, but without the
-- 'GenMap' optimization.
data TypeMapX a
  = TM { tm_var    :: VarMap a
       , tm_app    :: TypeMapG (TypeMapG a)
       , tm_tycon  :: DNameEnv a

         -- only InvisArg arrows here
       , tm_funty  :: TypeMapG (TypeMapG (TypeMapG a))
                       -- keyed on the argument, result rep, and result
                       -- constraints are never linear-restricted and are always lifted

       , tm_forall :: TypeMapG (BndrMap a) -- See Note [Binders] in GHC.Core.Map.Expr
       , tm_tylit  :: TyLitMap a
       , tm_coerce :: Maybe a
       }
    -- Note that there is no tyconapp case; see Note [Equality on AppTys] in GHC.Core.Type

-- | Squeeze out any synonyms, and change TyConApps to nested AppTys. Why the
-- last one? See Note [Equality on AppTys] in GHC.Core.Type
--
-- Note, however, that we keep Constraint and Type apart here, despite the fact
-- that they are both synonyms of TYPE 'LiftedRep (see #11715).
--
-- We also keep (Eq a => a) as a FunTy, distinct from ((->) (Eq a) a).
trieMapView :: Type -> Maybe Type
trieMapView ty
  -- First check for TyConApps that need to be expanded to
  -- AppTy chains.
  | Just (tc, tys@(_:_)) <- tcSplitTyConApp_maybe ty
  = Just $ foldl' AppTy (TyConApp tc []) tys

  -- Then resolve any remaining nullary synonyms.
  | Just ty' <- tcView ty = Just ty'
trieMapView _ = Nothing

instance TrieMap TypeMapX where
   type Key TypeMapX = DeBruijn Type
   emptyTM  = emptyT
   lookupTM = lkT
   alterTM  = xtT
   foldTM   = fdT
   mapTM    = mapT
   filterTM = filterT

instance Eq (DeBruijn Type) where
  env_t@(D env t) == env_t'@(D env' t')
    | Just new_t  <- tcView t  = D env new_t == env_t'
    | Just new_t' <- tcView t' = env_t       == D env' new_t'
    | otherwise
    = case (t, t') of
        (CastTy t1 _, _)  -> D env t1 == D env t'
        (_, CastTy t1' _) -> D env t  == D env t1'

        (TyVarTy v, TyVarTy v')
            -> case (lookupCME env v, lookupCME env' v') of
                (Just bv, Just bv') -> bv == bv'
                (Nothing, Nothing)  -> v == v'
                _ -> False
                -- See Note [Equality on AppTys] in GHC.Core.Type
        (AppTy t1 t2, s) | Just (t1', t2') <- repSplitAppTy_maybe s
            -> D env t1 == D env' t1' && D env t2 == D env' t2'
        (s, AppTy t1' t2') | Just (t1, t2) <- repSplitAppTy_maybe s
            -> D env t1 == D env' t1' && D env t2 == D env' t2'
        (FunTy v1 w1 t1 t2, FunTy v1' w1' t1' t2')
            -> v1 == v1' &&
               D env w1 == D env w1' &&
               D env t1 == D env' t1' &&
               D env t2 == D env' t2'
        (TyConApp tc tys, TyConApp tc' tys')
            -> tc == tc' && D env tys == D env' tys'
        (LitTy l, LitTy l')
            -> l == l'
        (ForAllTy (Bndr tv _) ty, ForAllTy (Bndr tv' _) ty')
            -> D env (varType tv)      == D env' (varType tv') &&
               D (extendCME env tv) ty == D (extendCME env' tv') ty'
        (CoercionTy {}, CoercionTy {})
            -> True
        _ -> False

instance {-# OVERLAPPING #-}
         Outputable a => Outputable (TypeMapG a) where
  ppr m = text "TypeMap elts" <+> ppr (foldTM (:) m [])

emptyT :: TypeMapX a
emptyT = TM { tm_var  = emptyTM
            , tm_app  = emptyTM
            , tm_tycon  = emptyDNameEnv
            , tm_funty  = emptyTM
            , tm_forall = emptyTM
            , tm_tylit  = emptyTyLitMap
            , tm_coerce = Nothing }

mapT :: (a->b) -> TypeMapX a -> TypeMapX b
mapT f (TM { tm_var  = tvar, tm_app = tapp, tm_tycon = ttycon
           , tm_funty = tfunty, tm_forall = tforall, tm_tylit = tlit
           , tm_coerce = tcoerce })
  = TM { tm_var    = mapTM f tvar
       , tm_app    = mapTM (mapTM f) tapp
       , tm_tycon  = mapTM f ttycon
       , tm_funty  = mapTM (mapTM (mapTM f)) tfunty
       , tm_forall = mapTM (mapTM f) tforall
       , tm_tylit  = mapTM f tlit
       , tm_coerce = fmap f tcoerce }

-----------------
lkT :: DeBruijn Type -> TypeMapX a -> Maybe a
lkT (D env ty) m = go ty m
  where
    go ty | Just ty' <- trieMapView ty = go ty'
    go (TyVarTy v)                 = tm_var    >.> lkVar env v
    go (AppTy t1 t2)               = tm_app    >.> lkG (D env t1)
                                               >=> lkG (D env t2)
    go (TyConApp tc [])            = tm_tycon  >.> lkDNamed tc
    go ty@(TyConApp _ (_:_))       = pprPanic "lkT TyConApp" (ppr ty)
    go (LitTy l)                   = tm_tylit  >.> lkTyLit l
    go (ForAllTy (Bndr tv _) ty)   = tm_forall >.> lkG (D (extendCME env tv) ty)
                                               >=> lkBndr env tv
    go (FunTy InvisArg _ arg res)
      | Just res_rep <- getRuntimeRep_maybe res
                                   = tm_funty >.> lkG (D env arg)
                                              >=> lkG (D env res_rep)
                                              >=> lkG (D env res)
    go ty@(FunTy {})               = pprPanic "lkT FunTy" (ppr ty)
    go (CastTy t _)                = go t
    go (CoercionTy {})             = tm_coerce

-----------------
xtT :: DeBruijn Type -> XT a -> TypeMapX a -> TypeMapX a
xtT (D env ty) f m | Just ty' <- trieMapView ty = xtT (D env ty') f m

xtT (D env (TyVarTy v))       f m = m { tm_var    = tm_var m |> xtVar env v f }
xtT (D env (AppTy t1 t2))     f m = m { tm_app    = tm_app m |> xtG (D env t1)
                                                            |>> xtG (D env t2) f }
xtT (D _   (TyConApp tc []))  f m = m { tm_tycon  = tm_tycon m |> xtDNamed tc f }
xtT (D env (FunTy InvisArg _ t1 t2)) f m = m { tm_funty = tm_funty m |> xtG (D env t1)
                                                                    |>> xtG (D env t2_rep)
                                                                    |>> xtG (D env t2) f }
  where t2_rep = expectJust "xtT FunTy InvisArg" (getRuntimeRep_maybe t2)
xtT (D _   (LitTy l))         f m = m { tm_tylit  = tm_tylit m |> xtTyLit l f }
xtT (D env (CastTy t _))      f m = xtT (D env t) f m
xtT (D _   (CoercionTy {}))   f m = m { tm_coerce = tm_coerce m |> f }
xtT (D env (ForAllTy (Bndr tv _) ty))  f m
  = m { tm_forall = tm_forall m |> xtG (D (extendCME env tv) ty)
                                |>> xtBndr env tv f }
xtT (D _   ty@(TyConApp _ (_:_))) _ _ = pprPanic "xtT TyConApp" (ppr ty)
xtT (D _   ty@(FunTy {}))         _ _ = pprPanic "xtT FunTy" (ppr ty)

fdT :: (a -> b -> b) -> TypeMapX a -> b -> b
fdT k m = foldTM k (tm_var m)
        . foldTM (foldTM k) (tm_app m)
        . foldTM k (tm_tycon m)
        . foldTM (foldTM (foldTM k)) (tm_funty m)
        . foldTM (foldTM k) (tm_forall m)
        . foldTyLit k (tm_tylit m)
        . foldMaybe k (tm_coerce m)

filterT :: (a -> Bool) -> TypeMapX a -> TypeMapX a
filterT f (TM { tm_var  = tvar, tm_app = tapp, tm_tycon = ttycon
              , tm_funty = tfunty, tm_forall = tforall, tm_tylit = tlit
              , tm_coerce = tcoerce })
  = TM { tm_var    = filterTM f tvar
       , tm_app    = mapTM (filterTM f) tapp
       , tm_tycon  = filterTM f ttycon
       , tm_funty  = mapTM (mapTM (filterTM f)) tfunty
       , tm_forall = mapTM (filterTM f) tforall
       , tm_tylit  = filterTM f tlit
       , tm_coerce = filterMaybe f tcoerce }

------------------------
data TyLitMap a = TLM { tlm_number :: Map.Map Integer a
                      , tlm_string :: UniqFM  FastString a
                      , tlm_char   :: Map.Map Char a
                      }

instance TrieMap TyLitMap where
   type Key TyLitMap = TyLit
   emptyTM  = emptyTyLitMap
   lookupTM = lkTyLit
   alterTM  = xtTyLit
   foldTM   = foldTyLit
   mapTM    = mapTyLit
   filterTM = filterTyLit

emptyTyLitMap :: TyLitMap a
emptyTyLitMap = TLM { tlm_number = Map.empty, tlm_string = emptyUFM, tlm_char = Map.empty }

mapTyLit :: (a->b) -> TyLitMap a -> TyLitMap b
mapTyLit f (TLM { tlm_number = tn, tlm_string = ts, tlm_char = tc })
  = TLM { tlm_number = Map.map f tn, tlm_string = mapUFM f ts, tlm_char = Map.map f tc }

lkTyLit :: TyLit -> TyLitMap a -> Maybe a
lkTyLit l =
  case l of
    NumTyLit n -> tlm_number >.> Map.lookup n
    StrTyLit n -> tlm_string >.> (`lookupUFM` n)
    CharTyLit n -> tlm_char >.> Map.lookup n

xtTyLit :: TyLit -> XT a -> TyLitMap a -> TyLitMap a
xtTyLit l f m =
  case l of
    NumTyLit n ->  m { tlm_number = Map.alter f n (tlm_number m) }
    StrTyLit n ->  m { tlm_string = alterUFM  f (tlm_string m) n }
    CharTyLit n -> m { tlm_char = Map.alter f n (tlm_char m) }

foldTyLit :: (a -> b -> b) -> TyLitMap a -> b -> b
foldTyLit l m = flip (foldUFM l) (tlm_string m)
              . flip (Map.foldr l) (tlm_number m)
              . flip (Map.foldr l) (tlm_char m)

filterTyLit :: (a -> Bool) -> TyLitMap a -> TyLitMap a
filterTyLit f (TLM { tlm_number = tn, tlm_string = ts, tlm_char = tc })
  = TLM { tlm_number = Map.filter f tn, tlm_string = filterUFM f ts, tlm_char = Map.filter f tc }

-------------------------------------------------
-- | @TypeMap a@ is a map from 'Type' to @a@.  If you are a client, this
-- is the type you want. The keys in this map may have different kinds.
newtype TypeMap a = TypeMap (TypeMapG (TypeMapG a))

lkTT :: DeBruijn Type -> TypeMap a -> Maybe a
lkTT (D env ty) (TypeMap m) = lkG (D env $ typeKind ty) m
                          >>= lkG (D env ty)

xtTT :: DeBruijn Type -> XT a -> TypeMap a -> TypeMap a
xtTT (D env ty) f (TypeMap m)
  = TypeMap (m |> xtG (D env $ typeKind ty)
               |>> xtG (D env ty) f)

-- Below are some client-oriented functions which operate on 'TypeMap'.

instance TrieMap TypeMap where
    type Key TypeMap = Type
    emptyTM = TypeMap emptyTM
    lookupTM k m = lkTT (deBruijnize k) m
    alterTM k f m = xtTT (deBruijnize k) f m
    foldTM k (TypeMap m) = foldTM (foldTM k) m
    mapTM f (TypeMap m) = TypeMap (mapTM (mapTM f) m)
    filterTM f (TypeMap m) = TypeMap (mapTM (filterTM f) m)

foldTypeMap :: (a -> b -> b) -> b -> TypeMap a -> b
foldTypeMap k z m = foldTM k m z

emptyTypeMap :: TypeMap a
emptyTypeMap = emptyTM

lookupTypeMap :: TypeMap a -> Type -> Maybe a
lookupTypeMap cm t = lookupTM t cm

extendTypeMap :: TypeMap a -> Type -> a -> TypeMap a
extendTypeMap m t v = alterTM t (const (Just v)) m

lookupTypeMapWithScope :: TypeMap a -> CmEnv -> Type -> Maybe a
lookupTypeMapWithScope m cm t = lkTT (D cm t) m

-- | Extend a 'TypeMap' with a type in the given context.
-- @extendTypeMapWithScope m (mkDeBruijnContext [a,b,c]) t v@ is equivalent to
-- @extendTypeMap m (forall a b c. t) v@, but allows reuse of the context over
-- multiple insertions.
extendTypeMapWithScope :: TypeMap a -> CmEnv -> Type -> a -> TypeMap a
extendTypeMapWithScope m cm t v = xtTT (D cm t) (const (Just v)) m

-- | Construct a deBruijn environment with the given variables in scope.
-- e.g. @mkDeBruijnEnv [a,b,c]@ constructs a context @forall a b c.@
mkDeBruijnContext :: [Var] -> CmEnv
mkDeBruijnContext = extendCMEs emptyCME

-- | A 'LooseTypeMap' doesn't do a kind-check. Thus, when lookup up (t |> g),
-- you'll find entries inserted under (t), even if (g) is non-reflexive.
newtype LooseTypeMap a
  = LooseTypeMap (TypeMapG a)

instance TrieMap LooseTypeMap where
  type Key LooseTypeMap = Type
  emptyTM = LooseTypeMap emptyTM
  lookupTM k (LooseTypeMap m) = lookupTM (deBruijnize k) m
  alterTM k f (LooseTypeMap m) = LooseTypeMap (alterTM (deBruijnize k) f m)
  foldTM f (LooseTypeMap m) = foldTM f m
  mapTM f (LooseTypeMap m) = LooseTypeMap (mapTM f m)
  filterTM f (LooseTypeMap m) = LooseTypeMap (filterTM f m)

{-
************************************************************************
*                                                                      *
                   Variables
*                                                                      *
************************************************************************
-}

type BoundVar = Int  -- Bound variables are deBruijn numbered
type BoundVarMap a = IntMap.IntMap a

data CmEnv = CME { cme_next :: !BoundVar
                 , cme_env  :: VarEnv BoundVar }

emptyCME :: CmEnv
emptyCME = CME { cme_next = 0, cme_env = emptyVarEnv }

extendCME :: CmEnv -> Var -> CmEnv
extendCME (CME { cme_next = bv, cme_env = env }) v
  = CME { cme_next = bv+1, cme_env = extendVarEnv env v bv }

extendCMEs :: CmEnv -> [Var] -> CmEnv
extendCMEs env vs = foldl' extendCME env vs

lookupCME :: CmEnv -> Var -> Maybe BoundVar
lookupCME (CME { cme_env = env }) v = lookupVarEnv env v

-- | @DeBruijn a@ represents @a@ modulo alpha-renaming.  This is achieved
-- by equipping the value with a 'CmEnv', which tracks an on-the-fly deBruijn
-- numbering.  This allows us to define an 'Eq' instance for @DeBruijn a@, even
-- if this was not (easily) possible for @a@.  Note: we purposely don't
-- export the constructor.  Make a helper function if you find yourself
-- needing it.
data DeBruijn a = D CmEnv a

-- | Synthesizes a @DeBruijn a@ from an @a@, by assuming that there are no
-- bound binders (an empty 'CmEnv').  This is usually what you want if there
-- isn't already a 'CmEnv' in scope.
deBruijnize :: a -> DeBruijn a
deBruijnize = D emptyCME

instance Eq (DeBruijn a) => Eq (DeBruijn [a]) where
    D _   []     == D _    []       = True
    D env (x:xs) == D env' (x':xs') = D env x  == D env' x' &&
                                      D env xs == D env' xs'
    _            == _               = False

instance Eq (DeBruijn a) => Eq (DeBruijn (Maybe a)) where
    D _   Nothing  == D _    Nothing   = True
    D env (Just x) == D env' (Just x') = D env x  == D env' x'
    _              == _                = False

--------- Variable binders -------------

-- | A 'BndrMap' is a 'TypeMapG' which allows us to distinguish between
-- binding forms whose binders have different types.  For example,
-- if we are doing a 'TrieMap' lookup on @\(x :: Int) -> ()@, we should
-- not pick up an entry in the 'TrieMap' for @\(x :: Bool) -> ()@:
-- we can disambiguate this by matching on the type (or kind, if this
-- a binder in a type) of the binder.
--
-- We also need to do the same for multiplicity! Which, since multiplicities are
-- encoded simply as a 'Type', amounts to have a Trie for a pair of types. Tries
-- of pairs are composition.
data BndrMap a = BndrMap (TypeMapG (MaybeMap TypeMapG a))

instance TrieMap BndrMap where
   type Key BndrMap = Var
   emptyTM  = BndrMap emptyTM
   lookupTM = lkBndr emptyCME
   alterTM  = xtBndr emptyCME
   foldTM   = fdBndrMap
   mapTM    = mapBndrMap
   filterTM = ftBndrMap

mapBndrMap :: (a -> b) -> BndrMap a -> BndrMap b
mapBndrMap f (BndrMap tm) = BndrMap (mapTM (mapTM f) tm)

fdBndrMap :: (a -> b -> b) -> BndrMap a -> b -> b
fdBndrMap f (BndrMap tm) = foldTM (foldTM f) tm


-- We need to use 'BndrMap' for 'Coercion', 'CoreExpr' AND 'Type', since all
-- of these data types have binding forms.

lkBndr :: CmEnv -> Var -> BndrMap a -> Maybe a
lkBndr env v (BndrMap tymap) = do
  multmap <- lkG (D env (varType v)) tymap
  lookupTM (D env <$> varMultMaybe v) multmap


xtBndr :: forall a . CmEnv -> Var -> XT a -> BndrMap a -> BndrMap a
xtBndr env v xt (BndrMap tymap)  =
  BndrMap (tymap |> xtG (D env (varType v)) |>> (alterTM (D env <$> varMultMaybe v) xt))

ftBndrMap :: (a -> Bool) -> BndrMap a -> BndrMap a
ftBndrMap f (BndrMap tm) = BndrMap (mapTM (filterTM f) tm)

--------- Variable occurrence -------------
data VarMap a = VM { vm_bvar   :: BoundVarMap a  -- Bound variable
                   , vm_fvar   :: DVarEnv a }      -- Free variable

instance TrieMap VarMap where
   type Key VarMap = Var
   emptyTM  = VM { vm_bvar = IntMap.empty, vm_fvar = emptyDVarEnv }
   lookupTM = lkVar emptyCME
   alterTM  = xtVar emptyCME
   foldTM   = fdVar
   mapTM    = mapVar
   filterTM = ftVar

mapVar :: (a->b) -> VarMap a -> VarMap b
mapVar f (VM { vm_bvar = bv, vm_fvar = fv })
  = VM { vm_bvar = mapTM f bv, vm_fvar = mapTM f fv }

lkVar :: CmEnv -> Var -> VarMap a -> Maybe a
lkVar env v
  | Just bv <- lookupCME env v = vm_bvar >.> lookupTM bv
  | otherwise                  = vm_fvar >.> lkDFreeVar v

xtVar :: CmEnv -> Var -> XT a -> VarMap a -> VarMap a
xtVar env v f m
  | Just bv <- lookupCME env v = m { vm_bvar = vm_bvar m |> alterTM bv f }
  | otherwise                  = m { vm_fvar = vm_fvar m |> xtDFreeVar v f }

fdVar :: (a -> b -> b) -> VarMap a -> b -> b
fdVar k m = foldTM k (vm_bvar m)
          . foldTM k (vm_fvar m)

lkDFreeVar :: Var -> DVarEnv a -> Maybe a
lkDFreeVar var env = lookupDVarEnv env var

xtDFreeVar :: Var -> XT a -> DVarEnv a -> DVarEnv a
xtDFreeVar v f m = alterDVarEnv f m v

ftVar :: (a -> Bool) -> VarMap a -> VarMap a
ftVar f (VM { vm_bvar = bv, vm_fvar = fv })
  = VM { vm_bvar = filterTM f bv, vm_fvar = filterTM f fv }

-------------------------------------------------
lkDNamed :: NamedThing n => n -> DNameEnv a -> Maybe a
lkDNamed n env = lookupDNameEnv env (getName n)

xtDNamed :: NamedThing n => n -> XT a -> DNameEnv a -> DNameEnv a
xtDNamed tc f m = alterDNameEnv f m (getName tc)

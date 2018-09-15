{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module CoreMap(
   -- * Maps over Core expressions
   CoreMap, emptyCoreMap, extendCoreMap, lookupCoreMap, foldCoreMap,
   -- * Maps over 'Type's
   TypeMap, emptyTypeMap, extendTypeMap, lookupTypeMap, foldTypeMap,
   LooseTypeMap,
   -- ** With explicit scoping
   CmEnv, lookupCME, extendTypeMapWithScope, lookupTypeMapWithScope,
   mkDeBruijnContext,
   -- * Maps over 'Maybe' values
   MaybeMap,
   -- * Maps over 'List' values
   ListMap,
   -- * Maps over 'Literal's
   LiteralMap,
   -- * Map for compressing leaves. See Note [Compressed TrieMap]
   GenMap,
   -- * 'TrieMap' class
   TrieMap(..), insertTM, deleteTM,
   lkDFreeVar, xtDFreeVar,
   lkDNamed, xtDNamed,
   (>.>), (|>), (|>>),
 ) where

import GhcPrelude

import TrieMap
import CoreSyn
import Coercion
import Name
import Type
import TyCoRep
import Var
import FastString(FastString)
import Util

import qualified Data.Map    as Map
import qualified Data.IntMap as IntMap
import VarEnv
import NameEnv
import Outputable
import Control.Monad( (>=>) )

{-
This module implements TrieMaps over Core related data structures
like CoreExpr or Type. It is built on the Tries from the TrieMap
module.

The code is very regular and boilerplate-like, but there is
some neat handling of *binders*.  In effect they are deBruijn
numbered on the fly.


-}

----------------------
-- Recall that
--   Control.Monad.(>=>) :: (a -> Maybe b) -> (b -> Maybe c) -> a -> Maybe c

-- NB: Be careful about RULES and type families (#5821).  So we should make sure
-- to specify @Key TypeMapX@ (and not @DeBruijn Type@, the reduced form)

-- The CoreMap makes heavy use of GenMap. However the CoreMap Types are not
-- known when defining GenMap so we can only specialize them here.

{-# SPECIALIZE lkG :: Key TypeMapX     -> TypeMapG a     -> Maybe a #-}
{-# SPECIALIZE lkG :: Key CoercionMapX -> CoercionMapG a -> Maybe a #-}
{-# SPECIALIZE lkG :: Key CoreMapX     -> CoreMapG a     -> Maybe a #-}


{-# SPECIALIZE xtG :: Key TypeMapX     -> XT a -> TypeMapG a -> TypeMapG a #-}
{-# SPECIALIZE xtG :: Key CoercionMapX -> XT a -> CoercionMapG a -> CoercionMapG a #-}
{-# SPECIALIZE xtG :: Key CoreMapX     -> XT a -> CoreMapG a -> CoreMapG a #-}

{-# SPECIALIZE mapG :: (a -> b) -> TypeMapG a     -> TypeMapG b #-}
{-# SPECIALIZE mapG :: (a -> b) -> CoercionMapG a -> CoercionMapG b #-}
{-# SPECIALIZE mapG :: (a -> b) -> CoreMapG a     -> CoreMapG b #-}

{-# SPECIALIZE fdG :: (a -> b -> b) -> TypeMapG a     -> b -> b #-}
{-# SPECIALIZE fdG :: (a -> b -> b) -> CoercionMapG a -> b -> b #-}
{-# SPECIALIZE fdG :: (a -> b -> b) -> CoreMapG a     -> b -> b #-}


{-
************************************************************************
*                                                                      *
                   CoreMap
*                                                                      *
************************************************************************
-}

lkDNamed :: NamedThing n => n -> DNameEnv a -> Maybe a
lkDNamed n env = lookupDNameEnv env (getName n)

xtDNamed :: NamedThing n => n -> XT a -> DNameEnv a -> DNameEnv a
xtDNamed tc f m = alterDNameEnv f m (getName tc)


{-
Note [Binders]
~~~~~~~~~~~~~~
 * In general we check binders as late as possible because types are
   less likely to differ than expression structure.  That's why
      cm_lam :: CoreMapG (TypeMapG a)
   rather than
      cm_lam :: TypeMapG (CoreMapG a)

 * We don't need to look at the type of some binders, notably
     - the case binder in (Case _ b _ _)
     - the binders in an alternative
   because they are totally fixed by the context

Note [Empty case alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* For a key (Case e b ty (alt:alts))  we don't need to look the return type
  'ty', because every alternative has that type.

* For a key (Case e b ty []) we MUST look at the return type 'ty', because
  otherwise (Case (error () "urk") _ Int  []) would compare equal to
            (Case (error () "urk") _ Bool [])
  which is utterly wrong (Trac #6097)

We could compare the return type regardless, but the wildly common case
is that it's unnecessary, so we have two fields (cm_case and cm_ecase)
for the two possibilities.  Only cm_ecase looks at the type.

See also Note [Empty case alternatives] in CoreSyn.
-}

-- | @CoreMap a@ is a map from 'CoreExpr' to @a@.  If you are a client, this
-- is the type you want.
newtype CoreMap a = CoreMap (CoreMapG a)

instance TrieMap CoreMap where
    type Key CoreMap = CoreExpr
    emptyTM = CoreMap emptyTM
    lookupTM k (CoreMap m) = lookupTM (deBruijnize k) m
    alterTM k f (CoreMap m) = CoreMap (alterTM (deBruijnize k) f m)
    foldTM k (CoreMap m) = foldTM k m
    mapTM f (CoreMap m) = CoreMap (mapTM f m)

-- | @CoreMapG a@ is a map from @DeBruijn CoreExpr@ to @a@.  The extended
-- key makes it suitable for recursive traversal, since it can track binders,
-- but it is strictly internal to this module.  If you are including a 'CoreMap'
-- inside another 'TrieMap', this is the type you want.
type CoreMapG = GenMap CoreMapX

-- | @CoreMapX a@ is the base map from @DeBruijn CoreExpr@ to @a@, but without
-- the 'GenMap' optimization.
data CoreMapX a
  = CM { cm_var   :: VarMap a
       , cm_lit   :: LiteralMap a
       , cm_co    :: CoercionMapG a
       , cm_type  :: TypeMapG a
       , cm_cast  :: CoreMapG (CoercionMapG a)
       , cm_tick  :: CoreMapG (TickishMap a)
       , cm_app   :: CoreMapG (CoreMapG a)
       , cm_lam   :: CoreMapG (BndrMap a)    -- Note [Binders]
       , cm_letn  :: CoreMapG (CoreMapG (BndrMap a))
       , cm_letr  :: ListMap CoreMapG (CoreMapG (ListMap BndrMap a))
       , cm_case  :: CoreMapG (ListMap AltMap a)
       , cm_ecase :: CoreMapG (TypeMapG a)    -- Note [Empty case alternatives]
     }

instance Eq (DeBruijn CoreExpr) where
  D env1 e1 == D env2 e2 = go e1 e2 where
    go (Var v1) (Var v2) = case (lookupCME env1 v1, lookupCME env2 v2) of
                            (Just b1, Just b2) -> b1 == b2
                            (Nothing, Nothing) -> v1 == v2
                            _ -> False
    go (Lit lit1)    (Lit lit2)      = lit1 == lit2
    go (Type t1)    (Type t2)        = D env1 t1 == D env2 t2
    go (Coercion co1) (Coercion co2) = D env1 co1 == D env2 co2
    go (Cast e1 co1) (Cast e2 co2) = D env1 co1 == D env2 co2 && go e1 e2
    go (App f1 a1)   (App f2 a2)   = go f1 f2 && go a1 a2
    -- This seems a bit dodgy, see 'eqTickish'
    go (Tick n1 e1)  (Tick n2 e2)  = n1 == n2 && go e1 e2

    go (Lam b1 e1)  (Lam b2 e2)
      =  D env1 (varType b1) == D env2 (varType b2)
      && D (extendCME env1 b1) e1 == D (extendCME env2 b2) e2

    go (Let (NonRec v1 r1) e1) (Let (NonRec v2 r2) e2)
      =  go r1 r2
      && D (extendCME env1 v1) e1 == D (extendCME env2 v2) e2

    go (Let (Rec ps1) e1) (Let (Rec ps2) e2)
      = equalLength ps1 ps2
      && D env1' rs1 == D env2' rs2
      && D env1' e1  == D env2' e2
      where
        (bs1,rs1) = unzip ps1
        (bs2,rs2) = unzip ps2
        env1' = extendCMEs env1 bs1
        env2' = extendCMEs env2 bs2

    go (Case e1 b1 t1 a1) (Case e2 b2 t2 a2)
      | null a1   -- See Note [Empty case alternatives]
      = null a2 && go e1 e2 && D env1 t1 == D env2 t2
      | otherwise
      =  go e1 e2 && D (extendCME env1 b1) a1 == D (extendCME env2 b2) a2

    go _ _ = False

emptyE :: CoreMapX a
emptyE = CM { cm_var = emptyTM, cm_lit = emptyTM
            , cm_co = emptyTM, cm_type = emptyTM
            , cm_cast = emptyTM, cm_app = emptyTM
            , cm_lam = emptyTM, cm_letn = emptyTM
            , cm_letr = emptyTM, cm_case = emptyTM
            , cm_ecase = emptyTM, cm_tick = emptyTM }

instance TrieMap CoreMapX where
   type Key CoreMapX = DeBruijn CoreExpr
   emptyTM  = emptyE
   lookupTM = lkE
   alterTM  = xtE
   foldTM   = fdE
   mapTM    = mapE

--------------------------
mapE :: (a->b) -> CoreMapX a -> CoreMapX b
mapE f (CM { cm_var = cvar, cm_lit = clit
           , cm_co = cco, cm_type = ctype
           , cm_cast = ccast , cm_app = capp
           , cm_lam = clam, cm_letn = cletn
           , cm_letr = cletr, cm_case = ccase
           , cm_ecase = cecase, cm_tick = ctick })
  = CM { cm_var = mapTM f cvar, cm_lit = mapTM f clit
       , cm_co = mapTM f cco, cm_type = mapTM f ctype
       , cm_cast = mapTM (mapTM f) ccast, cm_app = mapTM (mapTM f) capp
       , cm_lam = mapTM (mapTM f) clam, cm_letn = mapTM (mapTM (mapTM f)) cletn
       , cm_letr = mapTM (mapTM (mapTM f)) cletr, cm_case = mapTM (mapTM f) ccase
       , cm_ecase = mapTM (mapTM f) cecase, cm_tick = mapTM (mapTM f) ctick }

--------------------------
lookupCoreMap :: CoreMap a -> CoreExpr -> Maybe a
lookupCoreMap cm e = lookupTM e cm

extendCoreMap :: CoreMap a -> CoreExpr -> a -> CoreMap a
extendCoreMap m e v = alterTM e (\_ -> Just v) m

foldCoreMap :: (a -> b -> b) -> b -> CoreMap a -> b
foldCoreMap k z m = foldTM k m z

emptyCoreMap :: CoreMap a
emptyCoreMap = emptyTM

instance Outputable a => Outputable (CoreMap a) where
  ppr m = text "CoreMap elts" <+> ppr (foldTM (:) m [])

-------------------------
fdE :: (a -> b -> b) -> CoreMapX a -> b -> b
fdE k m
  = foldTM k (cm_var m)
  . foldTM k (cm_lit m)
  . foldTM k (cm_co m)
  . foldTM k (cm_type m)
  . foldTM (foldTM k) (cm_cast m)
  . foldTM (foldTM k) (cm_tick m)
  . foldTM (foldTM k) (cm_app m)
  . foldTM (foldTM k) (cm_lam m)
  . foldTM (foldTM (foldTM k)) (cm_letn m)
  . foldTM (foldTM (foldTM k)) (cm_letr m)
  . foldTM (foldTM k) (cm_case m)
  . foldTM (foldTM k) (cm_ecase m)

-- lkE: lookup in trie for expressions
lkE :: DeBruijn CoreExpr -> CoreMapX a -> Maybe a
lkE (D env expr) cm = go expr cm
  where
    go (Var v)              = cm_var  >.> lkVar env v
    go (Lit l)              = cm_lit  >.> lookupTM l
    go (Type t)             = cm_type >.> lkG (D env t)
    go (Coercion c)         = cm_co   >.> lkG (D env c)
    go (Cast e c)           = cm_cast >.> lkG (D env e) >=> lkG (D env c)
    go (Tick tickish e)     = cm_tick >.> lkG (D env e) >=> lkTickish tickish
    go (App e1 e2)          = cm_app  >.> lkG (D env e2) >=> lkG (D env e1)
    go (Lam v e)            = cm_lam  >.> lkG (D (extendCME env v) e)
                              >=> lkBndr env v
    go (Let (NonRec b r) e) = cm_letn >.> lkG (D env r)
                              >=> lkG (D (extendCME env b) e) >=> lkBndr env b
    go (Let (Rec prs) e)    = let (bndrs,rhss) = unzip prs
                                  env1 = extendCMEs env bndrs
                              in cm_letr
                                 >.> lkList (lkG . D env1) rhss
                                 >=> lkG (D env1 e)
                                 >=> lkList (lkBndr env1) bndrs
    go (Case e b ty as)     -- See Note [Empty case alternatives]
               | null as    = cm_ecase >.> lkG (D env e) >=> lkG (D env ty)
               | otherwise  = cm_case >.> lkG (D env e)
                              >=> lkList (lkA (extendCME env b)) as

xtE :: DeBruijn CoreExpr -> XT a -> CoreMapX a -> CoreMapX a
xtE (D env (Var v))              f m = m { cm_var  = cm_var m
                                                 |> xtVar env v f }
xtE (D env (Type t))             f m = m { cm_type = cm_type m
                                                 |> xtG (D env t) f }
xtE (D env (Coercion c))         f m = m { cm_co   = cm_co m
                                                 |> xtG (D env c) f }
xtE (D _   (Lit l))              f m = m { cm_lit  = cm_lit m  |> alterTM l f }
xtE (D env (Cast e c))           f m = m { cm_cast = cm_cast m |> xtG (D env e)
                                                 |>> xtG (D env c) f }
xtE (D env (Tick t e))           f m = m { cm_tick = cm_tick m |> xtG (D env e)
                                                 |>> xtTickish t f }
xtE (D env (App e1 e2))          f m = m { cm_app = cm_app m |> xtG (D env e2)
                                                 |>> xtG (D env e1) f }
xtE (D env (Lam v e))            f m = m { cm_lam = cm_lam m
                                                 |> xtG (D (extendCME env v) e)
                                                 |>> xtBndr env v f }
xtE (D env (Let (NonRec b r) e)) f m = m { cm_letn = cm_letn m
                                                 |> xtG (D (extendCME env b) e)
                                                 |>> xtG (D env r)
                                                 |>> xtBndr env b f }
xtE (D env (Let (Rec prs) e))    f m = m { cm_letr =
                                              let (bndrs,rhss) = unzip prs
                                                  env1 = extendCMEs env bndrs
                                              in cm_letr m
                                                 |>  xtList (xtG . D env1) rhss
                                                 |>> xtG (D env1 e)
                                                 |>> xtList (xtBndr env1)
                                                            bndrs f }
xtE (D env (Case e b ty as))     f m
                     | null as   = m { cm_ecase = cm_ecase m |> xtG (D env e)
                                                 |>> xtG (D env ty) f }
                     | otherwise = m { cm_case = cm_case m |> xtG (D env e)
                                                 |>> let env1 = extendCME env b
                                                     in xtList (xtA env1) as f }

-- TODO: this seems a bit dodgy, see 'eqTickish'
type TickishMap a = Map.Map (Tickish Id) a
lkTickish :: Tickish Id -> TickishMap a -> Maybe a
lkTickish = lookupTM

xtTickish :: Tickish Id -> XT a -> TickishMap a -> TickishMap a
xtTickish = alterTM

------------------------
data AltMap a   -- A single alternative
  = AM { am_deflt :: CoreMapG a
       , am_data  :: DNameEnv (CoreMapG a)
       , am_lit   :: LiteralMap (CoreMapG a) }

instance TrieMap AltMap where
   type Key AltMap = CoreAlt
   emptyTM  = AM { am_deflt = emptyTM
                 , am_data = emptyDNameEnv
                 , am_lit  = emptyTM }
   lookupTM = lkA emptyCME
   alterTM  = xtA emptyCME
   foldTM   = fdA
   mapTM    = mapA

instance Eq (DeBruijn CoreAlt) where
  D env1 a1 == D env2 a2 = go a1 a2 where
    go (DEFAULT, _, rhs1) (DEFAULT, _, rhs2)
        = D env1 rhs1 == D env2 rhs2
    go (LitAlt lit1, _, rhs1) (LitAlt lit2, _, rhs2)
        = lit1 == lit2 && D env1 rhs1 == D env2 rhs2
    go (DataAlt dc1, bs1, rhs1) (DataAlt dc2, bs2, rhs2)
        = dc1 == dc2 &&
          D (extendCMEs env1 bs1) rhs1 == D (extendCMEs env2 bs2) rhs2
    go _ _ = False

mapA :: (a->b) -> AltMap a -> AltMap b
mapA f (AM { am_deflt = adeflt, am_data = adata, am_lit = alit })
  = AM { am_deflt = mapTM f adeflt
       , am_data = mapTM (mapTM f) adata
       , am_lit = mapTM (mapTM f) alit }

lkA :: CmEnv -> CoreAlt -> AltMap a -> Maybe a
lkA env (DEFAULT,    _, rhs)  = am_deflt >.> lkG (D env rhs)
lkA env (LitAlt lit, _, rhs)  = am_lit >.> lookupTM lit >=> lkG (D env rhs)
lkA env (DataAlt dc, bs, rhs) = am_data >.> lkDNamed dc
                                        >=> lkG (D (extendCMEs env bs) rhs)

xtA :: CmEnv -> CoreAlt -> XT a -> AltMap a -> AltMap a
xtA env (DEFAULT, _, rhs)    f m =
    m { am_deflt = am_deflt m |> xtG (D env rhs) f }
xtA env (LitAlt l, _, rhs)   f m =
    m { am_lit   = am_lit m   |> alterTM l |>> xtG (D env rhs) f }
xtA env (DataAlt d, bs, rhs) f m =
    m { am_data  = am_data m  |> xtDNamed d
                             |>> xtG (D (extendCMEs env bs) rhs) f }

fdA :: (a -> b -> b) -> AltMap a -> b -> b
fdA k m = foldTM k (am_deflt m)
        . foldTM (foldTM k) (am_data m)
        . foldTM (foldTM k) (am_lit m)

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

type CoercionMapG = GenMap CoercionMapX
newtype CoercionMapX a = CoercionMapX (TypeMapX a)

instance TrieMap CoercionMapX where
  type Key CoercionMapX = DeBruijn Coercion
  emptyTM = CoercionMapX emptyTM
  lookupTM = lkC
  alterTM  = xtC
  foldTM f (CoercionMapX core_tm) = foldTM f core_tm
  mapTM f (CoercionMapX core_tm)  = CoercionMapX (mapTM f core_tm)

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
       , tm_forall :: TypeMapG (BndrMap a) -- See Note [Binders]
       , tm_tylit  :: TyLitMap a
       , tm_coerce :: Maybe a
       }
    -- Note that there is no tyconapp case; see Note [Equality on AppTys] in Type

-- | Squeeze out any synonyms, and change TyConApps to nested AppTys. Why the
-- last one? See Note [Equality on AppTys] in Type
--
-- Note, however, that we keep Constraint and Type apart here, despite the fact
-- that they are both synonyms of TYPE 'LiftedRep (see #11715).
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
                -- See Note [Equality on AppTys] in Type
        (AppTy t1 t2, s) | Just (t1', t2') <- repSplitAppTy_maybe s
            -> D env t1 == D env' t1' && D env t2 == D env' t2'
        (s, AppTy t1' t2') | Just (t1, t2) <- repSplitAppTy_maybe s
            -> D env t1 == D env' t1' && D env t2 == D env' t2'
        (FunTy t1 t2, FunTy t1' t2')
            -> D env t1 == D env' t1' && D env t2 == D env' t2'
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
            , tm_forall = emptyTM
            , tm_tylit  = emptyTyLitMap
            , tm_coerce = Nothing }

mapT :: (a->b) -> TypeMapX a -> TypeMapX b
mapT f (TM { tm_var  = tvar, tm_app = tapp, tm_tycon = ttycon
           , tm_forall = tforall, tm_tylit = tlit
           , tm_coerce = tcoerce })
  = TM { tm_var    = mapTM f tvar
       , tm_app    = mapTM (mapTM f) tapp
       , tm_tycon  = mapTM f ttycon
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
        . foldTM (foldTM k) (tm_forall m)
        . foldTyLit k (tm_tylit m)
        . foldMaybe k (tm_coerce m)

------------------------
data TyLitMap a = TLM { tlm_number :: Map.Map Integer a
                      , tlm_string :: Map.Map FastString a
                      }

instance TrieMap TyLitMap where
   type Key TyLitMap = TyLit
   emptyTM  = emptyTyLitMap
   lookupTM = lkTyLit
   alterTM  = xtTyLit
   foldTM   = foldTyLit
   mapTM    = mapTyLit

emptyTyLitMap :: TyLitMap a
emptyTyLitMap = TLM { tlm_number = Map.empty, tlm_string = Map.empty }

mapTyLit :: (a->b) -> TyLitMap a -> TyLitMap b
mapTyLit f (TLM { tlm_number = tn, tlm_string = ts })
  = TLM { tlm_number = Map.map f tn, tlm_string = Map.map f ts }

lkTyLit :: TyLit -> TyLitMap a -> Maybe a
lkTyLit l =
  case l of
    NumTyLit n -> tlm_number >.> Map.lookup n
    StrTyLit n -> tlm_string >.> Map.lookup n

xtTyLit :: TyLit -> XT a -> TyLitMap a -> TyLitMap a
xtTyLit l f m =
  case l of
    NumTyLit n -> m { tlm_number = tlm_number m |> Map.alter f n }
    StrTyLit n -> m { tlm_string = tlm_string m |> Map.alter f n }

foldTyLit :: (a -> b -> b) -> TyLitMap a -> b -> b
foldTyLit l m = flip (Map.foldr l) (tlm_string m)
              . flip (Map.foldr l) (tlm_number m)

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

--------- Variable binders -------------

-- | A 'BndrMap' is a 'TypeMapG' which allows us to distinguish between
-- binding forms whose binders have different types.  For example,
-- if we are doing a 'TrieMap' lookup on @\(x :: Int) -> ()@, we should
-- not pick up an entry in the 'TrieMap' for @\(x :: Bool) -> ()@:
-- we can disambiguate this by matching on the type (or kind, if this
-- a binder in a type) of the binder.
type BndrMap = TypeMapG

-- Note [Binders]
-- ~~~~~~~~~~~~~~
-- We need to use 'BndrMap' for 'Coercion', 'CoreExpr' AND 'Type', since all
-- of these data types have binding forms.

lkBndr :: CmEnv -> Var -> BndrMap a -> Maybe a
lkBndr env v m = lkG (D env (varType v)) m

xtBndr :: CmEnv -> Var -> XT a -> BndrMap a -> BndrMap a
xtBndr env v f = xtG (D env (varType v)) f

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

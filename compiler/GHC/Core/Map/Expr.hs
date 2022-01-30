{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# OPTIONS_GHC -Wno-orphans #-}
 -- Eq (DeBruijn CoreExpr) and Eq (DeBruijn CoreAlt)

module GHC.Core.Map.Expr (
   -- * Maps over Core expressions
   CoreMap, emptyCoreMap, extendCoreMap, lookupCoreMap, foldCoreMap,
   -- * Alpha equality
   eqDeBruijnExpr, eqCoreExpr,
   -- * 'TrieMap' class reexports
   TrieMap(..), insertTM, deleteTM,
   lkDFreeVar, xtDFreeVar,
   lkDNamed, xtDNamed,
   (>.>), (|>), (|>>),
 ) where

import GHC.Prelude

import GHC.Data.TrieMap
import GHC.Core.Map.Type
import GHC.Core
import GHC.Core.Type
import GHC.Types.Tickish
import GHC.Types.Var

import GHC.Utils.Misc
import GHC.Utils.Outputable

import qualified Data.Map    as Map
import GHC.Types.Name.Env
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

-- The CoreMap makes heavy use of GenMap. However the CoreMap Types are not
-- known when defining GenMap so we can only specialize them here.

{-# SPECIALIZE lkG :: Key CoreMapX     -> CoreMapG a     -> Maybe a #-}
{-# SPECIALIZE xtG :: Key CoreMapX     -> XT a -> CoreMapG a -> CoreMapG a #-}
{-# SPECIALIZE mapG :: (a -> b) -> CoreMapG a     -> CoreMapG b #-}
{-# SPECIALIZE fdG :: (a -> b -> b) -> CoreMapG a     -> b -> b #-}


{-
************************************************************************
*                                                                      *
                   CoreMap
*                                                                      *
************************************************************************
-}

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
  which is utterly wrong (#6097)

We could compare the return type regardless, but the wildly common case
is that it's unnecessary, so we have two fields (cm_case and cm_ecase)
for the two possibilities.  Only cm_ecase looks at the type.

See also Note [Empty case alternatives] in GHC.Core.
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
    filterTM f (CoreMap m) = CoreMap (filterTM f m)

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
    (==) = eqDeBruijnExpr

eqDeBruijnExpr :: DeBruijn CoreExpr -> DeBruijn CoreExpr -> Bool
eqDeBruijnExpr (D env1 e1) (D env2 e2) = go e1 e2 where
    go (Var v1) (Var v2) = eqDeBruijnVar (D env1 v1) (D env2 v2)
    go (Lit lit1)    (Lit lit2)      = lit1 == lit2
    -- See Note [Using tcView inside eqDeBruijnType] in GHC.Core.Map.Type
    go (Type t1)    (Type t2)        = eqDeBruijnType (D env1 t1) (D env2 t2)
    -- See Note [Alpha-equality for Coercion arguments]
    go (Coercion {}) (Coercion {}) = True
    go (Cast e1 co1) (Cast e2 co2) = D env1 co1 == D env2 co2 && go e1 e2
    go (App f1 a1)   (App f2 a2)   = go f1 f2 && go a1 a2
    go (Tick n1 e1) (Tick n2 e2)
      =  eqDeBruijnTickish (D env1 n1) (D env2 n2)
      && go e1 e2

    go (Lam b1 e1)  (Lam b2 e2)
          -- See Note [Using tcView inside eqDeBruijnType] in GHC.Core.Map.Type
      =  eqDeBruijnType (D env1 (varType b1)) (D env2 (varType b2))
      && D env1 (varMultMaybe b1) == D env2 (varMultMaybe b2)
      && eqDeBruijnExpr (D (extendCME env1 b1) e1) (D (extendCME env2 b2) e2)

    go (Let (NonRec v1 r1) e1) (Let (NonRec v2 r2) e2)
      =  go r1 r2 -- See Note [Alpha-equality for let-bindings]
      && eqDeBruijnExpr (D (extendCME env1 v1) e1) (D (extendCME env2 v2) e2)

    go (Let (Rec ps1) e1) (Let (Rec ps2) e2)
      = equalLength ps1 ps2
      -- See Note [Alpha-equality for let-bindings]
      && all2 (\b1 b2 -> -- See Note [Using tcView inside eqDeBruijnType] in
                         -- GHC.Core.Map.Type
                         eqDeBruijnType (D env1 (varType b1))
                                        (D env2 (varType b2)))
              bs1 bs2
      && D env1' rs1 == D env2' rs2
      && eqDeBruijnExpr (D env1' e1) (D env2' e2)
      where
        (bs1,rs1) = unzip ps1
        (bs2,rs2) = unzip ps2
        env1' = extendCMEs env1 bs1
        env2' = extendCMEs env2 bs2

    go (Case e1 b1 t1 a1) (Case e2 b2 t2 a2)
      | null a1   -- See Note [Empty case alternatives]
      = null a2 && go e1 e2 && D env1 t1 == D env2 t2
      | otherwise
      = go e1 e2 && D (extendCME env1 b1) a1 == D (extendCME env2 b2) a2

    go _ _ = False

eqDeBruijnTickish :: DeBruijn CoreTickish -> DeBruijn CoreTickish -> Bool
eqDeBruijnTickish (D env1 t1) (D env2 t2) = go t1 t2 where
    go (Breakpoint lext lid lids) (Breakpoint rext rid rids)
        =  lid == rid
        && D env1 lids == D env2 rids
        && lext == rext
    go l r = l == r

-- Compares for equality, modulo alpha
eqCoreExpr :: CoreExpr -> CoreExpr -> Bool
eqCoreExpr e1 e2 = eqDeBruijnExpr (deBruijnize e1) (deBruijnize e2)

{- Note [Alpha-equality for Coercion arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The 'Coercion' constructor only appears in argument positions, and so, if the
functions are equal, then the arguments must have equal types. Because the
comparison for coercions (correctly) checks only their types, checking for
alpha-equality of the coercions is redundant.
-}

{- Note [Alpha-equality for let-bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For /recursive/ let-bindings we need to check that the types of the binders
are alpha-equivalent. Otherwise

  letrec (x : Bool) = x in x

and

  letrec (y : Char) = y in y

would be considered alpha-equivalent, which they are obviously not.

For /non-recursive/ let-bindings, we do not have to check that the types of
the binders are alpha-equivalent. When the RHSs (the expressions) of the
non-recursive let-binders are well-formed and well-typed (which we assume they
are at this point in the compiler), and the RHSs are alpha-equivalent, then the
bindings must have the same type.

In addition, it is also worth pointing out that

  letrec { x = e1; y = e2 } in b

is NOT considered equal to

  letrec { y = e2; x = e1 } in b
-}

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
   filterTM = ftE

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

ftE :: (a->Bool) -> CoreMapX a -> CoreMapX a
ftE f (CM { cm_var = cvar, cm_lit = clit
          , cm_co = cco, cm_type = ctype
          , cm_cast = ccast , cm_app = capp
          , cm_lam = clam, cm_letn = cletn
          , cm_letr = cletr, cm_case = ccase
          , cm_ecase = cecase, cm_tick = ctick })
  = CM { cm_var = filterTM f cvar, cm_lit = filterTM f clit
       , cm_co = filterTM f cco, cm_type = filterTM f ctype
       , cm_cast = mapTM (filterTM f) ccast, cm_app = mapTM (filterTM f) capp
       , cm_lam = mapTM (filterTM f) clam, cm_letn = mapTM (mapTM (filterTM f)) cletn
       , cm_letr = mapTM (mapTM (filterTM f)) cletr, cm_case = mapTM (filterTM f) ccase
       , cm_ecase = mapTM (filterTM f) cecase, cm_tick = mapTM (filterTM f) ctick }

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
type TickishMap a = Map.Map CoreTickish a
lkTickish :: CoreTickish -> TickishMap a -> Maybe a
lkTickish = lookupTM

xtTickish :: CoreTickish -> XT a -> TickishMap a -> TickishMap a
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
   filterTM = ftA

instance Eq (DeBruijn CoreAlt) where
  D env1 a1 == D env2 a2 = go a1 a2 where
    go (Alt DEFAULT _ rhs1) (Alt DEFAULT _ rhs2)
        = D env1 rhs1 == D env2 rhs2
    go (Alt (LitAlt lit1) _ rhs1) (Alt (LitAlt lit2) _ rhs2)
        = lit1 == lit2 && D env1 rhs1 == D env2 rhs2
    go (Alt (DataAlt dc1) bs1 rhs1) (Alt (DataAlt dc2) bs2 rhs2)
        = dc1 == dc2 &&
          D (extendCMEs env1 bs1) rhs1 == D (extendCMEs env2 bs2) rhs2
    go _ _ = False

mapA :: (a->b) -> AltMap a -> AltMap b
mapA f (AM { am_deflt = adeflt, am_data = adata, am_lit = alit })
  = AM { am_deflt = mapTM f adeflt
       , am_data = mapTM (mapTM f) adata
       , am_lit = mapTM (mapTM f) alit }

ftA :: (a->Bool) -> AltMap a -> AltMap a
ftA f (AM { am_deflt = adeflt, am_data = adata, am_lit = alit })
  = AM { am_deflt = filterTM f adeflt
       , am_data = mapTM (filterTM f) adata
       , am_lit = mapTM (filterTM f) alit }

lkA :: CmEnv -> CoreAlt -> AltMap a -> Maybe a
lkA env (Alt DEFAULT      _  rhs) = am_deflt >.> lkG (D env rhs)
lkA env (Alt (LitAlt lit) _  rhs) = am_lit >.> lookupTM lit >=> lkG (D env rhs)
lkA env (Alt (DataAlt dc) bs rhs) = am_data >.> lkDNamed dc
                                        >=> lkG (D (extendCMEs env bs) rhs)

xtA :: CmEnv -> CoreAlt -> XT a -> AltMap a -> AltMap a
xtA env (Alt DEFAULT _ rhs)      f m =
    m { am_deflt = am_deflt m |> xtG (D env rhs) f }
xtA env (Alt (LitAlt l) _ rhs)   f m =
    m { am_lit   = am_lit m   |> alterTM l |>> xtG (D env rhs) f }
xtA env (Alt (DataAlt d) bs rhs) f m =
    m { am_data  = am_data m  |> xtDNamed d
                             |>> xtG (D (extendCMEs env bs) rhs) f }

fdA :: (a -> b -> b) -> AltMap a -> b -> b
fdA k m = foldTM k (am_deflt m)
        . foldTM (foldTM k) (am_data m)
        . foldTM (foldTM k) (am_lit m)

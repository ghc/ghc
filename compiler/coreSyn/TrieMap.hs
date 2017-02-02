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
module TrieMap(
   CoreMap, emptyCoreMap, extendCoreMap, lookupCoreMap, foldCoreMap,
   TypeMap, emptyTypeMap, extendTypeMap, lookupTypeMap, foldTypeMap,
   LooseTypeMap,
   MaybeMap,
   ListMap,
   TrieMap(..), insertTM, deleteTM,
   LiteralMap,
   lkDFreeVar, xtDFreeVar,
   lkDNamed, xtDNamed,
   (>.>), (|>), (|>>),
 ) where

import CoreSyn
import Coercion
import Literal
import Name
import Type
import TyCoRep
import Var
import UniqDFM
import Unique( Unique )
import FastString(FastString)

import qualified Data.Map    as Map
import qualified Data.IntMap as IntMap
import VarEnv
import NameEnv
import Outputable
import Control.Monad( (>=>) )

{-
This module implements TrieMaps, which are finite mappings
whose key is a structured value like a CoreExpr or Type.

The code is very regular and boilerplate-like, but there is
some neat handling of *binders*.  In effect they are deBruijn
numbered on the fly.

The regular pattern for handling TrieMaps on data structures was first
described (to my knowledge) in Connelly and Morris's 1995 paper "A
generalization of the Trie Data Structure"; there is also an accessible
description of the idea in Okasaki's book "Purely Functional Data
Structures", Section 10.3.2

************************************************************************
*                                                                      *
                   The TrieMap class
*                                                                      *
************************************************************************
-}

type XT a = Maybe a -> Maybe a  -- How to alter a non-existent elt (Nothing)
                                --               or an existing elt (Just)

class TrieMap m where
   type Key m :: *
   emptyTM  :: m a
   lookupTM :: forall b. Key m -> m b -> Maybe b
   alterTM  :: forall b. Key m -> XT b -> m b -> m b
   mapTM    :: (a->b) -> m a -> m b

   foldTM   :: (a -> b -> b) -> m a -> b -> b
      -- The unusual argument order here makes
      -- it easy to compose calls to foldTM;
      -- see for example fdE below

insertTM :: TrieMap m => Key m -> a -> m a -> m a
insertTM k v m = alterTM k (\_ -> Just v) m

deleteTM :: TrieMap m => Key m -> m a -> m a
deleteTM k m = alterTM k (\_ -> Nothing) m

----------------------
-- Recall that
--   Control.Monad.(>=>) :: (a -> Maybe b) -> (b -> Maybe c) -> a -> Maybe c

(>.>) :: (a -> b) -> (b -> c) -> a -> c
-- Reverse function composition (do f first, then g)
infixr 1 >.>
(f >.> g) x = g (f x)
infixr 1 |>, |>>

(|>) :: a -> (a->b) -> b     -- Reverse application
x |> f = f x

----------------------
(|>>) :: TrieMap m2
      => (XT (m2 a) -> m1 (m2 a) -> m1 (m2 a))
      -> (m2 a -> m2 a)
      -> m1 (m2 a) -> m1 (m2 a)
(|>>) f g = f (Just . g . deMaybe)

deMaybe :: TrieMap m => Maybe (m a) -> m a
deMaybe Nothing  = emptyTM
deMaybe (Just m) = m

{-
************************************************************************
*                                                                      *
                   IntMaps
*                                                                      *
************************************************************************
-}

instance TrieMap IntMap.IntMap where
  type Key IntMap.IntMap = Int
  emptyTM = IntMap.empty
  lookupTM k m = IntMap.lookup k m
  alterTM = xtInt
  foldTM k m z = IntMap.foldr k z m
  mapTM f m = IntMap.map f m

xtInt :: Int -> XT a -> IntMap.IntMap a -> IntMap.IntMap a
xtInt k f m = IntMap.alter f k m

instance Ord k => TrieMap (Map.Map k) where
  type Key (Map.Map k) = k
  emptyTM = Map.empty
  lookupTM = Map.lookup
  alterTM k f m = Map.alter f k m
  foldTM k m z = Map.foldr k z m
  mapTM f m = Map.map f m


{-
Note [foldTM determinism]
~~~~~~~~~~~~~~~~~~~~~~~~~
We want foldTM to be deterministic, which is why we have an instance of
TrieMap for UniqDFM, but not for UniqFM. Here's an example of some things that
go wrong if foldTM is nondeterministic. Consider:

  f a b = return (a <> b)

Depending on the order that the typechecker generates constraints you
get either:

  f :: (Monad m, Monoid a) => a -> a -> m a

or:

  f :: (Monoid a, Monad m) => a -> a -> m a

The generated code will be different after desugaring as the dictionaries
will be bound in different orders, leading to potential ABI incompatibility.

One way to solve this would be to notice that the typeclasses could be
sorted alphabetically.

Unfortunately that doesn't quite work with this example:

  f a b = let x = a <> a; y = b <> b in x

where you infer:

  f :: (Monoid m, Monoid m1) => m1 -> m -> m1

or:

  f :: (Monoid m1, Monoid m) => m1 -> m -> m1

Here you could decide to take the order of the type variables in the type
according to depth first traversal and use it to order the constraints.

The real trouble starts when the user enables incoherent instances and
the compiler has to make an arbitrary choice. Consider:

  class T a b where
    go :: a -> b -> String

  instance (Show b) => T Int b where
    go a b = show a ++ show b

  instance (Show a) => T a Bool where
    go a b = show a ++ show b

  f = go 10 True

GHC is free to choose either dictionary to implement f, but for the sake of
determinism we'd like it to be consistent when compiling the same sources
with the same flags.

inert_dicts :: DictMap is implemented with a TrieMap. In getUnsolvedInerts it
gets converted to a bag of (Wanted) Cts using a fold. Then in
solve_simple_wanteds it's merged with other WantedConstraints. We want the
conversion to a bag to be deterministic. For that purpose we use UniqDFM
instead of UniqFM to implement the TrieMap.

See Note [Deterministic UniqFM] in UniqDFM for more details on how it's made
deterministic.
-}

instance TrieMap UniqDFM where
  type Key UniqDFM = Unique
  emptyTM = emptyUDFM
  lookupTM k m = lookupUDFM m k
  alterTM k f m = alterUDFM f m k
  foldTM k m z = foldUDFM k z m
  mapTM f m = mapUDFM f m

{-
************************************************************************
*                                                                      *
                   Maybes
*                                                                      *
************************************************************************

If              m is a map from k -> val
then (MaybeMap m) is a map from (Maybe k) -> val
-}

data MaybeMap m a = MM { mm_nothing  :: Maybe a, mm_just :: m a }

instance TrieMap m => TrieMap (MaybeMap m) where
   type Key (MaybeMap m) = Maybe (Key m)
   emptyTM  = MM { mm_nothing = Nothing, mm_just = emptyTM }
   lookupTM = lkMaybe lookupTM
   alterTM  = xtMaybe alterTM
   foldTM   = fdMaybe
   mapTM    = mapMb

mapMb :: TrieMap m => (a->b) -> MaybeMap m a -> MaybeMap m b
mapMb f (MM { mm_nothing = mn, mm_just = mj })
  = MM { mm_nothing = fmap f mn, mm_just = mapTM f mj }

lkMaybe :: (forall b. k -> m b -> Maybe b)
        -> Maybe k -> MaybeMap m a -> Maybe a
lkMaybe _  Nothing  = mm_nothing
lkMaybe lk (Just x) = mm_just >.> lk x

xtMaybe :: (forall b. k -> XT b -> m b -> m b)
        -> Maybe k -> XT a -> MaybeMap m a -> MaybeMap m a
xtMaybe _  Nothing  f m = m { mm_nothing  = f (mm_nothing m) }
xtMaybe tr (Just x) f m = m { mm_just = mm_just m |> tr x f }

fdMaybe :: TrieMap m => (a -> b -> b) -> MaybeMap m a -> b -> b
fdMaybe k m = foldMaybe k (mm_nothing m)
            . foldTM k (mm_just m)

{-
************************************************************************
*                                                                      *
                   Lists
*                                                                      *
************************************************************************
-}

data ListMap m a
  = LM { lm_nil  :: Maybe a
       , lm_cons :: m (ListMap m a) }

instance TrieMap m => TrieMap (ListMap m) where
   type Key (ListMap m) = [Key m]
   emptyTM  = LM { lm_nil = Nothing, lm_cons = emptyTM }
   lookupTM = lkList lookupTM
   alterTM  = xtList alterTM
   foldTM   = fdList
   mapTM    = mapList

mapList :: TrieMap m => (a->b) -> ListMap m a -> ListMap m b
mapList f (LM { lm_nil = mnil, lm_cons = mcons })
  = LM { lm_nil = fmap f mnil, lm_cons = mapTM (mapTM f) mcons }

lkList :: TrieMap m => (forall b. k -> m b -> Maybe b)
        -> [k] -> ListMap m a -> Maybe a
lkList _  []     = lm_nil
lkList lk (x:xs) = lm_cons >.> lk x >=> lkList lk xs

xtList :: TrieMap m => (forall b. k -> XT b -> m b -> m b)
        -> [k] -> XT a -> ListMap m a -> ListMap m a
xtList _  []     f m = m { lm_nil  = f (lm_nil m) }
xtList tr (x:xs) f m = m { lm_cons = lm_cons m |> tr x |>> xtList tr xs f }

fdList :: forall m a b. TrieMap m
       => (a -> b -> b) -> ListMap m a -> b -> b
fdList k m = foldMaybe k          (lm_nil m)
           . foldTM    (fdList k) (lm_cons m)

foldMaybe :: (a -> b -> b) -> Maybe a -> b -> b
foldMaybe _ Nothing  b = b
foldMaybe k (Just a) b = k a b

{-
************************************************************************
*                                                                      *
                   Basic maps
*                                                                      *
************************************************************************
-}

lkDNamed :: NamedThing n => n -> DNameEnv a -> Maybe a
lkDNamed n env = lookupDNameEnv env (getName n)

xtDNamed :: NamedThing n => n -> XT a -> DNameEnv a -> DNameEnv a
xtDNamed tc f m = alterDNameEnv f m (getName tc)

------------------------
type LiteralMap  a = Map.Map Literal a

emptyLiteralMap :: LiteralMap a
emptyLiteralMap = emptyTM

lkLit :: Literal -> LiteralMap a -> Maybe a
lkLit = lookupTM

xtLit :: Literal -> XT a -> LiteralMap a -> LiteralMap a
xtLit = alterTM

{-
************************************************************************
*                                                                      *
                   GenMap
*                                                                      *
************************************************************************

Note [Compressed TrieMap]
~~~~~~~~~~~~~~~~~~~~~~~~~

The GenMap constructor augments TrieMaps with leaf compression.  This helps
solve the performance problem detailed in #9960: suppose we have a handful
H of entries in a TrieMap, each with a very large key, size K. If you fold over
such a TrieMap you'd expect time O(H). That would certainly be true of an
association list! But with TrieMap we actually have to navigate down a long
singleton structure to get to the elements, so it takes time O(K*H).  This
can really hurt on many type-level computation benchmarks:
see for example T9872d.

The point of a TrieMap is that you need to navigate to the point where only one
key remains, and then things should be fast.  So the point of a SingletonMap
is that, once we are down to a single (key,value) pair, we stop and
just use SingletonMap.

'EmptyMap' provides an even more basic (but essential) optimization: if there is
nothing in the map, don't bother building out the (possibly infinite) recursive
TrieMap structure!
-}

data GenMap m a
   = EmptyMap
   | SingletonMap (Key m) a
   | MultiMap (m a)

instance (Outputable a, Outputable (m a)) => Outputable (GenMap m a) where
  ppr EmptyMap = text "Empty map"
  ppr (SingletonMap _ v) = text "Singleton map" <+> ppr v
  ppr (MultiMap m) = ppr m

-- TODO undecidable instance
instance (Eq (Key m), TrieMap m) => TrieMap (GenMap m) where
   type Key (GenMap m) = Key m
   emptyTM  = EmptyMap
   lookupTM = lkG
   alterTM  = xtG
   foldTM   = fdG
   mapTM    = mapG

-- NB: Be careful about RULES and type families (#5821).  So we should make sure
-- to specify @Key TypeMapX@ (and not @DeBruijn Type@, the reduced form)

{-# SPECIALIZE lkG :: Key TypeMapX     -> TypeMapG a     -> Maybe a #-}
{-# SPECIALIZE lkG :: Key CoercionMapX -> CoercionMapG a -> Maybe a #-}
{-# SPECIALIZE lkG :: Key CoreMapX     -> CoreMapG a     -> Maybe a #-}
lkG :: (Eq (Key m), TrieMap m) => Key m -> GenMap m a -> Maybe a
lkG _ EmptyMap                         = Nothing
lkG k (SingletonMap k' v') | k == k'   = Just v'
                           | otherwise = Nothing
lkG k (MultiMap m)                     = lookupTM k m

{-# SPECIALIZE xtG :: Key TypeMapX     -> XT a -> TypeMapG a -> TypeMapG a #-}
{-# SPECIALIZE xtG :: Key CoercionMapX -> XT a -> CoercionMapG a -> CoercionMapG a #-}
{-# SPECIALIZE xtG :: Key CoreMapX     -> XT a -> CoreMapG a -> CoreMapG a #-}
xtG :: (Eq (Key m), TrieMap m) => Key m -> XT a -> GenMap m a -> GenMap m a
xtG k f EmptyMap
    = case f Nothing of
        Just v  -> SingletonMap k v
        Nothing -> EmptyMap
xtG k f m@(SingletonMap k' v')
    | k' == k
    -- The new key matches the (single) key already in the tree.  Hence,
    -- apply @f@ to @Just v'@ and build a singleton or empty map depending
    -- on the 'Just'/'Nothing' response respectively.
    = case f (Just v') of
        Just v'' -> SingletonMap k' v''
        Nothing  -> EmptyMap
    | otherwise
    -- We've hit a singleton tree for a different key than the one we are
    -- searching for. Hence apply @f@ to @Nothing@. If result is @Nothing@ then
    -- we can just return the old map. If not, we need a map with *two*
    -- entries. The easiest way to do that is to insert two items into an empty
    -- map of type @m a@.
    = case f Nothing of
        Nothing  -> m
        Just v   -> emptyTM |> alterTM k' (const (Just v'))
                           >.> alterTM k  (const (Just v))
                           >.> MultiMap
xtG k f (MultiMap m) = MultiMap (alterTM k f m)

{-# SPECIALIZE mapG :: (a -> b) -> TypeMapG a     -> TypeMapG b #-}
{-# SPECIALIZE mapG :: (a -> b) -> CoercionMapG a -> CoercionMapG b #-}
{-# SPECIALIZE mapG :: (a -> b) -> CoreMapG a     -> CoreMapG b #-}
mapG :: TrieMap m => (a -> b) -> GenMap m a -> GenMap m b
mapG _ EmptyMap = EmptyMap
mapG f (SingletonMap k v) = SingletonMap k (f v)
mapG f (MultiMap m) = MultiMap (mapTM f m)

{-# SPECIALIZE fdG :: (a -> b -> b) -> TypeMapG a     -> b -> b #-}
{-# SPECIALIZE fdG :: (a -> b -> b) -> CoercionMapG a -> b -> b #-}
{-# SPECIALIZE fdG :: (a -> b -> b) -> CoreMapG a     -> b -> b #-}
fdG :: TrieMap m => (a -> b -> b) -> GenMap m a -> b -> b
fdG _ EmptyMap = \z -> z
fdG k (SingletonMap _ v) = \z -> k v z
fdG k (MultiMap m) = foldTM k m

{-
************************************************************************
*                                                                      *
                   CoreMap
*                                                                      *
************************************************************************

Note [Binders]
~~~~~~~~~~~~~~
 * In general we check binders as late as possible because types are
   less likely to differ than expression structure.  That's why
      cm_lam :: CoreMapG (TypeMapG a)
   rather than
      cm_lam :: TypeMapG (CoreMapG a)

 * We don't need to look at the type of some binders, notalby
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
      = length ps1 == length ps2
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
emptyE = CM { cm_var = emptyTM, cm_lit = emptyLiteralMap
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
    go (Lit l)              = cm_lit  >.> lkLit l
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
xtE (D _   (Lit l))              f m = m { cm_lit  = cm_lit m  |> xtLit l f }
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
                 , am_lit  = emptyLiteralMap }
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
lkA env (LitAlt lit, _, rhs)  = am_lit >.> lkLit lit >=> lkG (D env rhs)
lkA env (DataAlt dc, bs, rhs) = am_data >.> lkDNamed dc
                                        >=> lkG (D (extendCMEs env bs) rhs)

xtA :: CmEnv -> CoreAlt -> XT a -> AltMap a -> AltMap a
xtA env (DEFAULT, _, rhs)    f m =
    m { am_deflt = am_deflt m |> xtG (D env rhs) f }
xtA env (LitAlt l, _, rhs)   f m =
    m { am_lit   = am_lit m   |> xtLit l |>> xtG (D env rhs) f }
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
-- the same kind.
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

-- | squeeze out any synonyms, convert Constraint to *, and change TyConApps
-- to nested AppTys. Why the last one? See Note [Equality on AppTys] in Type
trieMapView :: Type -> Maybe Type
trieMapView ty | Just ty' <- coreViewOneStarKind ty = Just ty'
trieMapView ty
  | Just (tc, tys@(_:_)) <- splitTyConApp_maybe ty
  = Just $ foldl AppTy (TyConApp tc []) tys
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
    | Just new_t  <- coreViewOneStarKind t  = D env new_t == env_t'
    | Just new_t' <- coreViewOneStarKind t' = env_t       == D env' new_t'
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
        (ForAllTy (TvBndr tv _) ty, ForAllTy (TvBndr tv' _) ty')
            -> D env (tyVarKind tv)    == D env' (tyVarKind tv') &&
               D (extendCME env tv) ty == D (extendCME env' tv') ty'
        (CoercionTy {}, CoercionTy {})
            -> True
        _ -> False

instance {-# OVERLAPPING #-}
         Outputable a => Outputable (TypeMapG a) where
  ppr m = text "TypeMap elts" <+> ppr (foldTM (:) m [])

emptyT :: TypeMapX a
emptyT = TM { tm_var  = emptyTM
            , tm_app  = EmptyMap
            , tm_tycon  = emptyDNameEnv
            , tm_forall = EmptyMap
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
    go (ForAllTy (TvBndr tv _) ty) = tm_forall >.> lkG (D (extendCME env tv) ty)
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
xtT (D env (ForAllTy (TvBndr tv _) ty))  f m
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

data CmEnv = CME { cme_next :: BoundVar
                 , cme_env  :: VarEnv BoundVar }

emptyCME :: CmEnv
emptyCME = CME { cme_next = 0, cme_env = emptyVarEnv }

extendCME :: CmEnv -> Var -> CmEnv
extendCME (CME { cme_next = bv, cme_env = env }) v
  = CME { cme_next = bv+1, cme_env = extendVarEnv env v bv }

extendCMEs :: CmEnv -> [Var] -> CmEnv
extendCMEs env vs = foldl extendCME env vs

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

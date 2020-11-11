{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}
module GHC.Data.TrieMap(
   -- * Maps over 'Maybe' values
   MaybeMap,
   -- * Maps over 'List' values
   ListMap,
   -- * Maps over 'Literal's
   LiteralMap,
   -- * 'TrieMap' class
   TrieMap(..), insertTM, deleteTM, foldMapTM, isEmptyTM,

   -- * Things helpful for adding additional Instances.
   (>.>), (|>), (|>>), XT,
   foldMaybe, filterMaybe,
   -- * Map for leaf compression
   GenMap,
   lkG, xtG, mapG, fdG,
   xtList, lkList

 ) where

import GHC.Prelude

import GHC.Types.Literal
import GHC.Types.Unique.DFM
import GHC.Types.Unique( Uniquable )

import qualified Data.Map    as Map
import qualified Data.IntMap as IntMap
import GHC.Utils.Outputable
import Control.Monad( (>=>) )
import Data.Kind( Type )

import qualified Data.Semigroup as S

{-
This module implements TrieMaps, which are finite mappings
whose key is a structured value like a CoreExpr or Type.

This file implements tries over general data structures.
Implementation for tries over Core Expressions/Types are
available in GHC.Core.Map.Expr.

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
   type Key m :: Type
   emptyTM  :: m a
   lookupTM :: forall b. Key m -> m b -> Maybe b
   alterTM  :: forall b. Key m -> XT b -> m b -> m b
   mapTM    :: (a->b) -> m a -> m b
   filterTM :: (a -> Bool) -> m a -> m a

   foldTM   :: (a -> b -> b) -> m a -> b -> b
      -- The unusual argument order here makes
      -- it easy to compose calls to foldTM;
      -- see for example fdE below

insertTM :: TrieMap m => Key m -> a -> m a -> m a
insertTM k v m = alterTM k (\_ -> Just v) m

deleteTM :: TrieMap m => Key m -> m a -> m a
deleteTM k m = alterTM k (\_ -> Nothing) m

foldMapTM :: (TrieMap m, Monoid r) => (a -> r) -> m a -> r
foldMapTM f m = foldTM (\ x r -> f x S.<> r) m mempty

-- This looks inefficient.
isEmptyTM :: TrieMap m => m a -> Bool
isEmptyTM m = foldTM (\ _ _ -> False) m True

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
  filterTM f m = IntMap.filter f m

xtInt :: Int -> XT a -> IntMap.IntMap a -> IntMap.IntMap a
xtInt k f m = IntMap.alter f k m

instance Ord k => TrieMap (Map.Map k) where
  type Key (Map.Map k) = k
  emptyTM = Map.empty
  lookupTM = Map.lookup
  alterTM k f m = Map.alter f k m
  foldTM k m z = Map.foldr k z m
  mapTM f m = Map.map f m
  filterTM f m = Map.filter f m


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

See Note [Deterministic UniqFM] in GHC.Types.Unique.DFM for more details on how it's made
deterministic.
-}

instance forall key. Uniquable key => TrieMap (UniqDFM key) where
  type Key (UniqDFM key) = key
  emptyTM = emptyUDFM
  lookupTM k m = lookupUDFM m k
  alterTM k f m = alterUDFM f m k
  foldTM k m z = foldUDFM k z m
  mapTM f m = mapUDFM f m
  filterTM f m = filterUDFM f m

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
   filterTM = ftMaybe

instance TrieMap m => Foldable (MaybeMap m) where
  foldMap = foldMapTM

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

ftMaybe :: TrieMap m => (a -> Bool) -> MaybeMap m a -> MaybeMap m a
ftMaybe f (MM { mm_nothing = mn, mm_just = mj })
  = MM { mm_nothing = filterMaybe f mn, mm_just = filterTM f mj }

foldMaybe :: (a -> b -> b) -> Maybe a -> b -> b
foldMaybe _ Nothing  b = b
foldMaybe k (Just a) b = k a b

filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe _ Nothing = Nothing
filterMaybe f input@(Just x) | f x       = input
                             | otherwise = Nothing

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
   filterTM = ftList

instance TrieMap m => Foldable (ListMap m) where
  foldMap = foldMapTM

instance (TrieMap m, Outputable a) => Outputable (ListMap m a) where
  ppr m = text "List elts" <+> ppr (foldTM (:) m [])

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

ftList :: TrieMap m => (a -> Bool) -> ListMap m a -> ListMap m a
ftList f (LM { lm_nil = mnil, lm_cons = mcons })
  = LM { lm_nil = filterMaybe f mnil, lm_cons = mapTM (filterTM f) mcons }

{-
************************************************************************
*                                                                      *
                   Basic maps
*                                                                      *
************************************************************************
-}

type LiteralMap  a = Map.Map Literal a

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

Compressed triemaps are heavily used by GHC.Core.Map.Expr. So we have to mark some things
as INLINEABLE to permit specialization.
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
   filterTM = ftG

instance (Eq (Key m), TrieMap m) => Foldable (GenMap m) where
  foldMap = foldMapTM

--We want to be able to specialize these functions when defining eg
--tries over (GenMap CoreExpr) which requires INLINEABLE

{-# INLINEABLE lkG #-}
lkG :: (Eq (Key m), TrieMap m) => Key m -> GenMap m a -> Maybe a
lkG _ EmptyMap                         = Nothing
lkG k (SingletonMap k' v') | k == k'   = Just v'
                           | otherwise = Nothing
lkG k (MultiMap m)                     = lookupTM k m

{-# INLINEABLE xtG #-}
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

{-# INLINEABLE mapG #-}
mapG :: TrieMap m => (a -> b) -> GenMap m a -> GenMap m b
mapG _ EmptyMap = EmptyMap
mapG f (SingletonMap k v) = SingletonMap k (f v)
mapG f (MultiMap m) = MultiMap (mapTM f m)

{-# INLINEABLE fdG #-}
fdG :: TrieMap m => (a -> b -> b) -> GenMap m a -> b -> b
fdG _ EmptyMap = \z -> z
fdG k (SingletonMap _ v) = \z -> k v z
fdG k (MultiMap m) = foldTM k m

{-# INLINEABLE ftG #-}
ftG :: TrieMap m => (a -> Bool) -> GenMap m a -> GenMap m a
ftG _ EmptyMap = EmptyMap
ftG f input@(SingletonMap _ v)
  | f v       = input
  | otherwise = EmptyMap
ftG f (MultiMap m) = MultiMap (filterTM f m)
  -- we don't have enough information to reconstruct the key to make
  -- a SingletonMap

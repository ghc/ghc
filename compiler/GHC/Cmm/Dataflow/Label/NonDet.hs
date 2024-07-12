{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Non-deterministic 'LabelMap' and 'LabelSet':
--
-- Labels that use non-deterministic Uniques will make this map
-- non-deterministic. In contrast, 'GHC.Cmm.Dataflow.Label.LabelMap' will be
-- deterministically ordered regardless of the non-deterministic origin of the
-- Uniques in the labels.
--
-- We want to make sure that non-deterministic label maps are only used when
-- the non-determinism of the map cannot affect the determinism of the code
-- generation output. To that effect, this label map only exposes operations
-- which don't allow non-determinism from the outside, e.g., it exposes maps
-- and lookups, but not traversals or to-list functions.
module GHC.Cmm.Dataflow.Label.NonDet
    ( Label
    , LabelMap
    , LabelSet
    , FactBase
    , lookupFact
    , mkHooplLabel
    -- * Set
    , setEmpty
    , setNull
    , setSize
    , setMember
    , setSingleton
    , setInsert
    , setDelete
    , setUnion
    , setUnions
    , setDifference
    , setIntersection
    , setIsSubsetOf
    , setFilter
    , setFromList
    , nonDetSetFoldl
    , nonDetSetFoldr
    , nonDetSetElems
    -- * Map
    , mapNull
    , mapSize
    , mapMember
    , mapLookup
    , mapFindWithDefault
    , mapEmpty
    , mapSingleton
    , mapInsert
    , mapInsertWith
    , mapDelete
    , mapAlter
    , mapAdjust
    , mapUnion
    , mapUnions
    , mapUnionWithKey
    , mapDifference
    , mapIntersection
    , mapIsSubmapOf
    , mapMap
    , mapMapWithKey
    , mapFilter
    , mapFilterWithKey
    , mapFromList
    , mapFromListWith
    , nonDetMapElems
    , nonDetMapFoldl
    , nonDetMapFoldr
    , nonDetMapFoldlWithKey
    , nonDetMapFoldMapWithKey
    , nonDetMapKeys
    , nonDetMapToList
    , nonDetMapM
    , allLM
    , anyLM
    , mapSum
    ) where

import GHC.Prelude

import GHC.Utils.Outputable

-- The code generator will eventually be using all the labels stored in a
-- LabelSet and LabelMap. For these reasons we use the strict variants of these
-- data structures. We inline selectively to enable the RULES in Word64Map/Set
-- to fire.
import GHC.Data.Word64Set (Word64Set)
import qualified GHC.Data.Word64Set as S
import GHC.Data.Word64Map.Strict (Word64Map)
import qualified GHC.Data.Word64Map.Strict as M

import Data.List (foldl1')

import GHC.Cmm.Dataflow.Label (Label(..), mkHooplLabel)
import GHC.Data.TrieMap (TrieMap(..))

-----------------------------------------------------------------------------
-- LabelSet

newtype LabelSet = LS Word64Set
  deriving newtype (Eq, Ord, Show, Monoid, Semigroup)

setNull :: LabelSet -> Bool
setNull (LS s) = S.null s

setSize :: LabelSet -> Int
setSize (LS s) = S.size s

setMember :: Label -> LabelSet -> Bool
setMember (Label k) (LS s) = S.member k s

setEmpty :: LabelSet
setEmpty = LS S.empty

setSingleton :: Label -> LabelSet
setSingleton (Label k) = LS (S.singleton k)

setInsert :: Label -> LabelSet -> LabelSet
setInsert (Label k) (LS s) = LS (S.insert k s)

setDelete :: Label -> LabelSet -> LabelSet
setDelete (Label k) (LS s) = LS (S.delete k s)

setUnion :: LabelSet -> LabelSet -> LabelSet
setUnion (LS x) (LS y) = LS (S.union x y)

{-# INLINE setUnions #-}
setUnions :: [LabelSet] -> LabelSet
setUnions [] = setEmpty
setUnions sets = foldl1' setUnion sets

setDifference :: LabelSet -> LabelSet -> LabelSet
setDifference (LS x) (LS y) = LS (S.difference x y)

setIntersection :: LabelSet -> LabelSet -> LabelSet
setIntersection (LS x) (LS y) = LS (S.intersection x y)

setIsSubsetOf :: LabelSet -> LabelSet -> Bool
setIsSubsetOf (LS x) (LS y) = S.isSubsetOf x y

setFilter :: (Label -> Bool) -> LabelSet -> LabelSet
setFilter f (LS s) = LS (S.filter (f . mkHooplLabel) s)

{-# INLINE setFromList #-}
setFromList :: [Label] -> LabelSet
setFromList ks  = LS (S.fromList (map lblToUnique ks))

{-# INLINE nonDetSetFoldl #-}
nonDetSetFoldl :: (t -> Label -> t) -> t -> LabelSet -> t
nonDetSetFoldl k z (LS s) = S.foldl (\a v -> k a (mkHooplLabel v)) z s

{-# INLINE nonDetSetFoldr #-}
nonDetSetFoldr :: (Label -> t -> t) -> t -> LabelSet -> t
nonDetSetFoldr k z (LS s) = S.foldr (\v a -> k (mkHooplLabel v) a) z s

{-# INLINE nonDetSetElems #-}
nonDetSetElems :: LabelSet -> [Label]
nonDetSetElems (LS s) = map mkHooplLabel (S.elems s)

-----------------------------------------------------------------------------
-- LabelMap

newtype LabelMap v = LM (Word64Map v)
  deriving newtype (Eq, Ord, Show, Functor)

mapNull :: LabelMap a -> Bool
mapNull (LM m) = M.null m

{-# INLINE mapSize #-}
mapSize :: LabelMap a -> Int
mapSize (LM m) = M.size m

mapMember :: Label -> LabelMap a -> Bool
mapMember (Label k) (LM m) = M.member k m

mapLookup :: Label -> LabelMap a -> Maybe a
mapLookup (Label k) (LM m) = M.lookup k m

mapFindWithDefault :: a -> Label -> LabelMap a -> a
mapFindWithDefault def (Label k) (LM m) = M.findWithDefault def k m

mapEmpty :: LabelMap v
mapEmpty = LM M.empty

mapSingleton :: Label -> v -> LabelMap v
mapSingleton (Label k) v = LM (M.singleton k v)

mapInsert :: Label -> v -> LabelMap v -> LabelMap v
mapInsert (Label k) v (LM m) = LM (M.insert k v m)

mapInsertWith :: (v -> v -> v) -> Label -> v -> LabelMap v -> LabelMap v
mapInsertWith f (Label k) v (LM m) = LM (M.insertWith f k v m)

mapDelete :: Label -> LabelMap v -> LabelMap v
mapDelete (Label k) (LM m) = LM (M.delete k m)

mapAlter :: (Maybe v -> Maybe v) -> Label -> LabelMap v -> LabelMap v
mapAlter f (Label k) (LM m) = LM (M.alter f k m)

mapAdjust :: (v -> v) -> Label -> LabelMap v -> LabelMap v
mapAdjust f (Label k) (LM m) = LM (M.adjust f k m)

mapUnion :: LabelMap v -> LabelMap v -> LabelMap v
mapUnion (LM x) (LM y) = LM (M.union x y)

{-# INLINE mapUnions #-}
mapUnions :: [LabelMap a] -> LabelMap a
mapUnions [] = mapEmpty
mapUnions maps = foldl1' mapUnion maps

mapUnionWithKey :: (Label -> v -> v -> v) -> LabelMap v -> LabelMap v -> LabelMap v
mapUnionWithKey f (LM x) (LM y) = LM (M.unionWithKey (f . mkHooplLabel) x y)

mapDifference :: LabelMap v -> LabelMap b -> LabelMap v
mapDifference (LM x) (LM y) = LM (M.difference x y)

mapIntersection :: LabelMap v -> LabelMap b -> LabelMap v
mapIntersection (LM x) (LM y) = LM (M.intersection x y)

mapIsSubmapOf :: Eq a => LabelMap a -> LabelMap a -> Bool
mapIsSubmapOf (LM x) (LM y) = M.isSubmapOf x y

mapMap :: (a -> v) -> LabelMap a -> LabelMap v
mapMap f (LM m) = LM (M.map f m)

mapMapWithKey :: (Label -> a -> v) -> LabelMap a -> LabelMap v
mapMapWithKey f (LM m) = LM (M.mapWithKey (f . mkHooplLabel) m)

{-# INLINEABLE mapFilter #-}
mapFilter :: (v -> Bool) -> LabelMap v -> LabelMap v
mapFilter f (LM m) = LM (M.filter f m)

{-# INLINEABLE mapFilterWithKey #-}
mapFilterWithKey :: (Label -> v -> Bool) -> LabelMap v -> LabelMap v
mapFilterWithKey f (LM m)  = LM (M.filterWithKey (f . mkHooplLabel) m)

{-# INLINE mapFromList #-}
mapFromList :: [(Label, v)] -> LabelMap v
mapFromList assocs = LM (M.fromList [(lblToUnique k, v) | (k, v) <- assocs])

mapFromListWith :: (v -> v -> v) -> [(Label, v)] -> LabelMap v
mapFromListWith f assocs = LM (M.fromListWith f [(lblToUnique k, v) | (k, v) <- assocs])

{-# INLINE nonDetMapElems #-}
nonDetMapElems :: LabelMap a -> [a]
nonDetMapElems (LM m) = M.elems m

{-# INLINE nonDetMapFoldl #-}
nonDetMapFoldl :: (a -> b -> a) -> a -> LabelMap b -> a
nonDetMapFoldl k z (LM m) = M.foldl k z m

{-# INLINE nonDetMapFoldr #-}
nonDetMapFoldr :: (a -> b -> b) -> b -> LabelMap a -> b
nonDetMapFoldr k z (LM m) = M.foldr k z m

{-# INLINE nonDetMapFoldlWithKey #-}
nonDetMapFoldlWithKey :: (t -> Label -> b -> t) -> t -> LabelMap b -> t
nonDetMapFoldlWithKey k z (LM m) = M.foldlWithKey (\a v -> k a (mkHooplLabel v)) z m

nonDetMapFoldMapWithKey :: Monoid m => (Label -> t -> m) -> LabelMap t -> m
nonDetMapFoldMapWithKey f (LM m) = M.foldMapWithKey (\k v -> f (mkHooplLabel k) v) m

{-# INLINE nonDetMapKeys #-}
nonDetMapKeys :: LabelMap a -> [Label]
nonDetMapKeys (LM m) = map (mkHooplLabel . fst) (M.toList m)

{-# INLINE nonDetMapToList #-}
nonDetMapToList :: LabelMap b -> [(Label, b)]
nonDetMapToList (LM m) = [(mkHooplLabel k, v) | (k, v) <- M.toList m]

nonDetMapM :: Monad m => (a -> m b) -> LabelMap a -> m (LabelMap b)
nonDetMapM f (LM m) = LM <$> M.traverseWithKey (\_ x -> f x) m

allLM :: (a -> Bool) -> LabelMap a -> Bool
allLM f (LM m) = all f m

anyLM :: (a -> Bool) -> LabelMap a -> Bool
anyLM f (LM m) = any f m

mapSum :: Num a => LabelMap a -> a
mapSum (LM m) = sum m

-----------------------------------------------------------------------------
-- Instances

instance Outputable LabelSet where
  ppr = ppr . nonDetSetElems

instance Outputable a => Outputable (LabelMap a) where
  ppr = ppr . nonDetMapToList

instance OutputableP env a => OutputableP env (LabelMap a) where
  pdoc env = pdoc env . nonDetMapToList

instance TrieMap LabelMap where
  type Key LabelMap = Label
  emptyTM       = mapEmpty
  lookupTM k m  = mapLookup k m
  alterTM k f m = mapAlter f k m
  foldTM = error "TrieMap: NonDet.LabelMap"
  filterTM f m  = mapFilter f m

-----------------------------------------------------------------------------
-- FactBase

type FactBase f = LabelMap f

lookupFact :: Label -> FactBase f -> Maybe f
lookupFact = mapLookup

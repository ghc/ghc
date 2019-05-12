-- | Defines common interfaces for maps and sets similar to the ones provided by containers.

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Collections
    ( IsSet(..), IsDetSet(..), IsNonDetSet(..)
    , setInsertList, setDeleteList, setUnions
    , IsMap(..)
    , mapInsertList, mapDeleteList, mapUnions
    ) where

import GhcPrelude

import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as S

import Data.List (foldl1')

class IsSet set where
  type ElemOf set

  setNull :: set -> Bool
  setSize :: set -> Int
  setMember :: ElemOf set -> set -> Bool

  setEmpty :: set
  setSingleton :: ElemOf set -> set
  setInsert :: ElemOf set -> set -> set
  setDelete :: ElemOf set -> set -> set

  setUnion :: set -> set -> set
  setDifference :: set -> set -> set
  setIntersection :: set -> set -> set
  setIsSubsetOf :: set -> set -> Bool
  setFilter :: (ElemOf set -> Bool) -> set -> set

  setFromList :: [ElemOf set] -> set

-- | Deterministic oder of elements for folds/elems.
--   See Note [Deterministic UniqFM]
class (IsSet set) => IsDetSet set where
  setFoldl :: (b -> ElemOf set -> b) -> b -> set -> b
  setFoldr :: (ElemOf set -> b -> b) -> b -> set -> b

  setElems :: set -> [ElemOf set]

class (IsSet set) => IsNonDetSet set where
  setNonDetFoldl :: (b -> ElemOf set -> b) -> b -> set -> b
  setNonDetFoldr :: (ElemOf set -> b -> b) -> b -> set -> b

  setNonDetElems :: set -> [ElemOf set]


-- Helper functions for IsSet class
setInsertList :: IsSet set => [ElemOf set] -> set -> set
setInsertList keys set = foldl' (flip setInsert) set keys

setDeleteList :: IsSet set => [ElemOf set] -> set -> set
setDeleteList keys set = foldl' (flip setDelete) set keys

setUnions :: IsSet set => [set] -> set
setUnions [] = setEmpty
setUnions sets = foldl1' setUnion sets


class IsMap map where
  type KeyOf map

  mapNull :: map a -> Bool
  mapSize :: map a -> Int
  mapMember :: KeyOf map -> map a -> Bool
  mapLookup :: KeyOf map -> map a -> Maybe a
  mapFindWithDefault :: a -> KeyOf map -> map a -> a

  mapEmpty :: map a
  mapSingleton :: KeyOf map -> a -> map a
  mapInsert :: KeyOf map -> a -> map a -> map a
  mapInsertWith :: (a -> a -> a) -> KeyOf map -> a -> map a -> map a
  mapDelete :: KeyOf map -> map a -> map a
  mapAlter :: (Maybe a -> Maybe a) -> KeyOf map -> map a -> map a
  mapAdjust :: (a -> a) -> KeyOf map -> map a -> map a

  mapUnion :: map a -> map a -> map a
  mapUnionWithKey :: (KeyOf map -> a -> a -> a) -> map a -> map a -> map a
  mapDifference :: map a -> map a -> map a
  mapIntersection :: map a -> map a -> map a
  mapIsSubmapOf :: Eq a => map a -> map a -> Bool

  mapMap :: (a -> b) -> map a -> map b
  mapMapWithKey :: (KeyOf map -> a -> b) -> map a -> map b
  mapFoldl :: (b -> a -> b) -> b -> map a -> b
  mapFoldr :: (a -> b -> b) -> b -> map a -> b
  mapFoldlWithKey :: (b -> KeyOf map -> a -> b) -> b -> map a -> b
  mapFoldMapWithKey :: Monoid m => (KeyOf map -> a -> m) -> map a -> m
  mapFilter :: (a -> Bool) -> map a -> map a
  mapFilterWithKey :: (KeyOf map -> a -> Bool) -> map a -> map a


  mapElems :: map a -> [a]
  mapKeys :: map a -> [KeyOf map]
  mapToList :: map a -> [(KeyOf map, a)]
  mapFromList :: [(KeyOf map, a)] -> map a
  mapFromListWith :: (a -> a -> a) -> [(KeyOf map,a)] -> map a

-- Helper functions for IsMap class
mapInsertList :: IsMap map => [(KeyOf map, a)] -> map a -> map a
mapInsertList assocs map = foldl' (flip (uncurry mapInsert)) map assocs

mapDeleteList :: IsMap map => [KeyOf map] -> map a -> map a
mapDeleteList keys map = foldl' (flip mapDelete) map keys

mapUnions :: IsMap map => [map a] -> map a
mapUnions [] = mapEmpty
mapUnions maps = foldl1' mapUnion maps

-----------------------------------------------------------------------------
-- Basic instances
-----------------------------------------------------------------------------

instance IsSet S.IntSet where
  type ElemOf S.IntSet = Int

  setNull s = S.null s
  setSize s = S.size s
  setMember k s = S.member k s

  setEmpty = S.empty
  setSingleton k = S.singleton k
  setFromList ks = S.fromList ks

  setInsert k s = S.insert k s
  setDelete k s = S.delete k s

  setUnion x y = S.union x y
  setDifference x y = S.difference x y
  setIntersection x y = S.intersection x y
  setIsSubsetOf x y = S.isSubsetOf x y
  setFilter f s = S.filter f s

instance IsDetSet S.IntSet where
  -- | Strict left fold
  setFoldl k z s = S.foldl' k z s
  setFoldr k z s = S.foldr k z s

  setElems s = S.elems s

instance IsNonDetSet S.IntSet where
  setNonDetFoldl k z s = S.foldl' k z s
  setNonDetFoldr k z s = S.foldr k z s

  setNonDetElems s = S.elems s

instance IsMap M.IntMap where
  type KeyOf M.IntMap = Int

  mapNull m = M.null m
  mapSize m = M.size m
  mapMember k m = M.member k m
  mapLookup k m = M.lookup k m
  mapFindWithDefault def k m = M.findWithDefault def k m

  mapEmpty = M.empty
  mapSingleton k v = M.singleton k v
  mapInsert k v m = M.insert k v m
  mapInsertWith f k v m = M.insertWith f k v m
  mapDelete k m = M.delete k m
  mapAlter f k m = M.alter f k m
  mapAdjust f k m = M.adjust f k m

  mapUnion x y = M.union x y
  mapUnionWithKey f x y = M.unionWithKey f x y
  mapDifference x y = M.difference x y
  mapIntersection x y = M.intersection x y
  mapIsSubmapOf x y = M.isSubmapOf x y

  mapMap f m = M.map f m
  mapMapWithKey f m = M.mapWithKey f m
  -- | Strict left fold.
  mapFoldl k z m = M.foldl' k z m
  mapFoldr k z m = M.foldr k z m
  mapFoldlWithKey k z m = M.foldlWithKey' k z m
  mapFoldMapWithKey f m = M.foldMapWithKey f m
  mapFilter f m = M.filter f m
  mapFilterWithKey f m = M.filterWithKey f m

  mapElems m = M.elems m
  mapKeys m = M.keys m
  mapToList m = M.toList m
  mapFromList assocs = M.fromList assocs
  mapFromListWith f assocs = M.fromListWith f assocs

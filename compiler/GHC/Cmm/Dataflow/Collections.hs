{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module GHC.Cmm.Dataflow.Collections
    ( IsSet(..)
    , setInsertList, setDeleteList, setUnions
    , IsMap(..)
    , mapInsertList, mapDeleteList, mapUnions
    , UniqueMap, UniqueSet
    ) where

import GHC.Prelude

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

  setFoldl :: (b -> ElemOf set -> b) -> b -> set -> b
  setFoldr :: (ElemOf set -> b -> b) -> b -> set -> b

  setElems :: set -> [ElemOf set]
  setFromList :: [ElemOf set] -> set

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

newtype UniqueSet = US S.IntSet deriving (Eq, Ord, Show, Semigroup, Monoid)

instance IsSet UniqueSet where
  type ElemOf UniqueSet = Int

  setNull (US s) = S.null s
  setSize (US s) = S.size s
  setMember k (US s) = S.member k s

  setEmpty = US S.empty
  setSingleton k = US (S.singleton k)
  setInsert k (US s) = US (S.insert k s)
  setDelete k (US s) = US (S.delete k s)

  setUnion (US x) (US y) = US (S.union x y)
  setDifference (US x) (US y) = US (S.difference x y)
  setIntersection (US x) (US y) = US (S.intersection x y)
  setIsSubsetOf (US x) (US y) = S.isSubsetOf x y
  setFilter f (US s) = US (S.filter f s)

  setFoldl k z (US s) = S.foldl' k z s
  setFoldr k z (US s) = S.foldr k z s

  setElems (US s) = S.elems s
  setFromList ks = US (S.fromList ks)

newtype UniqueMap v = UM (M.IntMap v)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance IsMap UniqueMap where
  type KeyOf UniqueMap = Int

  mapNull (UM m) = M.null m
  mapSize (UM m) = M.size m
  mapMember k (UM m) = M.member k m
  mapLookup k (UM m) = M.lookup k m
  mapFindWithDefault def k (UM m) = M.findWithDefault def k m

  mapEmpty = UM M.empty
  mapSingleton k v = UM (M.singleton k v)
  mapInsert k v (UM m) = UM (M.insert k v m)
  mapInsertWith f k v (UM m) = UM (M.insertWith f k v m)
  mapDelete k (UM m) = UM (M.delete k m)
  mapAlter f k (UM m) = UM (M.alter f k m)
  mapAdjust f k (UM m) = UM (M.adjust f k m)

  mapUnion (UM x) (UM y) = UM (M.union x y)
  mapUnionWithKey f (UM x) (UM y) = UM (M.unionWithKey f x y)
  mapDifference (UM x) (UM y) = UM (M.difference x y)
  mapIntersection (UM x) (UM y) = UM (M.intersection x y)
  mapIsSubmapOf (UM x) (UM y) = M.isSubmapOf x y

  mapMap f (UM m) = UM (M.map f m)
  mapMapWithKey f (UM m) = UM (M.mapWithKey f m)
  mapFoldl k z (UM m) = M.foldl' k z m
  mapFoldr k z (UM m) = M.foldr k z m
  mapFoldlWithKey k z (UM m) = M.foldlWithKey' k z m
  mapFoldMapWithKey f (UM m) = M.foldMapWithKey f m
  {-# INLINEABLE mapFilter #-}
  mapFilter f (UM m) = UM (M.filter f m)
  {-# INLINEABLE mapFilterWithKey #-}
  mapFilterWithKey f (UM m) = UM (M.filterWithKey f m)

  mapElems (UM m) = M.elems m
  mapKeys (UM m) = M.keys m
  {-# INLINEABLE mapToList #-}
  mapToList (UM m) = M.toList m
  mapFromList assocs = UM (M.fromList assocs)
  mapFromListWith f assocs = UM (M.fromListWith f assocs)

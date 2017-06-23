{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TypeFamilies #-}
module Hoopl.Unique
    ( Unique
    , UniqueMap
    , UniqueSet
    , intToUnique
    ) where

import qualified Data.IntMap as M
import qualified Data.IntSet as S

import Hoopl.Collections


-----------------------------------------------------------------------------
--              Unique
-----------------------------------------------------------------------------

type Unique = Int

intToUnique :: Int -> Unique
intToUnique = id

-----------------------------------------------------------------------------
-- UniqueSet

newtype UniqueSet = US S.IntSet deriving (Eq, Ord, Show)

instance IsSet UniqueSet where
  type ElemOf UniqueSet = Unique

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

  setFold k z (US s) = S.foldr k z s

  setElems (US s) = S.elems s
  setFromList ks = US (S.fromList ks)

-----------------------------------------------------------------------------
-- UniqueMap

newtype UniqueMap v = UM (M.IntMap v)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance IsMap UniqueMap where
  type KeyOf UniqueMap = Unique

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

  mapUnion (UM x) (UM y) = UM (M.union x y)
  mapUnionWithKey f (UM x) (UM y) = UM (M.unionWithKey (f . intToUnique) x y)
  mapDifference (UM x) (UM y) = UM (M.difference x y)
  mapIntersection (UM x) (UM y) = UM (M.intersection x y)
  mapIsSubmapOf (UM x) (UM y) = M.isSubmapOf x y

  mapMap f (UM m) = UM (M.map f m)
  mapMapWithKey f (UM m) = UM (M.mapWithKey (f . intToUnique) m)
  mapFold k z (UM m) = M.foldr k z m
  mapFoldWithKey k z (UM m) = M.foldrWithKey (k . intToUnique) z m
  mapFilter f (UM m) = UM (M.filter f m)

  mapElems (UM m) = M.elems m
  mapKeys (UM m) = M.keys m
  mapToList (UM m) = M.toList m
  mapFromList assocs = UM (M.fromList assocs)
  mapFromListWith f assocs = UM (M.fromListWith f assocs)

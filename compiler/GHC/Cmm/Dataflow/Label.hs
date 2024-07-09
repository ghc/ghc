{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module GHC.Cmm.Dataflow.Label
    ( Label(..)
    , LabelMap
    , LabelSet
    , mkHooplLabel
    , FactBase
    , lookupFact
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
    , setFoldl
    , setFoldr
    , setFromList
    , setElems
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
    , mapUnionWithKey
    , mapUnions
    , mapDifference
    , mapIntersection
    , mapMap
    , mapMapWithKey
    , mapFoldl
    , mapFoldr
    , mapFoldlWithKey
    , mapFilter
    , mapFilterWithKey
    , mapElems
    , mapKeys
    , mapToList
    , mapFromList
    , mapFromListWith
    ) where

import GHC.Prelude

import GHC.Utils.Outputable

import GHC.Types.Unique (Uniquable(..), mkUniqueGrimily, getKey)

-- The code generator will eventually be using all the labels stored in a
-- LabelSet and LabelMap. For these reasons we use the strict variants of these
-- data structures. We inline selectively to enable the RULES in Word64Map/Set
-- to fire.
import GHC.Data.Word64Set (Word64Set)
import qualified GHC.Data.Word64Set as S
import GHC.Data.TrieMap
import GHC.Types.Unique.DFM

import Data.Word (Word64)
import Data.List (foldl1')
import GHC.Data.Maybe (fromMaybe)


-----------------------------------------------------------------------------
--              Label
-----------------------------------------------------------------------------

newtype Label = Label { lblToUnique :: Word64 }
  deriving newtype (Eq, Ord)

mkHooplLabel :: Word64 -> Label
mkHooplLabel = Label

instance Show Label where
  show (Label n) = "L" ++ show n

instance Uniquable Label where
  getUnique label = mkUniqueGrimily (lblToUnique label)

instance Outputable Label where
  ppr label = ppr (getUnique label)

instance OutputableP env Label where
  pdoc _ l = ppr l

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

{-# INLINE setFoldl #-}
setFoldl :: (t -> Label -> t) -> t -> LabelSet -> t
setFoldl k z (LS s) = S.foldl (\a v -> k a (mkHooplLabel v)) z s

{-# INLINE setFoldr #-}
setFoldr :: (Label -> t -> t) -> t -> LabelSet -> t
setFoldr k z (LS s) = S.foldr (\v a -> k (mkHooplLabel v) a) z s

{-# INLINE setElems #-}
setElems :: LabelSet -> [Label]
setElems (LS s) = map mkHooplLabel (S.elems s)

{-# INLINE setFromList #-}
setFromList :: [Label] -> LabelSet
setFromList ks  = LS (S.fromList (map lblToUnique ks))

-----------------------------------------------------------------------------
-- LabelMap

newtype LabelMap v = LM (UniqDFM Word64 v)
  deriving newtype (Functor, Foldable)
  deriving stock   Traversable

mapNull :: LabelMap a -> Bool
mapNull (LM m) = isNullUDFM m

{-# INLINE mapSize #-}
mapSize :: LabelMap a -> Int
mapSize (LM m) = sizeUDFM m

mapMember :: Label -> LabelMap a -> Bool
mapMember (Label k) (LM m) = elemUDFM k m

mapLookup :: Label -> LabelMap a -> Maybe a
mapLookup (Label k) (LM m) = lookupUDFM m k

mapFindWithDefault :: a -> Label -> LabelMap a -> a
mapFindWithDefault def (Label k) (LM m) = fromMaybe def $ lookupUDFM m k

mapEmpty :: LabelMap v
mapEmpty = LM emptyUDFM

mapSingleton :: Label -> v -> LabelMap v
mapSingleton (Label k) v = LM (unitUDFM k v)

mapInsert :: Label -> v -> LabelMap v -> LabelMap v
mapInsert (Label k) v (LM m) = LM (addToUDFM m k v)

mapInsertWith :: (v -> v -> v) -> Label -> v -> LabelMap v -> LabelMap v
mapInsertWith f (Label k) v (LM m) = LM (addToUDFM_C f m k v)

mapDelete :: Label -> LabelMap v -> LabelMap v
mapDelete (Label k) (LM m) = LM (delFromUDFM m k)

mapAlter :: (Maybe v -> Maybe v) -> Label -> LabelMap v -> LabelMap v
mapAlter f (Label k) (LM m) = LM (alterUDFM f m k)

mapAdjust :: (v -> v) -> Label -> LabelMap v -> LabelMap v
mapAdjust f (Label k) (LM m) = LM (adjustUDFM f m k)

mapUnion :: LabelMap v -> LabelMap v -> LabelMap v
mapUnion (LM x) (LM y) = LM (plusUDFM x y)

mapUnionWithKey :: (Label -> v -> v -> v) -> LabelMap v -> LabelMap v -> LabelMap v
mapUnionWithKey f (LM x) (LM y) = LM (plusUDFM_CK (f . mkHooplLabel . getKey) x y)

{-# INLINE mapUnions #-}
mapUnions :: [LabelMap a] -> LabelMap a
mapUnions [] = mapEmpty
mapUnions maps = foldl1' mapUnion maps

mapDifference :: LabelMap v -> LabelMap b -> LabelMap v
mapDifference (LM x) (LM y) = LM (minusUDFM x y)

mapIntersection :: LabelMap v -> LabelMap v -> LabelMap v
mapIntersection (LM x) (LM y) = LM (intersectUDFM x y)

mapMap :: (a -> v) -> LabelMap a -> LabelMap v
mapMap f (LM m) = LM (mapUDFM f m)

mapMapWithKey :: (Label -> a -> v) -> LabelMap a -> LabelMap v
mapMapWithKey f (LM m) = LM (mapWithInternalKeyUDFM (f . mkHooplLabel) m)

{-# INLINE mapFoldl #-}
mapFoldl :: (a -> b -> a) -> a -> LabelMap b -> a
mapFoldl k z lm = mapFoldr (\b g x -> g (k x b)) id lm z -- foldl as foldr
  -- REVIEW: Is this implementation bad for performance?

{-# INLINE mapFoldr #-}
mapFoldr :: (a -> b -> b) -> b -> LabelMap a -> b
mapFoldr k z (LM m) = foldUDFM k z m

{-# INLINE mapFoldlWithKey #-}
mapFoldlWithKey :: (t -> Label -> b -> t) -> t -> LabelMap b -> t
mapFoldlWithKey k z (LM m) = foldWithKeyUDFM (\t v acc -> k acc (mkHooplLabel $ getKey t) v) z m

{-# INLINEABLE mapFilter #-}
mapFilter :: (v -> Bool) -> LabelMap v -> LabelMap v
mapFilter f (LM m) = LM (filterUDFM f m)

{-# INLINEABLE mapFilterWithKey #-}
mapFilterWithKey :: (Label -> v -> Bool) -> LabelMap v -> LabelMap v
mapFilterWithKey f (LM m)  = LM (filterUDFM_Directly (f . mkHooplLabel . getKey) m)

{-# INLINE mapElems #-}
mapElems :: LabelMap a -> [a]
mapElems (LM m) = eltsUDFM m

{-# INLINE mapKeys #-}
mapKeys :: LabelMap a -> [Label]
mapKeys (LM m) = map (mkHooplLabel . getKey . fst) (udfmToList m)

{-# INLINE mapToList #-}
mapToList :: LabelMap b -> [(Label, b)]
mapToList (LM m) = [(mkHooplLabel $ getKey k, v) | (k, v) <- udfmToList m]

{-# INLINE mapFromList #-}
mapFromList :: [(Label, v)] -> LabelMap v
mapFromList assocs = LM (listToUDFM_Directly [(mkUniqueGrimily $ lblToUnique k, v) | (k, v) <- assocs])

{-# INLINE mapFromListWith #-}
mapFromListWith :: (v -> v -> v) -> [(Label, v)] -> LabelMap v
mapFromListWith f assocs = LM (listToUDFM_C_Directly f [(mkUniqueGrimily $ lblToUnique k, v) | (k, v) <- assocs])

-----------------------------------------------------------------------------
-- Instances

instance Outputable LabelSet where
  ppr = ppr . setElems

instance Outputable a => Outputable (LabelMap a) where
  ppr = ppr . mapToList

instance OutputableP env a => OutputableP env (LabelMap a) where
  pdoc env = pdoc env . mapToList

instance TrieMap LabelMap where
  type Key LabelMap = Label
  emptyTM       = mapEmpty
  lookupTM k m  = mapLookup k m
  alterTM k f m = mapAlter f k m
  foldTM k m z  = mapFoldr k z m
  filterTM f m  = mapFilter f m

-----------------------------------------------------------------------------
-- FactBase

type FactBase f = LabelMap f

lookupFact :: Label -> FactBase f -> Maybe f
lookupFact = mapLookup

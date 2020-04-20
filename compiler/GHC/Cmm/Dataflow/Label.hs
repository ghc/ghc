{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GHC.Cmm.Dataflow.Label
    ( Label
    , LabelMap
    , LabelSet
    , FactBase
    , lookupFact
    , mkHooplLabel
    ) where

import GHC.Prelude

import GHC.Utils.Outputable

-- TODO: This should really just use GHC's Unique and Uniq{Set,FM}
import GHC.Cmm.Dataflow.Collections

import GHC.Types.Unique (Uniquable(..))
import GHC.Data.TrieMap


-----------------------------------------------------------------------------
--              Label
-----------------------------------------------------------------------------

newtype Label = Label { lblToUnique :: Int }
  deriving (Eq, Ord)

mkHooplLabel :: Int -> Label
mkHooplLabel = Label

instance Show Label where
  show (Label n) = "L" ++ show n

instance Uniquable Label where
  getUnique label = getUnique (lblToUnique label)

instance Outputable Label where
  ppr label = ppr (getUnique label)

-----------------------------------------------------------------------------
-- LabelSet

newtype LabelSet = LS UniqueSet deriving (Eq, Ord, Show, Monoid, Semigroup)

instance IsSet LabelSet where
  type ElemOf LabelSet = Label

  setNull (LS s) = setNull s
  setSize (LS s) = setSize s
  setMember (Label k) (LS s) = setMember k s

  setEmpty = LS setEmpty
  setSingleton (Label k) = LS (setSingleton k)
  setInsert (Label k) (LS s) = LS (setInsert k s)
  setDelete (Label k) (LS s) = LS (setDelete k s)

  setUnion (LS x) (LS y) = LS (setUnion x y)
  setDifference (LS x) (LS y) = LS (setDifference x y)
  setIntersection (LS x) (LS y) = LS (setIntersection x y)
  setIsSubsetOf (LS x) (LS y) = setIsSubsetOf x y
  setFilter f (LS s) = LS (setFilter (f . mkHooplLabel) s)
  setFoldl k z (LS s) = setFoldl (\a v -> k a (mkHooplLabel v)) z s
  setFoldr k z (LS s) = setFoldr (\v a -> k (mkHooplLabel v) a) z s

  setElems (LS s) = map mkHooplLabel (setElems s)
  setFromList ks = LS (setFromList (map lblToUnique ks))

-----------------------------------------------------------------------------
-- LabelMap

newtype LabelMap v = LM (UniqueMap v)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance IsMap LabelMap where
  type KeyOf LabelMap = Label

  mapNull (LM m) = mapNull m
  mapSize (LM m) = mapSize m
  mapMember (Label k) (LM m) = mapMember k m
  mapLookup (Label k) (LM m) = mapLookup k m
  mapFindWithDefault def (Label k) (LM m) = mapFindWithDefault def k m

  mapEmpty = LM mapEmpty
  mapSingleton (Label k) v = LM (mapSingleton k v)
  mapInsert (Label k) v (LM m) = LM (mapInsert k v m)
  mapInsertWith f (Label k) v (LM m) = LM (mapInsertWith f k v m)
  mapDelete (Label k) (LM m) = LM (mapDelete k m)
  mapAlter f (Label k) (LM m) = LM (mapAlter f k m)
  mapAdjust f (Label k) (LM m) = LM (mapAdjust f k m)

  mapUnion (LM x) (LM y) = LM (mapUnion x y)
  mapUnionWithKey f (LM x) (LM y) = LM (mapUnionWithKey (f . mkHooplLabel) x y)
  mapDifference (LM x) (LM y) = LM (mapDifference x y)
  mapIntersection (LM x) (LM y) = LM (mapIntersection x y)
  mapIsSubmapOf (LM x) (LM y) = mapIsSubmapOf x y

  mapMap f (LM m) = LM (mapMap f m)
  mapMapWithKey f (LM m) = LM (mapMapWithKey (f . mkHooplLabel) m)
  mapFoldl k z (LM m) = mapFoldl k z m
  mapFoldr k z (LM m) = mapFoldr k z m
  mapFoldlWithKey k z (LM m) =
      mapFoldlWithKey (\a v -> k a (mkHooplLabel v)) z m
  mapFoldMapWithKey f (LM m) = mapFoldMapWithKey (\k v -> f (mkHooplLabel k) v) m
  {-# INLINEABLE mapFilter #-}
  mapFilter f (LM m) = LM (mapFilter f m)
  {-# INLINEABLE mapFilterWithKey #-}
  mapFilterWithKey f (LM m) = LM (mapFilterWithKey (f . mkHooplLabel) m)

  mapElems (LM m) = mapElems m
  mapKeys (LM m) = map mkHooplLabel (mapKeys m)
  {-# INLINEABLE mapToList #-}
  mapToList (LM m) = [(mkHooplLabel k, v) | (k, v) <- mapToList m]
  mapFromList assocs = LM (mapFromList [(lblToUnique k, v) | (k, v) <- assocs])
  mapFromListWith f assocs = LM (mapFromListWith f [(lblToUnique k, v) | (k, v) <- assocs])

-----------------------------------------------------------------------------
-- Instances

instance Outputable LabelSet where
  ppr = ppr . setElems

instance Outputable a => Outputable (LabelMap a) where
  ppr = ppr . mapToList

instance TrieMap LabelMap where
  type Key LabelMap = Label
  emptyTM = mapEmpty
  lookupTM k m = mapLookup k m
  alterTM k f m = mapAlter f k m
  foldTM k m z = mapFoldr k z m
  mapTM f m = mapMap f m

-----------------------------------------------------------------------------
-- FactBase

type FactBase f = LabelMap f

lookupFact :: Label -> FactBase f -> Maybe f
lookupFact = mapLookup

-- | A tiny wrapper around 'IntSet.IntSet' for representing sets of 'Enum'
-- things.
{-# LANGUAGE TypeFamilies #-}

module EnumSet
    ( EnumSet
    , member
    , insert
    , delete
    , toList
    , fromList
    , empty
    ) where

import GhcPrelude

import qualified Data.IntSet as S
import qualified Data.IntSet as IntSet
import Collections

newtype EnumSet a = EnumSet IntSet.IntSet

instance Enum a => IsSet (EnumSet a) where
  type ElemOf (EnumSet a) = a

  setNull (EnumSet s) = S.null s
  setSize (EnumSet s) = S.size s
  setMember k (EnumSet s) = S.member (fromEnum k) s

  setEmpty = EnumSet $ S.empty
  setSingleton k = EnumSet $ S.singleton (fromEnum k)
  setInsert k (EnumSet s) = EnumSet $ S.insert (fromEnum k) s
  setDelete k (EnumSet s) = EnumSet $ S.delete (fromEnum k) s

  setUnion (EnumSet x) (EnumSet y) = EnumSet $ S.union x y
  setDifference (EnumSet x) (EnumSet y) = EnumSet $ S.difference x y
  setIntersection (EnumSet x) (EnumSet y) = EnumSet $ S.intersection x y
  setIsSubsetOf (EnumSet x) (EnumSet y) = S.isSubsetOf x y
  setFilter f (EnumSet s) = EnumSet $ S.filter (f . toEnum) s

  setFromList ks = EnumSet . S.fromList . map fromEnum $ ks

instance (Enum a) => IsDetSet (EnumSet a) where

  -- | Strict left fold
  setFoldl k z (EnumSet s) = S.foldl' (\b a -> k b (toEnum a)) z s
  setFoldr k z (EnumSet s) = S.foldr  (\a b -> k (toEnum a) b) z s

  setElems (EnumSet s) = map toEnum $ S.elems s

member :: Enum a => a -> EnumSet a -> Bool
member x (EnumSet s) = IntSet.member (fromEnum x) s

insert :: Enum a => a -> EnumSet a -> EnumSet a
insert x (EnumSet s) = EnumSet $ IntSet.insert (fromEnum x) s

delete :: Enum a => a -> EnumSet a -> EnumSet a
delete x (EnumSet s) = EnumSet $ IntSet.delete (fromEnum x) s

toList :: Enum a => EnumSet a -> [a]
toList (EnumSet s) = map toEnum $ IntSet.toList s

fromList :: Enum a => [a] -> EnumSet a
fromList = EnumSet . IntSet.fromList . map fromEnum

empty :: EnumSet a
empty = EnumSet IntSet.empty

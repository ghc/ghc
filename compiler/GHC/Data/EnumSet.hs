-- | A tiny wrapper around 'IntSet.IntSet' for representing sets of 'Enum'
-- things.
module GHC.Data.EnumSet
    ( EnumSet
    , member
    , insert
    , delete
    , toList
    , fromList
    , empty
    , difference
    ) where

import GHC.Prelude

import qualified Data.IntSet as IntSet

newtype EnumSet a = EnumSet IntSet.IntSet

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

difference :: EnumSet a -> EnumSet a -> EnumSet a
difference (EnumSet a) (EnumSet b) = EnumSet (IntSet.difference a b)

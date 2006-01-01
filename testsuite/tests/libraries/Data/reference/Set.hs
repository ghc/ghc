module Set where

import Prelude hiding (lookup, map, filter)
import qualified Data.List as L
import Data.Maybe

type Set a = [a]
-- with the additional invariant that an element cannot be present twice (using Eq equality)
-- NOTE: the list needs not to be sorted.

null :: Set a -> Bool
null = L.null

size :: Set a -> Int
size = L.length

member :: Ord a => a -> Set a -> Bool
member = L.elem

isSubsetOf :: Ord a => Set a -> Set a -> Bool
isSubsetOf s1 s2 = all (`member` s2) s1

isProperSubsetOf :: Ord a => Set a -> Set a -> Bool
isProperSubsetOf s1 s2 = size s1 < size s2 && isSubsetOf s1 s2

empty :: Set a
empty = []

singleton :: a -> Set a
singleton a = [a]

insert :: Eq a => a -> Set a -> Set a
insert x s = x:delete x s

delete :: Eq a => a -> Set a -> Set a
delete x s = L.filter (/= x) s

union :: Eq a => Set a -> Set a -> Set a
union s t = L.nub (s ++ t)

unions :: Eq a => [Set a] -> Set a
unions = foldl union []

difference :: Eq a => Set a -> Set a -> Set a
difference = (L.\\)

intersection :: Eq a => Set a -> Set a -> Set a
intersection = L.intersect

filter :: Eq a => (a -> Bool) -> Set a -> Set a
filter = L.filter

partition :: Eq a => (a -> Bool) -> Set a -> (Set a, Set a)
partition = L.partition

map :: (Eq a, Eq b) => (a -> b) -> Set a -> Set b
map f = L.nub . L.map f

elems :: Ord a => Set a -> [a]
elems = L.sort

toList :: Ord a => Set a -> [a]
toList = L.sort

fromList :: Eq a => [a] -> Set a
fromList = L.nub

toAscList :: Ord a => Set a -> [a]
toAscList = toList

fromAscList :: Eq a => [a] -> Set a
fromAscList = fromList




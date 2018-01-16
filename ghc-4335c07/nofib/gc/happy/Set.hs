module Set (
   Set, null, member, empty, singleton,
   union, difference, filter, fold,
   fromList, toAscList
) where

import Prelude hiding ( null, filter )
import Data.Set

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 603
null :: Set a -> Bool
null = isEmptySet

member :: Ord a => a -> Set a -> Bool
member = elementOf

empty  :: Set a
empty = emptySet

singleton :: a -> Set a
singleton = unitSet

difference :: Ord a => Set a -> Set a -> Set a
difference = minusSet

fromList :: Ord a => [a]  -> Set a
fromList = mkSet

filter :: Ord a => (a -> Bool) -> Set a -> Set a
filter p s = mkSet [ x | x <- setToList s, p x ]

fold :: (a -> b -> b) -> b -> Set a -> b
fold f z = foldr f z . setToList

toAscList :: Set a -> [a] 
toAscList = setToList
#endif

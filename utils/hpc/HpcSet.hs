module HpcSet (	module HpcSet ) where 

import qualified Data.Set as Set

type Set a = Set.Set a

empty  :: Set a
insert :: (Ord a) => a -> Set a -> Set a
member :: (Ord a) => a -> Set a -> Bool
null   :: Set a -> Bool
intersection :: Ord a => Set a -> Set a -> Set a
fromList :: Ord a => [a] -> Set a
toList :: Set a -> [a]
union :: Ord a => Set a -> Set a -> Set a

#if __GLASGOW_HASKELL__ < 604

empty  = Set.emptySet
insert = flip Set.addToSet
member = Set.elementOf
null   = Set.isEmptySet
intersection = Set.intersect
fromList = Set.mkSet
toList = Set.setToList
union = Set.union

#else

empty  = Set.empty
insert = Set.insert
member = Set.member
null   = Set.null
intersection = Set.intersection
fromList = Set.fromList
toList = Set.toList
union = Set.union

#endif


module Set (
   Set,
   member,
   empty, singleton, delete,
   union, unions,
   elems, fromList
) where

import Data.Set 

#if __GLASGOW_HASKELL__ < 603
member :: Ord a => a -> Set a -> Bool
member = elementOf

empty  :: Set a
empty = emptySet

singleton :: a -> Set a
singleton  = unitSet

delete :: Ord a => a -> Set a -> Set a
delete = flip delFromSet

unions :: Ord a => [Set a] -> Set a
unions = unionManySets

elems :: Set a -> [a] 
elems = setToList

fromList :: Ord a => [a] -> Set a 
fromList = mkSet
#endif

module Map (
   Map,
   member, lookup, findWithDefault,
   empty,
   insert, insertWith,
   union, unionWith, unions,
   elems,
   fromList, fromListWith,
   toAscList
) where

import Prelude hiding ( lookup )
import qualified Set

#if __GLASGOW_HASKELL__ < 503
import FiniteMap
#elif __GLASGOW_HASKELL__ < 603
import Data.FiniteMap
#else
import Data.Map
#endif

#if __GLASGOW_HASKELL__ < 603
type Map k a = FiniteMap k a

member :: Ord k => k -> Map k a -> Bool
member = elemFM

lookup :: Ord k => k -> Map k a -> Maybe a
lookup = flip lookupFM

findWithDefault :: Ord k => a -> k -> Map k a -> a
findWithDefault a k m = lookupWithDefaultFM m a k

empty :: Map k a
empty = emptyFM

insert :: Ord k => k -> a -> Map k a -> Map k a
insert k a m = addToFM m k a

insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith c k a m = addToFM_C c m k a

union :: Ord k => Map k a -> Map k a -> Map k a
union = flip plusFM

unionWith :: Ord k => (a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWith c l r = plusFM_C c r l

unions :: Ord k => [Map k a] -> Map k a
unions = foldr plusFM emptyFM

elems :: Map k a -> [a]
elems = eltsFM

fromList :: Ord k => [(k,a)] -> Map k a
fromList = listToFM

fromListWith :: Ord k => (a -> a -> a) -> [(k,a)] -> Map k a 
fromListWith = flip addListToFM_C emptyFM

toAscList :: Map k a -> [(k,a)]
toAscList = fmToList
#endif

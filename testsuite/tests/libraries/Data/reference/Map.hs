module Map where

import Prelude hiding (lookup, map)
import qualified Data.List as L
import Data.Maybe

type Map k a = [(k,a)]

type Set k = [k]

-- * utilities

withoutKey op f = op (\k x -> f x)

withoutKey2 op f = op (\k x y -> f x y)

comparing f = \x y -> f x `compare` f y

testing f = \x y -> f x == f y


-- * Operators
(!) k = fromJust . lookup k

--TODO (\\)

-- * Query
null :: Map k a -> Bool
null = L.null

size :: Map k a -> Int
size = L.length

member :: Ord k => k -> Map k a -> Bool
member k l = k `L.elem` keys l

lookup :: (Monad m, Ord k) => k -> Map k a -> m a
lookup k l = do (_,x) <- lookupAssoc k l
                return x

--lookupAssoc :: (Monad m, Ord k) => k -> Map k a -> m (k,a)
lookupAssoc k l = if L.null result then fail "Key not found" else return (head result)
    where result = [x | x <- l, fst x == k]

findWithDefault :: Ord k => a -> k -> Map k a -> a
findWithDefault a k = fromMaybe a . lookup k

-- * Construction
empty :: Map k a
empty = []

singleton :: k -> a -> Map k a
singleton k a = [(k,a)]

-- ** Insertion
insert :: Ord k => k -> a -> Map k a -> Map k a
insert = insertWith const

insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith f k x m = insertWithKey (\k x y -> f x y) k x m

insertWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWithKey f k x m = snd (insertLookupWithKey f k x m)

insertLookupWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> (Maybe a, Map k a)
insertLookupWithKey f k x m = (lookup k m, (k,x''):delete k m)
    where x'' = fromMaybe x $ fmap (\x' -> f k x x') (lookup k m)
                          

-- ** Delete\/Update

delete :: Ord k => k -> Map k a -> Map k a
delete k = filter (\x->fst x /= k)

adjust :: Ord k => (a -> a) -> k -> Map k a -> Map k a
adjust = withoutKey adjustWithKey

adjustWithKey :: Ord k => (k -> a -> a) -> k -> Map k a -> Map k a
adjustWithKey f k = L.map (\(k',x') -> (k',if k' == k then f k' x' else x'))

update :: Ord k => (a -> Maybe a) -> k -> Map k a -> Map k a
update = withoutKey updateWithKey

updateWithKey :: Ord k => (k -> a -> Maybe a) -> k -> Map k a -> Map k a
updateWithKey f k m = maybeToList (newElem) ++ delete k m
    where newElem = do (k',x') <- lookupAssoc k m
                       x'' <- f k' x'
                       return (k',x'')

updateLookupWithKey :: Ord k => (k -> a -> Maybe a) -> k -> Map k a -> (Maybe a, Map k a)
updateLookupWithKey f k m = (lookup k m, updateWithKey f k m)

-- * Combine

-- ** Union

union :: Ord k => Map k a -> Map k a -> Map k a
union = unionWith const

unionWith :: Ord k => (a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWith = withoutKey2 unionWithKey

unionWithKey :: Ord k => (k -> a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWithKey f m1 m2 = L.map coalese $ L.groupBy (testing fst) $ L.sortBy (comparing fst) (m1 ++ m2)
    where coalese [(k,a)] = (k,a)
          coalese [(k1,a1),(k2,a2)] = (k1, f k1 a1 a2)

unions :: Ord k => [Map k a] -> Map k a
unions = unionsWith const

unionsWith :: Ord k => (a -> a -> a) -> [Map k a] -> Map k a
unionsWith f = foldl (unionWith f) empty

difference :: Ord k => Map k a -> Map k b -> Map k a
difference = differenceWith (\_ _ -> Nothing)

differenceWith :: Ord k => (a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
differenceWith = withoutKey2 differenceWithKey

differenceWithKey :: Ord k => (k -> a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
differenceWithKey f m1 m2 = catMaybes $ L.map newElem m1
    where newElem (k,x) = do x <- case lookup k m2 of
                                    Nothing -> Just x
                                    Just y -> f k x y
                             return (k,x)         
                                     
            
-- ** Intersection
intersection :: Ord k => Map k a -> Map k b -> Map k a
intersection = intersectionWith const

intersectionWith :: Ord k => (a -> b -> c) -> Map k a -> Map k b -> Map k c
intersectionWith = withoutKey2 intersectionWithKey
            
intersectionWithKey :: Ord k => (k -> a -> b -> c) -> Map k a -> Map k b -> Map k c
intersectionWithKey f m1 m2 = [(k,f k x y) | (k,x) <- m1, y <- lookup k m2]

-- * Traversal
-- ** Map

-- TODO map = 

mapWithKey :: (k -> a -> b) -> Map k a -> Map k b
mapWithKey f m = [(k, f k x) | (k,x) <- m]

-- TODO mapAccum
-- TODO mapAccumWithKey

-- TODO mapKeys
-- TODO	    , mapKeys
-- TODO	    , mapKeysWith
-- TODO	    , mapKeysMonotonic

            -- ** Fold
-- TODO fold
-- TODO foldWithKey

            -- * Conversion
elems :: Ord k => Map k a -> [a]
elems = L.map snd . L.sortBy (comparing fst)

keys :: Ord k => Map k a -> [k]
keys = L.sort . L.map fst

keysSet :: Ord k => Map k a -> Set k
keysSet = keys

assocs ::  Ord k => Map k a -> [(k, a)]
assocs = L.sortBy (comparing fst)
            

            -- ** Lists
-- TODO toList
-- TODO fromList
-- TODO fromListWith
-- TODO fromListWithKey

            -- ** Ordered lists
toAscList :: Ord k => Map k a -> [(k, a)]
toAscList = L.sortBy (comparing fst)

fromAscList :: Eq k => [(k, a)] -> Map k a
fromAscList = L.nubBy (testing fst)

-- TODO fromAscListWith
-- TODO fromAscListWithKey
-- TODO fromDistinctAscList

            -- * Filter 
-- TODO filter
-- TODO filterWithKey
-- TODO partition
-- TODO partitionWithKey

-- TODO split         
-- TODO splitLookup   

            -- * Submap
-- TODO isSubmapOf, isSubmapOfBy
-- TODO isProperSubmapOf, isProperSubmapOfBy

            -- * Indexed 
-- TODO lookupIndex
-- TODO findIndex
-- TODO elemAt
-- TODO updateAt
-- TODO deleteAt

            -- * Min\/Max
-- TODO findMin
-- TODO findMax
-- TODO deleteMin
-- TODO deleteMax
-- TODO deleteFindMin
-- TODO deleteFindMax
-- TODO updateMin
-- TODO updateMax
-- TODO updateMinWithKey
-- TODO updateMaxWithKey
            
            -- * Debugging

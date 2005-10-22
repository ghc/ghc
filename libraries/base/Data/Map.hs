-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Map
-- Copyright   :  (c) Daan Leijen 2002
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- An efficient implementation of maps from keys to values (dictionaries).
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with Prelude functions.  eg.
--
-- >  import Data.Map as Map
--
-- The implementation of 'Map' is based on /size balanced/ binary trees (or
-- trees of /bounded balance/) as described by:
--
--    * Stephen Adams, \"/Efficient sets: a balancing act/\",
--	Journal of Functional Programming 3(4):553-562, October 1993,
--	<http://www.swiss.ai.mit.edu/~adams/BB>.
--
--    * J. Nievergelt and E.M. Reingold,
--	\"/Binary search trees of bounded balance/\",
--	SIAM journal of computing 2(1), March 1973.
-----------------------------------------------------------------------------

module Data.Map  ( 
            -- * Map type
              Map          -- instance Eq,Show,Read

            -- * Operators
            , (!), (\\)


            -- * Query
            , null
            , size
            , member
            , lookup
            , findWithDefault
            
            -- * Construction
            , empty
            , singleton

            -- ** Insertion
            , insert
            , insertWith, insertWithKey, insertLookupWithKey
            
            -- ** Delete\/Update
            , delete
            , adjust
            , adjustWithKey
            , update
            , updateWithKey
            , updateLookupWithKey

            -- * Combine

            -- ** Union
            , union         
            , unionWith          
            , unionWithKey
            , unions
	    , unionsWith

            -- ** Difference
            , difference
            , differenceWith
            , differenceWithKey
            
            -- ** Intersection
            , intersection           
            , intersectionWith
            , intersectionWithKey

            -- * Traversal
            -- ** Map
            , map
            , mapWithKey
            , mapAccum
            , mapAccumWithKey
	    , mapKeys
	    , mapKeysWith
	    , mapKeysMonotonic

            -- ** Fold
            , fold
            , foldWithKey

            -- * Conversion
            , elems
            , keys
	    , keysSet
            , assocs
            
            -- ** Lists
            , toList
            , fromList
            , fromListWith
            , fromListWithKey

            -- ** Ordered lists
            , toAscList
            , fromAscList
            , fromAscListWith
            , fromAscListWithKey
            , fromDistinctAscList

            -- * Filter 
            , filter
            , filterWithKey
            , partition
            , partitionWithKey

            , split         
            , splitLookup   

            -- * Submap
            , isSubmapOf, isSubmapOfBy
            , isProperSubmapOf, isProperSubmapOfBy

            -- * Indexed 
            , lookupIndex
            , findIndex
            , elemAt
            , updateAt
            , deleteAt

            -- * Min\/Max
            , findMin
            , findMax
            , deleteMin
            , deleteMax
            , deleteFindMin
            , deleteFindMax
            , updateMin
            , updateMax
            , updateMinWithKey
            , updateMaxWithKey
            
            -- * Debugging
            , showTree
            , showTreeWith
            , valid
            ) where

import Prelude hiding (lookup,map,filter,foldr,foldl,null)
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Typeable

{-
-- for quick check
import qualified Prelude
import qualified List
import Debug.QuickCheck       
import List(nub,sort)    
-}

#if __GLASGOW_HASKELL__
import Text.Read
import Data.Generics.Basics
import Data.Generics.Instances
#endif

{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}
infixl 9 !,\\ --

-- | /O(log n)/. Find the value at a key.
-- Calls 'error' when the element can not be found.
(!) :: Ord k => Map k a -> k -> a
m ! k    = find k m

-- | /O(n+m)/. See 'difference'.
(\\) :: Ord k => Map k a -> Map k b -> Map k a
m1 \\ m2 = difference m1 m2

{--------------------------------------------------------------------
  Size balanced trees.
--------------------------------------------------------------------}
-- | A Map from keys @k@ to values @a@. 
data Map k a  = Tip 
              | Bin {-# UNPACK #-} !Size !k a !(Map k a) !(Map k a) 

type Size     = Int

#if __GLASGOW_HASKELL__

{--------------------------------------------------------------------
  A Data instance  
--------------------------------------------------------------------}

-- This instance preserves data abstraction at the cost of inefficiency.
-- We omit reflection services for the sake of data abstraction.

instance (Data k, Data a, Ord k) => Data (Map k a) where
  gfoldl f z map = z fromList `f` (toList map)
  toConstr _     = error "toConstr"
  gunfold _ _    = error "gunfold"
  dataTypeOf _   = mkNorepType "Data.Map.Map"

#endif

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}
-- | /O(1)/. Is the map empty?
null :: Map k a -> Bool
null t
  = case t of
      Tip             -> True
      Bin sz k x l r  -> False

-- | /O(1)/. The number of elements in the map.
size :: Map k a -> Int
size t
  = case t of
      Tip             -> 0
      Bin sz k x l r  -> sz


-- | /O(log n)/. Lookup the value at a key in the map.
lookup :: (Monad m,Ord k) => k -> Map k a -> m a
lookup k t = case lookup' k t of
    Just x -> return x
    Nothing -> fail "Data.Map.lookup: Key not found"
lookup' :: Ord k => k -> Map k a -> Maybe a
lookup' k t
  = case t of
      Tip -> Nothing
      Bin sz kx x l r
          -> case compare k kx of
               LT -> lookup' k l
               GT -> lookup' k r
               EQ -> Just x       

-- | /O(log n)/. Is the key a member of the map?
member :: Ord k => k -> Map k a -> Bool
member k m
  = case lookup k m of
      Nothing -> False
      Just x  -> True

-- | /O(log n)/. Find the value at a key.
-- Calls 'error' when the element can not be found.
find :: Ord k => k -> Map k a -> a
find k m
  = case lookup k m of
      Nothing -> error "Map.find: element not in the map"
      Just x  -> x

-- | /O(log n)/. The expression @('findWithDefault' def k map)@ returns
-- the value at key @k@ or returns @def@ when the key is not in the map.
findWithDefault :: Ord k => a -> k -> Map k a -> a
findWithDefault def k m
  = case lookup k m of
      Nothing -> def
      Just x  -> x



{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
-- | /O(1)/. The empty map.
empty :: Map k a
empty 
  = Tip

-- | /O(1)/. A map with a single element.
singleton :: k -> a -> Map k a
singleton k x  
  = Bin 1 k x Tip Tip

{--------------------------------------------------------------------
  Insertion
--------------------------------------------------------------------}
-- | /O(log n)/. Insert a new key and value in the map.
-- If the key is already present in the map, the associated value is
-- replaced with the supplied value, i.e. 'insert' is equivalent to
-- @'insertWith' 'const'@.
insert :: Ord k => k -> a -> Map k a -> Map k a
insert kx x t
  = case t of
      Tip -> singleton kx x
      Bin sz ky y l r
          -> case compare kx ky of
               LT -> balance ky y (insert kx x l) r
               GT -> balance ky y l (insert kx x r)
               EQ -> Bin sz kx x l r

-- | /O(log n)/. Insert with a combining function.
insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith f k x m          
  = insertWithKey (\k x y -> f x y) k x m

-- | /O(log n)/. Insert with a combining function.
insertWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWithKey f kx x t
  = case t of
      Tip -> singleton kx x
      Bin sy ky y l r
          -> case compare kx ky of
               LT -> balance ky y (insertWithKey f kx x l) r
               GT -> balance ky y l (insertWithKey f kx x r)
               EQ -> Bin sy ky (f ky x y) l r

-- | /O(log n)/. The expression (@'insertLookupWithKey' f k x map@)
-- is a pair where the first element is equal to (@'lookup' k map@)
-- and the second element equal to (@'insertWithKey' f k x map@).
insertLookupWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> (Maybe a,Map k a)
insertLookupWithKey f kx x t
  = case t of
      Tip -> (Nothing, singleton kx x)
      Bin sy ky y l r
          -> case compare kx ky of
               LT -> let (found,l') = insertLookupWithKey f kx x l in (found,balance ky y l' r)
               GT -> let (found,r') = insertLookupWithKey f kx x r in (found,balance ky y l r')
               EQ -> (Just y, Bin sy ky (f ky x y) l r)

{--------------------------------------------------------------------
  Deletion
  [delete] is the inlined version of [deleteWith (\k x -> Nothing)]
--------------------------------------------------------------------}
-- | /O(log n)/. Delete a key and its value from the map. When the key is not
-- a member of the map, the original map is returned.
delete :: Ord k => k -> Map k a -> Map k a
delete k t
  = case t of
      Tip -> Tip
      Bin sx kx x l r 
          -> case compare k kx of
               LT -> balance kx x (delete k l) r
               GT -> balance kx x l (delete k r)
               EQ -> glue l r

-- | /O(log n)/. Adjust a value at a specific key. When the key is not
-- a member of the map, the original map is returned.
adjust :: Ord k => (a -> a) -> k -> Map k a -> Map k a
adjust f k m
  = adjustWithKey (\k x -> f x) k m

-- | /O(log n)/. Adjust a value at a specific key. When the key is not
-- a member of the map, the original map is returned.
adjustWithKey :: Ord k => (k -> a -> a) -> k -> Map k a -> Map k a
adjustWithKey f k m
  = updateWithKey (\k x -> Just (f k x)) k m

-- | /O(log n)/. The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
update :: Ord k => (a -> Maybe a) -> k -> Map k a -> Map k a
update f k m
  = updateWithKey (\k x -> f x) k m

-- | /O(log n)/. The expression (@'updateWithKey' f k map@) updates the
-- value @x@ at @k@ (if it is in the map). If (@f k x@) is 'Nothing',
-- the element is deleted. If it is (@'Just' y@), the key @k@ is bound
-- to the new value @y@.
updateWithKey :: Ord k => (k -> a -> Maybe a) -> k -> Map k a -> Map k a
updateWithKey f k t
  = case t of
      Tip -> Tip
      Bin sx kx x l r 
          -> case compare k kx of
               LT -> balance kx x (updateWithKey f k l) r
               GT -> balance kx x l (updateWithKey f k r)
               EQ -> case f kx x of
                       Just x' -> Bin sx kx x' l r
                       Nothing -> glue l r

-- | /O(log n)/. Lookup and update.
updateLookupWithKey :: Ord k => (k -> a -> Maybe a) -> k -> Map k a -> (Maybe a,Map k a)
updateLookupWithKey f k t
  = case t of
      Tip -> (Nothing,Tip)
      Bin sx kx x l r 
          -> case compare k kx of
               LT -> let (found,l') = updateLookupWithKey f k l in (found,balance kx x l' r)
               GT -> let (found,r') = updateLookupWithKey f k r in (found,balance kx x l r') 
               EQ -> case f kx x of
                       Just x' -> (Just x',Bin sx kx x' l r)
                       Nothing -> (Just x,glue l r)

{--------------------------------------------------------------------
  Indexing
--------------------------------------------------------------------}
-- | /O(log n)/. Return the /index/ of a key. The index is a number from
-- /0/ up to, but not including, the 'size' of the map. Calls 'error' when
-- the key is not a 'member' of the map.
findIndex :: Ord k => k -> Map k a -> Int
findIndex k t
  = case lookupIndex k t of
      Nothing  -> error "Map.findIndex: element is not in the map"
      Just idx -> idx

-- | /O(log n)/. Lookup the /index/ of a key. The index is a number from
-- /0/ up to, but not including, the 'size' of the map. 
lookupIndex :: (Monad m,Ord k) => k -> Map k a -> m Int
lookupIndex k t = case lookup 0 t of
    Nothing -> fail "Data.Map.lookupIndex: Key not found."
    Just x -> return x
  where
    lookup idx Tip  = Nothing
    lookup idx (Bin _ kx x l r)
      = case compare k kx of
          LT -> lookup idx l
          GT -> lookup (idx + size l + 1) r 
          EQ -> Just (idx + size l)

-- | /O(log n)/. Retrieve an element by /index/. Calls 'error' when an
-- invalid index is used.
elemAt :: Int -> Map k a -> (k,a)
elemAt i Tip = error "Map.elemAt: index out of range"
elemAt i (Bin _ kx x l r)
  = case compare i sizeL of
      LT -> elemAt i l
      GT -> elemAt (i-sizeL-1) r
      EQ -> (kx,x)
  where
    sizeL = size l

-- | /O(log n)/. Update the element at /index/. Calls 'error' when an
-- invalid index is used.
updateAt :: (k -> a -> Maybe a) -> Int -> Map k a -> Map k a
updateAt f i Tip  = error "Map.updateAt: index out of range"
updateAt f i (Bin sx kx x l r)
  = case compare i sizeL of
      LT -> updateAt f i l
      GT -> updateAt f (i-sizeL-1) r
      EQ -> case f kx x of
              Just x' -> Bin sx kx x' l r
              Nothing -> glue l r
  where
    sizeL = size l

-- | /O(log n)/. Delete the element at /index/.
-- Defined as (@'deleteAt' i map = 'updateAt' (\k x -> 'Nothing') i map@).
deleteAt :: Int -> Map k a -> Map k a
deleteAt i map
  = updateAt (\k x -> Nothing) i map


{--------------------------------------------------------------------
  Minimal, Maximal
--------------------------------------------------------------------}
-- | /O(log n)/. The minimal key of the map.
findMin :: Map k a -> (k,a)
findMin (Bin _ kx x Tip r)  = (kx,x)
findMin (Bin _ kx x l r)    = findMin l
findMin Tip                 = error "Map.findMin: empty tree has no minimal element"

-- | /O(log n)/. The maximal key of the map.
findMax :: Map k a -> (k,a)
findMax (Bin _ kx x l Tip)  = (kx,x)
findMax (Bin _ kx x l r)    = findMax r
findMax Tip                 = error "Map.findMax: empty tree has no maximal element"

-- | /O(log n)/. Delete the minimal key.
deleteMin :: Map k a -> Map k a
deleteMin (Bin _ kx x Tip r)  = r
deleteMin (Bin _ kx x l r)    = balance kx x (deleteMin l) r
deleteMin Tip                 = Tip

-- | /O(log n)/. Delete the maximal key.
deleteMax :: Map k a -> Map k a
deleteMax (Bin _ kx x l Tip)  = l
deleteMax (Bin _ kx x l r)    = balance kx x l (deleteMax r)
deleteMax Tip                 = Tip

-- | /O(log n)/. Update the value at the minimal key.
updateMin :: (a -> Maybe a) -> Map k a -> Map k a
updateMin f m
  = updateMinWithKey (\k x -> f x) m

-- | /O(log n)/. Update the value at the maximal key.
updateMax :: (a -> Maybe a) -> Map k a -> Map k a
updateMax f m
  = updateMaxWithKey (\k x -> f x) m


-- | /O(log n)/. Update the value at the minimal key.
updateMinWithKey :: (k -> a -> Maybe a) -> Map k a -> Map k a
updateMinWithKey f t
  = case t of
      Bin sx kx x Tip r  -> case f kx x of
                              Nothing -> r
                              Just x' -> Bin sx kx x' Tip r
      Bin sx kx x l r    -> balance kx x (updateMinWithKey f l) r
      Tip                -> Tip

-- | /O(log n)/. Update the value at the maximal key.
updateMaxWithKey :: (k -> a -> Maybe a) -> Map k a -> Map k a
updateMaxWithKey f t
  = case t of
      Bin sx kx x l Tip  -> case f kx x of
                              Nothing -> l
                              Just x' -> Bin sx kx x' l Tip
      Bin sx kx x l r    -> balance kx x l (updateMaxWithKey f r)
      Tip                -> Tip


{--------------------------------------------------------------------
  Union. 
--------------------------------------------------------------------}
-- | The union of a list of maps:
--   (@'unions' == 'Prelude.foldl' 'union' 'empty'@).
unions :: Ord k => [Map k a] -> Map k a
unions ts
  = foldlStrict union empty ts

-- | The union of a list of maps, with a combining operation:
--   (@'unionsWith' f == 'Prelude.foldl' ('unionWith' f) 'empty'@).
unionsWith :: Ord k => (a->a->a) -> [Map k a] -> Map k a
unionsWith f ts
  = foldlStrict (unionWith f) empty ts

-- | /O(n+m)/.
-- The expression (@'union' t1 t2@) takes the left-biased union of @t1@ and @t2@. 
-- It prefers @t1@ when duplicate keys are encountered,
-- i.e. (@'union' == 'unionWith' 'const'@).
-- The implementation uses the efficient /hedge-union/ algorithm.
-- Hedge-union is more efficient on (bigset `union` smallset)?
union :: Ord k => Map k a -> Map k a -> Map k a
union Tip t2  = t2
union t1 Tip  = t1
union t1 t2
   | size t1 >= size t2  = hedgeUnionL (const LT) (const GT) t1 t2
   | otherwise           = hedgeUnionR (const LT) (const GT) t2 t1

-- left-biased hedge union
hedgeUnionL cmplo cmphi t1 Tip 
  = t1
hedgeUnionL cmplo cmphi Tip (Bin _ kx x l r)
  = join kx x (filterGt cmplo l) (filterLt cmphi r)
hedgeUnionL cmplo cmphi (Bin _ kx x l r) t2
  = join kx x (hedgeUnionL cmplo cmpkx l (trim cmplo cmpkx t2)) 
              (hedgeUnionL cmpkx cmphi r (trim cmpkx cmphi t2))
  where
    cmpkx k  = compare kx k

-- right-biased hedge union
hedgeUnionR cmplo cmphi t1 Tip 
  = t1
hedgeUnionR cmplo cmphi Tip (Bin _ kx x l r)
  = join kx x (filterGt cmplo l) (filterLt cmphi r)
hedgeUnionR cmplo cmphi (Bin _ kx x l r) t2
  = join kx newx (hedgeUnionR cmplo cmpkx l lt) 
                 (hedgeUnionR cmpkx cmphi r gt)
  where
    cmpkx k     = compare kx k
    lt          = trim cmplo cmpkx t2
    (found,gt)  = trimLookupLo kx cmphi t2
    newx        = case found of
                    Nothing -> x
                    Just y  -> y

{--------------------------------------------------------------------
  Union with a combining function
--------------------------------------------------------------------}
-- | /O(n+m)/. Union with a combining function. The implementation uses the efficient /hedge-union/ algorithm.
unionWith :: Ord k => (a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWith f m1 m2
  = unionWithKey (\k x y -> f x y) m1 m2

-- | /O(n+m)/.
-- Union with a combining function. The implementation uses the efficient /hedge-union/ algorithm.
-- Hedge-union is more efficient on (bigset `union` smallset).
unionWithKey :: Ord k => (k -> a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWithKey f Tip t2  = t2
unionWithKey f t1 Tip  = t1
unionWithKey f t1 t2
  | size t1 >= size t2  = hedgeUnionWithKey f (const LT) (const GT) t1 t2
  | otherwise           = hedgeUnionWithKey flipf (const LT) (const GT) t2 t1
  where
    flipf k x y   = f k y x

hedgeUnionWithKey f cmplo cmphi t1 Tip 
  = t1
hedgeUnionWithKey f cmplo cmphi Tip (Bin _ kx x l r)
  = join kx x (filterGt cmplo l) (filterLt cmphi r)
hedgeUnionWithKey f cmplo cmphi (Bin _ kx x l r) t2
  = join kx newx (hedgeUnionWithKey f cmplo cmpkx l lt) 
                 (hedgeUnionWithKey f cmpkx cmphi r gt)
  where
    cmpkx k     = compare kx k
    lt          = trim cmplo cmpkx t2
    (found,gt)  = trimLookupLo kx cmphi t2
    newx        = case found of
                    Nothing -> x
                    Just y  -> f kx x y

{--------------------------------------------------------------------
  Difference
--------------------------------------------------------------------}
-- | /O(n+m)/. Difference of two maps. 
-- The implementation uses an efficient /hedge/ algorithm comparable with /hedge-union/.
difference :: Ord k => Map k a -> Map k b -> Map k a
difference Tip t2  = Tip
difference t1 Tip  = t1
difference t1 t2   = hedgeDiff (const LT) (const GT) t1 t2

hedgeDiff cmplo cmphi Tip t     
  = Tip
hedgeDiff cmplo cmphi (Bin _ kx x l r) Tip 
  = join kx x (filterGt cmplo l) (filterLt cmphi r)
hedgeDiff cmplo cmphi t (Bin _ kx x l r) 
  = merge (hedgeDiff cmplo cmpkx (trim cmplo cmpkx t) l) 
          (hedgeDiff cmpkx cmphi (trim cmpkx cmphi t) r)
  where
    cmpkx k = compare kx k   

-- | /O(n+m)/. Difference with a combining function. 
-- The implementation uses an efficient /hedge/ algorithm comparable with /hedge-union/.
differenceWith :: Ord k => (a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
differenceWith f m1 m2
  = differenceWithKey (\k x y -> f x y) m1 m2

-- | /O(n+m)/. Difference with a combining function. When two equal keys are
-- encountered, the combining function is applied to the key and both values.
-- If it returns 'Nothing', the element is discarded (proper set difference). If
-- it returns (@'Just' y@), the element is updated with a new value @y@. 
-- The implementation uses an efficient /hedge/ algorithm comparable with /hedge-union/.
differenceWithKey :: Ord k => (k -> a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
differenceWithKey f Tip t2  = Tip
differenceWithKey f t1 Tip  = t1
differenceWithKey f t1 t2   = hedgeDiffWithKey f (const LT) (const GT) t1 t2

hedgeDiffWithKey f cmplo cmphi Tip t     
  = Tip
hedgeDiffWithKey f cmplo cmphi (Bin _ kx x l r) Tip 
  = join kx x (filterGt cmplo l) (filterLt cmphi r)
hedgeDiffWithKey f cmplo cmphi t (Bin _ kx x l r) 
  = case found of
      Nothing -> merge tl tr
      Just y  -> case f kx y x of
                   Nothing -> merge tl tr
                   Just z  -> join kx z tl tr
  where
    cmpkx k     = compare kx k   
    lt          = trim cmplo cmpkx t
    (found,gt)  = trimLookupLo kx cmphi t
    tl          = hedgeDiffWithKey f cmplo cmpkx lt l
    tr          = hedgeDiffWithKey f cmpkx cmphi gt r



{--------------------------------------------------------------------
  Intersection
--------------------------------------------------------------------}
-- | /O(n+m)/. Intersection of two maps. The values in the first
-- map are returned, i.e. (@'intersection' m1 m2 == 'intersectionWith' 'const' m1 m2@).
intersection :: Ord k => Map k a -> Map k b -> Map k a
intersection m1 m2
  = intersectionWithKey (\k x y -> x) m1 m2

-- | /O(n+m)/. Intersection with a combining function.
intersectionWith :: Ord k => (a -> b -> c) -> Map k a -> Map k b -> Map k c
intersectionWith f m1 m2
  = intersectionWithKey (\k x y -> f x y) m1 m2

-- | /O(n+m)/. Intersection with a combining function.
-- Intersection is more efficient on (bigset `intersection` smallset)
intersectionWithKey :: Ord k => (k -> a -> b -> c) -> Map k a -> Map k b -> Map k c
intersectionWithKey f Tip t = Tip
intersectionWithKey f t Tip = Tip
intersectionWithKey f t1 t2
  | size t1 >= size t2  = intersectWithKey f t1 t2
  | otherwise           = intersectWithKey flipf t2 t1
  where
    flipf k x y   = f k y x

intersectWithKey f Tip t = Tip
intersectWithKey f t Tip = Tip
intersectWithKey f t (Bin _ kx x l r)
  = case found of
      Nothing -> merge tl tr
      Just y  -> join kx (f kx y x) tl tr
  where
    (lt,found,gt) = splitLookup kx t
    tl            = intersectWithKey f lt l
    tr            = intersectWithKey f gt r



{--------------------------------------------------------------------
  Submap
--------------------------------------------------------------------}
-- | /O(n+m)/. 
-- This function is defined as (@'isSubmapOf' = 'isSubmapOfBy' (==)@).
isSubmapOf :: (Ord k,Eq a) => Map k a -> Map k a -> Bool
isSubmapOf m1 m2
  = isSubmapOfBy (==) m1 m2

{- | /O(n+m)/. 
 The expression (@'isSubmapOfBy' f t1 t2@) returns 'True' if
 all keys in @t1@ are in tree @t2@, and when @f@ returns 'True' when
 applied to their respective values. For example, the following 
 expressions are all 'True':
 
 > isSubmapOfBy (==) (fromList [('a',1)]) (fromList [('a',1),('b',2)])
 > isSubmapOfBy (<=) (fromList [('a',1)]) (fromList [('a',1),('b',2)])
 > isSubmapOfBy (==) (fromList [('a',1),('b',2)]) (fromList [('a',1),('b',2)])

 But the following are all 'False':
 
 > isSubmapOfBy (==) (fromList [('a',2)]) (fromList [('a',1),('b',2)])
 > isSubmapOfBy (<)  (fromList [('a',1)]) (fromList [('a',1),('b',2)])
 > isSubmapOfBy (==) (fromList [('a',1),('b',2)]) (fromList [('a',1)])
-}
isSubmapOfBy :: Ord k => (a->b->Bool) -> Map k a -> Map k b -> Bool
isSubmapOfBy f t1 t2
  = (size t1 <= size t2) && (submap' f t1 t2)

submap' f Tip t = True
submap' f t Tip = False
submap' f (Bin _ kx x l r) t
  = case found of
      Nothing -> False
      Just y  -> f x y && submap' f l lt && submap' f r gt
  where
    (lt,found,gt) = splitLookup kx t

-- | /O(n+m)/. Is this a proper submap? (ie. a submap but not equal). 
-- Defined as (@'isProperSubmapOf' = 'isProperSubmapOfBy' (==)@).
isProperSubmapOf :: (Ord k,Eq a) => Map k a -> Map k a -> Bool
isProperSubmapOf m1 m2
  = isProperSubmapOfBy (==) m1 m2

{- | /O(n+m)/. Is this a proper submap? (ie. a submap but not equal).
 The expression (@'isProperSubmapOfBy' f m1 m2@) returns 'True' when
 @m1@ and @m2@ are not equal,
 all keys in @m1@ are in @m2@, and when @f@ returns 'True' when
 applied to their respective values. For example, the following 
 expressions are all 'True':
 
  > isProperSubmapOfBy (==) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
  > isProperSubmapOfBy (<=) (fromList [(1,1)]) (fromList [(1,1),(2,2)])

 But the following are all 'False':
 
  > isProperSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1),(2,2)])
  > isProperSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1)])
  > isProperSubmapOfBy (<)  (fromList [(1,1)])       (fromList [(1,1),(2,2)])
-}
isProperSubmapOfBy :: Ord k => (a -> b -> Bool) -> Map k a -> Map k b -> Bool
isProperSubmapOfBy f t1 t2
  = (size t1 < size t2) && (submap' f t1 t2)

{--------------------------------------------------------------------
  Filter and partition
--------------------------------------------------------------------}
-- | /O(n)/. Filter all values that satisfy the predicate.
filter :: Ord k => (a -> Bool) -> Map k a -> Map k a
filter p m
  = filterWithKey (\k x -> p x) m

-- | /O(n)/. Filter all keys\/values that satisfy the predicate.
filterWithKey :: Ord k => (k -> a -> Bool) -> Map k a -> Map k a
filterWithKey p Tip = Tip
filterWithKey p (Bin _ kx x l r)
  | p kx x    = join kx x (filterWithKey p l) (filterWithKey p r)
  | otherwise = merge (filterWithKey p l) (filterWithKey p r)


-- | /O(n)/. partition the map according to a predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
partition :: Ord k => (a -> Bool) -> Map k a -> (Map k a,Map k a)
partition p m
  = partitionWithKey (\k x -> p x) m

-- | /O(n)/. partition the map according to a predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
partitionWithKey :: Ord k => (k -> a -> Bool) -> Map k a -> (Map k a,Map k a)
partitionWithKey p Tip = (Tip,Tip)
partitionWithKey p (Bin _ kx x l r)
  | p kx x    = (join kx x l1 r1,merge l2 r2)
  | otherwise = (merge l1 r1,join kx x l2 r2)
  where
    (l1,l2) = partitionWithKey p l
    (r1,r2) = partitionWithKey p r


{--------------------------------------------------------------------
  Mapping
--------------------------------------------------------------------}
-- | /O(n)/. Map a function over all values in the map.
map :: (a -> b) -> Map k a -> Map k b
map f m
  = mapWithKey (\k x -> f x) m

-- | /O(n)/. Map a function over all values in the map.
mapWithKey :: (k -> a -> b) -> Map k a -> Map k b
mapWithKey f Tip = Tip
mapWithKey f (Bin sx kx x l r) 
  = Bin sx kx (f kx x) (mapWithKey f l) (mapWithKey f r)

-- | /O(n)/. The function 'mapAccum' threads an accumulating
-- argument through the map in ascending order of keys.
mapAccum :: (a -> b -> (a,c)) -> a -> Map k b -> (a,Map k c)
mapAccum f a m
  = mapAccumWithKey (\a k x -> f a x) a m

-- | /O(n)/. The function 'mapAccumWithKey' threads an accumulating
-- argument through the map in ascending order of keys.
mapAccumWithKey :: (a -> k -> b -> (a,c)) -> a -> Map k b -> (a,Map k c)
mapAccumWithKey f a t
  = mapAccumL f a t

-- | /O(n)/. The function 'mapAccumL' threads an accumulating
-- argument throught the map in ascending order of keys.
mapAccumL :: (a -> k -> b -> (a,c)) -> a -> Map k b -> (a,Map k c)
mapAccumL f a t
  = case t of
      Tip -> (a,Tip)
      Bin sx kx x l r
          -> let (a1,l') = mapAccumL f a l
                 (a2,x') = f a1 kx x
                 (a3,r') = mapAccumL f a2 r
             in (a3,Bin sx kx x' l' r')

-- | /O(n)/. The function 'mapAccumR' threads an accumulating
-- argument throught the map in descending order of keys.
mapAccumR :: (a -> k -> b -> (a,c)) -> a -> Map k b -> (a,Map k c)
mapAccumR f a t
  = case t of
      Tip -> (a,Tip)
      Bin sx kx x l r 
          -> let (a1,r') = mapAccumR f a r
                 (a2,x') = f a1 kx x
                 (a3,l') = mapAccumR f a2 l
             in (a3,Bin sx kx x' l' r')

-- | /O(n*log n)/. 
-- @'mapKeys' f s@ is the map obtained by applying @f@ to each key of @s@.
-- 
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the value at the smallest of
-- these keys is retained.

mapKeys :: Ord k2 => (k1->k2) -> Map k1 a -> Map k2 a
mapKeys = mapKeysWith (\x y->x)

-- | /O(n*log n)/. 
-- @'mapKeysWith' c f s@ is the map obtained by applying @f@ to each key of @s@.
-- 
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the associated values will be
-- combined using @c@.

mapKeysWith :: Ord k2 => (a -> a -> a) -> (k1->k2) -> Map k1 a -> Map k2 a
mapKeysWith c f = fromListWith c . List.map fFirst . toList
    where fFirst (x,y) = (f x, y)


-- | /O(n)/.
-- @'mapKeysMonotonic' f s == 'mapKeys' f s@, but works only when @f@
-- is strictly monotonic.
-- /The precondition is not checked./
-- Semi-formally, we have:
-- 
-- > and [x < y ==> f x < f y | x <- ls, y <- ls] 
-- >                     ==> mapKeysMonotonic f s == mapKeys f s
-- >     where ls = keys s

mapKeysMonotonic :: (k1->k2) -> Map k1 a -> Map k2 a
mapKeysMonotonic f Tip = Tip
mapKeysMonotonic f (Bin sz k x l r) =
    Bin sz (f k) x (mapKeysMonotonic f l) (mapKeysMonotonic f r)

{--------------------------------------------------------------------
  Folds  
--------------------------------------------------------------------}

-- | /O(n)/. Fold the values in the map, such that
-- @'fold' f z == 'Prelude.foldr' f z . 'elems'@.
-- For example,
--
-- > elems map = fold (:) [] map
--
fold :: (a -> b -> b) -> b -> Map k a -> b
fold f z m
  = foldWithKey (\k x z -> f x z) z m

-- | /O(n)/. Fold the keys and values in the map, such that
-- @'foldWithKey' f z == 'Prelude.foldr' ('uncurry' f) z . 'toAscList'@.
-- For example,
--
-- > keys map = foldWithKey (\k x ks -> k:ks) [] map
--
foldWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
foldWithKey f z t
  = foldr f z t

-- | /O(n)/. In-order fold.
foldi :: (k -> a -> b -> b -> b) -> b -> Map k a -> b 
foldi f z Tip               = z
foldi f z (Bin _ kx x l r)  = f kx x (foldi f z l) (foldi f z r)

-- | /O(n)/. Post-order fold.
foldr :: (k -> a -> b -> b) -> b -> Map k a -> b
foldr f z Tip              = z
foldr f z (Bin _ kx x l r) = foldr f (f kx x (foldr f z r)) l

-- | /O(n)/. Pre-order fold.
foldl :: (b -> k -> a -> b) -> b -> Map k a -> b
foldl f z Tip              = z
foldl f z (Bin _ kx x l r) = foldl f (f (foldl f z l) kx x) r

{--------------------------------------------------------------------
  List variations 
--------------------------------------------------------------------}
-- | /O(n)/.
-- Return all elements of the map in the ascending order of their keys.
elems :: Map k a -> [a]
elems m
  = [x | (k,x) <- assocs m]

-- | /O(n)/. Return all keys of the map in ascending order.
keys  :: Map k a -> [k]
keys m
  = [k | (k,x) <- assocs m]

-- | /O(n)/. The set of all keys of the map.
keysSet :: Map k a -> Set.Set k
keysSet m = Set.fromDistinctAscList (keys m)

-- | /O(n)/. Return all key\/value pairs in the map in ascending key order.
assocs :: Map k a -> [(k,a)]
assocs m
  = toList m

{--------------------------------------------------------------------
  Lists 
  use [foldlStrict] to reduce demand on the control-stack
--------------------------------------------------------------------}
-- | /O(n*log n)/. Build a map from a list of key\/value pairs. See also 'fromAscList'.
fromList :: Ord k => [(k,a)] -> Map k a 
fromList xs       
  = foldlStrict ins empty xs
  where
    ins t (k,x) = insert k x t

-- | /O(n*log n)/. Build a map from a list of key\/value pairs with a combining function. See also 'fromAscListWith'.
fromListWith :: Ord k => (a -> a -> a) -> [(k,a)] -> Map k a 
fromListWith f xs
  = fromListWithKey (\k x y -> f x y) xs

-- | /O(n*log n)/. Build a map from a list of key\/value pairs with a combining function. See also 'fromAscListWithKey'.
fromListWithKey :: Ord k => (k -> a -> a -> a) -> [(k,a)] -> Map k a 
fromListWithKey f xs 
  = foldlStrict ins empty xs
  where
    ins t (k,x) = insertWithKey f k x t

-- | /O(n)/. Convert to a list of key\/value pairs.
toList :: Map k a -> [(k,a)]
toList t      = toAscList t

-- | /O(n)/. Convert to an ascending list.
toAscList :: Map k a -> [(k,a)]
toAscList t   = foldr (\k x xs -> (k,x):xs) [] t

-- | /O(n)/. 
toDescList :: Map k a -> [(k,a)]
toDescList t  = foldl (\xs k x -> (k,x):xs) [] t


{--------------------------------------------------------------------
  Building trees from ascending/descending lists can be done in linear time.
  
  Note that if [xs] is ascending that: 
    fromAscList xs       == fromList xs
    fromAscListWith f xs == fromListWith f xs
--------------------------------------------------------------------}
-- | /O(n)/. Build a map from an ascending list in linear time.
-- /The precondition (input list is ascending) is not checked./
fromAscList :: Eq k => [(k,a)] -> Map k a 
fromAscList xs
  = fromAscListWithKey (\k x y -> x) xs

-- | /O(n)/. Build a map from an ascending list in linear time with a combining function for equal keys.
-- /The precondition (input list is ascending) is not checked./
fromAscListWith :: Eq k => (a -> a -> a) -> [(k,a)] -> Map k a 
fromAscListWith f xs
  = fromAscListWithKey (\k x y -> f x y) xs

-- | /O(n)/. Build a map from an ascending list in linear time with a
-- combining function for equal keys.
-- /The precondition (input list is ascending) is not checked./
fromAscListWithKey :: Eq k => (k -> a -> a -> a) -> [(k,a)] -> Map k a 
fromAscListWithKey f xs
  = fromDistinctAscList (combineEq f xs)
  where
  -- [combineEq f xs] combines equal elements with function [f] in an ordered list [xs]
  combineEq f xs
    = case xs of
        []     -> []
        [x]    -> [x]
        (x:xx) -> combineEq' x xx

  combineEq' z [] = [z]
  combineEq' z@(kz,zz) (x@(kx,xx):xs)
    | kx==kz    = let yy = f kx xx zz in combineEq' (kx,yy) xs
    | otherwise = z:combineEq' x xs


-- | /O(n)/. Build a map from an ascending list of distinct elements in linear time.
-- /The precondition is not checked./
fromDistinctAscList :: [(k,a)] -> Map k a 
fromDistinctAscList xs
  = build const (length xs) xs
  where
    -- 1) use continutations so that we use heap space instead of stack space.
    -- 2) special case for n==5 to build bushier trees. 
    build c 0 xs   = c Tip xs 
    build c 5 xs   = case xs of
                       ((k1,x1):(k2,x2):(k3,x3):(k4,x4):(k5,x5):xx) 
                            -> c (bin k4 x4 (bin k2 x2 (singleton k1 x1) (singleton k3 x3)) (singleton k5 x5)) xx
    build c n xs   = seq nr $ build (buildR nr c) nl xs
                   where
                     nl = n `div` 2
                     nr = n - nl - 1

    buildR n c l ((k,x):ys) = build (buildB l k x c) n ys
    buildB l k x c r zs     = c (bin k x l r) zs
                      


{--------------------------------------------------------------------
  Utility functions that return sub-ranges of the original
  tree. Some functions take a comparison function as argument to
  allow comparisons against infinite values. A function [cmplo k]
  should be read as [compare lo k].

  [trim cmplo cmphi t]  A tree that is either empty or where [cmplo k == LT]
                        and [cmphi k == GT] for the key [k] of the root.
  [filterGt cmp t]      A tree where for all keys [k]. [cmp k == LT]
  [filterLt cmp t]      A tree where for all keys [k]. [cmp k == GT]

  [split k t]           Returns two trees [l] and [r] where all keys
                        in [l] are <[k] and all keys in [r] are >[k].
  [splitLookup k t]     Just like [split] but also returns whether [k]
                        was found in the tree.
--------------------------------------------------------------------}

{--------------------------------------------------------------------
  [trim lo hi t] trims away all subtrees that surely contain no
  values between the range [lo] to [hi]. The returned tree is either
  empty or the key of the root is between @lo@ and @hi@.
--------------------------------------------------------------------}
trim :: (k -> Ordering) -> (k -> Ordering) -> Map k a -> Map k a
trim cmplo cmphi Tip = Tip
trim cmplo cmphi t@(Bin sx kx x l r)
  = case cmplo kx of
      LT -> case cmphi kx of
              GT -> t
              le -> trim cmplo cmphi l
      ge -> trim cmplo cmphi r
              
trimLookupLo :: Ord k => k -> (k -> Ordering) -> Map k a -> (Maybe a, Map k a)
trimLookupLo lo cmphi Tip = (Nothing,Tip)
trimLookupLo lo cmphi t@(Bin sx kx x l r)
  = case compare lo kx of
      LT -> case cmphi kx of
              GT -> (lookup lo t, t)
              le -> trimLookupLo lo cmphi l
      GT -> trimLookupLo lo cmphi r
      EQ -> (Just x,trim (compare lo) cmphi r)


{--------------------------------------------------------------------
  [filterGt k t] filter all keys >[k] from tree [t]
  [filterLt k t] filter all keys <[k] from tree [t]
--------------------------------------------------------------------}
filterGt :: Ord k => (k -> Ordering) -> Map k a -> Map k a
filterGt cmp Tip = Tip
filterGt cmp (Bin sx kx x l r)
  = case cmp kx of
      LT -> join kx x (filterGt cmp l) r
      GT -> filterGt cmp r
      EQ -> r
      
filterLt :: Ord k => (k -> Ordering) -> Map k a -> Map k a
filterLt cmp Tip = Tip
filterLt cmp (Bin sx kx x l r)
  = case cmp kx of
      LT -> filterLt cmp l
      GT -> join kx x l (filterLt cmp r)
      EQ -> l

{--------------------------------------------------------------------
  Split
--------------------------------------------------------------------}
-- | /O(log n)/. The expression (@'split' k map@) is a pair @(map1,map2)@ where
-- the keys in @map1@ are smaller than @k@ and the keys in @map2@ larger than @k@. Any key equal to @k@ is found in neither @map1@ nor @map2@.
split :: Ord k => k -> Map k a -> (Map k a,Map k a)
split k Tip = (Tip,Tip)
split k (Bin sx kx x l r)
  = case compare k kx of
      LT -> let (lt,gt) = split k l in (lt,join kx x gt r)
      GT -> let (lt,gt) = split k r in (join kx x l lt,gt)
      EQ -> (l,r)

-- | /O(log n)/. The expression (@'splitLookup' k map@) splits a map just
-- like 'split' but also returns @'lookup' k map@.
splitLookup :: Ord k => k -> Map k a -> (Map k a,Maybe a,Map k a)
splitLookup k Tip = (Tip,Nothing,Tip)
splitLookup k (Bin sx kx x l r)
  = case compare k kx of
      LT -> let (lt,z,gt) = splitLookup k l in (lt,z,join kx x gt r)
      GT -> let (lt,z,gt) = splitLookup k r in (join kx x l lt,z,gt)
      EQ -> (l,Just x,r)

{--------------------------------------------------------------------
  Utility functions that maintain the balance properties of the tree.
  All constructors assume that all values in [l] < [k] and all values
  in [r] > [k], and that [l] and [r] are valid trees.
  
  In order of sophistication:
    [Bin sz k x l r]  The type constructor.
    [bin k x l r]     Maintains the correct size, assumes that both [l]
                      and [r] are balanced with respect to each other.
    [balance k x l r] Restores the balance and size.
                      Assumes that the original tree was balanced and
                      that [l] or [r] has changed by at most one element.
    [join k x l r]    Restores balance and size. 

  Furthermore, we can construct a new tree from two trees. Both operations
  assume that all values in [l] < all values in [r] and that [l] and [r]
  are valid:
    [glue l r]        Glues [l] and [r] together. Assumes that [l] and
                      [r] are already balanced with respect to each other.
    [merge l r]       Merges two trees and restores balance.

  Note: in contrast to Adam's paper, we use (<=) comparisons instead
  of (<) comparisons in [join], [merge] and [balance]. 
  Quickcheck (on [difference]) showed that this was necessary in order 
  to maintain the invariants. It is quite unsatisfactory that I haven't 
  been able to find out why this is actually the case! Fortunately, it 
  doesn't hurt to be a bit more conservative.
--------------------------------------------------------------------}

{--------------------------------------------------------------------
  Join 
--------------------------------------------------------------------}
join :: Ord k => k -> a -> Map k a -> Map k a -> Map k a
join kx x Tip r  = insertMin kx x r
join kx x l Tip  = insertMax kx x l
join kx x l@(Bin sizeL ky y ly ry) r@(Bin sizeR kz z lz rz)
  | delta*sizeL <= sizeR  = balance kz z (join kx x l lz) rz
  | delta*sizeR <= sizeL  = balance ky y ly (join kx x ry r)
  | otherwise             = bin kx x l r


-- insertMin and insertMax don't perform potentially expensive comparisons.
insertMax,insertMin :: k -> a -> Map k a -> Map k a 
insertMax kx x t
  = case t of
      Tip -> singleton kx x
      Bin sz ky y l r
          -> balance ky y l (insertMax kx x r)
             
insertMin kx x t
  = case t of
      Tip -> singleton kx x
      Bin sz ky y l r
          -> balance ky y (insertMin kx x l) r
             
{--------------------------------------------------------------------
  [merge l r]: merges two trees.
--------------------------------------------------------------------}
merge :: Map k a -> Map k a -> Map k a
merge Tip r   = r
merge l Tip   = l
merge l@(Bin sizeL kx x lx rx) r@(Bin sizeR ky y ly ry)
  | delta*sizeL <= sizeR = balance ky y (merge l ly) ry
  | delta*sizeR <= sizeL = balance kx x lx (merge rx r)
  | otherwise            = glue l r

{--------------------------------------------------------------------
  [glue l r]: glues two trees together.
  Assumes that [l] and [r] are already balanced with respect to each other.
--------------------------------------------------------------------}
glue :: Map k a -> Map k a -> Map k a
glue Tip r = r
glue l Tip = l
glue l r   
  | size l > size r = let ((km,m),l') = deleteFindMax l in balance km m l' r
  | otherwise       = let ((km,m),r') = deleteFindMin r in balance km m l r'


-- | /O(log n)/. Delete and find the minimal element.
deleteFindMin :: Map k a -> ((k,a),Map k a)
deleteFindMin t 
  = case t of
      Bin _ k x Tip r -> ((k,x),r)
      Bin _ k x l r   -> let (km,l') = deleteFindMin l in (km,balance k x l' r)
      Tip             -> (error "Map.deleteFindMin: can not return the minimal element of an empty map", Tip)

-- | /O(log n)/. Delete and find the maximal element.
deleteFindMax :: Map k a -> ((k,a),Map k a)
deleteFindMax t
  = case t of
      Bin _ k x l Tip -> ((k,x),l)
      Bin _ k x l r   -> let (km,r') = deleteFindMax r in (km,balance k x l r')
      Tip             -> (error "Map.deleteFindMax: can not return the maximal element of an empty map", Tip)


{--------------------------------------------------------------------
  [balance l x r] balances two trees with value x.
  The sizes of the trees should balance after decreasing the
  size of one of them. (a rotation).

  [delta] is the maximal relative difference between the sizes of
          two trees, it corresponds with the [w] in Adams' paper.
  [ratio] is the ratio between an outer and inner sibling of the
          heavier subtree in an unbalanced setting. It determines
          whether a double or single rotation should be performed
          to restore balance. It is correspondes with the inverse
          of $\alpha$ in Adam's article.

  Note that:
  - [delta] should be larger than 4.646 with a [ratio] of 2.
  - [delta] should be larger than 3.745 with a [ratio] of 1.534.
  
  - A lower [delta] leads to a more 'perfectly' balanced tree.
  - A higher [delta] performs less rebalancing.

  - Balancing is automatic for random data and a balancing
    scheme is only necessary to avoid pathological worst cases.
    Almost any choice will do, and in practice, a rather large
    [delta] may perform better than smaller one.

  Note: in contrast to Adam's paper, we use a ratio of (at least) [2]
  to decide whether a single or double rotation is needed. Allthough
  he actually proves that this ratio is needed to maintain the
  invariants, his implementation uses an invalid ratio of [1].
--------------------------------------------------------------------}
delta,ratio :: Int
delta = 5
ratio = 2

balance :: k -> a -> Map k a -> Map k a -> Map k a
balance k x l r
  | sizeL + sizeR <= 1    = Bin sizeX k x l r
  | sizeR >= delta*sizeL  = rotateL k x l r
  | sizeL >= delta*sizeR  = rotateR k x l r
  | otherwise             = Bin sizeX k x l r
  where
    sizeL = size l
    sizeR = size r
    sizeX = sizeL + sizeR + 1

-- rotate
rotateL k x l r@(Bin _ _ _ ly ry)
  | size ly < ratio*size ry = singleL k x l r
  | otherwise               = doubleL k x l r

rotateR k x l@(Bin _ _ _ ly ry) r
  | size ry < ratio*size ly = singleR k x l r
  | otherwise               = doubleR k x l r

-- basic rotations
singleL k1 x1 t1 (Bin _ k2 x2 t2 t3)  = bin k2 x2 (bin k1 x1 t1 t2) t3
singleR k1 x1 (Bin _ k2 x2 t1 t2) t3  = bin k2 x2 t1 (bin k1 x1 t2 t3)

doubleL k1 x1 t1 (Bin _ k2 x2 (Bin _ k3 x3 t2 t3) t4) = bin k3 x3 (bin k1 x1 t1 t2) (bin k2 x2 t3 t4)
doubleR k1 x1 (Bin _ k2 x2 t1 (Bin _ k3 x3 t2 t3)) t4 = bin k3 x3 (bin k2 x2 t1 t2) (bin k1 x1 t3 t4)


{--------------------------------------------------------------------
  The bin constructor maintains the size of the tree
--------------------------------------------------------------------}
bin :: k -> a -> Map k a -> Map k a -> Map k a
bin k x l r
  = Bin (size l + size r + 1) k x l r


{--------------------------------------------------------------------
  Eq converts the tree to a list. In a lazy setting, this 
  actually seems one of the faster methods to compare two trees 
  and it is certainly the simplest :-)
--------------------------------------------------------------------}
instance (Eq k,Eq a) => Eq (Map k a) where
  t1 == t2  = (size t1 == size t2) && (toAscList t1 == toAscList t2)

{--------------------------------------------------------------------
  Ord 
--------------------------------------------------------------------}

instance (Ord k, Ord v) => Ord (Map k v) where
    compare m1 m2 = compare (toAscList m1) (toAscList m2)

{--------------------------------------------------------------------
  Functor
--------------------------------------------------------------------}
instance Functor (Map k) where
  fmap f m  = map f m

{--------------------------------------------------------------------
  Read
--------------------------------------------------------------------}
instance (Ord k, Read k, Read e) => Read (Map k e) where
#ifdef __GLASGOW_HASKELL__
  readPrec = parens $ prec 10 $ do
    Ident "fromList" <- lexP
    xs <- readPrec
    return (fromList xs)

  readListPrec = readListPrecDefault
#else
  readsPrec p = readParen (p > 10) $ \ r -> do
    ("fromList",s) <- lex r
    (xs,t) <- reads s
    return (fromList xs,t)
#endif

-- parses a pair of things with the syntax a:=b
readPair :: (Read a, Read b) => ReadS (a,b)
readPair s = do (a, ct1)    <- reads s
                (":=", ct2) <- lex ct1
                (b, ct3)    <- reads ct2
                return ((a,b), ct3)

{--------------------------------------------------------------------
  Show
--------------------------------------------------------------------}
instance (Show k, Show a) => Show (Map k a) where
  showsPrec d m  = showParen (d > 10) $
    showString "fromList " . shows (toList m)

showMap :: (Show k,Show a) => [(k,a)] -> ShowS
showMap []     
  = showString "{}" 
showMap (x:xs) 
  = showChar '{' . showElem x . showTail xs
  where
    showTail []     = showChar '}'
    showTail (x:xs) = showString ", " . showElem x . showTail xs
    
    showElem (k,x)  = shows k . showString " := " . shows x
  

-- | /O(n)/. Show the tree that implements the map. The tree is shown
-- in a compressed, hanging format.
showTree :: (Show k,Show a) => Map k a -> String
showTree m
  = showTreeWith showElem True False m
  where
    showElem k x  = show k ++ ":=" ++ show x


{- | /O(n)/. The expression (@'showTreeWith' showelem hang wide map@) shows
 the tree that implements the map. Elements are shown using the @showElem@ function. If @hang@ is
 'True', a /hanging/ tree is shown otherwise a rotated tree is shown. If
 @wide@ is 'True', an extra wide version is shown.

>  Map> let t = fromDistinctAscList [(x,()) | x <- [1..5]]
>  Map> putStrLn $ showTreeWith (\k x -> show (k,x)) True False t
>  (4,())
>  +--(2,())
>  |  +--(1,())
>  |  +--(3,())
>  +--(5,())
>
>  Map> putStrLn $ showTreeWith (\k x -> show (k,x)) True True t
>  (4,())
>  |
>  +--(2,())
>  |  |
>  |  +--(1,())
>  |  |
>  |  +--(3,())
>  |
>  +--(5,())
>
>  Map> putStrLn $ showTreeWith (\k x -> show (k,x)) False True t
>  +--(5,())
>  |
>  (4,())
>  |
>  |  +--(3,())
>  |  |
>  +--(2,())
>     |
>     +--(1,())

-}
showTreeWith :: (k -> a -> String) -> Bool -> Bool -> Map k a -> String
showTreeWith showelem hang wide t
  | hang      = (showsTreeHang showelem wide [] t) ""
  | otherwise = (showsTree showelem wide [] [] t) ""

showsTree :: (k -> a -> String) -> Bool -> [String] -> [String] -> Map k a -> ShowS
showsTree showelem wide lbars rbars t
  = case t of
      Tip -> showsBars lbars . showString "|\n"
      Bin sz kx x Tip Tip
          -> showsBars lbars . showString (showelem kx x) . showString "\n" 
      Bin sz kx x l r
          -> showsTree showelem wide (withBar rbars) (withEmpty rbars) r .
             showWide wide rbars .
             showsBars lbars . showString (showelem kx x) . showString "\n" .
             showWide wide lbars .
             showsTree showelem wide (withEmpty lbars) (withBar lbars) l

showsTreeHang :: (k -> a -> String) -> Bool -> [String] -> Map k a -> ShowS
showsTreeHang showelem wide bars t
  = case t of
      Tip -> showsBars bars . showString "|\n" 
      Bin sz kx x Tip Tip
          -> showsBars bars . showString (showelem kx x) . showString "\n" 
      Bin sz kx x l r
          -> showsBars bars . showString (showelem kx x) . showString "\n" . 
             showWide wide bars .
             showsTreeHang showelem wide (withBar bars) l .
             showWide wide bars .
             showsTreeHang showelem wide (withEmpty bars) r


showWide wide bars 
  | wide      = showString (concat (reverse bars)) . showString "|\n" 
  | otherwise = id

showsBars :: [String] -> ShowS
showsBars bars
  = case bars of
      [] -> id
      _  -> showString (concat (reverse (tail bars))) . showString node

node           = "+--"
withBar bars   = "|  ":bars
withEmpty bars = "   ":bars

{--------------------------------------------------------------------
  Typeable
--------------------------------------------------------------------}

#include "Typeable.h"
INSTANCE_TYPEABLE2(Map,mapTc,"Map")

{--------------------------------------------------------------------
  Assertions
--------------------------------------------------------------------}
-- | /O(n)/. Test if the internal map structure is valid.
valid :: Ord k => Map k a -> Bool
valid t
  = balanced t && ordered t && validsize t

ordered t
  = bounded (const True) (const True) t
  where
    bounded lo hi t
      = case t of
          Tip              -> True
          Bin sz kx x l r  -> (lo kx) && (hi kx) && bounded lo (<kx) l && bounded (>kx) hi r

-- | Exported only for "Debug.QuickCheck"
balanced :: Map k a -> Bool
balanced t
  = case t of
      Tip              -> True
      Bin sz kx x l r  -> (size l + size r <= 1 || (size l <= delta*size r && size r <= delta*size l)) &&
                          balanced l && balanced r


validsize t
  = (realsize t == Just (size t))
  where
    realsize t
      = case t of
          Tip             -> Just 0
          Bin sz kx x l r -> case (realsize l,realsize r) of
                              (Just n,Just m)  | n+m+1 == sz  -> Just sz
                              other            -> Nothing

{--------------------------------------------------------------------
  Utilities
--------------------------------------------------------------------}
foldlStrict f z xs
  = case xs of
      []     -> z
      (x:xx) -> let z' = f z x in seq z' (foldlStrict f z' xx)


{-
{--------------------------------------------------------------------
  Testing
--------------------------------------------------------------------}
testTree xs   = fromList [(x,"*") | x <- xs]
test1 = testTree [1..20]
test2 = testTree [30,29..10]
test3 = testTree [1,4,6,89,2323,53,43,234,5,79,12,9,24,9,8,423,8,42,4,8,9,3]

{--------------------------------------------------------------------
  QuickCheck
--------------------------------------------------------------------}
qcheck prop
  = check config prop
  where
    config = Config
      { configMaxTest = 500
      , configMaxFail = 5000
      , configSize    = \n -> (div n 2 + 3)
      , configEvery   = \n args -> let s = show n in s ++ [ '\b' | _ <- s ]
      }


{--------------------------------------------------------------------
  Arbitrary, reasonably balanced trees
--------------------------------------------------------------------}
instance (Enum k,Arbitrary a) => Arbitrary (Map k a) where
  arbitrary = sized (arbtree 0 maxkey)
            where maxkey  = 10000

arbtree :: (Enum k,Arbitrary a) => Int -> Int -> Int -> Gen (Map k a)
arbtree lo hi n
  | n <= 0        = return Tip
  | lo >= hi      = return Tip
  | otherwise     = do{ x  <- arbitrary 
                      ; i  <- choose (lo,hi)
                      ; m  <- choose (1,30)
                      ; let (ml,mr)  | m==(1::Int)= (1,2)
                                     | m==2       = (2,1)
                                     | m==3       = (1,1)
                                     | otherwise  = (2,2)
                      ; l  <- arbtree lo (i-1) (n `div` ml)
                      ; r  <- arbtree (i+1) hi (n `div` mr)
                      ; return (bin (toEnum i) x l r)
                      }  


{--------------------------------------------------------------------
  Valid tree's
--------------------------------------------------------------------}
forValid :: (Show k,Enum k,Show a,Arbitrary a,Testable b) => (Map k a -> b) -> Property
forValid f
  = forAll arbitrary $ \t -> 
--    classify (balanced t) "balanced" $
    classify (size t == 0) "empty" $
    classify (size t > 0  && size t <= 10) "small" $
    classify (size t > 10 && size t <= 64) "medium" $
    classify (size t > 64) "large" $
    balanced t ==> f t

forValidIntTree :: Testable a => (Map Int Int -> a) -> Property
forValidIntTree f
  = forValid f

forValidUnitTree :: Testable a => (Map Int () -> a) -> Property
forValidUnitTree f
  = forValid f


prop_Valid 
  = forValidUnitTree $ \t -> valid t

{--------------------------------------------------------------------
  Single, Insert, Delete
--------------------------------------------------------------------}
prop_Single :: Int -> Int -> Bool
prop_Single k x
  = (insert k x empty == singleton k x)

prop_InsertValid :: Int -> Property
prop_InsertValid k
  = forValidUnitTree $ \t -> valid (insert k () t)

prop_InsertDelete :: Int -> Map Int () -> Property
prop_InsertDelete k t
  = (lookup k t == Nothing) ==> delete k (insert k () t) == t

prop_DeleteValid :: Int -> Property
prop_DeleteValid k
  = forValidUnitTree $ \t -> 
    valid (delete k (insert k () t))

{--------------------------------------------------------------------
  Balance
--------------------------------------------------------------------}
prop_Join :: Int -> Property 
prop_Join k 
  = forValidUnitTree $ \t ->
    let (l,r) = split k t
    in valid (join k () l r)

prop_Merge :: Int -> Property 
prop_Merge k
  = forValidUnitTree $ \t ->
    let (l,r) = split k t
    in valid (merge l r)


{--------------------------------------------------------------------
  Union
--------------------------------------------------------------------}
prop_UnionValid :: Property
prop_UnionValid
  = forValidUnitTree $ \t1 ->
    forValidUnitTree $ \t2 ->
    valid (union t1 t2)

prop_UnionInsert :: Int -> Int -> Map Int Int -> Bool
prop_UnionInsert k x t
  = union (singleton k x) t == insert k x t

prop_UnionAssoc :: Map Int Int -> Map Int Int -> Map Int Int -> Bool
prop_UnionAssoc t1 t2 t3
  = union t1 (union t2 t3) == union (union t1 t2) t3

prop_UnionComm :: Map Int Int -> Map Int Int -> Bool
prop_UnionComm t1 t2
  = (union t1 t2 == unionWith (\x y -> y) t2 t1)

prop_UnionWithValid 
  = forValidIntTree $ \t1 ->
    forValidIntTree $ \t2 ->
    valid (unionWithKey (\k x y -> x+y) t1 t2)

prop_UnionWith :: [(Int,Int)] -> [(Int,Int)] -> Bool
prop_UnionWith xs ys
  = sum (elems (unionWith (+) (fromListWith (+) xs) (fromListWith (+) ys))) 
    == (sum (Prelude.map snd xs) + sum (Prelude.map snd ys))

prop_DiffValid
  = forValidUnitTree $ \t1 ->
    forValidUnitTree $ \t2 ->
    valid (difference t1 t2)

prop_Diff :: [(Int,Int)] -> [(Int,Int)] -> Bool
prop_Diff xs ys
  =  List.sort (keys (difference (fromListWith (+) xs) (fromListWith (+) ys))) 
    == List.sort ((List.\\) (nub (Prelude.map fst xs))  (nub (Prelude.map fst ys)))

prop_IntValid
  = forValidUnitTree $ \t1 ->
    forValidUnitTree $ \t2 ->
    valid (intersection t1 t2)

prop_Int :: [(Int,Int)] -> [(Int,Int)] -> Bool
prop_Int xs ys
  =  List.sort (keys (intersection (fromListWith (+) xs) (fromListWith (+) ys))) 
    == List.sort (nub ((List.intersect) (Prelude.map fst xs)  (Prelude.map fst ys)))

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}
prop_Ordered
  = forAll (choose (5,100)) $ \n ->
    let xs = [(x,()) | x <- [0..n::Int]] 
    in fromAscList xs == fromList xs

prop_List :: [Int] -> Bool
prop_List xs
  = (sort (nub xs) == [x | (x,()) <- toList (fromList [(x,()) | x <- xs])])
-}

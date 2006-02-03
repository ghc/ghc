{-# OPTIONS -cpp -fglasgow-exts -fno-bang-patterns #-} 
-----------------------------------------------------------------------------
-- Module      :  Data.IntMap
-- Copyright   :  (c) Daan Leijen 2002
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- An efficient implementation of maps from integer keys to values.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.  eg.
--
-- >  import Data.IntMap as Map
--
-- The implementation is based on /big-endian patricia trees/.  This data
-- structure performs especially well on binary operations like 'union'
-- and 'intersection'.  However, my benchmarks show that it is also
-- (much) faster on insertions and deletions when compared to a generic
-- size-balanced map implementation (see "Data.Map" and "Data.FiniteMap").
--
--    * Chris Okasaki and Andy Gill,  \"/Fast Mergeable Integer Maps/\",
--	Workshop on ML, September 1998, pages 77-86,
--	<http://www.cse.ogi.edu/~andy/pub/finite.htm>
--
--    * D.R. Morrison, \"/PATRICIA -- Practical Algorithm To Retrieve
--	Information Coded In Alphanumeric/\", Journal of the ACM, 15(4),
--	October 1968, pages 514-534.
--
-- Many operations have a worst-case complexity of /O(min(n,W))/.
-- This means that the operation can become linear in the number of
-- elements with a maximum of /W/ -- the number of bits in an 'Int'
-- (32 or 64).
-----------------------------------------------------------------------------

module Data.IntMap  ( 
            -- * Map type
              IntMap, Key          -- instance Eq,Show

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
            
            -- * Debugging
            , showTree
            , showTreeWith
            ) where


import Prelude hiding (lookup,map,filter,foldr,foldl,null)
import Data.Bits 
import Data.Int
import qualified Data.IntSet as IntSet
import Data.Monoid (Monoid(..))
import Data.Typeable
import Data.Foldable (Foldable(foldMap))

{-
-- just for testing
import qualified Prelude
import Debug.QuickCheck 
import List (nub,sort)
import qualified List
-}  

#if __GLASGOW_HASKELL__
import Text.Read
import Data.Generics.Basics
import Data.Generics.Instances
#endif

#if __GLASGOW_HASKELL__ >= 503
import GHC.Word
import GHC.Exts ( Word(..), Int(..), shiftRL# )
#elif __GLASGOW_HASKELL__
import Word
import GlaExts ( Word(..), Int(..), shiftRL# )
#else
import Data.Word
#endif

infixl 9 \\{-This comment teaches CPP correct behaviour -}

-- A "Nat" is a natural machine word (an unsigned Int)
type Nat = Word

natFromInt :: Key -> Nat
natFromInt i = fromIntegral i

intFromNat :: Nat -> Key
intFromNat w = fromIntegral w

shiftRL :: Nat -> Key -> Nat
#if __GLASGOW_HASKELL__
{--------------------------------------------------------------------
  GHC: use unboxing to get @shiftRL@ inlined.
--------------------------------------------------------------------}
shiftRL (W# x) (I# i)
  = W# (shiftRL# x i)
#else
shiftRL x i   = shiftR x i
#endif

{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}

-- | /O(min(n,W))/. Find the value at a key.
-- Calls 'error' when the element can not be found.

(!) :: IntMap a -> Key -> a
m ! k    = find' k m

-- | /O(n+m)/. See 'difference'.
(\\) :: IntMap a -> IntMap b -> IntMap a
m1 \\ m2 = difference m1 m2

{--------------------------------------------------------------------
  Types  
--------------------------------------------------------------------}
-- | A map of integers to values @a@.
data IntMap a = Nil
              | Tip {-# UNPACK #-} !Key a
              | Bin {-# UNPACK #-} !Prefix {-# UNPACK #-} !Mask !(IntMap a) !(IntMap a) 

type Prefix = Int
type Mask   = Int
type Key    = Int

instance Monoid (IntMap a) where
    mempty  = empty
    mappend = union
    mconcat = unions

instance Foldable IntMap where
    foldMap f Nil = mempty
    foldMap f (Tip _k v) = f v
    foldMap f (Bin _ _ l r) = foldMap f l `mappend` foldMap f r

#if __GLASGOW_HASKELL__

{--------------------------------------------------------------------
  A Data instance  
--------------------------------------------------------------------}

-- This instance preserves data abstraction at the cost of inefficiency.
-- We omit reflection services for the sake of data abstraction.

instance Data a => Data (IntMap a) where
  gfoldl f z im = z fromList `f` (toList im)
  toConstr _    = error "toConstr"
  gunfold _ _   = error "gunfold"
  dataTypeOf _  = mkNorepType "Data.IntMap.IntMap"
  dataCast1 f   = gcast1 f

#endif

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}
-- | /O(1)/. Is the map empty?
null :: IntMap a -> Bool
null Nil   = True
null other = False

-- | /O(n)/. Number of elements in the map.
size :: IntMap a -> Int
size t
  = case t of
      Bin p m l r -> size l + size r
      Tip k x -> 1
      Nil     -> 0

-- | /O(min(n,W))/. Is the key a member of the map?
member :: Key -> IntMap a -> Bool
member k m
  = case lookup k m of
      Nothing -> False
      Just x  -> True
    
-- | /O(min(n,W))/. Lookup the value at a key in the map.
lookup :: Key -> IntMap a -> Maybe a
lookup k t
  = let nk = natFromInt k  in seq nk (lookupN nk t)

lookupN :: Nat -> IntMap a -> Maybe a
lookupN k t
  = case t of
      Bin p m l r 
        | zeroN k (natFromInt m) -> lookupN k l
        | otherwise              -> lookupN k r
      Tip kx x 
        | (k == natFromInt kx)  -> Just x
        | otherwise             -> Nothing
      Nil -> Nothing

find' :: Key -> IntMap a -> a
find' k m
  = case lookup k m of
      Nothing -> error ("IntMap.find: key " ++ show k ++ " is not an element of the map")
      Just x  -> x


-- | /O(min(n,W))/. The expression @('findWithDefault' def k map)@
-- returns the value at key @k@ or returns @def@ when the key is not an
-- element of the map.
findWithDefault :: a -> Key -> IntMap a -> a
findWithDefault def k m
  = case lookup k m of
      Nothing -> def
      Just x  -> x

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
-- | /O(1)/. The empty map.
empty :: IntMap a
empty
  = Nil

-- | /O(1)/. A map of one element.
singleton :: Key -> a -> IntMap a
singleton k x
  = Tip k x

{--------------------------------------------------------------------
  Insert
--------------------------------------------------------------------}
-- | /O(min(n,W))/. Insert a new key\/value pair in the map.
-- If the key is already present in the map, the associated value is
-- replaced with the supplied value, i.e. 'insert' is equivalent to
-- @'insertWith' 'const'@.
insert :: Key -> a -> IntMap a -> IntMap a
insert k x t
  = case t of
      Bin p m l r 
        | nomatch k p m -> join k (Tip k x) p t
        | zero k m      -> Bin p m (insert k x l) r
        | otherwise     -> Bin p m l (insert k x r)
      Tip ky y 
        | k==ky         -> Tip k x
        | otherwise     -> join k (Tip k x) ky t
      Nil -> Tip k x

-- right-biased insertion, used by 'union'
-- | /O(min(n,W))/. Insert with a combining function.
-- @'insertWith' f key value mp@ 
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert @f new_value old_value@.
insertWith :: (a -> a -> a) -> Key -> a -> IntMap a -> IntMap a
insertWith f k x t
  = insertWithKey (\k x y -> f x y) k x t

-- | /O(min(n,W))/. Insert with a combining function.
-- @'insertWithKey' f key value mp@ 
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert @f key new_value old_value@.
insertWithKey :: (Key -> a -> a -> a) -> Key -> a -> IntMap a -> IntMap a
insertWithKey f k x t
  = case t of
      Bin p m l r 
        | nomatch k p m -> join k (Tip k x) p t
        | zero k m      -> Bin p m (insertWithKey f k x l) r
        | otherwise     -> Bin p m l (insertWithKey f k x r)
      Tip ky y 
        | k==ky         -> Tip k (f k x y)
        | otherwise     -> join k (Tip k x) ky t
      Nil -> Tip k x


-- | /O(min(n,W))/. The expression (@'insertLookupWithKey' f k x map@)
-- is a pair where the first element is equal to (@'lookup' k map@)
-- and the second element equal to (@'insertWithKey' f k x map@).
insertLookupWithKey :: (Key -> a -> a -> a) -> Key -> a -> IntMap a -> (Maybe a, IntMap a)
insertLookupWithKey f k x t
  = case t of
      Bin p m l r 
        | nomatch k p m -> (Nothing,join k (Tip k x) p t)
        | zero k m      -> let (found,l') = insertLookupWithKey f k x l in (found,Bin p m l' r)
        | otherwise     -> let (found,r') = insertLookupWithKey f k x r in (found,Bin p m l r')
      Tip ky y 
        | k==ky         -> (Just y,Tip k (f k x y))
        | otherwise     -> (Nothing,join k (Tip k x) ky t)
      Nil -> (Nothing,Tip k x)


{--------------------------------------------------------------------
  Deletion
  [delete] is the inlined version of [deleteWith (\k x -> Nothing)]
--------------------------------------------------------------------}
-- | /O(min(n,W))/. Delete a key and its value from the map. When the key is not
-- a member of the map, the original map is returned.
delete :: Key -> IntMap a -> IntMap a
delete k t
  = case t of
      Bin p m l r 
        | nomatch k p m -> t
        | zero k m      -> bin p m (delete k l) r
        | otherwise     -> bin p m l (delete k r)
      Tip ky y 
        | k==ky         -> Nil
        | otherwise     -> t
      Nil -> Nil

-- | /O(min(n,W))/. Adjust a value at a specific key. When the key is not
-- a member of the map, the original map is returned.
adjust ::  (a -> a) -> Key -> IntMap a -> IntMap a
adjust f k m
  = adjustWithKey (\k x -> f x) k m

-- | /O(min(n,W))/. Adjust a value at a specific key. When the key is not
-- a member of the map, the original map is returned.
adjustWithKey ::  (Key -> a -> a) -> Key -> IntMap a -> IntMap a
adjustWithKey f k m
  = updateWithKey (\k x -> Just (f k x)) k m

-- | /O(min(n,W))/. The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
update ::  (a -> Maybe a) -> Key -> IntMap a -> IntMap a
update f k m
  = updateWithKey (\k x -> f x) k m

-- | /O(min(n,W))/. The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f k x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
updateWithKey ::  (Key -> a -> Maybe a) -> Key -> IntMap a -> IntMap a
updateWithKey f k t
  = case t of
      Bin p m l r 
        | nomatch k p m -> t
        | zero k m      -> bin p m (updateWithKey f k l) r
        | otherwise     -> bin p m l (updateWithKey f k r)
      Tip ky y 
        | k==ky         -> case (f k y) of
                             Just y' -> Tip ky y'
                             Nothing -> Nil
        | otherwise     -> t
      Nil -> Nil

-- | /O(min(n,W))/. Lookup and update.
updateLookupWithKey ::  (Key -> a -> Maybe a) -> Key -> IntMap a -> (Maybe a,IntMap a)
updateLookupWithKey f k t
  = case t of
      Bin p m l r 
        | nomatch k p m -> (Nothing,t)
        | zero k m      -> let (found,l') = updateLookupWithKey f k l in (found,bin p m l' r)
        | otherwise     -> let (found,r') = updateLookupWithKey f k r in (found,bin p m l r')
      Tip ky y 
        | k==ky         -> case (f k y) of
                             Just y' -> (Just y,Tip ky y')
                             Nothing -> (Just y,Nil)
        | otherwise     -> (Nothing,t)
      Nil -> (Nothing,Nil)


{--------------------------------------------------------------------
  Union
--------------------------------------------------------------------}
-- | The union of a list of maps.
unions :: [IntMap a] -> IntMap a
unions xs
  = foldlStrict union empty xs

-- | The union of a list of maps, with a combining operation
unionsWith :: (a->a->a) -> [IntMap a] -> IntMap a
unionsWith f ts
  = foldlStrict (unionWith f) empty ts

-- | /O(n+m)/. The (left-biased) union of two maps. 
-- It prefers the first map when duplicate keys are encountered,
-- i.e. (@'union' == 'unionWith' 'const'@).
union :: IntMap a -> IntMap a -> IntMap a
union t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2  = union1
  | shorter m2 m1  = union2
  | p1 == p2       = Bin p1 m1 (union l1 l2) (union r1 r2)
  | otherwise      = join p1 t1 p2 t2
  where
    union1  | nomatch p2 p1 m1  = join p1 t1 p2 t2
            | zero p2 m1        = Bin p1 m1 (union l1 t2) r1
            | otherwise         = Bin p1 m1 l1 (union r1 t2)

    union2  | nomatch p1 p2 m2  = join p1 t1 p2 t2
            | zero p1 m2        = Bin p2 m2 (union t1 l2) r2
            | otherwise         = Bin p2 m2 l2 (union t1 r2)

union (Tip k x) t = insert k x t
union t (Tip k x) = insertWith (\x y -> y) k x t  -- right bias
union Nil t       = t
union t Nil       = t

-- | /O(n+m)/. The union with a combining function. 
unionWith :: (a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
unionWith f m1 m2
  = unionWithKey (\k x y -> f x y) m1 m2

-- | /O(n+m)/. The union with a combining function. 
unionWithKey :: (Key -> a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
unionWithKey f t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2  = union1
  | shorter m2 m1  = union2
  | p1 == p2       = Bin p1 m1 (unionWithKey f l1 l2) (unionWithKey f r1 r2)
  | otherwise      = join p1 t1 p2 t2
  where
    union1  | nomatch p2 p1 m1  = join p1 t1 p2 t2
            | zero p2 m1        = Bin p1 m1 (unionWithKey f l1 t2) r1
            | otherwise         = Bin p1 m1 l1 (unionWithKey f r1 t2)

    union2  | nomatch p1 p2 m2  = join p1 t1 p2 t2
            | zero p1 m2        = Bin p2 m2 (unionWithKey f t1 l2) r2
            | otherwise         = Bin p2 m2 l2 (unionWithKey f t1 r2)

unionWithKey f (Tip k x) t = insertWithKey f k x t
unionWithKey f t (Tip k x) = insertWithKey (\k x y -> f k y x) k x t  -- right bias
unionWithKey f Nil t  = t
unionWithKey f t Nil  = t

{--------------------------------------------------------------------
  Difference
--------------------------------------------------------------------}
-- | /O(n+m)/. Difference between two maps (based on keys). 
difference :: IntMap a -> IntMap b -> IntMap a
difference t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2  = difference1
  | shorter m2 m1  = difference2
  | p1 == p2       = bin p1 m1 (difference l1 l2) (difference r1 r2)
  | otherwise      = t1
  where
    difference1 | nomatch p2 p1 m1  = t1
                | zero p2 m1        = bin p1 m1 (difference l1 t2) r1
                | otherwise         = bin p1 m1 l1 (difference r1 t2)

    difference2 | nomatch p1 p2 m2  = t1
                | zero p1 m2        = difference t1 l2
                | otherwise         = difference t1 r2

difference t1@(Tip k x) t2 
  | member k t2  = Nil
  | otherwise    = t1

difference Nil t       = Nil
difference t (Tip k x) = delete k t
difference t Nil       = t

-- | /O(n+m)/. Difference with a combining function. 
differenceWith :: (a -> b -> Maybe a) -> IntMap a -> IntMap b -> IntMap a
differenceWith f m1 m2
  = differenceWithKey (\k x y -> f x y) m1 m2

-- | /O(n+m)/. Difference with a combining function. When two equal keys are
-- encountered, the combining function is applied to the key and both values.
-- If it returns 'Nothing', the element is discarded (proper set difference).
-- If it returns (@'Just' y@), the element is updated with a new value @y@. 
differenceWithKey :: (Key -> a -> b -> Maybe a) -> IntMap a -> IntMap b -> IntMap a
differenceWithKey f t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2  = difference1
  | shorter m2 m1  = difference2
  | p1 == p2       = bin p1 m1 (differenceWithKey f l1 l2) (differenceWithKey f r1 r2)
  | otherwise      = t1
  where
    difference1 | nomatch p2 p1 m1  = t1
                | zero p2 m1        = bin p1 m1 (differenceWithKey f l1 t2) r1
                | otherwise         = bin p1 m1 l1 (differenceWithKey f r1 t2)

    difference2 | nomatch p1 p2 m2  = t1
                | zero p1 m2        = differenceWithKey f t1 l2
                | otherwise         = differenceWithKey f t1 r2

differenceWithKey f t1@(Tip k x) t2 
  = case lookup k t2 of
      Just y  -> case f k x y of
                   Just y' -> Tip k y'
                   Nothing -> Nil
      Nothing -> t1

differenceWithKey f Nil t       = Nil
differenceWithKey f t (Tip k y) = updateWithKey (\k x -> f k x y) k t
differenceWithKey f t Nil       = t


{--------------------------------------------------------------------
  Intersection
--------------------------------------------------------------------}
-- | /O(n+m)/. The (left-biased) intersection of two maps (based on keys). 
intersection :: IntMap a -> IntMap b -> IntMap a
intersection t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2  = intersection1
  | shorter m2 m1  = intersection2
  | p1 == p2       = bin p1 m1 (intersection l1 l2) (intersection r1 r2)
  | otherwise      = Nil
  where
    intersection1 | nomatch p2 p1 m1  = Nil
                  | zero p2 m1        = intersection l1 t2
                  | otherwise         = intersection r1 t2

    intersection2 | nomatch p1 p2 m2  = Nil
                  | zero p1 m2        = intersection t1 l2
                  | otherwise         = intersection t1 r2

intersection t1@(Tip k x) t2 
  | member k t2  = t1
  | otherwise    = Nil
intersection t (Tip k x) 
  = case lookup k t of
      Just y  -> Tip k y
      Nothing -> Nil
intersection Nil t = Nil
intersection t Nil = Nil

-- | /O(n+m)/. The intersection with a combining function. 
intersectionWith :: (a -> b -> a) -> IntMap a -> IntMap b -> IntMap a
intersectionWith f m1 m2
  = intersectionWithKey (\k x y -> f x y) m1 m2

-- | /O(n+m)/. The intersection with a combining function. 
intersectionWithKey :: (Key -> a -> b -> a) -> IntMap a -> IntMap b -> IntMap a
intersectionWithKey f t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2  = intersection1
  | shorter m2 m1  = intersection2
  | p1 == p2       = bin p1 m1 (intersectionWithKey f l1 l2) (intersectionWithKey f r1 r2)
  | otherwise      = Nil
  where
    intersection1 | nomatch p2 p1 m1  = Nil
                  | zero p2 m1        = intersectionWithKey f l1 t2
                  | otherwise         = intersectionWithKey f r1 t2

    intersection2 | nomatch p1 p2 m2  = Nil
                  | zero p1 m2        = intersectionWithKey f t1 l2
                  | otherwise         = intersectionWithKey f t1 r2

intersectionWithKey f t1@(Tip k x) t2 
  = case lookup k t2 of
      Just y  -> Tip k (f k x y)
      Nothing -> Nil
intersectionWithKey f t1 (Tip k y) 
  = case lookup k t1 of
      Just x  -> Tip k (f k x y)
      Nothing -> Nil
intersectionWithKey f Nil t = Nil
intersectionWithKey f t Nil = Nil


{--------------------------------------------------------------------
  Submap
--------------------------------------------------------------------}
-- | /O(n+m)/. Is this a proper submap? (ie. a submap but not equal). 
-- Defined as (@'isProperSubmapOf' = 'isProperSubmapOfBy' (==)@).
isProperSubmapOf :: Eq a => IntMap a -> IntMap a -> Bool
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
isProperSubmapOfBy :: (a -> b -> Bool) -> IntMap a -> IntMap b -> Bool
isProperSubmapOfBy pred t1 t2
  = case submapCmp pred t1 t2 of 
      LT -> True
      ge -> False

submapCmp pred t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2  = GT
  | shorter m2 m1  = submapCmpLt
  | p1 == p2       = submapCmpEq
  | otherwise      = GT  -- disjoint
  where
    submapCmpLt | nomatch p1 p2 m2  = GT
                | zero p1 m2        = submapCmp pred t1 l2
                | otherwise         = submapCmp pred t1 r2
    submapCmpEq = case (submapCmp pred l1 l2, submapCmp pred r1 r2) of
                    (GT,_ ) -> GT
                    (_ ,GT) -> GT
                    (EQ,EQ) -> EQ
                    other   -> LT

submapCmp pred (Bin p m l r) t  = GT
submapCmp pred (Tip kx x) (Tip ky y)  
  | (kx == ky) && pred x y = EQ
  | otherwise              = GT  -- disjoint
submapCmp pred (Tip k x) t      
  = case lookup k t of
     Just y  | pred x y -> LT
     other   -> GT -- disjoint
submapCmp pred Nil Nil = EQ
submapCmp pred Nil t   = LT

-- | /O(n+m)/. Is this a submap?
-- Defined as (@'isSubmapOf' = 'isSubmapOfBy' (==)@).
isSubmapOf :: Eq a => IntMap a -> IntMap a -> Bool
isSubmapOf m1 m2
  = isSubmapOfBy (==) m1 m2

{- | /O(n+m)/. 
 The expression (@'isSubmapOfBy' f m1 m2@) returns 'True' if
 all keys in @m1@ are in @m2@, and when @f@ returns 'True' when
 applied to their respective values. For example, the following 
 expressions are all 'True':
 
  > isSubmapOfBy (==) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
  > isSubmapOfBy (<=) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
  > isSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1),(2,2)])

 But the following are all 'False':
 
  > isSubmapOfBy (==) (fromList [(1,2)]) (fromList [(1,1),(2,2)])
  > isSubmapOfBy (<) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
  > isSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1)])
-}

isSubmapOfBy :: (a -> b -> Bool) -> IntMap a -> IntMap b -> Bool
isSubmapOfBy pred t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2  = False
  | shorter m2 m1  = match p1 p2 m2 && (if zero p1 m2 then isSubmapOfBy pred t1 l2
                                                      else isSubmapOfBy pred t1 r2)                     
  | otherwise      = (p1==p2) && isSubmapOfBy pred l1 l2 && isSubmapOfBy pred r1 r2
isSubmapOfBy pred (Bin p m l r) t  = False
isSubmapOfBy pred (Tip k x) t      = case lookup k t of
                                   Just y  -> pred x y
                                   Nothing -> False 
isSubmapOfBy pred Nil t            = True

{--------------------------------------------------------------------
  Mapping
--------------------------------------------------------------------}
-- | /O(n)/. Map a function over all values in the map.
map :: (a -> b) -> IntMap a -> IntMap b
map f m
  = mapWithKey (\k x -> f x) m

-- | /O(n)/. Map a function over all values in the map.
mapWithKey :: (Key -> a -> b) -> IntMap a -> IntMap b
mapWithKey f t  
  = case t of
      Bin p m l r -> Bin p m (mapWithKey f l) (mapWithKey f r)
      Tip k x     -> Tip k (f k x)
      Nil         -> Nil

-- | /O(n)/. The function @'mapAccum'@ threads an accumulating
-- argument through the map in ascending order of keys.
mapAccum :: (a -> b -> (a,c)) -> a -> IntMap b -> (a,IntMap c)
mapAccum f a m
  = mapAccumWithKey (\a k x -> f a x) a m

-- | /O(n)/. The function @'mapAccumWithKey'@ threads an accumulating
-- argument through the map in ascending order of keys.
mapAccumWithKey :: (a -> Key -> b -> (a,c)) -> a -> IntMap b -> (a,IntMap c)
mapAccumWithKey f a t
  = mapAccumL f a t

-- | /O(n)/. The function @'mapAccumL'@ threads an accumulating
-- argument through the map in ascending order of keys.
mapAccumL :: (a -> Key -> b -> (a,c)) -> a -> IntMap b -> (a,IntMap c)
mapAccumL f a t
  = case t of
      Bin p m l r -> let (a1,l') = mapAccumL f a l
                         (a2,r') = mapAccumL f a1 r
                     in (a2,Bin p m l' r')
      Tip k x     -> let (a',x') = f a k x in (a',Tip k x')
      Nil         -> (a,Nil)


-- | /O(n)/. The function @'mapAccumR'@ threads an accumulating
-- argument throught the map in descending order of keys.
mapAccumR :: (a -> Key -> b -> (a,c)) -> a -> IntMap b -> (a,IntMap c)
mapAccumR f a t
  = case t of
      Bin p m l r -> let (a1,r') = mapAccumR f a r
                         (a2,l') = mapAccumR f a1 l
                     in (a2,Bin p m l' r')
      Tip k x     -> let (a',x') = f a k x in (a',Tip k x')
      Nil         -> (a,Nil)

{--------------------------------------------------------------------
  Filter
--------------------------------------------------------------------}
-- | /O(n)/. Filter all values that satisfy some predicate.
filter :: (a -> Bool) -> IntMap a -> IntMap a
filter p m
  = filterWithKey (\k x -> p x) m

-- | /O(n)/. Filter all keys\/values that satisfy some predicate.
filterWithKey :: (Key -> a -> Bool) -> IntMap a -> IntMap a
filterWithKey pred t
  = case t of
      Bin p m l r 
        -> bin p m (filterWithKey pred l) (filterWithKey pred r)
      Tip k x 
        | pred k x  -> t
        | otherwise -> Nil
      Nil -> Nil

-- | /O(n)/. partition the map according to some predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
partition :: (a -> Bool) -> IntMap a -> (IntMap a,IntMap a)
partition p m
  = partitionWithKey (\k x -> p x) m

-- | /O(n)/. partition the map according to some predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
partitionWithKey :: (Key -> a -> Bool) -> IntMap a -> (IntMap a,IntMap a)
partitionWithKey pred t
  = case t of
      Bin p m l r 
        -> let (l1,l2) = partitionWithKey pred l
               (r1,r2) = partitionWithKey pred r
           in (bin p m l1 r1, bin p m l2 r2)
      Tip k x 
        | pred k x  -> (t,Nil)
        | otherwise -> (Nil,t)
      Nil -> (Nil,Nil)


-- | /O(log n)/. The expression (@'split' k map@) is a pair @(map1,map2)@
-- where all keys in @map1@ are lower than @k@ and all keys in
-- @map2@ larger than @k@. Any key equal to @k@ is found in neither @map1@ nor @map2@.
split :: Key -> IntMap a -> (IntMap a,IntMap a)
split k t
  = case t of
      Bin p m l r 
          | m < 0 -> (if k >= 0 -- handle negative numbers.
                      then let (lt,gt) = split' k l in (union r lt, gt)
                      else let (lt,gt) = split' k r in (lt, union gt l))
          | otherwise   -> split' k t
      Tip ky y 
        | k>ky      -> (t,Nil)
        | k<ky      -> (Nil,t)
        | otherwise -> (Nil,Nil)
      Nil -> (Nil,Nil)

split' :: Key -> IntMap a -> (IntMap a,IntMap a)
split' k t
  = case t of
      Bin p m l r
        | nomatch k p m -> if k>p then (t,Nil) else (Nil,t)
        | zero k m  -> let (lt,gt) = split k l in (lt,union gt r)
        | otherwise -> let (lt,gt) = split k r in (union l lt,gt)
      Tip ky y 
        | k>ky      -> (t,Nil)
        | k<ky      -> (Nil,t)
        | otherwise -> (Nil,Nil)
      Nil -> (Nil,Nil)

-- | /O(log n)/. Performs a 'split' but also returns whether the pivot
-- key was found in the original map.
splitLookup :: Key -> IntMap a -> (IntMap a,Maybe a,IntMap a)
splitLookup k t
  = case t of
      Bin p m l r
          | m < 0 -> (if k >= 0 -- handle negative numbers.
                      then let (lt,found,gt) = splitLookup' k l in (union r lt,found, gt)
                      else let (lt,found,gt) = splitLookup' k r in (lt,found, union gt l))
          | otherwise   -> splitLookup' k t
      Tip ky y 
        | k>ky      -> (t,Nothing,Nil)
        | k<ky      -> (Nil,Nothing,t)
        | otherwise -> (Nil,Just y,Nil)
      Nil -> (Nil,Nothing,Nil)

splitLookup' :: Key -> IntMap a -> (IntMap a,Maybe a,IntMap a)
splitLookup' k t
  = case t of
      Bin p m l r
        | nomatch k p m -> if k>p then (t,Nothing,Nil) else (Nil,Nothing,t)
        | zero k m  -> let (lt,found,gt) = splitLookup k l in (lt,found,union gt r)
        | otherwise -> let (lt,found,gt) = splitLookup k r in (union l lt,found,gt)
      Tip ky y 
        | k>ky      -> (t,Nothing,Nil)
        | k<ky      -> (Nil,Nothing,t)
        | otherwise -> (Nil,Just y,Nil)
      Nil -> (Nil,Nothing,Nil)

{--------------------------------------------------------------------
  Fold
--------------------------------------------------------------------}
-- | /O(n)/. Fold the values in the map, such that
-- @'fold' f z == 'Prelude.foldr' f z . 'elems'@.
-- For example,
--
-- > elems map = fold (:) [] map
--
fold :: (a -> b -> b) -> b -> IntMap a -> b
fold f z t
  = foldWithKey (\k x y -> f x y) z t

-- | /O(n)/. Fold the keys and values in the map, such that
-- @'foldWithKey' f z == 'Prelude.foldr' ('uncurry' f) z . 'toAscList'@.
-- For example,
--
-- > keys map = foldWithKey (\k x ks -> k:ks) [] map
--
foldWithKey :: (Key -> a -> b -> b) -> b -> IntMap a -> b
foldWithKey f z t
  = foldr f z t

foldr :: (Key -> a -> b -> b) -> b -> IntMap a -> b
foldr f z t
  = case t of
      Bin 0 m l r | m < 0 -> foldr' f (foldr' f z l) r  -- put negative numbers before.
      Bin _ _ _ _ -> foldr' f z t
      Tip k x     -> f k x z
      Nil         -> z

foldr' :: (Key -> a -> b -> b) -> b -> IntMap a -> b
foldr' f z t
  = case t of
      Bin p m l r -> foldr' f (foldr' f z r) l
      Tip k x     -> f k x z
      Nil         -> z



{--------------------------------------------------------------------
  List variations 
--------------------------------------------------------------------}
-- | /O(n)/.
-- Return all elements of the map in the ascending order of their keys.
elems :: IntMap a -> [a]
elems m
  = foldWithKey (\k x xs -> x:xs) [] m  

-- | /O(n)/. Return all keys of the map in ascending order.
keys  :: IntMap a -> [Key]
keys m
  = foldWithKey (\k x ks -> k:ks) [] m

-- | /O(n*min(n,W))/. The set of all keys of the map.
keysSet :: IntMap a -> IntSet.IntSet
keysSet m = IntSet.fromDistinctAscList (keys m)


-- | /O(n)/. Return all key\/value pairs in the map in ascending key order.
assocs :: IntMap a -> [(Key,a)]
assocs m
  = toList m


{--------------------------------------------------------------------
  Lists 
--------------------------------------------------------------------}
-- | /O(n)/. Convert the map to a list of key\/value pairs.
toList :: IntMap a -> [(Key,a)]
toList t
  = foldWithKey (\k x xs -> (k,x):xs) [] t

-- | /O(n)/. Convert the map to a list of key\/value pairs where the
-- keys are in ascending order.
toAscList :: IntMap a -> [(Key,a)]
toAscList t   
  = -- NOTE: the following algorithm only works for big-endian trees
    let (pos,neg) = span (\(k,x) -> k >=0) (foldr (\k x xs -> (k,x):xs) [] t) in neg ++ pos

-- | /O(n*min(n,W))/. Create a map from a list of key\/value pairs.
fromList :: [(Key,a)] -> IntMap a
fromList xs
  = foldlStrict ins empty xs
  where
    ins t (k,x)  = insert k x t

-- | /O(n*min(n,W))/.  Create a map from a list of key\/value pairs with a combining function. See also 'fromAscListWith'.
fromListWith :: (a -> a -> a) -> [(Key,a)] -> IntMap a 
fromListWith f xs
  = fromListWithKey (\k x y -> f x y) xs

-- | /O(n*min(n,W))/.  Build a map from a list of key\/value pairs with a combining function. See also fromAscListWithKey'.
fromListWithKey :: (Key -> a -> a -> a) -> [(Key,a)] -> IntMap a 
fromListWithKey f xs 
  = foldlStrict ins empty xs
  where
    ins t (k,x) = insertWithKey f k x t

-- | /O(n*min(n,W))/. Build a map from a list of key\/value pairs where
-- the keys are in ascending order.
fromAscList :: [(Key,a)] -> IntMap a
fromAscList xs
  = fromList xs

-- | /O(n*min(n,W))/. Build a map from a list of key\/value pairs where
-- the keys are in ascending order, with a combining function on equal keys.
fromAscListWith :: (a -> a -> a) -> [(Key,a)] -> IntMap a
fromAscListWith f xs
  = fromListWith f xs

-- | /O(n*min(n,W))/. Build a map from a list of key\/value pairs where
-- the keys are in ascending order, with a combining function on equal keys.
fromAscListWithKey :: (Key -> a -> a -> a) -> [(Key,a)] -> IntMap a
fromAscListWithKey f xs
  = fromListWithKey f xs

-- | /O(n*min(n,W))/. Build a map from a list of key\/value pairs where
-- the keys are in ascending order and all distinct.
fromDistinctAscList :: [(Key,a)] -> IntMap a
fromDistinctAscList xs
  = fromList xs


{--------------------------------------------------------------------
  Eq 
--------------------------------------------------------------------}
instance Eq a => Eq (IntMap a) where
  t1 == t2  = equal t1 t2
  t1 /= t2  = nequal t1 t2

equal :: Eq a => IntMap a -> IntMap a -> Bool
equal (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  = (m1 == m2) && (p1 == p2) && (equal l1 l2) && (equal r1 r2) 
equal (Tip kx x) (Tip ky y)
  = (kx == ky) && (x==y)
equal Nil Nil = True
equal t1 t2   = False

nequal :: Eq a => IntMap a -> IntMap a -> Bool
nequal (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  = (m1 /= m2) || (p1 /= p2) || (nequal l1 l2) || (nequal r1 r2) 
nequal (Tip kx x) (Tip ky y)
  = (kx /= ky) || (x/=y)
nequal Nil Nil = False
nequal t1 t2   = True

{--------------------------------------------------------------------
  Ord 
--------------------------------------------------------------------}

instance Ord a => Ord (IntMap a) where
    compare m1 m2 = compare (toList m1) (toList m2)

{--------------------------------------------------------------------
  Functor 
--------------------------------------------------------------------}

instance Functor IntMap where
    fmap = map

{--------------------------------------------------------------------
  Show 
--------------------------------------------------------------------}

instance Show a => Show (IntMap a) where
  showsPrec d m   = showParen (d > 10) $
    showString "fromList " . shows (toList m)

showMap :: (Show a) => [(Key,a)] -> ShowS
showMap []     
  = showString "{}" 
showMap (x:xs) 
  = showChar '{' . showElem x . showTail xs
  where
    showTail []     = showChar '}'
    showTail (x:xs) = showChar ',' . showElem x . showTail xs
    
    showElem (k,x)  = shows k . showString ":=" . shows x

{--------------------------------------------------------------------
  Read
--------------------------------------------------------------------}
instance (Read e) => Read (IntMap e) where
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

{--------------------------------------------------------------------
  Typeable
--------------------------------------------------------------------}

#include "Typeable.h"
INSTANCE_TYPEABLE1(IntMap,intMapTc,"IntMap")

{--------------------------------------------------------------------
  Debugging
--------------------------------------------------------------------}
-- | /O(n)/. Show the tree that implements the map. The tree is shown
-- in a compressed, hanging format.
showTree :: Show a => IntMap a -> String
showTree s
  = showTreeWith True False s


{- | /O(n)/. The expression (@'showTreeWith' hang wide map@) shows
 the tree that implements the map. If @hang@ is
 'True', a /hanging/ tree is shown otherwise a rotated tree is shown. If
 @wide@ is 'True', an extra wide version is shown.
-}
showTreeWith :: Show a => Bool -> Bool -> IntMap a -> String
showTreeWith hang wide t
  | hang      = (showsTreeHang wide [] t) ""
  | otherwise = (showsTree wide [] [] t) ""

showsTree :: Show a => Bool -> [String] -> [String] -> IntMap a -> ShowS
showsTree wide lbars rbars t
  = case t of
      Bin p m l r
          -> showsTree wide (withBar rbars) (withEmpty rbars) r .
             showWide wide rbars .
             showsBars lbars . showString (showBin p m) . showString "\n" .
             showWide wide lbars .
             showsTree wide (withEmpty lbars) (withBar lbars) l
      Tip k x
          -> showsBars lbars . showString " " . shows k . showString ":=" . shows x . showString "\n" 
      Nil -> showsBars lbars . showString "|\n"

showsTreeHang :: Show a => Bool -> [String] -> IntMap a -> ShowS
showsTreeHang wide bars t
  = case t of
      Bin p m l r
          -> showsBars bars . showString (showBin p m) . showString "\n" . 
             showWide wide bars .
             showsTreeHang wide (withBar bars) l .
             showWide wide bars .
             showsTreeHang wide (withEmpty bars) r
      Tip k x
          -> showsBars bars . showString " " . shows k . showString ":=" . shows x . showString "\n" 
      Nil -> showsBars bars . showString "|\n" 
      
showBin p m
  = "*" -- ++ show (p,m)

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
  Helpers
--------------------------------------------------------------------}
{--------------------------------------------------------------------
  Join
--------------------------------------------------------------------}
join :: Prefix -> IntMap a -> Prefix -> IntMap a -> IntMap a
join p1 t1 p2 t2
  | zero p1 m = Bin p m t1 t2
  | otherwise = Bin p m t2 t1
  where
    m = branchMask p1 p2
    p = mask p1 m

{--------------------------------------------------------------------
  @bin@ assures that we never have empty trees within a tree.
--------------------------------------------------------------------}
bin :: Prefix -> Mask -> IntMap a -> IntMap a -> IntMap a
bin p m l Nil = l
bin p m Nil r = r
bin p m l r   = Bin p m l r

  
{--------------------------------------------------------------------
  Endian independent bit twiddling
--------------------------------------------------------------------}
zero :: Key -> Mask -> Bool
zero i m
  = (natFromInt i) .&. (natFromInt m) == 0

nomatch,match :: Key -> Prefix -> Mask -> Bool
nomatch i p m
  = (mask i m) /= p

match i p m
  = (mask i m) == p

mask :: Key -> Mask -> Prefix
mask i m
  = maskW (natFromInt i) (natFromInt m)


zeroN :: Nat -> Nat -> Bool
zeroN i m = (i .&. m) == 0

{--------------------------------------------------------------------
  Big endian operations  
--------------------------------------------------------------------}
maskW :: Nat -> Nat -> Prefix
maskW i m
  = intFromNat (i .&. (complement (m-1) `xor` m))

shorter :: Mask -> Mask -> Bool
shorter m1 m2
  = (natFromInt m1) > (natFromInt m2)

branchMask :: Prefix -> Prefix -> Mask
branchMask p1 p2
  = intFromNat (highestBitMask (natFromInt p1 `xor` natFromInt p2))
  
{----------------------------------------------------------------------
  Finding the highest bit (mask) in a word [x] can be done efficiently in
  three ways:
  * convert to a floating point value and the mantissa tells us the 
    [log2(x)] that corresponds with the highest bit position. The mantissa 
    is retrieved either via the standard C function [frexp] or by some bit 
    twiddling on IEEE compatible numbers (float). Note that one needs to 
    use at least [double] precision for an accurate mantissa of 32 bit 
    numbers.
  * use bit twiddling, a logarithmic sequence of bitwise or's and shifts (bit).
  * use processor specific assembler instruction (asm).

  The most portable way would be [bit], but is it efficient enough?
  I have measured the cycle counts of the different methods on an AMD 
  Athlon-XP 1800 (~ Pentium III 1.8Ghz) using the RDTSC instruction:

  highestBitMask: method  cycles
                  --------------
                   frexp   200
                   float    33
                   bit      11
                   asm      12

  highestBit:     method  cycles
                  --------------
                   frexp   195
                   float    33
                   bit      11
                   asm      11

  Wow, the bit twiddling is on today's RISC like machines even faster
  than a single CISC instruction (BSR)!
----------------------------------------------------------------------}

{----------------------------------------------------------------------
  [highestBitMask] returns a word where only the highest bit is set.
  It is found by first setting all bits in lower positions than the 
  highest bit and than taking an exclusive or with the original value.
  Allthough the function may look expensive, GHC compiles this into
  excellent C code that subsequently compiled into highly efficient
  machine code. The algorithm is derived from Jorg Arndt's FXT library.
----------------------------------------------------------------------}
highestBitMask :: Nat -> Nat
highestBitMask x
  = case (x .|. shiftRL x 1) of 
     x -> case (x .|. shiftRL x 2) of 
      x -> case (x .|. shiftRL x 4) of 
       x -> case (x .|. shiftRL x 8) of 
        x -> case (x .|. shiftRL x 16) of 
         x -> case (x .|. shiftRL x 32) of   -- for 64 bit platforms
          x -> (x `xor` (shiftRL x 1))


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
testTree :: [Int] -> IntMap Int
testTree xs   = fromList [(x,x*x*30696 `mod` 65521) | x <- xs]
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
instance Arbitrary a => Arbitrary (IntMap a) where
  arbitrary = do{ ks <- arbitrary
                ; xs <- mapM (\k -> do{ x <- arbitrary; return (k,x)}) ks
                ; return (fromList xs)
                }


{--------------------------------------------------------------------
  Single, Insert, Delete
--------------------------------------------------------------------}
prop_Single :: Key -> Int -> Bool
prop_Single k x
  = (insert k x empty == singleton k x)

prop_InsertDelete :: Key -> Int -> IntMap Int -> Property
prop_InsertDelete k x t
  = not (member k t) ==> delete k (insert k x t) == t

prop_UpdateDelete :: Key -> IntMap Int -> Bool  
prop_UpdateDelete k t
  = update (const Nothing) k t == delete k t


{--------------------------------------------------------------------
  Union
--------------------------------------------------------------------}
prop_UnionInsert :: Key -> Int -> IntMap Int -> Bool
prop_UnionInsert k x t
  = union (singleton k x) t == insert k x t

prop_UnionAssoc :: IntMap Int -> IntMap Int -> IntMap Int -> Bool
prop_UnionAssoc t1 t2 t3
  = union t1 (union t2 t3) == union (union t1 t2) t3

prop_UnionComm :: IntMap Int -> IntMap Int -> Bool
prop_UnionComm t1 t2
  = (union t1 t2 == unionWith (\x y -> y) t2 t1)


prop_Diff :: [(Key,Int)] -> [(Key,Int)] -> Bool
prop_Diff xs ys
  =  List.sort (keys (difference (fromListWith (+) xs) (fromListWith (+) ys))) 
    == List.sort ((List.\\) (nub (Prelude.map fst xs))  (nub (Prelude.map fst ys)))

prop_Int :: [(Key,Int)] -> [(Key,Int)] -> Bool
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

prop_List :: [Key] -> Bool
prop_List xs
  = (sort (nub xs) == [x | (x,()) <- toAscList (fromList [(x,()) | x <- xs])])
-}

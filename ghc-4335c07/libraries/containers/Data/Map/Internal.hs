{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards #-}
#if __GLASGOW_HASKELL__
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
#endif
#if __GLASGOW_HASKELL__ >= 703
{-# LANGUAGE Trustworthy #-}
#endif
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
#define USE_MAGIC_PROXY 1
#endif

#ifdef USE_MAGIC_PROXY
{-# LANGUAGE MagicHash #-}
#endif

#include "containers.h"

#if !(WORD_SIZE_IN_BITS >= 61)
#define DEFINE_ALTERF_FALLBACK 1
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Map.Internal
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- = WARNING
--
-- This module is considered __internal__.
--
-- The Package Versioning Policy __does not apply__.
--
-- This contents of this module may change __in any way whatsoever__
-- and __without any warning__ between minor versions of this package.
--
-- Authors importing this module are expected to track development
-- closely.
--
-- = Description
--
-- An efficient implementation of maps from keys to values (dictionaries).
--
-- Since many function names (but not the type name) clash with
-- "Prelude" names, this module is usually imported @qualified@, e.g.
--
-- >  import Data.Map (Map)
-- >  import qualified Data.Map as Map
--
-- The implementation of 'Map' is based on /size balanced/ binary trees (or
-- trees of /bounded balance/) as described by:
--
--    * Stephen Adams, \"/Efficient sets: a balancing act/\",
--     Journal of Functional Programming 3(4):553-562, October 1993,
--     <http://www.swiss.ai.mit.edu/~adams/BB/>.
--    * J. Nievergelt and E.M. Reingold,
--      \"/Binary search trees of bounded balance/\",
--      SIAM journal of computing 2(1), March 1973.
--
--  Bounds for 'union', 'intersection', and 'difference' are as given
--  by
--
--    * Guy Blelloch, Daniel Ferizovic, and Yihan Sun,
--      \"/Just Join for Parallel Ordered Sets/\",
--      <https://arxiv.org/abs/1602.02120v3>.
--
-- Note that the implementation is /left-biased/ -- the elements of a
-- first argument are always preferred to the second, for example in
-- 'union' or 'insert'.
--
-- Operation comments contain the operation time complexity in
-- the Big-O notation <http://en.wikipedia.org/wiki/Big_O_notation>.
-----------------------------------------------------------------------------

-- [Note: Using INLINABLE]
-- ~~~~~~~~~~~~~~~~~~~~~~~
-- It is crucial to the performance that the functions specialize on the Ord
-- type when possible. GHC 7.0 and higher does this by itself when it sees th
-- unfolding of a function -- that is why all public functions are marked
-- INLINABLE (that exposes the unfolding).


-- [Note: Using INLINE]
-- ~~~~~~~~~~~~~~~~~~~~
-- For other compilers and GHC pre 7.0, we mark some of the functions INLINE.
-- We mark the functions that just navigate down the tree (lookup, insert,
-- delete and similar). That navigation code gets inlined and thus specialized
-- when possible. There is a price to pay -- code growth. The code INLINED is
-- therefore only the tree navigation, all the real work (rebalancing) is not
-- INLINED by using a NOINLINE.
--
-- All methods marked INLINE have to be nonrecursive -- a 'go' function doing
-- the real work is provided.


-- [Note: Type of local 'go' function]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- If the local 'go' function uses an Ord class, it sometimes heap-allocates
-- the Ord dictionary when the 'go' function does not have explicit type.
-- In that case we give 'go' explicit type. But this slightly decrease
-- performance, as the resulting 'go' function can float out to top level.


-- [Note: Local 'go' functions and capturing]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- As opposed to Map, when 'go' function captures an argument, increased
-- heap-allocation can occur: sometimes in a polymorphic function, the 'go'
-- floats out of its enclosing function and then it heap-allocates the
-- dictionary and the argument. Maybe it floats out too late and strictness
-- analyzer cannot see that these could be passed on stack.
--

-- [Note: Order of constructors]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The order of constructors of Map matters when considering performance.
-- Currently in GHC 7.0, when type has 2 constructors, a forward conditional
-- jump is made when successfully matching second constructor. Successful match
-- of first constructor results in the forward jump not taken.
-- On GHC 7.0, reordering constructors from Tip | Bin to Bin | Tip
-- improves the benchmark by up to 10% on x86.

module Data.Map.Internal (
    -- * Map type
      Map(..)          -- instance Eq,Show,Read

    -- * Operators
    , (!), (!?), (\\)

    -- * Query
    , null
    , size
    , member
    , notMember
    , lookup
    , findWithDefault
    , lookupLT
    , lookupGT
    , lookupLE
    , lookupGE

    -- * Construction
    , empty
    , singleton

    -- ** Insertion
    , insert
    , insertWith
    , insertWithKey
    , insertLookupWithKey

    -- ** Delete\/Update
    , delete
    , adjust
    , adjustWithKey
    , update
    , updateWithKey
    , updateLookupWithKey
    , alter
    , alterF

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

    -- ** General combining function
    , SimpleWhenMissing
    , SimpleWhenMatched
    , runWhenMatched
    , runWhenMissing
    , merge
    -- *** @WhenMatched@ tactics
    , zipWithMaybeMatched
    , zipWithMatched
    -- *** @WhenMissing@ tactics
    , mapMaybeMissing
    , dropMissing
    , preserveMissing
    , mapMissing
    , filterMissing

    -- ** Applicative general combining function
    , WhenMissing (..)
    , WhenMatched (..)
    , mergeA

    -- *** @WhenMatched@ tactics
    -- | The tactics described for 'merge' work for
    -- 'mergeA' as well. Furthermore, the following
    -- are available.
    , zipWithMaybeAMatched
    , zipWithAMatched

    -- *** @WhenMissing@ tactics
    -- | The tactics described for 'merge' work for
    -- 'mergeA' as well. Furthermore, the following
    -- are available.
    , traverseMaybeMissing
    , traverseMissing
    , filterAMissing

    -- ** Deprecated general combining function

    , mergeWithKey

    -- * Traversal
    -- ** Map
    , map
    , mapWithKey
    , traverseWithKey
    , traverseMaybeWithKey
    , mapAccum
    , mapAccumWithKey
    , mapAccumRWithKey
    , mapKeys
    , mapKeysWith
    , mapKeysMonotonic

    -- * Folds
    , foldr
    , foldl
    , foldrWithKey
    , foldlWithKey
    , foldMapWithKey

    -- ** Strict folds
    , foldr'
    , foldl'
    , foldrWithKey'
    , foldlWithKey'

    -- * Conversion
    , elems
    , keys
    , assocs
    , keysSet
    , fromSet

    -- ** Lists
    , toList
    , fromList
    , fromListWith
    , fromListWithKey

    -- ** Ordered lists
    , toAscList
    , toDescList
    , fromAscList
    , fromAscListWith
    , fromAscListWithKey
    , fromDistinctAscList
    , fromDescList
    , fromDescListWith
    , fromDescListWithKey
    , fromDistinctDescList

    -- * Filter
    , filter
    , filterWithKey

    , takeWhileAntitone
    , dropWhileAntitone
    , spanAntitone

    , restrictKeys
    , withoutKeys
    , partition
    , partitionWithKey

    , mapMaybe
    , mapMaybeWithKey
    , mapEither
    , mapEitherWithKey

    , split
    , splitLookup
    , splitRoot

    -- * Submap
    , isSubmapOf, isSubmapOfBy
    , isProperSubmapOf, isProperSubmapOfBy

    -- * Indexed
    , lookupIndex
    , findIndex
    , elemAt
    , updateAt
    , deleteAt
    , take
    , drop
    , splitAt

    -- * Min\/Max
    , lookupMin
    , lookupMax
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
    , minView
    , maxView
    , minViewWithKey
    , maxViewWithKey

    -- Used by the strict version
    , AreWeStrict (..)
    , atKeyImpl
#if __GLASGOW_HASKELL__ && MIN_VERSION_base(4,8,0)
    , atKeyPlain
#endif
    , bin
    , balance
    , balanceL
    , balanceR
    , delta
    , insertMax
    , link
    , link2
    , glue
    , MaybeS(..)
    , Identity(..)

    -- Used by Map.Merge.Lazy
    , mapWhenMissing
    , mapWhenMatched
    , lmapWhenMissing
    , contramapFirstWhenMatched
    , contramapSecondWhenMatched
    , mapGentlyWhenMissing
    , mapGentlyWhenMatched
    ) where

#if MIN_VERSION_base(4,8,0)
import Data.Functor.Identity (Identity (..))
import Control.Applicative (liftA3)
#else
import Control.Applicative (Applicative(..), (<$>), liftA3)
import Data.Monoid (Monoid(..))
import Data.Traversable (Traversable(traverse))
#endif
#if MIN_VERSION_base(4,9,0)
import Data.Functor.Classes
import Data.Semigroup (Semigroup((<>), stimes), stimesIdempotentMonoid)
#endif
import Control.Applicative (Const (..))
import Control.DeepSeq (NFData(rnf))
import Data.Bits (shiftL, shiftR)
import qualified Data.Foldable as Foldable
import Data.Typeable
import Prelude hiding (lookup, map, filter, foldr, foldl, null, splitAt, take, drop)

import qualified Data.Set.Internal as Set
import Data.Set.Internal (Set)
import Utils.Containers.Internal.PtrEquality (ptrEq)
import Utils.Containers.Internal.StrictFold
import Utils.Containers.Internal.StrictPair
import Utils.Containers.Internal.StrictMaybe
import Utils.Containers.Internal.BitQueue
#ifdef DEFINE_ALTERF_FALLBACK
import Utils.Containers.Internal.BitUtil (wordSize)
#endif

#if __GLASGOW_HASKELL__
import GHC.Exts (build, lazy)
#if !MIN_VERSION_base(4,8,0)
import Data.Functor ((<$))
#endif
#ifdef USE_MAGIC_PROXY
import GHC.Exts (Proxy#, proxy# )
#endif
#if __GLASGOW_HASKELL__ >= 708
import qualified GHC.Exts as GHCExts
#endif
import Text.Read hiding (lift)
import Data.Data
import qualified Control.Category as Category
#endif
#if __GLASGOW_HASKELL__ >= 708
import Data.Coerce
#endif


{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}
infixl 9 !,!?,\\ --

-- | /O(log n)/. Find the value at a key.
-- Calls 'error' when the element can not be found.
--
-- > fromList [(5,'a'), (3,'b')] ! 1    Error: element not in the map
-- > fromList [(5,'a'), (3,'b')] ! 5 == 'a'

(!) :: Ord k => Map k a -> k -> a
(!) m k = find k m
#if __GLASGOW_HASKELL__
{-# INLINE (!) #-}
#endif

-- | /O(log n)/. Find the value at a key.
-- Returns 'Nothing' when the element can not be found.
--
-- prop> fromList [(5, 'a'), (3, 'b')] !? 1 == Nothing
-- prop> fromList [(5, 'a'), (3, 'b')] !? 5 == Just 'a'

(!?) :: Ord k => Map k a -> k -> Maybe a
(!?) m k = lookup k m
#if __GLASGOW_HASKELL__
{-# INLINE (!?) #-}
#endif

-- | Same as 'difference'.
(\\) :: Ord k => Map k a -> Map k b -> Map k a
m1 \\ m2 = difference m1 m2
#if __GLASGOW_HASKELL__
{-# INLINE (\\) #-}
#endif

{--------------------------------------------------------------------
  Size balanced trees.
--------------------------------------------------------------------}
-- | A Map from keys @k@ to values @a@.

-- See Note: Order of constructors
data Map k a  = Bin {-# UNPACK #-} !Size !k a !(Map k a) !(Map k a)
              | Tip

type Size     = Int

#if __GLASGOW_HASKELL__ >= 708
type role Map nominal representational
#endif

instance (Ord k) => Monoid (Map k v) where
    mempty  = empty
    mconcat = unions
#if !(MIN_VERSION_base(4,9,0))
    mappend = union
#else
    mappend = (<>)

instance (Ord k) => Semigroup (Map k v) where
    (<>)    = union
    stimes  = stimesIdempotentMonoid
#endif

#if __GLASGOW_HASKELL__

{--------------------------------------------------------------------
  A Data instance
--------------------------------------------------------------------}

-- This instance preserves data abstraction at the cost of inefficiency.
-- We provide limited reflection services for the sake of data abstraction.

instance (Data k, Data a, Ord k) => Data (Map k a) where
  gfoldl f z m   = z fromList `f` toList m
  toConstr _     = fromListConstr
  gunfold k z c  = case constrIndex c of
    1 -> k (z fromList)
    _ -> error "gunfold"
  dataTypeOf _   = mapDataType
  dataCast2 f    = gcast2 f

fromListConstr :: Constr
fromListConstr = mkConstr mapDataType "fromList" [] Prefix

mapDataType :: DataType
mapDataType = mkDataType "Data.Map.Internal.Map" [fromListConstr]

#endif

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}
-- | /O(1)/. Is the map empty?
--
-- > Data.Map.null (empty)           == True
-- > Data.Map.null (singleton 1 'a') == False

null :: Map k a -> Bool
null Tip      = True
null (Bin {}) = False
{-# INLINE null #-}

-- | /O(1)/. The number of elements in the map.
--
-- > size empty                                   == 0
-- > size (singleton 1 'a')                       == 1
-- > size (fromList([(1,'a'), (2,'c'), (3,'b')])) == 3

size :: Map k a -> Int
size Tip              = 0
size (Bin sz _ _ _ _) = sz
{-# INLINE size #-}


-- | /O(log n)/. Lookup the value at a key in the map.
--
-- The function will return the corresponding value as @('Just' value)@,
-- or 'Nothing' if the key isn't in the map.
--
-- An example of using @lookup@:
--
-- > import Prelude hiding (lookup)
-- > import Data.Map
-- >
-- > employeeDept = fromList([("John","Sales"), ("Bob","IT")])
-- > deptCountry = fromList([("IT","USA"), ("Sales","France")])
-- > countryCurrency = fromList([("USA", "Dollar"), ("France", "Euro")])
-- >
-- > employeeCurrency :: String -> Maybe String
-- > employeeCurrency name = do
-- >     dept <- lookup name employeeDept
-- >     country <- lookup dept deptCountry
-- >     lookup country countryCurrency
-- >
-- > main = do
-- >     putStrLn $ "John's currency: " ++ (show (employeeCurrency "John"))
-- >     putStrLn $ "Pete's currency: " ++ (show (employeeCurrency "Pete"))
--
-- The output of this program:
--
-- >   John's currency: Just "Euro"
-- >   Pete's currency: Nothing
lookup :: Ord k => k -> Map k a -> Maybe a
lookup = go
  where
    go !_ Tip = Nothing
    go k (Bin _ kx x l r) = case compare k kx of
      LT -> go k l
      GT -> go k r
      EQ -> Just x
#if __GLASGOW_HASKELL__
{-# INLINABLE lookup #-}
#else
{-# INLINE lookup #-}
#endif

-- | /O(log n)/. Is the key a member of the map? See also 'notMember'.
--
-- > member 5 (fromList [(5,'a'), (3,'b')]) == True
-- > member 1 (fromList [(5,'a'), (3,'b')]) == False
member :: Ord k => k -> Map k a -> Bool
member = go
  where
    go !_ Tip = False
    go k (Bin _ kx _ l r) = case compare k kx of
      LT -> go k l
      GT -> go k r
      EQ -> True
#if __GLASGOW_HASKELL__
{-# INLINABLE member #-}
#else
{-# INLINE member #-}
#endif

-- | /O(log n)/. Is the key not a member of the map? See also 'member'.
--
-- > notMember 5 (fromList [(5,'a'), (3,'b')]) == False
-- > notMember 1 (fromList [(5,'a'), (3,'b')]) == True

notMember :: Ord k => k -> Map k a -> Bool
notMember k m = not $ member k m
#if __GLASGOW_HASKELL__
{-# INLINABLE notMember #-}
#else
{-# INLINE notMember #-}
#endif

-- | /O(log n)/. Find the value at a key.
-- Calls 'error' when the element can not be found.
find :: Ord k => k -> Map k a -> a
find = go
  where
    go !_ Tip = error "Map.!: given key is not an element in the map"
    go k (Bin _ kx x l r) = case compare k kx of
      LT -> go k l
      GT -> go k r
      EQ -> x
#if __GLASGOW_HASKELL__
{-# INLINABLE find #-}
#else
{-# INLINE find #-}
#endif

-- | /O(log n)/. The expression @('findWithDefault' def k map)@ returns
-- the value at key @k@ or returns default value @def@
-- when the key is not in the map.
--
-- > findWithDefault 'x' 1 (fromList [(5,'a'), (3,'b')]) == 'x'
-- > findWithDefault 'x' 5 (fromList [(5,'a'), (3,'b')]) == 'a'
findWithDefault :: Ord k => a -> k -> Map k a -> a
findWithDefault = go
  where
    go def !_ Tip = def
    go def k (Bin _ kx x l r) = case compare k kx of
      LT -> go def k l
      GT -> go def k r
      EQ -> x
#if __GLASGOW_HASKELL__
{-# INLINABLE findWithDefault #-}
#else
{-# INLINE findWithDefault #-}
#endif

-- | /O(log n)/. Find largest key smaller than the given one and return the
-- corresponding (key, value) pair.
--
-- > lookupLT 3 (fromList [(3,'a'), (5,'b')]) == Nothing
-- > lookupLT 4 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')
lookupLT :: Ord k => k -> Map k v -> Maybe (k, v)
lookupLT = goNothing
  where
    goNothing !_ Tip = Nothing
    goNothing k (Bin _ kx x l r) | k <= kx = goNothing k l
                                 | otherwise = goJust k kx x r

    goJust !_ kx' x' Tip = Just (kx', x')
    goJust k kx' x' (Bin _ kx x l r) | k <= kx = goJust k kx' x' l
                                     | otherwise = goJust k kx x r
#if __GLASGOW_HASKELL__
{-# INLINABLE lookupLT #-}
#else
{-# INLINE lookupLT #-}
#endif

-- | /O(log n)/. Find smallest key greater than the given one and return the
-- corresponding (key, value) pair.
--
-- > lookupGT 4 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')
-- > lookupGT 5 (fromList [(3,'a'), (5,'b')]) == Nothing
lookupGT :: Ord k => k -> Map k v -> Maybe (k, v)
lookupGT = goNothing
  where
    goNothing !_ Tip = Nothing
    goNothing k (Bin _ kx x l r) | k < kx = goJust k kx x l
                                 | otherwise = goNothing k r

    goJust !_ kx' x' Tip = Just (kx', x')
    goJust k kx' x' (Bin _ kx x l r) | k < kx = goJust k kx x l
                                     | otherwise = goJust k kx' x' r
#if __GLASGOW_HASKELL__
{-# INLINABLE lookupGT #-}
#else
{-# INLINE lookupGT #-}
#endif

-- | /O(log n)/. Find largest key smaller or equal to the given one and return
-- the corresponding (key, value) pair.
--
-- > lookupLE 2 (fromList [(3,'a'), (5,'b')]) == Nothing
-- > lookupLE 4 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')
-- > lookupLE 5 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')
lookupLE :: Ord k => k -> Map k v -> Maybe (k, v)
lookupLE = goNothing
  where
    goNothing !_ Tip = Nothing
    goNothing k (Bin _ kx x l r) = case compare k kx of LT -> goNothing k l
                                                        EQ -> Just (kx, x)
                                                        GT -> goJust k kx x r

    goJust !_ kx' x' Tip = Just (kx', x')
    goJust k kx' x' (Bin _ kx x l r) = case compare k kx of LT -> goJust k kx' x' l
                                                            EQ -> Just (kx, x)
                                                            GT -> goJust k kx x r
#if __GLASGOW_HASKELL__
{-# INLINABLE lookupLE #-}
#else
{-# INLINE lookupLE #-}
#endif

-- | /O(log n)/. Find smallest key greater or equal to the given one and return
-- the corresponding (key, value) pair.
--
-- > lookupGE 3 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')
-- > lookupGE 4 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')
-- > lookupGE 6 (fromList [(3,'a'), (5,'b')]) == Nothing
lookupGE :: Ord k => k -> Map k v -> Maybe (k, v)
lookupGE = goNothing
  where
    goNothing !_ Tip = Nothing
    goNothing k (Bin _ kx x l r) = case compare k kx of LT -> goJust k kx x l
                                                        EQ -> Just (kx, x)
                                                        GT -> goNothing k r

    goJust !_ kx' x' Tip = Just (kx', x')
    goJust k kx' x' (Bin _ kx x l r) = case compare k kx of LT -> goJust k kx x l
                                                            EQ -> Just (kx, x)
                                                            GT -> goJust k kx' x' r
#if __GLASGOW_HASKELL__
{-# INLINABLE lookupGE #-}
#else
{-# INLINE lookupGE #-}
#endif

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
-- | /O(1)/. The empty map.
--
-- > empty      == fromList []
-- > size empty == 0

empty :: Map k a
empty = Tip
{-# INLINE empty #-}

-- | /O(1)/. A map with a single element.
--
-- > singleton 1 'a'        == fromList [(1, 'a')]
-- > size (singleton 1 'a') == 1

singleton :: k -> a -> Map k a
singleton k x = Bin 1 k x Tip Tip
{-# INLINE singleton #-}

{--------------------------------------------------------------------
  Insertion
--------------------------------------------------------------------}
-- | /O(log n)/. Insert a new key and value in the map.
-- If the key is already present in the map, the associated value is
-- replaced with the supplied value. 'insert' is equivalent to
-- @'insertWith' 'const'@.
--
-- > insert 5 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'x')]
-- > insert 7 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'a'), (7, 'x')]
-- > insert 5 'x' empty                         == singleton 5 'x'

-- See Note: Type of local 'go' function
-- See Note: Avoiding worker/wrapper
insert :: Ord k => k -> a -> Map k a -> Map k a
insert kx0 = go kx0 kx0
  where
    -- Unlike insertR, we only get sharing here
    -- when the inserted value is at the same address
    -- as the present value. We try anyway; this condition
    -- seems particularly likely to occur in 'union'.
    go :: Ord k => k -> k -> a -> Map k a -> Map k a
    go orig !_  x Tip = singleton (lazy orig) x
    go orig !kx x t@(Bin sz ky y l r) =
        case compare kx ky of
            LT | l' `ptrEq` l -> t
               | otherwise -> balanceL ky y l' r
               where !l' = go orig kx x l
            GT | r' `ptrEq` r -> t
               | otherwise -> balanceR ky y l r'
               where !r' = go orig kx x r
            EQ | x `ptrEq` y && (lazy orig `seq` (orig `ptrEq` ky)) -> t
               | otherwise -> Bin sz (lazy orig) x l r
#if __GLASGOW_HASKELL__
{-# INLINABLE insert #-}
#else
{-# INLINE insert #-}
#endif

#ifndef __GLASGOW_HASKELL__
lazy :: a -> a
lazy a = a
#endif

-- [Note: Avoiding worker/wrapper]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 'insert' has to go to great lengths to get pointer equality right and
-- to prevent unnecessary allocation. The trouble is that GHC *really* wants
-- to unbox the key and throw away the boxed one. This is bad for us, because
-- we want to compare the pointer of the box we are given to the one already
-- present if they compare EQ. It's also bad for us because it leads to the
-- key being *reboxed* if it's actually stored in the map. Ugh! So we pass the
-- 'go' function *two copies* of the key we're given. One of them we use for
-- comparisons; the other we keep in our pocket. To prevent worker/wrapper from
-- messing with the copy in our pocket, we sprinkle about calls to the magical
-- function 'lazy'. This is all horrible, but it seems to work okay.


-- Insert a new key and value in the map if it is not already present.
-- Used by `union`.

-- See Note: Type of local 'go' function
-- See Note: Avoiding worker/wrapper
insertR :: Ord k => k -> a -> Map k a -> Map k a
insertR kx0 = go kx0 kx0
  where
    go :: Ord k => k -> k -> a -> Map k a -> Map k a
    go orig !_  x Tip = singleton (lazy orig) x
    go orig !kx x t@(Bin _ ky y l r) =
        case compare kx ky of
            LT | l' `ptrEq` l -> t
               | otherwise -> balanceL ky y l' r
               where !l' = go orig kx x l
            GT | r' `ptrEq` r -> t
               | otherwise -> balanceR ky y l r'
               where !r' = go orig kx x r
            EQ -> t
#if __GLASGOW_HASKELL__
{-# INLINABLE insertR #-}
#else
{-# INLINE insertR #-}
#endif

-- | /O(log n)/. Insert with a function, combining new value and old value.
-- @'insertWith' f key value mp@
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert the pair @(key, f new_value old_value)@.
--
-- > insertWith (++) 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "xxxa")]
-- > insertWith (++) 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
-- > insertWith (++) 5 "xxx" empty                         == singleton 5 "xxx"

insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith = go
  where
    -- We have no hope of making pointer equality tricks work
    -- here, because lazy insertWith *always* changes the tree,
    -- either adding a new entry or replacing an element with a
    -- thunk.
    go :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
    go _ !kx x Tip = singleton kx x
    go f !kx x (Bin sy ky y l r) =
        case compare kx ky of
            LT -> balanceL ky y (go f kx x l) r
            GT -> balanceR ky y l (go f kx x r)
            EQ -> Bin sy kx (f x y) l r

#if __GLASGOW_HASKELL__
{-# INLINABLE insertWith #-}
#else
{-# INLINE insertWith #-}
#endif

-- | A helper function for 'unionWith'. When the key is already in
-- the map, the key is left alone, not replaced. The combining
-- function is flipped--it is applied to the old value and then the
-- new value.

insertWithR :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWithR = go
  where
    go :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
    go _ !kx x Tip = singleton kx x
    go f !kx x (Bin sy ky y l r) =
        case compare kx ky of
            LT -> balanceL ky y (go f kx x l) r
            GT -> balanceR ky y l (go f kx x r)
            EQ -> Bin sy ky (f y x) l r
#if __GLASGOW_HASKELL__
{-# INLINABLE insertWithR #-}
#else
{-# INLINE insertWithR #-}
#endif

-- | /O(log n)/. Insert with a function, combining key, new value and old value.
-- @'insertWithKey' f key value mp@
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert the pair @(key,f key new_value old_value)@.
-- Note that the key passed to f is the same key passed to 'insertWithKey'.
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > insertWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:xxx|a")]
-- > insertWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
-- > insertWithKey f 5 "xxx" empty                         == singleton 5 "xxx"

-- See Note: Type of local 'go' function
insertWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWithKey = go
  where
    go :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
    go _ !kx x Tip = singleton kx x
    go f kx x (Bin sy ky y l r) =
        case compare kx ky of
            LT -> balanceL ky y (go f kx x l) r
            GT -> balanceR ky y l (go f kx x r)
            EQ -> Bin sy kx (f kx x y) l r
#if __GLASGOW_HASKELL__
{-# INLINABLE insertWithKey #-}
#else
{-# INLINE insertWithKey #-}
#endif

-- | A helper function for 'unionWithKey'. When the key is already in
-- the map, the key is left alone, not replaced. The combining
-- function is flipped--it is applied to the old value and then the
-- new value.
insertWithKeyR :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWithKeyR = go
  where
    go :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
    go _ !kx x Tip = singleton kx x
    go f kx x (Bin sy ky y l r) =
        case compare kx ky of
            LT -> balanceL ky y (go f kx x l) r
            GT -> balanceR ky y l (go f kx x r)
            EQ -> Bin sy ky (f ky y x) l r
#if __GLASGOW_HASKELL__
{-# INLINABLE insertWithKeyR #-}
#else
{-# INLINE insertWithKeyR #-}
#endif

-- | /O(log n)/. Combines insert operation with old value retrieval.
-- The expression (@'insertLookupWithKey' f k x map@)
-- is a pair where the first element is equal to (@'lookup' k map@)
-- and the second element equal to (@'insertWithKey' f k x map@).
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > insertLookupWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "5:xxx|a")])
-- > insertLookupWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a"), (7, "xxx")])
-- > insertLookupWithKey f 5 "xxx" empty                         == (Nothing,  singleton 5 "xxx")
--
-- This is how to define @insertLookup@ using @insertLookupWithKey@:
--
-- > let insertLookup kx x t = insertLookupWithKey (\_ a _ -> a) kx x t
-- > insertLookup 5 "x" (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "x")])
-- > insertLookup 7 "x" (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a"), (7, "x")])

-- See Note: Type of local 'go' function
insertLookupWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a
                    -> (Maybe a, Map k a)
insertLookupWithKey f0 k0 x0 = toPair . go f0 k0 x0
  where
    go :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> StrictPair (Maybe a) (Map k a)
    go _ !kx x Tip = (Nothing :*: singleton kx x)
    go f kx x (Bin sy ky y l r) =
        case compare kx ky of
            LT -> let !(found :*: l') = go f kx x l
                      !t' = balanceL ky y l' r
                  in (found :*: t')
            GT -> let !(found :*: r') = go f kx x r
                      !t' = balanceR ky y l r'
                  in (found :*: t')
            EQ -> (Just y :*: Bin sy kx (f kx x y) l r)
#if __GLASGOW_HASKELL__
{-# INLINABLE insertLookupWithKey #-}
#else
{-# INLINE insertLookupWithKey #-}
#endif

{--------------------------------------------------------------------
  Deletion
--------------------------------------------------------------------}
-- | /O(log n)/. Delete a key and its value from the map. When the key is not
-- a member of the map, the original map is returned.
--
-- > delete 5 (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > delete 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > delete 5 empty                         == empty

-- See Note: Type of local 'go' function
delete :: Ord k => k -> Map k a -> Map k a
delete = go
  where
    go :: Ord k => k -> Map k a -> Map k a
    go !_ Tip = Tip
    go k t@(Bin _ kx x l r) =
        case compare k kx of
            LT | l' `ptrEq` l -> t
               | otherwise -> balanceR kx x l' r
               where !l' = go k l
            GT | r' `ptrEq` r -> t
               | otherwise -> balanceL kx x l r'
               where !r' = go k r
            EQ -> glue l r
#if __GLASGOW_HASKELL__
{-# INLINABLE delete #-}
#else
{-# INLINE delete #-}
#endif

-- | /O(log n)/. Update a value at a specific key with the result of the provided function.
-- When the key is not
-- a member of the map, the original map is returned.
--
-- > adjust ("new " ++) 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- > adjust ("new " ++) 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > adjust ("new " ++) 7 empty                         == empty

adjust :: Ord k => (a -> a) -> k -> Map k a -> Map k a
adjust f = adjustWithKey (\_ x -> f x)
#if __GLASGOW_HASKELL__
{-# INLINABLE adjust #-}
#else
{-# INLINE adjust #-}
#endif

-- | /O(log n)/. Adjust a value at a specific key. When the key is not
-- a member of the map, the original map is returned.
--
-- > let f key x = (show key) ++ ":new " ++ x
-- > adjustWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
-- > adjustWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > adjustWithKey f 7 empty                         == empty

adjustWithKey :: Ord k => (k -> a -> a) -> k -> Map k a -> Map k a
adjustWithKey = go
  where
    go :: Ord k => (k -> a -> a) -> k -> Map k a -> Map k a
    go _ !_ Tip = Tip
    go f k (Bin sx kx x l r) =
        case compare k kx of
           LT -> Bin sx kx x (go f k l) r
           GT -> Bin sx kx x l (go f k r)
           EQ -> Bin sx kx (f kx x) l r
#if __GLASGOW_HASKELL__
{-# INLINABLE adjustWithKey #-}
#else
{-# INLINE adjustWithKey #-}
#endif

-- | /O(log n)/. The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > update f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- > update f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > update f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

update :: Ord k => (a -> Maybe a) -> k -> Map k a -> Map k a
update f = updateWithKey (\_ x -> f x)
#if __GLASGOW_HASKELL__
{-# INLINABLE update #-}
#else
{-# INLINE update #-}
#endif

-- | /O(log n)/. The expression (@'updateWithKey' f k map@) updates the
-- value @x@ at @k@ (if it is in the map). If (@f k x@) is 'Nothing',
-- the element is deleted. If it is (@'Just' y@), the key @k@ is bound
-- to the new value @y@.
--
-- > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- > updateWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
-- > updateWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > updateWithKey f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

-- See Note: Type of local 'go' function
updateWithKey :: Ord k => (k -> a -> Maybe a) -> k -> Map k a -> Map k a
updateWithKey = go
  where
    go :: Ord k => (k -> a -> Maybe a) -> k -> Map k a -> Map k a
    go _ !_ Tip = Tip
    go f k(Bin sx kx x l r) =
        case compare k kx of
           LT -> balanceR kx x (go f k l) r
           GT -> balanceL kx x l (go f k r)
           EQ -> case f kx x of
                   Just x' -> Bin sx kx x' l r
                   Nothing -> glue l r
#if __GLASGOW_HASKELL__
{-# INLINABLE updateWithKey #-}
#else
{-# INLINE updateWithKey #-}
#endif

-- | /O(log n)/. Lookup and update. See also 'updateWithKey'.
-- The function returns changed value, if it is updated.
-- Returns the original key value if the map entry is deleted.
--
-- > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- > updateLookupWithKey f 5 (fromList [(5,"a"), (3,"b")]) == (Just "5:new a", fromList [(3, "b"), (5, "5:new a")])
-- > updateLookupWithKey f 7 (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a")])
-- > updateLookupWithKey f 3 (fromList [(5,"a"), (3,"b")]) == (Just "b", singleton 5 "a")

-- See Note: Type of local 'go' function
updateLookupWithKey :: Ord k => (k -> a -> Maybe a) -> k -> Map k a -> (Maybe a,Map k a)
updateLookupWithKey f0 k0 = toPair . go f0 k0
 where
   go :: Ord k => (k -> a -> Maybe a) -> k -> Map k a -> StrictPair (Maybe a) (Map k a)
   go _ !_ Tip = (Nothing :*: Tip)
   go f k (Bin sx kx x l r) =
          case compare k kx of
               LT -> let !(found :*: l') = go f k l
                         !t' = balanceR kx x l' r
                     in (found :*: t')
               GT -> let !(found :*: r') = go f k r
                         !t' = balanceL kx x l r'
                     in (found :*: t')
               EQ -> case f kx x of
                       Just x' -> (Just x' :*: Bin sx kx x' l r)
                       Nothing -> let !glued = glue l r
                                  in (Just x :*: glued)
#if __GLASGOW_HASKELL__
{-# INLINABLE updateLookupWithKey #-}
#else
{-# INLINE updateLookupWithKey #-}
#endif

-- | /O(log n)/. The expression (@'alter' f k map@) alters the value @x@ at @k@, or absence thereof.
-- 'alter' can be used to insert, delete, or update a value in a 'Map'.
-- In short : @'lookup' k ('alter' f k m) = f ('lookup' k m)@.
--
-- > let f _ = Nothing
-- > alter f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > alter f 5 (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- >
-- > let f _ = Just "c"
-- > alter f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "c")]
-- > alter f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "c")]

-- See Note: Type of local 'go' function
alter :: Ord k => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
alter = go
  where
    go :: Ord k => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
    go f !k Tip = case f Nothing of
               Nothing -> Tip
               Just x  -> singleton k x

    go f k (Bin sx kx x l r) = case compare k kx of
               LT -> balance kx x (go f k l) r
               GT -> balance kx x l (go f k r)
               EQ -> case f (Just x) of
                       Just x' -> Bin sx kx x' l r
                       Nothing -> glue l r
#if __GLASGOW_HASKELL__
{-# INLINABLE alter #-}
#else
{-# INLINE alter #-}
#endif

-- Used to choose the appropriate alterF implementation.
data AreWeStrict = Strict | Lazy

-- | /O(log n)/. The expression (@'alterF' f k map@) alters the value @x@ at
-- @k@, or absence thereof.  'alterF' can be used to inspect, insert, delete,
-- or update a value in a 'Map'.  In short: @'lookup' k \<$\> 'alterF' f k m = f
-- ('lookup' k m)@.
--
-- Example:
--
-- @
-- interactiveAlter :: Int -> Map Int String -> IO (Map Int String)
-- interactiveAlter k m = alterF f k m where
--   f Nothing -> do
--      putStrLn $ show k ++
--          " was not found in the map. Would you like to add it?"
--      getUserResponse1 :: IO (Maybe String)
--   f (Just old) -> do
--      putStrLn "The key is currently bound to " ++ show old ++
--          ". Would you like to change or delete it?"
--      getUserresponse2 :: IO (Maybe String)
-- @
--
-- 'alterF' is the most general operation for working with an individual
-- key that may or may not be in a given map. When used with trivial
-- functors like 'Identity' and 'Const', it is often slightly slower than
-- more specialized combinators like 'lookup' and 'insert'. However, when
-- the functor is non-trivial and key comparison is not particularly cheap,
-- it is the fastest way.
--
-- Note on rewrite rules:
--
-- This module includes GHC rewrite rules to optimize 'alterF' for
-- the 'Const' and 'Identity' functors. In general, these rules
-- improve performance. The sole exception is that when using
-- 'Identity', deleting a key that is already absent takes longer
-- than it would without the rules. If you expect this to occur
-- a very large fraction of the time, you might consider using a
-- private copy of the 'Identity' type.
--
-- Note: 'alterF' is a flipped version of the 'at' combinator from
-- 'Control.Lens.At'.
--
-- @since 0.5.8
alterF :: (Functor f, Ord k)
       => (Maybe a -> f (Maybe a)) -> k -> Map k a -> f (Map k a)
alterF f k m = atKeyImpl Lazy k f m

#ifndef __GLASGOW_HASKELL__
{-# INLINE alterF #-}
#else
{-# INLINABLE [2] alterF #-}

-- We can save a little time by recognizing the special case of
-- `Control.Applicative.Const` and just doing a lookup.
{-# RULES
"alterF/Const" forall k (f :: Maybe a -> Const b (Maybe a)) . alterF f k = \m -> Const . getConst . f $ lookup k m
 #-}

#if MIN_VERSION_base(4,8,0)
-- base 4.8 and above include Data.Functor.Identity, so we can
-- save a pretty decent amount of time by handling it specially.
{-# RULES
"alterF/Identity" forall k f . alterF f k = atKeyIdentity k f
 #-}
#endif
#endif

atKeyImpl :: (Functor f, Ord k) =>
      AreWeStrict -> k -> (Maybe a -> f (Maybe a)) -> Map k a -> f (Map k a)
#ifdef DEFINE_ALTERF_FALLBACK
atKeyImpl strict !k f m
-- It doesn't seem sensible to worry about overflowing the queue
-- if the word size is 61 or more. If I calculate it correctly,
-- that would take a map with nearly a quadrillion entries.
  | wordSize < 61 && size m >= alterFCutoff = alterFFallback strict k f m
#endif
atKeyImpl strict !k f m = case lookupTrace k m of
  TraceResult mv q -> (<$> f mv) $ \ fres ->
    case fres of
      Nothing -> case mv of
                   Nothing -> m
                   Just old -> deleteAlong old q m
      Just new -> case strict of
         Strict -> new `seq` case mv of
                      Nothing -> insertAlong q k new m
                      Just _ -> replaceAlong q new m
         Lazy -> case mv of
                      Nothing -> insertAlong q k new m
                      Just _ -> replaceAlong q new m

{-# INLINE atKeyImpl #-}

#ifdef DEFINE_ALTERF_FALLBACK
alterFCutoff :: Int
#if WORD_SIZE_IN_BITS == 32
alterFCutoff = 55744454
#else
alterFCutoff = case wordSize of
      30 -> 17637893
      31 -> 31356255
      32 -> 55744454
      x -> (4^(x*2-2)) `quot` (3^(x*2-2))  -- Unlikely
#endif
#endif

data TraceResult a = TraceResult (Maybe a) {-# UNPACK #-} !BitQueue

-- Look up a key and return a result indicating whether it was found
-- and what path was taken.
lookupTrace :: Ord k => k -> Map k a -> TraceResult a
lookupTrace = go emptyQB
  where
    go :: Ord k => BitQueueB -> k -> Map k a -> TraceResult a
    go !q !_ Tip = TraceResult Nothing (buildQ q)
    go q k (Bin _ kx x l r) = case compare k kx of
      LT -> (go $! q `snocQB` False) k l
      GT -> (go $! q `snocQB` True) k r
      EQ -> TraceResult (Just x) (buildQ q)

-- GHC 7.8 doesn't manage to unbox the queue properly
-- unless we explicitly inline this function. This stuff
-- is a bit touchy, unfortunately.
#if __GLASGOW_HASKELL__ >= 710
{-# INLINABLE lookupTrace #-}
#else
{-# INLINE lookupTrace #-}
#endif

-- Insert at a location (which will always be a leaf)
-- described by the path passed in.
insertAlong :: BitQueue -> k -> a -> Map k a -> Map k a
insertAlong !_ kx x Tip = singleton kx x
insertAlong q kx x (Bin sz ky y l r) =
  case unconsQ q of
        Just (False, tl) -> balanceL ky y (insertAlong tl kx x l) r
        Just (True,tl) -> balanceR ky y l (insertAlong tl kx x r)
        Nothing -> Bin sz kx x l r  -- Shouldn't happen

-- Delete from a location (which will always be a node)
-- described by the path passed in.
--
-- This is fairly horrifying! We don't actually have any
-- use for the old value we're deleting. But if GHC sees
-- that, then it will allocate a thunk representing the
-- Map with the key deleted before we have any reason to
-- believe we'll actually want that. This transformation
-- enhances sharing, but we don't care enough about that.
-- So deleteAlong needs to take the old value, and we need
-- to convince GHC somehow that it actually uses it. We
-- can't NOINLINE deleteAlong, because that would prevent
-- the BitQueue from being unboxed. So instead we pass the
-- old value to a NOINLINE constant function and then
-- convince GHC that we use the result throughout the
-- computation. Doing the obvious thing and just passing
-- the value itself through the recursion costs 3-4% time,
-- so instead we convert the value to a magical zero-width
-- proxy that's ultimately erased.
deleteAlong :: any -> BitQueue -> Map k a -> Map k a
deleteAlong old !q0 !m = go (bogus old) q0 m where
#ifdef USE_MAGIC_PROXY
  go :: Proxy# () -> BitQueue -> Map k a -> Map k a
#else
  go :: any -> BitQueue -> Map k a -> Map k a
#endif
  go !_ !_ Tip = Tip
  go foom q (Bin _ ky y l r) =
      case unconsQ q of
        Just (False, tl) -> balanceR ky y (go foom tl l) r
        Just (True, tl) -> balanceL ky y l (go foom tl r)
        Nothing -> glue l r

#ifdef USE_MAGIC_PROXY
{-# NOINLINE bogus #-}
bogus :: a -> Proxy# ()
bogus _ = proxy#
#else
-- No point hiding in this case.
{-# INLINE bogus #-}
bogus :: a -> a
bogus a = a
#endif

-- Replace the value found in the node described
-- by the given path with a new one.
replaceAlong :: BitQueue -> a -> Map k a -> Map k a
replaceAlong !_ _ Tip = Tip -- Should not happen
replaceAlong q  x (Bin sz ky y l r) =
      case unconsQ q of
        Just (False, tl) -> Bin sz ky y (replaceAlong tl x l) r
        Just (True,tl) -> Bin sz ky y l (replaceAlong tl x r)
        Nothing -> Bin sz ky x l r

#if __GLASGOW_HASKELL__ && MIN_VERSION_base(4,8,0)
atKeyIdentity :: Ord k => k -> (Maybe a -> Identity (Maybe a)) -> Map k a -> Identity (Map k a)
atKeyIdentity k f t = Identity $ atKeyPlain Lazy k (coerce f) t
{-# INLINABLE atKeyIdentity #-}

atKeyPlain :: Ord k => AreWeStrict -> k -> (Maybe a -> Maybe a) -> Map k a -> Map k a
atKeyPlain strict k0 f0 t = case go k0 f0 t of
    AltSmaller t' -> t'
    AltBigger t' -> t'
    AltAdj t' -> t'
    AltSame -> t
  where
    go :: Ord k => k -> (Maybe a -> Maybe a) -> Map k a -> Altered k a
    go !k f Tip = case f Nothing of
                   Nothing -> AltSame
                   Just x  -> case strict of
                     Lazy -> AltBigger $ singleton k x
                     Strict -> x `seq` (AltBigger $ singleton k x)

    go k f (Bin sx kx x l r) = case compare k kx of
                   LT -> case go k f l of
                           AltSmaller l' -> AltSmaller $ balanceR kx x l' r
                           AltBigger l' -> AltBigger $ balanceL kx x l' r
                           AltAdj l' -> AltAdj $ Bin sx kx x l' r
                           AltSame -> AltSame
                   GT -> case go k f r of
                           AltSmaller r' -> AltSmaller $ balanceL kx x l r'
                           AltBigger r' -> AltBigger $ balanceR kx x l r'
                           AltAdj r' -> AltAdj $ Bin sx kx x l r'
                           AltSame -> AltSame
                   EQ -> case f (Just x) of
                           Just x' -> case strict of
                             Lazy -> AltAdj $ Bin sx kx x' l r
                             Strict -> x' `seq` (AltAdj $ Bin sx kx x' l r)
                           Nothing -> AltSmaller $ glue l r
{-# INLINE atKeyPlain #-}

data Altered k a = AltSmaller !(Map k a) | AltBigger !(Map k a) | AltAdj !(Map k a) | AltSame
#endif

#ifdef DEFINE_ALTERF_FALLBACK
-- When the map is too large to use a bit queue, we fall back to
-- this much slower version which uses a more "natural" implementation
-- improved with Yoneda to avoid repeated fmaps. This works okayish for
-- some operations, but it's pretty lousy for lookups.
alterFFallback :: (Functor f, Ord k)
   => AreWeStrict -> k -> (Maybe a -> f (Maybe a)) -> Map k a -> f (Map k a)
alterFFallback Lazy k f t = alterFYoneda k (\m q -> q <$> f m) t id
alterFFallback Strict k f t = alterFYoneda k (\m q -> q . forceMaybe <$> f m) t id
  where
    forceMaybe Nothing = Nothing
    forceMaybe may@(Just !_) = may
{-# NOINLINE alterFFallback #-}

alterFYoneda :: Ord k =>
      k -> (Maybe a -> (Maybe a -> b) -> f b) -> Map k a -> (Map k a -> b) -> f b
alterFYoneda = go
  where
    go :: Ord k =>
      k -> (Maybe a -> (Maybe a -> b) -> f b) -> Map k a -> (Map k a -> b) -> f b
    go !k f Tip g = f Nothing $ \ mx -> case mx of
      Nothing -> g Tip
      Just x -> g (singleton k x)
    go k f (Bin sx kx x l r) g = case compare k kx of
               LT -> go k f l (\m -> g (balance kx x m r))
               GT -> go k f r (\m -> g (balance kx x l m))
               EQ -> f (Just x) $ \ mx' -> case mx' of
                       Just x' -> g (Bin sx kx x' l r)
                       Nothing -> g (glue l r)
{-# INLINE alterFYoneda #-}
#endif

{--------------------------------------------------------------------
  Indexing
--------------------------------------------------------------------}
-- | /O(log n)/. Return the /index/ of a key, which is its zero-based index in
-- the sequence sorted by keys. The index is a number from /0/ up to, but not
-- including, the 'size' of the map. Calls 'error' when the key is not
-- a 'member' of the map.
--
-- > findIndex 2 (fromList [(5,"a"), (3,"b")])    Error: element is not in the map
-- > findIndex 3 (fromList [(5,"a"), (3,"b")]) == 0
-- > findIndex 5 (fromList [(5,"a"), (3,"b")]) == 1
-- > findIndex 6 (fromList [(5,"a"), (3,"b")])    Error: element is not in the map

-- See Note: Type of local 'go' function
findIndex :: Ord k => k -> Map k a -> Int
findIndex = go 0
  where
    go :: Ord k => Int -> k -> Map k a -> Int
    go !_   !_ Tip  = error "Map.findIndex: element is not in the map"
    go idx k (Bin _ kx _ l r) = case compare k kx of
      LT -> go idx k l
      GT -> go (idx + size l + 1) k r
      EQ -> idx + size l
#if __GLASGOW_HASKELL__
{-# INLINABLE findIndex #-}
#endif

-- | /O(log n)/. Lookup the /index/ of a key, which is its zero-based index in
-- the sequence sorted by keys. The index is a number from /0/ up to, but not
-- including, the 'size' of the map.
--
-- > isJust (lookupIndex 2 (fromList [(5,"a"), (3,"b")]))   == False
-- > fromJust (lookupIndex 3 (fromList [(5,"a"), (3,"b")])) == 0
-- > fromJust (lookupIndex 5 (fromList [(5,"a"), (3,"b")])) == 1
-- > isJust (lookupIndex 6 (fromList [(5,"a"), (3,"b")]))   == False

-- See Note: Type of local 'go' function
lookupIndex :: Ord k => k -> Map k a -> Maybe Int
lookupIndex = go 0
  where
    go :: Ord k => Int -> k -> Map k a -> Maybe Int
    go !_  !_ Tip  = Nothing
    go idx k (Bin _ kx _ l r) = case compare k kx of
      LT -> go idx k l
      GT -> go (idx + size l + 1) k r
      EQ -> Just $! idx + size l
#if __GLASGOW_HASKELL__
{-# INLINABLE lookupIndex #-}
#endif

-- | /O(log n)/. Retrieve an element by its /index/, i.e. by its zero-based
-- index in the sequence sorted by keys. If the /index/ is out of range (less
-- than zero, greater or equal to 'size' of the map), 'error' is called.
--
-- > elemAt 0 (fromList [(5,"a"), (3,"b")]) == (3,"b")
-- > elemAt 1 (fromList [(5,"a"), (3,"b")]) == (5, "a")
-- > elemAt 2 (fromList [(5,"a"), (3,"b")])    Error: index out of range

elemAt :: Int -> Map k a -> (k,a)
elemAt !_ Tip = error "Map.elemAt: index out of range"
elemAt i (Bin _ kx x l r)
  = case compare i sizeL of
      LT -> elemAt i l
      GT -> elemAt (i-sizeL-1) r
      EQ -> (kx,x)
  where
    sizeL = size l

-- | Take a given number of entries in key order, beginning
-- with the smallest keys.
--
-- @
-- take n = 'fromDistinctAscList' . 'Prelude.take' n . 'toAscList'
-- @

take :: Int -> Map k a -> Map k a
take i m | i >= size m = m
take i0 m0 = go i0 m0
  where
    go i !_ | i <= 0 = Tip
    go !_ Tip = Tip
    go i (Bin _ kx x l r) =
      case compare i sizeL of
        LT -> go i l
        GT -> link kx x l (go (i - sizeL - 1) r)
        EQ -> l
      where sizeL = size l

-- | Drop a given number of entries in key order, beginning
-- with the smallest keys.
--
-- @
-- drop n = 'fromDistinctAscList' . 'Prelude.drop' n . 'toAscList'
-- @
drop :: Int -> Map k a -> Map k a
drop i m | i >= size m = Tip
drop i0 m0 = go i0 m0
  where
    go i m | i <= 0 = m
    go !_ Tip = Tip
    go i (Bin _ kx x l r) =
      case compare i sizeL of
        LT -> link kx x (go i l) r
        GT -> go (i - sizeL - 1) r
        EQ -> insertMin kx x r
      where sizeL = size l

-- | /O(log n)/. Split a map at a particular index.
--
-- @
-- splitAt !n !xs = ('take' n xs, 'drop' n xs)
-- @
splitAt :: Int -> Map k a -> (Map k a, Map k a)
splitAt i0 m0
  | i0 >= size m0 = (m0, Tip)
  | otherwise = toPair $ go i0 m0
  where
    go i m | i <= 0 = Tip :*: m
    go !_ Tip = Tip :*: Tip
    go i (Bin _ kx x l r)
      = case compare i sizeL of
          LT -> case go i l of
                  ll :*: lr -> ll :*: link kx x lr r
          GT -> case go (i - sizeL - 1) r of
                  rl :*: rr -> link kx x l rl :*: rr
          EQ -> l :*: insertMin kx x r
      where sizeL = size l

-- | /O(log n)/. Update the element at /index/, i.e. by its zero-based index in
-- the sequence sorted by keys. If the /index/ is out of range (less than zero,
-- greater or equal to 'size' of the map), 'error' is called.
--
-- > updateAt (\ _ _ -> Just "x") 0    (fromList [(5,"a"), (3,"b")]) == fromList [(3, "x"), (5, "a")]
-- > updateAt (\ _ _ -> Just "x") 1    (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "x")]
-- > updateAt (\ _ _ -> Just "x") 2    (fromList [(5,"a"), (3,"b")])    Error: index out of range
-- > updateAt (\ _ _ -> Just "x") (-1) (fromList [(5,"a"), (3,"b")])    Error: index out of range
-- > updateAt (\_ _  -> Nothing)  0    (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
-- > updateAt (\_ _  -> Nothing)  1    (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > updateAt (\_ _  -> Nothing)  2    (fromList [(5,"a"), (3,"b")])    Error: index out of range
-- > updateAt (\_ _  -> Nothing)  (-1) (fromList [(5,"a"), (3,"b")])    Error: index out of range

updateAt :: (k -> a -> Maybe a) -> Int -> Map k a -> Map k a
updateAt f !i t =
  case t of
    Tip -> error "Map.updateAt: index out of range"
    Bin sx kx x l r -> case compare i sizeL of
      LT -> balanceR kx x (updateAt f i l) r
      GT -> balanceL kx x l (updateAt f (i-sizeL-1) r)
      EQ -> case f kx x of
              Just x' -> Bin sx kx x' l r
              Nothing -> glue l r
      where
        sizeL = size l

-- | /O(log n)/. Delete the element at /index/, i.e. by its zero-based index in
-- the sequence sorted by keys. If the /index/ is out of range (less than zero,
-- greater or equal to 'size' of the map), 'error' is called.
--
-- > deleteAt 0  (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"
-- > deleteAt 1  (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > deleteAt 2 (fromList [(5,"a"), (3,"b")])     Error: index out of range
-- > deleteAt (-1) (fromList [(5,"a"), (3,"b")])  Error: index out of range

deleteAt :: Int -> Map k a -> Map k a
deleteAt !i t =
  case t of
    Tip -> error "Map.deleteAt: index out of range"
    Bin _ kx x l r -> case compare i sizeL of
      LT -> balanceR kx x (deleteAt i l) r
      GT -> balanceL kx x l (deleteAt (i-sizeL-1) r)
      EQ -> glue l r
      where
        sizeL = size l


{--------------------------------------------------------------------
  Minimal, Maximal
--------------------------------------------------------------------}

lookupMinSure :: k -> a -> Map k a -> (k, a)
lookupMinSure k a Tip = (k, a)
lookupMinSure _ _ (Bin _ k a l _) = lookupMinSure k a l

-- | /O(log n)/. The minimal key of the map. Returns 'Nothing' if the map is empty.
--
-- > lookupMin (fromList [(5,"a"), (3,"b")]) == Just (3,"b")
-- > findMin empty = Nothing
--
-- @since 0.5.9

lookupMin :: Map k a -> Maybe (k,a)
lookupMin Tip = Nothing
lookupMin (Bin _ k x l _) = Just $! lookupMinSure k x l

-- | /O(log n)/. The minimal key of the map. Calls 'error' if the map is empty.
--
-- > findMin (fromList [(5,"a"), (3,"b")]) == (3,"b")
-- > findMin empty                            Error: empty map has no minimal element

findMin :: Map k a -> (k,a)
findMin t
  | Just r <- lookupMin t = r
  | otherwise = error "Map.findMin: empty map has no minimal element"

-- | /O(log n)/. The maximal key of the map. Calls 'error' if the map is empty.
--
-- > findMax (fromList [(5,"a"), (3,"b")]) == (5,"a")
-- > findMax empty                            Error: empty map has no maximal element

lookupMaxSure :: k -> a -> Map k a -> (k, a)
lookupMaxSure k a Tip = (k, a)
lookupMaxSure _ _ (Bin _ k a _ r) = lookupMaxSure k a r

-- | /O(log n)/. The maximal key of the map. Returns 'Nothing' if the map is empty.
--
-- > lookupMax (fromList [(5,"a"), (3,"b")]) == Just (5,"a")
-- > lookupMax empty = Nothing
--
-- @since 0.5.9

lookupMax :: Map k a -> Maybe (k, a)
lookupMax Tip = Nothing
lookupMax (Bin _ k x _ r) = Just $! lookupMaxSure k x r

findMax :: Map k a -> (k,a)
findMax t
  | Just r <- lookupMax t = r
  | otherwise = error "Map.findMax: empty map has no maximal element"

-- | /O(log n)/. Delete the minimal key. Returns an empty map if the map is empty.
--
-- > deleteMin (fromList [(5,"a"), (3,"b"), (7,"c")]) == fromList [(5,"a"), (7,"c")]
-- > deleteMin empty == empty

deleteMin :: Map k a -> Map k a
deleteMin (Bin _ _  _ Tip r)  = r
deleteMin (Bin _ kx x l r)    = balanceR kx x (deleteMin l) r
deleteMin Tip                 = Tip

-- | /O(log n)/. Delete the maximal key. Returns an empty map if the map is empty.
--
-- > deleteMax (fromList [(5,"a"), (3,"b"), (7,"c")]) == fromList [(3,"b"), (5,"a")]
-- > deleteMax empty == empty

deleteMax :: Map k a -> Map k a
deleteMax (Bin _ _  _ l Tip)  = l
deleteMax (Bin _ kx x l r)    = balanceL kx x l (deleteMax r)
deleteMax Tip                 = Tip

-- | /O(log n)/. Update the value at the minimal key.
--
-- > updateMin (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "Xb"), (5, "a")]
-- > updateMin (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

updateMin :: (a -> Maybe a) -> Map k a -> Map k a
updateMin f m
  = updateMinWithKey (\_ x -> f x) m

-- | /O(log n)/. Update the value at the maximal key.
--
-- > updateMax (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "Xa")]
-- > updateMax (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"

updateMax :: (a -> Maybe a) -> Map k a -> Map k a
updateMax f m
  = updateMaxWithKey (\_ x -> f x) m


-- | /O(log n)/. Update the value at the minimal key.
--
-- > updateMinWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"3:b"), (5,"a")]
-- > updateMinWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

updateMinWithKey :: (k -> a -> Maybe a) -> Map k a -> Map k a
updateMinWithKey _ Tip                 = Tip
updateMinWithKey f (Bin sx kx x Tip r) = case f kx x of
                                           Nothing -> r
                                           Just x' -> Bin sx kx x' Tip r
updateMinWithKey f (Bin _ kx x l r)    = balanceR kx x (updateMinWithKey f l) r

-- | /O(log n)/. Update the value at the maximal key.
--
-- > updateMaxWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"b"), (5,"5:a")]
-- > updateMaxWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"

updateMaxWithKey :: (k -> a -> Maybe a) -> Map k a -> Map k a
updateMaxWithKey _ Tip                 = Tip
updateMaxWithKey f (Bin sx kx x l Tip) = case f kx x of
                                           Nothing -> l
                                           Just x' -> Bin sx kx x' l Tip
updateMaxWithKey f (Bin _ kx x l r)    = balanceL kx x l (updateMaxWithKey f r)

-- | /O(log n)/. Retrieves the minimal (key,value) pair of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
--
-- > minViewWithKey (fromList [(5,"a"), (3,"b")]) == Just ((3,"b"), singleton 5 "a")
-- > minViewWithKey empty == Nothing

minViewWithKey :: Map k a -> Maybe ((k,a), Map k a)
minViewWithKey Tip = Nothing
minViewWithKey (Bin _ k x l r) =
  case minViewSure k x l r of
    MinView km xm t -> Just ((km, xm), t)

-- | /O(log n)/. Retrieves the maximal (key,value) pair of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
--
-- > maxViewWithKey (fromList [(5,"a"), (3,"b")]) == Just ((5,"a"), singleton 3 "b")
-- > maxViewWithKey empty == Nothing

maxViewWithKey :: Map k a -> Maybe ((k,a), Map k a)
maxViewWithKey Tip = Nothing
maxViewWithKey (Bin _ k x l r) =
  case maxViewSure k x l r of
    MaxView km xm t -> Just ((km, xm), t)

-- | /O(log n)/. Retrieves the value associated with minimal key of the
-- map, and the map stripped of that element, or 'Nothing' if passed an
-- empty map.
--
-- > minView (fromList [(5,"a"), (3,"b")]) == Just ("b", singleton 5 "a")
-- > minView empty == Nothing

minView :: Map k a -> Maybe (a, Map k a)
minView t = case minViewWithKey t of
              Nothing -> Nothing
              Just ((_, x), t') -> Just (x, t')

-- | /O(log n)/. Retrieves the value associated with maximal key of the
-- map, and the map stripped of that element, or 'Nothing' if passed an
-- empty map.
--
-- > maxView (fromList [(5,"a"), (3,"b")]) == Just ("a", singleton 3 "b")
-- > maxView empty == Nothing

maxView :: Map k a -> Maybe (a, Map k a)
maxView t = case maxViewWithKey t of
              Nothing -> Nothing
              Just ((_, x), t') -> Just (x, t')

{--------------------------------------------------------------------
  Union.
--------------------------------------------------------------------}
-- | The union of a list of maps:
--   (@'unions' == 'Prelude.foldl' 'union' 'empty'@).
--
-- > unions [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
-- >     == fromList [(3, "b"), (5, "a"), (7, "C")]
-- > unions [(fromList [(5, "A3"), (3, "B3")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "a"), (3, "b")])]
-- >     == fromList [(3, "B3"), (5, "A3"), (7, "C")]

unions :: Ord k => [Map k a] -> Map k a
unions ts
  = foldlStrict union empty ts
#if __GLASGOW_HASKELL__
{-# INLINABLE unions #-}
#endif

-- | The union of a list of maps, with a combining operation:
--   (@'unionsWith' f == 'Prelude.foldl' ('unionWith' f) 'empty'@).
--
-- > unionsWith (++) [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
-- >     == fromList [(3, "bB3"), (5, "aAA3"), (7, "C")]

unionsWith :: Ord k => (a->a->a) -> [Map k a] -> Map k a
unionsWith f ts
  = foldlStrict (unionWith f) empty ts
#if __GLASGOW_HASKELL__
{-# INLINABLE unionsWith #-}
#endif

-- | /O(m*log(n\/m + 1)), m <= n/.
-- The expression (@'union' t1 t2@) takes the left-biased union of @t1@ and @t2@.
-- It prefers @t1@ when duplicate keys are encountered,
-- i.e. (@'union' == 'unionWith' 'const'@).
--
-- > union (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "a"), (7, "C")]

union :: Ord k => Map k a -> Map k a -> Map k a
union t1 Tip  = t1
union t1 (Bin _ k x Tip Tip) = insertR k x t1
union (Bin _ k x Tip Tip) t2 = insert k x t2
union Tip t2 = t2
union t1@(Bin _ k1 x1 l1 r1) t2 = case split k1 t2 of
  (l2, r2) | l1l2 `ptrEq` l1 && r1r2 `ptrEq` r1 -> t1
           | otherwise -> link k1 x1 l1l2 r1r2
           where !l1l2 = union l1 l2
                 !r1r2 = union r1 r2
#if __GLASGOW_HASKELL__
{-# INLINABLE union #-}
#endif

{--------------------------------------------------------------------
  Union with a combining function
--------------------------------------------------------------------}
-- | /O(m*log(n\/m + 1)), m <= n/. Union with a combining function.
--
-- > unionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "aA"), (7, "C")]

unionWith :: Ord k => (a -> a -> a) -> Map k a -> Map k a -> Map k a
-- QuickCheck says pointer equality never happens here.
unionWith _f t1 Tip = t1
unionWith f t1 (Bin _ k x Tip Tip) = insertWithR f k x t1
unionWith f (Bin _ k x Tip Tip) t2 = insertWith f k x t2
unionWith _f Tip t2 = t2
unionWith f (Bin _ k1 x1 l1 r1) t2 = case splitLookup k1 t2 of
  (l2, mb, r2) -> case mb of
      Nothing -> link k1 x1 l1l2 r1r2
      Just x2 -> link k1 (f x1 x2) l1l2 r1r2
    where !l1l2 = unionWith f l1 l2
          !r1r2 = unionWith f r1 r2
#if __GLASGOW_HASKELL__
{-# INLINABLE unionWith #-}
#endif

-- | /O(m*log(n\/m + 1)), m <= n/.
-- Union with a combining function.
--
-- > let f key left_value right_value = (show key) ++ ":" ++ left_value ++ "|" ++ right_value
-- > unionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "5:a|A"), (7, "C")]

unionWithKey :: Ord k => (k -> a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWithKey _f t1 Tip = t1
unionWithKey f t1 (Bin _ k x Tip Tip) = insertWithKeyR f k x t1
unionWithKey f (Bin _ k x Tip Tip) t2 = insertWithKey f k x t2
unionWithKey _f Tip t2 = t2
unionWithKey f (Bin _ k1 x1 l1 r1) t2 = case splitLookup k1 t2 of
  (l2, mb, r2) -> case mb of
      Nothing -> link k1 x1 l1l2 r1r2
      Just x2 -> link k1 (f k1 x1 x2) l1l2 r1r2
    where !l1l2 = unionWithKey f l1 l2
          !r1r2 = unionWithKey f r1 r2
#if __GLASGOW_HASKELL__
{-# INLINABLE unionWithKey #-}
#endif

{--------------------------------------------------------------------
  Difference
--------------------------------------------------------------------}

-- We don't currently attempt to use any pointer equality tricks for
-- 'difference'. To do so, we'd have to match on the first argument
-- and split the second. Unfortunately, the proof of the time bound
-- relies on doing it the way we do, and it's not clear whether that
-- bound holds the other way.

-- | /O(m*log(n\/m + 1)), m <= n/. Difference of two maps.
-- Return elements of the first map not existing in the second map.
--
-- > difference (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 3 "b"

difference :: Ord k => Map k a -> Map k b -> Map k a
difference Tip _   = Tip
difference t1 Tip  = t1
difference t1 (Bin _ k _ l2 r2) = case split k t1 of
  (l1, r1)
    | size l1l2 + size r1r2 == size t1 -> t1
    | otherwise -> link2 l1l2 r1r2
    where
      !l1l2 = difference l1 l2
      !r1r2 = difference r1 r2
#if __GLASGOW_HASKELL__
{-# INLINABLE difference #-}
#endif

-- | /O(m*log(n\/m + 1)), m <= n/. Remove all keys in a 'Set' from a 'Map'.
--
-- @
-- m `withoutKeys` s = 'filterWithKey' (\k _ -> k `'Set.notMember'` s) m
-- @
--
-- @since 0.5.8

withoutKeys :: Ord k => Map k a -> Set k -> Map k a
withoutKeys Tip _ = Tip
withoutKeys m Set.Tip = m
withoutKeys m (Set.Bin _ k ls rs) = case splitMember k m of
  (lm, b, rm)
     | not b && lm' `ptrEq` lm && rm' `ptrEq` rm -> m
     | otherwise -> link2 lm' rm'
     where
       !lm' = withoutKeys lm ls
       !rm' = withoutKeys rm rs
#if __GLASGOW_HASKELL__
{-# INLINABLE withoutKeys #-}
#endif

-- | /O(n+m)/. Difference with a combining function.
-- When two equal keys are
-- encountered, the combining function is applied to the values of these keys.
-- If it returns 'Nothing', the element is discarded (proper set difference). If
-- it returns (@'Just' y@), the element is updated with a new value @y@.
--
-- > let f al ar = if al == "b" then Just (al ++ ":" ++ ar) else Nothing
-- > differenceWith f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (7, "C")])
-- >     == singleton 3 "b:B"
differenceWith :: Ord k => (a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
differenceWith f = merge preserveMissing dropMissing $
       zipWithMaybeMatched (\_ x y -> f x y)
#if __GLASGOW_HASKELL__
{-# INLINABLE differenceWith #-}
#endif

-- | /O(n+m)/. Difference with a combining function. When two equal keys are
-- encountered, the combining function is applied to the key and both values.
-- If it returns 'Nothing', the element is discarded (proper set difference). If
-- it returns (@'Just' y@), the element is updated with a new value @y@.
--
-- > let f k al ar = if al == "b" then Just ((show k) ++ ":" ++ al ++ "|" ++ ar) else Nothing
-- > differenceWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (10, "C")])
-- >     == singleton 3 "3:b|B"

differenceWithKey :: Ord k => (k -> a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
differenceWithKey f =
  merge preserveMissing dropMissing (zipWithMaybeMatched f)
#if __GLASGOW_HASKELL__
{-# INLINABLE differenceWithKey #-}
#endif


{--------------------------------------------------------------------
  Intersection
--------------------------------------------------------------------}
-- | /O(m*log(n\/m + 1)), m <= n/. Intersection of two maps.
-- Return data in the first map for the keys existing in both maps.
-- (@'intersection' m1 m2 == 'intersectionWith' 'const' m1 m2@).
--
-- > intersection (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "a"

intersection :: Ord k => Map k a -> Map k b -> Map k a
intersection Tip _ = Tip
intersection _ Tip = Tip
intersection t1@(Bin _ k x l1 r1) t2
  | mb = if l1l2 `ptrEq` l1 && r1r2 `ptrEq` r1
         then t1
         else link k x l1l2 r1r2
  | otherwise = link2 l1l2 r1r2
  where
    !(l2, mb, r2) = splitMember k t2
    !l1l2 = intersection l1 l2
    !r1r2 = intersection r1 r2
#if __GLASGOW_HASKELL__
{-# INLINABLE intersection #-}
#endif

-- | /O(m*log(n\/m + 1)), m <= n/. Restrict a 'Map' to only those keys
-- found in a 'Set'.
--
-- @
-- m `restrictKeys` s = 'filterWithKey' (\k _ -> k `'Set.member'` s) m
-- @
--
-- @since 0.5.8
restrictKeys :: Ord k => Map k a -> Set k -> Map k a
restrictKeys Tip _ = Tip
restrictKeys _ Set.Tip = Tip
restrictKeys m@(Bin _ k x l1 r1) s
  | b = if l1l2 `ptrEq` l1 && r1r2 `ptrEq` r1
        then m
        else link k x l1l2 r1r2
  | otherwise = link2 l1l2 r1r2
  where
    !(l2, b, r2) = Set.splitMember k s
    !l1l2 = restrictKeys l1 l2
    !r1r2 = restrictKeys r1 r2
#if __GLASGOW_HASKELL__
{-# INLINABLE restrictKeys #-}
#endif

-- | /O(m*log(n\/m + 1)), m <= n/. Intersection with a combining function.
--
-- > intersectionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "aA"

intersectionWith :: Ord k => (a -> b -> c) -> Map k a -> Map k b -> Map k c
-- We have no hope of pointer equality tricks here because every single
-- element in the result will be a thunk.
intersectionWith _f Tip _ = Tip
intersectionWith _f _ Tip = Tip
intersectionWith f (Bin _ k x1 l1 r1) t2 = case mb of
    Just x2 -> link k (f x1 x2) l1l2 r1r2
    Nothing -> link2 l1l2 r1r2
  where
    !(l2, mb, r2) = splitLookup k t2
    !l1l2 = intersectionWith f l1 l2
    !r1r2 = intersectionWith f r1 r2
#if __GLASGOW_HASKELL__
{-# INLINABLE intersectionWith #-}
#endif

-- | /O(m*log(n\/m + 1)), m <= n/. Intersection with a combining function.
--
-- > let f k al ar = (show k) ++ ":" ++ al ++ "|" ++ ar
-- > intersectionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "5:a|A"

intersectionWithKey :: Ord k => (k -> a -> b -> c) -> Map k a -> Map k b -> Map k c
intersectionWithKey _f Tip _ = Tip
intersectionWithKey _f _ Tip = Tip
intersectionWithKey f (Bin _ k x1 l1 r1) t2 = case mb of
    Just x2 -> link k (f k x1 x2) l1l2 r1r2
    Nothing -> link2 l1l2 r1r2
  where
    !(l2, mb, r2) = splitLookup k t2
    !l1l2 = intersectionWithKey f l1 l2
    !r1r2 = intersectionWithKey f r1 r2
#if __GLASGOW_HASKELL__
{-# INLINABLE intersectionWithKey #-}
#endif

#if !MIN_VERSION_base (4,8,0)
-- | The identity type.
newtype Identity a = Identity { runIdentity :: a }
#if __GLASGOW_HASKELL__ == 708
instance Functor Identity where
  fmap = coerce
instance Applicative Identity where
  (<*>) = coerce
  pure = Identity
#else
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
instance Applicative Identity where
  Identity f <*> Identity x = Identity (f x)
  pure = Identity
#endif
#endif

-- | A tactic for dealing with keys present in one map but not the other in
-- 'merge' or 'mergeA'.
--
-- A tactic of type @ WhenMissing f k x z @ is an abstract representation
-- of a function of type @ k -> x -> f (Maybe z) @.

data WhenMissing f k x y = WhenMissing
  { missingSubtree :: Map k x -> f (Map k y)
  , missingKey :: k -> x -> f (Maybe y)}

instance (Applicative f, Monad f) => Functor (WhenMissing f k x) where
  fmap = mapWhenMissing
  {-# INLINE fmap #-}

instance (Applicative f, Monad f)
         => Category.Category (WhenMissing f k) where
  id = preserveMissing
  f . g = traverseMaybeMissing $
    \ k x -> missingKey g k x >>= \y ->
         case y of
           Nothing -> pure Nothing
           Just q -> missingKey f k q
  {-# INLINE id #-}
  {-# INLINE (.) #-}

-- | Equivalent to @ ReaderT k (ReaderT x (MaybeT f)) @.
instance (Applicative f, Monad f) => Applicative (WhenMissing f k x) where
  pure x = mapMissing (\ _ _ -> x)
  f <*> g = traverseMaybeMissing $ \k x -> do
         res1 <- missingKey f k x
         case res1 of
           Nothing -> pure Nothing
           Just r -> (pure $!) . fmap r =<< missingKey g k x
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

-- | Equivalent to @ ReaderT k (ReaderT x (MaybeT f)) @.
instance (Applicative f, Monad f) => Monad (WhenMissing f k x) where
#if !MIN_VERSION_base(4,8,0)
  return = pure
#endif
  m >>= f = traverseMaybeMissing $ \k x -> do
         res1 <- missingKey m k x
         case res1 of
           Nothing -> pure Nothing
           Just r -> missingKey (f r) k x
  {-# INLINE (>>=) #-}

-- | Map covariantly over a @'WhenMissing' f k x@.
mapWhenMissing :: (Applicative f, Monad f)
               => (a -> b)
               -> WhenMissing f k x a -> WhenMissing f k x b
mapWhenMissing f t = WhenMissing
    { missingSubtree = \m -> missingSubtree t m >>= \m' -> pure $! fmap f m'
    , missingKey = \k x -> missingKey t k x >>= \q -> (pure $! fmap f q) }
{-# INLINE mapWhenMissing #-}

-- | Map covariantly over a @'WhenMissing' f k x@, using only a 'Functor f'
-- constraint.
mapGentlyWhenMissing :: Functor f
               => (a -> b)
               -> WhenMissing f k x a -> WhenMissing f k x b
mapGentlyWhenMissing f t = WhenMissing
    { missingSubtree = \m -> fmap f <$> missingSubtree t m
    , missingKey = \k x -> fmap f <$> missingKey t k x }
{-# INLINE mapGentlyWhenMissing #-}

-- | Map covariantly over a @'WhenMatched' f k x@, using only a 'Functor f'
-- constraint.
mapGentlyWhenMatched :: Functor f
               => (a -> b)
               -> WhenMatched f k x y a -> WhenMatched f k x y b
mapGentlyWhenMatched f t = zipWithMaybeAMatched $
  \k x y -> fmap f <$> runWhenMatched t k x y
{-# INLINE mapGentlyWhenMatched #-}

-- | Map contravariantly over a @'WhenMissing' f k _ x@.
lmapWhenMissing :: (b -> a) -> WhenMissing f k a x -> WhenMissing f k b x
lmapWhenMissing f t = WhenMissing
  { missingSubtree = \m -> missingSubtree t (fmap f m)
  , missingKey = \k x -> missingKey t k (f x) }
{-# INLINE lmapWhenMissing #-}

-- | Map contravariantly over a @'WhenMatched' f k _ y z@.
contramapFirstWhenMatched :: (b -> a)
                          -> WhenMatched f k a y z
                          -> WhenMatched f k b y z
contramapFirstWhenMatched f t = WhenMatched $
  \k x y -> runWhenMatched t k (f x) y
{-# INLINE contramapFirstWhenMatched #-}

-- | Map contravariantly over a @'WhenMatched' f k x _ z@.
contramapSecondWhenMatched :: (b -> a)
                           -> WhenMatched f k x a z
                           -> WhenMatched f k x b z
contramapSecondWhenMatched f t = WhenMatched $
  \k x y -> runWhenMatched t k x (f y)
{-# INLINE contramapSecondWhenMatched #-}

-- | A tactic for dealing with keys present in one map but not the other in
-- 'merge'.
--
-- A tactic of type @ SimpleWhenMissing k x z @ is an abstract representation
-- of a function of type @ k -> x -> Maybe z @.
type SimpleWhenMissing = WhenMissing Identity

-- | A tactic for dealing with keys present in both
-- maps in 'merge' or 'mergeA'.
--
-- A tactic of type @ WhenMatched f k x y z @ is an abstract representation
-- of a function of type @ k -> x -> y -> f (Maybe z) @.
newtype WhenMatched f k x y z = WhenMatched
  { matchedKey :: k -> x -> y -> f (Maybe z) }

-- | Along with zipWithMaybeAMatched, witnesses the isomorphism between
-- @WhenMatched f k x y z@ and @k -> x -> y -> f (Maybe z)@.
runWhenMatched :: WhenMatched f k x y z -> k -> x -> y -> f (Maybe z)
runWhenMatched = matchedKey
{-# INLINE runWhenMatched #-}

-- | Along with traverseMaybeMissing, witnesses the isomorphism between
-- @WhenMissing f k x y@ and @k -> x -> f (Maybe y)@.
runWhenMissing :: WhenMissing f k x y -> k -> x -> f (Maybe y)
runWhenMissing = missingKey
{-# INLINE runWhenMissing #-}

instance Functor f => Functor (WhenMatched f k x y) where
  fmap = mapWhenMatched
  {-# INLINE fmap #-}

instance (Monad f, Applicative f) => Category.Category (WhenMatched f k x) where
  id = zipWithMatched (\_ _ y -> y)
  f . g = zipWithMaybeAMatched $
            \k x y -> do
              res <- runWhenMatched g k x y
              case res of
                Nothing -> pure Nothing
                Just r -> runWhenMatched f k x r
  {-# INLINE id #-}
  {-# INLINE (.) #-}

-- | Equivalent to @ ReaderT k (ReaderT x (ReaderT y (MaybeT f))) @
instance (Monad f, Applicative f) => Applicative (WhenMatched f k x y) where
  pure x = zipWithMatched (\_ _ _ -> x)
  fs <*> xs = zipWithMaybeAMatched $ \k x y -> do
    res <- runWhenMatched fs k x y
    case res of
      Nothing -> pure Nothing
      Just r -> (pure $!) . fmap r =<< runWhenMatched xs k x y
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

-- | Equivalent to @ ReaderT k (ReaderT x (ReaderT y (MaybeT f))) @
instance (Monad f, Applicative f) => Monad (WhenMatched f k x y) where
#if !MIN_VERSION_base(4,8,0)
  return = pure
#endif
  m >>= f = zipWithMaybeAMatched $ \k x y -> do
    res <- runWhenMatched m k x y
    case res of
      Nothing -> pure Nothing
      Just r -> runWhenMatched (f r) k x y
  {-# INLINE (>>=) #-}

-- | Map covariantly over a @'WhenMatched' f k x y@.
mapWhenMatched :: Functor f
               => (a -> b)
               -> WhenMatched f k x y a
               -> WhenMatched f k x y b
mapWhenMatched f (WhenMatched g) = WhenMatched $ \k x y -> fmap (fmap f) (g k x y)
{-# INLINE mapWhenMatched #-}

-- | A tactic for dealing with keys present in both maps in 'merge'.
--
-- A tactic of type @ SimpleWhenMatched k x y z @ is an abstract representation
-- of a function of type @ k -> x -> y -> Maybe z @.
type SimpleWhenMatched = WhenMatched Identity

-- | When a key is found in both maps, apply a function to the
-- key and values and use the result in the merged map.
--
-- @
-- zipWithMatched :: (k -> x -> y -> z)
--                -> SimpleWhenMatched k x y z
-- @
zipWithMatched :: Applicative f
               => (k -> x -> y -> z)
               -> WhenMatched f k x y z
zipWithMatched f = WhenMatched $ \ k x y -> pure . Just $ f k x y
{-# INLINE zipWithMatched #-}

-- | When a key is found in both maps, apply a function to the
-- key and values to produce an action and use its result in the merged map.
zipWithAMatched :: Applicative f
                => (k -> x -> y -> f z)
                -> WhenMatched f k x y z
zipWithAMatched f = WhenMatched $ \ k x y -> Just <$> f k x y
{-# INLINE zipWithAMatched #-}

-- | When a key is found in both maps, apply a function to the
-- key and values and maybe use the result in the merged map.
--
-- @
-- zipWithMaybeMatched :: (k -> x -> y -> Maybe z)
--                     -> SimpleWhenMatched k x y z
-- @
zipWithMaybeMatched :: Applicative f
                    => (k -> x -> y -> Maybe z)
                    -> WhenMatched f k x y z
zipWithMaybeMatched f = WhenMatched $ \ k x y -> pure $ f k x y
{-# INLINE zipWithMaybeMatched #-}

-- | When a key is found in both maps, apply a function to the
-- key and values, perform the resulting action, and maybe use
-- the result in the merged map.
--
-- This is the fundamental 'WhenMatched' tactic.
zipWithMaybeAMatched :: (k -> x -> y -> f (Maybe z))
                     -> WhenMatched f k x y z
zipWithMaybeAMatched f = WhenMatched $ \ k x y -> f k x y
{-# INLINE zipWithMaybeAMatched #-}

-- | Drop all the entries whose keys are missing from the other
-- map.
--
-- @
-- dropMissing :: SimpleWhenMissing k x y
-- @
--
-- prop> dropMissing = mapMaybeMissing (\_ _ -> Nothing)
--
-- but @dropMissing@ is much faster.
dropMissing :: Applicative f => WhenMissing f k x y
dropMissing = WhenMissing
  { missingSubtree = const (pure Tip)
  , missingKey = \_ _ -> pure Nothing }
{-# INLINE dropMissing #-}

-- | Preserve, unchanged, the entries whose keys are missing from
-- the other map.
--
-- @
-- preserveMissing :: SimpleWhenMissing k x x
-- @
--
-- prop> preserveMissing = Merge.Lazy.mapMaybeMissing (\_ x -> Just x)
--
-- but @preserveMissing@ is much faster.
preserveMissing :: Applicative f => WhenMissing f k x x
preserveMissing = WhenMissing
  { missingSubtree = pure
  , missingKey = \_ v -> pure (Just v) }
{-# INLINE preserveMissing #-}

-- | Map over the entries whose keys are missing from the other map.
--
-- @
-- mapMissing :: (k -> x -> y) -> SimpleWhenMissing k x y
-- @
--
-- prop> mapMissing f = mapMaybeMissing (\k x -> Just $ f k x)
--
-- but @mapMissing@ is somewhat faster.
mapMissing :: Applicative f => (k -> x -> y) -> WhenMissing f k x y
mapMissing f = WhenMissing
  { missingSubtree = \m -> pure $! mapWithKey f m
  , missingKey = \ k x -> pure $ Just (f k x) }
{-# INLINE mapMissing #-}

-- | Map over the entries whose keys are missing from the other map,
-- optionally removing some. This is the most powerful 'SimpleWhenMissing'
-- tactic, but others are usually more efficient.
--
-- @
-- mapMaybeMissing :: (k -> x -> Maybe y) -> SimpleWhenMissing k x y
-- @
--
-- prop> mapMaybeMissing f = traverseMaybeMissing (\k x -> pure (f k x))
--
-- but @mapMaybeMissing@ uses fewer unnecessary 'Applicative' operations.
mapMaybeMissing :: Applicative f => (k -> x -> Maybe y) -> WhenMissing f k x y
mapMaybeMissing f = WhenMissing
  { missingSubtree = \m -> pure $! mapMaybeWithKey f m
  , missingKey = \k x -> pure $! f k x }
{-# INLINE mapMaybeMissing #-}

-- | Filter the entries whose keys are missing from the other map.
--
-- @
-- filterMissing :: (k -> x -> Bool) -> SimpleWhenMissing k x x
-- @
--
-- prop> filterMissing f = Merge.Lazy.mapMaybeMissing $ \k x -> guard (f k x) *> Just x
--
-- but this should be a little faster.
filterMissing :: Applicative f
              => (k -> x -> Bool) -> WhenMissing f k x x
filterMissing f = WhenMissing
  { missingSubtree = \m -> pure $! filterWithKey f m
  , missingKey = \k x -> pure $! if f k x then Just x else Nothing }
{-# INLINE filterMissing #-}

-- | Filter the entries whose keys are missing from the other map
-- using some 'Applicative' action.
--
-- @
-- filterAMissing f = Merge.Lazy.traverseMaybeMissing $
--   \k x -> (\b -> guard b *> Just x) <$> f k x
-- @
--
-- but this should be a little faster.
filterAMissing :: Applicative f
              => (k -> x -> f Bool) -> WhenMissing f k x x
filterAMissing f = WhenMissing
  { missingSubtree = \m -> filterWithKeyA f m
  , missingKey = \k x -> bool Nothing (Just x) <$> f k x }
{-# INLINE filterAMissing #-}

-- | This wasn't in Data.Bool until 4.7.0, so we define it here
bool :: a -> a -> Bool -> a
bool f _ False = f
bool _ t True  = t

-- | Traverse over the entries whose keys are missing from the other map.
traverseMissing :: Applicative f
                    => (k -> x -> f y) -> WhenMissing f k x y
traverseMissing f = WhenMissing
  { missingSubtree = traverseWithKey f
  , missingKey = \k x -> Just <$> f k x }
{-# INLINE traverseMissing #-}

-- | Traverse over the entries whose keys are missing from the other map,
-- optionally producing values to put in the result.
-- This is the most powerful 'WhenMissing' tactic, but others are usually
-- more efficient.
traverseMaybeMissing :: Applicative f
                      => (k -> x -> f (Maybe y)) -> WhenMissing f k x y
traverseMaybeMissing f = WhenMissing
  { missingSubtree = traverseMaybeWithKey f
  , missingKey = f }
{-# INLINE traverseMaybeMissing #-}

-- | Merge two maps.
--
-- @merge@ takes two 'WhenMissing' tactics, a 'WhenMatched'
-- tactic and two maps. It uses the tactics to merge the maps.
-- Its behavior is best understood via its fundamental tactics,
-- 'mapMaybeMissing' and 'zipWithMaybeMatched'.
--
-- Consider
--
-- @
-- merge (mapMaybeMissing g1)
--              (mapMaybeMissing g2)
--              (zipWithMaybeMatched f)
--              m1 m2
-- @
--
-- Take, for example,
--
-- @
-- m1 = [(0, 'a'), (1, 'b'), (3,'c'), (4, 'd')]
-- m2 = [(1, "one"), (2, "two"), (4, "three")]
-- @
--
-- @merge@ will first ''align'' these maps by key:
--
-- @
-- m1 = [(0, 'a'), (1, 'b'),               (3,'c'), (4, 'd')]
-- m2 =           [(1, "one"), (2, "two"),          (4, "three")]
-- @
--
-- It will then pass the individual entries and pairs of entries
-- to @g1@, @g2@, or @f@ as appropriate:
--
-- @
-- maybes = [g1 0 'a', f 1 'b' "one", g2 2 "two", g1 3 'c', f 4 'd' "three"]
-- @
--
-- This produces a 'Maybe' for each key:
--
-- @
-- keys =     0        1          2           3        4
-- results = [Nothing, Just True, Just False, Nothing, Just True]
-- @
--
-- Finally, the @Just@ results are collected into a map:
--
-- @
-- return value = [(1, True), (2, False), (4, True)]
-- @
--
-- The other tactics below are optimizations or simplifications of
-- 'mapMaybeMissing' for special cases. Most importantly,
--
-- * 'dropMissing' drops all the keys.
-- * 'preserveMissing' leaves all the entries alone.
--
-- When 'merge' is given three arguments, it is inlined at the call
-- site. To prevent excessive inlining, you should typically use 'merge'
-- to define your custom combining functions.
--
--
-- Examples:
--
-- prop> unionWithKey f = merge preserveMissing preserveMissing (zipWithMatched f)
-- prop> intersectionWithKey f = merge dropMissing dropMissing (zipWithMatched f)
-- prop> differenceWith f = merge diffPreserve diffDrop f
-- prop> symmetricDifference = merge diffPreserve diffPreserve (\ _ _ _ -> Nothing)
-- prop> mapEachPiece f g h = merge (diffMapWithKey f) (diffMapWithKey g)
--
-- @since 0.5.8
merge :: Ord k
             => SimpleWhenMissing k a c -- ^ What to do with keys in @m1@ but not @m2@
             -> SimpleWhenMissing k b c -- ^ What to do with keys in @m2@ but not @m1@
             -> SimpleWhenMatched k a b c -- ^ What to do with keys in both @m1@ and @m2@
             -> Map k a -- ^ Map @m1@
             -> Map k b -- ^ Map @m2@
             -> Map k c
merge g1 g2 f m1 m2 = runIdentity $
  mergeA g1 g2 f m1 m2
{-# INLINE merge #-}

-- | An applicative version of 'merge'.
--
-- @mergeA@ takes two 'WhenMissing' tactics, a 'WhenMatched'
-- tactic and two maps. It uses the tactics to merge the maps.
-- Its behavior is best understood via its fundamental tactics,
-- 'traverseMaybeMissing' and 'zipWithMaybeAMatched'.
--
-- Consider
--
-- @
-- mergeA (traverseMaybeMissing g1)
--               (traverseMaybeMissing g2)
--               (zipWithMaybeAMatched f)
--               m1 m2
-- @
--
-- Take, for example,
--
-- @
-- m1 = [(0, 'a'), (1, 'b'), (3,'c'), (4, 'd')]
-- m2 = [(1, "one"), (2, "two"), (4, "three")]
-- @
--
-- @mergeA@ will first ''align'' these maps by key:
--
-- @
-- m1 = [(0, 'a'), (1, 'b'),               (3,'c'), (4, 'd')]
-- m2 =           [(1, "one"), (2, "two"),          (4, "three")]
-- @
--
-- It will then pass the individual entries and pairs of entries
-- to @g1@, @g2@, or @f@ as appropriate:
--
-- @
-- actions = [g1 0 'a', f 1 'b' "one", g2 2 "two", g1 3 'c', f 4 'd' "three"]
-- @
--
-- Next, it will perform the actions in the @actions@ list in order from
-- left to right.
--
-- @
-- keys =     0        1          2           3        4
-- results = [Nothing, Just True, Just False, Nothing, Just True]
-- @
--
-- Finally, the @Just@ results are collected into a map:
--
-- @
-- return value = [(1, True), (2, False), (4, True)]
-- @
--
-- The other tactics below are optimizations or simplifications of
-- 'traverseMaybeMissing' for special cases. Most importantly,
--
-- * 'dropMissing' drops all the keys.
-- * 'preserveMissing' leaves all the entries alone.
-- * 'mapMaybeMissing' does not use the 'Applicative' context.
--
-- When 'mergeA' is given three arguments, it is inlined at the call
-- site. To prevent excessive inlining, you should generally only use
-- 'mergeA' to define custom combining functions.
--
-- @since 0.5.8
mergeA
  :: (Applicative f, Ord k)
  => WhenMissing f k a c -- ^ What to do with keys in @m1@ but not @m2@
  -> WhenMissing f k b c -- ^ What to do with keys in @m2@ but not @m1@
  -> WhenMatched f k a b c -- ^ What to do with keys in both @m1@ and @m2@
  -> Map k a -- ^ Map @m1@
  -> Map k b -- ^ Map @m2@
  -> f (Map k c)
mergeA
    WhenMissing{missingSubtree = g1t, missingKey = g1k}
    WhenMissing{missingSubtree = g2t}
    (WhenMatched f) = go
  where
    go t1 Tip = g1t t1
    go Tip t2 = g2t t2
    go (Bin _ kx x1 l1 r1) t2 = case splitLookup kx t2 of
      (l2, mx2, r2) -> case mx2 of
          Nothing -> liftA3 (\l' mx' r' -> maybe link2 (link kx) mx' l' r')
                        l1l2 (g1k kx x1) r1r2
          Just x2 -> liftA3 (\l' mx' r' -> maybe link2 (link kx) mx' l' r')
                        l1l2 (f kx x1 x2) r1r2
        where
          !l1l2 = go l1 l2
          !r1r2 = go r1 r2
{-# INLINE mergeA #-}


{--------------------------------------------------------------------
  MergeWithKey
--------------------------------------------------------------------}

-- | /O(n+m)/. An unsafe general combining function.
--
-- WARNING: This function can produce corrupt maps and its results
-- may depend on the internal structures of its inputs. Users should
-- prefer 'merge' or 'mergeA'.
--
-- When 'mergeWithKey' is given three arguments, it is inlined to the call
-- site. You should therefore use 'mergeWithKey' only to define custom
-- combining functions. For example, you could define 'unionWithKey',
-- 'differenceWithKey' and 'intersectionWithKey' as
--
-- > myUnionWithKey f m1 m2 = mergeWithKey (\k x1 x2 -> Just (f k x1 x2)) id id m1 m2
-- > myDifferenceWithKey f m1 m2 = mergeWithKey f id (const empty) m1 m2
-- > myIntersectionWithKey f m1 m2 = mergeWithKey (\k x1 x2 -> Just (f k x1 x2)) (const empty) (const empty) m1 m2
--
-- When calling @'mergeWithKey' combine only1 only2@, a function combining two
-- 'Map's is created, such that
--
-- * if a key is present in both maps, it is passed with both corresponding
--   values to the @combine@ function. Depending on the result, the key is either
--   present in the result with specified value, or is left out;
--
-- * a nonempty subtree present only in the first map is passed to @only1@ and
--   the output is added to the result;
--
-- * a nonempty subtree present only in the second map is passed to @only2@ and
--   the output is added to the result.
--
-- The @only1@ and @only2@ methods /must return a map with a subset (possibly empty) of the keys of the given map/.
-- The values can be modified arbitrarily. Most common variants of @only1@ and
-- @only2@ are 'id' and @'const' 'empty'@, but for example @'map' f@,
-- @'filterWithKey' f@, or @'mapMaybeWithKey' f@ could be used for any @f@.

mergeWithKey :: Ord k
             => (k -> a -> b -> Maybe c)
             -> (Map k a -> Map k c)
             -> (Map k b -> Map k c)
             -> Map k a -> Map k b -> Map k c
mergeWithKey f g1 g2 = go
  where
    go Tip t2 = g2 t2
    go t1 Tip = g1 t1
    go (Bin _ kx x l1 r1) t2 =
      case found of
        Nothing -> case g1 (singleton kx x) of
                     Tip -> link2 l' r'
                     (Bin _ _ x' Tip Tip) -> link kx x' l' r'
                     _ -> error "mergeWithKey: Given function only1 does not fulfill required conditions (see documentation)"
        Just x2 -> case f kx x x2 of
                     Nothing -> link2 l' r'
                     Just x' -> link kx x' l' r'
      where
        (l2, found, r2) = splitLookup kx t2
        l' = go l1 l2
        r' = go r1 r2
{-# INLINE mergeWithKey #-}

{--------------------------------------------------------------------
  Submap
--------------------------------------------------------------------}
-- | /O(m*log(n\/m + 1)), m <= n/.
-- This function is defined as (@'isSubmapOf' = 'isSubmapOfBy' (==)@).
--
isSubmapOf :: (Ord k,Eq a) => Map k a -> Map k a -> Bool
isSubmapOf m1 m2 = isSubmapOfBy (==) m1 m2
#if __GLASGOW_HASKELL__
{-# INLINABLE isSubmapOf #-}
#endif

{- | /O(m*log(n\/m + 1)), m <= n/.
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
#if __GLASGOW_HASKELL__
{-# INLINABLE isSubmapOfBy #-}
#endif

submap' :: Ord a => (b -> c -> Bool) -> Map a b -> Map a c -> Bool
submap' _ Tip _ = True
submap' _ _ Tip = False
submap' f (Bin _ kx x l r) t
  = case found of
      Nothing -> False
      Just y  -> f x y && submap' f l lt && submap' f r gt
  where
    (lt,found,gt) = splitLookup kx t
#if __GLASGOW_HASKELL__
{-# INLINABLE submap' #-}
#endif

-- | /O(m*log(n\/m + 1)), m <= n/. Is this a proper submap? (ie. a submap but not equal).
-- Defined as (@'isProperSubmapOf' = 'isProperSubmapOfBy' (==)@).
isProperSubmapOf :: (Ord k,Eq a) => Map k a -> Map k a -> Bool
isProperSubmapOf m1 m2
  = isProperSubmapOfBy (==) m1 m2
#if __GLASGOW_HASKELL__
{-# INLINABLE isProperSubmapOf #-}
#endif

{- | /O(m*log(n\/m + 1)), m <= n/. Is this a proper submap? (ie. a submap but not equal).
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
#if __GLASGOW_HASKELL__
{-# INLINABLE isProperSubmapOfBy #-}
#endif

{--------------------------------------------------------------------
  Filter and partition
--------------------------------------------------------------------}
-- | /O(n)/. Filter all values that satisfy the predicate.
--
-- > filter (> "a") (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > filter (> "x") (fromList [(5,"a"), (3,"b")]) == empty
-- > filter (< "a") (fromList [(5,"a"), (3,"b")]) == empty

filter :: (a -> Bool) -> Map k a -> Map k a
filter p m
  = filterWithKey (\_ x -> p x) m

-- | /O(n)/. Filter all keys\/values that satisfy the predicate.
--
-- > filterWithKey (\k _ -> k > 4) (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

filterWithKey :: (k -> a -> Bool) -> Map k a -> Map k a
filterWithKey _ Tip = Tip
filterWithKey p t@(Bin _ kx x l r)
  | p kx x    = if pl `ptrEq` l && pr `ptrEq` r
                then t
                else link kx x pl pr
  | otherwise = link2 pl pr
  where !pl = filterWithKey p l
        !pr = filterWithKey p r

-- | /O(n)/. Filter keys and values using an 'Applicative'
-- predicate.
filterWithKeyA :: Applicative f => (k -> a -> f Bool) -> Map k a -> f (Map k a)
filterWithKeyA _ Tip = pure Tip
filterWithKeyA p t@(Bin _ kx x l r) =
  liftA3 combine (p kx x) (filterWithKeyA p l) (filterWithKeyA p r)
  where
    combine True pl pr
      | pl `ptrEq` l && pr `ptrEq` r = t
      | otherwise = link kx x pl pr
    combine False pl pr = link2 pl pr

-- | /O(log n)/. Take while a predicate on the keys holds.
-- The user is responsible for ensuring that for all keys @j@ and @k@ in the map,
-- @j \< k ==\> p j \>= p k@. See note at 'spanAntitone'.
--
-- @
-- takeWhileAntitone p = 'fromDistinctAscList' . 'Data.List.takeWhile' (p . fst) . 'toList'
-- takeWhileAntitone p = 'filterWithKey' (\k _ -> p k)
-- @

takeWhileAntitone :: (k -> Bool) -> Map k a -> Map k a
takeWhileAntitone _ Tip = Tip
takeWhileAntitone p (Bin _ kx x l r)
  | p kx = link kx x l (takeWhileAntitone p r)
  | otherwise = takeWhileAntitone p l

-- | /O(log n)/. Drop while a predicate on the keys holds.
-- The user is responsible for ensuring that for all keys @j@ and @k@ in the map,
-- @j \< k ==\> p j \>= p k@. See note at 'spanAntitone'.
--
-- @
-- dropWhileAntitone p = 'fromDistinctAscList' . 'Data.List.dropWhile' (p . fst) . 'toList'
-- dropWhileAntitone p = 'filterWithKey' (\k -> not (p k))
-- @

dropWhileAntitone :: (k -> Bool) -> Map k a -> Map k a
dropWhileAntitone _ Tip = Tip
dropWhileAntitone p (Bin _ kx x l r)
  | p kx = dropWhileAntitone p r
  | otherwise = link kx x (dropWhileAntitone p l) r

-- | /O(log n)/. Divide a map at the point where a predicate on the keys stops holding.
-- The user is responsible for ensuring that for all keys @j@ and @k@ in the map,
-- @j \< k ==\> p j \>= p k@.
--
-- @
-- spanAntitone p xs = ('takeWhileAntitone' p xs, 'dropWhileAntitone' p xs)
-- spanAntitone p xs = partition p xs
-- @
--
-- Note: if @p@ is not actually antitone, then @spanAntitone@ will split the map
-- at some /unspecified/ point where the predicate switches from holding to not
-- holding (where the predicate is seen to hold before the first key and to fail
-- after the last key).

spanAntitone :: (k -> Bool) -> Map k a -> (Map k a, Map k a)
spanAntitone p0 m = toPair (go p0 m)
  where
    go _ Tip = Tip :*: Tip
    go p (Bin _ kx x l r)
      | p kx = let u :*: v = go p r in link kx x l u :*: v
      | otherwise = let u :*: v = go p l in u :*: link kx x v r

-- | /O(n)/. Partition the map according to a predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
--
-- > partition (> "a") (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", singleton 5 "a")
-- > partition (< "x") (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], empty)
-- > partition (> "x") (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3, "b"), (5, "a")])

partition :: (a -> Bool) -> Map k a -> (Map k a,Map k a)
partition p m
  = partitionWithKey (\_ x -> p x) m

-- | /O(n)/. Partition the map according to a predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
--
-- > partitionWithKey (\ k _ -> k > 3) (fromList [(5,"a"), (3,"b")]) == (singleton 5 "a", singleton 3 "b")
-- > partitionWithKey (\ k _ -> k < 7) (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], empty)
-- > partitionWithKey (\ k _ -> k > 7) (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3, "b"), (5, "a")])

partitionWithKey :: (k -> a -> Bool) -> Map k a -> (Map k a,Map k a)
partitionWithKey p0 t0 = toPair $ go p0 t0
  where
    go _ Tip = (Tip :*: Tip)
    go p t@(Bin _ kx x l r)
      | p kx x    = (if l1 `ptrEq` l && r1 `ptrEq` r
                     then t
                     else link kx x l1 r1) :*: link2 l2 r2
      | otherwise = link2 l1 r1 :*:
                    (if l2 `ptrEq` l && r2 `ptrEq` r
                     then t
                     else link kx x l2 r2)
      where
        (l1 :*: l2) = go p l
        (r1 :*: r2) = go p r

-- | /O(n)/. Map values and collect the 'Just' results.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > mapMaybe f (fromList [(5,"a"), (3,"b")]) == singleton 5 "new a"

mapMaybe :: (a -> Maybe b) -> Map k a -> Map k b
mapMaybe f = mapMaybeWithKey (\_ x -> f x)

-- | /O(n)/. Map keys\/values and collect the 'Just' results.
--
-- > let f k _ = if k < 5 then Just ("key : " ++ (show k)) else Nothing
-- > mapMaybeWithKey f (fromList [(5,"a"), (3,"b")]) == singleton 3 "key : 3"

mapMaybeWithKey :: (k -> a -> Maybe b) -> Map k a -> Map k b
mapMaybeWithKey _ Tip = Tip
mapMaybeWithKey f (Bin _ kx x l r) = case f kx x of
  Just y  -> link kx y (mapMaybeWithKey f l) (mapMaybeWithKey f r)
  Nothing -> link2 (mapMaybeWithKey f l) (mapMaybeWithKey f r)

-- | /O(n)/. Traverse keys\/values and collect the 'Just' results.
--
-- @since 0.5.8
traverseMaybeWithKey :: Applicative f
                     => (k -> a -> f (Maybe b)) -> Map k a -> f (Map k b)
traverseMaybeWithKey = go
  where
    go _ Tip = pure Tip
    go f (Bin _ kx x Tip Tip) = maybe Tip (\x' -> Bin 1 kx x' Tip Tip) <$> f kx x
    go f (Bin _ kx x l r) = liftA3 combine (go f l) (f kx x) (go f r)
      where
        combine !l' mx !r' = case mx of
          Nothing -> link2 l' r'
          Just x' -> link kx x' l' r'

-- | /O(n)/. Map values and separate the 'Left' and 'Right' results.
--
-- > let f a = if a < "c" then Left a else Right a
-- > mapEither f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (fromList [(3,"b"), (5,"a")], fromList [(1,"x"), (7,"z")])
-- >
-- > mapEither (\ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (empty, fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])

mapEither :: (a -> Either b c) -> Map k a -> (Map k b, Map k c)
mapEither f m
  = mapEitherWithKey (\_ x -> f x) m

-- | /O(n)/. Map keys\/values and separate the 'Left' and 'Right' results.
--
-- > let f k a = if k < 5 then Left (k * 2) else Right (a ++ a)
-- > mapEitherWithKey f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (fromList [(1,2), (3,6)], fromList [(5,"aa"), (7,"zz")])
-- >
-- > mapEitherWithKey (\_ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (empty, fromList [(1,"x"), (3,"b"), (5,"a"), (7,"z")])

mapEitherWithKey :: (k -> a -> Either b c) -> Map k a -> (Map k b, Map k c)
mapEitherWithKey f0 t0 = toPair $ go f0 t0
  where
    go _ Tip = (Tip :*: Tip)
    go f (Bin _ kx x l r) = case f kx x of
      Left y  -> link kx y l1 r1 :*: link2 l2 r2
      Right z -> link2 l1 r1 :*: link kx z l2 r2
     where
        (l1 :*: l2) = go f l
        (r1 :*: r2) = go f r

{--------------------------------------------------------------------
  Mapping
--------------------------------------------------------------------}
-- | /O(n)/. Map a function over all values in the map.
--
-- > map (++ "x") (fromList [(5,"a"), (3,"b")]) == fromList [(3, "bx"), (5, "ax")]

map :: (a -> b) -> Map k a -> Map k b
map f = go where
  go Tip = Tip
  go (Bin sx kx x l r) = Bin sx kx (f x) (go l) (go r)
-- We use a `go` function to allow `map` to inline. This makes
-- a big difference if someone uses `map (const x) m` instead
-- of `x <$ m`; it doesn't seem to do any harm.

#ifdef __GLASGOW_HASKELL__
{-# NOINLINE [1] map #-}
{-# RULES
"map/map" forall f g xs . map f (map g xs) = map (f . g) xs
 #-}
#endif
#if __GLASGOW_HASKELL__ >= 709
-- Safe coercions were introduced in 7.8, but did not work well with RULES yet.
{-# RULES
"map/coerce" map coerce = coerce
 #-}
#endif

-- | /O(n)/. Map a function over all values in the map.
--
-- > let f key x = (show key) ++ ":" ++ x
-- > mapWithKey f (fromList [(5,"a"), (3,"b")]) == fromList [(3, "3:b"), (5, "5:a")]

mapWithKey :: (k -> a -> b) -> Map k a -> Map k b
mapWithKey _ Tip = Tip
mapWithKey f (Bin sx kx x l r) = Bin sx kx (f kx x) (mapWithKey f l) (mapWithKey f r)

#ifdef __GLASGOW_HASKELL__
{-# NOINLINE [1] mapWithKey #-}
{-# RULES
"mapWithKey/mapWithKey" forall f g xs . mapWithKey f (mapWithKey g xs) =
  mapWithKey (\k a -> f k (g k a)) xs
"mapWithKey/map" forall f g xs . mapWithKey f (map g xs) =
  mapWithKey (\k a -> f k (g a)) xs
"map/mapWithKey" forall f g xs . map f (mapWithKey g xs) =
  mapWithKey (\k a -> f (g k a)) xs
 #-}
#endif

-- | /O(n)/.
-- @'traverseWithKey' f m == 'fromList' <$> 'traverse' (\(k, v) -> (,) k <$> f k v) ('toList' m)@
-- That is, behaves exactly like a regular 'traverse' except that the traversing
-- function also has access to the key associated with a value.
--
-- > traverseWithKey (\k v -> if odd k then Just (succ v) else Nothing) (fromList [(1, 'a'), (5, 'e')]) == Just (fromList [(1, 'b'), (5, 'f')])
-- > traverseWithKey (\k v -> if odd k then Just (succ v) else Nothing) (fromList [(2, 'c')])           == Nothing
traverseWithKey :: Applicative t => (k -> a -> t b) -> Map k a -> t (Map k b)
traverseWithKey f = go
  where
    go Tip = pure Tip
    go (Bin 1 k v _ _) = (\v' -> Bin 1 k v' Tip Tip) <$> f k v
    go (Bin s k v l r) = liftA3 (flip (Bin s k)) (go l) (f k v) (go r)
{-# INLINE traverseWithKey #-}

-- | /O(n)/. The function 'mapAccum' threads an accumulating
-- argument through the map in ascending order of keys.
--
-- > let f a b = (a ++ b, b ++ "X")
-- > mapAccum f "Everything: " (fromList [(5,"a"), (3,"b")]) == ("Everything: ba", fromList [(3, "bX"), (5, "aX")])

mapAccum :: (a -> b -> (a,c)) -> a -> Map k b -> (a,Map k c)
mapAccum f a m
  = mapAccumWithKey (\a' _ x' -> f a' x') a m

-- | /O(n)/. The function 'mapAccumWithKey' threads an accumulating
-- argument through the map in ascending order of keys.
--
-- > let f a k b = (a ++ " " ++ (show k) ++ "-" ++ b, b ++ "X")
-- > mapAccumWithKey f "Everything:" (fromList [(5,"a"), (3,"b")]) == ("Everything: 3-b 5-a", fromList [(3, "bX"), (5, "aX")])

mapAccumWithKey :: (a -> k -> b -> (a,c)) -> a -> Map k b -> (a,Map k c)
mapAccumWithKey f a t
  = mapAccumL f a t

-- | /O(n)/. The function 'mapAccumL' threads an accumulating
-- argument through the map in ascending order of keys.
mapAccumL :: (a -> k -> b -> (a,c)) -> a -> Map k b -> (a,Map k c)
mapAccumL _ a Tip               = (a,Tip)
mapAccumL f a (Bin sx kx x l r) =
  let (a1,l') = mapAccumL f a l
      (a2,x') = f a1 kx x
      (a3,r') = mapAccumL f a2 r
  in (a3,Bin sx kx x' l' r')

-- | /O(n)/. The function 'mapAccumR' threads an accumulating
-- argument through the map in descending order of keys.
mapAccumRWithKey :: (a -> k -> b -> (a,c)) -> a -> Map k b -> (a,Map k c)
mapAccumRWithKey _ a Tip = (a,Tip)
mapAccumRWithKey f a (Bin sx kx x l r) =
  let (a1,r') = mapAccumRWithKey f a r
      (a2,x') = f a1 kx x
      (a3,l') = mapAccumRWithKey f a2 l
  in (a3,Bin sx kx x' l' r')

-- | /O(n*log n)/.
-- @'mapKeys' f s@ is the map obtained by applying @f@ to each key of @s@.
--
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the value at the greatest of the
-- original keys is retained.
--
-- > mapKeys (+ 1) (fromList [(5,"a"), (3,"b")])                        == fromList [(4, "b"), (6, "a")]
-- > mapKeys (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 1 "c"
-- > mapKeys (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 3 "c"

mapKeys :: Ord k2 => (k1->k2) -> Map k1 a -> Map k2 a
mapKeys f = fromList . foldrWithKey (\k x xs -> (f k, x) : xs) []
#if __GLASGOW_HASKELL__
{-# INLINABLE mapKeys #-}
#endif

-- | /O(n*log n)/.
-- @'mapKeysWith' c f s@ is the map obtained by applying @f@ to each key of @s@.
--
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the associated values will be
-- combined using @c@.
--
-- > mapKeysWith (++) (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 1 "cdab"
-- > mapKeysWith (++) (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 3 "cdab"

mapKeysWith :: Ord k2 => (a -> a -> a) -> (k1->k2) -> Map k1 a -> Map k2 a
mapKeysWith c f = fromListWith c . foldrWithKey (\k x xs -> (f k, x) : xs) []
#if __GLASGOW_HASKELL__
{-# INLINABLE mapKeysWith #-}
#endif


-- | /O(n)/.
-- @'mapKeysMonotonic' f s == 'mapKeys' f s@, but works only when @f@
-- is strictly monotonic.
-- That is, for any values @x@ and @y@, if @x@ < @y@ then @f x@ < @f y@.
-- /The precondition is not checked./
-- Semi-formally, we have:
--
-- > and [x < y ==> f x < f y | x <- ls, y <- ls]
-- >                     ==> mapKeysMonotonic f s == mapKeys f s
-- >     where ls = keys s
--
-- This means that @f@ maps distinct original keys to distinct resulting keys.
-- This function has better performance than 'mapKeys'.
--
-- > mapKeysMonotonic (\ k -> k * 2) (fromList [(5,"a"), (3,"b")]) == fromList [(6, "b"), (10, "a")]
-- > valid (mapKeysMonotonic (\ k -> k * 2) (fromList [(5,"a"), (3,"b")])) == True
-- > valid (mapKeysMonotonic (\ _ -> 1)     (fromList [(5,"a"), (3,"b")])) == False

mapKeysMonotonic :: (k1->k2) -> Map k1 a -> Map k2 a
mapKeysMonotonic _ Tip = Tip
mapKeysMonotonic f (Bin sz k x l r) =
    Bin sz (f k) x (mapKeysMonotonic f l) (mapKeysMonotonic f r)

{--------------------------------------------------------------------
  Folds
--------------------------------------------------------------------}

-- | /O(n)/. Fold the values in the map using the given right-associative
-- binary operator, such that @'foldr' f z == 'Prelude.foldr' f z . 'elems'@.
--
-- For example,
--
-- > elems map = foldr (:) [] map
--
-- > let f a len = len + (length a)
-- > foldr f 0 (fromList [(5,"a"), (3,"bbb")]) == 4
foldr :: (a -> b -> b) -> b -> Map k a -> b
foldr f z = go z
  where
    go z' Tip             = z'
    go z' (Bin _ _ x l r) = go (f x (go z' r)) l
{-# INLINE foldr #-}

-- | /O(n)/. A strict version of 'foldr'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldr' :: (a -> b -> b) -> b -> Map k a -> b
foldr' f z = go z
  where
    go !z' Tip             = z'
    go z' (Bin _ _ x l r) = go (f x (go z' r)) l
{-# INLINE foldr' #-}

-- | /O(n)/. Fold the values in the map using the given left-associative
-- binary operator, such that @'foldl' f z == 'Prelude.foldl' f z . 'elems'@.
--
-- For example,
--
-- > elems = reverse . foldl (flip (:)) []
--
-- > let f len a = len + (length a)
-- > foldl f 0 (fromList [(5,"a"), (3,"bbb")]) == 4
foldl :: (a -> b -> a) -> a -> Map k b -> a
foldl f z = go z
  where
    go z' Tip             = z'
    go z' (Bin _ _ x l r) = go (f (go z' l) x) r
{-# INLINE foldl #-}

-- | /O(n)/. A strict version of 'foldl'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldl' :: (a -> b -> a) -> a -> Map k b -> a
foldl' f z = go z
  where
    go !z' Tip             = z'
    go z' (Bin _ _ x l r) = go (f (go z' l) x) r
{-# INLINE foldl' #-}

-- | /O(n)/. Fold the keys and values in the map using the given right-associative
-- binary operator, such that
-- @'foldrWithKey' f z == 'Prelude.foldr' ('uncurry' f) z . 'toAscList'@.
--
-- For example,
--
-- > keys map = foldrWithKey (\k x ks -> k:ks) [] map
--
-- > let f k a result = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
-- > foldrWithKey f "Map: " (fromList [(5,"a"), (3,"b")]) == "Map: (5:a)(3:b)"
foldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
foldrWithKey f z = go z
  where
    go z' Tip             = z'
    go z' (Bin _ kx x l r) = go (f kx x (go z' r)) l
{-# INLINE foldrWithKey #-}

-- | /O(n)/. A strict version of 'foldrWithKey'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldrWithKey' :: (k -> a -> b -> b) -> b -> Map k a -> b
foldrWithKey' f z = go z
  where
    go !z' Tip              = z'
    go z' (Bin _ kx x l r) = go (f kx x (go z' r)) l
{-# INLINE foldrWithKey' #-}

-- | /O(n)/. Fold the keys and values in the map using the given left-associative
-- binary operator, such that
-- @'foldlWithKey' f z == 'Prelude.foldl' (\\z' (kx, x) -> f z' kx x) z . 'toAscList'@.
--
-- For example,
--
-- > keys = reverse . foldlWithKey (\ks k x -> k:ks) []
--
-- > let f result k a = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
-- > foldlWithKey f "Map: " (fromList [(5,"a"), (3,"b")]) == "Map: (3:b)(5:a)"
foldlWithKey :: (a -> k -> b -> a) -> a -> Map k b -> a
foldlWithKey f z = go z
  where
    go z' Tip              = z'
    go z' (Bin _ kx x l r) = go (f (go z' l) kx x) r
{-# INLINE foldlWithKey #-}

-- | /O(n)/. A strict version of 'foldlWithKey'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldlWithKey' :: (a -> k -> b -> a) -> a -> Map k b -> a
foldlWithKey' f z = go z
  where
    go !z' Tip              = z'
    go z' (Bin _ kx x l r) = go (f (go z' l) kx x) r
{-# INLINE foldlWithKey' #-}

-- | /O(n)/. Fold the keys and values in the map using the given monoid, such that
--
-- @'foldMapWithKey' f = 'Prelude.fold' . 'mapWithKey' f@
--
-- This can be an asymptotically faster than 'foldrWithKey' or 'foldlWithKey' for some monoids.
foldMapWithKey :: Monoid m => (k -> a -> m) -> Map k a -> m
foldMapWithKey f = go
  where
    go Tip             = mempty
    go (Bin 1 k v _ _) = f k v
    go (Bin _ k v l r) = go l `mappend` (f k v `mappend` go r)
{-# INLINE foldMapWithKey #-}

{--------------------------------------------------------------------
  List variations
--------------------------------------------------------------------}
-- | /O(n)/.
-- Return all elements of the map in the ascending order of their keys.
-- Subject to list fusion.
--
-- > elems (fromList [(5,"a"), (3,"b")]) == ["b","a"]
-- > elems empty == []

elems :: Map k a -> [a]
elems = foldr (:) []

-- | /O(n)/. Return all keys of the map in ascending order. Subject to list
-- fusion.
--
-- > keys (fromList [(5,"a"), (3,"b")]) == [3,5]
-- > keys empty == []

keys  :: Map k a -> [k]
keys = foldrWithKey (\k _ ks -> k : ks) []

-- | /O(n)/. An alias for 'toAscList'. Return all key\/value pairs in the map
-- in ascending key order. Subject to list fusion.
--
-- > assocs (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
-- > assocs empty == []

assocs :: Map k a -> [(k,a)]
assocs m
  = toAscList m

-- | /O(n)/. The set of all keys of the map.
--
-- > keysSet (fromList [(5,"a"), (3,"b")]) == Data.Set.fromList [3,5]
-- > keysSet empty == Data.Set.empty

keysSet :: Map k a -> Set.Set k
keysSet Tip = Set.Tip
keysSet (Bin sz kx _ l r) = Set.Bin sz kx (keysSet l) (keysSet r)

-- | /O(n)/. Build a map from a set of keys and a function which for each key
-- computes its value.
--
-- > fromSet (\k -> replicate k 'a') (Data.Set.fromList [3, 5]) == fromList [(5,"aaaaa"), (3,"aaa")]
-- > fromSet undefined Data.Set.empty == empty

fromSet :: (k -> a) -> Set.Set k -> Map k a
fromSet _ Set.Tip = Tip
fromSet f (Set.Bin sz x l r) = Bin sz x (f x) (fromSet f l) (fromSet f r)

{--------------------------------------------------------------------
  Lists
  use [foldlStrict] to reduce demand on the control-stack
--------------------------------------------------------------------}
#if __GLASGOW_HASKELL__ >= 708
instance (Ord k) => GHCExts.IsList (Map k v) where
  type Item (Map k v) = (k,v)
  fromList = fromList
  toList   = toList
#endif

-- | /O(n*log n)/. Build a map from a list of key\/value pairs. See also 'fromAscList'.
-- If the list contains more than one value for the same key, the last value
-- for the key is retained.
--
-- If the keys of the list are ordered, linear-time implementation is used,
-- with the performance equal to 'fromDistinctAscList'.
--
-- > fromList [] == empty
-- > fromList [(5,"a"), (3,"b"), (5, "c")] == fromList [(5,"c"), (3,"b")]
-- > fromList [(5,"c"), (3,"b"), (5, "a")] == fromList [(5,"a"), (3,"b")]

-- For some reason, when 'singleton' is used in fromList or in
-- create, it is not inlined, so we inline it manually.
fromList :: Ord k => [(k,a)] -> Map k a
fromList [] = Tip
fromList [(kx, x)] = Bin 1 kx x Tip Tip
fromList ((kx0, x0) : xs0) | not_ordered kx0 xs0 = fromList' (Bin 1 kx0 x0 Tip Tip) xs0
                           | otherwise = go (1::Int) (Bin 1 kx0 x0 Tip Tip) xs0
  where
    not_ordered _ [] = False
    not_ordered kx ((ky,_) : _) = kx >= ky
    {-# INLINE not_ordered #-}

    fromList' t0 xs = foldlStrict ins t0 xs
      where ins t (k,x) = insert k x t

    go !_ t [] = t
    go _ t [(kx, x)] = insertMax kx x t
    go s l xs@((kx, x) : xss) | not_ordered kx xss = fromList' l xs
                              | otherwise = case create s xss of
                                  (r, ys, []) -> go (s `shiftL` 1) (link kx x l r) ys
                                  (r, _,  ys) -> fromList' (link kx x l r) ys

    -- The create is returning a triple (tree, xs, ys). Both xs and ys
    -- represent not yet processed elements and only one of them can be nonempty.
    -- If ys is nonempty, the keys in ys are not ordered with respect to tree
    -- and must be inserted using fromList'. Otherwise the keys have been
    -- ordered so far.
    create !_ [] = (Tip, [], [])
    create s xs@(xp : xss)
      | s == 1 = case xp of (kx, x) | not_ordered kx xss -> (Bin 1 kx x Tip Tip, [], xss)
                                    | otherwise -> (Bin 1 kx x Tip Tip, xss, [])
      | otherwise = case create (s `shiftR` 1) xs of
                      res@(_, [], _) -> res
                      (l, [(ky, y)], zs) -> (insertMax ky y l, [], zs)
                      (l, ys@((ky, y):yss), _) | not_ordered ky yss -> (l, [], ys)
                                               | otherwise -> case create (s `shiftR` 1) yss of
                                                   (r, zs, ws) -> (link ky y l r, zs, ws)
#if __GLASGOW_HASKELL__
{-# INLINABLE fromList #-}
#endif

-- | /O(n*log n)/. Build a map from a list of key\/value pairs with a combining function. See also 'fromAscListWith'.
--
-- > fromListWith (++) [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"a")] == fromList [(3, "ab"), (5, "aba")]
-- > fromListWith (++) [] == empty

fromListWith :: Ord k => (a -> a -> a) -> [(k,a)] -> Map k a
fromListWith f xs
  = fromListWithKey (\_ x y -> f x y) xs
#if __GLASGOW_HASKELL__
{-# INLINABLE fromListWith #-}
#endif

-- | /O(n*log n)/. Build a map from a list of key\/value pairs with a combining function. See also 'fromAscListWithKey'.
--
-- > let f k a1 a2 = (show k) ++ a1 ++ a2
-- > fromListWithKey f [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"a")] == fromList [(3, "3ab"), (5, "5a5ba")]
-- > fromListWithKey f [] == empty

fromListWithKey :: Ord k => (k -> a -> a -> a) -> [(k,a)] -> Map k a
fromListWithKey f xs
  = foldlStrict ins empty xs
  where
    ins t (k,x) = insertWithKey f k x t
#if __GLASGOW_HASKELL__
{-# INLINABLE fromListWithKey #-}
#endif

-- | /O(n)/. Convert the map to a list of key\/value pairs. Subject to list fusion.
--
-- > toList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
-- > toList empty == []

toList :: Map k a -> [(k,a)]
toList = toAscList

-- | /O(n)/. Convert the map to a list of key\/value pairs where the keys are
-- in ascending order. Subject to list fusion.
--
-- > toAscList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]

toAscList :: Map k a -> [(k,a)]
toAscList = foldrWithKey (\k x xs -> (k,x):xs) []

-- | /O(n)/. Convert the map to a list of key\/value pairs where the keys
-- are in descending order. Subject to list fusion.
--
-- > toDescList (fromList [(5,"a"), (3,"b")]) == [(5,"a"), (3,"b")]

toDescList :: Map k a -> [(k,a)]
toDescList = foldlWithKey (\xs k x -> (k,x):xs) []

-- List fusion for the list generating functions.
#if __GLASGOW_HASKELL__
-- The foldrFB and foldlFB are fold{r,l}WithKey equivalents, used for list fusion.
-- They are important to convert unfused methods back, see mapFB in prelude.
foldrFB :: (k -> a -> b -> b) -> b -> Map k a -> b
foldrFB = foldrWithKey
{-# INLINE[0] foldrFB #-}
foldlFB :: (a -> k -> b -> a) -> a -> Map k b -> a
foldlFB = foldlWithKey
{-# INLINE[0] foldlFB #-}

-- Inline assocs and toList, so that we need to fuse only toAscList.
{-# INLINE assocs #-}
{-# INLINE toList #-}

-- The fusion is enabled up to phase 2 included. If it does not succeed,
-- convert in phase 1 the expanded elems,keys,to{Asc,Desc}List calls back to
-- elems,keys,to{Asc,Desc}List.  In phase 0, we inline fold{lr}FB (which were
-- used in a list fusion, otherwise it would go away in phase 1), and let compiler
-- do whatever it wants with elems,keys,to{Asc,Desc}List -- it was forbidden to
-- inline it before phase 0, otherwise the fusion rules would not fire at all.
{-# NOINLINE[0] elems #-}
{-# NOINLINE[0] keys #-}
{-# NOINLINE[0] toAscList #-}
{-# NOINLINE[0] toDescList #-}
{-# RULES "Map.elems" [~1] forall m . elems m = build (\c n -> foldrFB (\_ x xs -> c x xs) n m) #-}
{-# RULES "Map.elemsBack" [1] foldrFB (\_ x xs -> x : xs) [] = elems #-}
{-# RULES "Map.keys" [~1] forall m . keys m = build (\c n -> foldrFB (\k _ xs -> c k xs) n m) #-}
{-# RULES "Map.keysBack" [1] foldrFB (\k _ xs -> k : xs) [] = keys #-}
{-# RULES "Map.toAscList" [~1] forall m . toAscList m = build (\c n -> foldrFB (\k x xs -> c (k,x) xs) n m) #-}
{-# RULES "Map.toAscListBack" [1] foldrFB (\k x xs -> (k, x) : xs) [] = toAscList #-}
{-# RULES "Map.toDescList" [~1] forall m . toDescList m = build (\c n -> foldlFB (\xs k x -> c (k,x) xs) n m) #-}
{-# RULES "Map.toDescListBack" [1] foldlFB (\xs k x -> (k, x) : xs) [] = toDescList #-}
#endif

{--------------------------------------------------------------------
  Building trees from ascending/descending lists can be done in linear time.

  Note that if [xs] is ascending that:
    fromAscList xs       == fromList xs
    fromAscListWith f xs == fromListWith f xs
--------------------------------------------------------------------}
-- | /O(n)/. Build a map from an ascending list in linear time.
-- /The precondition (input list is ascending) is not checked./
--
-- > fromAscList [(3,"b"), (5,"a")]          == fromList [(3, "b"), (5, "a")]
-- > fromAscList [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "b")]
-- > valid (fromAscList [(3,"b"), (5,"a"), (5,"b")]) == True
-- > valid (fromAscList [(5,"a"), (3,"b"), (5,"b")]) == False

fromAscList :: Eq k => [(k,a)] -> Map k a
fromAscList xs
  = fromDistinctAscList (combineEq xs)
  where
  -- [combineEq f xs] combines equal elements with function [f] in an ordered list [xs]
  combineEq xs'
    = case xs' of
        []     -> []
        [x]    -> [x]
        (x:xx) -> combineEq' x xx

  combineEq' z [] = [z]
  combineEq' z@(kz,_) (x@(kx,xx):xs')
    | kx==kz    = combineEq' (kx,xx) xs'
    | otherwise = z:combineEq' x xs'
#if __GLASGOW_HASKELL__
{-# INLINABLE fromAscList #-}
#endif

-- | /O(n)/. Build a map from a descending list in linear time.
-- /The precondition (input list is descending) is not checked./
--
-- > fromDescList [(5,"a"), (3,"b")]          == fromList [(3, "b"), (5, "a")]
-- > fromDescList [(5,"a"), (5,"b"), (3,"b")] == fromList [(3, "b"), (5, "b")]
-- > valid (fromDescList [(5,"a"), (5,"b"), (3,"b")]) == True
-- > valid (fromDescList [(5,"a"), (3,"b"), (5,"b")]) == False

fromDescList :: Eq k => [(k,a)] -> Map k a
fromDescList xs = fromDistinctDescList (combineEq xs)
  where
  -- [combineEq f xs] combines equal elements with function [f] in an ordered list [xs]
  combineEq xs'
    = case xs' of
        []     -> []
        [x]    -> [x]
        (x:xx) -> combineEq' x xx

  combineEq' z [] = [z]
  combineEq' z@(kz,_) (x@(kx,xx):xs')
    | kx==kz    = combineEq' (kx,xx) xs'
    | otherwise = z:combineEq' x xs'
#if __GLASGOW_HASKELL__
{-# INLINABLE fromDescList #-}
#endif

-- | /O(n)/. Build a map from an ascending list in linear time with a combining function for equal keys.
-- /The precondition (input list is ascending) is not checked./
--
-- > fromAscListWith (++) [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "ba")]
-- > valid (fromAscListWith (++) [(3,"b"), (5,"a"), (5,"b")]) == True
-- > valid (fromAscListWith (++) [(5,"a"), (3,"b"), (5,"b")]) == False

fromAscListWith :: Eq k => (a -> a -> a) -> [(k,a)] -> Map k a
fromAscListWith f xs
  = fromAscListWithKey (\_ x y -> f x y) xs
#if __GLASGOW_HASKELL__
{-# INLINABLE fromAscListWith #-}
#endif

-- | /O(n)/. Build a map from a descending list in linear time with a combining function for equal keys.
-- /The precondition (input list is descending) is not checked./
--
-- > fromDescListWith (++) [(5,"a"), (5,"b"), (3,"b")] == fromList [(3, "b"), (5, "ba")]
-- > valid (fromDescListWith (++) [(5,"a"), (5,"b"), (3,"b")]) == True
-- > valid (fromDescListWith (++) [(5,"a"), (3,"b"), (5,"b")]) == False

fromDescListWith :: Eq k => (a -> a -> a) -> [(k,a)] -> Map k a
fromDescListWith f xs
  = fromDescListWithKey (\_ x y -> f x y) xs
#if __GLASGOW_HASKELL__
{-# INLINABLE fromDescListWith #-}
#endif

-- | /O(n)/. Build a map from an ascending list in linear time with a
-- combining function for equal keys.
-- /The precondition (input list is ascending) is not checked./
--
-- > let f k a1 a2 = (show k) ++ ":" ++ a1 ++ a2
-- > fromAscListWithKey f [(3,"b"), (5,"a"), (5,"b"), (5,"b")] == fromList [(3, "b"), (5, "5:b5:ba")]
-- > valid (fromAscListWithKey f [(3,"b"), (5,"a"), (5,"b"), (5,"b")]) == True
-- > valid (fromAscListWithKey f [(5,"a"), (3,"b"), (5,"b"), (5,"b")]) == False

fromAscListWithKey :: Eq k => (k -> a -> a -> a) -> [(k,a)] -> Map k a
fromAscListWithKey f xs
  = fromDistinctAscList (combineEq f xs)
  where
  -- [combineEq f xs] combines equal elements with function [f] in an ordered list [xs]
  combineEq _ xs'
    = case xs' of
        []     -> []
        [x]    -> [x]
        (x:xx) -> combineEq' x xx

  combineEq' z [] = [z]
  combineEq' z@(kz,zz) (x@(kx,xx):xs')
    | kx==kz    = let yy = f kx xx zz in combineEq' (kx,yy) xs'
    | otherwise = z:combineEq' x xs'
#if __GLASGOW_HASKELL__
{-# INLINABLE fromAscListWithKey #-}
#endif

-- | /O(n)/. Build a map from a descending list in linear time with a
-- combining function for equal keys.
-- /The precondition (input list is descending) is not checked./
--
-- > let f k a1 a2 = (show k) ++ ":" ++ a1 ++ a2
-- > fromDescListWithKey f [(5,"a"), (5,"b"), (5,"b"), (3,"b")] == fromList [(3, "b"), (5, "5:b5:ba")]
-- > valid (fromDescListWithKey f [(5,"a"), (5,"b"), (5,"b"), (3,"b")]) == True
-- > valid (fromDescListWithKey f [(5,"a"), (3,"b"), (5,"b"), (5,"b")]) == False
fromDescListWithKey :: Eq k => (k -> a -> a -> a) -> [(k,a)] -> Map k a
fromDescListWithKey f xs
  = fromDistinctDescList (combineEq f xs)
  where
  -- [combineEq f xs] combines equal elements with function [f] in an ordered list [xs]
  combineEq _ xs'
    = case xs' of
        []     -> []
        [x]    -> [x]
        (x:xx) -> combineEq' x xx

  combineEq' z [] = [z]
  combineEq' z@(kz,zz) (x@(kx,xx):xs')
    | kx==kz    = let yy = f kx xx zz in combineEq' (kx,yy) xs'
    | otherwise = z:combineEq' x xs'
#if __GLASGOW_HASKELL__
{-# INLINABLE fromDescListWithKey #-}
#endif


-- | /O(n)/. Build a map from an ascending list of distinct elements in linear time.
-- /The precondition is not checked./
--
-- > fromDistinctAscList [(3,"b"), (5,"a")] == fromList [(3, "b"), (5, "a")]
-- > valid (fromDistinctAscList [(3,"b"), (5,"a")])          == True
-- > valid (fromDistinctAscList [(3,"b"), (5,"a"), (5,"b")]) == False

-- For some reason, when 'singleton' is used in fromDistinctAscList or in
-- create, it is not inlined, so we inline it manually.
fromDistinctAscList :: [(k,a)] -> Map k a
fromDistinctAscList [] = Tip
fromDistinctAscList ((kx0, x0) : xs0) = go (1::Int) (Bin 1 kx0 x0 Tip Tip) xs0
  where
    go !_ t [] = t
    go s l ((kx, x) : xs) = case create s xs of
                                (r :*: ys) -> let !t' = link kx x l r
                                              in go (s `shiftL` 1) t' ys

    create !_ [] = (Tip :*: [])
    create s xs@(x' : xs')
      | s == 1 = case x' of (kx, x) -> (Bin 1 kx x Tip Tip :*: xs')
      | otherwise = case create (s `shiftR` 1) xs of
                      res@(_ :*: []) -> res
                      (l :*: (ky, y):ys) -> case create (s `shiftR` 1) ys of
                        (r :*: zs) -> (link ky y l r :*: zs)

-- | /O(n)/. Build a map from a descending list of distinct elements in linear time.
-- /The precondition is not checked./
--
-- > fromDistinctDescList [(5,"a"), (3,"b")] == fromList [(3, "b"), (5, "a")]
-- > valid (fromDistinctDescList [(5,"a"), (3,"b")])          == True
-- > valid (fromDistinctDescList [(5,"a"), (5,"b"), (3,"b")]) == False

-- For some reason, when 'singleton' is used in fromDistinctDescList or in
-- create, it is not inlined, so we inline it manually.
fromDistinctDescList :: [(k,a)] -> Map k a
fromDistinctDescList [] = Tip
fromDistinctDescList ((kx0, x0) : xs0) = go (1 :: Int) (Bin 1 kx0 x0 Tip Tip) xs0
  where
     go !_ t [] = t
     go s r ((kx, x) : xs) = case create s xs of
                               (l :*: ys) -> let !t' = link kx x l r
                                             in go (s `shiftL` 1) t' ys

     create !_ [] = (Tip :*: [])
     create s xs@(x' : xs')
       | s == 1 = case x' of (kx, x) -> (Bin 1 kx x Tip Tip :*: xs')
       | otherwise = case create (s `shiftR` 1) xs of
                       res@(_ :*: []) -> res
                       (r :*: (ky, y):ys) -> case create (s `shiftR` 1) ys of
                         (l :*: zs) -> (link ky y l r :*: zs)

{-
-- Functions very similar to these were used to implement
-- hedge union, intersection, and difference algorithms that we no
-- longer use. These functions, however, seem likely to be useful
-- in their own right, so I'm leaving them here in case we end up
-- exporting them.

{--------------------------------------------------------------------
  [filterGt b t] filter all keys >[b] from tree [t]
  [filterLt b t] filter all keys <[b] from tree [t]
--------------------------------------------------------------------}
filterGt :: Ord k => k -> Map k v -> Map k v
filterGt !_ Tip = Tip
filterGt !b (Bin _ kx x l r) =
  case compare b kx of LT -> link kx x (filterGt b l) r
                       EQ -> r
                       GT -> filterGt b r
#if __GLASGOW_HASKELL__
{-# INLINABLE filterGt #-}
#endif

filterLt :: Ord k => k -> Map k v -> Map k v
filterLt !_ Tip = Tip
filterLt !b (Bin _ kx x l r) =
  case compare kx b of LT -> link kx x l (filterLt b r)
                       EQ -> l
                       GT -> filterLt b l
#if __GLASGOW_HASKELL__
{-# INLINABLE filterLt #-}
#endif
-}

{--------------------------------------------------------------------
  Split
--------------------------------------------------------------------}
-- | /O(log n)/. The expression (@'split' k map@) is a pair @(map1,map2)@ where
-- the keys in @map1@ are smaller than @k@ and the keys in @map2@ larger than @k@.
-- Any key equal to @k@ is found in neither @map1@ nor @map2@.
--
-- > split 2 (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3,"b"), (5,"a")])
-- > split 3 (fromList [(5,"a"), (3,"b")]) == (empty, singleton 5 "a")
-- > split 4 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", singleton 5 "a")
-- > split 5 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", empty)
-- > split 6 (fromList [(5,"a"), (3,"b")]) == (fromList [(3,"b"), (5,"a")], empty)

split :: Ord k => k -> Map k a -> (Map k a,Map k a)
split !k0 t0 = toPair $ go k0 t0
  where
    go k t =
      case t of
        Tip            -> Tip :*: Tip
        Bin _ kx x l r -> case compare k kx of
          LT -> let (lt :*: gt) = go k l in lt :*: link kx x gt r
          GT -> let (lt :*: gt) = go k r in link kx x l lt :*: gt
          EQ -> (l :*: r)
#if __GLASGOW_HASKELL__
{-# INLINABLE split #-}
#endif

-- | /O(log n)/. The expression (@'splitLookup' k map@) splits a map just
-- like 'split' but also returns @'lookup' k map@.
--
-- > splitLookup 2 (fromList [(5,"a"), (3,"b")]) == (empty, Nothing, fromList [(3,"b"), (5,"a")])
-- > splitLookup 3 (fromList [(5,"a"), (3,"b")]) == (empty, Just "b", singleton 5 "a")
-- > splitLookup 4 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", Nothing, singleton 5 "a")
-- > splitLookup 5 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", Just "a", empty)
-- > splitLookup 6 (fromList [(5,"a"), (3,"b")]) == (fromList [(3,"b"), (5,"a")], Nothing, empty)
splitLookup :: Ord k => k -> Map k a -> (Map k a,Maybe a,Map k a)
splitLookup k0 m = case go k0 m of
     StrictTriple l mv r -> (l, mv, r)
  where
    go :: Ord k => k -> Map k a -> StrictTriple (Map k a) (Maybe a) (Map k a)
    go !k t =
      case t of
        Tip            -> StrictTriple Tip Nothing Tip
        Bin _ kx x l r -> case compare k kx of
          LT -> let StrictTriple lt z gt = go k l
                    !gt' = link kx x gt r
                in StrictTriple lt z gt'
          GT -> let StrictTriple lt z gt = go k r
                    !lt' = link kx x l lt
                in StrictTriple lt' z gt
          EQ -> StrictTriple l (Just x) r
#if __GLASGOW_HASKELL__
{-# INLINABLE splitLookup #-}
#endif

-- | A variant of 'splitLookup' that indicates only whether the
-- key was present, rather than producing its value. This is used to
-- implement 'intersection' to avoid allocating unnecessary 'Just'
-- constructors.
splitMember :: Ord k => k -> Map k a -> (Map k a,Bool,Map k a)
splitMember k0 m = case go k0 m of
     StrictTriple l mv r -> (l, mv, r)
  where
    go :: Ord k => k -> Map k a -> StrictTriple (Map k a) Bool (Map k a)
    go !k t =
      case t of
        Tip            -> StrictTriple Tip False Tip
        Bin _ kx x l r -> case compare k kx of
          LT -> let StrictTriple lt z gt = go k l
                    !gt' = link kx x gt r
                in StrictTriple lt z gt'
          GT -> let StrictTriple lt z gt = go k r
                    !lt' = link kx x l lt
                in StrictTriple lt' z gt
          EQ -> StrictTriple l True r
#if __GLASGOW_HASKELL__
{-# INLINABLE splitMember #-}
#endif

data StrictTriple a b c = StrictTriple !a !b !c

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
    [link k x l r]    Restores balance and size.

  Furthermore, we can construct a new tree from two trees. Both operations
  assume that all values in [l] < all values in [r] and that [l] and [r]
  are valid:
    [glue l r]        Glues [l] and [r] together. Assumes that [l] and
                      [r] are already balanced with respect to each other.
    [link2 l r]       Merges two trees and restores balance.
--------------------------------------------------------------------}

{--------------------------------------------------------------------
  Link
--------------------------------------------------------------------}
link :: k -> a -> Map k a -> Map k a -> Map k a
link kx x Tip r  = insertMin kx x r
link kx x l Tip  = insertMax kx x l
link kx x l@(Bin sizeL ky y ly ry) r@(Bin sizeR kz z lz rz)
  | delta*sizeL < sizeR  = balanceL kz z (link kx x l lz) rz
  | delta*sizeR < sizeL  = balanceR ky y ly (link kx x ry r)
  | otherwise            = bin kx x l r


-- insertMin and insertMax don't perform potentially expensive comparisons.
insertMax,insertMin :: k -> a -> Map k a -> Map k a
insertMax kx x t
  = case t of
      Tip -> singleton kx x
      Bin _ ky y l r
          -> balanceR ky y l (insertMax kx x r)

insertMin kx x t
  = case t of
      Tip -> singleton kx x
      Bin _ ky y l r
          -> balanceL ky y (insertMin kx x l) r

{--------------------------------------------------------------------
  [link2 l r]: merges two trees.
--------------------------------------------------------------------}
link2 :: Map k a -> Map k a -> Map k a
link2 Tip r   = r
link2 l Tip   = l
link2 l@(Bin sizeL kx x lx rx) r@(Bin sizeR ky y ly ry)
  | delta*sizeL < sizeR = balanceL ky y (link2 l ly) ry
  | delta*sizeR < sizeL = balanceR kx x lx (link2 rx r)
  | otherwise           = glue l r

{--------------------------------------------------------------------
  [glue l r]: glues two trees together.
  Assumes that [l] and [r] are already balanced with respect to each other.
--------------------------------------------------------------------}
glue :: Map k a -> Map k a -> Map k a
glue Tip r = r
glue l Tip = l
glue l@(Bin sl kl xl ll lr) r@(Bin sr kr xr rl rr)
  | sl > sr = let !(MaxView km m l') = maxViewSure kl xl ll lr in balanceR km m l' r
  | otherwise = let !(MinView km m r') = minViewSure kr xr rl rr in balanceL km m l r'

data MinView k a = MinView !k a !(Map k a)
data MaxView k a = MaxView !k a !(Map k a)

minViewSure :: k -> a -> Map k a -> Map k a -> MinView k a
minViewSure = go
  where
    go k x Tip r = MinView k x r
    go k x (Bin _ kl xl ll lr) r =
      case go kl xl ll lr of
        MinView km xm l' -> MinView km xm (balanceR k x l' r)

maxViewSure :: k -> a -> Map k a -> Map k a -> MaxView k a
maxViewSure = go
  where
    go k x l Tip = MaxView k x l
    go k x l (Bin _ kr xr rl rr) =
      case go kr xr rl rr of
        MaxView km xm r' -> MaxView km xm (balanceL k x l r')

-- | /O(log n)/. Delete and find the minimal element.
--
-- > deleteFindMin (fromList [(5,"a"), (3,"b"), (10,"c")]) == ((3,"b"), fromList[(5,"a"), (10,"c")])
-- > deleteFindMin                                            Error: can not return the minimal element of an empty map

deleteFindMin :: Map k a -> ((k,a),Map k a)
deleteFindMin t = case minViewWithKey t of
  Nothing -> (error "Map.deleteFindMin: can not return the minimal element of an empty map", Tip)
  Just res -> res

-- | /O(log n)/. Delete and find the maximal element.
--
-- > deleteFindMax (fromList [(5,"a"), (3,"b"), (10,"c")]) == ((10,"c"), fromList [(3,"b"), (5,"a")])
-- > deleteFindMax empty                                      Error: can not return the maximal element of an empty map

deleteFindMax :: Map k a -> ((k,a),Map k a)
deleteFindMax t = case maxViewWithKey t of
  Nothing -> (error "Map.deleteFindMax: can not return the maximal element of an empty map", Tip)
  Just res -> res

{--------------------------------------------------------------------
  [balance l x r] balances two trees with value x.
  The sizes of the trees should balance after decreasing the
  size of one of them. (a rotation).

  [delta] is the maximal relative difference between the sizes of
          two trees, it corresponds with the [w] in Adams' paper.
  [ratio] is the ratio between an outer and inner sibling of the
          heavier subtree in an unbalanced setting. It determines
          whether a double or single rotation should be performed
          to restore balance. It is corresponds with the inverse
          of $\alpha$ in Adam's article.

  Note that according to the Adam's paper:
  - [delta] should be larger than 4.646 with a [ratio] of 2.
  - [delta] should be larger than 3.745 with a [ratio] of 1.534.

  But the Adam's paper is erroneous:
  - It can be proved that for delta=2 and delta>=5 there does
    not exist any ratio that would work.
  - Delta=4.5 and ratio=2 does not work.

  That leaves two reasonable variants, delta=3 and delta=4,
  both with ratio=2.

  - A lower [delta] leads to a more 'perfectly' balanced tree.
  - A higher [delta] performs less rebalancing.

  In the benchmarks, delta=3 is faster on insert operations,
  and delta=4 has slightly better deletes. As the insert speedup
  is larger, we currently use delta=3.

--------------------------------------------------------------------}
delta,ratio :: Int
delta = 3
ratio = 2

-- The balance function is equivalent to the following:
--
--   balance :: k -> a -> Map k a -> Map k a -> Map k a
--   balance k x l r
--     | sizeL + sizeR <= 1    = Bin sizeX k x l r
--     | sizeR > delta*sizeL   = rotateL k x l r
--     | sizeL > delta*sizeR   = rotateR k x l r
--     | otherwise             = Bin sizeX k x l r
--     where
--       sizeL = size l
--       sizeR = size r
--       sizeX = sizeL + sizeR + 1
--
--   rotateL :: a -> b -> Map a b -> Map a b -> Map a b
--   rotateL k x l r@(Bin _ _ _ ly ry) | size ly < ratio*size ry = singleL k x l r
--                                     | otherwise               = doubleL k x l r
--
--   rotateR :: a -> b -> Map a b -> Map a b -> Map a b
--   rotateR k x l@(Bin _ _ _ ly ry) r | size ry < ratio*size ly = singleR k x l r
--                                     | otherwise               = doubleR k x l r
--
--   singleL, singleR :: a -> b -> Map a b -> Map a b -> Map a b
--   singleL k1 x1 t1 (Bin _ k2 x2 t2 t3)  = bin k2 x2 (bin k1 x1 t1 t2) t3
--   singleR k1 x1 (Bin _ k2 x2 t1 t2) t3  = bin k2 x2 t1 (bin k1 x1 t2 t3)
--
--   doubleL, doubleR :: a -> b -> Map a b -> Map a b -> Map a b
--   doubleL k1 x1 t1 (Bin _ k2 x2 (Bin _ k3 x3 t2 t3) t4) = bin k3 x3 (bin k1 x1 t1 t2) (bin k2 x2 t3 t4)
--   doubleR k1 x1 (Bin _ k2 x2 t1 (Bin _ k3 x3 t2 t3)) t4 = bin k3 x3 (bin k2 x2 t1 t2) (bin k1 x1 t3 t4)
--
-- It is only written in such a way that every node is pattern-matched only once.

balance :: k -> a -> Map k a -> Map k a -> Map k a
balance k x l r = case l of
  Tip -> case r of
           Tip -> Bin 1 k x Tip Tip
           (Bin _ _ _ Tip Tip) -> Bin 2 k x Tip r
           (Bin _ rk rx Tip rr@(Bin _ _ _ _ _)) -> Bin 3 rk rx (Bin 1 k x Tip Tip) rr
           (Bin _ rk rx (Bin _ rlk rlx _ _) Tip) -> Bin 3 rlk rlx (Bin 1 k x Tip Tip) (Bin 1 rk rx Tip Tip)
           (Bin rs rk rx rl@(Bin rls rlk rlx rll rlr) rr@(Bin rrs _ _ _ _))
             | rls < ratio*rrs -> Bin (1+rs) rk rx (Bin (1+rls) k x Tip rl) rr
             | otherwise -> Bin (1+rs) rlk rlx (Bin (1+size rll) k x Tip rll) (Bin (1+rrs+size rlr) rk rx rlr rr)

  (Bin ls lk lx ll lr) -> case r of
           Tip -> case (ll, lr) of
                    (Tip, Tip) -> Bin 2 k x l Tip
                    (Tip, (Bin _ lrk lrx _ _)) -> Bin 3 lrk lrx (Bin 1 lk lx Tip Tip) (Bin 1 k x Tip Tip)
                    ((Bin _ _ _ _ _), Tip) -> Bin 3 lk lx ll (Bin 1 k x Tip Tip)
                    ((Bin lls _ _ _ _), (Bin lrs lrk lrx lrl lrr))
                      | lrs < ratio*lls -> Bin (1+ls) lk lx ll (Bin (1+lrs) k x lr Tip)
                      | otherwise -> Bin (1+ls) lrk lrx (Bin (1+lls+size lrl) lk lx ll lrl) (Bin (1+size lrr) k x lrr Tip)
           (Bin rs rk rx rl rr)
              | rs > delta*ls  -> case (rl, rr) of
                   (Bin rls rlk rlx rll rlr, Bin rrs _ _ _ _)
                     | rls < ratio*rrs -> Bin (1+ls+rs) rk rx (Bin (1+ls+rls) k x l rl) rr
                     | otherwise -> Bin (1+ls+rs) rlk rlx (Bin (1+ls+size rll) k x l rll) (Bin (1+rrs+size rlr) rk rx rlr rr)
                   (_, _) -> error "Failure in Data.Map.balance"
              | ls > delta*rs  -> case (ll, lr) of
                   (Bin lls _ _ _ _, Bin lrs lrk lrx lrl lrr)
                     | lrs < ratio*lls -> Bin (1+ls+rs) lk lx ll (Bin (1+rs+lrs) k x lr r)
                     | otherwise -> Bin (1+ls+rs) lrk lrx (Bin (1+lls+size lrl) lk lx ll lrl) (Bin (1+rs+size lrr) k x lrr r)
                   (_, _) -> error "Failure in Data.Map.balance"
              | otherwise -> Bin (1+ls+rs) k x l r
{-# NOINLINE balance #-}

-- Functions balanceL and balanceR are specialised versions of balance.
-- balanceL only checks whether the left subtree is too big,
-- balanceR only checks whether the right subtree is too big.

-- balanceL is called when left subtree might have been inserted to or when
-- right subtree might have been deleted from.
balanceL :: k -> a -> Map k a -> Map k a -> Map k a
balanceL k x l r = case r of
  Tip -> case l of
           Tip -> Bin 1 k x Tip Tip
           (Bin _ _ _ Tip Tip) -> Bin 2 k x l Tip
           (Bin _ lk lx Tip (Bin _ lrk lrx _ _)) -> Bin 3 lrk lrx (Bin 1 lk lx Tip Tip) (Bin 1 k x Tip Tip)
           (Bin _ lk lx ll@(Bin _ _ _ _ _) Tip) -> Bin 3 lk lx ll (Bin 1 k x Tip Tip)
           (Bin ls lk lx ll@(Bin lls _ _ _ _) lr@(Bin lrs lrk lrx lrl lrr))
             | lrs < ratio*lls -> Bin (1+ls) lk lx ll (Bin (1+lrs) k x lr Tip)
             | otherwise -> Bin (1+ls) lrk lrx (Bin (1+lls+size lrl) lk lx ll lrl) (Bin (1+size lrr) k x lrr Tip)

  (Bin rs _ _ _ _) -> case l of
           Tip -> Bin (1+rs) k x Tip r

           (Bin ls lk lx ll lr)
              | ls > delta*rs  -> case (ll, lr) of
                   (Bin lls _ _ _ _, Bin lrs lrk lrx lrl lrr)
                     | lrs < ratio*lls -> Bin (1+ls+rs) lk lx ll (Bin (1+rs+lrs) k x lr r)
                     | otherwise -> Bin (1+ls+rs) lrk lrx (Bin (1+lls+size lrl) lk lx ll lrl) (Bin (1+rs+size lrr) k x lrr r)
                   (_, _) -> error "Failure in Data.Map.balanceL"
              | otherwise -> Bin (1+ls+rs) k x l r
{-# NOINLINE balanceL #-}

-- balanceR is called when right subtree might have been inserted to or when
-- left subtree might have been deleted from.
balanceR :: k -> a -> Map k a -> Map k a -> Map k a
balanceR k x l r = case l of
  Tip -> case r of
           Tip -> Bin 1 k x Tip Tip
           (Bin _ _ _ Tip Tip) -> Bin 2 k x Tip r
           (Bin _ rk rx Tip rr@(Bin _ _ _ _ _)) -> Bin 3 rk rx (Bin 1 k x Tip Tip) rr
           (Bin _ rk rx (Bin _ rlk rlx _ _) Tip) -> Bin 3 rlk rlx (Bin 1 k x Tip Tip) (Bin 1 rk rx Tip Tip)
           (Bin rs rk rx rl@(Bin rls rlk rlx rll rlr) rr@(Bin rrs _ _ _ _))
             | rls < ratio*rrs -> Bin (1+rs) rk rx (Bin (1+rls) k x Tip rl) rr
             | otherwise -> Bin (1+rs) rlk rlx (Bin (1+size rll) k x Tip rll) (Bin (1+rrs+size rlr) rk rx rlr rr)

  (Bin ls _ _ _ _) -> case r of
           Tip -> Bin (1+ls) k x l Tip

           (Bin rs rk rx rl rr)
              | rs > delta*ls  -> case (rl, rr) of
                   (Bin rls rlk rlx rll rlr, Bin rrs _ _ _ _)
                     | rls < ratio*rrs -> Bin (1+ls+rs) rk rx (Bin (1+ls+rls) k x l rl) rr
                     | otherwise -> Bin (1+ls+rs) rlk rlx (Bin (1+ls+size rll) k x l rll) (Bin (1+rrs+size rlr) rk rx rlr rr)
                   (_, _) -> error "Failure in Data.Map.balanceR"
              | otherwise -> Bin (1+ls+rs) k x l r
{-# NOINLINE balanceR #-}


{--------------------------------------------------------------------
  The bin constructor maintains the size of the tree
--------------------------------------------------------------------}
bin :: k -> a -> Map k a -> Map k a -> Map k a
bin k x l r
  = Bin (size l + size r + 1) k x l r
{-# INLINE bin #-}


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

#if MIN_VERSION_base(4,9,0)
{--------------------------------------------------------------------
  Lifted instances
--------------------------------------------------------------------}

instance Eq2 Map where
    liftEq2 eqk eqv m n =
        size m == size n && liftEq (liftEq2 eqk eqv) (toList m) (toList n)

instance Eq k => Eq1 (Map k) where
    liftEq = liftEq2 (==)

instance Ord2 Map where
    liftCompare2 cmpk cmpv m n =
        liftCompare (liftCompare2 cmpk cmpv) (toList m) (toList n)

instance Ord k => Ord1 (Map k) where
    liftCompare = liftCompare2 compare

instance Show2 Map where
    liftShowsPrec2 spk slk spv slv d m =
        showsUnaryWith (liftShowsPrec sp sl) "fromList" d (toList m)
      where
        sp = liftShowsPrec2 spk slk spv slv
        sl = liftShowList2 spk slk spv slv

instance Show k => Show1 (Map k) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Ord k, Read k) => Read1 (Map k) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp' rl') "fromList" fromList
      where
        rp' = liftReadsPrec rp rl
        rl' = liftReadList rp rl
#endif

{--------------------------------------------------------------------
  Functor
--------------------------------------------------------------------}
instance Functor (Map k) where
  fmap f m  = map f m
#ifdef __GLASGOW_HASKELL__
  _ <$ Tip = Tip
  a <$ (Bin sx kx _ l r) = Bin sx kx a (a <$ l) (a <$ r)
#endif

instance Traversable (Map k) where
  traverse f = traverseWithKey (\_ -> f)
  {-# INLINE traverse #-}

instance Foldable.Foldable (Map k) where
  fold = go
    where go Tip = mempty
          go (Bin 1 _ v _ _) = v
          go (Bin _ _ v l r) = go l `mappend` (v `mappend` go r)
  {-# INLINABLE fold #-}
  foldr = foldr
  {-# INLINE foldr #-}
  foldl = foldl
  {-# INLINE foldl #-}
  foldMap f t = go t
    where go Tip = mempty
          go (Bin 1 _ v _ _) = f v
          go (Bin _ _ v l r) = go l `mappend` (f v `mappend` go r)
  {-# INLINE foldMap #-}

#if MIN_VERSION_base(4,6,0)
  foldl' = foldl'
  {-# INLINE foldl' #-}
  foldr' = foldr'
  {-# INLINE foldr' #-}
#endif
#if MIN_VERSION_base(4,8,0)
  length = size
  {-# INLINE length #-}
  null   = null
  {-# INLINE null #-}
  toList = elems -- NB: Foldable.toList /= Map.toList
  {-# INLINE toList #-}
  elem = go
    where go !_ Tip = False
          go x (Bin _ _ v l r) = x == v || go x l || go x r
  {-# INLINABLE elem #-}
  maximum = start
    where start Tip = error "Data.Foldable.maximum (for Data.Map): empty map"
          start (Bin _ _ v l r) = go (go v l) r

          go !m Tip = m
          go m (Bin _ _ v l r) = go (go (max m v) l) r
  {-# INLINABLE maximum #-}
  minimum = start
    where start Tip = error "Data.Foldable.minimum (for Data.Map): empty map"
          start (Bin _ _ v l r) = go (go v l) r

          go !m Tip = m
          go m (Bin _ _ v l r) = go (go (min m v) l) r
  {-# INLINABLE minimum #-}
  sum = foldl' (+) 0
  {-# INLINABLE sum #-}
  product = foldl' (*) 1
  {-# INLINABLE product #-}
#endif

instance (NFData k, NFData a) => NFData (Map k a) where
    rnf Tip = ()
    rnf (Bin _ kx x l r) = rnf kx `seq` rnf x `seq` rnf l `seq` rnf r

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

{--------------------------------------------------------------------
  Show
--------------------------------------------------------------------}
instance (Show k, Show a) => Show (Map k a) where
  showsPrec d m  = showParen (d > 10) $
    showString "fromList " . shows (toList m)

{--------------------------------------------------------------------
  Typeable
--------------------------------------------------------------------}

INSTANCE_TYPEABLE2(Map)

{--------------------------------------------------------------------
  Utilities
--------------------------------------------------------------------}

-- | /O(1)/.  Decompose a map into pieces based on the structure of the underlying
-- tree.  This function is useful for consuming a map in parallel.
--
-- No guarantee is made as to the sizes of the pieces; an internal, but
-- deterministic process determines this.  However, it is guaranteed that the pieces
-- returned will be in ascending order (all elements in the first submap less than all
-- elements in the second, and so on).
--
-- Examples:
--
-- > splitRoot (fromList (zip [1..6] ['a'..])) ==
-- >   [fromList [(1,'a'),(2,'b'),(3,'c')],fromList [(4,'d')],fromList [(5,'e'),(6,'f')]]
--
-- > splitRoot empty == []
--
--  Note that the current implementation does not return more than three submaps,
--  but you should not depend on this behaviour because it can change in the
--  future without notice.
splitRoot :: Map k b -> [Map k b]
splitRoot orig =
  case orig of
    Tip           -> []
    Bin _ k v l r -> [l, singleton k v, r]
{-# INLINE splitRoot #-}

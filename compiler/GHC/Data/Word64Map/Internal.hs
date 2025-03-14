{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_HADDOCK not-home #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Word64Map.Internal
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008
--                (c) wren romano 2016
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
-- The contents of this module may change __in any way whatsoever__
-- and __without any warning__ between minor versions of this package.
--
-- Authors importing this module are expected to track development
-- closely.
--
-- = Description
--
-- This defines the data structures and core (hidden) manipulations
-- on representations.
--
-- @since 0.5.9
-----------------------------------------------------------------------------

-- [Note: INLINE bit fiddling]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- It is essential that the bit fiddling functions like mask, zero, branchMask
-- etc are inlined. If they do not, the memory allocation skyrockets. The GHC
-- usually gets it right, but it is disastrous if it does not. Therefore we
-- explicitly mark these functions INLINE.


-- [Note: Local 'go' functions and capturing]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Care must be taken when using 'go' function which captures an argument.
-- Sometimes (for example when the argument is passed to a data constructor,
-- as in insert), GHC heap-allocates more than necessary. Therefore C-- code
-- must be checked for increased allocation when creating and modifying such
-- functions.


-- [Note: Order of constructors]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The order of constructors of Word64Map matters when considering performance.
-- Currently in GHC 7.0, when type has 3 constructors, they are matched from
-- the first to the last -- the best performance is achieved when the
-- constructors are ordered by frequency.
-- On GHC 7.0, reordering constructors from Nil | Tip | Bin to Bin | Tip | Nil
-- improves the benchmark by circa 10%.
--

module GHC.Data.Word64Map.Internal (
    -- * Map type
      Word64Map(..), Key          -- instance Eq,Show

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
    , disjoint

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

    -- ** Compose
    , compose

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
    , mergeWithKey'

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

    -- * Filter
    , filter
    , filterWithKey
    , restrictKeys
    , withoutKeys
    , partition
    , partitionWithKey

    , takeWhileAntitone
    , dropWhileAntitone
    , spanAntitone

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

    -- * Debugging
    , showTree
    , showTreeWith

    -- * Internal types
    , Mask, Prefix, Nat

    -- * Utility
    , natFromInt
    , intFromNat
    , link
    , linkWithMask
    , bin
    , binCheckLeft
    , binCheckRight
    , zero
    , nomatch
    , match
    , mask
    , maskW
    , shorter
    , branchMask
    , highestBitMask

    -- * Used by "Word64Map.Merge.Lazy" and "Word64Map.Merge.Strict"
    , mapWhenMissing
    , mapWhenMatched
    , lmapWhenMissing
    , contramapFirstWhenMatched
    , contramapSecondWhenMatched
    , mapGentlyWhenMissing
    , mapGentlyWhenMatched
    ) where

import GHC.Prelude.Basic hiding
  (lookup, filter, foldr, foldl, foldl', null, map)

import Data.Functor.Identity (Identity (..))
import Data.Semigroup (Semigroup(stimes,(<>)),stimesIdempotentMonoid)
import Data.Functor.Classes

import Control.DeepSeq (NFData(rnf))
import qualified Data.Foldable as Foldable
import Data.Maybe (fromMaybe)

import GHC.Data.Word64Set.Internal (Key)
import qualified GHC.Data.Word64Set.Internal as Word64Set
import GHC.Utils.Containers.Internal.BitUtil
import GHC.Utils.Containers.Internal.StrictPair

import Data.Coerce
import Data.Data (Data(..), Constr, mkConstr, constrIndex, Fixity(Prefix),
                  DataType, mkDataType, gcast1)
import GHC.Exts (build)
import qualified GHC.Exts as GHCExts
import Text.Read
import qualified Control.Category as Category
import Data.Word


-- A "Nat" is a 64 bit machine word (an unsigned Int64)
type Nat = Word64

natFromInt :: Key -> Nat
natFromInt = id
{-# INLINE natFromInt #-}

intFromNat :: Nat -> Key
intFromNat = id
{-# INLINE intFromNat #-}

{--------------------------------------------------------------------
  Types
--------------------------------------------------------------------}


-- | A map of integers to values @a@.

-- See Note: Order of constructors
data Word64Map a = Bin {-# UNPACK #-} !Prefix
                    {-# UNPACK #-} !Mask
                    !(Word64Map a)
                    !(Word64Map a)
-- Fields:
--   prefix: The most significant bits shared by all keys in this Bin.
--   mask: The switching bit to determine if a key should follow the left
--         or right subtree of a 'Bin'.
-- Invariant: Nil is never found as a child of Bin.
-- Invariant: The Mask is a power of 2. It is the largest bit position at which
--            two keys of the map differ.
-- Invariant: Prefix is the common high-order bits that all elements share to
--            the left of the Mask bit.
-- Invariant: In (Bin prefix mask left right), left consists of the elements that
--            don't have the mask bit set; right is all the elements that do.
              | Tip {-# UNPACK #-} !Key a
              | Nil

type Prefix = Word64
type Mask   = Word64


-- Some stuff from "Data.Word64Set.Internal", for 'restrictKeys' and
-- 'withoutKeys' to use.
type Word64SetPrefix = Word64
type Word64SetBitMap = Word64

bitmapOf :: Word64 -> Word64SetBitMap
bitmapOf x = shiftLL 1 (fromIntegral (x .&. Word64Set.suffixBitMask))
{-# INLINE bitmapOf #-}

{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}

-- | \(O(\min(n,W))\). Find the value at a key.
-- Calls 'error' when the element can not be found.
--
-- > fromList [(5,'a'), (3,'b')] ! 1    Error: element not in the map
-- > fromList [(5,'a'), (3,'b')] ! 5 == 'a'

(!) :: Word64Map a -> Key -> a
(!) m k = find k m

-- | \(O(\min(n,W))\). Find the value at a key.
-- Returns 'Nothing' when the element can not be found.
--
-- > fromList [(5,'a'), (3,'b')] !? 1 == Nothing
-- > fromList [(5,'a'), (3,'b')] !? 5 == Just 'a'
--
-- @since 0.5.11

(!?) :: Word64Map a -> Key -> Maybe a
(!?) m k = lookup k m

-- | Same as 'difference'.
(\\) :: Word64Map a -> Word64Map b -> Word64Map a
m1 \\ m2 = difference m1 m2

infixl 9 !?,\\{-This comment teaches CPP correct behaviour -}

{--------------------------------------------------------------------
  Types
--------------------------------------------------------------------}

instance Monoid (Word64Map a) where
    mempty  = empty
    mconcat = unions
    mappend = (<>)

-- | @since 0.5.7
instance Semigroup (Word64Map a) where
    (<>)    = union
    stimes  = stimesIdempotentMonoid

-- | Folds in order of increasing key.
instance Foldable.Foldable Word64Map where
  fold = go
    where go Nil = mempty
          go (Tip _ v) = v
          go (Bin _ m l r)
            | m < 0     = go r `mappend` go l
            | otherwise = go l `mappend` go r
  {-# INLINABLE fold #-}
  foldr = foldr
  {-# INLINE foldr #-}
  foldl = foldl
  {-# INLINE foldl #-}
  foldMap f t = go t
    where go Nil = mempty
          go (Tip _ v) = f v
          go (Bin _ m l r)
            | m < 0     = go r `mappend` go l
            | otherwise = go l `mappend` go r
  {-# INLINE foldMap #-}
  foldl' = foldl'
  {-# INLINE foldl' #-}
  foldr' = foldr'
  {-# INLINE foldr' #-}
  length = size
  {-# INLINE length #-}
  null   = null
  {-# INLINE null #-}
  toList = elems -- NB: Foldable.toList /= Word64Map.toList
  {-# INLINE toList #-}
  elem = go
    where go !_ Nil = False
          go x (Tip _ y) = x == y
          go x (Bin _ _ l r) = go x l || go x r
  {-# INLINABLE elem #-}
  maximum = start
    where start Nil = error "Data.Foldable.maximum (for Data.Word64Map): empty map"
          start (Tip _ y) = y
          start (Bin _ m l r)
            | m < 0     = go (start r) l
            | otherwise = go (start l) r

          go !m Nil = m
          go m (Tip _ y) = max m y
          go m (Bin _ _ l r) = go (go m l) r
  {-# INLINABLE maximum #-}
  minimum = start
    where start Nil = error "Data.Foldable.minimum (for Data.Word64Map): empty map"
          start (Tip _ y) = y
          start (Bin _ m l r)
            | m < 0     = go (start r) l
            | otherwise = go (start l) r

          go !m Nil = m
          go m (Tip _ y) = min m y
          go m (Bin _ _ l r) = go (go m l) r
  {-# INLINABLE minimum #-}
  sum = foldl' (+) 0
  {-# INLINABLE sum #-}
  product = foldl' (*) 1
  {-# INLINABLE product #-}

-- | Traverses in order of increasing key.
instance Traversable Word64Map where
    traverse f = traverseWithKey (\_ -> f)
    {-# INLINE traverse #-}

instance NFData a => NFData (Word64Map a) where
    rnf Nil = ()
    rnf (Tip _ v) = rnf v
    rnf (Bin _ _ l r) = rnf l `seq` rnf r


{--------------------------------------------------------------------
  A Data instance
--------------------------------------------------------------------}

-- This instance preserves data abstraction at the cost of inefficiency.
-- We provide limited reflection services for the sake of data abstraction.

instance Data a => Data (Word64Map a) where
  gfoldl f z im = z fromList `f` (toList im)
  toConstr _     = fromListConstr
  gunfold k z c  = case constrIndex c of
    1 -> k (z fromList)
    _ -> error "gunfold"
  dataTypeOf _   = intMapDataType
  dataCast1 f    = gcast1 f

fromListConstr :: Constr
fromListConstr = mkConstr intMapDataType "fromList" [] Prefix

intMapDataType :: DataType
intMapDataType = mkDataType "Data.Word64Map.Internal.Word64Map" [fromListConstr]


{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}
-- | \(O(1)\). Is the map empty?
--
-- > Data.Word64Map.null (empty)           == True
-- > Data.Word64Map.null (singleton 1 'a') == False

null :: Word64Map a -> Bool
null Nil = True
null _   = False
{-# INLINE null #-}

-- | \(O(n)\). Number of elements in the map.
--
-- > size empty                                   == 0
-- > size (singleton 1 'a')                       == 1
-- > size (fromList([(1,'a'), (2,'c'), (3,'b')])) == 3
size :: Word64Map a -> Int
size = go 0
  where
    go !acc (Bin _ _ l r) = go (go acc l) r
    go acc (Tip _ _) = 1 + acc
    go acc Nil = acc

-- | \(O(\min(n,W))\). Is the key a member of the map?
--
-- > member 5 (fromList [(5,'a'), (3,'b')]) == True
-- > member 1 (fromList [(5,'a'), (3,'b')]) == False

-- See Note: Local 'go' functions and capturing]
member :: Key -> Word64Map a -> Bool
member !k = go
  where
    go (Bin p m l r) | nomatch k p m = False
                     | zero k m  = go l
                     | otherwise = go r
    go (Tip kx _) = k == kx
    go Nil = False

-- | \(O(\min(n,W))\). Is the key not a member of the map?
--
-- > notMember 5 (fromList [(5,'a'), (3,'b')]) == False
-- > notMember 1 (fromList [(5,'a'), (3,'b')]) == True

notMember :: Key -> Word64Map a -> Bool
notMember k m = not $ member k m

-- | \(O(\min(n,W))\). Lookup the value at a key in the map. See also 'Data.Map.lookup'.

-- See Note: Local 'go' functions and capturing
lookup :: Key -> Word64Map a -> Maybe a
lookup !k = go
  where
    go (Bin _p m l r) | zero k m  = go l
                      | otherwise = go r
    go (Tip kx x) | k == kx   = Just x
                  | otherwise = Nothing
    go Nil = Nothing

-- See Note: Local 'go' functions and capturing]
find :: Key -> Word64Map a -> a
find !k = go
  where
    go (Bin _p m l r) | zero k m  = go l
                      | otherwise = go r
    go (Tip kx x) | k == kx   = x
                  | otherwise = not_found
    go Nil = not_found

    not_found = error ("Word64Map.!: key " ++ show k ++ " is not an element of the map")

-- | \(O(\min(n,W))\). The expression @('findWithDefault' def k map)@
-- returns the value at key @k@ or returns @def@ when the key is not an
-- element of the map.
--
-- > findWithDefault 'x' 1 (fromList [(5,'a'), (3,'b')]) == 'x'
-- > findWithDefault 'x' 5 (fromList [(5,'a'), (3,'b')]) == 'a'

-- See Note: Local 'go' functions and capturing]
findWithDefault :: a -> Key -> Word64Map a -> a
findWithDefault def !k = go
  where
    go (Bin p m l r) | nomatch k p m = def
                     | zero k m  = go l
                     | otherwise = go r
    go (Tip kx x) | k == kx   = x
                  | otherwise = def
    go Nil = def

-- | \(O(\min(n,W))\). Find largest key smaller than the given one and return the
-- corresponding (key, value) pair.
--
-- > lookupLT 3 (fromList [(3,'a'), (5,'b')]) == Nothing
-- > lookupLT 4 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')

-- See Note: Local 'go' functions and capturing.
lookupLT :: Key -> Word64Map a -> Maybe (Key, a)
lookupLT !k t = case t of
    Bin _ m l r | m < 0 -> if k >= 0 then go r l else go Nil r
    _ -> go Nil t
  where
    go def (Bin p m l r)
      | nomatch k p m = if k < p then unsafeFindMax def else unsafeFindMax r
      | zero k m  = go def l
      | otherwise = go l r
    go def (Tip ky y)
      | k <= ky   = unsafeFindMax def
      | otherwise = Just (ky, y)
    go def Nil = unsafeFindMax def

-- | \(O(\min(n,W))\). Find smallest key greater than the given one and return the
-- corresponding (key, value) pair.
--
-- > lookupGT 4 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')
-- > lookupGT 5 (fromList [(3,'a'), (5,'b')]) == Nothing

-- See Note: Local 'go' functions and capturing.
lookupGT :: Key -> Word64Map a -> Maybe (Key, a)
lookupGT !k t = case t of
    Bin _ m l r | m < 0 -> if k >= 0 then go Nil l else go l r
    _ -> go Nil t
  where
    go def (Bin p m l r)
      | nomatch k p m = if k < p then unsafeFindMin l else unsafeFindMin def
      | zero k m  = go r l
      | otherwise = go def r
    go def (Tip ky y)
      | k >= ky   = unsafeFindMin def
      | otherwise = Just (ky, y)
    go def Nil = unsafeFindMin def

-- | \(O(\min(n,W))\). Find largest key smaller or equal to the given one and return
-- the corresponding (key, value) pair.
--
-- > lookupLE 2 (fromList [(3,'a'), (5,'b')]) == Nothing
-- > lookupLE 4 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')
-- > lookupLE 5 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')

-- See Note: Local 'go' functions and capturing.
lookupLE :: Key -> Word64Map a -> Maybe (Key, a)
lookupLE !k t = case t of
    Bin _ m l r | m < 0 -> if k >= 0 then go r l else go Nil r
    _ -> go Nil t
  where
    go def (Bin p m l r)
      | nomatch k p m = if k < p then unsafeFindMax def else unsafeFindMax r
      | zero k m  = go def l
      | otherwise = go l r
    go def (Tip ky y)
      | k < ky    = unsafeFindMax def
      | otherwise = Just (ky, y)
    go def Nil = unsafeFindMax def

-- | \(O(\min(n,W))\). Find smallest key greater or equal to the given one and return
-- the corresponding (key, value) pair.
--
-- > lookupGE 3 (fromList [(3,'a'), (5,'b')]) == Just (3, 'a')
-- > lookupGE 4 (fromList [(3,'a'), (5,'b')]) == Just (5, 'b')
-- > lookupGE 6 (fromList [(3,'a'), (5,'b')]) == Nothing

-- See Note: Local 'go' functions and capturing.
lookupGE :: Key -> Word64Map a -> Maybe (Key, a)
lookupGE !k t = case t of
    Bin _ m l r | m < 0 -> if k >= 0 then go Nil l else go l r
    _ -> go Nil t
  where
    go def (Bin p m l r)
      | nomatch k p m = if k < p then unsafeFindMin l else unsafeFindMin def
      | zero k m  = go r l
      | otherwise = go def r
    go def (Tip ky y)
      | k > ky    = unsafeFindMin def
      | otherwise = Just (ky, y)
    go def Nil = unsafeFindMin def


-- Helper function for lookupGE and lookupGT. It assumes that if a Bin node is
-- given, it has m > 0.
unsafeFindMin :: Word64Map a -> Maybe (Key, a)
unsafeFindMin Nil = Nothing
unsafeFindMin (Tip ky y) = Just (ky, y)
unsafeFindMin (Bin _ _ l _) = unsafeFindMin l

-- Helper function for lookupLE and lookupLT. It assumes that if a Bin node is
-- given, it has m > 0.
unsafeFindMax :: Word64Map a -> Maybe (Key, a)
unsafeFindMax Nil = Nothing
unsafeFindMax (Tip ky y) = Just (ky, y)
unsafeFindMax (Bin _ _ _ r) = unsafeFindMax r

{--------------------------------------------------------------------
  Disjoint
--------------------------------------------------------------------}
-- | \(O(n+m)\). Check whether the key sets of two maps are disjoint
-- (i.e. their 'intersection' is empty).
--
-- > disjoint (fromList [(2,'a')]) (fromList [(1,()), (3,())])   == True
-- > disjoint (fromList [(2,'a')]) (fromList [(1,'a'), (2,'b')]) == False
-- > disjoint (fromList [])        (fromList [])                 == True
--
-- > disjoint a b == null (intersection a b)
--
-- @since 0.6.2.1
disjoint :: Word64Map a -> Word64Map b -> Bool
disjoint Nil _ = True
disjoint _ Nil = True
disjoint (Tip kx _) ys = notMember kx ys
disjoint xs (Tip ky _) = notMember ky xs
disjoint t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2 = disjoint1
  | shorter m2 m1 = disjoint2
  | p1 == p2      = disjoint l1 l2 && disjoint r1 r2
  | otherwise     = True
  where
    disjoint1 | nomatch p2 p1 m1 = True
              | zero p2 m1       = disjoint l1 t2
              | otherwise        = disjoint r1 t2
    disjoint2 | nomatch p1 p2 m2 = True
              | zero p1 m2       = disjoint t1 l2
              | otherwise        = disjoint t1 r2

{--------------------------------------------------------------------
  Compose
--------------------------------------------------------------------}
-- | Relate the keys of one map to the values of
-- the other, by using the values of the former as keys for lookups
-- in the latter.
--
-- Complexity: \( O(n * \min(m,W)) \), where \(m\) is the size of the first argument
--
-- > compose (fromList [('a', "A"), ('b', "B")]) (fromList [(1,'a'),(2,'b'),(3,'z')]) = fromList [(1,"A"),(2,"B")]
--
-- @
-- ('compose' bc ab '!?') = (bc '!?') <=< (ab '!?')
-- @
--
-- __Note:__ Prior to v0.6.4, "Data.Word64Map.Strict" exposed a version of
-- 'compose' that forced the values of the output 'Word64Map'. This version does
-- not force these values.
--
-- @since 0.6.3.1
compose :: Word64Map c -> Word64Map Word64 -> Word64Map c
compose bc !ab
  | null bc = empty
  | otherwise = mapMaybe (bc !?) ab

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
-- | \(O(1)\). The empty map.
--
-- > empty      == fromList []
-- > size empty == 0

empty :: Word64Map a
empty
  = Nil
{-# INLINE empty #-}

-- | \(O(1)\). A map of one element.
--
-- > singleton 1 'a'        == fromList [(1, 'a')]
-- > size (singleton 1 'a') == 1

singleton :: Key -> a -> Word64Map a
singleton k x
  = Tip k x
{-# INLINE singleton #-}

{--------------------------------------------------------------------
  Insert
--------------------------------------------------------------------}
-- | \(O(\min(n,W))\). Insert a new key\/value pair in the map.
-- If the key is already present in the map, the associated value is
-- replaced with the supplied value, i.e. 'insert' is equivalent to
-- @'insertWith' 'const'@.
--
-- > insert 5 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'x')]
-- > insert 7 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'a'), (7, 'x')]
-- > insert 5 'x' empty                         == singleton 5 'x'

insert :: Key -> a -> Word64Map a -> Word64Map a
insert !k x t@(Bin p m l r)
  | nomatch k p m = link k (Tip k x) p t
  | zero k m      = Bin p m (insert k x l) r
  | otherwise     = Bin p m l (insert k x r)
insert k x t@(Tip ky _)
  | k==ky         = Tip k x
  | otherwise     = link k (Tip k x) ky t
insert k x Nil = Tip k x

-- right-biased insertion, used by 'union'
-- | \(O(\min(n,W))\). Insert with a combining function.
-- @'insertWith' f key value mp@
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert @f new_value old_value@.
--
-- > insertWith (++) 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "xxxa")]
-- > insertWith (++) 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
-- > insertWith (++) 5 "xxx" empty                         == singleton 5 "xxx"

insertWith :: (a -> a -> a) -> Key -> a -> Word64Map a -> Word64Map a
insertWith f k x t
  = insertWithKey (\_ x' y' -> f x' y') k x t

-- | \(O(\min(n,W))\). Insert with a combining function.
-- @'insertWithKey' f key value mp@
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert @f key new_value old_value@.
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > insertWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:xxx|a")]
-- > insertWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
-- > insertWithKey f 5 "xxx" empty                         == singleton 5 "xxx"

insertWithKey :: (Key -> a -> a -> a) -> Key -> a -> Word64Map a -> Word64Map a
insertWithKey f !k x t@(Bin p m l r)
  | nomatch k p m = link k (Tip k x) p t
  | zero k m      = Bin p m (insertWithKey f k x l) r
  | otherwise     = Bin p m l (insertWithKey f k x r)
insertWithKey f k x t@(Tip ky y)
  | k == ky       = Tip k (f k x y)
  | otherwise     = link k (Tip k x) ky t
insertWithKey _ k x Nil = Tip k x

-- | \(O(\min(n,W))\). The expression (@'insertLookupWithKey' f k x map@)
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

insertLookupWithKey :: (Key -> a -> a -> a) -> Key -> a -> Word64Map a -> (Maybe a, Word64Map a)
insertLookupWithKey f !k x t@(Bin p m l r)
  | nomatch k p m = (Nothing,link k (Tip k x) p t)
  | zero k m      = let (found,l') = insertLookupWithKey f k x l
                    in (found,Bin p m l' r)
  | otherwise     = let (found,r') = insertLookupWithKey f k x r
                    in (found,Bin p m l r')
insertLookupWithKey f k x t@(Tip ky y)
  | k == ky       = (Just y,Tip k (f k x y))
  | otherwise     = (Nothing,link k (Tip k x) ky t)
insertLookupWithKey _ k x Nil = (Nothing,Tip k x)


{--------------------------------------------------------------------
  Deletion
--------------------------------------------------------------------}
-- | \(O(\min(n,W))\). Delete a key and its value from the map. When the key is not
-- a member of the map, the original map is returned.
--
-- > delete 5 (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > delete 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > delete 5 empty                         == empty

delete :: Key -> Word64Map a -> Word64Map a
delete !k t@(Bin p m l r)
  | nomatch k p m = t
  | zero k m      = binCheckLeft p m (delete k l) r
  | otherwise     = binCheckRight p m l (delete k r)
delete k t@(Tip ky _)
  | k == ky       = Nil
  | otherwise     = t
delete _k Nil = Nil

-- | \(O(\min(n,W))\). Adjust a value at a specific key. When the key is not
-- a member of the map, the original map is returned.
--
-- > adjust ("new " ++) 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- > adjust ("new " ++) 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > adjust ("new " ++) 7 empty                         == empty

adjust ::  (a -> a) -> Key -> Word64Map a -> Word64Map a
adjust f k m
  = adjustWithKey (\_ x -> f x) k m

-- | \(O(\min(n,W))\). Adjust a value at a specific key. When the key is not
-- a member of the map, the original map is returned.
--
-- > let f key x = (show key) ++ ":new " ++ x
-- > adjustWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
-- > adjustWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > adjustWithKey f 7 empty                         == empty

adjustWithKey ::  (Key -> a -> a) -> Key -> Word64Map a -> Word64Map a
adjustWithKey f !k (Bin p m l r)
  | zero k m      = Bin p m (adjustWithKey f k l) r
  | otherwise     = Bin p m l (adjustWithKey f k r)
adjustWithKey f k t@(Tip ky y)
  | k == ky       = Tip ky (f k y)
  | otherwise     = t
adjustWithKey _ _ Nil = Nil


-- | \(O(\min(n,W))\). The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > update f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- > update f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > update f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

update ::  (a -> Maybe a) -> Key -> Word64Map a -> Word64Map a
update f
  = updateWithKey (\_ x -> f x)

-- | \(O(\min(n,W))\). The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f k x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
--
-- > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- > updateWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
-- > updateWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > updateWithKey f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

updateWithKey ::  (Key -> a -> Maybe a) -> Key -> Word64Map a -> Word64Map a
updateWithKey f !k (Bin p m l r)
  | zero k m      = binCheckLeft p m (updateWithKey f k l) r
  | otherwise     = binCheckRight p m l (updateWithKey f k r)
updateWithKey f k t@(Tip ky y)
  | k == ky       = case (f k y) of
                      Just y' -> Tip ky y'
                      Nothing -> Nil
  | otherwise     = t
updateWithKey _ _ Nil = Nil

-- | \(O(\min(n,W))\). Lookup and update.
-- The function returns original value, if it is updated.
-- This is different behavior than 'Data.Map.updateLookupWithKey'.
-- Returns the original key value if the map entry is deleted.
--
-- > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- > updateLookupWithKey f 5 (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "5:new a")])
-- > updateLookupWithKey f 7 (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a")])
-- > updateLookupWithKey f 3 (fromList [(5,"a"), (3,"b")]) == (Just "b", singleton 5 "a")

updateLookupWithKey ::  (Key -> a -> Maybe a) -> Key -> Word64Map a -> (Maybe a,Word64Map a)
updateLookupWithKey f !k (Bin p m l r)
  | zero k m      = let !(found,l') = updateLookupWithKey f k l
                    in (found,binCheckLeft p m l' r)
  | otherwise     = let !(found,r') = updateLookupWithKey f k r
                    in (found,binCheckRight p m l r')
updateLookupWithKey f k t@(Tip ky y)
  | k==ky         = case (f k y) of
                      Just y' -> (Just y,Tip ky y')
                      Nothing -> (Just y,Nil)
  | otherwise     = (Nothing,t)
updateLookupWithKey _ _ Nil = (Nothing,Nil)


-- | \(O(\min(n,W))\). The expression (@'alter' f k map@) alters the value @x@ at @k@, or absence thereof.
-- 'alter' can be used to insert, delete, or update a value in an 'Word64Map'.
-- In short : @'lookup' k ('alter' f k m) = f ('lookup' k m)@.
alter :: (Maybe a -> Maybe a) -> Key -> Word64Map a -> Word64Map a
alter f !k t@(Bin p m l r)
  | nomatch k p m = case f Nothing of
                      Nothing -> t
                      Just x -> link k (Tip k x) p t
  | zero k m      = binCheckLeft p m (alter f k l) r
  | otherwise     = binCheckRight p m l (alter f k r)
alter f k t@(Tip ky y)
  | k==ky         = case f (Just y) of
                      Just x -> Tip ky x
                      Nothing -> Nil
  | otherwise     = case f Nothing of
                      Just x -> link k (Tip k x) ky t
                      Nothing -> Tip ky y
alter f k Nil     = case f Nothing of
                      Just x -> Tip k x
                      Nothing -> Nil

-- | \(O(\min(n,W))\). The expression (@'alterF' f k map@) alters the value @x@ at
-- @k@, or absence thereof.  'alterF' can be used to inspect, insert, delete,
-- or update a value in an 'Word64Map'.  In short : @'lookup' k <$> 'alterF' f k m = f
-- ('lookup' k m)@.
--
-- Example:
--
-- @
-- interactiveAlter :: Int -> Word64Map String -> IO (Word64Map String)
-- interactiveAlter k m = alterF f k m where
--   f Nothing = do
--      putStrLn $ show k ++
--          " was not found in the map. Would you like to add it?"
--      getUserResponse1 :: IO (Maybe String)
--   f (Just old) = do
--      putStrLn $ "The key is currently bound to " ++ show old ++
--          ". Would you like to change or delete it?"
--      getUserResponse2 :: IO (Maybe String)
-- @
--
-- 'alterF' is the most general operation for working with an individual
-- key that may or may not be in a given map.
--
-- Note: 'alterF' is a flipped version of the @at@ combinator from
-- @Control.Lens.At@.
--
-- @since 0.5.8

alterF :: Functor f
       => (Maybe a -> f (Maybe a)) -> Key -> Word64Map a -> f (Word64Map a)
-- This implementation was stolen from 'Control.Lens.At'.
alterF f k m = (<$> f mv) $ \fres ->
  case fres of
    Nothing -> maybe m (const (delete k m)) mv
    Just v' -> insert k v' m
  where mv = lookup k m

{--------------------------------------------------------------------
  Union
--------------------------------------------------------------------}
-- | The union of a list of maps.
--
-- > unions [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
-- >     == fromList [(3, "b"), (5, "a"), (7, "C")]
-- > unions [(fromList [(5, "A3"), (3, "B3")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "a"), (3, "b")])]
-- >     == fromList [(3, "B3"), (5, "A3"), (7, "C")]

unions :: Foldable f => f (Word64Map a) -> Word64Map a
unions xs
  = Foldable.foldl' union empty xs

-- | The union of a list of maps, with a combining operation.
--
-- > unionsWith (++) [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
-- >     == fromList [(3, "bB3"), (5, "aAA3"), (7, "C")]

unionsWith :: Foldable f => (a->a->a) -> f (Word64Map a) -> Word64Map a
unionsWith f ts
  = Foldable.foldl' (unionWith f) empty ts

-- | \(O(n+m)\). The (left-biased) union of two maps.
-- It prefers the first map when duplicate keys are encountered,
-- i.e. (@'union' == 'unionWith' 'const'@).
--
-- > union (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "a"), (7, "C")]

union :: Word64Map a -> Word64Map a -> Word64Map a
union m1 m2
  = mergeWithKey' Bin const id id m1 m2

-- | \(O(n+m)\). The union with a combining function.
--
-- > unionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "aA"), (7, "C")]

unionWith :: (a -> a -> a) -> Word64Map a -> Word64Map a -> Word64Map a
unionWith f m1 m2
  = unionWithKey (\_ x y -> f x y) m1 m2

-- | \(O(n+m)\). The union with a combining function.
--
-- > let f key left_value right_value = (show key) ++ ":" ++ left_value ++ "|" ++ right_value
-- > unionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "5:a|A"), (7, "C")]

unionWithKey :: (Key -> a -> a -> a) -> Word64Map a -> Word64Map a -> Word64Map a
unionWithKey f m1 m2
  = mergeWithKey' Bin (\(Tip k1 x1) (Tip _k2 x2) -> Tip k1 (f k1 x1 x2)) id id m1 m2

{--------------------------------------------------------------------
  Difference
--------------------------------------------------------------------}
-- | \(O(n+m)\). Difference between two maps (based on keys).
--
-- > difference (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 3 "b"

difference :: Word64Map a -> Word64Map b -> Word64Map a
difference m1 m2
  = mergeWithKey (\_ _ _ -> Nothing) id (const Nil) m1 m2

-- | \(O(n+m)\). Difference with a combining function.
--
-- > let f al ar = if al == "b" then Just (al ++ ":" ++ ar) else Nothing
-- > differenceWith f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (7, "C")])
-- >     == singleton 3 "b:B"

differenceWith :: (a -> b -> Maybe a) -> Word64Map a -> Word64Map b -> Word64Map a
differenceWith f m1 m2
  = differenceWithKey (\_ x y -> f x y) m1 m2

-- | \(O(n+m)\). Difference with a combining function. When two equal keys are
-- encountered, the combining function is applied to the key and both values.
-- If it returns 'Nothing', the element is discarded (proper set difference).
-- If it returns (@'Just' y@), the element is updated with a new value @y@.
--
-- > let f k al ar = if al == "b" then Just ((show k) ++ ":" ++ al ++ "|" ++ ar) else Nothing
-- > differenceWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (10, "C")])
-- >     == singleton 3 "3:b|B"

differenceWithKey :: (Key -> a -> b -> Maybe a) -> Word64Map a -> Word64Map b -> Word64Map a
differenceWithKey f m1 m2
  = mergeWithKey f id (const Nil) m1 m2


-- TODO(wrengr): re-verify that asymptotic bound
-- | \(O(n+m)\). Remove all the keys in a given set from a map.
--
-- @
-- m \`withoutKeys\` s = 'filterWithKey' (\\k _ -> k ``Word64Set.notMember`` s) m
-- @
--
-- @since 0.5.8
withoutKeys :: Word64Map a -> Word64Set.Word64Set -> Word64Map a
withoutKeys t1@(Bin p1 m1 l1 r1) t2@(Word64Set.Bin p2 m2 l2 r2)
    | shorter m1 m2  = difference1
    | shorter m2 m1  = difference2
    | p1 == p2       = bin p1 m1 (withoutKeys l1 l2) (withoutKeys r1 r2)
    | otherwise      = t1
    where
    difference1
        | nomatch p2 p1 m1  = t1
        | zero p2 m1        = binCheckLeft p1 m1 (withoutKeys l1 t2) r1
        | otherwise         = binCheckRight p1 m1 l1 (withoutKeys r1 t2)
    difference2
        | nomatch p1 p2 m2  = t1
        | zero p1 m2        = withoutKeys t1 l2
        | otherwise         = withoutKeys t1 r2
withoutKeys t1@(Bin p1 m1 _ _) (Word64Set.Tip p2 bm2) =
    let minbit = bitmapOf p1
        lt_minbit = minbit - 1
        maxbit = bitmapOf (p1 .|. (m1 .|. (m1 - 1)))
        gt_maxbit = (-maxbit) `xor` maxbit
    -- TODO(wrengr): should we manually inline/unroll 'updatePrefix'
    -- and 'withoutBM' here, in order to avoid redundant case analyses?
    in updatePrefix p2 t1 $ withoutBM (bm2 .|. lt_minbit .|. gt_maxbit)
withoutKeys t1@(Bin _ _ _ _) Word64Set.Nil = t1
withoutKeys t1@(Tip k1 _) t2
    | k1 `Word64Set.member` t2 = Nil
    | otherwise = t1
withoutKeys Nil _ = Nil


updatePrefix
    :: Word64SetPrefix -> Word64Map a -> (Word64Map a -> Word64Map a) -> Word64Map a
updatePrefix !kp t@(Bin p m l r) f
    | m .&. Word64Set.suffixBitMask /= 0 =
        if p .&. Word64Set.prefixBitMask == kp then f t else t
    | nomatch kp p m = t
    | zero kp m      = binCheckLeft p m (updatePrefix kp l f) r
    | otherwise      = binCheckRight p m l (updatePrefix kp r f)
updatePrefix kp t@(Tip kx _) f
    | kx .&. Word64Set.prefixBitMask == kp = f t
    | otherwise = t
updatePrefix _ Nil _ = Nil


withoutBM :: Word64SetBitMap -> Word64Map a -> Word64Map a
withoutBM 0 t = t
withoutBM bm (Bin p m l r) =
    let leftBits = bitmapOf (p .|. m) - 1
        bmL = bm .&. leftBits
        bmR = bm `xor` bmL -- = (bm .&. complement leftBits)
    in  bin p m (withoutBM bmL l) (withoutBM bmR r)
withoutBM bm t@(Tip k _)
    -- TODO(wrengr): need we manually inline 'Word64Set.Member' here?
    | k `Word64Set.member` Word64Set.Tip (k .&. Word64Set.prefixBitMask) bm = Nil
    | otherwise = t
withoutBM _ Nil = Nil


{--------------------------------------------------------------------
  Intersection
--------------------------------------------------------------------}
-- | \(O(n+m)\). The (left-biased) intersection of two maps (based on keys).
--
-- > intersection (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "a"

intersection :: Word64Map a -> Word64Map b -> Word64Map a
intersection m1 m2
  = mergeWithKey' bin const (const Nil) (const Nil) m1 m2


-- TODO(wrengr): re-verify that asymptotic bound
-- | \(O(n+m)\). The restriction of a map to the keys in a set.
--
-- @
-- m \`restrictKeys\` s = 'filterWithKey' (\\k _ -> k ``Word64Set.member`` s) m
-- @
--
-- @since 0.5.8
restrictKeys :: Word64Map a -> Word64Set.Word64Set -> Word64Map a
restrictKeys t1@(Bin p1 m1 l1 r1) t2@(Word64Set.Bin p2 m2 l2 r2)
    | shorter m1 m2  = intersection1
    | shorter m2 m1  = intersection2
    | p1 == p2       = bin p1 m1 (restrictKeys l1 l2) (restrictKeys r1 r2)
    | otherwise      = Nil
    where
    intersection1
        | nomatch p2 p1 m1  = Nil
        | zero p2 m1        = restrictKeys l1 t2
        | otherwise         = restrictKeys r1 t2
    intersection2
        | nomatch p1 p2 m2  = Nil
        | zero p1 m2        = restrictKeys t1 l2
        | otherwise         = restrictKeys t1 r2
restrictKeys t1@(Bin p1 m1 _ _) (Word64Set.Tip p2 bm2) =
    let minbit = bitmapOf p1
        ge_minbit = complement (minbit - 1)
        maxbit = bitmapOf (p1 .|. (m1 .|. (m1 - 1)))
        le_maxbit = maxbit .|. (maxbit - 1)
    -- TODO(wrengr): should we manually inline/unroll 'lookupPrefix'
    -- and 'restrictBM' here, in order to avoid redundant case analyses?
    in restrictBM (bm2 .&. ge_minbit .&. le_maxbit) (lookupPrefix p2 t1)
restrictKeys (Bin _ _ _ _) Word64Set.Nil = Nil
restrictKeys t1@(Tip k1 _) t2
    | k1 `Word64Set.member` t2 = t1
    | otherwise = Nil
restrictKeys Nil _ = Nil


-- | \(O(\min(n,W))\). Restrict to the sub-map with all keys matching
-- a key prefix.
lookupPrefix :: Word64SetPrefix -> Word64Map a -> Word64Map a
lookupPrefix !kp t@(Bin p m l r)
    | m .&. Word64Set.suffixBitMask /= 0 =
        if p .&. Word64Set.prefixBitMask == kp then t else Nil
    | nomatch kp p m = Nil
    | zero kp m      = lookupPrefix kp l
    | otherwise      = lookupPrefix kp r
lookupPrefix kp t@(Tip kx _)
    | (kx .&. Word64Set.prefixBitMask) == kp = t
    | otherwise = Nil
lookupPrefix _ Nil = Nil


restrictBM :: Word64SetBitMap -> Word64Map a -> Word64Map a
restrictBM 0 _ = Nil
restrictBM bm (Bin p m l r) =
    let leftBits = bitmapOf (p .|. m) - 1
        bmL = bm .&. leftBits
        bmR = bm `xor` bmL -- = (bm .&. complement leftBits)
    in  bin p m (restrictBM bmL l) (restrictBM bmR r)
restrictBM bm t@(Tip k _)
    -- TODO(wrengr): need we manually inline 'Word64Set.Member' here?
    | k `Word64Set.member` Word64Set.Tip (k .&. Word64Set.prefixBitMask) bm = t
    | otherwise = Nil
restrictBM _ Nil = Nil


-- | \(O(n+m)\). The intersection with a combining function.
--
-- > intersectionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "aA"

intersectionWith :: (a -> b -> c) -> Word64Map a -> Word64Map b -> Word64Map c
intersectionWith f m1 m2
  = intersectionWithKey (\_ x y -> f x y) m1 m2

-- | \(O(n+m)\). The intersection with a combining function.
--
-- > let f k al ar = (show k) ++ ":" ++ al ++ "|" ++ ar
-- > intersectionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "5:a|A"

intersectionWithKey :: (Key -> a -> b -> c) -> Word64Map a -> Word64Map b -> Word64Map c
intersectionWithKey f m1 m2
  = mergeWithKey' bin (\(Tip k1 x1) (Tip _k2 x2) -> Tip k1 (f k1 x1 x2)) (const Nil) (const Nil) m1 m2

{--------------------------------------------------------------------
  MergeWithKey
--------------------------------------------------------------------}

-- | \(O(n+m)\). A high-performance universal combining function. Using
-- 'mergeWithKey', all combining functions can be defined without any loss of
-- efficiency (with exception of 'union', 'difference' and 'intersection',
-- where sharing of some nodes is lost with 'mergeWithKey').
--
-- Please make sure you know what is going on when using 'mergeWithKey',
-- otherwise you can be surprised by unexpected code growth or even
-- corruption of the data structure.
--
-- When 'mergeWithKey' is given three arguments, it is inlined to the call
-- site. You should therefore use 'mergeWithKey' only to define your custom
-- combining functions. For example, you could define 'unionWithKey',
-- 'differenceWithKey' and 'intersectionWithKey' as
--
-- > myUnionWithKey f m1 m2 = mergeWithKey (\k x1 x2 -> Just (f k x1 x2)) id id m1 m2
-- > myDifferenceWithKey f m1 m2 = mergeWithKey f id (const empty) m1 m2
-- > myIntersectionWithKey f m1 m2 = mergeWithKey (\k x1 x2 -> Just (f k x1 x2)) (const empty) (const empty) m1 m2
--
-- When calling @'mergeWithKey' combine only1 only2@, a function combining two
-- 'Word64Map's is created, such that
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
-- @only2@ are 'id' and @'const' 'empty'@, but for example @'map' f@ or
-- @'filterWithKey' f@ could be used for any @f@.

mergeWithKey :: (Key -> a -> b -> Maybe c) -> (Word64Map a -> Word64Map c) -> (Word64Map b -> Word64Map c)
             -> Word64Map a -> Word64Map b -> Word64Map c
mergeWithKey f g1 g2 = mergeWithKey' bin combine g1 g2
  where -- We use the lambda form to avoid non-exhaustive pattern matches warning.
        combine = \(Tip k1 x1) (Tip _k2 x2) ->
          case f k1 x1 x2 of
            Nothing -> Nil
            Just x -> Tip k1 x
        {-# INLINE combine #-}
{-# INLINE mergeWithKey #-}

-- Slightly more general version of mergeWithKey. It differs in the following:
--
-- * the combining function operates on maps instead of keys and values. The
--   reason is to enable sharing in union, difference and intersection.
--
-- * mergeWithKey' is given an equivalent of bin. The reason is that in union*,
--   Bin constructor can be used, because we know both subtrees are nonempty.

mergeWithKey' :: (Prefix -> Mask -> Word64Map c -> Word64Map c -> Word64Map c)
              -> (Word64Map a -> Word64Map b -> Word64Map c) -> (Word64Map a -> Word64Map c) -> (Word64Map b -> Word64Map c)
              -> Word64Map a -> Word64Map b -> Word64Map c
mergeWithKey' bin' f g1 g2 = go
  where
    go t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
      | shorter m1 m2  = merge1
      | shorter m2 m1  = merge2
      | p1 == p2       = bin' p1 m1 (go l1 l2) (go r1 r2)
      | otherwise      = maybe_link p1 (g1 t1) p2 (g2 t2)
      where
        merge1 | nomatch p2 p1 m1  = maybe_link p1 (g1 t1) p2 (g2 t2)
               | zero p2 m1        = bin' p1 m1 (go l1 t2) (g1 r1)
               | otherwise         = bin' p1 m1 (g1 l1) (go r1 t2)
        merge2 | nomatch p1 p2 m2  = maybe_link p1 (g1 t1) p2 (g2 t2)
               | zero p1 m2        = bin' p2 m2 (go t1 l2) (g2 r2)
               | otherwise         = bin' p2 m2 (g2 l2) (go t1 r2)

    go t1'@(Bin _ _ _ _) t2'@(Tip k2' _) = merge0 t2' k2' t1'
      where
        merge0 t2 k2 t1@(Bin p1 m1 l1 r1)
          | nomatch k2 p1 m1 = maybe_link p1 (g1 t1) k2 (g2 t2)
          | zero k2 m1 = bin' p1 m1 (merge0 t2 k2 l1) (g1 r1)
          | otherwise  = bin' p1 m1 (g1 l1) (merge0 t2 k2 r1)
        merge0 t2 k2 t1@(Tip k1 _)
          | k1 == k2 = f t1 t2
          | otherwise = maybe_link k1 (g1 t1) k2 (g2 t2)
        merge0 t2 _  Nil = g2 t2

    go t1@(Bin _ _ _ _) Nil = g1 t1

    go t1'@(Tip k1' _) t2' = merge0 t1' k1' t2'
      where
        merge0 t1 k1 t2@(Bin p2 m2 l2 r2)
          | nomatch k1 p2 m2 = maybe_link k1 (g1 t1) p2 (g2 t2)
          | zero k1 m2 = bin' p2 m2 (merge0 t1 k1 l2) (g2 r2)
          | otherwise  = bin' p2 m2 (g2 l2) (merge0 t1 k1 r2)
        merge0 t1 k1 t2@(Tip k2 _)
          | k1 == k2 = f t1 t2
          | otherwise = maybe_link k1 (g1 t1) k2 (g2 t2)
        merge0 t1 _  Nil = g1 t1

    go Nil t2 = g2 t2

    maybe_link _ Nil _ t2 = t2
    maybe_link _ t1 _ Nil = t1
    maybe_link p1 t1 p2 t2 = link p1 t1 p2 t2
    {-# INLINE maybe_link #-}
{-# INLINE mergeWithKey' #-}


{--------------------------------------------------------------------
  mergeA
--------------------------------------------------------------------}

-- | A tactic for dealing with keys present in one map but not the
-- other in 'merge' or 'mergeA'.
--
-- A tactic of type @WhenMissing f k x z@ is an abstract representation
-- of a function of type @Key -> x -> f (Maybe z)@.
--
-- @since 0.5.9

data WhenMissing f x y = WhenMissing
  { missingSubtree :: Word64Map x -> f (Word64Map y)
  , missingKey :: Key -> x -> f (Maybe y)}

-- | @since 0.5.9
instance (Applicative f, Monad f) => Functor (WhenMissing f x) where
  fmap = mapWhenMissing
  {-# INLINE fmap #-}


-- | @since 0.5.9
instance (Applicative f, Monad f) => Category.Category (WhenMissing f)
  where
    id = preserveMissing
    f . g =
      traverseMaybeMissing $ \ k x -> do
        y <- missingKey g k x
        case y of
          Nothing -> pure Nothing
          Just q  -> missingKey f k q
    {-# INLINE id #-}
    {-# INLINE (.) #-}


-- | Equivalent to @ReaderT k (ReaderT x (MaybeT f))@.
--
-- @since 0.5.9
instance (Applicative f, Monad f) => Applicative (WhenMissing f x) where
  pure x = mapMissing (\ _ _ -> x)
  f <*> g =
    traverseMaybeMissing $ \k x -> do
      res1 <- missingKey f k x
      case res1 of
        Nothing -> pure Nothing
        Just r  -> (pure $!) . fmap r =<< missingKey g k x
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}


-- | Equivalent to @ReaderT k (ReaderT x (MaybeT f))@.
--
-- @since 0.5.9
instance (Applicative f, Monad f) => Monad (WhenMissing f x) where
  m >>= f =
    traverseMaybeMissing $ \k x -> do
      res1 <- missingKey m k x
      case res1 of
        Nothing -> pure Nothing
        Just r  -> missingKey (f r) k x
  {-# INLINE (>>=) #-}


-- | Map covariantly over a @'WhenMissing' f x@.
--
-- @since 0.5.9
mapWhenMissing
  :: (Applicative f, Monad f)
  => (a -> b)
  -> WhenMissing f x a
  -> WhenMissing f x b
mapWhenMissing f t = WhenMissing
  { missingSubtree = \m -> missingSubtree t m >>= \m' -> pure $! fmap f m'
  , missingKey     = \k x -> missingKey t k x >>= \q -> (pure $! fmap f q) }
{-# INLINE mapWhenMissing #-}


-- | Map covariantly over a @'WhenMissing' f x@, using only a
-- 'Functor f' constraint.
mapGentlyWhenMissing
  :: Functor f
  => (a -> b)
  -> WhenMissing f x a
  -> WhenMissing f x b
mapGentlyWhenMissing f t = WhenMissing
  { missingSubtree = \m -> fmap f <$> missingSubtree t m
  , missingKey     = \k x -> fmap f <$> missingKey t k x }
{-# INLINE mapGentlyWhenMissing #-}


-- | Map covariantly over a @'WhenMatched' f k x@, using only a
-- 'Functor f' constraint.
mapGentlyWhenMatched
  :: Functor f
  => (a -> b)
  -> WhenMatched f x y a
  -> WhenMatched f x y b
mapGentlyWhenMatched f t =
  zipWithMaybeAMatched $ \k x y -> fmap f <$> runWhenMatched t k x y
{-# INLINE mapGentlyWhenMatched #-}


-- | Map contravariantly over a @'WhenMissing' f _ x@.
--
-- @since 0.5.9
lmapWhenMissing :: (b -> a) -> WhenMissing f a x -> WhenMissing f b x
lmapWhenMissing f t = WhenMissing
  { missingSubtree = \m -> missingSubtree t (fmap f m)
  , missingKey     = \k x -> missingKey t k (f x) }
{-# INLINE lmapWhenMissing #-}


-- | Map contravariantly over a @'WhenMatched' f _ y z@.
--
-- @since 0.5.9
contramapFirstWhenMatched
  :: (b -> a)
  -> WhenMatched f a y z
  -> WhenMatched f b y z
contramapFirstWhenMatched f t =
  WhenMatched $ \k x y -> runWhenMatched t k (f x) y
{-# INLINE contramapFirstWhenMatched #-}


-- | Map contravariantly over a @'WhenMatched' f x _ z@.
--
-- @since 0.5.9
contramapSecondWhenMatched
  :: (b -> a)
  -> WhenMatched f x a z
  -> WhenMatched f x b z
contramapSecondWhenMatched f t =
  WhenMatched $ \k x y -> runWhenMatched t k x (f y)
{-# INLINE contramapSecondWhenMatched #-}


-- | A tactic for dealing with keys present in one map but not the
-- other in 'merge'.
--
-- A tactic of type @SimpleWhenMissing x z@ is an abstract
-- representation of a function of type @Key -> x -> Maybe z@.
--
-- @since 0.5.9
type SimpleWhenMissing = WhenMissing Identity


-- | A tactic for dealing with keys present in both maps in 'merge'
-- or 'mergeA'.
--
-- A tactic of type @WhenMatched f x y z@ is an abstract representation
-- of a function of type @Key -> x -> y -> f (Maybe z)@.
--
-- @since 0.5.9
newtype WhenMatched f x y z = WhenMatched
  { matchedKey :: Key -> x -> y -> f (Maybe z) }


-- | Along with zipWithMaybeAMatched, witnesses the isomorphism
-- between @WhenMatched f x y z@ and @Key -> x -> y -> f (Maybe z)@.
--
-- @since 0.5.9
runWhenMatched :: WhenMatched f x y z -> Key -> x -> y -> f (Maybe z)
runWhenMatched = matchedKey
{-# INLINE runWhenMatched #-}


-- | Along with traverseMaybeMissing, witnesses the isomorphism
-- between @WhenMissing f x y@ and @Key -> x -> f (Maybe y)@.
--
-- @since 0.5.9
runWhenMissing :: WhenMissing f x y -> Key-> x -> f (Maybe y)
runWhenMissing = missingKey
{-# INLINE runWhenMissing #-}


-- | @since 0.5.9
instance Functor f => Functor (WhenMatched f x y) where
  fmap = mapWhenMatched
  {-# INLINE fmap #-}


-- | @since 0.5.9
instance (Monad f, Applicative f) => Category.Category (WhenMatched f x)
  where
    id = zipWithMatched (\_ _ y -> y)
    f . g =
      zipWithMaybeAMatched $ \k x y -> do
        res <- runWhenMatched g k x y
        case res of
          Nothing -> pure Nothing
          Just r  -> runWhenMatched f k x r
    {-# INLINE id #-}
    {-# INLINE (.) #-}


-- | Equivalent to @ReaderT Key (ReaderT x (ReaderT y (MaybeT f)))@
--
-- @since 0.5.9
instance (Monad f, Applicative f) => Applicative (WhenMatched f x y) where
  pure x = zipWithMatched (\_ _ _ -> x)
  fs <*> xs =
    zipWithMaybeAMatched $ \k x y -> do
      res <- runWhenMatched fs k x y
      case res of
        Nothing -> pure Nothing
        Just r  -> (pure $!) . fmap r =<< runWhenMatched xs k x y
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}


-- | Equivalent to @ReaderT Key (ReaderT x (ReaderT y (MaybeT f)))@
--
-- @since 0.5.9
instance (Monad f, Applicative f) => Monad (WhenMatched f x y) where
  m >>= f =
    zipWithMaybeAMatched $ \k x y -> do
      res <- runWhenMatched m k x y
      case res of
        Nothing -> pure Nothing
        Just r  -> runWhenMatched (f r) k x y
  {-# INLINE (>>=) #-}


-- | Map covariantly over a @'WhenMatched' f x y@.
--
-- @since 0.5.9
mapWhenMatched
  :: Functor f
  => (a -> b)
  -> WhenMatched f x y a
  -> WhenMatched f x y b
mapWhenMatched f (WhenMatched g) =
  WhenMatched $ \k x y -> fmap (fmap f) (g k x y)
{-# INLINE mapWhenMatched #-}


-- | A tactic for dealing with keys present in both maps in 'merge'.
--
-- A tactic of type @SimpleWhenMatched x y z@ is an abstract
-- representation of a function of type @Key -> x -> y -> Maybe z@.
--
-- @since 0.5.9
type SimpleWhenMatched = WhenMatched Identity


-- | When a key is found in both maps, apply a function to the key
-- and values and use the result in the merged map.
--
-- > zipWithMatched
-- >   :: (Key -> x -> y -> z)
-- >   -> SimpleWhenMatched x y z
--
-- @since 0.5.9
zipWithMatched
  :: Applicative f
  => (Key -> x -> y -> z)
  -> WhenMatched f x y z
zipWithMatched f = WhenMatched $ \ k x y -> pure . Just $ f k x y
{-# INLINE zipWithMatched #-}


-- | When a key is found in both maps, apply a function to the key
-- and values to produce an action and use its result in the merged
-- map.
--
-- @since 0.5.9
zipWithAMatched
  :: Applicative f
  => (Key -> x -> y -> f z)
  -> WhenMatched f x y z
zipWithAMatched f = WhenMatched $ \ k x y -> Just <$> f k x y
{-# INLINE zipWithAMatched #-}


-- | When a key is found in both maps, apply a function to the key
-- and values and maybe use the result in the merged map.
--
-- > zipWithMaybeMatched
-- >   :: (Key -> x -> y -> Maybe z)
-- >   -> SimpleWhenMatched x y z
--
-- @since 0.5.9
zipWithMaybeMatched
  :: Applicative f
  => (Key -> x -> y -> Maybe z)
  -> WhenMatched f x y z
zipWithMaybeMatched f = WhenMatched $ \ k x y -> pure $ f k x y
{-# INLINE zipWithMaybeMatched #-}


-- | When a key is found in both maps, apply a function to the key
-- and values, perform the resulting action, and maybe use the
-- result in the merged map.
--
-- This is the fundamental 'WhenMatched' tactic.
--
-- @since 0.5.9
zipWithMaybeAMatched
  :: (Key -> x -> y -> f (Maybe z))
  -> WhenMatched f x y z
zipWithMaybeAMatched f = WhenMatched $ \ k x y -> f k x y
{-# INLINE zipWithMaybeAMatched #-}


-- | Drop all the entries whose keys are missing from the other
-- map.
--
-- > dropMissing :: SimpleWhenMissing x y
--
-- prop> dropMissing = mapMaybeMissing (\_ _ -> Nothing)
--
-- but @dropMissing@ is much faster.
--
-- @since 0.5.9
dropMissing :: Applicative f => WhenMissing f x y
dropMissing = WhenMissing
  { missingSubtree = const (pure Nil)
  , missingKey     = \_ _ -> pure Nothing }
{-# INLINE dropMissing #-}


-- | Preserve, unchanged, the entries whose keys are missing from
-- the other map.
--
-- > preserveMissing :: SimpleWhenMissing x x
--
-- prop> preserveMissing = Merge.Lazy.mapMaybeMissing (\_ x -> Just x)
--
-- but @preserveMissing@ is much faster.
--
-- @since 0.5.9
preserveMissing :: Applicative f => WhenMissing f x x
preserveMissing = WhenMissing
  { missingSubtree = pure
  , missingKey     = \_ v -> pure (Just v) }
{-# INLINE preserveMissing #-}


-- | Map over the entries whose keys are missing from the other map.
--
-- > mapMissing :: (k -> x -> y) -> SimpleWhenMissing x y
--
-- prop> mapMissing f = mapMaybeMissing (\k x -> Just $ f k x)
--
-- but @mapMissing@ is somewhat faster.
--
-- @since 0.5.9
mapMissing :: Applicative f => (Key -> x -> y) -> WhenMissing f x y
mapMissing f = WhenMissing
  { missingSubtree = \m -> pure $! mapWithKey f m
  , missingKey     = \k x -> pure $ Just (f k x) }
{-# INLINE mapMissing #-}


-- | Map over the entries whose keys are missing from the other
-- map, optionally removing some. This is the most powerful
-- 'SimpleWhenMissing' tactic, but others are usually more efficient.
--
-- > mapMaybeMissing :: (Key -> x -> Maybe y) -> SimpleWhenMissing x y
--
-- prop> mapMaybeMissing f = traverseMaybeMissing (\k x -> pure (f k x))
--
-- but @mapMaybeMissing@ uses fewer unnecessary 'Applicative'
-- operations.
--
-- @since 0.5.9
mapMaybeMissing
  :: Applicative f => (Key -> x -> Maybe y) -> WhenMissing f x y
mapMaybeMissing f = WhenMissing
  { missingSubtree = \m -> pure $! mapMaybeWithKey f m
  , missingKey     = \k x -> pure $! f k x }
{-# INLINE mapMaybeMissing #-}


-- | Filter the entries whose keys are missing from the other map.
--
-- > filterMissing :: (k -> x -> Bool) -> SimpleWhenMissing x x
--
-- prop> filterMissing f = Merge.Lazy.mapMaybeMissing $ \k x -> guard (f k x) *> Just x
--
-- but this should be a little faster.
--
-- @since 0.5.9
filterMissing
  :: Applicative f => (Key -> x -> Bool) -> WhenMissing f x x
filterMissing f = WhenMissing
  { missingSubtree = \m -> pure $! filterWithKey f m
  , missingKey     = \k x -> pure $! if f k x then Just x else Nothing }
{-# INLINE filterMissing #-}


-- | Filter the entries whose keys are missing from the other map
-- using some 'Applicative' action.
--
-- > filterAMissing f = Merge.Lazy.traverseMaybeMissing $
-- >   \k x -> (\b -> guard b *> Just x) <$> f k x
--
-- but this should be a little faster.
--
-- @since 0.5.9
filterAMissing
  :: Applicative f => (Key -> x -> f Bool) -> WhenMissing f x x
filterAMissing f = WhenMissing
  { missingSubtree = \m -> filterWithKeyA f m
  , missingKey     = \k x -> bool Nothing (Just x) <$> f k x }
{-# INLINE filterAMissing #-}


-- | \(O(n)\). Filter keys and values using an 'Applicative' predicate.
filterWithKeyA
  :: Applicative f => (Key -> a -> f Bool) -> Word64Map a -> f (Word64Map a)
filterWithKeyA _ Nil           = pure Nil
filterWithKeyA f t@(Tip k x)   = (\b -> if b then t else Nil) <$> f k x
filterWithKeyA f (Bin p m l r)
  | m < 0     = liftA2 (flip (bin p m)) (filterWithKeyA f r) (filterWithKeyA f l)
  | otherwise = liftA2 (bin p m) (filterWithKeyA f l) (filterWithKeyA f r)

-- | This wasn't in Data.Bool until 4.7.0, so we define it here
bool :: a -> a -> Bool -> a
bool f _ False = f
bool _ t True  = t


-- | Traverse over the entries whose keys are missing from the other
-- map.
--
-- @since 0.5.9
traverseMissing
  :: Applicative f => (Key -> x -> f y) -> WhenMissing f x y
traverseMissing f = WhenMissing
  { missingSubtree = traverseWithKey f
  , missingKey = \k x -> Just <$> f k x }
{-# INLINE traverseMissing #-}


-- | Traverse over the entries whose keys are missing from the other
-- map, optionally producing values to put in the result. This is
-- the most powerful 'WhenMissing' tactic, but others are usually
-- more efficient.
--
-- @since 0.5.9
traverseMaybeMissing
  :: Applicative f => (Key -> x -> f (Maybe y)) -> WhenMissing f x y
traverseMaybeMissing f = WhenMissing
  { missingSubtree = traverseMaybeWithKey f
  , missingKey = f }
{-# INLINE traverseMaybeMissing #-}


-- | \(O(n)\). Traverse keys\/values and collect the 'Just' results.
--
-- @since 0.6.4
traverseMaybeWithKey
  :: Applicative f => (Key -> a -> f (Maybe b)) -> Word64Map a -> f (Word64Map b)
traverseMaybeWithKey f = go
    where
    go Nil           = pure Nil
    go (Tip k x)     = maybe Nil (Tip k) <$> f k x
    go (Bin p m l r)
      | m < 0     = liftA2 (flip (bin p m)) (go r) (go l)
      | otherwise = liftA2 (bin p m) (go l) (go r)


-- | Merge two maps.
--
-- 'merge' takes two 'WhenMissing' tactics, a 'WhenMatched' tactic
-- and two maps. It uses the tactics to merge the maps. Its behavior
-- is best understood via its fundamental tactics, 'mapMaybeMissing'
-- and 'zipWithMaybeMatched'.
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
-- m1 = [(0, \'a\'), (1, \'b\'), (3, \'c\'), (4, \'d\')]
-- m2 = [(1, "one"), (2, "two"), (4, "three")]
-- @
--
-- 'merge' will first \"align\" these maps by key:
--
-- @
-- m1 = [(0, \'a\'), (1, \'b\'),               (3, \'c\'), (4, \'d\')]
-- m2 =           [(1, "one"), (2, "two"),           (4, "three")]
-- @
--
-- It will then pass the individual entries and pairs of entries
-- to @g1@, @g2@, or @f@ as appropriate:
--
-- @
-- maybes = [g1 0 \'a\', f 1 \'b\' "one", g2 2 "two", g1 3 \'c\', f 4 \'d\' "three"]
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
-- site. To prevent excessive inlining, you should typically use
-- 'merge' to define your custom combining functions.
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
-- @since 0.5.9
merge
  :: SimpleWhenMissing a c -- ^ What to do with keys in @m1@ but not @m2@
  -> SimpleWhenMissing b c -- ^ What to do with keys in @m2@ but not @m1@
  -> SimpleWhenMatched a b c -- ^ What to do with keys in both @m1@ and @m2@
  -> Word64Map a -- ^ Map @m1@
  -> Word64Map b -- ^ Map @m2@
  -> Word64Map c
merge g1 g2 f m1 m2 =
  runIdentity $ mergeA g1 g2 f m1 m2
{-# INLINE merge #-}


-- | An applicative version of 'merge'.
--
-- 'mergeA' takes two 'WhenMissing' tactics, a 'WhenMatched'
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
-- m1 = [(0, \'a\'), (1, \'b\'), (3,\'c\'), (4, \'d\')]
-- m2 = [(1, "one"), (2, "two"), (4, "three")]
-- @
--
-- 'mergeA' will first \"align\" these maps by key:
--
-- @
-- m1 = [(0, \'a\'), (1, \'b\'),               (3, \'c\'), (4, \'d\')]
-- m2 =           [(1, "one"), (2, "two"),           (4, "three")]
-- @
--
-- It will then pass the individual entries and pairs of entries
-- to @g1@, @g2@, or @f@ as appropriate:
--
-- @
-- actions = [g1 0 \'a\', f 1 \'b\' "one", g2 2 "two", g1 3 \'c\', f 4 \'d\' "three"]
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
-- @since 0.5.9
mergeA
  :: (Applicative f)
  => WhenMissing f a c -- ^ What to do with keys in @m1@ but not @m2@
  -> WhenMissing f b c -- ^ What to do with keys in @m2@ but not @m1@
  -> WhenMatched f a b c -- ^ What to do with keys in both @m1@ and @m2@
  -> Word64Map a -- ^ Map @m1@
  -> Word64Map b -- ^ Map @m2@
  -> f (Word64Map c)
mergeA
    WhenMissing{missingSubtree = g1t, missingKey = g1k}
    WhenMissing{missingSubtree = g2t, missingKey = g2k}
    WhenMatched{matchedKey = f}
    = go
  where
    go t1  Nil = g1t t1
    go Nil t2  = g2t t2

    -- This case is already covered below.
    -- go (Tip k1 x1) (Tip k2 x2) = mergeTips k1 x1 k2 x2

    go (Tip k1 x1) t2' = merge2 t2'
      where
        merge2 t2@(Bin p2 m2 l2 r2)
          | nomatch k1 p2 m2 = linkA k1 (subsingletonBy g1k k1 x1) p2 (g2t t2)
          | zero k1 m2       = binA p2 m2 (merge2 l2) (g2t r2)
          | otherwise        = binA p2 m2 (g2t l2) (merge2 r2)
        merge2 (Tip k2 x2)   = mergeTips k1 x1 k2 x2
        merge2 Nil           = subsingletonBy g1k k1 x1

    go t1' (Tip k2 x2) = merge1 t1'
      where
        merge1 t1@(Bin p1 m1 l1 r1)
          | nomatch k2 p1 m1 = linkA p1 (g1t t1) k2 (subsingletonBy g2k k2 x2)
          | zero k2 m1       = binA p1 m1 (merge1 l1) (g1t r1)
          | otherwise        = binA p1 m1 (g1t l1) (merge1 r1)
        merge1 (Tip k1 x1)   = mergeTips k1 x1 k2 x2
        merge1 Nil           = subsingletonBy g2k k2 x2

    go t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
      | shorter m1 m2  = merge1
      | shorter m2 m1  = merge2
      | p1 == p2       = binA p1 m1 (go l1 l2) (go r1 r2)
      | otherwise      = linkA p1 (g1t t1) p2 (g2t t2)
      where
        merge1 | nomatch p2 p1 m1  = linkA p1 (g1t t1) p2 (g2t t2)
               | zero p2 m1        = binA p1 m1 (go  l1 t2) (g1t r1)
               | otherwise         = binA p1 m1 (g1t l1)    (go  r1 t2)
        merge2 | nomatch p1 p2 m2  = linkA p1 (g1t t1) p2 (g2t t2)
               | zero p1 m2        = binA p2 m2 (go  t1 l2) (g2t    r2)
               | otherwise         = binA p2 m2 (g2t    l2) (go  t1 r2)

    subsingletonBy gk k x = maybe Nil (Tip k) <$> gk k x
    {-# INLINE subsingletonBy #-}

    mergeTips k1 x1 k2 x2
      | k1 == k2  = maybe Nil (Tip k1) <$> f k1 x1 x2
      | k1 <  k2  = liftA2 (subdoubleton k1 k2) (g1k k1 x1) (g2k k2 x2)
        {-
        = link_ k1 k2 <$> subsingletonBy g1k k1 x1 <*> subsingletonBy g2k k2 x2
        -}
      | otherwise = liftA2 (subdoubleton k2 k1) (g2k k2 x2) (g1k k1 x1)
    {-# INLINE mergeTips #-}

    subdoubleton _ _   Nothing Nothing     = Nil
    subdoubleton _ k2  Nothing (Just y2)   = Tip k2 y2
    subdoubleton k1 _  (Just y1) Nothing   = Tip k1 y1
    subdoubleton k1 k2 (Just y1) (Just y2) = link k1 (Tip k1 y1) k2 (Tip k2 y2)
    {-# INLINE subdoubleton #-}

    -- A variant of 'link_' which makes sure to execute side-effects
    -- in the right order.
    linkA
        :: Applicative f
        => Prefix -> f (Word64Map a)
        -> Prefix -> f (Word64Map a)
        -> f (Word64Map a)
    linkA p1 t1 p2 t2
      | zero p1 m = binA p m t1 t2
      | otherwise = binA p m t2 t1
      where
        m = branchMask p1 p2
        p = mask p1 m
    {-# INLINE linkA #-}

    -- A variant of 'bin' that ensures that effects for negative keys are executed
    -- first.
    binA
        :: Applicative f
        => Prefix
        -> Mask
        -> f (Word64Map a)
        -> f (Word64Map a)
        -> f (Word64Map a)
    binA p m a b
      | m < 0     = liftA2 (flip (bin p m)) b a
      | otherwise = liftA2       (bin p m)  a b
    {-# INLINE binA #-}
{-# INLINE mergeA #-}


{--------------------------------------------------------------------
  Min\/Max
--------------------------------------------------------------------}

-- | \(O(\min(n,W))\). Update the value at the minimal key.
--
-- > updateMinWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"3:b"), (5,"a")]
-- > updateMinWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

updateMinWithKey :: (Key -> a -> Maybe a) -> Word64Map a -> Word64Map a
updateMinWithKey f t =
  case t of Bin p m l r | m < 0 -> binCheckRight p m l (go f r)
            _ -> go f t
  where
    go f' (Bin p m l r) = binCheckLeft p m (go f' l) r
    go f' (Tip k y) = case f' k y of
                        Just y' -> Tip k y'
                        Nothing -> Nil
    go _ Nil = error "updateMinWithKey Nil"

-- | \(O(\min(n,W))\). Update the value at the maximal key.
--
-- > updateMaxWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"b"), (5,"5:a")]
-- > updateMaxWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"

updateMaxWithKey :: (Key -> a -> Maybe a) -> Word64Map a -> Word64Map a
updateMaxWithKey f t =
  case t of Bin p m l r | m < 0 -> binCheckLeft p m (go f l) r
            _ -> go f t
  where
    go f' (Bin p m l r) = binCheckRight p m l (go f' r)
    go f' (Tip k y) = case f' k y of
                        Just y' -> Tip k y'
                        Nothing -> Nil
    go _ Nil = error "updateMaxWithKey Nil"


data View a = View {-# UNPACK #-} !Key a !(Word64Map a)

-- | \(O(\min(n,W))\). Retrieves the maximal (key,value) pair of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
--
-- > maxViewWithKey (fromList [(5,"a"), (3,"b")]) == Just ((5,"a"), singleton 3 "b")
-- > maxViewWithKey empty == Nothing

maxViewWithKey :: Word64Map a -> Maybe ((Key, a), Word64Map a)
maxViewWithKey t = case t of
  Nil -> Nothing
  _ -> Just $ case maxViewWithKeySure t of
                View k v t' -> ((k, v), t')
{-# INLINE maxViewWithKey #-}

maxViewWithKeySure :: Word64Map a -> View a
maxViewWithKeySure t =
  case t of
    Nil -> error "maxViewWithKeySure Nil"
    Bin p m l r | m < 0 ->
      case go l of View k a l' -> View k a (binCheckLeft p m l' r)
    _ -> go t
  where
    go (Bin p m l r) =
        case go r of View k a r' -> View k a (binCheckRight p m l r')
    go (Tip k y) = View k y Nil
    go Nil = error "maxViewWithKey_go Nil"
-- See note on NOINLINE at minViewWithKeySure
{-# NOINLINE maxViewWithKeySure #-}

-- | \(O(\min(n,W))\). Retrieves the minimal (key,value) pair of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
--
-- > minViewWithKey (fromList [(5,"a"), (3,"b")]) == Just ((3,"b"), singleton 5 "a")
-- > minViewWithKey empty == Nothing

minViewWithKey :: Word64Map a -> Maybe ((Key, a), Word64Map a)
minViewWithKey t =
  case t of
    Nil -> Nothing
    _ -> Just $ case minViewWithKeySure t of
                  View k v t' -> ((k, v), t')
-- We inline this to give GHC the best possible chance of
-- getting rid of the Maybe, pair, and Int constructors, as
-- well as a thunk under the Just. That is, we really want to
-- be certain this inlines!
{-# INLINE minViewWithKey #-}

minViewWithKeySure :: Word64Map a -> View a
minViewWithKeySure t =
  case t of
    Nil -> error "minViewWithKeySure Nil"
    Bin p m l r | m < 0 ->
      case go r of
        View k a r' -> View k a (binCheckRight p m l r')
    _ -> go t
  where
    go (Bin p m l r) =
        case go l of View k a l' -> View k a (binCheckLeft p m l' r)
    go (Tip k y) = View k y Nil
    go Nil = error "minViewWithKey_go Nil"
-- There's never anything significant to be gained by inlining
-- this. Sufficiently recent GHC versions will inline the wrapper
-- anyway, which should be good enough.
{-# NOINLINE minViewWithKeySure #-}

-- | \(O(\min(n,W))\). Update the value at the maximal key.
--
-- > updateMax (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "Xa")]
-- > updateMax (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"

updateMax :: (a -> Maybe a) -> Word64Map a -> Word64Map a
updateMax f = updateMaxWithKey (const f)

-- | \(O(\min(n,W))\). Update the value at the minimal key.
--
-- > updateMin (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "Xb"), (5, "a")]
-- > updateMin (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

updateMin :: (a -> Maybe a) -> Word64Map a -> Word64Map a
updateMin f = updateMinWithKey (const f)

-- | \(O(\min(n,W))\). Retrieves the maximal key of the map, and the map
-- stripped of that element, or 'Nothing' if passed an empty map.
maxView :: Word64Map a -> Maybe (a, Word64Map a)
maxView t = fmap (\((_, x), t') -> (x, t')) (maxViewWithKey t)

-- | \(O(\min(n,W))\). Retrieves the minimal key of the map, and the map
-- stripped of that element, or 'Nothing' if passed an empty map.
minView :: Word64Map a -> Maybe (a, Word64Map a)
minView t = fmap (\((_, x), t') -> (x, t')) (minViewWithKey t)

-- | \(O(\min(n,W))\). Delete and find the maximal element.
-- This function throws an error if the map is empty. Use 'maxViewWithKey'
-- if the map may be empty.
deleteFindMax :: Word64Map a -> ((Key, a), Word64Map a)
deleteFindMax = fromMaybe (error "deleteFindMax: empty map has no maximal element") . maxViewWithKey

-- | \(O(\min(n,W))\). Delete and find the minimal element.
-- This function throws an error if the map is empty. Use 'minViewWithKey'
-- if the map may be empty.
deleteFindMin :: Word64Map a -> ((Key, a), Word64Map a)
deleteFindMin = fromMaybe (error "deleteFindMin: empty map has no minimal element") . minViewWithKey

-- | \(O(\min(n,W))\). The minimal key of the map. Returns 'Nothing' if the map is empty.
lookupMin :: Word64Map a -> Maybe (Key, a)
lookupMin Nil = Nothing
lookupMin (Tip k v) = Just (k,v)
lookupMin (Bin _ m l r)
  | m < 0     = go r
  | otherwise = go l
    where go (Tip k v)      = Just (k,v)
          go (Bin _ _ l' _) = go l'
          go Nil            = Nothing

-- | \(O(\min(n,W))\). The minimal key of the map. Calls 'error' if the map is empty.
-- Use 'minViewWithKey' if the map may be empty.
findMin :: Word64Map a -> (Key, a)
findMin t
  | Just r <- lookupMin t = r
  | otherwise = error "findMin: empty map has no minimal element"

-- | \(O(\min(n,W))\). The maximal key of the map. Returns 'Nothing' if the map is empty.
lookupMax :: Word64Map a -> Maybe (Key, a)
lookupMax Nil = Nothing
lookupMax (Tip k v) = Just (k,v)
lookupMax (Bin _ m l r)
  | m < 0     = go l
  | otherwise = go r
    where go (Tip k v)      = Just (k,v)
          go (Bin _ _ _ r') = go r'
          go Nil            = Nothing

-- | \(O(\min(n,W))\). The maximal key of the map. Calls 'error' if the map is empty.
-- Use 'maxViewWithKey' if the map may be empty.
findMax :: Word64Map a -> (Key, a)
findMax t
  | Just r <- lookupMax t = r
  | otherwise = error "findMax: empty map has no maximal element"

-- | \(O(\min(n,W))\). Delete the minimal key. Returns an empty map if the map is empty.
--
-- Note that this is a change of behaviour for consistency with 'Data.Map.Map' &#8211;
-- versions prior to 0.5 threw an error if the 'Word64Map' was already empty.
deleteMin :: Word64Map a -> Word64Map a
deleteMin = maybe Nil snd . minView

-- | \(O(\min(n,W))\). Delete the maximal key. Returns an empty map if the map is empty.
--
-- Note that this is a change of behaviour for consistency with 'Data.Map.Map' &#8211;
-- versions prior to 0.5 threw an error if the 'Word64Map' was already empty.
deleteMax :: Word64Map a -> Word64Map a
deleteMax = maybe Nil snd . maxView


{--------------------------------------------------------------------
  Submap
--------------------------------------------------------------------}
-- | \(O(n+m)\). Is this a proper submap? (ie. a submap but not equal).
-- Defined as (@'isProperSubmapOf' = 'isProperSubmapOfBy' (==)@).
isProperSubmapOf :: Eq a => Word64Map a -> Word64Map a -> Bool
isProperSubmapOf m1 m2
  = isProperSubmapOfBy (==) m1 m2

{- | \(O(n+m)\). Is this a proper submap? (ie. a submap but not equal).
 The expression (@'isProperSubmapOfBy' f m1 m2@) returns 'True' when
 @keys m1@ and @keys m2@ are not equal,
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
isProperSubmapOfBy :: (a -> b -> Bool) -> Word64Map a -> Word64Map b -> Bool
isProperSubmapOfBy predicate t1 t2
  = case submapCmp predicate t1 t2 of
      LT -> True
      _  -> False

submapCmp :: (a -> b -> Bool) -> Word64Map a -> Word64Map b -> Ordering
submapCmp predicate t1@(Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  | shorter m1 m2  = GT
  | shorter m2 m1  = submapCmpLt
  | p1 == p2       = submapCmpEq
  | otherwise      = GT  -- disjoint
  where
    submapCmpLt | nomatch p1 p2 m2  = GT
                | zero p1 m2        = submapCmp predicate t1 l2
                | otherwise         = submapCmp predicate t1 r2
    submapCmpEq = case (submapCmp predicate l1 l2, submapCmp predicate r1 r2) of
                    (GT,_ ) -> GT
                    (_ ,GT) -> GT
                    (EQ,EQ) -> EQ
                    _       -> LT

submapCmp _         (Bin _ _ _ _) _  = GT
submapCmp predicate (Tip kx x) (Tip ky y)
  | (kx == ky) && predicate x y = EQ
  | otherwise                   = GT  -- disjoint
submapCmp predicate (Tip k x) t
  = case lookup k t of
     Just y | predicate x y -> LT
     _                      -> GT -- disjoint
submapCmp _    Nil Nil = EQ
submapCmp _    Nil _   = LT

-- | \(O(n+m)\). Is this a submap?
-- Defined as (@'isSubmapOf' = 'isSubmapOfBy' (==)@).
isSubmapOf :: Eq a => Word64Map a -> Word64Map a -> Bool
isSubmapOf m1 m2
  = isSubmapOfBy (==) m1 m2

{- | \(O(n+m)\).
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
isSubmapOfBy :: (a -> b -> Bool) -> Word64Map a -> Word64Map b -> Bool
isSubmapOfBy predicate t1@(Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  | shorter m1 m2  = False
  | shorter m2 m1  = match p1 p2 m2 &&
                       if zero p1 m2
                       then isSubmapOfBy predicate t1 l2
                       else isSubmapOfBy predicate t1 r2
  | otherwise      = (p1==p2) && isSubmapOfBy predicate l1 l2 && isSubmapOfBy predicate r1 r2
isSubmapOfBy _         (Bin _ _ _ _) _ = False
isSubmapOfBy predicate (Tip k x) t     = case lookup k t of
                                         Just y  -> predicate x y
                                         Nothing -> False
isSubmapOfBy _         Nil _           = True

{--------------------------------------------------------------------
  Mapping
--------------------------------------------------------------------}
-- | \(O(n)\). Map a function over all values in the map.
--
-- > map (++ "x") (fromList [(5,"a"), (3,"b")]) == fromList [(3, "bx"), (5, "ax")]

map :: (a -> b) -> Word64Map a -> Word64Map b
map f = go
  where
    go (Bin p m l r) = Bin p m (go l) (go r)
    go (Tip k x)     = Tip k (f x)
    go Nil           = Nil

{-# NOINLINE [1] map #-}
{-# RULES
"map/map" forall f g xs . map f (map g xs) = map (f . g) xs
"map/coerce" map coerce = coerce
 #-}

-- | \(O(n)\). Map a function over all values in the map.
--
-- > let f key x = (show key) ++ ":" ++ x
-- > mapWithKey f (fromList [(5,"a"), (3,"b")]) == fromList [(3, "3:b"), (5, "5:a")]

mapWithKey :: (Key -> a -> b) -> Word64Map a -> Word64Map b
mapWithKey f t
  = case t of
      Bin p m l r -> Bin p m (mapWithKey f l) (mapWithKey f r)
      Tip k x     -> Tip k (f k x)
      Nil         -> Nil

{-# NOINLINE [1] mapWithKey #-}
{-# RULES
"mapWithKey/mapWithKey" forall f g xs . mapWithKey f (mapWithKey g xs) =
  mapWithKey (\k a -> f k (g k a)) xs
"mapWithKey/map" forall f g xs . mapWithKey f (map g xs) =
  mapWithKey (\k a -> f k (g a)) xs
"map/mapWithKey" forall f g xs . map f (mapWithKey g xs) =
  mapWithKey (\k a -> f (g k a)) xs
 #-}

-- | \(O(n)\).
-- @'traverseWithKey' f s == 'fromList' <$> 'traverse' (\(k, v) -> (,) k <$> f k v) ('toList' m)@
-- That is, behaves exactly like a regular 'traverse' except that the traversing
-- function also has access to the key associated with a value.
--
-- > traverseWithKey (\k v -> if odd k then Just (succ v) else Nothing) (fromList [(1, 'a'), (5, 'e')]) == Just (fromList [(1, 'b'), (5, 'f')])
-- > traverseWithKey (\k v -> if odd k then Just (succ v) else Nothing) (fromList [(2, 'c')])           == Nothing
traverseWithKey :: Applicative t => (Key -> a -> t b) -> Word64Map a -> t (Word64Map b)
traverseWithKey f = go
  where
    go Nil = pure Nil
    go (Tip k v) = Tip k <$> f k v
    go (Bin p m l r)
      | m < 0     = liftA2 (flip (Bin p m)) (go r) (go l)
      | otherwise = liftA2 (Bin p m) (go l) (go r)
{-# INLINE traverseWithKey #-}

-- | \(O(n)\). The function @'mapAccum'@ threads an accumulating
-- argument through the map in ascending order of keys.
--
-- > let f a b = (a ++ b, b ++ "X")
-- > mapAccum f "Everything: " (fromList [(5,"a"), (3,"b")]) == ("Everything: ba", fromList [(3, "bX"), (5, "aX")])

mapAccum :: (a -> b -> (a,c)) -> a -> Word64Map b -> (a,Word64Map c)
mapAccum f = mapAccumWithKey (\a' _ x -> f a' x)

-- | \(O(n)\). The function @'mapAccumWithKey'@ threads an accumulating
-- argument through the map in ascending order of keys.
--
-- > let f a k b = (a ++ " " ++ (show k) ++ "-" ++ b, b ++ "X")
-- > mapAccumWithKey f "Everything:" (fromList [(5,"a"), (3,"b")]) == ("Everything: 3-b 5-a", fromList [(3, "bX"), (5, "aX")])

mapAccumWithKey :: (a -> Key -> b -> (a,c)) -> a -> Word64Map b -> (a,Word64Map c)
mapAccumWithKey f a t
  = mapAccumL f a t

-- | \(O(n)\). The function @'mapAccumL'@ threads an accumulating
-- argument through the map in ascending order of keys.
mapAccumL :: (a -> Key -> b -> (a,c)) -> a -> Word64Map b -> (a,Word64Map c)
mapAccumL f a t
  = case t of
      Bin p m l r
        | m < 0 ->
            let (a1,r') = mapAccumL f a r
                (a2,l') = mapAccumL f a1 l
            in (a2,Bin p m l' r')
        | otherwise  ->
            let (a1,l') = mapAccumL f a l
                (a2,r') = mapAccumL f a1 r
            in (a2,Bin p m l' r')
      Tip k x     -> let (a',x') = f a k x in (a',Tip k x')
      Nil         -> (a,Nil)

-- | \(O(n)\). The function @'mapAccumRWithKey'@ threads an accumulating
-- argument through the map in descending order of keys.
mapAccumRWithKey :: (a -> Key -> b -> (a,c)) -> a -> Word64Map b -> (a,Word64Map c)
mapAccumRWithKey f a t
  = case t of
      Bin p m l r
        | m < 0 ->
            let (a1,l') = mapAccumRWithKey f a l
                (a2,r') = mapAccumRWithKey f a1 r
            in (a2,Bin p m l' r')
        | otherwise  ->
            let (a1,r') = mapAccumRWithKey f a r
                (a2,l') = mapAccumRWithKey f a1 l
            in (a2,Bin p m l' r')
      Tip k x     -> let (a',x') = f a k x in (a',Tip k x')
      Nil         -> (a,Nil)

-- | \(O(n \min(n,W))\).
-- @'mapKeys' f s@ is the map obtained by applying @f@ to each key of @s@.
--
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the value at the greatest of the
-- original keys is retained.
--
-- > mapKeys (+ 1) (fromList [(5,"a"), (3,"b")])                        == fromList [(4, "b"), (6, "a")]
-- > mapKeys (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 1 "c"
-- > mapKeys (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 3 "c"

mapKeys :: (Key->Key) -> Word64Map a -> Word64Map a
mapKeys f = fromList . foldrWithKey (\k x xs -> (f k, x) : xs) []

-- | \(O(n \min(n,W))\).
-- @'mapKeysWith' c f s@ is the map obtained by applying @f@ to each key of @s@.
--
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the associated values will be
-- combined using @c@.
--
-- > mapKeysWith (++) (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 1 "cdab"
-- > mapKeysWith (++) (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 3 "cdab"

mapKeysWith :: (a -> a -> a) -> (Key->Key) -> Word64Map a -> Word64Map a
mapKeysWith c f
  = fromListWith c . foldrWithKey (\k x xs -> (f k, x) : xs) []

-- | \(O(n \min(n,W))\).
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
-- This function has slightly better performance than 'mapKeys'.
--
-- > mapKeysMonotonic (\ k -> k * 2) (fromList [(5,"a"), (3,"b")]) == fromList [(6, "b"), (10, "a")]

mapKeysMonotonic :: (Key->Key) -> Word64Map a -> Word64Map a
mapKeysMonotonic f
  = fromDistinctAscList . foldrWithKey (\k x xs -> (f k, x) : xs) []

{--------------------------------------------------------------------
  Filter
--------------------------------------------------------------------}
-- | \(O(n)\). Filter all values that satisfy some predicate.
--
-- > filter (> "a") (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > filter (> "x") (fromList [(5,"a"), (3,"b")]) == empty
-- > filter (< "a") (fromList [(5,"a"), (3,"b")]) == empty

filter :: (a -> Bool) -> Word64Map a -> Word64Map a
filter p m
  = filterWithKey (\_ x -> p x) m

-- | \(O(n)\). Filter all keys\/values that satisfy some predicate.
--
-- > filterWithKey (\k _ -> k > 4) (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

filterWithKey :: (Key -> a -> Bool) -> Word64Map a -> Word64Map a
filterWithKey predicate = go
    where
    go Nil           = Nil
    go t@(Tip k x)   = if predicate k x then t else Nil
    go (Bin p m l r) = bin p m (go l) (go r)

-- | \(O(n)\). Partition the map according to some predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
--
-- > partition (> "a") (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", singleton 5 "a")
-- > partition (< "x") (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], empty)
-- > partition (> "x") (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3, "b"), (5, "a")])

partition :: (a -> Bool) -> Word64Map a -> (Word64Map a,Word64Map a)
partition p m
  = partitionWithKey (\_ x -> p x) m

-- | \(O(n)\). Partition the map according to some predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
--
-- > partitionWithKey (\ k _ -> k > 3) (fromList [(5,"a"), (3,"b")]) == (singleton 5 "a", singleton 3 "b")
-- > partitionWithKey (\ k _ -> k < 7) (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], empty)
-- > partitionWithKey (\ k _ -> k > 7) (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3, "b"), (5, "a")])

partitionWithKey :: (Key -> a -> Bool) -> Word64Map a -> (Word64Map a,Word64Map a)
partitionWithKey predicate0 t0 = toPair $ go predicate0 t0
  where
    go predicate t =
      case t of
        Bin p m l r ->
          let (l1 :*: l2) = go predicate l
              (r1 :*: r2) = go predicate r
          in bin p m l1 r1 :*: bin p m l2 r2
        Tip k x
          | predicate k x -> (t :*: Nil)
          | otherwise     -> (Nil :*: t)
        Nil -> (Nil :*: Nil)

-- | \(O(\min(n,W))\). Take while a predicate on the keys holds.
-- The user is responsible for ensuring that for all @Int@s, @j \< k ==\> p j \>= p k@.
-- See note at 'spanAntitone'.
--
-- @
-- takeWhileAntitone p = 'fromDistinctAscList' . 'Data.List.takeWhile' (p . fst) . 'toList'
-- takeWhileAntitone p = 'filterWithKey' (\\k _ -> p k)
-- @
--
-- @since 0.6.7
takeWhileAntitone :: (Key -> Bool) -> Word64Map a -> Word64Map a
takeWhileAntitone predicate t =
  case t of
    Bin p m l r
      | m < 0 ->
        if predicate 0 -- handle negative numbers.
        then bin p m (go predicate l) r
        else go predicate r
    _ -> go predicate t
  where
    go predicate' (Bin p m l r)
      | predicate' $! p+m = bin p m l (go predicate' r)
      | otherwise         = go predicate' l
    go predicate' t'@(Tip ky _)
      | predicate' ky = t'
      | otherwise     = Nil
    go _ Nil = Nil

-- | \(O(\min(n,W))\). Drop while a predicate on the keys holds.
-- The user is responsible for ensuring that for all @Int@s, @j \< k ==\> p j \>= p k@.
-- See note at 'spanAntitone'.
--
-- @
-- dropWhileAntitone p = 'fromDistinctAscList' . 'Data.List.dropWhile' (p . fst) . 'toList'
-- dropWhileAntitone p = 'filterWithKey' (\\k _ -> not (p k))
-- @
--
-- @since 0.6.7
dropWhileAntitone :: (Key -> Bool) -> Word64Map a -> Word64Map a
dropWhileAntitone predicate t =
  case t of
    Bin p m l r
      | m < 0 ->
        if predicate 0 -- handle negative numbers.
        then go predicate l
        else bin p m l (go predicate r)
    _ -> go predicate t
  where
    go predicate' (Bin p m l r)
      | predicate' $! p+m = go predicate' r
      | otherwise         = bin p m (go predicate' l) r
    go predicate' t'@(Tip ky _)
      | predicate' ky = Nil
      | otherwise     = t'
    go _ Nil = Nil

-- | \(O(\min(n,W))\). Divide a map at the point where a predicate on the keys stops holding.
-- The user is responsible for ensuring that for all @Int@s, @j \< k ==\> p j \>= p k@.
--
-- @
-- spanAntitone p xs = ('takeWhileAntitone' p xs, 'dropWhileAntitone' p xs)
-- spanAntitone p xs = 'partitionWithKey' (\\k _ -> p k) xs
-- @
--
-- Note: if @p@ is not actually antitone, then @spanAntitone@ will split the map
-- at some /unspecified/ point.
--
-- @since 0.6.7
spanAntitone :: (Key -> Bool) -> Word64Map a -> (Word64Map a, Word64Map a)
spanAntitone predicate t =
  case t of
    Bin p m l r
      | m < 0 ->
        if predicate 0 -- handle negative numbers.
        then
          case go predicate l of
            (lt :*: gt) ->
              let !lt' = bin p m lt r
              in (lt', gt)
        else
          case go predicate r of
            (lt :*: gt) ->
              let !gt' = bin p m l gt
              in (lt, gt')
    _ -> case go predicate t of
          (lt :*: gt) -> (lt, gt)
  where
    go predicate' (Bin p m l r)
      | predicate' $! p+m = case go predicate' r of (lt :*: gt) -> bin p m l lt :*: gt
      | otherwise         = case go predicate' l of (lt :*: gt) -> lt :*: bin p m gt r
    go predicate' t'@(Tip ky _)
      | predicate' ky = (t' :*: Nil)
      | otherwise     = (Nil :*: t')
    go _ Nil = (Nil :*: Nil)

-- | \(O(n)\). Map values and collect the 'Just' results.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > mapMaybe f (fromList [(5,"a"), (3,"b")]) == singleton 5 "new a"

mapMaybe :: (a -> Maybe b) -> Word64Map a -> Word64Map b
mapMaybe f = mapMaybeWithKey (\_ x -> f x)

-- | \(O(n)\). Map keys\/values and collect the 'Just' results.
--
-- > let f k _ = if k < 5 then Just ("key : " ++ (show k)) else Nothing
-- > mapMaybeWithKey f (fromList [(5,"a"), (3,"b")]) == singleton 3 "key : 3"

mapMaybeWithKey :: (Key -> a -> Maybe b) -> Word64Map a -> Word64Map b
mapMaybeWithKey f (Bin p m l r)
  = bin p m (mapMaybeWithKey f l) (mapMaybeWithKey f r)
mapMaybeWithKey f (Tip k x) = case f k x of
  Just y  -> Tip k y
  Nothing -> Nil
mapMaybeWithKey _ Nil = Nil

-- | \(O(n)\). Map values and separate the 'Left' and 'Right' results.
--
-- > let f a = if a < "c" then Left a else Right a
-- > mapEither f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (fromList [(3,"b"), (5,"a")], fromList [(1,"x"), (7,"z")])
-- >
-- > mapEither (\ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (empty, fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])

mapEither :: (a -> Either b c) -> Word64Map a -> (Word64Map b, Word64Map c)
mapEither f m
  = mapEitherWithKey (\_ x -> f x) m

-- | \(O(n)\). Map keys\/values and separate the 'Left' and 'Right' results.
--
-- > let f k a = if k < 5 then Left (k * 2) else Right (a ++ a)
-- > mapEitherWithKey f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (fromList [(1,2), (3,6)], fromList [(5,"aa"), (7,"zz")])
-- >
-- > mapEitherWithKey (\_ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (empty, fromList [(1,"x"), (3,"b"), (5,"a"), (7,"z")])

mapEitherWithKey :: (Key -> a -> Either b c) -> Word64Map a -> (Word64Map b, Word64Map c)
mapEitherWithKey f0 t0 = toPair $ go f0 t0
  where
    go f (Bin p m l r) =
      bin p m l1 r1 :*: bin p m l2 r2
      where
        (l1 :*: l2) = go f l
        (r1 :*: r2) = go f r
    go f (Tip k x) = case f k x of
      Left y  -> (Tip k y :*: Nil)
      Right z -> (Nil :*: Tip k z)
    go _ Nil = (Nil :*: Nil)

-- | \(O(\min(n,W))\). The expression (@'split' k map@) is a pair @(map1,map2)@
-- where all keys in @map1@ are lower than @k@ and all keys in
-- @map2@ larger than @k@. Any key equal to @k@ is found in neither @map1@ nor @map2@.
--
-- > split 2 (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3,"b"), (5,"a")])
-- > split 3 (fromList [(5,"a"), (3,"b")]) == (empty, singleton 5 "a")
-- > split 4 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", singleton 5 "a")
-- > split 5 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", empty)
-- > split 6 (fromList [(5,"a"), (3,"b")]) == (fromList [(3,"b"), (5,"a")], empty)

split :: Key -> Word64Map a -> (Word64Map a, Word64Map a)
split k t =
  case t of
    Bin p m l r
      | m < 0 ->
        if k >= 0 -- handle negative numbers.
        then
          case go k l of
            (lt :*: gt) ->
              let !lt' = bin p m lt r
              in (lt', gt)
        else
          case go k r of
            (lt :*: gt) ->
              let !gt' = bin p m l gt
              in (lt, gt')
    _ -> case go k t of
          (lt :*: gt) -> (lt, gt)
  where
    go k' t'@(Bin p m l r)
      | nomatch k' p m = if k' > p then t' :*: Nil else Nil :*: t'
      | zero k' m = case go k' l of (lt :*: gt) -> lt :*: bin p m gt r
      | otherwise = case go k' r of (lt :*: gt) -> bin p m l lt :*: gt
    go k' t'@(Tip ky _)
      | k' > ky   = (t' :*: Nil)
      | k' < ky   = (Nil :*: t')
      | otherwise = (Nil :*: Nil)
    go _ Nil = (Nil :*: Nil)


data SplitLookup a = SplitLookup !(Word64Map a) !(Maybe a) !(Word64Map a)

mapLT :: (Word64Map a -> Word64Map a) -> SplitLookup a -> SplitLookup a
mapLT f (SplitLookup lt fnd gt) = SplitLookup (f lt) fnd gt
{-# INLINE mapLT #-}

mapGT :: (Word64Map a -> Word64Map a) -> SplitLookup a -> SplitLookup a
mapGT f (SplitLookup lt fnd gt) = SplitLookup lt fnd (f gt)
{-# INLINE mapGT #-}

-- | \(O(\min(n,W))\). Performs a 'split' but also returns whether the pivot
-- key was found in the original map.
--
-- > splitLookup 2 (fromList [(5,"a"), (3,"b")]) == (empty, Nothing, fromList [(3,"b"), (5,"a")])
-- > splitLookup 3 (fromList [(5,"a"), (3,"b")]) == (empty, Just "b", singleton 5 "a")
-- > splitLookup 4 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", Nothing, singleton 5 "a")
-- > splitLookup 5 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", Just "a", empty)
-- > splitLookup 6 (fromList [(5,"a"), (3,"b")]) == (fromList [(3,"b"), (5,"a")], Nothing, empty)

splitLookup :: Key -> Word64Map a -> (Word64Map a, Maybe a, Word64Map a)
splitLookup k t =
  case
    case t of
      Bin p m l r
        | m < 0 ->
          if k >= 0 -- handle negative numbers.
          then mapLT (flip (bin p m) r) (go k l)
          else mapGT (bin p m l) (go k r)
      _ -> go k t
  of SplitLookup lt fnd gt -> (lt, fnd, gt)
  where
    go k' t'@(Bin p m l r)
      | nomatch k' p m =
          if k' > p
          then SplitLookup t' Nothing Nil
          else SplitLookup Nil Nothing t'
      | zero k' m = mapGT (flip (bin p m) r) (go k' l)
      | otherwise = mapLT (bin p m l) (go k' r)
    go k' t'@(Tip ky y)
      | k' > ky   = SplitLookup t'  Nothing  Nil
      | k' < ky   = SplitLookup Nil Nothing  t'
      | otherwise = SplitLookup Nil (Just y) Nil
    go _ Nil      = SplitLookup Nil Nothing  Nil

{--------------------------------------------------------------------
  Fold
--------------------------------------------------------------------}
-- | \(O(n)\). Fold the values in the map using the given right-associative
-- binary operator, such that @'foldr' f z == 'Prelude.foldr' f z . 'elems'@.
--
-- For example,
--
-- > elems map = foldr (:) [] map
--
-- > let f a len = len + (length a)
-- > foldr f 0 (fromList [(5,"a"), (3,"bbb")]) == 4
foldr :: (a -> b -> b) -> b -> Word64Map a -> b
foldr f z = \t ->      -- Use lambda t to be inlinable with two arguments only.
  case t of
    Bin _ m l r
      | m < 0 -> go (go z l) r -- put negative numbers before
      | otherwise -> go (go z r) l
    _ -> go z t
  where
    go z' Nil           = z'
    go z' (Tip _ x)     = f x z'
    go z' (Bin _ _ l r) = go (go z' r) l
{-# INLINE foldr #-}

-- | \(O(n)\). A strict version of 'foldr'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldr' :: (a -> b -> b) -> b -> Word64Map a -> b
foldr' f z = \t ->      -- Use lambda t to be inlinable with two arguments only.
  case t of
    Bin _ m l r
      | m < 0 -> go (go z l) r -- put negative numbers before
      | otherwise -> go (go z r) l
    _ -> go z t
  where
    go !z' Nil          = z'
    go z' (Tip _ x)     = f x z'
    go z' (Bin _ _ l r) = go (go z' r) l
{-# INLINE foldr' #-}

-- | \(O(n)\). Fold the values in the map using the given left-associative
-- binary operator, such that @'foldl' f z == 'Prelude.foldl' f z . 'elems'@.
--
-- For example,
--
-- > elems = reverse . foldl (flip (:)) []
--
-- > let f len a = len + (length a)
-- > foldl f 0 (fromList [(5,"a"), (3,"bbb")]) == 4
foldl :: (a -> b -> a) -> a -> Word64Map b -> a
foldl f z = \t ->      -- Use lambda t to be inlinable with two arguments only.
  case t of
    Bin _ m l r
      | m < 0 -> go (go z r) l -- put negative numbers before
      | otherwise -> go (go z l) r
    _ -> go z t
  where
    go z' Nil           = z'
    go z' (Tip _ x)     = f z' x
    go z' (Bin _ _ l r) = go (go z' l) r
{-# INLINE foldl #-}

-- | \(O(n)\). A strict version of 'foldl'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldl' :: (a -> b -> a) -> a -> Word64Map b -> a
foldl' f z = \t ->      -- Use lambda t to be inlinable with two arguments only.
  case t of
    Bin _ m l r
      | m < 0 -> go (go z r) l -- put negative numbers before
      | otherwise -> go (go z l) r
    _ -> go z t
  where
    go !z' Nil          = z'
    go z' (Tip _ x)     = f z' x
    go z' (Bin _ _ l r) = go (go z' l) r
{-# INLINE foldl' #-}

-- | \(O(n)\). Fold the keys and values in the map using the given right-associative
-- binary operator, such that
-- @'foldrWithKey' f z == 'Prelude.foldr' ('uncurry' f) z . 'toAscList'@.
--
-- For example,
--
-- > keys map = foldrWithKey (\k x ks -> k:ks) [] map
--
-- > let f k a result = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
-- > foldrWithKey f "Map: " (fromList [(5,"a"), (3,"b")]) == "Map: (5:a)(3:b)"
foldrWithKey :: (Key -> a -> b -> b) -> b -> Word64Map a -> b
foldrWithKey f z = \t ->      -- Use lambda t to be inlinable with two arguments only.
  case t of
    Bin _ m l r
      | m < 0 -> go (go z l) r -- put negative numbers before
      | otherwise -> go (go z r) l
    _ -> go z t
  where
    go z' Nil           = z'
    go z' (Tip kx x)    = f kx x z'
    go z' (Bin _ _ l r) = go (go z' r) l
{-# INLINE foldrWithKey #-}

-- | \(O(n)\). A strict version of 'foldrWithKey'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldrWithKey' :: (Key -> a -> b -> b) -> b -> Word64Map a -> b
foldrWithKey' f z = \t ->      -- Use lambda t to be inlinable with two arguments only.
  case t of
    Bin _ m l r
      | m < 0 -> go (go z l) r -- put negative numbers before
      | otherwise -> go (go z r) l
    _ -> go z t
  where
    go !z' Nil          = z'
    go z' (Tip kx x)    = f kx x z'
    go z' (Bin _ _ l r) = go (go z' r) l
{-# INLINE foldrWithKey' #-}

-- | \(O(n)\). Fold the keys and values in the map using the given left-associative
-- binary operator, such that
-- @'foldlWithKey' f z == 'Prelude.foldl' (\\z' (kx, x) -> f z' kx x) z . 'toAscList'@.
--
-- For example,
--
-- > keys = reverse . foldlWithKey (\ks k x -> k:ks) []
--
-- > let f result k a = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
-- > foldlWithKey f "Map: " (fromList [(5,"a"), (3,"b")]) == "Map: (3:b)(5:a)"
foldlWithKey :: (a -> Key -> b -> a) -> a -> Word64Map b -> a
foldlWithKey f z = \t ->      -- Use lambda t to be inlinable with two arguments only.
  case t of
    Bin _ m l r
      | m < 0 -> go (go z r) l -- put negative numbers before
      | otherwise -> go (go z l) r
    _ -> go z t
  where
    go z' Nil           = z'
    go z' (Tip kx x)    = f z' kx x
    go z' (Bin _ _ l r) = go (go z' l) r
{-# INLINE foldlWithKey #-}

-- | \(O(n)\). A strict version of 'foldlWithKey'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldlWithKey' :: (a -> Key -> b -> a) -> a -> Word64Map b -> a
foldlWithKey' f z = \t ->      -- Use lambda t to be inlinable with two arguments only.
  case t of
    Bin _ m l r
      | m < 0 -> go (go z r) l -- put negative numbers before
      | otherwise -> go (go z l) r
    _ -> go z t
  where
    go !z' Nil          = z'
    go z' (Tip kx x)    = f z' kx x
    go z' (Bin _ _ l r) = go (go z' l) r
{-# INLINE foldlWithKey' #-}

-- | \(O(n)\). Fold the keys and values in the map using the given monoid, such that
--
-- @'foldMapWithKey' f = 'Prelude.fold' . 'mapWithKey' f@
--
-- This can be an asymptotically faster than 'foldrWithKey' or 'foldlWithKey' for some monoids.
--
-- @since 0.5.4
foldMapWithKey :: Monoid m => (Key -> a -> m) -> Word64Map a -> m
foldMapWithKey f = go
  where
    go Nil           = mempty
    go (Tip kx x)    = f kx x
    go (Bin _ m l r)
      | m < 0     = go r `mappend` go l
      | otherwise = go l `mappend` go r
{-# INLINE foldMapWithKey #-}

{--------------------------------------------------------------------
  List variations
--------------------------------------------------------------------}
-- | \(O(n)\).
-- Return all elements of the map in the ascending order of their keys.
-- Subject to list fusion.
--
-- > elems (fromList [(5,"a"), (3,"b")]) == ["b","a"]
-- > elems empty == []

elems :: Word64Map a -> [a]
elems = foldr (:) []

-- | \(O(n)\). Return all keys of the map in ascending order. Subject to list
-- fusion.
--
-- > keys (fromList [(5,"a"), (3,"b")]) == [3,5]
-- > keys empty == []

keys  :: Word64Map a -> [Key]
keys = foldrWithKey (\k _ ks -> k : ks) []

-- | \(O(n)\). An alias for 'toAscList'. Returns all key\/value pairs in the
-- map in ascending key order. Subject to list fusion.
--
-- > assocs (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
-- > assocs empty == []

assocs :: Word64Map a -> [(Key,a)]
assocs = toAscList

-- | \(O(n \min(n,W))\). The set of all keys of the map.
--
-- > keysSet (fromList [(5,"a"), (3,"b")]) == Data.Word64Set.fromList [3,5]
-- > keysSet empty == Data.Word64Set.empty

keysSet :: Word64Map a -> Word64Set.Word64Set
keysSet Nil = Word64Set.Nil
keysSet (Tip kx _) = Word64Set.singleton kx
keysSet (Bin p m l r)
  | m .&. Word64Set.suffixBitMask == 0 = Word64Set.Bin p m (keysSet l) (keysSet r)
  | otherwise = Word64Set.Tip (p .&. Word64Set.prefixBitMask) (computeBm (computeBm 0 l) r)
  where computeBm !acc (Bin _ _ l' r') = computeBm (computeBm acc l') r'
        computeBm acc (Tip kx _) = acc .|. Word64Set.bitmapOf kx
        computeBm _   Nil = error "Data.Word64Set.keysSet: Nil"

-- | \(O(n)\). Build a map from a set of keys and a function which for each key
-- computes its value.
--
-- > fromSet (\k -> replicate k 'a') (Data.Word64Set.fromList [3, 5]) == fromList [(5,"aaaaa"), (3,"aaa")]
-- > fromSet undefined Data.Word64Set.empty == empty

fromSet :: (Key -> a) -> Word64Set.Word64Set -> Word64Map a
fromSet _ Word64Set.Nil = Nil
fromSet f (Word64Set.Bin p m l r) = Bin p m (fromSet f l) (fromSet f r)
fromSet f (Word64Set.Tip kx bm) = buildTree f kx bm (Word64Set.suffixBitMask + 1)
  where
    -- This is slightly complicated, as we to convert the dense
    -- representation of Word64Set into tree representation of Word64Map.
    --
    -- We are given a nonzero bit mask 'bmask' of 'bits' bits with
    -- prefix 'prefix'. We split bmask into halves corresponding
    -- to left and right subtree. If they are both nonempty, we
    -- create a Bin node, otherwise exactly one of them is nonempty
    -- and we construct the Word64Map from that half.
    buildTree g !prefix !bmask bits = case bits of
      0 -> Tip prefix (g prefix)
      _ -> case intFromNat ((natFromInt bits) `shiftRL` 1) of
        bits2
          | bmask .&. ((1 `shiftLL` fromIntegral bits2) - 1) == 0 ->
              buildTree g (prefix + bits2) (bmask `shiftRL` fromIntegral bits2) bits2
          | (bmask `shiftRL` fromIntegral bits2) .&. ((1 `shiftLL` fromIntegral bits2) - 1) == 0 ->
              buildTree g prefix bmask bits2
          | otherwise ->
              Bin prefix bits2
                (buildTree g prefix bmask bits2)
                (buildTree g (prefix + bits2) (bmask `shiftRL` fromIntegral bits2) bits2)

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}

-- | @since 0.5.6.2
instance GHCExts.IsList (Word64Map a) where
  type Item (Word64Map a) = (Key,a)
  fromList = fromList
  toList   = toList

-- | \(O(n)\). Convert the map to a list of key\/value pairs. Subject to list
-- fusion.
--
-- > toList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
-- > toList empty == []

toList :: Word64Map a -> [(Key,a)]
toList = toAscList

-- | \(O(n)\). Convert the map to a list of key\/value pairs where the
-- keys are in ascending order. Subject to list fusion.
--
-- > toAscList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]

toAscList :: Word64Map a -> [(Key,a)]
toAscList = foldrWithKey (\k x xs -> (k,x):xs) []

-- | \(O(n)\). Convert the map to a list of key\/value pairs where the keys
-- are in descending order. Subject to list fusion.
--
-- > toDescList (fromList [(5,"a"), (3,"b")]) == [(5,"a"), (3,"b")]

toDescList :: Word64Map a -> [(Key,a)]
toDescList = foldlWithKey (\xs k x -> (k,x):xs) []

-- List fusion for the list generating functions.
-- The foldrFB and foldlFB are fold{r,l}WithKey equivalents, used for list fusion.
-- They are important to convert unfused methods back, see mapFB in prelude.
foldrFB :: (Key -> a -> b -> b) -> b -> Word64Map a -> b
foldrFB = foldrWithKey
{-# INLINE[0] foldrFB #-}
foldlFB :: (a -> Key -> b -> a) -> a -> Word64Map b -> a
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
{-# RULES "Word64Map.elems" [~1] forall m . elems m = build (\c n -> foldrFB (\_ x xs -> c x xs) n m) #-}
{-# RULES "Word64Map.elemsBack" [1] foldrFB (\_ x xs -> x : xs) [] = elems #-}
{-# RULES "Word64Map.keys" [~1] forall m . keys m = build (\c n -> foldrFB (\k _ xs -> c k xs) n m) #-}
{-# RULES "Word64Map.keysBack" [1] foldrFB (\k _ xs -> k : xs) [] = keys #-}
{-# RULES "Word64Map.toAscList" [~1] forall m . toAscList m = build (\c n -> foldrFB (\k x xs -> c (k,x) xs) n m) #-}
{-# RULES "Word64Map.toAscListBack" [1] foldrFB (\k x xs -> (k, x) : xs) [] = toAscList #-}
{-# RULES "Word64Map.toDescList" [~1] forall m . toDescList m = build (\c n -> foldlFB (\xs k x -> c (k,x) xs) n m) #-}
{-# RULES "Word64Map.toDescListBack" [1] foldlFB (\xs k x -> (k, x) : xs) [] = toDescList #-}


-- | \(O(n \min(n,W))\). Create a map from a list of key\/value pairs.
--
-- > fromList [] == empty
-- > fromList [(5,"a"), (3,"b"), (5, "c")] == fromList [(5,"c"), (3,"b")]
-- > fromList [(5,"c"), (3,"b"), (5, "a")] == fromList [(5,"a"), (3,"b")]

fromList :: [(Key,a)] -> Word64Map a
fromList xs
  = Foldable.foldl' ins empty xs
  where
    ins t (k,x)  = insert k x t

-- | \(O(n \min(n,W))\). Create a map from a list of key\/value pairs with a combining function. See also 'fromAscListWith'.
--
-- > fromListWith (++) [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"c")] == fromList [(3, "ab"), (5, "cba")]
-- > fromListWith (++) [] == empty

fromListWith :: (a -> a -> a) -> [(Key,a)] -> Word64Map a
fromListWith f xs
  = fromListWithKey (\_ x y -> f x y) xs

-- | \(O(n \min(n,W))\). Build a map from a list of key\/value pairs with a combining function. See also fromAscListWithKey'.
--
-- > let f key new_value old_value = show key ++ ":" ++ new_value ++ "|" ++ old_value
-- > fromListWithKey f [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"c")] == fromList [(3, "3:a|b"), (5, "5:c|5:b|a")]
-- > fromListWithKey f [] == empty

fromListWithKey :: (Key -> a -> a -> a) -> [(Key,a)] -> Word64Map a
fromListWithKey f xs
  = Foldable.foldl' ins empty xs
  where
    ins t (k,x) = insertWithKey f k x t

-- | \(O(n)\). Build a map from a list of key\/value pairs where
-- the keys are in ascending order.
--
-- > fromAscList [(3,"b"), (5,"a")]          == fromList [(3, "b"), (5, "a")]
-- > fromAscList [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "b")]

fromAscList :: [(Key,a)] -> Word64Map a
fromAscList = fromMonoListWithKey Nondistinct (\_ x _ -> x)
{-# NOINLINE fromAscList #-}

-- | \(O(n)\). Build a map from a list of key\/value pairs where
-- the keys are in ascending order, with a combining function on equal keys.
-- /The precondition (input list is ascending) is not checked./
--
-- > fromAscListWith (++) [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "ba")]

fromAscListWith :: (a -> a -> a) -> [(Key,a)] -> Word64Map a
fromAscListWith f = fromMonoListWithKey Nondistinct (\_ x y -> f x y)
{-# NOINLINE fromAscListWith #-}

-- | \(O(n)\). Build a map from a list of key\/value pairs where
-- the keys are in ascending order, with a combining function on equal keys.
-- /The precondition (input list is ascending) is not checked./
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > fromAscListWithKey f [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "5:b|a")]

fromAscListWithKey :: (Key -> a -> a -> a) -> [(Key,a)] -> Word64Map a
fromAscListWithKey f = fromMonoListWithKey Nondistinct f
{-# NOINLINE fromAscListWithKey #-}

-- | \(O(n)\). Build a map from a list of key\/value pairs where
-- the keys are in ascending order and all distinct.
-- /The precondition (input list is strictly ascending) is not checked./
--
-- > fromDistinctAscList [(3,"b"), (5,"a")] == fromList [(3, "b"), (5, "a")]

fromDistinctAscList :: [(Key,a)] -> Word64Map a
fromDistinctAscList = fromMonoListWithKey Distinct (\_ x _ -> x)
{-# NOINLINE fromDistinctAscList #-}

-- | \(O(n)\). Build a map from a list of key\/value pairs with monotonic keys
-- and a combining function.
--
-- The precise conditions under which this function works are subtle:
-- For any branch mask, keys with the same prefix w.r.t. the branch
-- mask must occur consecutively in the list.

fromMonoListWithKey :: Distinct -> (Key -> a -> a -> a) -> [(Key,a)] -> Word64Map a
fromMonoListWithKey distinct f = go
  where
    go []              = Nil
    go ((kx,vx) : zs1) = addAll' kx vx zs1

    -- `addAll'` collects all keys equal to `kx` into a single value,
    -- and then proceeds with `addAll`.
    addAll' !kx vx []
        = Tip kx vx
    addAll' !kx vx ((ky,vy) : zs)
        | Nondistinct <- distinct, kx == ky
        = let v = f kx vy vx in addAll' ky v zs
        -- inlined: | otherwise = addAll kx (Tip kx vx) (ky : zs)
        | m <- branchMask kx ky
        , Inserted ty zs' <- addMany' m ky vy zs
        = addAll kx (linkWithMask m ky ty {-kx-} (Tip kx vx)) zs'

    -- for `addAll` and `addMany`, kx is /a/ key inside the tree `tx`
    -- `addAll` consumes the rest of the list, adding to the tree `tx`
    addAll !_kx !tx []
        = tx
    addAll !kx !tx ((ky,vy) : zs)
        | m <- branchMask kx ky
        , Inserted ty zs' <- addMany' m ky vy zs
        = addAll kx (linkWithMask m ky ty {-kx-} tx) zs'

    -- `addMany'` is similar to `addAll'`, but proceeds with `addMany'`.
    addMany' !_m !kx vx []
        = Inserted (Tip kx vx) []
    addMany' !m !kx vx zs0@((ky,vy) : zs)
        | Nondistinct <- distinct, kx == ky
        = let v = f kx vy vx in addMany' m ky v zs
        -- inlined: | otherwise = addMany m kx (Tip kx vx) (ky : zs)
        | mask kx m /= mask ky m
        = Inserted (Tip kx vx) zs0
        | mxy <- branchMask kx ky
        , Inserted ty zs' <- addMany' mxy ky vy zs
        = addMany m kx (linkWithMask mxy ky ty {-kx-} (Tip kx vx)) zs'

    -- `addAll` adds to `tx` all keys whose prefix w.r.t. `m` agrees with `kx`.
    addMany !_m !_kx tx []
        = Inserted tx []
    addMany !m !kx tx zs0@((ky,vy) : zs)
        | mask kx m /= mask ky m
        = Inserted tx zs0
        | mxy <- branchMask kx ky
        , Inserted ty zs' <- addMany' mxy ky vy zs
        = addMany m kx (linkWithMask mxy ky ty {-kx-} tx) zs'
{-# INLINE fromMonoListWithKey #-}

data Inserted a = Inserted !(Word64Map a) ![(Key,a)]

data Distinct = Distinct | Nondistinct

{--------------------------------------------------------------------
  Eq
--------------------------------------------------------------------}
instance Eq a => Eq (Word64Map a) where
  t1 == t2  = equal t1 t2
  t1 /= t2  = nequal t1 t2

equal :: Eq a => Word64Map a -> Word64Map a -> Bool
equal (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  = (m1 == m2) && (p1 == p2) && (equal l1 l2) && (equal r1 r2)
equal (Tip kx x) (Tip ky y)
  = (kx == ky) && (x==y)
equal Nil Nil = True
equal _   _   = False

nequal :: Eq a => Word64Map a -> Word64Map a -> Bool
nequal (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  = (m1 /= m2) || (p1 /= p2) || (nequal l1 l2) || (nequal r1 r2)
nequal (Tip kx x) (Tip ky y)
  = (kx /= ky) || (x/=y)
nequal Nil Nil = False
nequal _   _   = True

-- | @since 0.5.9
instance Eq1 Word64Map where
  liftEq eq (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
    = (m1 == m2) && (p1 == p2) && (liftEq eq l1 l2) && (liftEq eq r1 r2)
  liftEq eq (Tip kx x) (Tip ky y)
    = (kx == ky) && (eq x y)
  liftEq _eq Nil Nil = True
  liftEq _eq _   _   = False

{--------------------------------------------------------------------
  Ord
--------------------------------------------------------------------}

instance Ord a => Ord (Word64Map a) where
    compare m1 m2 = compare (toList m1) (toList m2)

-- | @since 0.5.9
instance Ord1 Word64Map where
  liftCompare cmp m n =
    liftCompare (liftCompare cmp) (toList m) (toList n)

{--------------------------------------------------------------------
  Functor
--------------------------------------------------------------------}

instance Functor Word64Map where
    fmap = map

    a <$ Bin p m l r = Bin p m (a <$ l) (a <$ r)
    a <$ Tip k _     = Tip k a
    _ <$ Nil         = Nil

{--------------------------------------------------------------------
  Show
--------------------------------------------------------------------}

instance Show a => Show (Word64Map a) where
  showsPrec d m   = showParen (d > 10) $
    showString "fromList " . shows (toList m)

-- | @since 0.5.9
instance Show1 Word64Map where
    liftShowsPrec sp sl d m =
        showsUnaryWith (liftShowsPrec sp' sl') "fromList" d (toList m)
      where
        sp' = liftShowsPrec sp sl
        sl' = liftShowList sp sl

{--------------------------------------------------------------------
  Read
--------------------------------------------------------------------}
instance (Read e) => Read (Word64Map e) where
  readPrec = parens $ prec 10 $ do
    Ident "fromList" <- lexP
    xs <- readPrec
    return (fromList xs)

  readListPrec = readListPrecDefault

-- | @since 0.5.9
instance Read1 Word64Map where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp' rl') "fromList" fromList
      where
        rp' = liftReadsPrec rp rl
        rl' = liftReadList rp rl

{--------------------------------------------------------------------
  Helpers
--------------------------------------------------------------------}
{--------------------------------------------------------------------
  Link
--------------------------------------------------------------------}
link :: Prefix -> Word64Map a -> Prefix -> Word64Map a -> Word64Map a
link p1 t1 p2 t2 = linkWithMask (branchMask p1 p2) p1 t1 {-p2-} t2
{-# INLINE link #-}

-- `linkWithMask` is useful when the `branchMask` has already been computed
linkWithMask :: Mask -> Prefix -> Word64Map a -> Word64Map a -> Word64Map a
linkWithMask m p1 t1 {-p2-} t2
  | zero p1 m = Bin p m t1 t2
  | otherwise = Bin p m t2 t1
  where
    p = mask p1 m
{-# INLINE linkWithMask #-}

{--------------------------------------------------------------------
  @bin@ assures that we never have empty trees within a tree.
--------------------------------------------------------------------}
bin :: Prefix -> Mask -> Word64Map a -> Word64Map a -> Word64Map a
bin _ _ l Nil = l
bin _ _ Nil r = r
bin p m l r   = Bin p m l r
{-# INLINE bin #-}

-- binCheckLeft only checks that the left subtree is non-empty
binCheckLeft :: Prefix -> Mask -> Word64Map a -> Word64Map a -> Word64Map a
binCheckLeft _ _ Nil r = r
binCheckLeft p m l r   = Bin p m l r
{-# INLINE binCheckLeft #-}

-- binCheckRight only checks that the right subtree is non-empty
binCheckRight :: Prefix -> Mask -> Word64Map a -> Word64Map a -> Word64Map a
binCheckRight _ _ l Nil = l
binCheckRight p m l r   = Bin p m l r
{-# INLINE binCheckRight #-}

{--------------------------------------------------------------------
  Endian independent bit twiddling
--------------------------------------------------------------------}

-- | Should this key follow the left subtree of a 'Bin' with switching
-- bit @m@? N.B., the answer is only valid when @match i p m@ is true.
zero :: Key -> Mask -> Bool
zero i m
  = (natFromInt i) .&. (natFromInt m) == 0
{-# INLINE zero #-}

nomatch,match :: Key -> Prefix -> Mask -> Bool

-- | Does the key @i@ differ from the prefix @p@ before getting to
-- the switching bit @m@?
nomatch i p m
  = (mask i m) /= p
{-# INLINE nomatch #-}

-- | Does the key @i@ match the prefix @p@ (up to but not including
-- bit @m@)?
match i p m
  = (mask i m) == p
{-# INLINE match #-}


-- | The prefix of key @i@ up to (but not including) the switching
-- bit @m@.
mask :: Key -> Mask -> Prefix
mask i m
  = maskW (natFromInt i) (natFromInt m)
{-# INLINE mask #-}


{--------------------------------------------------------------------
  Big endian operations
--------------------------------------------------------------------}

-- | The prefix of key @i@ up to (but not including) the switching
-- bit @m@.
maskW :: Nat -> Nat -> Prefix
maskW i m
  = intFromNat (i .&. ((-m) `xor` m))
{-# INLINE maskW #-}

-- | Does the left switching bit specify a shorter prefix?
shorter :: Mask -> Mask -> Bool
shorter m1 m2
  = (natFromInt m1) > (natFromInt m2)
{-# INLINE shorter #-}

-- | The first switching bit where the two prefixes disagree.
branchMask :: Prefix -> Prefix -> Mask
branchMask p1 p2
  = intFromNat (highestBitMask (natFromInt p1 `xor` natFromInt p2))
{-# INLINE branchMask #-}

{--------------------------------------------------------------------
  Utilities
--------------------------------------------------------------------}

-- | \(O(1)\).  Decompose a map into pieces based on the structure
-- of the underlying tree. This function is useful for consuming a
-- map in parallel.
--
-- No guarantee is made as to the sizes of the pieces; an internal, but
-- deterministic process determines this.  However, it is guaranteed that the
-- pieces returned will be in ascending order (all elements in the first submap
-- less than all elements in the second, and so on).
--
-- Examples:
--
-- > splitRoot (fromList (zip [1..6::Int] ['a'..])) ==
-- >   [fromList [(1,'a'),(2,'b'),(3,'c')],fromList [(4,'d'),(5,'e'),(6,'f')]]
--
-- > splitRoot empty == []
--
--  Note that the current implementation does not return more than two submaps,
--  but you should not depend on this behaviour because it can change in the
--  future without notice.
splitRoot :: Word64Map a -> [Word64Map a]
splitRoot orig =
  case orig of
    Nil -> []
    x@(Tip _ _) -> [x]
    Bin _ m l r | m < 0 -> [r, l]
                | otherwise -> [l, r]
{-# INLINE splitRoot #-}


{--------------------------------------------------------------------
  Debugging
--------------------------------------------------------------------}

-- | \(O(n \min(n,W))\). Show the tree that implements the map. The tree is shown
-- in a compressed, hanging format.
showTree :: Show a => Word64Map a -> String
showTree s
  = showTreeWith True False s


{- | \(O(n \min(n,W))\). The expression (@'showTreeWith' hang wide map@) shows
 the tree that implements the map. If @hang@ is
 'True', a /hanging/ tree is shown otherwise a rotated tree is shown. If
 @wide@ is 'True', an extra wide version is shown.
-}
showTreeWith :: Show a => Bool -> Bool -> Word64Map a -> String
showTreeWith hang wide t
  | hang      = (showsTreeHang wide [] t) ""
  | otherwise = (showsTree wide [] [] t) ""

showsTree :: Show a => Bool -> [String] -> [String] -> Word64Map a -> ShowS
showsTree wide lbars rbars t = case t of
  Bin p m l r ->
    showsTree wide (withBar rbars) (withEmpty rbars) r .
    showWide wide rbars .
    showsBars lbars . showString (showBin p m) . showString "\n" .
    showWide wide lbars .
    showsTree wide (withEmpty lbars) (withBar lbars) l
  Tip k x ->
    showsBars lbars .
    showString " " . shows k . showString ":=" . shows x . showString "\n"
  Nil -> showsBars lbars . showString "|\n"

showsTreeHang :: Show a => Bool -> [String] -> Word64Map a -> ShowS
showsTreeHang wide bars t = case t of
  Bin p m l r ->
    showsBars bars . showString (showBin p m) . showString "\n" .
    showWide wide bars .
    showsTreeHang wide (withBar bars) l .
    showWide wide bars .
    showsTreeHang wide (withEmpty bars) r
  Tip k x ->
    showsBars bars .
    showString " " . shows k . showString ":=" . shows x . showString "\n"
  Nil -> showsBars bars . showString "|\n"

showBin :: Prefix -> Mask -> String
showBin _ _
  = "*" -- ++ show (p,m)

showWide :: Bool -> [String] -> String -> String
showWide wide bars
  | wide      = showString (concat (reverse bars)) . showString "|\n"
  | otherwise = id

showsBars :: [String] -> ShowS
showsBars bars
  = case bars of
      [] -> id
      _ : tl -> showString (concat (reverse tl)) . showString node

node :: String
node = "+--"

withBar, withEmpty :: [String] -> [String]
withBar bars   = "|  ":bars
withEmpty bars = "   ":bars

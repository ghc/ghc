{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
#if __GLASGOW_HASKELL__
{-# LANGUAGE MagicHash, DeriveDataTypeable, StandaloneDeriving #-}
#endif
#if !defined(TESTING) && __GLASGOW_HASKELL__ >= 703
{-# LANGUAGE Trustworthy #-}
#endif
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE TypeFamilies #-}
#endif

#include "containers.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IntSet.Internal
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Joachim Breitner 2011
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
-- An efficient implementation of integer sets.
--
-- These modules are intended to be imported qualified, to avoid name
-- clashes with Prelude functions, e.g.
--
-- >  import Data.IntSet (IntSet)
-- >  import qualified Data.IntSet as IntSet
--
-- The implementation is based on /big-endian patricia trees/.  This data
-- structure performs especially well on binary operations like 'union'
-- and 'intersection'.  However, my benchmarks show that it is also
-- (much) faster on insertions and deletions when compared to a generic
-- size-balanced set implementation (see "Data.Set").
--
--    * Chris Okasaki and Andy Gill,  \"/Fast Mergeable Integer Maps/\",
--      Workshop on ML, September 1998, pages 77-86,
--      <http://citeseer.ist.psu.edu/okasaki98fast.html>
--
--    * D.R. Morrison, \"/PATRICIA -- Practical Algorithm To Retrieve
--      Information Coded In Alphanumeric/\", Journal of the ACM, 15(4),
--      October 1968, pages 514-534.
--
-- Additionally, this implementation places bitmaps in the leaves of the tree.
-- Their size is the natural size of a machine word (32 or 64 bits) and greatly
-- reduce memory footprint and execution times for dense sets, e.g. sets where
-- it is likely that many values lie close to each other. The asymptotics are
-- not affected by this optimization.
--
-- Many operations have a worst-case complexity of /O(min(n,W))/.
-- This means that the operation can become linear in the number of
-- elements with a maximum of /W/ -- the number of bits in an 'Int'
-- (32 or 64).
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
-- The order of constructors of IntSet matters when considering performance.
-- Currently in GHC 7.0, when type has 3 constructors, they are matched from
-- the first to the last -- the best performance is achieved when the
-- constructors are ordered by frequency.
-- On GHC 7.0, reordering constructors from Nil | Tip | Bin to Bin | Tip | Nil
-- improves the benchmark by circa 10%.

module Data.IntSet.Internal (
    -- * Set type
      IntSet(..), Key -- instance Eq,Show

    -- * Operators
    , (\\)

    -- * Query
    , null
    , size
    , member
    , notMember
    , lookupLT
    , lookupGT
    , lookupLE
    , lookupGE
    , isSubsetOf
    , isProperSubsetOf

    -- * Construction
    , empty
    , singleton
    , insert
    , delete

    -- * Combine
    , union
    , unions
    , difference
    , intersection

    -- * Filter
    , filter
    , partition
    , split
    , splitMember
    , splitRoot

    -- * Map
    , map

    -- * Folds
    , foldr
    , foldl
    -- ** Strict folds
    , foldr'
    , foldl'
    -- ** Legacy folds
    , fold

    -- * Min\/Max
    , findMin
    , findMax
    , deleteMin
    , deleteMax
    , deleteFindMin
    , deleteFindMax
    , maxView
    , minView

    -- * Conversion

    -- ** List
    , elems
    , toList
    , fromList

    -- ** Ordered list
    , toAscList
    , toDescList
    , fromAscList
    , fromDistinctAscList

    -- * Debugging
    , showTree
    , showTreeWith

    -- * Internals
    , match
    , suffixBitMask
    , prefixBitMask
    , bitmapOf
    ) where

import Control.DeepSeq (NFData(rnf))
import Data.Bits
import qualified Data.List as List
import Data.Maybe (fromMaybe)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid(..))
import Data.Word (Word)
#endif
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup((<>), stimes), stimesIdempotentMonoid)
#endif
import Data.Typeable
import Prelude hiding (filter, foldr, foldl, null, map)

import Utils.Containers.Internal.BitUtil
import Utils.Containers.Internal.StrictFold
import Utils.Containers.Internal.StrictPair

#if __GLASGOW_HASKELL__
import Data.Data (Data(..), Constr, mkConstr, constrIndex, Fixity(Prefix), DataType, mkDataType)
import Text.Read
#endif

#if __GLASGOW_HASKELL__
import GHC.Exts (Int(..), build)
#if __GLASGOW_HASKELL__ >= 708
import qualified GHC.Exts as GHCExts
#endif
import GHC.Prim (indexInt8OffAddr#)
#endif


infixl 9 \\{-This comment teaches CPP correct behaviour -}

-- A "Nat" is a natural machine word (an unsigned Int)
type Nat = Word

natFromInt :: Int -> Nat
natFromInt i = fromIntegral i
{-# INLINE natFromInt #-}

intFromNat :: Nat -> Int
intFromNat w = fromIntegral w
{-# INLINE intFromNat #-}

{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}
-- | /O(n+m)/. See 'difference'.
(\\) :: IntSet -> IntSet -> IntSet
m1 \\ m2 = difference m1 m2

{--------------------------------------------------------------------
  Types
--------------------------------------------------------------------}

-- | A set of integers.

-- See Note: Order of constructors
data IntSet = Bin {-# UNPACK #-} !Prefix {-# UNPACK #-} !Mask !IntSet !IntSet
-- Invariant: Nil is never found as a child of Bin.
-- Invariant: The Mask is a power of 2.  It is the largest bit position at which
--            two elements of the set differ.
-- Invariant: Prefix is the common high-order bits that all elements share to
--            the left of the Mask bit.
-- Invariant: In Bin prefix mask left right, left consists of the elements that
--            don't have the mask bit set; right is all the elements that do.
            | Tip {-# UNPACK #-} !Prefix {-# UNPACK #-} !BitMap
-- Invariant: The Prefix is zero for all but the last 5 (on 32 bit arches) or 6
--            bits (on 64 bit arches). The values of the map represented by a tip
--            are the prefix plus the indices of the set bits in the bit map.
            | Nil

-- A number stored in a set is stored as
-- * Prefix (all but last 5-6 bits) and
-- * BitMap (last 5-6 bits stored as a bitmask)
--   Last 5-6 bits are called a Suffix.

type Prefix = Int
type Mask   = Int
type BitMap = Word
type Key    = Int

instance Monoid IntSet where
    mempty  = empty
    mconcat = unions
#if !(MIN_VERSION_base(4,9,0))
    mappend = union
#else
    mappend = (<>)

instance Semigroup IntSet where
    (<>)    = union
    stimes  = stimesIdempotentMonoid
#endif

#if __GLASGOW_HASKELL__

{--------------------------------------------------------------------
  A Data instance
--------------------------------------------------------------------}

-- This instance preserves data abstraction at the cost of inefficiency.
-- We provide limited reflection services for the sake of data abstraction.

instance Data IntSet where
  gfoldl f z is = z fromList `f` (toList is)
  toConstr _     = fromListConstr
  gunfold k z c  = case constrIndex c of
    1 -> k (z fromList)
    _ -> error "gunfold"
  dataTypeOf _   = intSetDataType

fromListConstr :: Constr
fromListConstr = mkConstr intSetDataType "fromList" [] Prefix

intSetDataType :: DataType
intSetDataType = mkDataType "Data.IntSet.Internal.IntSet" [fromListConstr]

#endif

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}
-- | /O(1)/. Is the set empty?
null :: IntSet -> Bool
null Nil = True
null _   = False
{-# INLINE null #-}

-- | /O(n)/. Cardinality of the set.
size :: IntSet -> Int
size = go 0
  where
    go !acc (Bin _ _ l r) = go (go acc l) r
    go acc (Tip _ bm) = acc + bitcount 0 bm
    go acc Nil = acc

-- | /O(min(n,W))/. Is the value a member of the set?

-- See Note: Local 'go' functions and capturing]
member :: Key -> IntSet -> Bool
member !x = go
  where
    go (Bin p m l r)
      | nomatch x p m = False
      | zero x m      = go l
      | otherwise     = go r
    go (Tip y bm) = prefixOf x == y && bitmapOf x .&. bm /= 0
    go Nil = False

-- | /O(min(n,W))/. Is the element not in the set?
notMember :: Key -> IntSet -> Bool
notMember k = not . member k

-- | /O(log n)/. Find largest element smaller than the given one.
--
-- > lookupLT 3 (fromList [3, 5]) == Nothing
-- > lookupLT 5 (fromList [3, 5]) == Just 3

-- See Note: Local 'go' functions and capturing.
lookupLT :: Key -> IntSet -> Maybe Key
lookupLT !x t = case t of
    Bin _ m l r | m < 0 -> if x >= 0 then go r l else go Nil r
    _ -> go Nil t
  where
    go def (Bin p m l r) | nomatch x p m = if x < p then unsafeFindMax def else unsafeFindMax r
                         | zero x m  = go def l
                         | otherwise = go l r
    go def (Tip kx bm) | prefixOf x > kx = Just $ kx + highestBitSet bm
                       | prefixOf x == kx && maskLT /= 0 = Just $ kx + highestBitSet maskLT
                       | otherwise = unsafeFindMax def
                       where maskLT = (bitmapOf x - 1) .&. bm
    go def Nil = unsafeFindMax def


-- | /O(log n)/. Find smallest element greater than the given one.
--
-- > lookupGT 4 (fromList [3, 5]) == Just 5
-- > lookupGT 5 (fromList [3, 5]) == Nothing

-- See Note: Local 'go' functions and capturing.
lookupGT :: Key -> IntSet -> Maybe Key
lookupGT !x t = case t of
    Bin _ m l r | m < 0 -> if x >= 0 then go Nil l else go l r
    _ -> go Nil t
  where
    go def (Bin p m l r) | nomatch x p m = if x < p then unsafeFindMin l else unsafeFindMin def
                         | zero x m  = go r l
                         | otherwise = go def r
    go def (Tip kx bm) | prefixOf x < kx = Just $ kx + lowestBitSet bm
                       | prefixOf x == kx && maskGT /= 0 = Just $ kx + lowestBitSet maskGT
                       | otherwise = unsafeFindMin def
                       where maskGT = (- ((bitmapOf x) `shiftLL` 1)) .&. bm
    go def Nil = unsafeFindMin def


-- | /O(log n)/. Find largest element smaller or equal to the given one.
--
-- > lookupLE 2 (fromList [3, 5]) == Nothing
-- > lookupLE 4 (fromList [3, 5]) == Just 3
-- > lookupLE 5 (fromList [3, 5]) == Just 5

-- See Note: Local 'go' functions and capturing.
lookupLE :: Key -> IntSet -> Maybe Key
lookupLE !x t = case t of
    Bin _ m l r | m < 0 -> if x >= 0 then go r l else go Nil r
    _ -> go Nil t
  where
    go def (Bin p m l r) | nomatch x p m = if x < p then unsafeFindMax def else unsafeFindMax r
                         | zero x m  = go def l
                         | otherwise = go l r
    go def (Tip kx bm) | prefixOf x > kx = Just $ kx + highestBitSet bm
                       | prefixOf x == kx && maskLE /= 0 = Just $ kx + highestBitSet maskLE
                       | otherwise = unsafeFindMax def
                       where maskLE = (((bitmapOf x) `shiftLL` 1) - 1) .&. bm
    go def Nil = unsafeFindMax def


-- | /O(log n)/. Find smallest element greater or equal to the given one.
--
-- > lookupGE 3 (fromList [3, 5]) == Just 3
-- > lookupGE 4 (fromList [3, 5]) == Just 5
-- > lookupGE 6 (fromList [3, 5]) == Nothing

-- See Note: Local 'go' functions and capturing.
lookupGE :: Key -> IntSet -> Maybe Key
lookupGE !x t = case t of
    Bin _ m l r | m < 0 -> if x >= 0 then go Nil l else go l r
    _ -> go Nil t
  where
    go def (Bin p m l r) | nomatch x p m = if x < p then unsafeFindMin l else unsafeFindMin def
                         | zero x m  = go r l
                         | otherwise = go def r
    go def (Tip kx bm) | prefixOf x < kx = Just $ kx + lowestBitSet bm
                       | prefixOf x == kx && maskGE /= 0 = Just $ kx + lowestBitSet maskGE
                       | otherwise = unsafeFindMin def
                       where maskGE = (- (bitmapOf x)) .&. bm
    go def Nil = unsafeFindMin def



-- Helper function for lookupGE and lookupGT. It assumes that if a Bin node is
-- given, it has m > 0.
unsafeFindMin :: IntSet -> Maybe Key
unsafeFindMin Nil = Nothing
unsafeFindMin (Tip kx bm) = Just $ kx + lowestBitSet bm
unsafeFindMin (Bin _ _ l _) = unsafeFindMin l

-- Helper function for lookupLE and lookupLT. It assumes that if a Bin node is
-- given, it has m > 0.
unsafeFindMax :: IntSet -> Maybe Key
unsafeFindMax Nil = Nothing
unsafeFindMax (Tip kx bm) = Just $ kx + highestBitSet bm
unsafeFindMax (Bin _ _ _ r) = unsafeFindMax r

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
-- | /O(1)/. The empty set.
empty :: IntSet
empty
  = Nil
{-# INLINE empty #-}

-- | /O(1)/. A set of one element.
singleton :: Key -> IntSet
singleton x
  = Tip (prefixOf x) (bitmapOf x)
{-# INLINE singleton #-}

{--------------------------------------------------------------------
  Insert
--------------------------------------------------------------------}
-- | /O(min(n,W))/. Add a value to the set. There is no left- or right bias for
-- IntSets.
insert :: Key -> IntSet -> IntSet
insert !x = insertBM (prefixOf x) (bitmapOf x)

-- Helper function for insert and union.
insertBM :: Prefix -> BitMap -> IntSet -> IntSet
insertBM !kx !bm t@(Bin p m l r)
  | nomatch kx p m = link kx (Tip kx bm) p t
  | zero kx m      = Bin p m (insertBM kx bm l) r
  | otherwise      = Bin p m l (insertBM kx bm r)
insertBM kx bm t@(Tip kx' bm')
  | kx' == kx = Tip kx' (bm .|. bm')
  | otherwise = link kx (Tip kx bm) kx' t
insertBM kx bm Nil = Tip kx bm

-- | /O(min(n,W))/. Delete a value in the set. Returns the
-- original set when the value was not present.
delete :: Key -> IntSet -> IntSet
delete !x = deleteBM (prefixOf x) (bitmapOf x)

-- Deletes all values mentioned in the BitMap from the set.
-- Helper function for delete and difference.
deleteBM :: Prefix -> BitMap -> IntSet -> IntSet
deleteBM !kx !bm t@(Bin p m l r)
  | nomatch kx p m = t
  | zero kx m      = bin p m (deleteBM kx bm l) r
  | otherwise      = bin p m l (deleteBM kx bm r)
deleteBM kx bm t@(Tip kx' bm')
  | kx' == kx = tip kx (bm' .&. complement bm)
  | otherwise = t
deleteBM _ _ Nil = Nil


{--------------------------------------------------------------------
  Union
--------------------------------------------------------------------}
-- | The union of a list of sets.
unions :: [IntSet] -> IntSet
unions xs
  = foldlStrict union empty xs


-- | /O(n+m)/. The union of two sets.
union :: IntSet -> IntSet -> IntSet
union t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2  = union1
  | shorter m2 m1  = union2
  | p1 == p2       = Bin p1 m1 (union l1 l2) (union r1 r2)
  | otherwise      = link p1 t1 p2 t2
  where
    union1  | nomatch p2 p1 m1  = link p1 t1 p2 t2
            | zero p2 m1        = Bin p1 m1 (union l1 t2) r1
            | otherwise         = Bin p1 m1 l1 (union r1 t2)

    union2  | nomatch p1 p2 m2  = link p1 t1 p2 t2
            | zero p1 m2        = Bin p2 m2 (union t1 l2) r2
            | otherwise         = Bin p2 m2 l2 (union t1 r2)

union t@(Bin _ _ _ _) (Tip kx bm) = insertBM kx bm t
union t@(Bin _ _ _ _) Nil = t
union (Tip kx bm) t = insertBM kx bm t
union Nil t = t


{--------------------------------------------------------------------
  Difference
--------------------------------------------------------------------}
-- | /O(n+m)/. Difference between two sets.
difference :: IntSet -> IntSet -> IntSet
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

difference t@(Bin _ _ _ _) (Tip kx bm) = deleteBM kx bm t
difference t@(Bin _ _ _ _) Nil = t

difference t1@(Tip kx bm) t2 = differenceTip t2
  where differenceTip (Bin p2 m2 l2 r2) | nomatch kx p2 m2 = t1
                                        | zero kx m2 = differenceTip l2
                                        | otherwise = differenceTip r2
        differenceTip (Tip kx2 bm2) | kx == kx2 = tip kx (bm .&. complement bm2)
                                    | otherwise = t1
        differenceTip Nil = t1

difference Nil _     = Nil



{--------------------------------------------------------------------
  Intersection
--------------------------------------------------------------------}
-- | /O(n+m)/. The intersection of two sets.
intersection :: IntSet -> IntSet -> IntSet
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

intersection t1@(Bin _ _ _ _) (Tip kx2 bm2) = intersectBM t1
  where intersectBM (Bin p1 m1 l1 r1) | nomatch kx2 p1 m1 = Nil
                                      | zero kx2 m1       = intersectBM l1
                                      | otherwise         = intersectBM r1
        intersectBM (Tip kx1 bm1) | kx1 == kx2 = tip kx1 (bm1 .&. bm2)
                                  | otherwise = Nil
        intersectBM Nil = Nil

intersection (Bin _ _ _ _) Nil = Nil

intersection (Tip kx1 bm1) t2 = intersectBM t2
  where intersectBM (Bin p2 m2 l2 r2) | nomatch kx1 p2 m2 = Nil
                                      | zero kx1 m2       = intersectBM l2
                                      | otherwise         = intersectBM r2
        intersectBM (Tip kx2 bm2) | kx1 == kx2 = tip kx1 (bm1 .&. bm2)
                                  | otherwise = Nil
        intersectBM Nil = Nil

intersection Nil _ = Nil

{--------------------------------------------------------------------
  Subset
--------------------------------------------------------------------}
-- | /O(n+m)/. Is this a proper subset? (ie. a subset but not equal).
isProperSubsetOf :: IntSet -> IntSet -> Bool
isProperSubsetOf t1 t2
  = case subsetCmp t1 t2 of
      LT -> True
      _  -> False

subsetCmp :: IntSet -> IntSet -> Ordering
subsetCmp t1@(Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  | shorter m1 m2  = GT
  | shorter m2 m1  = case subsetCmpLt of
                       GT -> GT
                       _  -> LT
  | p1 == p2       = subsetCmpEq
  | otherwise      = GT  -- disjoint
  where
    subsetCmpLt | nomatch p1 p2 m2  = GT
                | zero p1 m2        = subsetCmp t1 l2
                | otherwise         = subsetCmp t1 r2
    subsetCmpEq = case (subsetCmp l1 l2, subsetCmp r1 r2) of
                    (GT,_ ) -> GT
                    (_ ,GT) -> GT
                    (EQ,EQ) -> EQ
                    _       -> LT

subsetCmp (Bin _ _ _ _) _  = GT
subsetCmp (Tip kx1 bm1) (Tip kx2 bm2)
  | kx1 /= kx2                  = GT -- disjoint
  | bm1 == bm2                  = EQ
  | bm1 .&. complement bm2 == 0 = LT
  | otherwise                   = GT
subsetCmp t1@(Tip kx _) (Bin p m l r)
  | nomatch kx p m = GT
  | zero kx m      = case subsetCmp t1 l of GT -> GT ; _ -> LT
  | otherwise      = case subsetCmp t1 r of GT -> GT ; _ -> LT
subsetCmp (Tip _ _) Nil = GT -- disjoint
subsetCmp Nil Nil = EQ
subsetCmp Nil _   = LT

-- | /O(n+m)/. Is this a subset?
-- @(s1 `isSubsetOf` s2)@ tells whether @s1@ is a subset of @s2@.

isSubsetOf :: IntSet -> IntSet -> Bool
isSubsetOf t1@(Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  | shorter m1 m2  = False
  | shorter m2 m1  = match p1 p2 m2 && (if zero p1 m2 then isSubsetOf t1 l2
                                                      else isSubsetOf t1 r2)
  | otherwise      = (p1==p2) && isSubsetOf l1 l2 && isSubsetOf r1 r2
isSubsetOf (Bin _ _ _ _) _  = False
isSubsetOf (Tip kx1 bm1) (Tip kx2 bm2) = kx1 == kx2 && bm1 .&. complement bm2 == 0
isSubsetOf t1@(Tip kx _) (Bin p m l r)
  | nomatch kx p m = False
  | zero kx m      = isSubsetOf t1 l
  | otherwise      = isSubsetOf t1 r
isSubsetOf (Tip _ _) Nil = False
isSubsetOf Nil _         = True


{--------------------------------------------------------------------
  Filter
--------------------------------------------------------------------}
-- | /O(n)/. Filter all elements that satisfy some predicate.
filter :: (Key -> Bool) -> IntSet -> IntSet
filter predicate t
  = case t of
      Bin p m l r
        -> bin p m (filter predicate l) (filter predicate r)
      Tip kx bm
        -> tip kx (foldl'Bits 0 (bitPred kx) 0 bm)
      Nil -> Nil
  where bitPred kx bm bi | predicate (kx + bi) = bm .|. bitmapOfSuffix bi
                         | otherwise           = bm
        {-# INLINE bitPred #-}

-- | /O(n)/. partition the set according to some predicate.
partition :: (Key -> Bool) -> IntSet -> (IntSet,IntSet)
partition predicate0 t0 = toPair $ go predicate0 t0
  where
    go predicate t
      = case t of
          Bin p m l r
            -> let (l1 :*: l2) = go predicate l
                   (r1 :*: r2) = go predicate r
               in bin p m l1 r1 :*: bin p m l2 r2
          Tip kx bm
            -> let bm1 = foldl'Bits 0 (bitPred kx) 0 bm
               in  tip kx bm1 :*: tip kx (bm `xor` bm1)
          Nil -> (Nil :*: Nil)
      where bitPred kx bm bi | predicate (kx + bi) = bm .|. bitmapOfSuffix bi
                             | otherwise           = bm
            {-# INLINE bitPred #-}


-- | /O(min(n,W))/. The expression (@'split' x set@) is a pair @(set1,set2)@
-- where @set1@ comprises the elements of @set@ less than @x@ and @set2@
-- comprises the elements of @set@ greater than @x@.
--
-- > split 3 (fromList [1..5]) == (fromList [1,2], fromList [4,5])
split :: Key -> IntSet -> (IntSet,IntSet)
split x t =
  case t of
      Bin _ m l r
          | m < 0 -> if x >= 0  -- handle negative numbers.
                     then case go x l of (lt :*: gt) -> let !lt' = union lt r
                                                        in (lt', gt)
                     else case go x r of (lt :*: gt) -> let !gt' = union gt l
                                                        in (lt, gt')
      _ -> case go x t of
          (lt :*: gt) -> (lt, gt)
  where
    go !x' t'@(Bin p m l r)
        | match x' p m = if zero x' m
                         then case go x' l of
                             (lt :*: gt) -> lt :*: union gt r
                         else case go x' r of
                             (lt :*: gt) -> union lt l :*: gt
        | otherwise   = if x' < p then (Nil :*: t')
                        else (t' :*: Nil)
    go x' t'@(Tip kx' bm)
        | kx' > x'          = (Nil :*: t')
          -- equivalent to kx' > prefixOf x'
        | kx' < prefixOf x' = (t' :*: Nil)
        | otherwise = tip kx' (bm .&. lowerBitmap) :*: tip kx' (bm .&. higherBitmap)
            where lowerBitmap = bitmapOf x' - 1
                  higherBitmap = complement (lowerBitmap + bitmapOf x')
    go _ Nil = (Nil :*: Nil)

-- | /O(min(n,W))/. Performs a 'split' but also returns whether the pivot
-- element was found in the original set.
splitMember :: Key -> IntSet -> (IntSet,Bool,IntSet)
splitMember x t =
  case t of
      Bin _ m l r | m < 0 -> if x >= 0
                             then case go x l of
                                 (lt, fnd, gt) -> let !lt' = union lt r
                                                  in (lt', fnd, gt)
                             else case go x r of
                                 (lt, fnd, gt) -> let !gt' = union gt l
                                                  in (lt, fnd, gt')
      _ -> go x t
  where
    go x' t'@(Bin p m l r)
        | match x' p m = if zero x' m
                         then case go x' l of
                             (lt, fnd, gt) -> (lt, fnd, union gt r)
                         else case go x' r of
                             (lt, fnd, gt) -> (union lt l, fnd, gt)
        | otherwise   = if x' < p then (Nil, False, t') else (t', False, Nil)
    go x' t'@(Tip kx' bm)
        | kx' > x'          = (Nil, False, t')
          -- equivalent to kx' > prefixOf x'
        | kx' < prefixOf x' = (t', False, Nil)
        | otherwise = let !lt = tip kx' (bm .&. lowerBitmap)
                          !found = (bm .&. bitmapOfx') /= 0
                          !gt = tip kx' (bm .&. higherBitmap)
                      in (lt, found, gt)
            where bitmapOfx' = bitmapOf x'
                  lowerBitmap = bitmapOfx' - 1
                  higherBitmap = complement (lowerBitmap + bitmapOfx')
    go _ Nil = (Nil, False, Nil)

{----------------------------------------------------------------------
  Min/Max
----------------------------------------------------------------------}

-- | /O(min(n,W))/. Retrieves the maximal key of the set, and the set
-- stripped of that element, or 'Nothing' if passed an empty set.
maxView :: IntSet -> Maybe (Key, IntSet)
maxView t =
  case t of Nil -> Nothing
            Bin p m l r | m < 0 -> case go l of (result, l') -> Just (result, bin p m l' r)
            _ -> Just (go t)
  where
    go (Bin p m l r) = case go r of (result, r') -> (result, bin p m l r')
    go (Tip kx bm) = case highestBitSet bm of bi -> (kx + bi, tip kx (bm .&. complement (bitmapOfSuffix bi)))
    go Nil = error "maxView Nil"

-- | /O(min(n,W))/. Retrieves the minimal key of the set, and the set
-- stripped of that element, or 'Nothing' if passed an empty set.
minView :: IntSet -> Maybe (Key, IntSet)
minView t =
  case t of Nil -> Nothing
            Bin p m l r | m < 0 -> case go r of (result, r') -> Just (result, bin p m l r')
            _ -> Just (go t)
  where
    go (Bin p m l r) = case go l of (result, l') -> (result, bin p m l' r)
    go (Tip kx bm) = case lowestBitSet bm of bi -> (kx + bi, tip kx (bm .&. complement (bitmapOfSuffix bi)))
    go Nil = error "minView Nil"

-- | /O(min(n,W))/. Delete and find the minimal element.
--
-- > deleteFindMin set = (findMin set, deleteMin set)
deleteFindMin :: IntSet -> (Key, IntSet)
deleteFindMin = fromMaybe (error "deleteFindMin: empty set has no minimal element") . minView

-- | /O(min(n,W))/. Delete and find the maximal element.
--
-- > deleteFindMax set = (findMax set, deleteMax set)
deleteFindMax :: IntSet -> (Key, IntSet)
deleteFindMax = fromMaybe (error "deleteFindMax: empty set has no maximal element") . maxView


-- | /O(min(n,W))/. The minimal element of the set.
findMin :: IntSet -> Key
findMin Nil = error "findMin: empty set has no minimal element"
findMin (Tip kx bm) = kx + lowestBitSet bm
findMin (Bin _ m l r)
  |   m < 0   = find r
  | otherwise = find l
    where find (Tip kx bm) = kx + lowestBitSet bm
          find (Bin _ _ l' _) = find l'
          find Nil            = error "findMin Nil"

-- | /O(min(n,W))/. The maximal element of a set.
findMax :: IntSet -> Key
findMax Nil = error "findMax: empty set has no maximal element"
findMax (Tip kx bm) = kx + highestBitSet bm
findMax (Bin _ m l r)
  |   m < 0   = find l
  | otherwise = find r
    where find (Tip kx bm) = kx + highestBitSet bm
          find (Bin _ _ _ r') = find r'
          find Nil            = error "findMax Nil"


-- | /O(min(n,W))/. Delete the minimal element. Returns an empty set if the set is empty.
--
-- Note that this is a change of behaviour for consistency with 'Data.Set.Set' &#8211;
-- versions prior to 0.5 threw an error if the 'IntSet' was already empty.
deleteMin :: IntSet -> IntSet
deleteMin = maybe Nil snd . minView

-- | /O(min(n,W))/. Delete the maximal element. Returns an empty set if the set is empty.
--
-- Note that this is a change of behaviour for consistency with 'Data.Set.Set' &#8211;
-- versions prior to 0.5 threw an error if the 'IntSet' was already empty.
deleteMax :: IntSet -> IntSet
deleteMax = maybe Nil snd . maxView

{----------------------------------------------------------------------
  Map
----------------------------------------------------------------------}

-- | /O(n*min(n,W))/.
-- @'map' f s@ is the set obtained by applying @f@ to each element of @s@.
--
-- It's worth noting that the size of the result may be smaller if,
-- for some @(x,y)@, @x \/= y && f x == f y@

map :: (Key -> Key) -> IntSet -> IntSet
map f = fromList . List.map f . toList

{--------------------------------------------------------------------
  Fold
--------------------------------------------------------------------}
-- | /O(n)/. Fold the elements in the set using the given right-associative
-- binary operator. This function is an equivalent of 'foldr' and is present
-- for compatibility only.
--
-- /Please note that fold will be deprecated in the future and removed./
fold :: (Key -> b -> b) -> b -> IntSet -> b
fold = foldr
{-# INLINE fold #-}

-- | /O(n)/. Fold the elements in the set using the given right-associative
-- binary operator, such that @'foldr' f z == 'Prelude.foldr' f z . 'toAscList'@.
--
-- For example,
--
-- > toAscList set = foldr (:) [] set
foldr :: (Key -> b -> b) -> b -> IntSet -> b
foldr f z = \t ->      -- Use lambda t to be inlinable with two arguments only.
  case t of Bin _ m l r | m < 0 -> go (go z l) r -- put negative numbers before
                        | otherwise -> go (go z r) l
            _ -> go z t
  where
    go z' Nil           = z'
    go z' (Tip kx bm)   = foldrBits kx f z' bm
    go z' (Bin _ _ l r) = go (go z' r) l
{-# INLINE foldr #-}

-- | /O(n)/. A strict version of 'foldr'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldr' :: (Key -> b -> b) -> b -> IntSet -> b
foldr' f z = \t ->      -- Use lambda t to be inlinable with two arguments only.
  case t of Bin _ m l r | m < 0 -> go (go z l) r -- put negative numbers before
                        | otherwise -> go (go z r) l
            _ -> go z t
  where
    go !z' Nil           = z'
    go z' (Tip kx bm)   = foldr'Bits kx f z' bm
    go z' (Bin _ _ l r) = go (go z' r) l
{-# INLINE foldr' #-}

-- | /O(n)/. Fold the elements in the set using the given left-associative
-- binary operator, such that @'foldl' f z == 'Prelude.foldl' f z . 'toAscList'@.
--
-- For example,
--
-- > toDescList set = foldl (flip (:)) [] set
foldl :: (a -> Key -> a) -> a -> IntSet -> a
foldl f z = \t ->      -- Use lambda t to be inlinable with two arguments only.
  case t of Bin _ m l r | m < 0 -> go (go z r) l -- put negative numbers before
                        | otherwise -> go (go z l) r
            _ -> go z t
  where
    go z' Nil           = z'
    go z' (Tip kx bm)   = foldlBits kx f z' bm
    go z' (Bin _ _ l r) = go (go z' l) r
{-# INLINE foldl #-}

-- | /O(n)/. A strict version of 'foldl'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldl' :: (a -> Key -> a) -> a -> IntSet -> a
foldl' f z = \t ->      -- Use lambda t to be inlinable with two arguments only.
  case t of Bin _ m l r | m < 0 -> go (go z r) l -- put negative numbers before
                        | otherwise -> go (go z l) r
            _ -> go z t
  where
    go !z' Nil           = z'
    go z' (Tip kx bm)   = foldl'Bits kx f z' bm
    go z' (Bin _ _ l r) = go (go z' l) r
{-# INLINE foldl' #-}

{--------------------------------------------------------------------
  List variations
--------------------------------------------------------------------}
-- | /O(n)/. An alias of 'toAscList'. The elements of a set in ascending order.
-- Subject to list fusion.
elems :: IntSet -> [Key]
elems
  = toAscList

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}
#if __GLASGOW_HASKELL__ >= 708
instance GHCExts.IsList IntSet where
  type Item IntSet = Key
  fromList = fromList
  toList   = toList
#endif

-- | /O(n)/. Convert the set to a list of elements. Subject to list fusion.
toList :: IntSet -> [Key]
toList
  = toAscList

-- | /O(n)/. Convert the set to an ascending list of elements. Subject to list
-- fusion.
toAscList :: IntSet -> [Key]
toAscList = foldr (:) []

-- | /O(n)/. Convert the set to a descending list of elements. Subject to list
-- fusion.
toDescList :: IntSet -> [Key]
toDescList = foldl (flip (:)) []

-- List fusion for the list generating functions.
#if __GLASGOW_HASKELL__
-- The foldrFB and foldlFB are foldr and foldl equivalents, used for list fusion.
-- They are important to convert unfused to{Asc,Desc}List back, see mapFB in prelude.
foldrFB :: (Key -> b -> b) -> b -> IntSet -> b
foldrFB = foldr
{-# INLINE[0] foldrFB #-}
foldlFB :: (a -> Key -> a) -> a -> IntSet -> a
foldlFB = foldl
{-# INLINE[0] foldlFB #-}

-- Inline elems and toList, so that we need to fuse only toAscList.
{-# INLINE elems #-}
{-# INLINE toList #-}

-- The fusion is enabled up to phase 2 included. If it does not succeed,
-- convert in phase 1 the expanded to{Asc,Desc}List calls back to
-- to{Asc,Desc}List.  In phase 0, we inline fold{lr}FB (which were used in
-- a list fusion, otherwise it would go away in phase 1), and let compiler do
-- whatever it wants with to{Asc,Desc}List -- it was forbidden to inline it
-- before phase 0, otherwise the fusion rules would not fire at all.
{-# NOINLINE[0] toAscList #-}
{-# NOINLINE[0] toDescList #-}
{-# RULES "IntSet.toAscList" [~1] forall s . toAscList s = build (\c n -> foldrFB c n s) #-}
{-# RULES "IntSet.toAscListBack" [1] foldrFB (:) [] = toAscList #-}
{-# RULES "IntSet.toDescList" [~1] forall s . toDescList s = build (\c n -> foldlFB (\xs x -> c x xs) n s) #-}
{-# RULES "IntSet.toDescListBack" [1] foldlFB (\xs x -> x : xs) [] = toDescList #-}
#endif


-- | /O(n*min(n,W))/. Create a set from a list of integers.
fromList :: [Key] -> IntSet
fromList xs
  = foldlStrict ins empty xs
  where
    ins t x  = insert x t

-- | /O(n)/. Build a set from an ascending list of elements.
-- /The precondition (input list is ascending) is not checked./
fromAscList :: [Key] -> IntSet
fromAscList [] = Nil
fromAscList (x0 : xs0) = fromDistinctAscList (combineEq x0 xs0)
  where
    combineEq x' [] = [x']
    combineEq x' (x:xs)
      | x==x'     = combineEq x' xs
      | otherwise = x' : combineEq x xs

-- | /O(n)/. Build a set from an ascending list of distinct elements.
-- /The precondition (input list is strictly ascending) is not checked./
fromDistinctAscList :: [Key] -> IntSet
fromDistinctAscList []         = Nil
fromDistinctAscList (z0 : zs0) = work (prefixOf z0) (bitmapOf z0) zs0 Nada
  where
    -- 'work' accumulates all values that go into one tip, before passing this Tip
    -- to 'reduce'
    work kx bm []     stk = finish kx (Tip kx bm) stk
    work kx bm (z:zs) stk | kx == prefixOf z = work kx (bm .|. bitmapOf z) zs stk
    work kx bm (z:zs) stk = reduce z zs (branchMask z kx) kx (Tip kx bm) stk

    reduce z zs _ px tx Nada = work (prefixOf z) (bitmapOf z) zs (Push px tx Nada)
    reduce z zs m px tx stk@(Push py ty stk') =
        let mxy = branchMask px py
            pxy = mask px mxy
        in  if shorter m mxy
                 then reduce z zs m pxy (Bin pxy mxy ty tx) stk'
                 else work (prefixOf z) (bitmapOf z) zs (Push px tx stk)

    finish _  t  Nada = t
    finish px tx (Push py ty stk) = finish p (link py ty px tx) stk
        where m = branchMask px py
              p = mask px m

data Stack = Push {-# UNPACK #-} !Prefix !IntSet !Stack | Nada


{--------------------------------------------------------------------
  Eq
--------------------------------------------------------------------}
instance Eq IntSet where
  t1 == t2  = equal t1 t2
  t1 /= t2  = nequal t1 t2

equal :: IntSet -> IntSet -> Bool
equal (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  = (m1 == m2) && (p1 == p2) && (equal l1 l2) && (equal r1 r2)
equal (Tip kx1 bm1) (Tip kx2 bm2)
  = kx1 == kx2 && bm1 == bm2
equal Nil Nil = True
equal _   _   = False

nequal :: IntSet -> IntSet -> Bool
nequal (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  = (m1 /= m2) || (p1 /= p2) || (nequal l1 l2) || (nequal r1 r2)
nequal (Tip kx1 bm1) (Tip kx2 bm2)
  = kx1 /= kx2 || bm1 /= bm2
nequal Nil Nil = False
nequal _   _   = True

{--------------------------------------------------------------------
  Ord
--------------------------------------------------------------------}

instance Ord IntSet where
    compare s1 s2 = compare (toAscList s1) (toAscList s2)
    -- tentative implementation. See if more efficient exists.

{--------------------------------------------------------------------
  Show
--------------------------------------------------------------------}
instance Show IntSet where
  showsPrec p xs = showParen (p > 10) $
    showString "fromList " . shows (toList xs)

{--------------------------------------------------------------------
  Read
--------------------------------------------------------------------}
instance Read IntSet where
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

INSTANCE_TYPEABLE0(IntSet)

{--------------------------------------------------------------------
  NFData
--------------------------------------------------------------------}

-- The IntSet constructors consist only of strict fields of Ints and
-- IntSets, thus the default NFData instance which evaluates to whnf
-- should suffice
instance NFData IntSet where rnf x = seq x ()

{--------------------------------------------------------------------
  Debugging
--------------------------------------------------------------------}
-- | /O(n)/. Show the tree that implements the set. The tree is shown
-- in a compressed, hanging format.
showTree :: IntSet -> String
showTree s
  = showTreeWith True False s


{- | /O(n)/. The expression (@'showTreeWith' hang wide map@) shows
 the tree that implements the set. If @hang@ is
 'True', a /hanging/ tree is shown otherwise a rotated tree is shown. If
 @wide@ is 'True', an extra wide version is shown.
-}
showTreeWith :: Bool -> Bool -> IntSet -> String
showTreeWith hang wide t
  | hang      = (showsTreeHang wide [] t) ""
  | otherwise = (showsTree wide [] [] t) ""

showsTree :: Bool -> [String] -> [String] -> IntSet -> ShowS
showsTree wide lbars rbars t
  = case t of
      Bin p m l r
          -> showsTree wide (withBar rbars) (withEmpty rbars) r .
             showWide wide rbars .
             showsBars lbars . showString (showBin p m) . showString "\n" .
             showWide wide lbars .
             showsTree wide (withEmpty lbars) (withBar lbars) l
      Tip kx bm
          -> showsBars lbars . showString " " . shows kx . showString " + " .
                                                showsBitMap bm . showString "\n"
      Nil -> showsBars lbars . showString "|\n"

showsTreeHang :: Bool -> [String] -> IntSet -> ShowS
showsTreeHang wide bars t
  = case t of
      Bin p m l r
          -> showsBars bars . showString (showBin p m) . showString "\n" .
             showWide wide bars .
             showsTreeHang wide (withBar bars) l .
             showWide wide bars .
             showsTreeHang wide (withEmpty bars) r
      Tip kx bm
          -> showsBars bars . showString " " . shows kx . showString " + " .
                                               showsBitMap bm . showString "\n"
      Nil -> showsBars bars . showString "|\n"

showBin :: Prefix -> Mask -> String
showBin _ _
  = "*" -- ++ show (p,m)

showWide :: Bool -> [String] -> String -> String
showWide wide bars
  | wide      = showString (concat (reverse bars)) . showString "|\n"
  | otherwise = id

showsBars :: [String] -> ShowS
showsBars [] = id
showsBars bars = showString (concat (reverse (tail bars))) . showString node

showsBitMap :: Word -> ShowS
showsBitMap = showString . showBitMap

showBitMap :: Word -> String
showBitMap w = show $ foldrBits 0 (:) [] w

node :: String
node           = "+--"

withBar, withEmpty :: [String] -> [String]
withBar bars   = "|  ":bars
withEmpty bars = "   ":bars


{--------------------------------------------------------------------
  Helpers
--------------------------------------------------------------------}
{--------------------------------------------------------------------
  Link
--------------------------------------------------------------------}
link :: Prefix -> IntSet -> Prefix -> IntSet -> IntSet
link p1 t1 p2 t2
  | zero p1 m = Bin p m t1 t2
  | otherwise = Bin p m t2 t1
  where
    m = branchMask p1 p2
    p = mask p1 m
{-# INLINE link #-}

{--------------------------------------------------------------------
  @bin@ assures that we never have empty trees within a tree.
--------------------------------------------------------------------}
bin :: Prefix -> Mask -> IntSet -> IntSet -> IntSet
bin _ _ l Nil = l
bin _ _ Nil r = r
bin p m l r   = Bin p m l r
{-# INLINE bin #-}

{--------------------------------------------------------------------
  @tip@ assures that we never have empty bitmaps within a tree.
--------------------------------------------------------------------}
tip :: Prefix -> BitMap -> IntSet
tip _ 0 = Nil
tip kx bm = Tip kx bm
{-# INLINE tip #-}


{----------------------------------------------------------------------
  Functions that generate Prefix and BitMap of a Key or a Suffix.
----------------------------------------------------------------------}

suffixBitMask :: Int
#if MIN_VERSION_base(4,7,0)
suffixBitMask = finiteBitSize (undefined::Word) - 1
#else
suffixBitMask = bitSize (undefined::Word) - 1
#endif
{-# INLINE suffixBitMask #-}

prefixBitMask :: Int
prefixBitMask = complement suffixBitMask
{-# INLINE prefixBitMask #-}

prefixOf :: Int -> Prefix
prefixOf x = x .&. prefixBitMask
{-# INLINE prefixOf #-}

suffixOf :: Int -> Int
suffixOf x = x .&. suffixBitMask
{-# INLINE suffixOf #-}

bitmapOfSuffix :: Int -> BitMap
bitmapOfSuffix s = 1 `shiftLL` s
{-# INLINE bitmapOfSuffix #-}

bitmapOf :: Int -> BitMap
bitmapOf x = bitmapOfSuffix (suffixOf x)
{-# INLINE bitmapOf #-}


{--------------------------------------------------------------------
  Endian independent bit twiddling
--------------------------------------------------------------------}
zero :: Int -> Mask -> Bool
zero i m
  = (natFromInt i) .&. (natFromInt m) == 0
{-# INLINE zero #-}

nomatch,match :: Int -> Prefix -> Mask -> Bool
nomatch i p m
  = (mask i m) /= p
{-# INLINE nomatch #-}

match i p m
  = (mask i m) == p
{-# INLINE match #-}

-- Suppose a is largest such that 2^a divides 2*m.
-- Then mask i m is i with the low a bits zeroed out.
mask :: Int -> Mask -> Prefix
mask i m
  = maskW (natFromInt i) (natFromInt m)
{-# INLINE mask #-}

{--------------------------------------------------------------------
  Big endian operations
--------------------------------------------------------------------}
maskW :: Nat -> Nat -> Prefix
maskW i m
  = intFromNat (i .&. (complement (m-1) `xor` m))
{-# INLINE maskW #-}

shorter :: Mask -> Mask -> Bool
shorter m1 m2
  = (natFromInt m1) > (natFromInt m2)
{-# INLINE shorter #-}

branchMask :: Prefix -> Prefix -> Mask
branchMask p1 p2
  = intFromNat (highestBitMask (natFromInt p1 `xor` natFromInt p2))
{-# INLINE branchMask #-}

{----------------------------------------------------------------------
  To get best performance, we provide fast implementations of
  lowestBitSet, highestBitSet and fold[lr][l]Bits for GHC.
  If the intel bsf and bsr instructions ever become GHC primops,
  this code should be reimplemented using these.

  Performance of this code is crucial for folds, toList, filter, partition.

  The signatures of methods in question are placed after this comment.
----------------------------------------------------------------------}

lowestBitSet :: Nat -> Int
highestBitSet :: Nat -> Int
foldlBits :: Int -> (a -> Int -> a) -> a -> Nat -> a
foldl'Bits :: Int -> (a -> Int -> a) -> a -> Nat -> a
foldrBits :: Int -> (Int -> a -> a) -> a -> Nat -> a
foldr'Bits :: Int -> (Int -> a -> a) -> a -> Nat -> a

{-# INLINE lowestBitSet #-}
{-# INLINE highestBitSet #-}
{-# INLINE foldlBits #-}
{-# INLINE foldl'Bits #-}
{-# INLINE foldrBits #-}
{-# INLINE foldr'Bits #-}

#if defined(__GLASGOW_HASKELL__) && (WORD_SIZE_IN_BITS==32 || WORD_SIZE_IN_BITS==64)
{----------------------------------------------------------------------
  For lowestBitSet we use wordsize-dependant implementation based on
  multiplication and DeBrujn indeces, which was proposed by Edward Kmett
  <http://haskell.org/pipermail/libraries/2011-September/016749.html>

  The core of this implementation is fast indexOfTheOnlyBit,
  which is given a Nat with exactly one bit set, and returns
  its index.

  Lot of effort was put in these implementations, please benchmark carefully
  before changing this code.
----------------------------------------------------------------------}

indexOfTheOnlyBit :: Nat -> Int
{-# INLINE indexOfTheOnlyBit #-}
indexOfTheOnlyBit bitmask =
  I# (lsbArray `indexInt8OffAddr#` unboxInt (intFromNat ((bitmask * magic) `shiftRL` offset)))
  where unboxInt (I# i) = i
#if WORD_SIZE_IN_BITS==32
        magic = 0x077CB531
        offset = 27
        !lsbArray = "\0\1\28\2\29\14\24\3\30\22\20\15\25\17\4\8\31\27\13\23\21\19\16\7\26\12\18\6\11\5\10\9"#
#else
        magic = 0x07EDD5E59A4E28C2
        offset = 58
        !lsbArray = "\63\0\58\1\59\47\53\2\60\39\48\27\54\33\42\3\61\51\37\40\49\18\28\20\55\30\34\11\43\14\22\4\62\57\46\52\38\26\32\41\50\36\17\19\29\10\13\21\56\45\25\31\35\16\9\12\44\24\15\8\23\7\6\5"#
#endif
-- The lsbArray gets inlined to every call site of indexOfTheOnlyBit.
-- That cannot be easily avoided, as GHC forbids top-level Addr# literal.
-- One could go around that by supplying getLsbArray :: () -> Addr# marked
-- as NOINLINE. But the code size of calling it and processing the result
-- is 48B on 32-bit and 56B on 64-bit architectures -- so the 32B and 64B array
-- is actually improvement on 32-bit and only a 8B size increase on 64-bit.

lowestBitMask :: Nat -> Nat
lowestBitMask x = x .&. negate x
{-# INLINE lowestBitMask #-}

-- Reverse the order of bits in the Nat.
revNat :: Nat -> Nat
#if WORD_SIZE_IN_BITS==32
revNat x1 = case ((x1 `shiftRL` 1) .&. 0x55555555) .|. ((x1 .&. 0x55555555) `shiftLL` 1) of
              x2 -> case ((x2 `shiftRL` 2) .&. 0x33333333) .|. ((x2 .&. 0x33333333) `shiftLL` 2) of
                 x3 -> case ((x3 `shiftRL` 4) .&. 0x0F0F0F0F) .|. ((x3 .&. 0x0F0F0F0F) `shiftLL` 4) of
                   x4 -> case ((x4 `shiftRL` 8) .&. 0x00FF00FF) .|. ((x4 .&. 0x00FF00FF) `shiftLL` 8) of
                     x5 -> ( x5 `shiftRL` 16             ) .|. ( x5               `shiftLL` 16);
#else
revNat x1 = case ((x1 `shiftRL` 1) .&. 0x5555555555555555) .|. ((x1 .&. 0x5555555555555555) `shiftLL` 1) of
              x2 -> case ((x2 `shiftRL` 2) .&. 0x3333333333333333) .|. ((x2 .&. 0x3333333333333333) `shiftLL` 2) of
                 x3 -> case ((x3 `shiftRL` 4) .&. 0x0F0F0F0F0F0F0F0F) .|. ((x3 .&. 0x0F0F0F0F0F0F0F0F) `shiftLL` 4) of
                   x4 -> case ((x4 `shiftRL` 8) .&. 0x00FF00FF00FF00FF) .|. ((x4 .&. 0x00FF00FF00FF00FF) `shiftLL` 8) of
                     x5 -> case ((x5 `shiftRL` 16) .&. 0x0000FFFF0000FFFF) .|. ((x5 .&. 0x0000FFFF0000FFFF) `shiftLL` 16) of
                       x6 -> ( x6 `shiftRL` 32             ) .|. ( x6               `shiftLL` 32);
#endif

lowestBitSet x = indexOfTheOnlyBit (lowestBitMask x)

highestBitSet x = indexOfTheOnlyBit (highestBitMask x)

foldlBits prefix f z bitmap = go bitmap z
  where go 0 acc = acc
        go bm acc = go (bm `xor` bitmask) ((f acc) $! (prefix+bi))
          where
            !bitmask = lowestBitMask bm
            !bi = indexOfTheOnlyBit bitmask

foldl'Bits prefix f z bitmap = go bitmap z
  where go 0 acc = acc
        go bm !acc = go (bm `xor` bitmask) ((f acc) $! (prefix+bi))
          where !bitmask = lowestBitMask bm
                !bi = indexOfTheOnlyBit bitmask

foldrBits prefix f z bitmap = go (revNat bitmap) z
  where go 0 acc = acc
        go bm acc = go (bm `xor` bitmask) ((f $! (prefix+(WORD_SIZE_IN_BITS-1)-bi)) acc)
          where !bitmask = lowestBitMask bm
                !bi = indexOfTheOnlyBit bitmask


foldr'Bits prefix f z bitmap = go (revNat bitmap) z
  where go 0 acc = acc
        go bm !acc = go (bm `xor` bitmask) ((f $! (prefix+(WORD_SIZE_IN_BITS-1)-bi)) acc)
          where !bitmask = lowestBitMask bm
                !bi = indexOfTheOnlyBit bitmask

#else
{----------------------------------------------------------------------
  In general case we use logarithmic implementation of
  lowestBitSet and highestBitSet, which works up to bit sizes of 64.

  Folds are linear scans.
----------------------------------------------------------------------}

lowestBitSet n0 =
    let (n1,b1) = if n0 .&. 0xFFFFFFFF /= 0 then (n0,0)  else (n0 `shiftRL` 32, 32)
        (n2,b2) = if n1 .&. 0xFFFF /= 0     then (n1,b1) else (n1 `shiftRL` 16, 16+b1)
        (n3,b3) = if n2 .&. 0xFF /= 0       then (n2,b2) else (n2 `shiftRL` 8,  8+b2)
        (n4,b4) = if n3 .&. 0xF /= 0        then (n3,b3) else (n3 `shiftRL` 4,  4+b3)
        (n5,b5) = if n4 .&. 0x3 /= 0        then (n4,b4) else (n4 `shiftRL` 2,  2+b4)
        b6      = if n5 .&. 0x1 /= 0        then     b5  else                   1+b5
    in b6

highestBitSet n0 =
    let (n1,b1) = if n0 .&. 0xFFFFFFFF00000000 /= 0 then (n0 `shiftRL` 32, 32)    else (n0,0)
        (n2,b2) = if n1 .&. 0xFFFF0000 /= 0         then (n1 `shiftRL` 16, 16+b1) else (n1,b1)
        (n3,b3) = if n2 .&. 0xFF00 /= 0             then (n2 `shiftRL` 8,  8+b2)  else (n2,b2)
        (n4,b4) = if n3 .&. 0xF0 /= 0               then (n3 `shiftRL` 4,  4+b3)  else (n3,b3)
        (n5,b5) = if n4 .&. 0xC /= 0                then (n4 `shiftRL` 2,  2+b4)  else (n4,b4)
        b6      = if n5 .&. 0x2 /= 0                then                   1+b5   else     b5
    in b6

foldlBits prefix f z bm = let lb = lowestBitSet bm
                          in  go (prefix+lb) z (bm `shiftRL` lb)
  where go !_ acc 0 = acc
        go bi acc n | n `testBit` 0 = go (bi + 1) (f acc bi) (n `shiftRL` 1)
                    | otherwise     = go (bi + 1)    acc     (n `shiftRL` 1)

foldl'Bits prefix f z bm = let lb = lowestBitSet bm
                           in  go (prefix+lb) z (bm `shiftRL` lb)
  where go !_ !acc 0 = acc
        go bi acc n | n `testBit` 0 = go (bi + 1) (f acc bi) (n `shiftRL` 1)
                    | otherwise     = go (bi + 1)    acc     (n `shiftRL` 1)

foldrBits prefix f z bm = let lb = lowestBitSet bm
                          in  go (prefix+lb) (bm `shiftRL` lb)
  where go !_ 0 = z
        go bi n | n `testBit` 0 = f bi (go (bi + 1) (n `shiftRL` 1))
                | otherwise     =       go (bi + 1) (n `shiftRL` 1)

foldr'Bits prefix f z bm = let lb = lowestBitSet bm
                           in  go (prefix+lb) (bm `shiftRL` lb)
  where
        go !_ 0 = z
        go bi n | n `testBit` 0 = f bi $! go (bi + 1) (n `shiftRL` 1)
                | otherwise     =         go (bi + 1) (n `shiftRL` 1)

#endif

{----------------------------------------------------------------------
  [bitcount] as posted by David F. Place to haskell-cafe on April 11, 2006,
  based on the code on
  http://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetKernighan,
  where the following source is given:
    Published in 1988, the C Programming Language 2nd Ed. (by Brian W.
    Kernighan and Dennis M. Ritchie) mentions this in exercise 2-9. On April
    19, 2006 Don Knuth pointed out to me that this method "was first published
    by Peter Wegner in CACM 3 (1960), 322. (Also discovered independently by
    Derrick Lehmer and published in 1964 in a book edited by Beckenbach.)"
----------------------------------------------------------------------}

bitcount :: Int -> Word -> Int
#if MIN_VERSION_base(4,5,0)
bitcount a x = a + popCount x
#else
bitcount a0 x0 = go a0 x0
  where go a 0 = a
        go a x = go (a + 1) (x .&. (x-1))
#endif
{-# INLINE bitcount #-}


{--------------------------------------------------------------------
  Utilities
--------------------------------------------------------------------}

-- | /O(1)/.  Decompose a set into pieces based on the structure of the underlying
-- tree.  This function is useful for consuming a set in parallel.
--
-- No guarantee is made as to the sizes of the pieces; an internal, but
-- deterministic process determines this.  However, it is guaranteed that the
-- pieces returned will be in ascending order (all elements in the first submap
-- less than all elements in the second, and so on).
--
-- Examples:
--
-- > splitRoot (fromList [1..120]) == [fromList [1..63],fromList [64..120]]
-- > splitRoot empty == []
--
--  Note that the current implementation does not return more than two subsets,
--  but you should not depend on this behaviour because it can change in the
--  future without notice. Also, the current version does not continue
--  splitting all the way to individual singleton sets -- it stops at some
--  point.
splitRoot :: IntSet -> [IntSet]
splitRoot Nil = []
-- NOTE: we don't currently split below Tip, but we could.
splitRoot x@(Tip _ _) = [x]
splitRoot (Bin _ m l r) | m < 0 = [r, l]
                        | otherwise = [l, r]
{-# INLINE splitRoot #-}

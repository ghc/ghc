{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Word64Set.Internal
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
-- The contents of this module may change __in any way whatsoever__
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
-- >  import Data.Word64Set (Word64Set)
-- >  import qualified Data.Word64Set as Word64Set
--
-- The implementation is based on /big-endian patricia trees/.  This data
-- structure performs especially well on binary operations like 'union'
-- and 'intersection'.  However, my benchmarks show that it is also
-- (much) faster on insertions and deletions when compared to a generic
-- size-balanced set implementation (see "Data.Set").
--
--    * Chris Okasaki and Andy Gill,  \"/Fast Mergeable Integer Maps/\",
--      Workshop on ML, September 1998, pages 77-86,
--      <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.37.5452>
--
--    * D.R. Morrison, \"/PATRICIA -- Practical Algorithm To Retrieve Information Coded In Alphanumeric/\",
--      Journal of the ACM, 15(4), October 1968, pages 514-534.
--
-- Additionally, this implementation places bitmaps in the leaves of the tree.
-- Their size is the natural size of a machine word (32 or 64 bits) and greatly
-- reduce memory footprint and execution times for dense sets, e.g. sets where
-- it is likely that many values lie close to each other. The asymptotics are
-- not affected by this optimization.
--
-- Many operations have a worst-case complexity of \(O(\min(n,W))\).
-- This means that the operation can become linear in the number of
-- elements with a maximum of \(W\) -- the number of bits in an 'Int'
-- (32 or 64).
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
-- The order of constructors of Word64Set matters when considering performance.
-- Currently in GHC 7.0, when type has 3 constructors, they are matched from
-- the first to the last -- the best performance is achieved when the
-- constructors are ordered by frequency.
-- On GHC 7.0, reordering constructors from Nil | Tip | Bin to Bin | Tip | Nil
-- improves the benchmark by circa 10%.

module GHC.Data.Word64Set.Internal (
    -- * Set type
      Word64Set(..), Key -- instance Eq,Show
    , Prefix, Mask, BitMap

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
    , disjoint

    -- * Construction
    , empty
    , singleton
    , insert
    , delete
    , alterF

    -- * Combine
    , union
    , unions
    , difference
    , intersection

    -- * Filter
    , filter
    , partition

    , takeWhileAntitone
    , dropWhileAntitone
    , spanAntitone

    , split
    , splitMember
    , splitRoot

    -- * Map
    , map
    , mapMonotonic

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
    , zero
    ) where

import Control.Applicative (Const(..))
import Control.DeepSeq (NFData(rnf))
import Data.Bits
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup(stimes, (<>)), stimesIdempotentMonoid)
import GHC.Prelude.Basic hiding
  (filter, foldr, foldl, foldl', null, map)
import Data.Word ( Word64 )

import GHC.Utils.Containers.Internal.BitUtil
import GHC.Utils.Containers.Internal.StrictPair

import Data.Data (Data(..), Constr, mkConstr, constrIndex, DataType, mkDataType)
import qualified Data.Data
import Text.Read

import qualified GHC.Exts

import Data.Functor.Identity (Identity(..))

infixl 9 \\{-This comment teaches CPP correct behaviour -}

-- A "Nat" is a 64 bit machine word
type Nat = Word64

natFromInt :: Word64 -> Nat
natFromInt = id
{-# INLINE natFromInt #-}

intFromNat :: Nat -> Word64
intFromNat = id
{-# INLINE intFromNat #-}

{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}
-- | \(O(n+m)\). See 'difference'.
(\\) :: Word64Set -> Word64Set -> Word64Set
m1 \\ m2 = difference m1 m2

{--------------------------------------------------------------------
  Types
--------------------------------------------------------------------}

-- | A set of integers.

-- See Note: Order of constructors
data Word64Set = Bin {-# UNPACK #-} !Prefix {-# UNPACK #-} !Mask !Word64Set !Word64Set
-- Invariant: Nil is never found as a child of Bin.
-- Invariant: The Mask is a power of 2.  It is the largest bit position at which
--            two elements of the set differ.
-- Invariant: Prefix is the common high-order bits that all elements share to
--            the left of the Mask bit.
-- Invariant: In Bin prefix mask left right, left consists of the elements that
--            don't have the mask bit set; right is all the elements that do.
            | Tip {-# UNPACK #-} !Prefix {-# UNPACK #-} !BitMap
-- Invariant: The Prefix is zero for the last 6 bits. The values of the set
--            represented by a tip are the prefix plus the indices of the set
--            bits in the bit map.
            | Nil

-- A number stored in a set is stored as
-- * Prefix (all but last 6 bits) and
-- * BitMap (last 6 bits stored as a bitmask)
--   Last 6 bits are called a Suffix.

type Prefix = Word64
type Mask   = Word64
type BitMap = Word64
type Key    = Word64

instance Monoid Word64Set where
    mempty  = empty
    mconcat = unions
    mappend = (<>)

-- | @since 0.5.7
instance Semigroup Word64Set where
    (<>)    = union
    stimes  = stimesIdempotentMonoid


{--------------------------------------------------------------------
  A Data instance
--------------------------------------------------------------------}

-- This instance preserves data abstraction at the cost of inefficiency.
-- We provide limited reflection services for the sake of data abstraction.

instance Data Word64Set where
  gfoldl f z is = z fromList `f` (toList is)
  toConstr _     = fromListConstr
  gunfold k z c  = case constrIndex c of
    1 -> k (z fromList)
    _ -> error "gunfold"
  dataTypeOf _   = intSetDataType

fromListConstr :: Constr
fromListConstr = mkConstr intSetDataType "fromList" [] Data.Data.Prefix

intSetDataType :: DataType
intSetDataType = mkDataType "Data.Word64Set.Internal.Word64Set" [fromListConstr]


{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}
-- | \(O(1)\). Is the set empty?
null :: Word64Set -> Bool
null Nil = True
null _   = False
{-# INLINE null #-}

-- | \(O(n)\). Cardinality of the set.
size :: Word64Set -> Int
size = go 0
  where
    go !acc (Bin _ _ l r) = go (go acc l) r
    go acc (Tip _ bm) = acc + bitcount 0 bm
    go acc Nil = acc

-- | \(O(\min(n,W))\). Is the value a member of the set?

-- See Note: Local 'go' functions and capturing.
member :: Key -> Word64Set -> Bool
member !x = go
  where
    go (Bin p m l r)
      | nomatch x p m = False
      | zero x m      = go l
      | otherwise     = go r
    go (Tip y bm) = prefixOf x == y && bitmapOf x .&. bm /= 0
    go Nil = False

-- | \(O(\min(n,W))\). Is the element not in the set?
notMember :: Key -> Word64Set -> Bool
notMember k = not . member k

-- | \(O(\min(n,W))\). Find largest element smaller than the given one.
--
-- > lookupLT 3 (fromList [3, 5]) == Nothing
-- > lookupLT 5 (fromList [3, 5]) == Just 3

-- See Note: Local 'go' functions and capturing.
lookupLT :: Key -> Word64Set -> Maybe Key
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


-- | \(O(\min(n,W))\). Find smallest element greater than the given one.
--
-- > lookupGT 4 (fromList [3, 5]) == Just 5
-- > lookupGT 5 (fromList [3, 5]) == Nothing

-- See Note: Local 'go' functions and capturing.
lookupGT :: Key -> Word64Set -> Maybe Key
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


-- | \(O(\min(n,W))\). Find largest element smaller or equal to the given one.
--
-- > lookupLE 2 (fromList [3, 5]) == Nothing
-- > lookupLE 4 (fromList [3, 5]) == Just 3
-- > lookupLE 5 (fromList [3, 5]) == Just 5

-- See Note: Local 'go' functions and capturing.
lookupLE :: Key -> Word64Set -> Maybe Key
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


-- | \(O(\min(n,W))\). Find smallest element greater or equal to the given one.
--
-- > lookupGE 3 (fromList [3, 5]) == Just 3
-- > lookupGE 4 (fromList [3, 5]) == Just 5
-- > lookupGE 6 (fromList [3, 5]) == Nothing

-- See Note: Local 'go' functions and capturing.
lookupGE :: Key -> Word64Set -> Maybe Key
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
unsafeFindMin :: Word64Set -> Maybe Key
unsafeFindMin Nil = Nothing
unsafeFindMin (Tip kx bm) = Just $ kx + lowestBitSet bm
unsafeFindMin (Bin _ _ l _) = unsafeFindMin l

-- Helper function for lookupLE and lookupLT. It assumes that if a Bin node is
-- given, it has m > 0.
unsafeFindMax :: Word64Set -> Maybe Key
unsafeFindMax Nil = Nothing
unsafeFindMax (Tip kx bm) = Just $ kx + highestBitSet bm
unsafeFindMax (Bin _ _ _ r) = unsafeFindMax r

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
-- | \(O(1)\). The empty set.
empty :: Word64Set
empty
  = Nil
{-# INLINE empty #-}

-- | \(O(1)\). A set of one element.
singleton :: Key -> Word64Set
singleton x
  = Tip (prefixOf x) (bitmapOf x)
{-# INLINE singleton #-}

{--------------------------------------------------------------------
  Insert
--------------------------------------------------------------------}
-- | \(O(\min(n,W))\). Add a value to the set. There is no left- or right bias for
-- Word64Sets.
insert :: Key -> Word64Set -> Word64Set
insert !x = insertBM (prefixOf x) (bitmapOf x)

-- Helper function for insert and union.
insertBM :: Prefix -> BitMap -> Word64Set -> Word64Set
insertBM !kx !bm t@(Bin p m l r)
  | nomatch kx p m = link kx (Tip kx bm) p t
  | zero kx m      = Bin p m (insertBM kx bm l) r
  | otherwise      = Bin p m l (insertBM kx bm r)
insertBM kx bm t@(Tip kx' bm')
  | kx' == kx = Tip kx' (bm .|. bm')
  | otherwise = link kx (Tip kx bm) kx' t
insertBM kx bm Nil = Tip kx bm

-- | \(O(\min(n,W))\). Delete a value in the set. Returns the
-- original set when the value was not present.
delete :: Key -> Word64Set -> Word64Set
delete !x = deleteBM (prefixOf x) (bitmapOf x)

-- Deletes all values mentioned in the BitMap from the set.
-- Helper function for delete and difference.
deleteBM :: Prefix -> BitMap -> Word64Set -> Word64Set
deleteBM !kx !bm t@(Bin p m l r)
  | nomatch kx p m = t
  | zero kx m      = bin p m (deleteBM kx bm l) r
  | otherwise      = bin p m l (deleteBM kx bm r)
deleteBM kx bm t@(Tip kx' bm')
  | kx' == kx = tip kx (bm' .&. complement bm)
  | otherwise = t
deleteBM _ _ Nil = Nil

-- | \(O(\min(n,W))\). @('alterF' f x s)@ can delete or insert @x@ in @s@ depending
-- on whether it is already present in @s@.
--
-- In short:
--
-- @
-- 'member' x \<$\> 'alterF' f x s = f ('member' x s)
-- @
--
-- Note: 'alterF' is a variant of the @at@ combinator from "Control.Lens.At".
--
-- @since 0.6.3.1
alterF :: Functor f => (Bool -> f Bool) -> Key -> Word64Set -> f Word64Set
alterF f k s = fmap choose (f member_)
  where
    member_ = member k s

    (inserted, deleted)
      | member_   = (s         , delete k s)
      | otherwise = (insert k s, s         )

    choose True  = inserted
    choose False = deleted
{-# INLINABLE [2] alterF #-}

{-# RULES
"alterF/Const" forall k (f :: Bool -> Const a Bool) . alterF f k = \s -> Const . getConst . f $ member k s
 #-}

{-# SPECIALIZE alterF :: (Bool -> Identity Bool) -> Key -> Word64Set -> Identity Word64Set #-}

{--------------------------------------------------------------------
  Union
--------------------------------------------------------------------}
-- | The union of a list of sets.

{-# INLINABLE unions #-}
unions :: [Word64Set] -> Word64Set
unions = List.foldl' union empty

-- | \(O(n+m)\). The union of two sets.
union :: Word64Set -> Word64Set -> Word64Set
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
-- | \(O(n+m)\). Difference between two sets.
difference :: Word64Set -> Word64Set -> Word64Set
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
-- | \(O(n+m)\). The intersection of two sets.
intersection :: Word64Set -> Word64Set -> Word64Set
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
-- | \(O(n+m)\). Is this a proper subset? (ie. a subset but not equal).
isProperSubsetOf :: Word64Set -> Word64Set -> Bool
isProperSubsetOf t1 t2
  = case subsetCmp t1 t2 of
      LT -> True
      _  -> False

subsetCmp :: Word64Set -> Word64Set -> Ordering
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

-- | \(O(n+m)\). Is this a subset?
-- @(s1 \`isSubsetOf\` s2)@ tells whether @s1@ is a subset of @s2@.

isSubsetOf :: Word64Set -> Word64Set -> Bool
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
  Disjoint
--------------------------------------------------------------------}
-- | \(O(n+m)\). Check whether two sets are disjoint (i.e. their intersection
--   is empty).
--
-- > disjoint (fromList [2,4,6])   (fromList [1,3])     == True
-- > disjoint (fromList [2,4,6,8]) (fromList [2,3,5,7]) == False
-- > disjoint (fromList [1,2])     (fromList [1,2,3,4]) == False
-- > disjoint (fromList [])        (fromList [])        == True
--
-- @since 0.5.11
disjoint :: Word64Set -> Word64Set -> Bool
disjoint t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2  = disjoint1
  | shorter m2 m1  = disjoint2
  | p1 == p2       = disjoint l1 l2 && disjoint r1 r2
  | otherwise      = True
  where
    disjoint1 | nomatch p2 p1 m1  = True
              | zero p2 m1        = disjoint l1 t2
              | otherwise         = disjoint r1 t2

    disjoint2 | nomatch p1 p2 m2  = True
              | zero p1 m2        = disjoint t1 l2
              | otherwise         = disjoint t1 r2

disjoint t1@(Bin _ _ _ _) (Tip kx2 bm2) = disjointBM t1
  where disjointBM (Bin p1 m1 l1 r1) | nomatch kx2 p1 m1 = True
                                     | zero kx2 m1       = disjointBM l1
                                     | otherwise         = disjointBM r1
        disjointBM (Tip kx1 bm1) | kx1 == kx2 = (bm1 .&. bm2) == 0
                                 | otherwise = True
        disjointBM Nil = True

disjoint (Bin _ _ _ _) Nil = True

disjoint (Tip kx1 bm1) t2 = disjointBM t2
  where disjointBM (Bin p2 m2 l2 r2) | nomatch kx1 p2 m2 = True
                                     | zero kx1 m2       = disjointBM l2
                                     | otherwise         = disjointBM r2
        disjointBM (Tip kx2 bm2) | kx1 == kx2 = (bm1 .&. bm2) == 0
                                 | otherwise = True
        disjointBM Nil = True

disjoint Nil _ = True


{--------------------------------------------------------------------
  Filter
--------------------------------------------------------------------}
-- | \(O(n)\). Filter all elements that satisfy some predicate.
filter :: (Key -> Bool) -> Word64Set -> Word64Set
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

-- | \(O(n)\). partition the set according to some predicate.
partition :: (Key -> Bool) -> Word64Set -> (Word64Set,Word64Set)
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

-- | \(O(\min(n,W))\). Take while a predicate on the elements holds.
-- The user is responsible for ensuring that for all @Int@s, @j \< k ==\> p j \>= p k@.
-- See note at 'spanAntitone'.
--
-- @
-- takeWhileAntitone p = 'fromDistinctAscList' . 'Data.List.takeWhile' p . 'toList'
-- takeWhileAntitone p = 'filter' p
-- @
--
-- @since 0.6.7
takeWhileAntitone :: (Key -> Bool) -> Word64Set -> Word64Set
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
    go predicate' (Tip kx bm) = tip kx (takeWhileAntitoneBits kx predicate' bm)
    go _ Nil = Nil

-- | \(O(\min(n,W))\). Drop while a predicate on the elements holds.
-- The user is responsible for ensuring that for all @Int@s, @j \< k ==\> p j \>= p k@.
-- See note at 'spanAntitone'.
--
-- @
-- dropWhileAntitone p = 'fromDistinctAscList' . 'Data.List.dropWhile' p . 'toList'
-- dropWhileAntitone p = 'filter' (not . p)
-- @
--
-- @since 0.6.7
dropWhileAntitone :: (Key -> Bool) -> Word64Set -> Word64Set
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
    go predicate' (Tip kx bm) = tip kx (bm `xor` takeWhileAntitoneBits kx predicate' bm)
    go _ Nil = Nil

-- | \(O(\min(n,W))\). Divide a set at the point where a predicate on the elements stops holding.
-- The user is responsible for ensuring that for all @Int@s, @j \< k ==\> p j \>= p k@.
--
-- @
-- spanAntitone p xs = ('takeWhileAntitone' p xs, 'dropWhileAntitone' p xs)
-- spanAntitone p xs = 'partition' p xs
-- @
--
-- Note: if @p@ is not actually antitone, then @spanAntitone@ will split the set
-- at some /unspecified/ point.
--
-- @since 0.6.7
spanAntitone :: (Key -> Bool) -> Word64Set -> (Word64Set, Word64Set)
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
    go predicate' (Tip kx bm) = let bm' = takeWhileAntitoneBits kx predicate' bm
                                in (tip kx bm' :*: tip kx (bm `xor` bm'))
    go _ Nil = (Nil :*: Nil)

-- | \(O(\min(n,W))\). The expression (@'split' x set@) is a pair @(set1,set2)@
-- where @set1@ comprises the elements of @set@ less than @x@ and @set2@
-- comprises the elements of @set@ greater than @x@.
--
-- > split 3 (fromList [1..5]) == (fromList [1,2], fromList [4,5])
split :: Key -> Word64Set -> (Word64Set,Word64Set)
split x t =
  case t of
    Bin p m l r
      | m < 0 ->
        if x >= 0  -- handle negative numbers.
        then
          case go x l of
            (lt :*: gt) ->
              let !lt' = bin p m lt r
              in (lt', gt)
        else
          case go x r of
            (lt :*: gt) ->
              let !gt' = bin p m l gt
              in (lt, gt')
    _ -> case go x t of
          (lt :*: gt) -> (lt, gt)
  where
    go !x' t'@(Bin p m l r)
        | nomatch x' p m = if x' < p then (Nil :*: t') else (t' :*: Nil)
        | zero x' m      = case go x' l of (lt :*: gt) -> lt :*: bin p m gt r
        | otherwise      = case go x' r of (lt :*: gt) -> bin p m l lt :*: gt
    go x' t'@(Tip kx' bm)
        | kx' > x'          = (Nil :*: t')
          -- equivalent to kx' > prefixOf x'
        | kx' < prefixOf x' = (t' :*: Nil)
        | otherwise = tip kx' (bm .&. lowerBitmap) :*: tip kx' (bm .&. higherBitmap)
            where lowerBitmap = bitmapOf x' - 1
                  higherBitmap = complement (lowerBitmap + bitmapOf x')
    go _ Nil = (Nil :*: Nil)

-- | \(O(\min(n,W))\). Performs a 'split' but also returns whether the pivot
-- element was found in the original set.
splitMember :: Key -> Word64Set -> (Word64Set,Bool,Word64Set)
splitMember x t =
  case t of
    Bin p m l r
      | m < 0 ->
        if x >= 0 -- handle negative numbers.
        then
          case go x l of
            (lt, fnd, gt) ->
              let !lt' = bin p m lt r
              in (lt', fnd, gt)
        else
          case go x r of
            (lt, fnd, gt) ->
              let !gt' = bin p m l gt
              in (lt, fnd, gt')
    _ -> go x t
  where
    go x' t'@(Bin p m l r)
        | nomatch x' p m = if x' < p then (Nil, False, t') else (t', False, Nil)
        | zero x' m =
          case go x' l of
            (lt, fnd, gt) ->
              let !gt' = bin p m gt r
              in (lt, fnd, gt')
        | otherwise =
          case go x' r of
            (lt, fnd, gt) ->
              let !lt' = bin p m l lt
              in (lt', fnd, gt)
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

-- | \(O(\min(n,W))\). Retrieves the maximal key of the set, and the set
-- stripped of that element, or 'Nothing' if passed an empty set.
maxView :: Word64Set -> Maybe (Key, Word64Set)
maxView t =
  case t of Nil -> Nothing
            Bin p m l r | m < 0 -> case go l of (result, l') -> Just (result, bin p m l' r)
            _ -> Just (go t)
  where
    go (Bin p m l r) = case go r of (result, r') -> (result, bin p m l r')
    go (Tip kx bm) = case highestBitSet bm of bi -> (kx + bi, tip kx (bm .&. complement (bitmapOfSuffix bi)))
    go Nil = error "maxView Nil"

-- | \(O(\min(n,W))\). Retrieves the minimal key of the set, and the set
-- stripped of that element, or 'Nothing' if passed an empty set.
minView :: Word64Set -> Maybe (Key, Word64Set)
minView t =
  case t of Nil -> Nothing
            Bin p m l r | m < 0 -> case go r of (result, r') -> Just (result, bin p m l r')
            _ -> Just (go t)
  where
    go (Bin p m l r) = case go l of (result, l') -> (result, bin p m l' r)
    go (Tip kx bm) = case lowestBitSet bm of bi -> (kx + bi, tip kx (bm .&. complement (bitmapOfSuffix bi)))
    go Nil = error "minView Nil"

-- | \(O(\min(n,W))\). Delete and find the minimal element.
--
-- > deleteFindMin set = (findMin set, deleteMin set)
deleteFindMin :: Word64Set -> (Key, Word64Set)
deleteFindMin = fromMaybe (error "deleteFindMin: empty set has no minimal element") . minView

-- | \(O(\min(n,W))\). Delete and find the maximal element.
--
-- > deleteFindMax set = (findMax set, deleteMax set)
deleteFindMax :: Word64Set -> (Key, Word64Set)
deleteFindMax = fromMaybe (error "deleteFindMax: empty set has no maximal element") . maxView


-- | \(O(\min(n,W))\). The minimal element of the set.
findMin :: Word64Set -> Key
findMin Nil = error "findMin: empty set has no minimal element"
findMin (Tip kx bm) = kx + lowestBitSet bm
findMin (Bin _ m l r)
  |   m < 0   = find r
  | otherwise = find l
    where find (Tip kx bm) = kx + lowestBitSet bm
          find (Bin _ _ l' _) = find l'
          find Nil            = error "findMin Nil"

-- | \(O(\min(n,W))\). The maximal element of a set.
findMax :: Word64Set -> Key
findMax Nil = error "findMax: empty set has no maximal element"
findMax (Tip kx bm) = kx + highestBitSet bm
findMax (Bin _ m l r)
  |   m < 0   = find l
  | otherwise = find r
    where find (Tip kx bm) = kx + highestBitSet bm
          find (Bin _ _ _ r') = find r'
          find Nil            = error "findMax Nil"


-- | \(O(\min(n,W))\). Delete the minimal element. Returns an empty set if the set is empty.
--
-- Note that this is a change of behaviour for consistency with 'Data.Set.Set' &#8211;
-- versions prior to 0.5 threw an error if the 'Word64Set' was already empty.
deleteMin :: Word64Set -> Word64Set
deleteMin = maybe Nil snd . minView

-- | \(O(\min(n,W))\). Delete the maximal element. Returns an empty set if the set is empty.
--
-- Note that this is a change of behaviour for consistency with 'Data.Set.Set' &#8211;
-- versions prior to 0.5 threw an error if the 'Word64Set' was already empty.
deleteMax :: Word64Set -> Word64Set
deleteMax = maybe Nil snd . maxView

{----------------------------------------------------------------------
  Map
----------------------------------------------------------------------}

-- | \(O(n \min(n,W))\).
-- @'map' f s@ is the set obtained by applying @f@ to each element of @s@.
--
-- It's worth noting that the size of the result may be smaller if,
-- for some @(x,y)@, @x \/= y && f x == f y@

map :: (Key -> Key) -> Word64Set -> Word64Set
map f = fromList . List.map f . toList

-- | \(O(n)\). The
--
-- @'mapMonotonic' f s == 'map' f s@, but works only when @f@ is strictly increasing.
-- /The precondition is not checked./
-- Semi-formally, we have:
--
-- > and [x < y ==> f x < f y | x <- ls, y <- ls]
-- >                     ==> mapMonotonic f s == map f s
-- >     where ls = toList s
--
-- @since 0.6.3.1

-- Note that for now the test is insufficient to support any fancier implementation.
mapMonotonic :: (Key -> Key) -> Word64Set -> Word64Set
mapMonotonic f = fromDistinctAscList . List.map f . toAscList


{--------------------------------------------------------------------
  Fold
--------------------------------------------------------------------}
-- | \(O(n)\). Fold the elements in the set using the given right-associative
-- binary operator. This function is an equivalent of 'foldr' and is present
-- for compatibility only.
--
-- /Please note that fold will be deprecated in the future and removed./
fold :: (Key -> b -> b) -> b -> Word64Set -> b
fold = foldr
{-# INLINE fold #-}

-- | \(O(n)\). Fold the elements in the set using the given right-associative
-- binary operator, such that @'foldr' f z == 'Prelude.foldr' f z . 'toAscList'@.
--
-- For example,
--
-- > toAscList set = foldr (:) [] set
foldr :: (Key -> b -> b) -> b -> Word64Set -> b
foldr f z = \t ->      -- Use lambda t to be inlinable with two arguments only.
  case t of Bin _ m l r | m < 0 -> go (go z l) r -- put negative numbers before
                        | otherwise -> go (go z r) l
            _ -> go z t
  where
    go z' Nil           = z'
    go z' (Tip kx bm)   = foldrBits kx f z' bm
    go z' (Bin _ _ l r) = go (go z' r) l
{-# INLINE foldr #-}

-- | \(O(n)\). A strict version of 'foldr'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldr' :: (Key -> b -> b) -> b -> Word64Set -> b
foldr' f z = \t ->      -- Use lambda t to be inlinable with two arguments only.
  case t of Bin _ m l r | m < 0 -> go (go z l) r -- put negative numbers before
                        | otherwise -> go (go z r) l
            _ -> go z t
  where
    go !z' Nil           = z'
    go z' (Tip kx bm)   = foldr'Bits kx f z' bm
    go z' (Bin _ _ l r) = go (go z' r) l
{-# INLINE foldr' #-}

-- | \(O(n)\). Fold the elements in the set using the given left-associative
-- binary operator, such that @'foldl' f z == 'Prelude.foldl' f z . 'toAscList'@.
--
-- For example,
--
-- > toDescList set = foldl (flip (:)) [] set
foldl :: (a -> Key -> a) -> a -> Word64Set -> a
foldl f z = \t ->      -- Use lambda t to be inlinable with two arguments only.
  case t of Bin _ m l r | m < 0 -> go (go z r) l -- put negative numbers before
                        | otherwise -> go (go z l) r
            _ -> go z t
  where
    go z' Nil           = z'
    go z' (Tip kx bm)   = foldlBits kx f z' bm
    go z' (Bin _ _ l r) = go (go z' l) r
{-# INLINE foldl #-}

-- | \(O(n)\). A strict version of 'foldl'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldl' :: (a -> Key -> a) -> a -> Word64Set -> a
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
-- | \(O(n)\). An alias of 'toAscList'. The elements of a set in ascending order.
-- Subject to list fusion.
elems :: Word64Set -> [Key]
elems
  = toAscList

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}

-- | @since 0.5.6.2
instance GHC.Exts.IsList Word64Set where
  type Item Word64Set = Key
  fromList = fromList
  toList   = toList

-- | \(O(n)\). Convert the set to a list of elements. Subject to list fusion.
toList :: Word64Set -> [Key]
toList
  = toAscList

-- | \(O(n)\). Convert the set to an ascending list of elements. Subject to list
-- fusion.
toAscList :: Word64Set -> [Key]
toAscList = foldr (:) []

-- | \(O(n)\). Convert the set to a descending list of elements. Subject to list
-- fusion.
toDescList :: Word64Set -> [Key]
toDescList = foldl (flip (:)) []

-- List fusion for the list generating functions.
-- The foldrFB and foldlFB are foldr and foldl equivalents, used for list fusion.
-- They are important to convert unfused to{Asc,Desc}List back, see mapFB in prelude.
foldrFB :: (Key -> b -> b) -> b -> Word64Set -> b
foldrFB = foldr
{-# INLINE[0] foldrFB #-}
foldlFB :: (a -> Key -> a) -> a -> Word64Set -> a
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
{-# RULES "Word64Set.toAscList" [~1] forall s . toAscList s = GHC.Exts.build (\c n -> foldrFB c n s) #-}
{-# RULES "Word64Set.toAscListBack" [1] foldrFB (:) [] = toAscList #-}
{-# RULES "Word64Set.toDescList" [~1] forall s . toDescList s = GHC.Exts.build (\c n -> foldlFB (\xs x -> c x xs) n s) #-}
{-# RULES "Word64Set.toDescListBack" [1] foldlFB (\xs x -> x : xs) [] = toDescList #-}


-- | \(O(n \min(n,W))\). Create a set from a list of integers.
{-# INLINABLE fromList #-}
fromList :: [Key] -> Word64Set
fromList = List.foldl' ins empty
  where
    ins t x  = insert x t

-- | \(O(n)\). Build a set from an ascending list of elements.
-- /The precondition (input list is ascending) is not checked./
fromAscList :: [Key] -> Word64Set
fromAscList = fromMonoList
{-# NOINLINE fromAscList #-}

-- | \(O(n)\). Build a set from an ascending list of distinct elements.
-- /The precondition (input list is strictly ascending) is not checked./
fromDistinctAscList :: [Key] -> Word64Set
fromDistinctAscList = fromAscList
{-# INLINE fromDistinctAscList #-}

-- | \(O(n)\). Build a set from a monotonic list of elements.
--
-- The precise conditions under which this function works are subtle:
-- For any branch mask, keys with the same prefix w.r.t. the branch
-- mask must occur consecutively in the list.
fromMonoList :: [Key] -> Word64Set
fromMonoList []         = Nil
fromMonoList (kx : zs1) = addAll' (prefixOf kx) (bitmapOf kx) zs1
  where
    -- `addAll'` collects all keys with the prefix `px` into a single
    -- bitmap, and then proceeds with `addAll`.
    addAll' !px !bm []
        = Tip px bm
    addAll' !px !bm (ky : zs)
        | px == prefixOf ky
        = addAll' px (bm .|. bitmapOf ky) zs
        -- inlined: | otherwise = addAll px (Tip px bm) (ky : zs)
        | py <- prefixOf ky
        , m <- branchMask px py
        , Inserted ty zs' <- addMany' m py (bitmapOf ky) zs
        = addAll px (linkWithMask m py ty {-px-} (Tip px bm)) zs'

    -- for `addAll` and `addMany`, px is /a/ prefix inside the tree `tx`
    -- `addAll` consumes the rest of the list, adding to the tree `tx`
    addAll !_px !tx []
        = tx
    addAll !px !tx (ky : zs)
        | py <- prefixOf ky
        , m <- branchMask px py
        , Inserted ty zs' <- addMany' m py (bitmapOf ky) zs
        = addAll px (linkWithMask m py ty {-px-} tx) zs'

    -- `addMany'` is similar to `addAll'`, but proceeds with `addMany'`.
    addMany' !_m !px !bm []
        = Inserted (Tip px bm) []
    addMany' !m !px !bm zs0@(ky : zs)
        | px == prefixOf ky
        = addMany' m px (bm .|. bitmapOf ky) zs
        -- inlined: | otherwise = addMany m px (Tip px bm) (ky : zs)
        | mask px m /= mask ky m
        = Inserted (Tip (prefixOf px) bm) zs0
        | py <- prefixOf ky
        , mxy <- branchMask px py
        , Inserted ty zs' <- addMany' mxy py (bitmapOf ky) zs
        = addMany m px (linkWithMask mxy py ty {-px-} (Tip px bm)) zs'

    -- `addAll` adds to `tx` all keys whose prefix w.r.t. `m` agrees with `px`.
    addMany !_m !_px tx []
        = Inserted tx []
    addMany !m !px tx zs0@(ky : zs)
        | mask px m /= mask ky m
        = Inserted tx zs0
        | py <- prefixOf ky
        , mxy <- branchMask px py
        , Inserted ty zs' <- addMany' mxy py (bitmapOf ky) zs
        = addMany m px (linkWithMask mxy py ty {-px-} tx) zs'
{-# INLINE fromMonoList #-}

data Inserted = Inserted !Word64Set ![Key]

{--------------------------------------------------------------------
  Eq
--------------------------------------------------------------------}
instance Eq Word64Set where
  t1 == t2  = equal t1 t2
  t1 /= t2  = nequal t1 t2

equal :: Word64Set -> Word64Set -> Bool
equal (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  = (m1 == m2) && (p1 == p2) && (equal l1 l2) && (equal r1 r2)
equal (Tip kx1 bm1) (Tip kx2 bm2)
  = kx1 == kx2 && bm1 == bm2
equal Nil Nil = True
equal _   _   = False

nequal :: Word64Set -> Word64Set -> Bool
nequal (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  = (m1 /= m2) || (p1 /= p2) || (nequal l1 l2) || (nequal r1 r2)
nequal (Tip kx1 bm1) (Tip kx2 bm2)
  = kx1 /= kx2 || bm1 /= bm2
nequal Nil Nil = False
nequal _   _   = True

{--------------------------------------------------------------------
  Ord
--------------------------------------------------------------------}

instance Ord Word64Set where
    compare s1 s2 = compare (toAscList s1) (toAscList s2)
    -- tentative implementation. See if more efficient exists.

{--------------------------------------------------------------------
  Show
--------------------------------------------------------------------}
instance Show Word64Set where
  showsPrec p xs = showParen (p > 10) $
    showString "fromList " . shows (toList xs)

{--------------------------------------------------------------------
  Read
--------------------------------------------------------------------}
instance Read Word64Set where
  readPrec = parens $ prec 10 $ do
    Ident "fromList" <- lexP
    xs <- readPrec
    return (fromList xs)

  readListPrec = readListPrecDefault

{--------------------------------------------------------------------
  NFData
--------------------------------------------------------------------}

-- The Word64Set constructors consist only of strict fields of Ints and
-- Word64Sets, thus the default NFData instance which evaluates to whnf
-- should suffice
instance NFData Word64Set where rnf x = seq x ()

{--------------------------------------------------------------------
  Debugging
--------------------------------------------------------------------}
-- | \(O(n \min(n,W))\). Show the tree that implements the set. The tree is shown
-- in a compressed, hanging format.
showTree :: Word64Set -> String
showTree s
  = showTreeWith True False s


{- | \(O(n \min(n,W))\). The expression (@'showTreeWith' hang wide map@) shows
 the tree that implements the set. If @hang@ is
 'True', a /hanging/ tree is shown otherwise a rotated tree is shown. If
 @wide@ is 'True', an extra wide version is shown.
-}
showTreeWith :: Bool -> Bool -> Word64Set -> String
showTreeWith hang wide t
  | hang      = (showsTreeHang wide [] t) ""
  | otherwise = (showsTree wide [] [] t) ""

showsTree :: Bool -> [String] -> [String] -> Word64Set -> ShowS
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

showsTreeHang :: Bool -> [String] -> Word64Set -> ShowS
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
showsBars (_ : tl) = showString (concat (reverse tl)) . showString node

showsBitMap :: Word64 -> ShowS
showsBitMap = showString . showBitMap

showBitMap :: Word64 -> String
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
link :: Prefix -> Word64Set -> Prefix -> Word64Set -> Word64Set
link p1 t1 p2 t2 = linkWithMask (branchMask p1 p2) p1 t1 {-p2-} t2
{-# INLINE link #-}

-- `linkWithMask` is useful when the `branchMask` has already been computed
linkWithMask :: Mask -> Prefix -> Word64Set -> Word64Set -> Word64Set
linkWithMask m p1 t1 {-p2-} t2
  | zero p1 m = Bin p m t1 t2
  | otherwise = Bin p m t2 t1
  where
    p = mask p1 m
{-# INLINE linkWithMask #-}

{--------------------------------------------------------------------
  @bin@ assures that we never have empty trees within a tree.
--------------------------------------------------------------------}
bin :: Prefix -> Mask -> Word64Set -> Word64Set -> Word64Set
bin _ _ l Nil = l
bin _ _ Nil r = r
bin p m l r   = Bin p m l r
{-# INLINE bin #-}

{--------------------------------------------------------------------
  @tip@ assures that we never have empty bitmaps within a tree.
--------------------------------------------------------------------}
tip :: Prefix -> BitMap -> Word64Set
tip _ 0 = Nil
tip kx bm = Tip kx bm
{-# INLINE tip #-}


{----------------------------------------------------------------------
  Functions that generate Prefix and BitMap of a Key or a Suffix.
----------------------------------------------------------------------}

suffixBitMask :: Word64
suffixBitMask = fromIntegral (finiteBitSize (undefined::Word64)) - 1
{-# INLINE suffixBitMask #-}

prefixBitMask :: Word64
prefixBitMask = complement suffixBitMask
{-# INLINE prefixBitMask #-}

prefixOf :: Word64 -> Prefix
prefixOf x = x .&. prefixBitMask
{-# INLINE prefixOf #-}

suffixOf :: Word64 -> Word64
suffixOf x = x .&. suffixBitMask
{-# INLINE suffixOf #-}

bitmapOfSuffix :: Word64 -> BitMap
bitmapOfSuffix s = 1 `shiftLL` fromIntegral s
{-# INLINE bitmapOfSuffix #-}

bitmapOf :: Word64 -> BitMap
bitmapOf x = bitmapOfSuffix (suffixOf x)
{-# INLINE bitmapOf #-}


{--------------------------------------------------------------------
  Endian independent bit twiddling
--------------------------------------------------------------------}
-- Returns True iff the bits set in i and the Mask m are disjoint.
zero :: Word64 -> Mask -> Bool
zero i m
  = (natFromInt i) .&. (natFromInt m) == 0
{-# INLINE zero #-}

nomatch,match :: Word64 -> Prefix -> Mask -> Bool
nomatch i p m
  = (mask i m) /= p
{-# INLINE nomatch #-}

match i p m
  = (mask i m) == p
{-# INLINE match #-}

-- Suppose a is largest such that 2^a divides 2*m.
-- Then mask i m is i with the low a bits zeroed out.
mask :: Word64 -> Mask -> Prefix
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

lowestBitSet :: Nat -> Word64
highestBitSet :: Nat -> Word64
foldlBits :: Word64 -> (a -> Word64 -> a) -> a -> Nat -> a
foldl'Bits :: Word64 -> (a -> Word64 -> a) -> a -> Nat -> a
foldrBits :: Word64 -> (Word64 -> a -> a) -> a -> Nat -> a
foldr'Bits :: Word64 -> (Word64 -> a -> a) -> a -> Nat -> a
takeWhileAntitoneBits :: Word64 -> (Word64 -> Bool) -> Nat -> Nat

{-# INLINE lowestBitSet #-}
{-# INLINE highestBitSet #-}
{-# INLINE foldlBits #-}
{-# INLINE foldl'Bits #-}
{-# INLINE foldrBits #-}
{-# INLINE foldr'Bits #-}
{-# INLINE takeWhileAntitoneBits #-}

indexOfTheOnlyBit :: Nat -> Word64
{-# INLINE indexOfTheOnlyBit #-}
indexOfTheOnlyBit bitmask = fromIntegral $ countTrailingZeros bitmask

lowestBitSet x = fromIntegral $ countTrailingZeros x

highestBitSet x = fromIntegral $ 63 - countLeadingZeros x

lowestBitMask :: Nat -> Nat
lowestBitMask x = x .&. negate x
{-# INLINE lowestBitMask #-}

-- Reverse the order of bits in the Nat.
revNat :: Nat -> Nat
revNat x1 = case ((x1 `shiftRL` 1) .&. 0x5555555555555555) .|. ((x1 .&. 0x5555555555555555) `shiftLL` 1) of
              x2 -> case ((x2 `shiftRL` 2) .&. 0x3333333333333333) .|. ((x2 .&. 0x3333333333333333) `shiftLL` 2) of
                 x3 -> case ((x3 `shiftRL` 4) .&. 0x0F0F0F0F0F0F0F0F) .|. ((x3 .&. 0x0F0F0F0F0F0F0F0F) `shiftLL` 4) of
                   x4 -> case ((x4 `shiftRL` 8) .&. 0x00FF00FF00FF00FF) .|. ((x4 .&. 0x00FF00FF00FF00FF) `shiftLL` 8) of
                     x5 -> case ((x5 `shiftRL` 16) .&. 0x0000FFFF0000FFFF) .|. ((x5 .&. 0x0000FFFF0000FFFF) `shiftLL` 16) of
                       x6 -> ( x6 `shiftRL` 32             ) .|. ( x6               `shiftLL` 32);

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
        go bm acc = go (bm `xor` bitmask) ((f $! (prefix+63-bi)) acc)
          where !bitmask = lowestBitMask bm
                !bi = indexOfTheOnlyBit bitmask


foldr'Bits prefix f z bitmap = go (revNat bitmap) z
  where go 0 acc = acc
        go bm !acc = go (bm `xor` bitmask) ((f $! (prefix+63-bi)) acc)
          where !bitmask = lowestBitMask bm
                !bi = indexOfTheOnlyBit bitmask

takeWhileAntitoneBits prefix predicate bitmap =
  -- Binary search for the first index where the predicate returns false, but skip a predicate
  -- call if the high half of the current range is empty. This ensures
  -- min (log2 64 + 1 = 7) (popcount bitmap) predicate calls.
  let next d h (n',b') =
        if n' .&. h /= 0 && (predicate $! prefix + fromIntegral (b'+d)) then (n' `shiftRL` d, b'+d) else (n',b')
      {-# INLINE next #-}
      (_,b) = next 1  0x2 $
              next 2  0xC $
              next 4  0xF0 $
              next 8  0xFF00 $
              next 16 0xFFFF0000 $
              next 32 0xFFFFFFFF00000000 $
              (bitmap,0)
      m = if b /= 0 || (bitmap .&. 0x1 /= 0 && predicate prefix)
          then ((2 `shiftLL` b) - 1)
          else ((1 `shiftLL` b) - 1)
  in bitmap .&. m


{--------------------------------------------------------------------
  Utilities
--------------------------------------------------------------------}

-- | \(O(1)\).  Decompose a set into pieces based on the structure of the underlying
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
splitRoot :: Word64Set -> [Word64Set]
splitRoot Nil = []
-- NOTE: we don't currently split below Tip, but we could.
splitRoot x@(Tip _ _) = [x]
splitRoot (Bin _ m l r) | m < 0 = [r, l]
                        | otherwise = [l, r]
{-# INLINE splitRoot #-}

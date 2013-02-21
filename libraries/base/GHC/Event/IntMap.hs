{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, MagicHash, NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Event.IntMap
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- An efficient implementation of maps from integer keys to values.
--
-- Since many function names (but not the type name) clash with
-- "Prelude" names, this module is usually imported @qualified@, e.g.
--
-- >  import Data.IntMap (IntMap)
-- >  import qualified Data.IntMap as IntMap
--
-- The implementation is based on /big-endian patricia trees/.  This data
-- structure performs especially well on binary operations like 'union'
-- and 'intersection'.  However, my benchmarks show that it is also
-- (much) faster on insertions and deletions when compared to a generic
-- size-balanced map implementation (see "Data.Map").
--
--    * Chris Okasaki and Andy Gill,  \"/Fast Mergeable Integer Maps/\",
--      Workshop on ML, September 1998, pages 77-86,
--      <http://citeseer.ist.psu.edu/okasaki98fast.html>
--
--    * D.R. Morrison, \"/PATRICIA -- Practical Algorithm To Retrieve
--      Information Coded In Alphanumeric/\", Journal of the ACM, 15(4),
--      October 1968, pages 514-534.
--
-- Operation comments contain the operation time complexity in
-- the Big-O notation <http://en.wikipedia.org/wiki/Big_O_notation>.
-- Many operations have a worst-case complexity of /O(min(n,W))/.
-- This means that the operation can become linear in the number of
-- elements with a maximum of /W/ -- the number of bits in an 'Int'
-- (32 or 64).
--
-----------------------------------------------------------------------------

module GHC.Event.IntMap
    (
    -- * Map type
    IntMap
    , Key

    -- * Query
    , lookup
    , member

    -- * Construction
    , empty

    -- * Insertion
    , insertWith

    -- * Delete\/Update
    , delete
    , updateWith

    -- * Traversal
    -- ** Fold
    , foldWithKey

    -- * Conversion
    , keys
    ) where

import Data.Bits

import Data.Maybe (Maybe(..))
import GHC.Base hiding (foldr)
import GHC.Num (Num(..))
import GHC.Real (fromIntegral)
import GHC.Show (Show(showsPrec), showParen, shows, showString)

#if !defined(__GLASGOW_HASKELL__)
import Data.Word
#endif

-- | A @Nat@ is a natural machine word (an unsigned Int)
type Nat = Word

natFromInt :: Key -> Nat
natFromInt i = fromIntegral i

intFromNat :: Nat -> Key
intFromNat w = fromIntegral w

shiftRL :: Nat -> Key -> Nat
#if __GLASGOW_HASKELL__
-- GHC: use unboxing to get @shiftRL@ inlined.
shiftRL (W# x) (I# i) = W# (shiftRL# x i)
#else
shiftRL x i = shiftR x i
#endif

------------------------------------------------------------------------
-- Types

-- | A map of integers to values @a@.
data IntMap a = Nil
              | Tip {-# UNPACK #-} !Key !a
              | Bin {-# UNPACK #-} !Prefix
                    {-# UNPACK #-} !Mask
                    !(IntMap a)
                    !(IntMap a)

type Prefix = Int
type Mask   = Int
type Key    = Int

------------------------------------------------------------------------
-- Query

-- | /O(min(n,W))/ Lookup the value at a key in the map.  See also
-- 'Data.Map.lookup'.
lookup :: Key -> IntMap a -> Maybe a
lookup k t = let nk = natFromInt k in seq nk (lookupN nk t)

lookupN :: Nat -> IntMap a -> Maybe a
lookupN k t
  = case t of
      Bin _ m l r
        | zeroN k (natFromInt m) -> lookupN k l
        | otherwise              -> lookupN k r
      Tip kx x
        | (k == natFromInt kx)  -> Just x
        | otherwise             -> Nothing
      Nil -> Nothing

-- | /O(min(n,W))/. Is the key a member of the map?
--
-- > member 5 (fromList [(5,'a'), (3,'b')]) == True
-- > member 1 (fromList [(5,'a'), (3,'b')]) == False

member :: Key -> IntMap a -> Bool
member k m
  = case lookup k m of
      Nothing -> False
      Just _  -> True

------------------------------------------------------------------------
-- Construction

-- | /O(1)/ The empty map.
--
-- > empty      == fromList []
-- > size empty == 0
empty :: IntMap a
empty = Nil

------------------------------------------------------------------------
-- Insert

-- | /O(min(n,W))/ Insert with a function, combining new value and old
-- value.  @insertWith f key value mp@ will insert the pair (key,
-- value) into @mp@ if key does not exist in the map.  If the key does
-- exist, the function will insert the pair (key, f new_value
-- old_value).  The result is a pair where the first element is the
-- old value, if one was present, and the second is the modified map.
insertWith :: (a -> a -> a) -> Key -> a -> IntMap a -> (Maybe a, IntMap a)
insertWith f k x t = case t of
    Bin p m l r
        | nomatch k p m -> (Nothing, join k (Tip k x) p t)
        | zero k m      -> let (found, l') = insertWith f k x l
                           in (found, Bin p m l' r)
        | otherwise     -> let (found, r') = insertWith f k x r
                           in (found, Bin p m l r')
    Tip ky y
        | k == ky       -> (Just y, Tip k (f x y))
        | otherwise     -> (Nothing, join k (Tip k x) ky t)
    Nil                 -> (Nothing, Tip k x)


------------------------------------------------------------------------
-- Delete/Update

-- | /O(min(n,W))/. Delete a key and its value from the map.  When the
-- key is not a member of the map, the original map is returned.  The
-- result is a pair where the first element is the value associated
-- with the deleted key, if one existed, and the second element is the
-- modified map.
delete :: Key -> IntMap a -> (Maybe a, IntMap a)
delete k t = case t of
   Bin p m l r
        | nomatch k p m -> (Nothing, t)
        | zero k m      -> let (found, l') = delete k l
                           in (found, bin p m l' r)
        | otherwise     -> let (found, r') = delete k r
                           in (found, bin p m l r')
   Tip ky y
        | k == ky       -> (Just y, Nil)
        | otherwise     -> (Nothing, t)
   Nil                  -> (Nothing, Nil)

updateWith :: (a -> Maybe a) -> Key -> IntMap a -> (Maybe a, IntMap a)
updateWith f k t = case t of
    Bin p m l r
        | nomatch k p m -> (Nothing, t)
        | zero k m      -> let (found, l') = updateWith f k l
                           in (found, bin p m l' r)
        | otherwise     -> let (found, r') = updateWith f k r
                           in (found, bin p m l r')
    Tip ky y
        | k == ky       -> case (f y) of
                               Just y' -> (Just y, Tip ky y')
                               Nothing -> (Just y, Nil)
        | otherwise     -> (Nothing, t)
    Nil                 -> (Nothing, Nil)
-- | /O(n)/. Fold the keys and values in the map, such that
-- @'foldWithKey' f z == 'Prelude.foldr' ('uncurry' f) z . 'toAscList'@.
-- For example,
--
-- > keys map = foldWithKey (\k x ks -> k:ks) [] map
--
-- > let f k a result = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
-- > foldWithKey f "Map: " (fromList [(5,"a"), (3,"b")]) == "Map: (5:a)(3:b)"

foldWithKey :: (Key -> a -> b -> b) -> b -> IntMap a -> b
foldWithKey f z t
  = foldr f z t

-- | /O(n)/. Convert the map to a list of key\/value pairs.
--
-- > toList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
-- > toList empty == []

toList :: IntMap a -> [(Key,a)]
toList t
  = foldWithKey (\k x xs -> (k,x):xs) [] t

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
      Bin _ _ l r -> foldr' f (foldr' f z r) l
      Tip k x     -> f k x z
      Nil         -> z

-- | /O(n)/. Return all keys of the map in ascending order.
--
-- > keys (fromList [(5,"a"), (3,"b")]) == [3,5]
-- > keys empty == []

keys  :: IntMap a -> [Key]
keys m
  = foldWithKey (\k _ ks -> k:ks) [] m

------------------------------------------------------------------------
-- Eq

instance Eq a => Eq (IntMap a) where
    t1 == t2 = equal t1 t2
    t1 /= t2 = nequal t1 t2

equal :: Eq a => IntMap a -> IntMap a -> Bool
equal (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
    = (m1 == m2) && (p1 == p2) && (equal l1 l2) && (equal r1 r2)
equal (Tip kx x) (Tip ky y)
    = (kx == ky) && (x==y)
equal Nil Nil = True
equal _   _   = False

nequal :: Eq a => IntMap a -> IntMap a -> Bool
nequal (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
    = (m1 /= m2) || (p1 /= p2) || (nequal l1 l2) || (nequal r1 r2)
nequal (Tip kx x) (Tip ky y)
    = (kx /= ky) || (x/=y)
nequal Nil Nil = False
nequal _   _   = True

instance Show a => Show (IntMap a) where
  showsPrec d m   = showParen (d > 10) $
    showString "fromList " . shows (toList m)

------------------------------------------------------------------------
-- Utility functions

join :: Prefix -> IntMap a -> Prefix -> IntMap a -> IntMap a
join p1 t1 p2 t2
  | zero p1 m = Bin p m t1 t2
  | otherwise = Bin p m t2 t1
  where
    m = branchMask p1 p2
    p = mask p1 m

-- | @bin@ assures that we never have empty trees within a tree.
bin :: Prefix -> Mask -> IntMap a -> IntMap a -> IntMap a
bin _ _ l Nil = l
bin _ _ Nil r = r
bin p m l r   = Bin p m l r

------------------------------------------------------------------------
-- Endian independent bit twiddling

zero :: Key -> Mask -> Bool
zero i m = (natFromInt i) .&. (natFromInt m) == 0

nomatch :: Key -> Prefix -> Mask -> Bool
nomatch i p m = (mask i m) /= p

mask :: Key -> Mask -> Prefix
mask i m = maskW (natFromInt i) (natFromInt m)

zeroN :: Nat -> Nat -> Bool
zeroN i m = (i .&. m) == 0

------------------------------------------------------------------------
-- Big endian operations

maskW :: Nat -> Nat -> Prefix
maskW i m = intFromNat (i .&. (complement (m-1) `xor` m))

branchMask :: Prefix -> Prefix -> Mask
branchMask p1 p2
    = intFromNat (highestBitMask (natFromInt p1 `xor` natFromInt p2))

-- The highestBitMask implementation is based on
-- http://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2
-- which has been put in the public domain.

-- | Return a word where only the highest bit is set.
highestBitMask :: Nat -> Nat
highestBitMask x1 = let x2 = x1 .|. x1 `shiftRL` 1
                        x3 = x2 .|. x2 `shiftRL` 2
                        x4 = x3 .|. x3 `shiftRL` 4
                        x5 = x4 .|. x4 `shiftRL` 8
                        x6 = x5 .|. x5 `shiftRL` 16
#if !(WORD_SIZE_IN_BITS==32)
                        x7 = x6 .|. x6 `shiftRL` 32
                     in x7 `xor` (x7 `shiftRL` 1)
#else
                     in x6 `xor` (x6 `shiftRL` 1)
#endif
{-# INLINE highestBitMask #-}

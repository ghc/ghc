-- |
-- Module      : Data.Array.Byte
-- Copyright   : (c) Roman Leshchinskiy 2009-2012
-- License     : BSD-style
--
-- Maintainer  : libraries@haskell.org
-- Portability : non-portable
--
-- Derived from @primitive@ package.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Array.Byte (
  ByteArray(..),
  MutableByteArray(..),
) where

import Data.Bits ((.&.), unsafeShiftR)
import Data.Data (mkNoRepType, Data(..), Typeable)
import qualified Data.Foldable as F
import Data.Semigroup
import GHC.Show (intToDigit)
import GHC.Exts
import GHC.ST (ST(..), runST)
import GHC.Word (Word8(..))

-- | Boxed wrapper for 'ByteArray#'.
--
-- Since 'ByteArray#' is an unlifted type and not a member of kind 'Data.Kind.Type',
-- things like @[ByteArray#]@ or @IO ByteArray#@ are ill-typed. To work around this
-- inconvenience this module provides a standard boxed wrapper, inhabiting 'Data.Kind.Type'.
-- Clients are expected to use 'ByteArray' in higher-level APIs,
-- but wrap and unwrap 'ByteArray' internally as they please
-- and use functions from "GHC.Exts".
--
-- @since 4.17.0.0
data ByteArray = ByteArray ByteArray#

-- | Boxed wrapper for 'MutableByteArray#'.
--
-- Since 'MutableByteArray#' is an unlifted type and not a member of kind 'Data.Kind.Type',
-- things like @[MutableByteArray#]@ or @IO MutableByteArray#@ are ill-typed. To work around this
-- inconvenience this module provides a standard boxed wrapper, inhabiting 'Data.Kind.Type'.
-- Clients are expected to use 'MutableByteArray' in higher-level APIs,
-- but wrap and unwrap 'MutableByteArray' internally as they please
-- and use functions from "GHC.Exts".
--
-- @since 4.17.0.0
data MutableByteArray s = MutableByteArray (MutableByteArray# s)

-- | Create a new mutable byte array of the specified size in bytes.
--
-- /Note:/ this function does not check if the input is non-negative.
newByteArray :: Int -> ST s (MutableByteArray s)
{-# INLINE newByteArray #-}
newByteArray (I# n#) =
  ST (\s# -> case newByteArray# n# s# of
    (# s'#, arr# #) -> (# s'#, MutableByteArray arr# #))

-- | Convert a mutable byte array to an immutable one without copying. The
-- array should not be modified after the conversion.
unsafeFreezeByteArray :: MutableByteArray s -> ST s ByteArray
{-# INLINE unsafeFreezeByteArray #-}
unsafeFreezeByteArray (MutableByteArray arr#) =
  ST (\s# -> case unsafeFreezeByteArray# arr# s# of
    (# s'#, arr'# #) -> (# s'#, ByteArray arr'# #))

-- | Size of the byte array in bytes.
sizeofByteArray :: ByteArray -> Int
{-# INLINE sizeofByteArray #-}
sizeofByteArray (ByteArray arr#) = I# (sizeofByteArray# arr#)

-- | Read byte at specific index.
indexByteArray :: ByteArray -> Int -> Word8
{-# INLINE indexByteArray #-}
indexByteArray (ByteArray arr#) (I# i#) = W8# (indexWord8Array# arr# i#)

-- | Write byte at specific index.
writeByteArray :: MutableByteArray s -> Int -> Word8 -> ST s ()
{-# INLINE writeByteArray #-}
writeByteArray (MutableByteArray arr#) (I# i#) (W8# x#) =
  ST (\s# -> case writeWord8Array# arr# i# x# s# of
    s'# -> (# s'#, () #))

-- | Explode 'ByteArray' into a list of bytes.
byteArrayToList :: ByteArray -> [Word8]
{-# INLINE byteArrayToList #-}
byteArrayToList arr = go 0
  where
    go i
      | i < maxI  = indexByteArray arr i : go (i+1)
      | otherwise = []
    maxI = sizeofByteArray arr

-- | Create a 'ByteArray' from a list of a known length. If the length
--   of the list does not match the given length, this throws an exception.
byteArrayFromListN :: Int -> [Word8] -> ByteArray
byteArrayFromListN n ys = runST $ do
    marr <- newByteArray n
    let go !ix [] = if ix == n
          then return ()
          else error $ "Data.Array.Byte.byteArrayFromListN: list length less than specified size"
        go !ix (x : xs) = if ix < n
          then do
            writeByteArray marr ix x
            go (ix + 1) xs
          else error $ "Data.Array.Byte.byteArrayFromListN: list length greater than specified size"
    go 0 ys
    unsafeFreezeByteArray marr

-- | Copy a slice of an immutable byte array to a mutable byte array.
--
-- /Note:/ this function does not do bounds or overlap checking.
copyByteArray
  :: MutableByteArray s -- ^ destination array
  -> Int                -- ^ offset into destination array
  -> ByteArray          -- ^ source array
  -> Int                -- ^ offset into source array
  -> Int                -- ^ number of bytes to copy
  -> ST s ()
{-# INLINE copyByteArray #-}
copyByteArray (MutableByteArray dst#) (I# doff#) (ByteArray src#) (I# soff#) (I# sz#) =
  ST (\s# -> case copyByteArray# src# soff# dst# doff# sz# s# of
    s'# -> (# s'#, () #))

-- | @since 4.17.0.0
instance Data ByteArray where
  toConstr _ = error "toConstr"
  gunfold _ _ = error "gunfold"
  dataTypeOf _ = mkNoRepType "Data.Array.Byte.ByteArray"

-- | @since 4.17.0.0
instance Typeable s => Data (MutableByteArray s) where
  toConstr _ = error "toConstr"
  gunfold _ _ = error "gunfold"
  dataTypeOf _ = mkNoRepType "Data.Array.Byte.MutableByteArray"

-- | @since 4.17.0.0
instance Show ByteArray where
  showsPrec _ ba =
      showString "[" . go 0
    where
      showW8 :: Word8 -> String -> String
      showW8 !w s =
          '0'
        : 'x'
        : intToDigit (fromIntegral (unsafeShiftR w 4))
        : intToDigit (fromIntegral (w .&. 0x0F))
        : s
      go i
        | i < sizeofByteArray ba = comma . showW8 (indexByteArray ba i :: Word8) . go (i+1)
        | otherwise              = showChar ']'
        where
          comma | i == 0    = id
                | otherwise = showString ", "

-- | Compare prefixes of given length.
compareByteArraysFromBeginning :: ByteArray -> ByteArray -> Int -> Ordering
{-# INLINE compareByteArraysFromBeginning #-}
compareByteArraysFromBeginning (ByteArray ba1#) (ByteArray ba2#) (I# n#)
  = compare (I# (compareByteArrays# ba1# 0# ba2# 0# n#)) 0

-- | Do two byte arrays share the same pointer?
sameByteArray :: ByteArray# -> ByteArray# -> Bool
sameByteArray ba1 ba2 =
    case reallyUnsafePtrEquality# (unsafeCoerce# ba1 :: ()) (unsafeCoerce# ba2 :: ()) of
      r -> isTrue# r

-- | @since 4.17.0.0
instance Eq ByteArray where
  ba1@(ByteArray ba1#) == ba2@(ByteArray ba2#)
    | sameByteArray ba1# ba2# = True
    | n1 /= n2 = False
    | otherwise = compareByteArraysFromBeginning ba1 ba2 n1 == EQ
    where
      n1 = sizeofByteArray ba1
      n2 = sizeofByteArray ba2

-- | @since 4.17.0.0
instance Eq (MutableByteArray s) where
  (==) (MutableByteArray arr#) (MutableByteArray brr#)
    = isTrue# (sameMutableByteArray# arr# brr#)

-- | Non-lexicographic ordering. This compares the lengths of
-- the byte arrays first and uses a lexicographic ordering if
-- the lengths are equal. Subject to change between major versions.
-- @since 4.17.0.0
instance Ord ByteArray where
  ba1@(ByteArray ba1#) `compare` ba2@(ByteArray ba2#)
    | sameByteArray ba1# ba2# = EQ
    | n1 /= n2 = n1 `compare` n2
    | otherwise = compareByteArraysFromBeginning ba1 ba2 n1
    where
      n1 = sizeofByteArray ba1
      n2 = sizeofByteArray ba2
-- The primop compareByteArrays# (invoked from 'compareByteArraysFromBeginning')
-- performs a check for pointer equality as well. However, it
-- is included here because it is likely better to check for pointer equality
-- before checking for length equality. Getting the length requires deferencing
-- the pointers, which could cause accesses to memory that is not in the cache.
-- By contrast, a pointer equality check is always extremely cheap.

-- | Append two byte arrays.
appendByteArray :: ByteArray -> ByteArray -> ByteArray
appendByteArray a b = runST $ do
  marr <- newByteArray (sizeofByteArray a + sizeofByteArray b)
  copyByteArray marr 0 a 0 (sizeofByteArray a)
  copyByteArray marr (sizeofByteArray a) b 0 (sizeofByteArray b)
  unsafeFreezeByteArray marr

-- | Concatenate a list of 'ByteArray's.
concatByteArray :: [ByteArray] -> ByteArray
concatByteArray arrs = runST $ do
  let len = calcLength arrs 0
  marr <- newByteArray len
  pasteByteArrays marr 0 arrs
  unsafeFreezeByteArray marr

-- | Dump immutable 'ByteArray's into a mutable one, starting from a given offset.
pasteByteArrays :: MutableByteArray s -> Int -> [ByteArray] -> ST s ()
pasteByteArrays !_ !_ [] = return ()
pasteByteArrays !marr !ix (x : xs) = do
  copyByteArray marr ix x 0 (sizeofByteArray x)
  pasteByteArrays marr (ix + sizeofByteArray x) xs

-- | Compute total length of 'ByteArray's, increased by accumulator.
calcLength :: [ByteArray] -> Int -> Int
calcLength [] !n = n
calcLength (x : xs) !n = calcLength xs (sizeofByteArray x + n)

-- | An array of zero length.
emptyByteArray :: ByteArray
emptyByteArray = runST (newByteArray 0 >>= unsafeFreezeByteArray)

-- | Replicate 'ByteArray' given number of times and concatenate all together.
replicateByteArray :: Int -> ByteArray -> ByteArray
replicateByteArray n arr = runST $ do
  marr <- newByteArray (n * sizeofByteArray arr)
  let go i = if i < n
        then do
          copyByteArray marr (i * sizeofByteArray arr) arr 0 (sizeofByteArray arr)
          go (i + 1)
        else return ()
  go 0
  unsafeFreezeByteArray marr

-- | @since 4.17.0.0
instance Semigroup ByteArray where
  (<>) = appendByteArray
  sconcat = mconcat . F.toList
  stimes i arr
    | itgr < 1 = emptyByteArray
    | itgr <= (fromIntegral (maxBound :: Int)) = replicateByteArray (fromIntegral itgr) arr
    | otherwise = error "Data.Array.Byte#stimes: cannot allocate the requested amount of memory"
    where itgr = toInteger i :: Integer

-- | @since 4.17.0.0
instance Monoid ByteArray where
  mempty = emptyByteArray
  mconcat = concatByteArray

-- | @since 4.17.0.0
instance IsList ByteArray where
  type Item ByteArray = Word8

  toList = byteArrayToList
  fromList xs = byteArrayFromListN (length xs) xs
  fromListN = byteArrayFromListN

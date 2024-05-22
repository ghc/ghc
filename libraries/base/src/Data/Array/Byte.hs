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
{-# LANGUAGE TemplateHaskellQuotes #-}

module Data.Array.Byte (
  ByteArray(..),
  MutableByteArray(..),
) where

import GHC.Internal.Data.Bits ((.&.), unsafeShiftR)
import GHC.Internal.Data.Data (mkNoRepType, Data(..))
import GHC.Internal.Data.Typeable (Typeable)
import qualified GHC.Internal.Data.Foldable as F
import GHC.Internal.Data.Maybe (fromMaybe)
import Data.Semigroup
import GHC.Internal.Exts
import GHC.Num.Integer (Integer(..))
import GHC.Internal.Show (intToDigit)
import GHC.Internal.ST (ST(..), runST)
import GHC.Internal.Word (Word8(..))
import GHC.Internal.TH.Syntax
import GHC.Internal.TH.Lift
import GHC.Internal.ForeignPtr
import Prelude

-- | Lifted wrapper for 'ByteArray#'.
--
-- Since 'ByteArray#' is an unlifted type and not a member of kind 'Data.Kind.Type',
-- things like @[ByteArray#]@ or @IO ByteArray#@ are ill-typed. To work around this
-- inconvenience this module provides a standard lifted wrapper, inhabiting 'Data.Kind.Type'.
-- Clients are expected to use 'ByteArray' in higher-level APIs,
-- but wrap and unwrap 'ByteArray' internally as they please
-- and use functions from "GHC.Exts".
--
-- The memory representation of a 'ByteArray' is:
--
-- > ╭─────────────┬───╮  ╭────────┬──────┬─────────╮
-- > │ Constructor │ * ┼─➤│ Header │ Size │ Payload │
-- > ╰─────────────┴───╯  ╰────────┴──────┴─────────╯
--
-- And its overhead is the following:
--
-- * 'ByteArray' constructor: 1 word
-- * Pointer to 'ByteArray#': 1 word
-- * 'ByteArray#' Header: 1 word
-- * 'ByteArray#' Size: 1 word
--
-- Where a word is the unit of heap allocation,
-- measuring 8 bytes on 64-bit systems, and 4 bytes on 32-bit systems.
--
-- @since 4.17.0.0
data ByteArray = ByteArray ByteArray#

-- | Lifted wrapper for 'MutableByteArray#'.
--
-- Since 'MutableByteArray#' is an unlifted type and not a member of kind 'Data.Kind.Type',
-- things like @[MutableByteArray#]@ or @IO MutableByteArray#@ are ill-typed. To work around this
-- inconvenience this module provides a standard lifted wrapper, inhabiting 'Data.Kind.Type'.
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
byteArrayFromListN n ys
  | n >= 0 = runST $ do
    marr <- newByteArray n
    let go !ix [] = if ix == n
          then return ()
          else errorWithoutStackTrace $ "Data.Array.Byte.byteArrayFromListN: list length less than specified size"
        go !ix (x : xs) = if ix < n
          then do
            writeByteArray marr ix x
            go (ix + 1) xs
          else errorWithoutStackTrace $ "Data.Array.Byte.byteArrayFromListN: list length greater than specified size"
    go 0 ys
    unsafeFreezeByteArray marr
  | otherwise = errorWithoutStackTrace "Data.Array.Byte.ByteArrayFromListN: specified size is negative"

-- | Copy a slice of an immutable byte array to a mutable byte array.
--
-- /Note:/ this function does not do bounds or overlap checking.
unsafeCopyByteArray
  :: MutableByteArray s -- ^ destination array
  -> Int                -- ^ offset into destination array
  -> ByteArray          -- ^ source array
  -> Int                -- ^ offset into source array
  -> Int                -- ^ number of bytes to copy
  -> ST s ()
{-# INLINE unsafeCopyByteArray #-}
unsafeCopyByteArray (MutableByteArray dst#) (I# doff#) (ByteArray src#) (I# soff#) (I# sz#) =
  ST (\s# -> case copyByteArray# src# soff# dst# doff# sz# s# of
    s'# -> (# s'#, () #))

-- | Copy a slice from one mutable byte array to another
-- or to the same mutable byte array.
--
-- /Note:/ this function does not do bounds or overlap checking.
unsafeCopyMutableByteArray
  :: MutableByteArray s -- ^ destination array
  -> Int                -- ^ offset into destination array
  -> MutableByteArray s -- ^ source array
  -> Int                -- ^ offset into source array
  -> Int                -- ^ number of bytes to copy
  -> ST s ()
{-# INLINE unsafeCopyMutableByteArray #-}
unsafeCopyMutableByteArray (MutableByteArray dst#) (I# doff#) (MutableByteArray src#) (I# soff#) (I# sz#) =
  ST (\s# -> case copyMutableByteArrayNonOverlapping# src# soff# dst# doff# sz# s# of
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

instance Lift ByteArray where
  liftTyped = unsafeCodeCoerce . lift
  lift (ByteArray b) =
    [| addrToByteArray $(lift len)
                       $(pure . LitE . BytesPrimL $ Bytes ptr 0 (fromIntegral len))
    |]
    where
      len# = sizeofByteArray# b
      len = I# len#
      pb :: ByteArray#
      !(ByteArray pb)
        | isTrue# (isByteArrayPinned# b) = ByteArray b
        | otherwise = runST $ ST $
          \s -> case newPinnedByteArray# len# s of
            (# s', mb #) -> case copyByteArray# b 0# mb 0# len# s' of
              s'' -> case unsafeFreezeByteArray# mb s'' of
                (# s''', ret #) -> (# s''', ByteArray ret #)
      ptr :: ForeignPtr Word8
      ptr = ForeignPtr (byteArrayContents# pb) (PlainPtr (unsafeCoerce# pb))

addrToByteArray :: Int -> Addr# -> ByteArray
addrToByteArray (I# len) addr = runST $ ST $
  \s -> case newByteArray# len s of
    (# s', mb #) -> case copyAddrToByteArray# addr mb 0# len s' of
      s'' -> case unsafeFreezeByteArray# mb s'' of
        (# s''', ret #) -> (# s''', ByteArray ret #)

-- | Compare prefixes of given length.
compareByteArraysFromBeginning :: ByteArray -> ByteArray -> Int -> Ordering
{-# INLINE compareByteArraysFromBeginning #-}
compareByteArraysFromBeginning (ByteArray ba1#) (ByteArray ba2#) (I# n#)
  = compare (I# (compareByteArrays# ba1# 0# ba2# 0# n#)) 0

-- | Do two byte arrays share the same pointer?
sameByteArray :: ByteArray# -> ByteArray# -> Bool
sameByteArray ba1 ba2 =
    case sameByteArray# ba1 ba2 of r -> isTrue# r

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
--
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
appendByteArray ba1 ba2 = runST $ do
  let n1 = sizeofByteArray ba1
      n2 = sizeofByteArray ba2
      totSz = fromMaybe (sizeOverflowError "appendByteArray")
                        (checkedIntAdd n1 n2)
  marr <- newByteArray totSz
  unsafeCopyByteArray marr 0  ba1 0 n1
  unsafeCopyByteArray marr n1 ba2 0 n2
  unsafeFreezeByteArray marr

-- | Concatenate a list of 'ByteArray's.
concatByteArray :: [ByteArray] -> ByteArray
concatByteArray arrs = runST $ do
  let addLen acc arr = fromMaybe (sizeOverflowError "concatByteArray")
                                 (checkedIntAdd acc (sizeofByteArray arr))
      totLen = F.foldl' addLen 0 arrs
  marr <- newByteArray totLen
  pasteByteArrays marr 0 arrs
  unsafeFreezeByteArray marr

-- | Dump immutable 'ByteArray's into a mutable one, starting from a given offset.
pasteByteArrays :: MutableByteArray s -> Int -> [ByteArray] -> ST s ()
pasteByteArrays !_ !_ [] = return ()
pasteByteArrays !marr !ix (x : xs) = do
  unsafeCopyByteArray marr ix x 0 (sizeofByteArray x)
  pasteByteArrays marr (ix + sizeofByteArray x) xs

-- | An array of zero length.
emptyByteArray :: ByteArray
emptyByteArray = runST (newByteArray 0 >>= unsafeFreezeByteArray)

-- | Concatenates a given number of copies of an input ByteArray.
stimesPolymorphic :: Integral t => t -> ByteArray -> ByteArray
{-# INLINABLE stimesPolymorphic #-}
stimesPolymorphic nRaw !arr = case toInteger nRaw of
  IS nInt#
    | isTrue# (nInt# >#  0#) -> stimesPositiveInt (I# nInt#) arr
    | isTrue# (nInt# >=# 0#) -> emptyByteArray
      -- This check is redundant for unsigned types like Word.
      -- Using >=# intead of ==# may make it easier for GHC to notice that.
    | otherwise -> stimesNegativeErr
  IP _
    | sizeofByteArray arr == 0 -> emptyByteArray
    | otherwise -> stimesOverflowErr
  IN _ -> stimesNegativeErr

stimesNegativeErr :: ByteArray
stimesNegativeErr =
  errorWithoutStackTrace "stimes @ByteArray: negative multiplier"

stimesOverflowErr :: a
stimesOverflowErr = sizeOverflowError "stimes"

stimesPositiveInt :: Int -> ByteArray -> ByteArray
{-# NOINLINE stimesPositiveInt #-}
-- NOINLINE to prevent its duplication in specialisations of stimesPolymorphic
stimesPositiveInt n arr = runST $ do
  let inpSz = sizeofByteArray arr
      tarSz = fromMaybe stimesOverflowErr (checkedIntMultiply n inpSz)
  marr <- newByteArray tarSz
  unsafeCopyByteArray marr 0 arr 0 inpSz
  let
    halfTarSz = (tarSz - 1) `div` 2
    go copied
      | copied <= halfTarSz = do
          unsafeCopyMutableByteArray marr copied marr 0 copied
          go (copied + copied)
      | otherwise = unsafeCopyMutableByteArray marr copied marr 0 (tarSz - copied)
  go inpSz
  unsafeFreezeByteArray marr

-- | @since 4.17.0.0
instance Semigroup ByteArray where
  (<>) = appendByteArray
  sconcat = mconcat . F.toList
  {-# INLINE stimes #-}
  stimes = stimesPolymorphic

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


sizeOverflowError :: String -> a
sizeOverflowError fun
  = errorWithoutStackTrace $ "Data.Array.Byte." ++ fun ++ ": size overflow"


-- TODO: Export these from a better home.

-- | Adds two @Int@s, returning @Nothing@ if this results in an overflow
checkedIntAdd :: Int -> Int -> Maybe Int
checkedIntAdd (I# x#) (I# y#) = case addIntC# x# y# of
  (# res, 0# #) -> Just (I# res)
  _ -> Nothing

-- | Multiplies two @Int@s, returning @Nothing@ if this results in an overflow
checkedIntMultiply :: Int -> Int -> Maybe Int
checkedIntMultiply (I# x#) (I# y#) = case timesInt2# x# y# of
  (# 0#, _hi, lo #) -> Just (I# lo)
  _ -> Nothing

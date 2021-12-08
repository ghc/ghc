{-# LANGUAGE BangPatterns, CPP, MagicHash, RankNTypes, RecordWildCards,
    UnboxedTuples #-}

-- |
-- Module      :  Data.Attoparsec.Text.Buffer
-- Copyright   :  Bryan O'Sullivan 2007-2015
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- An immutable buffer that supports cheap appends.

-- A Buffer is divided into an immutable read-only zone, followed by a
-- mutable area that we've preallocated, but not yet written to.
--
-- We overallocate at the end of a Buffer so that we can cheaply
-- append.  Since a user of an existing Buffer cannot see past the end
-- of its immutable zone into the data that will change during an
-- append, this is safe.
--
-- Once we run out of space at the end of a Buffer, we do the usual
-- doubling of the buffer size.

module Data.Attoparsec.Text.Buffer
    (
      Buffer
    , buffer
    , unbuffer
    , unbufferAt
    , length
    , pappend
    , iter
    , iter_
    , substring
    , lengthCodeUnits
    , dropCodeUnits
    ) where

import Control.Exception (assert)
import Data.Bits (shiftR)
import Data.List (foldl1')
import Data.Monoid as Mon (Monoid(..))
import Data.Text ()
import Data.Text.Internal (Text(..))
#if MIN_VERSION_text(2,0,0)
import Data.Text.Internal.Encoding.Utf8 (utf8LengthByLeader)
import Data.Text.Unsafe (iterArray, lengthWord8)
#else
import Data.Text.Internal.Encoding.Utf16 (chr2)
import Data.Text.Internal.Unsafe.Char (unsafeChr)
import Data.Text.Unsafe (lengthWord16)
#endif
import Data.Text.Unsafe (Iter(..))
import Foreign.Storable (sizeOf)
import GHC.Exts (Int(..), indexIntArray#, unsafeCoerce#, writeIntArray#)
import GHC.ST (ST(..), runST)
import Prelude hiding (length)
import qualified Data.Text.Array as A

-- If _cap is zero, this buffer is empty.
data Buffer = Buf {
      _arr :: {-# UNPACK #-} !A.Array
    , _off :: {-# UNPACK #-} !Int
    , _len :: {-# UNPACK #-} !Int
    , _cap :: {-# UNPACK #-} !Int
    , _gen :: {-# UNPACK #-} !Int
    }

instance Show Buffer where
    showsPrec p = showsPrec p . unbuffer

-- | The initial 'Buffer' has no mutable zone, so we can avoid all
-- copies in the (hopefully) common case of no further input being fed
-- to us.
buffer :: Text -> Buffer
buffer (Text arr off len) = Buf arr off len len 0

unbuffer :: Buffer -> Text
unbuffer (Buf arr off len _ _) = Text arr off len

unbufferAt :: Int -> Buffer -> Text
unbufferAt s (Buf arr off len _ _) =
  assert (s >= 0 && s <= len) $
  Text arr (off+s) (len-s)

instance Semigroup Buffer where
    (Buf _ _ _ 0 _) <> b                     = b
    a               <> (Buf _ _ _ 0 _)       = a
    buf             <> (Buf arr off len _ _) = append buf arr off len
    {-# INLINE (<>) #-}

instance Monoid Buffer where
    mempty = Buf A.empty 0 0 0 0
    {-# INLINE mempty #-}

    mappend = (<>)

    mconcat [] = Mon.mempty
    mconcat xs = foldl1' (<>) xs

pappend :: Buffer -> Text -> Buffer
pappend (Buf _ _ _ 0 _) t      = buffer t
pappend buf (Text arr off len) = append buf arr off len

append :: Buffer -> A.Array -> Int -> Int -> Buffer
append (Buf arr0 off0 len0 cap0 gen0) !arr1 !off1 !len1 = runST $ do
  let woff    = sizeOf (0::Int) `shiftR` 1
      newlen  = len0 + len1
      !gen    = if gen0 == 0 then 0 else readGen arr0
  if gen == gen0 && newlen <= cap0
    then do
      let newgen = gen + 1
      marr <- unsafeThaw arr0
      writeGen marr newgen
#if MIN_VERSION_text(2,0,0)
      A.copyI newlen marr (off0+len0) arr1 off1
#else
      A.copyI marr (off0+len0) arr1 off1 (off0+newlen)
#endif
      arr2 <- A.unsafeFreeze marr
      return (Buf arr2 off0 newlen cap0 newgen)
    else do
      let newcap = newlen * 2
          newgen = 1
      marr <- A.new (newcap + woff)
      writeGen marr newgen
#if MIN_VERSION_text(2,0,0)
      A.copyI len0 marr woff arr0 off0
      A.copyI newlen marr (woff+len0) arr1 off1
#else
      A.copyI marr woff arr0 off0 (woff+len0)
      A.copyI marr (woff+len0) arr1 off1 (woff+newlen)
#endif
      arr2 <- A.unsafeFreeze marr
      return (Buf arr2 woff newlen newcap newgen)

length :: Buffer -> Int
length (Buf _ _ len _ _) = len
{-# INLINE length #-}

substring :: Int -> Int -> Buffer -> Text
substring s l (Buf arr off len _ _) =
  assert (s >= 0 && s <= len) .
  assert (l >= 0 && l <= len-s) $
  Text arr (off+s) l
{-# INLINE substring #-}

#if MIN_VERSION_text(2,0,0)

lengthCodeUnits :: Text -> Int
lengthCodeUnits = lengthWord8

dropCodeUnits :: Int -> Buffer -> Text
dropCodeUnits s (Buf arr off len _ _) =
  assert (s >= 0 && s <= len) $
  Text arr (off+s) (len-s)
{-# INLINE dropCodeUnits #-}

-- | /O(1)/ Iterate (unsafely) one step forwards through a UTF-8
-- array, returning the current character and the delta to add to give
-- the next offset to iterate at.
iter :: Buffer -> Int -> Iter
iter (Buf arr off _ _ _) i = iterArray arr (off + i)
{-# INLINE iter #-}

-- | /O(1)/ Iterate one step through a UTF-8 array, returning the
-- delta to add to give the next offset to iterate at.
iter_ :: Buffer -> Int -> Int
iter_ (Buf arr off _ _ _) i = utf8LengthByLeader $ A.unsafeIndex arr (off+i)
{-# INLINE iter_ #-}

unsafeThaw :: A.Array -> ST s (A.MArray s)
unsafeThaw (A.ByteArray a) = ST $ \s# ->
                          (# s#, A.MutableByteArray (unsafeCoerce# a) #)

readGen :: A.Array -> Int
readGen (A.ByteArray a) = case indexIntArray# a 0# of r# -> I# r#

writeGen :: A.MArray s -> Int -> ST s ()
writeGen (A.MutableByteArray a) (I# gen#) = ST $ \s0# ->
  case writeIntArray# a 0# gen# s0# of
    s1# -> (# s1#, () #)

#else

lengthCodeUnits :: Text -> Int
lengthCodeUnits = lengthWord16

dropCodeUnits :: Int -> Buffer -> Text
dropCodeUnits s (Buf arr off len _ _) =
  assert (s >= 0 && s <= len) $
  Text arr (off+s) (len-s)
{-# INLINE dropCodeUnits #-}

-- | /O(1)/ Iterate (unsafely) one step forwards through a UTF-16
-- array, returning the current character and the delta to add to give
-- the next offset to iterate at.
iter :: Buffer -> Int -> Iter
iter (Buf arr off _ _ _) i
    | m < 0xD800 || m > 0xDBFF = Iter (unsafeChr m) 1
    | otherwise                = Iter (chr2 m n) 2
  where m = A.unsafeIndex arr j
        n = A.unsafeIndex arr k
        j = off + i
        k = j + 1
{-# INLINE iter #-}

-- | /O(1)/ Iterate one step through a UTF-16 array, returning the
-- delta to add to give the next offset to iterate at.
iter_ :: Buffer -> Int -> Int
iter_ (Buf arr off _ _ _) i | m < 0xD800 || m > 0xDBFF = 1
                                | otherwise                = 2
  where m = A.unsafeIndex arr (off+i)
{-# INLINE iter_ #-}

unsafeThaw :: A.Array -> ST s (A.MArray s)
unsafeThaw A.Array{..} = ST $ \s# ->
                          (# s#, A.MArray (unsafeCoerce# aBA) #)

readGen :: A.Array -> Int
readGen a = case indexIntArray# (A.aBA a) 0# of r# -> I# r#

writeGen :: A.MArray s -> Int -> ST s ()
writeGen a (I# gen#) = ST $ \s0# ->
  case writeIntArray# (A.maBA a) 0# gen# s0# of
    s1# -> (# s1#, () #)

#endif

{-# LANGUAGE BangPatterns #-}
-- |
-- Module      :  Data.Attoparsec.ByteString.Buffer
-- Copyright   :  Bryan O'Sullivan 2007-2015
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- An "immutable" buffer that supports cheap appends.
--
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
--
-- The fact of having a mutable buffer really helps with performance,
-- but it does have a consequence: if someone misuses the Partial API
-- that attoparsec uses by calling the same continuation repeatedly
-- (which never makes sense in practice), they could overwrite data.
--
-- Since the API *looks* pure, it should *act* pure, too, so we use
-- two generation counters (one mutable, one immutable) to track the
-- number of appends to a mutable buffer. If the counters ever get out
-- of sync, someone is appending twice to a mutable buffer, so we
-- duplicate the entire buffer in order to preserve the immutability
-- of its older self.
--
-- While we could go a step further and gain protection against API
-- abuse on a multicore system, by use of an atomic increment
-- instruction to bump the mutable generation counter, that would be
-- very expensive, and feels like it would also be in the realm of the
-- ridiculous.  Clients should never call a continuation more than
-- once; we lack a linear type system that could enforce this; and
-- there's only so far we should go to accommodate broken uses.

module Data.Attoparsec.ByteString.Buffer
    (
      Buffer
    , buffer
    , unbuffer
    , pappend
    , length
    , unsafeIndex
    , substring
    , unsafeDrop
    ) where

import Control.Exception (assert)
import Data.ByteString.Internal (ByteString(..), memcpy, nullForeignPtr)
import Data.Attoparsec.Internal.Fhthagn (inlinePerformIO)
import Data.List (foldl1')
import Data.Monoid as Mon (Monoid(..))
import Data.Semigroup (Semigroup(..))
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (peek, peekByteOff, poke, sizeOf)
import GHC.ForeignPtr (mallocPlainForeignPtrBytes)
import Prelude hiding (length)

-- If _cap is zero, this buffer is empty.
data Buffer = Buf {
      _fp  :: {-# UNPACK #-} !(ForeignPtr Word8)
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
buffer :: ByteString -> Buffer
buffer (PS fp off len) = Buf fp off len len 0

unbuffer :: Buffer -> ByteString
unbuffer (Buf fp off len _ _) = PS fp off len

instance Semigroup Buffer where
    (Buf _ _ _ 0 _) <> b                    = b
    a               <> (Buf _ _ _ 0 _)      = a
    buf             <> (Buf fp off len _ _) = append buf fp off len

instance Monoid Buffer where
    mempty = Buf nullForeignPtr 0 0 0 0

    mappend = (<>)

    mconcat [] = Mon.mempty
    mconcat xs = foldl1' mappend xs

pappend :: Buffer -> ByteString -> Buffer
pappend (Buf _ _ _ 0 _) bs  = buffer bs
pappend buf (PS fp off len) = append buf fp off len

append :: Buffer -> ForeignPtr a -> Int -> Int -> Buffer
append (Buf fp0 off0 len0 cap0 gen0) !fp1 !off1 !len1 =
  inlinePerformIO . withForeignPtr fp0 $ \ptr0 ->
    withForeignPtr fp1 $ \ptr1 -> do
      let genSize = sizeOf (0::Int)
          newlen  = len0 + len1
      gen <- if gen0 == 0
             then return 0
             else peek (castPtr ptr0)
      if gen == gen0 && newlen <= cap0
        then do
          let newgen = gen + 1
          poke (castPtr ptr0) newgen
          memcpy (ptr0 `plusPtr` (off0+len0))
                 (ptr1 `plusPtr` off1)
                 (fromIntegral len1)
          return (Buf fp0 off0 newlen cap0 newgen)
        else do
          let newcap = newlen * 2
          fp <- mallocPlainForeignPtrBytes (newcap + genSize)
          withForeignPtr fp $ \ptr_ -> do
            let ptr    = ptr_ `plusPtr` genSize
                newgen = 1
            poke (castPtr ptr_) newgen
            memcpy ptr (ptr0 `plusPtr` off0) (fromIntegral len0)
            memcpy (ptr `plusPtr` len0) (ptr1 `plusPtr` off1)
                   (fromIntegral len1)
            return (Buf fp genSize newlen newcap newgen)

length :: Buffer -> Int
length (Buf _ _ len _ _) = len
{-# INLINE length #-}

unsafeIndex :: Buffer -> Int -> Word8
unsafeIndex (Buf fp off len _ _) i = assert (i >= 0 && i < len) .
    inlinePerformIO . withForeignPtr fp $ flip peekByteOff (off+i)
{-# INLINE unsafeIndex #-}

substring :: Int -> Int -> Buffer -> ByteString
substring s l (Buf fp off len _ _) =
  assert (s >= 0 && s <= len) .
  assert (l >= 0 && l <= len-s) $
  PS fp (off+s) l
{-# INLINE substring #-}

unsafeDrop :: Int -> Buffer -> ByteString
unsafeDrop s (Buf fp off len _ _) =
  assert (s >= 0 && s <= len) $
  PS fp (off+s) (len-s)
{-# INLINE unsafeDrop #-}

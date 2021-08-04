-- |
-- Module      : Data.ByteArray.Methods
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : stable
-- Portability : Good
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
module Data.ByteArray.Methods
    ( alloc
    , allocAndFreeze
    , create
    , unsafeCreate
    , pack
    , unpack
    , uncons
    , empty
    , singleton
    , cons
    , snoc
    , null
    , replicate
    , zero
    , copy
    , take
    , drop
    , span
    , reverse
    , convert
    , copyRet
    , copyAndFreeze
    , splitAt
    , xor
    , index
    , eq
    , constEq
    , any
    , all
    , append
    , concat
    ) where

import           Data.ByteArray.Types
import           Data.Memory.Internal.Compat
import           Data.Memory.Internal.Imports hiding (empty)
import           Data.Memory.PtrMethods
import           Data.Monoid
import           Foreign.Storable
import           Foreign.Ptr

import           Prelude hiding (length, take, drop, span, reverse, concat, replicate, splitAt, null, pred, last, any, all)
import qualified Prelude

#if defined(WITH_BYTESTRING_SUPPORT) && defined(WITH_BASEMENT_SUPPORT)
import qualified Data.ByteString as SPE (ByteString)
import qualified Basement.UArray as SPE (UArray)
import qualified Basement.Block  as SPE (Block)
#endif

-- | Allocate a new bytearray of specific size, and run the initializer on this memory
alloc :: ByteArray ba => Int -> (Ptr p -> IO ()) -> IO ba
alloc n f
    | n < 0     = alloc 0 f
    | otherwise = snd `fmap` allocRet n f
{-# INLINE alloc #-}

-- | Allocate a new bytearray of specific size, and run the initializer on this memory
create :: ByteArray ba => Int -> (Ptr p -> IO ()) -> IO ba
create n f = alloc n f

-- | similar to 'alloc' but hide the allocation and initializer in a pure context
allocAndFreeze :: ByteArray a => Int -> (Ptr p -> IO ()) -> a
allocAndFreeze sz f = unsafeDoIO (alloc sz f)
{-# NOINLINE allocAndFreeze #-}

-- | similar to 'create' but hide the allocation and initializer in a pure context
unsafeCreate :: ByteArray a => Int -> (Ptr p -> IO ()) -> a
unsafeCreate sz f = unsafeDoIO (alloc sz f)
{-# NOINLINE unsafeCreate #-}

inlineUnsafeCreate :: ByteArray a => Int -> (Ptr p -> IO ()) -> a
inlineUnsafeCreate !sz f = unsafeDoIO (alloc sz f)
{-# INLINE inlineUnsafeCreate #-}

-- | Create an empty byte array
empty :: ByteArray a => a
empty = unsafeDoIO (alloc 0 $ \_ -> return ())

-- | Check if a byte array is empty
null :: ByteArrayAccess a => a -> Bool
null b = length b == 0

-- | Pack a list of bytes into a bytearray
pack :: ByteArray a => [Word8] -> a
pack l = inlineUnsafeCreate (Prelude.length l) (fill l)
  where fill []     _  = return ()
        fill (x:xs) !p = poke p x >> fill xs (p `plusPtr` 1)
        {-# INLINE fill #-}
{-# NOINLINE pack #-}

-- | Un-pack a bytearray into a list of bytes
unpack :: ByteArrayAccess a => a -> [Word8]
unpack bs = loop 0
  where !len = length bs
        loop i
            | i == len  = []
            | otherwise =
                let !v = unsafeDoIO $ withByteArray bs (\p -> peekByteOff p i)
                 in v : loop (i+1)

-- | returns the first byte, and the remaining bytearray if the bytearray is not null
uncons :: ByteArray a => a -> Maybe (Word8, a)
uncons a
    | null a    = Nothing
    | otherwise = Just (index a 0, drop 1 a)

-- | Create a byte array from a single byte
singleton :: ByteArray a => Word8 -> a
singleton b = unsafeCreate 1 (\p -> pokeByteOff p 0 b)

-- | prepend a single byte to a byte array
cons :: ByteArray a => Word8 -> a -> a
cons b ba = unsafeCreate (len + 1) $ \d -> withByteArray ba $ \s -> do
    pokeByteOff d 0 b
    memCopy (d `plusPtr` 1) s len
  where len = length ba

-- | append a single byte to a byte array
snoc :: ByteArray a =>  a -> Word8 -> a
snoc ba b = unsafeCreate (len + 1) $ \d -> withByteArray ba $ \s -> do
    memCopy d s len
    pokeByteOff d len b
  where len = length ba

-- | Create a xor of bytes between a and b.
--
-- the returns byte array is the size of the smallest input.
xor :: (ByteArrayAccess a, ByteArrayAccess b, ByteArray c) => a -> b -> c
xor a b =
    unsafeCreate n $ \pc ->
    withByteArray a  $ \pa ->
    withByteArray b  $ \pb ->
        memXor pc pa pb n
  where
        n  = min la lb
        la = length a
        lb = length b

-- | return a specific byte indexed by a number from 0 in a bytearray
--
-- unsafe, no bound checking are done
index :: ByteArrayAccess a => a -> Int -> Word8
index b i = unsafeDoIO $ withByteArray b $ \p -> peek (p `plusPtr` i)

-- | Split a bytearray at a specific length in two bytearray
splitAt :: ByteArray bs => Int -> bs -> (bs, bs)
splitAt n bs
    | n <= 0    = (empty, bs)
    | n >= len  = (bs, empty)
    | otherwise = unsafeDoIO $ do
        withByteArray bs $ \p -> do
            b1 <- alloc n $ \r -> memCopy r p n
            b2 <- alloc (len - n) $ \r -> memCopy r (p `plusPtr` n) (len - n)
            return (b1, b2)
  where len = length bs

-- | Take the first @n@ byte of a bytearray
take :: ByteArray bs => Int -> bs -> bs
take n bs
    | n <= 0    = empty
    | otherwise = unsafeCreate m $ \d -> withByteArray bs $ \s -> memCopy d s m
  where
    !m   = min len n
    !len = length bs

-- | drop the first @n@ byte of a bytearray
drop :: ByteArray bs => Int -> bs -> bs
drop n bs
    | n <= 0    = bs
    | nb == 0   = empty
    | otherwise = unsafeCreate nb $ \d -> withByteArray bs $ \s -> memCopy d (s `plusPtr` ofs) nb
  where
    ofs = min len n
    nb  = len - ofs
    len = length bs

-- | Split a bytearray at the point where @pred@ becomes invalid
span :: ByteArray bs => (Word8 -> Bool) -> bs -> (bs, bs)
span pred bs
    | null bs   = (bs, bs)
    | otherwise = let n = loop 0 in (take n bs, drop n bs)
  where loop !i
            | i >= len          = len
            | pred (index bs i) = loop (i+1)
            | otherwise         = i
        len = length bs

-- | Reverse a bytearray
reverse :: ByteArray bs => bs -> bs
reverse bs = unsafeCreate n $ \d -> withByteArray bs $ \s -> memReverse d s n
  where n = length bs

-- | Concatenate bytearray into a larger bytearray
concat :: (ByteArrayAccess bin, ByteArray bout) => [bin] -> bout
concat l = unsafeCreate retLen (loopCopy l)
  where
    retLen = sum $ map length l

    loopCopy []     _   = return ()
    loopCopy (x:xs) dst = do
        copyByteArrayToPtr x dst
        loopCopy xs (dst `plusPtr` chunkLen)
      where
        !chunkLen = length x

-- | append one bytearray to the other
append :: ByteArray bs => bs -> bs -> bs
append = mappend

-- | Duplicate a bytearray into another bytearray, and run an initializer on it
copy :: (ByteArrayAccess bs1, ByteArray bs2) => bs1 -> (Ptr p -> IO ()) -> IO bs2
copy bs f =
    alloc (length bs) $ \d -> do
        copyByteArrayToPtr bs d
        f (castPtr d)

-- | Similar to 'copy' but also provide a way to return a value from the initializer
copyRet :: (ByteArrayAccess bs1, ByteArray bs2) => bs1 -> (Ptr p -> IO a) -> IO (a, bs2)
copyRet bs f =
    allocRet (length bs) $ \d -> do
        copyByteArrayToPtr bs d
        f (castPtr d)

-- | Similiar to 'copy' but expect the resulting bytearray in a pure context
copyAndFreeze :: (ByteArrayAccess bs1, ByteArray bs2) => bs1 -> (Ptr p -> IO ()) -> bs2
copyAndFreeze bs f =
    inlineUnsafeCreate (length bs) $ \d -> do
        copyByteArrayToPtr bs d
        f (castPtr d)
{-# NOINLINE copyAndFreeze #-}

-- | Create a bytearray of a specific size containing a repeated byte value
replicate :: ByteArray ba => Int -> Word8 -> ba
replicate 0 _ = empty
replicate n b
    | n < 0     = empty
    | otherwise = inlineUnsafeCreate n $ \ptr -> memSet ptr b n
{-# NOINLINE replicate #-}

-- | Create a bytearray of a specific size initialized to 0
zero :: ByteArray ba => Int -> ba
zero 0 = empty
zero n
    | n < 0     = empty
    | otherwise = unsafeCreate n $ \ptr -> memSet ptr 0 n
{-# NOINLINE zero #-}

-- | Check if two bytearray are equals
--
-- This is not constant time, as soon some byte differs the function will
-- returns. use 'constEq' in sensitive context where timing matters.
eq :: (ByteArrayAccess bs1, ByteArrayAccess bs2) => bs1 -> bs2 -> Bool
eq b1 b2
    | l1 /= l2  = False
    | otherwise = unsafeDoIO $ withByteArray b1 $ \p1 -> withByteArray b2 $ \p2 -> memEqual p1 p2 l1
  where
    l1 = length b1
    l2 = length b2

-- | A constant time equality test for 2 ByteArrayAccess values.
--
-- If values are of 2 different sizes, the function will abort early
-- without comparing any bytes.
--
-- compared to == , this function will go over all the bytes
-- present before yielding a result even when knowing the
-- overall result early in the processing.
constEq :: (ByteArrayAccess bs1, ByteArrayAccess bs2) => bs1 -> bs2 -> Bool
constEq b1 b2
    | l1 /= l2  = False
    | otherwise = unsafeDoIO $ withByteArray b1 $ \p1 -> withByteArray b2 $ \p2 -> memConstEqual p1 p2 l1
  where
    !l1 = length b1
    !l2 = length b2

-- | Check if any element of a byte array satisfies a predicate
any :: (ByteArrayAccess ba) => (Word8 -> Bool) -> ba -> Bool
any f b
    | null b    = False
    | otherwise = unsafeDoIO $ withByteArray b $ \p -> loop p 0
  where
    len = length b
    loop p i
        | i == len  = return False
        | otherwise = do
            w <- peekByteOff p i
            if f w then return True else loop p (i+1)

-- | Check if all elements of a byte array satisfy a predicate
all :: (ByteArrayAccess ba) => (Word8 -> Bool) -> ba -> Bool
all f b = not (any (not . f) b)

-- | Convert a bytearray to another type of bytearray
convert :: (ByteArrayAccess bin, ByteArray bout) => bin -> bout
convert bs = inlineUnsafeCreate (length bs) (copyByteArrayToPtr bs)
#if defined(WITH_BYTESTRING_SUPPORT) && defined(WITH_BASEMENT_SUPPORT)
{-# SPECIALIZE convert :: SPE.ByteString -> SPE.UArray Word8 #-}
{-# SPECIALIZE convert :: SPE.UArray Word8 -> SPE.ByteString #-}
{-# SPECIALIZE convert :: SPE.ByteString -> SPE.Block Word8 #-}
{-# SPECIALIZE convert :: SPE.Block Word8 -> SPE.ByteString #-}
#endif

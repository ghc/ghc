-- |
-- Module      : Data.ByteArray.Sized
-- License     : BSD-style
-- Maintainer  : Nicolas Di Prima <nicolas@primetype.co.uk>
-- Stability   : stable
-- Portability : Good
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE NoStarIsType #-}
#endif

module Data.ByteArray.Sized
    ( ByteArrayN(..)
    , SizedByteArray
    , unSizedByteArray
    , sizedByteArray
    , unsafeSizedByteArray

    , -- * ByteArrayN operators
      alloc
    , create
    , allocAndFreeze
    , unsafeCreate
    , inlineUnsafeCreate
    , empty
    , pack
    , unpack
    , cons
    , snoc
    , xor
    , index
    , splitAt
    , take
    , drop
    , append
    , copy
    , copyRet
    , copyAndFreeze
    , replicate
    , zero
    , convert
    , fromByteArrayAccess
    , unsafeFromByteArrayAccess
    ) where

import Basement.Imports
import Basement.NormalForm
import Basement.Nat
import Basement.Numerical.Additive ((+))
import Basement.Numerical.Subtractive ((-))

import Basement.Sized.List (ListN, unListN, toListN)

import           Foreign.Storable
import           Foreign.Ptr
import           Data.Maybe (fromMaybe)

import           Data.Memory.Internal.Compat
import           Data.Memory.PtrMethods

import Data.Proxy (Proxy(..))

import Data.ByteArray.Types (ByteArrayAccess(..), ByteArray)
import qualified Data.ByteArray.Types as ByteArray (allocRet)

#if MIN_VERSION_basement(0,0,7)
import           Basement.BlockN (BlockN)
import qualified Basement.BlockN as BlockN
import qualified Basement.PrimType as Base
import           Basement.Types.OffsetSize (Countable)
#endif

-- | Type class to emulate exactly the behaviour of 'ByteArray' but with
-- a known length at compile time
--
class (ByteArrayAccess c, KnownNat n) => ByteArrayN (n :: Nat) c | c -> n where
    -- | just like 'allocRet' but with the size at the type level
    allocRet :: forall p a
              . Proxy n
             -> (Ptr p -> IO a)
             -> IO (a, c)

-- | Wrapper around any collection type with the size as type parameter
--
newtype SizedByteArray (n :: Nat) ba = SizedByteArray { unSizedByteArray :: ba }
  deriving (Eq, Show, Typeable, Ord, NormalForm)

-- | create a 'SizedByteArray' from the given 'ByteArrayAccess' if the
-- size is the same as the target size.
--
sizedByteArray :: forall n ba . (KnownNat n, ByteArrayAccess ba)
               => ba
               -> Maybe (SizedByteArray n ba)
sizedByteArray ba
    | length ba == n = Just $ SizedByteArray ba
    | otherwise      = Nothing
  where
    n = fromInteger $ natVal (Proxy @n)

-- | just like the 'sizedByteArray' function but throw an exception if
-- the size is invalid.
unsafeSizedByteArray :: forall n ba . (ByteArrayAccess ba, KnownNat n) => ba -> SizedByteArray n ba
unsafeSizedByteArray = fromMaybe (error "The size is invalid") . sizedByteArray

instance (ByteArrayAccess ba, KnownNat n) => ByteArrayAccess (SizedByteArray n ba) where
    length _ = fromInteger $ natVal (Proxy @n)
    withByteArray (SizedByteArray ba) = withByteArray ba

instance (KnownNat n, ByteArray ba) => ByteArrayN n (SizedByteArray n ba) where
    allocRet p f = do
        (a, ba) <- ByteArray.allocRet n f
        pure (a, SizedByteArray ba)
      where
        n = fromInteger $ natVal p

#if MIN_VERSION_basement(0,0,7)
instance ( ByteArrayAccess (BlockN n ty)
         , PrimType ty
         , KnownNat n
         , Countable ty n
         , KnownNat nbytes
         , nbytes ~ (Base.PrimSize ty * n)
         ) => ByteArrayN nbytes (BlockN n ty) where
    allocRet _ f = do
        mba <- BlockN.new @n
        a   <- BlockN.withMutablePtrHint True False mba (f . castPtr)
        ba  <- BlockN.freeze mba
        return (a, ba)
#endif


-- | Allocate a new bytearray of specific size, and run the initializer on this memory
alloc :: forall n ba p . (ByteArrayN n ba, KnownNat n)
      => (Ptr p -> IO ())
      -> IO ba
alloc f = snd <$> allocRet (Proxy @n) f

-- | Allocate a new bytearray of specific size, and run the initializer on this memory
create :: forall n ba p . (ByteArrayN n ba, KnownNat n)
       => (Ptr p -> IO ())
       -> IO ba
create = alloc @n
{-# NOINLINE create #-}

-- | similar to 'allocN' but hide the allocation and initializer in a pure context
allocAndFreeze :: forall n ba p . (ByteArrayN n ba, KnownNat n)
               => (Ptr p -> IO ()) -> ba
allocAndFreeze f = unsafeDoIO (alloc @n f)
{-# NOINLINE allocAndFreeze #-}

-- | similar to 'createN' but hide the allocation and initializer in a pure context
unsafeCreate :: forall n ba p . (ByteArrayN n ba, KnownNat n)
             => (Ptr p -> IO ()) -> ba
unsafeCreate f = unsafeDoIO (alloc @n f)
{-# NOINLINE unsafeCreate #-}

inlineUnsafeCreate :: forall n ba p . (ByteArrayN n ba, KnownNat n)
                   => (Ptr p -> IO ()) -> ba
inlineUnsafeCreate f = unsafeDoIO (alloc @n f)
{-# INLINE inlineUnsafeCreate #-}

-- | Create an empty byte array
empty :: forall ba . ByteArrayN 0 ba => ba
empty = unsafeDoIO (alloc @0 $ \_ -> return ())

-- | Pack a list of bytes into a bytearray
pack :: forall n ba . (ByteArrayN n ba, KnownNat n) => ListN n Word8 -> ba
pack l = inlineUnsafeCreate @n (fill $ unListN l)
  where fill []     _  = return ()
        fill (x:xs) !p = poke p x >> fill xs (p `plusPtr` 1)
        {-# INLINE fill #-}
{-# NOINLINE pack #-}

-- | Un-pack a bytearray into a list of bytes
unpack :: forall n ba
        . (ByteArrayN n ba, KnownNat n, NatWithinBound Int n, ByteArrayAccess ba)
       => ba -> ListN n Word8
unpack bs =  fromMaybe (error "the impossible appened") $ toListN @n $ loop 0
  where !len = length bs
        loop i
            | i == len  = []
            | otherwise =
                let !v = unsafeDoIO $ withByteArray bs (`peekByteOff` i)
                 in v : loop (i+1)

-- | prepend a single byte to a byte array
cons :: forall ni no bi bo
      . ( ByteArrayN ni bi, ByteArrayN no bo, ByteArrayAccess bi
        , KnownNat ni, KnownNat no
        , (ni + 1) ~ no
        )
     => Word8 -> bi -> bo
cons b ba = unsafeCreate @no $ \d -> withByteArray ba $ \s -> do
    pokeByteOff d 0 b
    memCopy (d `plusPtr` 1) s len
  where
    !len = fromInteger $ natVal (Proxy @ni)

-- | append a single byte to a byte array
snoc :: forall bi bo ni no
      . ( ByteArrayN ni bi, ByteArrayN no bo, ByteArrayAccess bi
        , KnownNat ni, KnownNat no
        , (ni + 1) ~ no
        )
     => bi -> Word8 -> bo
snoc ba b = unsafeCreate @no $ \d -> withByteArray ba $ \s -> do
    memCopy d s len
    pokeByteOff d len b
  where
    !len = fromInteger $ natVal (Proxy @ni)

-- | Create a xor of bytes between a and b.
--
-- the returns byte array is the size of the smallest input.
xor :: forall n a b c
     . ( ByteArrayN n a, ByteArrayN n b, ByteArrayN n c
       , ByteArrayAccess a, ByteArrayAccess b
       , KnownNat n
       )
    => a -> b -> c
xor a b =
    unsafeCreate @n $ \pc ->
    withByteArray a  $ \pa ->
    withByteArray b  $ \pb ->
        memXor pc pa pb n
  where
    n  = fromInteger (natVal (Proxy @n))

-- | return a specific byte indexed by a number from 0 in a bytearray
--
-- unsafe, no bound checking are done
index :: forall n na ba
       . ( ByteArrayN na ba, ByteArrayAccess ba
         , KnownNat na, KnownNat n
         , n <= na
         )
      => ba -> Proxy n -> Word8
index b pi = unsafeDoIO $ withByteArray b $ \p -> peek (p `plusPtr` i)
  where
    i = fromInteger $ natVal pi

-- | Split a bytearray at a specific length in two bytearray
splitAt :: forall nblhs nbi nbrhs bi blhs brhs
         . ( ByteArrayN nbi bi, ByteArrayN nblhs blhs, ByteArrayN nbrhs brhs
           , ByteArrayAccess bi
           , KnownNat nbi, KnownNat nblhs, KnownNat nbrhs
           , nblhs <= nbi, (nbrhs + nblhs) ~ nbi
           )
        => bi -> (blhs, brhs)
splitAt bs = unsafeDoIO $
    withByteArray bs $ \p -> do
        b1 <- alloc @nblhs $ \r -> memCopy r p n
        b2 <- alloc @nbrhs $ \r -> memCopy r (p `plusPtr` n) (len - n)
        return (b1, b2)
  where
    n = fromInteger $ natVal (Proxy @nblhs)
    len = length bs

-- | Take the first @n@ byte of a bytearray
take :: forall nbo nbi bi bo
      . ( ByteArrayN nbi bi, ByteArrayN nbo bo
        , ByteArrayAccess bi
        , KnownNat nbi, KnownNat nbo
        , nbo <= nbi
        )
     => bi -> bo
take bs = unsafeCreate @nbo $ \d -> withByteArray bs $ \s -> memCopy d s m
  where
    !m   = min len n
    !len = length bs
    !n   = fromInteger $ natVal (Proxy @nbo)

-- | drop the first @n@ byte of a bytearray
drop :: forall n nbi nbo bi bo
      . ( ByteArrayN nbi bi, ByteArrayN nbo bo
        , ByteArrayAccess bi
        , KnownNat n, KnownNat nbi, KnownNat nbo
        , (nbo + n) ~ nbi
        )
     => Proxy n -> bi -> bo
drop pn bs = unsafeCreate @nbo $ \d ->
    withByteArray bs $ \s ->
    memCopy d (s `plusPtr` ofs) nb
  where
    ofs = min len n
    nb  = len - ofs
    len = length bs
    n   = fromInteger $ natVal pn

-- | append one bytearray to the other
append :: forall nblhs nbrhs nbout blhs brhs bout
        . ( ByteArrayN nblhs blhs, ByteArrayN nbrhs brhs, ByteArrayN nbout bout
          , ByteArrayAccess blhs, ByteArrayAccess brhs
          , KnownNat nblhs, KnownNat nbrhs, KnownNat nbout
          , (nbrhs + nblhs) ~ nbout
          )
       => blhs -> brhs -> bout
append blhs brhs = unsafeCreate @nbout $ \p ->
    withByteArray blhs $ \plhs ->
    withByteArray brhs $ \prhs -> do
        memCopy p plhs (length blhs)
        memCopy (p `plusPtr` length blhs) prhs (length brhs)

-- | Duplicate a bytearray into another bytearray, and run an initializer on it
copy :: forall n bs1 bs2 p
      . ( ByteArrayN n bs1, ByteArrayN n bs2
        , ByteArrayAccess bs1
        , KnownNat n
        )
     => bs1 -> (Ptr p -> IO ()) -> IO bs2
copy bs f = alloc @n $ \d -> do
    withByteArray bs $ \s -> memCopy d s (length bs)
    f (castPtr d)

-- | Similar to 'copy' but also provide a way to return a value from the initializer
copyRet :: forall n bs1 bs2 p a
         . ( ByteArrayN n bs1, ByteArrayN n bs2
           , ByteArrayAccess bs1
           , KnownNat n
           )
        => bs1 -> (Ptr p -> IO a) -> IO (a, bs2)
copyRet bs f =
    allocRet (Proxy @n) $ \d -> do
        withByteArray bs $ \s -> memCopy d s (length bs)
        f (castPtr d)

-- | Similiar to 'copy' but expect the resulting bytearray in a pure context
copyAndFreeze :: forall n bs1 bs2 p
               . ( ByteArrayN n bs1, ByteArrayN n bs2
                 , ByteArrayAccess bs1
                 , KnownNat n
                 )
              => bs1 -> (Ptr p -> IO ()) -> bs2
copyAndFreeze bs f =
    inlineUnsafeCreate @n $ \d -> do
        copyByteArrayToPtr bs d
        f (castPtr d)
{-# NOINLINE copyAndFreeze #-}

-- | Create a bytearray of a specific size containing a repeated byte value
replicate :: forall n ba . (ByteArrayN n ba, KnownNat n)
          => Word8 -> ba
replicate b = inlineUnsafeCreate @n $ \ptr -> memSet ptr b (fromInteger $ natVal $ Proxy @n)
{-# NOINLINE replicate #-}

-- | Create a bytearray of a specific size initialized to 0
zero :: forall n ba . (ByteArrayN n ba, KnownNat n) => ba
zero = unsafeCreate @n $ \ptr -> memSet ptr 0 (fromInteger $ natVal $ Proxy @n)
{-# NOINLINE zero #-}

-- | Convert a bytearray to another type of bytearray
convert :: forall n bin bout
         . ( ByteArrayN n bin, ByteArrayN n bout
           , KnownNat n
           )
        => bin -> bout
convert bs = inlineUnsafeCreate @n (copyByteArrayToPtr bs)

-- | Convert a ByteArrayAccess to another type of bytearray
--
-- This function returns nothing if the size is not compatible
fromByteArrayAccess :: forall n bin bout
                     . ( ByteArrayAccess bin, ByteArrayN n bout
                       , KnownNat n
                       )
                    => bin -> Maybe bout
fromByteArrayAccess bs
    | l == n    = Just $ inlineUnsafeCreate @n (copyByteArrayToPtr bs)
    | otherwise = Nothing
  where
    l = length bs
    n = fromInteger $ natVal (Proxy @n)

-- | Convert a ByteArrayAccess to another type of bytearray
unsafeFromByteArrayAccess :: forall n bin bout
                           . ( ByteArrayAccess bin, ByteArrayN n bout
                             , KnownNat n
                           )
                          => bin -> bout
unsafeFromByteArrayAccess bs = case fromByteArrayAccess @n @bin @bout bs of
    Nothing -> error "Invalid Size"
    Just v  -> v

-- Module      : Basement.PrimType
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module Basement.PrimType
    ( PrimType(..)
    , PrimMemoryComparable
    , primBaIndex
    , primMbaRead
    , primMbaWrite
    , primArrayIndex
    , primMutableArrayRead
    , primMutableArrayWrite
    , primOffsetOfE
    , primOffsetRecast
    , sizeRecast
    , offsetAsSize
    , sizeAsOffset
    , sizeInBytes
    , offsetInBytes
    , offsetInElements
    , offsetIsAligned
    , primWordGetByteAndShift
    , primWord64GetByteAndShift
    , primWord64GetHiLo
    ) where

#include "MachDeps.h"

import           GHC.Prim
import           GHC.Int
import           GHC.Types
import           GHC.Word
import           Data.Bits
import           Data.Proxy
import           Basement.Compat.Base
import           Basement.Compat.C.Types
import           Basement.Numerical.Subtractive
import           Basement.Types.OffsetSize
import           Basement.Types.Char7 (Char7(..))
import           Basement.Endianness
import           Basement.Types.Word128 (Word128(..))
import           Basement.Types.Word256 (Word256(..))
import           Basement.Monad
import           Basement.Nat
import qualified Prelude (quot)

#if WORD_SIZE_IN_BITS < 64
import           GHC.IntWord64
#endif

#ifdef FOUNDATION_BOUNDS_CHECK

divBytes :: PrimType ty => Offset ty -> (Int -> Int)
divBytes ofs = \x -> x `Prelude.quot` (getSize Proxy ofs)
  where
    getSize :: PrimType ty => Proxy ty -> Offset ty -> Int
    getSize p _ = let (CountOf sz) = primSizeInBytes p in sz

baLength :: PrimType ty => Offset ty -> ByteArray# -> Int
baLength ofs ba = divBytes ofs (I# (sizeofByteArray# ba))

mbaLength :: PrimType ty => Offset ty -> MutableByteArray# st -> Int
mbaLength ofs ba = divBytes ofs (I# (sizeofMutableByteArray# ba))

aLength :: Array# ty -> Int
aLength ba = I# (sizeofArray# ba)

maLength :: MutableArray# st ty -> Int
maLength ba = I# (sizeofMutableArray# ba)

boundCheckError :: [Char] -> Offset ty -> Int -> a
boundCheckError ty (Offset ofs) len =
    error (ty <> " offset=" <> show ofs <> " len=" <> show len)

baCheck :: PrimType ty => ByteArray# -> Offset ty -> Bool
baCheck ba ofs@(Offset o) = o < 0 || o >= baLength ofs ba

mbaCheck :: PrimType ty => MutableByteArray# st -> Offset ty -> Bool
mbaCheck mba ofs@(Offset o) = o < 0 || o >= mbaLength ofs mba

aCheck :: Array# ty -> Offset ty -> Bool
aCheck ba (Offset o) = o < 0 || o >= aLength ba

maCheck :: MutableArray# st ty -> Offset ty -> Bool
maCheck ma (Offset o) = o < 0 || o >= maLength ma

primBaIndex :: PrimType ty => ByteArray# -> Offset ty -> ty
primBaIndex ba ofs
    | baCheck ba ofs = boundCheckError "bytearray-index" ofs (baLength ofs ba)
    | otherwise      = primBaUIndex ba ofs
{-# NOINLINE primBaIndex #-}

primMbaRead :: (PrimType ty, PrimMonad prim) => MutableByteArray# (PrimState prim) -> Offset ty -> prim ty
primMbaRead mba ofs
    | mbaCheck mba ofs = boundCheckError "mutablebytearray-read" ofs (mbaLength ofs mba)
    | otherwise        = primMbaURead mba ofs
{-# NOINLINE primMbaRead #-}

primMbaWrite :: (PrimType ty, PrimMonad prim) => MutableByteArray# (PrimState prim) -> Offset ty -> ty -> prim ()
primMbaWrite mba ofs ty
    | mbaCheck mba ofs = boundCheckError "mutablebytearray-write" ofs (mbaLength ofs mba)
    | otherwise        = primMbaUWrite mba ofs ty
{-# NOINLINE primMbaWrite #-}

primArrayIndex :: Array# ty -> Offset ty -> ty
primArrayIndex a o@(Offset (I# ofs))
    | aCheck a o = boundCheckError "array-index" o (aLength a)
    | otherwise  = let !(# v #) = indexArray# a ofs in v
{-# NOINLINE primArrayIndex #-}

primMutableArrayRead :: PrimMonad prim => MutableArray# (PrimState prim) ty -> Offset ty -> prim ty
primMutableArrayRead ma o@(Offset (I# ofs))
    | maCheck ma o = boundCheckError "array-read" o (maLength ma)
    | otherwise    = primitive $ \s1 -> readArray# ma ofs s1
{-# NOINLINE primMutableArrayRead #-}

primMutableArrayWrite :: PrimMonad prim => MutableArray# (PrimState prim) ty -> Offset ty -> ty -> prim ()
primMutableArrayWrite ma o@(Offset (I# ofs)) v
    | maCheck ma o = boundCheckError "array-write" o (maLength ma)
    | otherwise    = primitive $ \s1 -> let !s2 = writeArray# ma ofs v s1 in (# s2, () #)
{-# NOINLINE primMutableArrayWrite #-}

#else

primBaIndex :: PrimType ty => ByteArray# -> Offset ty -> ty
primBaIndex = primBaUIndex
{-# INLINE primBaIndex #-}

primMbaRead :: (PrimType ty, PrimMonad prim) => MutableByteArray# (PrimState prim) -> Offset ty -> prim ty
primMbaRead = primMbaURead
{-# INLINE primMbaRead #-}

primMbaWrite :: (PrimType ty, PrimMonad prim) => MutableByteArray# (PrimState prim) -> Offset ty -> ty -> prim ()
primMbaWrite = primMbaUWrite
{-# INLINE primMbaWrite #-}

primArrayIndex :: Array# ty -> Offset ty -> ty
primArrayIndex a (Offset (I# ofs)) = let !(# v #) = indexArray# a ofs in v
{-# INLINE primArrayIndex #-}

primMutableArrayRead :: PrimMonad prim => MutableArray# (PrimState prim) ty -> Offset ty -> prim ty
primMutableArrayRead ma (Offset (I# ofs)) = primitive $ \s1 -> readArray# ma ofs s1
{-# INLINE primMutableArrayRead #-}

primMutableArrayWrite :: PrimMonad prim => MutableArray# (PrimState prim) ty -> Offset ty -> ty -> prim ()
primMutableArrayWrite ma (Offset (I# ofs)) v =
    primitive $ \s1 -> let !s2 = writeArray# ma ofs v s1 in (# s2, () #)
{-# INLINE primMutableArrayWrite #-}

#endif

-- | Represent the accessor for types that can be stored in the UArray and MUArray.
--
-- Types need to be a instance of storable and have fixed sized.
class Eq ty => PrimType ty where
    -- | type level size of the given `ty`
    type PrimSize ty :: Nat

    -- | get the size in bytes of a ty element
    primSizeInBytes :: Proxy ty -> CountOf Word8

    -- | get the shift size
    primShiftToBytes :: Proxy ty -> Int

    -----
    -- ByteArray section
    -----

    -- | return the element stored at a specific index
    primBaUIndex :: ByteArray# -> Offset ty -> ty

    -----
    -- MutableByteArray section
    -----

    -- | Read an element at an index in a mutable array
    primMbaURead :: PrimMonad prim
                => MutableByteArray# (PrimState prim) -- ^ mutable array to read from
                -> Offset ty                         -- ^ index of the element to retrieve
                -> prim ty                           -- ^ the element returned

    -- | Write an element to a specific cell in a mutable array.
    primMbaUWrite :: PrimMonad prim
                 => MutableByteArray# (PrimState prim) -- ^ mutable array to modify
                 -> Offset ty                         -- ^ index of the element to modify
                 -> ty                                 -- ^ the new value to store
                 -> prim ()

    -----
    -- Addr# section
    -----

    -- | Read from Address, without a state. the value read should be considered a constant for all
    -- pratical purpose, otherwise bad thing will happens.
    primAddrIndex :: Addr# -> Offset ty -> ty

    -- | Read a value from Addr in a specific primitive monad
    primAddrRead :: PrimMonad prim
                 => Addr#
                 -> Offset ty
                 -> prim ty
    -- | Write a value to Addr in a specific primitive monad
    primAddrWrite :: PrimMonad prim
                  => Addr#
                  -> Offset ty
                  -> ty
                  -> prim ()

sizeInt, sizeWord :: CountOf Word8
shiftInt, shiftWord :: Int
#if WORD_SIZE_IN_BITS == 64
sizeInt = CountOf 8
sizeWord = CountOf 8
shiftInt = 3
shiftWord = 3
#else
sizeInt = CountOf 4
sizeWord = CountOf 4
shiftInt = 2
shiftWord = 2
#endif

{-# SPECIALIZE [3] primBaUIndex :: ByteArray# -> Offset Word8 -> Word8 #-}

instance PrimType Int where
#if WORD_SIZE_IN_BITS == 64
    type PrimSize Int = 8
#else
    type PrimSize Int = 4
#endif
    primSizeInBytes _ = sizeInt
    {-# INLINE primSizeInBytes #-}
    primShiftToBytes _ = shiftInt
    {-# INLINE primShiftToBytes #-}
    primBaUIndex ba (Offset (I# n)) = I# (indexIntArray# ba n)
    {-# INLINE primBaUIndex #-}
    primMbaURead mba (Offset (I# n)) = primitive $ \s1 -> let !(# s2, r #) = readIntArray# mba n s1 in (# s2, I# r #)
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba (Offset (I# n)) (I# w) = primitive $ \s1 -> (# writeIntArray# mba n w s1, () #)
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr (Offset (I# n)) = I# (indexIntOffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let !(# s2, r #) = readIntOffAddr# addr n s1 in (# s2, I# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (I# w) = primitive $ \s1 -> (# writeIntOffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}

instance PrimType Word where
#if WORD_SIZE_IN_BITS == 64
    type PrimSize Word = 8
#else
    type PrimSize Word = 4
#endif
    primSizeInBytes _ = sizeWord
    {-# INLINE primSizeInBytes #-}
    primShiftToBytes _ = shiftWord
    {-# INLINE primShiftToBytes #-}
    primBaUIndex ba (Offset (I# n)) = W# (indexWordArray# ba n)
    {-# INLINE primBaUIndex #-}
    primMbaURead mba (Offset (I# n)) = primitive $ \s1 -> let !(# s2, r #) = readWordArray# mba n s1 in (# s2, W# r #)
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba (Offset (I# n)) (W# w) = primitive $ \s1 -> (# writeWordArray# mba n w s1, () #)
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr (Offset (I# n)) = W# (indexWordOffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let !(# s2, r #) = readWordOffAddr# addr n s1 in (# s2, W# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (W# w) = primitive $ \s1 -> (# writeWordOffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}

instance PrimType Word8 where
    type PrimSize Word8 = 1
    primSizeInBytes _ = CountOf 1
    {-# INLINE primSizeInBytes #-}
    primShiftToBytes _ = 0
    {-# INLINE primShiftToBytes #-}
    primBaUIndex ba (Offset (I# n)) = W8# (indexWord8Array# ba n)
    {-# INLINE primBaUIndex #-}
    primMbaURead mba (Offset (I# n)) = primitive $ \s1 -> let !(# s2, r #) = readWord8Array# mba n s1 in (# s2, W8# r #)
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba (Offset (I# n)) (W8# w) = primitive $ \s1 -> (# writeWord8Array# mba n w s1, () #)
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr (Offset (I# n)) = W8# (indexWord8OffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let !(# s2, r #) = readWord8OffAddr# addr n s1 in (# s2, W8# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (W8# w) = primitive $ \s1 -> (# writeWord8OffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}

instance PrimType Word16 where
    type PrimSize Word16 = 2
    primSizeInBytes _ = CountOf 2
    {-# INLINE primSizeInBytes #-}
    primShiftToBytes _ = 1
    {-# INLINE primShiftToBytes #-}
    primBaUIndex ba (Offset (I# n)) = W16# (indexWord16Array# ba n)
    {-# INLINE primBaUIndex #-}
    primMbaURead mba (Offset (I# n)) = primitive $ \s1 -> let !(# s2, r #) = readWord16Array# mba n s1 in (# s2, W16# r #)
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba (Offset (I# n)) (W16# w) = primitive $ \s1 -> (# writeWord16Array# mba n w s1, () #)
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr (Offset (I# n)) = W16# (indexWord16OffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let !(# s2, r #) = readWord16OffAddr# addr n s1 in (# s2, W16# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (W16# w) = primitive $ \s1 -> (# writeWord16OffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}
instance PrimType Word32 where
    type PrimSize Word32 = 4
    primSizeInBytes _ = CountOf 4
    {-# INLINE primSizeInBytes #-}
    primShiftToBytes _ = 2
    {-# INLINE primShiftToBytes #-}
    primBaUIndex ba (Offset (I# n)) = W32# (indexWord32Array# ba n)
    {-# INLINE primBaUIndex #-}
    primMbaURead mba (Offset (I# n)) = primitive $ \s1 -> let !(# s2, r #) = readWord32Array# mba n s1 in (# s2, W32# r #)
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba (Offset (I# n)) (W32# w) = primitive $ \s1 -> (# writeWord32Array# mba n w s1, () #)
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr (Offset (I# n)) = W32# (indexWord32OffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let !(# s2, r #) = readWord32OffAddr# addr n s1 in (# s2, W32# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (W32# w) = primitive $ \s1 -> (# writeWord32OffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}
instance PrimType Word64 where
    type PrimSize Word64 = 8
    primSizeInBytes _ = CountOf 8
    {-# INLINE primSizeInBytes #-}
    primShiftToBytes _ = 3
    {-# INLINE primShiftToBytes #-}
    primBaUIndex ba (Offset (I# n)) = W64# (indexWord64Array# ba n)
    {-# INLINE primBaUIndex #-}
    primMbaURead mba (Offset (I# n)) = primitive $ \s1 -> let !(# s2, r #) = readWord64Array# mba n s1 in (# s2, W64# r #)
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba (Offset (I# n)) (W64# w) = primitive $ \s1 -> (# writeWord64Array# mba n w s1, () #)
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr (Offset (I# n)) = W64# (indexWord64OffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let !(# s2, r #) = readWord64OffAddr# addr n s1 in (# s2, W64# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (W64# w) = primitive $ \s1 -> (# writeWord64OffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}
instance PrimType Word128 where
    type PrimSize Word128 = 16
    primSizeInBytes _ = CountOf 16
    {-# INLINE primSizeInBytes #-}
    primShiftToBytes _ = 4
    {-# INLINE primShiftToBytes #-}
    primBaUIndex ba n =
        Word128 (W64# (indexWord64Array# ba n1)) (W64# (indexWord64Array# ba n2))
      where (# n1, n2 #) = offset128_64 n
    {-# INLINE primBaUIndex #-}
    primMbaURead mba n = primitive $ \s1 -> let !(# s2, r1 #) = readWord64Array# mba n1 s1
                                                !(# s3, r2 #) = readWord64Array# mba n2 s2
                                             in (# s3, Word128 (W64# r1) (W64# r2) #)
      where (# n1, n2 #) = offset128_64 n
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba n (Word128 (W64# w1) (W64# w2)) = primitive $ \s1 ->
        let !s2 = writeWord64Array# mba n1 w1 s1
         in (# writeWord64Array# mba n2 w2 s2, () #)
      where (# n1, n2 #) = offset128_64 n
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr n = Word128 (W64# (indexWord64OffAddr# addr n1)) (W64# (indexWord64OffAddr# addr n2))
      where (# n1, n2 #) = offset128_64 n
    {-# INLINE primAddrIndex #-}
    primAddrRead addr n = primitive $ \s1 -> let !(# s2, r1 #) = readWord64OffAddr# addr n1 s1
                                                 !(# s3, r2 #) = readWord64OffAddr# addr n2 s2
                                              in (# s3, Word128 (W64# r1) (W64# r2) #)
      where (# n1, n2 #) = offset128_64 n
    {-# INLINE primAddrRead #-}
    primAddrWrite addr n (Word128 (W64# w1) (W64# w2)) = primitive $ \s1 ->
        let !s2 = writeWord64OffAddr# addr n1 w1 s1
         in (# writeWord64OffAddr# addr n2 w2 s2, () #)
      where (# n1, n2 #) = offset128_64 n
    {-# INLINE primAddrWrite #-}
instance PrimType Word256 where
    type PrimSize Word256 = 32
    primSizeInBytes _ = CountOf 32
    {-# INLINE primSizeInBytes #-}
    primShiftToBytes _ = 5
    {-# INLINE primShiftToBytes #-}
    primBaUIndex ba n =
        Word256 (W64# (indexWord64Array# ba n1)) (W64# (indexWord64Array# ba n2))
                (W64# (indexWord64Array# ba n3)) (W64# (indexWord64Array# ba n4))
      where (# n1, n2, n3, n4 #) = offset256_64 n
    {-# INLINE primBaUIndex #-}
    primMbaURead mba n = primitive $ \s1 -> let !(# s2, r1 #) = readWord64Array# mba n1 s1
                                                !(# s3, r2 #) = readWord64Array# mba n2 s2
                                                !(# s4, r3 #) = readWord64Array# mba n3 s3
                                                !(# s5, r4 #) = readWord64Array# mba n4 s4
                                             in (# s5, Word256 (W64# r1) (W64# r2) (W64# r3) (W64# r4) #)
      where (# n1, n2, n3, n4 #) = offset256_64 n
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba n (Word256 (W64# w1) (W64# w2) (W64# w3) (W64# w4)) = primitive $ \s1 ->
        let !s2 = writeWord64Array# mba n1 w1 s1
            !s3 = writeWord64Array# mba n2 w2 s2
            !s4 = writeWord64Array# mba n3 w3 s3
         in (# writeWord64Array# mba n4 w4 s4, () #)
      where (# n1, n2, n3, n4 #) = offset256_64 n
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr n = Word256 (W64# (indexWord64OffAddr# addr n1)) (W64# (indexWord64OffAddr# addr n2))
                                   (W64# (indexWord64OffAddr# addr n3)) (W64# (indexWord64OffAddr# addr n4))
      where (# n1, n2, n3, n4 #) = offset256_64 n
    {-# INLINE primAddrIndex #-}
    primAddrRead addr n = primitive $ \s1 -> let !(# s2, r1 #) = readWord64OffAddr# addr n1 s1
                                                 !(# s3, r2 #) = readWord64OffAddr# addr n2 s2
                                                 !(# s4, r3 #) = readWord64OffAddr# addr n3 s3
                                                 !(# s5, r4 #) = readWord64OffAddr# addr n4 s4
                                              in (# s5, Word256 (W64# r1) (W64# r2) (W64# r3) (W64# r4) #)
      where (# n1, n2, n3, n4 #) = offset256_64 n
    {-# INLINE primAddrRead #-}
    primAddrWrite addr n (Word256 (W64# w1) (W64# w2) (W64# w3) (W64# w4)) = primitive $ \s1 ->
        let !s2 = writeWord64OffAddr# addr n1 w1 s1
            !s3 = writeWord64OffAddr# addr n2 w2 s2
            !s4 = writeWord64OffAddr# addr n3 w3 s3
         in (# writeWord64OffAddr# addr n4 w4 s4, () #)
      where (# n1, n2, n3, n4 #) = offset256_64 n
    {-# INLINE primAddrWrite #-}
instance PrimType Int8 where
    type PrimSize Int8 = 1
    primSizeInBytes _ = CountOf 1
    {-# INLINE primSizeInBytes #-}
    primShiftToBytes _ = 0
    {-# INLINE primShiftToBytes #-}
    primBaUIndex ba (Offset (I# n)) = I8# (indexInt8Array# ba n)
    {-# INLINE primBaUIndex #-}
    primMbaURead mba (Offset (I# n)) = primitive $ \s1 -> let !(# s2, r #) = readInt8Array# mba n s1 in (# s2, I8# r #)
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba (Offset (I# n)) (I8# w) = primitive $ \s1 -> (# writeInt8Array# mba n w s1, () #)
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr (Offset (I# n)) = I8# (indexInt8OffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let !(# s2, r #) = readInt8OffAddr# addr n s1 in (# s2, I8# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (I8# w) = primitive $ \s1 -> (# writeInt8OffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}
instance PrimType Int16 where
    type PrimSize Int16 = 2
    primSizeInBytes _ = CountOf 2
    {-# INLINE primSizeInBytes #-}
    primShiftToBytes _ = 1
    {-# INLINE primShiftToBytes #-}
    primBaUIndex ba (Offset (I# n)) = I16# (indexInt16Array# ba n)
    {-# INLINE primBaUIndex #-}
    primMbaURead mba (Offset (I# n)) = primitive $ \s1 -> let !(# s2, r #) = readInt16Array# mba n s1 in (# s2, I16# r #)
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba (Offset (I# n)) (I16# w) = primitive $ \s1 -> (# writeInt16Array# mba n w s1, () #)
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr (Offset (I# n)) = I16# (indexInt16OffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let !(# s2, r #) = readInt16OffAddr# addr n s1 in (# s2, I16# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (I16# w) = primitive $ \s1 -> (# writeInt16OffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}
instance PrimType Int32 where
    type PrimSize Int32 = 4
    primSizeInBytes _ = CountOf 4
    {-# INLINE primSizeInBytes #-}
    primShiftToBytes _ = 2
    {-# INLINE primShiftToBytes #-}
    primBaUIndex ba (Offset (I# n)) = I32# (indexInt32Array# ba n)
    {-# INLINE primBaUIndex #-}
    primMbaURead mba (Offset (I# n)) = primitive $ \s1 -> let !(# s2, r #) = readInt32Array# mba n s1 in (# s2, I32# r #)
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba (Offset (I# n)) (I32# w) = primitive $ \s1 -> (# writeInt32Array# mba n w s1, () #)
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr (Offset (I# n)) = I32# (indexInt32OffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let !(# s2, r #) = readInt32OffAddr# addr n s1 in (# s2, I32# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (I32# w) = primitive $ \s1 -> (# writeInt32OffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}
instance PrimType Int64 where
    type PrimSize Int64 = 8
    primSizeInBytes _ = CountOf 8
    {-# INLINE primSizeInBytes #-}
    primShiftToBytes _ = 3
    {-# INLINE primShiftToBytes #-}
    primBaUIndex ba (Offset (I# n)) = I64# (indexInt64Array# ba n)
    {-# INLINE primBaUIndex #-}
    primMbaURead mba (Offset (I# n)) = primitive $ \s1 -> let !(# s2, r #) = readInt64Array# mba n s1 in (# s2, I64# r #)
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba (Offset (I# n)) (I64# w) = primitive $ \s1 -> (# writeInt64Array# mba n w s1, () #)
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr (Offset (I# n)) = I64# (indexInt64OffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let !(# s2, r #) = readInt64OffAddr# addr n s1 in (# s2, I64# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (I64# w) = primitive $ \s1 -> (# writeInt64OffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}

instance PrimType Float where
    type PrimSize Float = 4
    primSizeInBytes _ = CountOf 4
    {-# INLINE primSizeInBytes #-}
    primShiftToBytes _ = 2
    {-# INLINE primShiftToBytes #-}
    primBaUIndex ba (Offset (I# n)) = F# (indexFloatArray# ba n)
    {-# INLINE primBaUIndex #-}
    primMbaURead mba (Offset (I# n)) = primitive $ \s1 -> let !(# s2, r #) = readFloatArray# mba n s1 in (# s2, F# r #)
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba (Offset (I# n)) (F# w) = primitive $ \s1 -> (# writeFloatArray# mba n w s1, () #)
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr (Offset (I# n)) = F# (indexFloatOffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let !(# s2, r #) = readFloatOffAddr# addr n s1 in (# s2, F# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (F# w) = primitive $ \s1 -> (# writeFloatOffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}
instance PrimType Double where
    type PrimSize Double = 8
    primSizeInBytes _ = CountOf 8
    {-# INLINE primSizeInBytes #-}
    primShiftToBytes _ = 3
    {-# INLINE primShiftToBytes #-}
    primBaUIndex ba (Offset (I# n)) = D# (indexDoubleArray# ba n)
    {-# INLINE primBaUIndex #-}
    primMbaURead mba (Offset (I# n)) = primitive $ \s1 -> let !(# s2, r #) = readDoubleArray# mba n s1 in (# s2, D# r #)
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba (Offset (I# n)) (D# w) = primitive $ \s1 -> (# writeDoubleArray# mba n w s1, () #)
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr (Offset (I# n)) = D# (indexDoubleOffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let !(# s2, r #) = readDoubleOffAddr# addr n s1 in (# s2, D# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (D# w) = primitive $ \s1 -> (# writeDoubleOffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}

instance PrimType Char where
    type PrimSize Char = 4
    primSizeInBytes _ = CountOf 4
    {-# INLINE primSizeInBytes #-}
    primShiftToBytes _ = 2
    {-# INLINE primShiftToBytes #-}
    primBaUIndex ba (Offset (I# n)) = C# (indexWideCharArray# ba n)
    {-# INLINE primBaUIndex #-}
    primMbaURead mba (Offset (I# n)) = primitive $ \s1 -> let !(# s2, r #) = readWideCharArray# mba n s1 in (# s2, C# r #)
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba (Offset (I# n)) (C# w) = primitive $ \s1 -> (# writeWideCharArray# mba n w s1, () #)
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr (Offset (I# n)) = C# (indexWideCharOffAddr# addr n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset (I# n)) = primitive $ \s1 -> let !(# s2, r #) = readWideCharOffAddr# addr n s1 in (# s2, C# r #)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset (I# n)) (C# w) = primitive $ \s1 -> (# writeWideCharOffAddr# addr n w s1, () #)
    {-# INLINE primAddrWrite #-}

instance PrimType CChar where
    type PrimSize CChar = 1
    primSizeInBytes _ = CountOf 1
    {-# INLINE primSizeInBytes #-}
    primShiftToBytes _ = 0
    {-# INLINE primShiftToBytes #-}
    primBaUIndex ba (Offset n) = CChar (primBaUIndex ba (Offset n))
    {-# INLINE primBaUIndex #-}
    primMbaURead mba (Offset n) = CChar <$> primMbaURead mba (Offset n)
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba (Offset n) (CChar int8) = primMbaUWrite mba (Offset n) int8
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr (Offset n) = CChar $ primAddrIndex addr (Offset n)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset n) = CChar <$> primAddrRead addr (Offset n)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset n) (CChar int8) = primAddrWrite addr (Offset n) int8
    {-# INLINE primAddrWrite #-}
instance PrimType CUChar where
    type PrimSize CUChar = 1
    primSizeInBytes _ = CountOf 1
    {-# INLINE primSizeInBytes #-}
    primShiftToBytes _ = 0
    {-# INLINE primShiftToBytes #-}
    primBaUIndex ba (Offset n) = CUChar (primBaUIndex ba (Offset n :: Offset Word8))
    {-# INLINE primBaUIndex #-}
    primMbaURead mba (Offset n) = CUChar <$> primMbaURead mba (Offset n :: Offset Word8)
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba (Offset n) (CUChar w8) = primMbaUWrite mba (Offset n) w8
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr (Offset n) = CUChar $ primAddrIndex addr (Offset n :: Offset Word8)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset n) = CUChar <$> primAddrRead addr (Offset n :: Offset Word8)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset n) (CUChar w8) = primAddrWrite addr (Offset n) w8
    {-# INLINE primAddrWrite #-}

instance PrimType Char7 where
    type PrimSize Char7 = 1
    primSizeInBytes _ = CountOf 1
    {-# INLINE primSizeInBytes #-}
    primShiftToBytes _ = 0
    {-# INLINE primShiftToBytes #-}
    primBaUIndex ba (Offset n) = Char7 (primBaUIndex ba (Offset n :: Offset Word8))
    {-# INLINE primBaUIndex #-}
    primMbaURead mba (Offset n) = Char7 <$> primMbaURead mba (Offset n :: Offset Word8)
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba (Offset n) (Char7 w8) = primMbaUWrite mba (Offset n) w8
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr (Offset n) = Char7 $ primAddrIndex addr (Offset n :: Offset Word8)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset n) = Char7 <$> primAddrRead addr (Offset n :: Offset Word8)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset n) (Char7 w8) = primAddrWrite addr (Offset n) w8
    {-# INLINE primAddrWrite #-}

instance PrimType a => PrimType (LE a) where
    type PrimSize (LE a) = PrimSize a
    primSizeInBytes _ = primSizeInBytes (Proxy :: Proxy a)
    {-# INLINE primSizeInBytes #-}
    primShiftToBytes _ = primShiftToBytes (Proxy :: Proxy a)
    {-# INLINE primShiftToBytes #-}
    primBaUIndex ba (Offset a) = LE $ primBaUIndex ba (Offset a)
    {-# INLINE primBaUIndex #-}
    primMbaURead ba (Offset a) = LE <$> primMbaURead ba (Offset a)
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba (Offset a) (LE w) = primMbaUWrite mba (Offset a) w
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr (Offset a) = LE $ primAddrIndex addr (Offset a)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset a) = LE <$> primAddrRead addr (Offset a)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset a) (LE w) = primAddrWrite addr (Offset a) w
    {-# INLINE primAddrWrite #-}
instance PrimType a => PrimType (BE a) where
    type PrimSize (BE a) = PrimSize a
    primSizeInBytes _ = primSizeInBytes (Proxy :: Proxy a)
    {-# INLINE primSizeInBytes #-}
    primShiftToBytes _ = primShiftToBytes (Proxy :: Proxy a)
    {-# INLINE primShiftToBytes #-}
    primBaUIndex ba (Offset a) = BE $ primBaUIndex ba (Offset a)
    {-# INLINE primBaUIndex #-}
    primMbaURead ba (Offset a) = BE <$> primMbaURead ba (Offset a)
    {-# INLINE primMbaURead #-}
    primMbaUWrite mba (Offset a) (BE w) = primMbaUWrite mba (Offset a) w
    {-# INLINE primMbaUWrite #-}
    primAddrIndex addr (Offset a) = BE $ primAddrIndex addr (Offset a)
    {-# INLINE primAddrIndex #-}
    primAddrRead addr (Offset a) = BE <$> primAddrRead addr (Offset a)
    {-# INLINE primAddrRead #-}
    primAddrWrite addr (Offset a) (BE w) = primAddrWrite addr (Offset a) w
    {-# INLINE primAddrWrite #-}

-- | A constraint class for serializable type that have an unique
-- memory compare representation
--
-- e.g. Float and Double have -0.0 and 0.0 which are Eq individual,
-- yet have a different memory representation which doesn't allow
-- for memcmp operation
class PrimMemoryComparable ty where

instance PrimMemoryComparable Int where
instance PrimMemoryComparable Word where
instance PrimMemoryComparable Word8 where
instance PrimMemoryComparable Word16 where
instance PrimMemoryComparable Word32 where
instance PrimMemoryComparable Word64 where
instance PrimMemoryComparable Word128 where
instance PrimMemoryComparable Word256 where
instance PrimMemoryComparable Int8 where
instance PrimMemoryComparable Int16 where
instance PrimMemoryComparable Int32 where
instance PrimMemoryComparable Int64 where
instance PrimMemoryComparable Char where
instance PrimMemoryComparable CChar where
instance PrimMemoryComparable CUChar where
instance PrimMemoryComparable a => PrimMemoryComparable (LE a) where
instance PrimMemoryComparable a => PrimMemoryComparable (BE a) where

offset128_64 :: Offset Word128 -> (# Int#, Int# #)
offset128_64 (Offset (I# i)) = (# n , n +# 1# #)
  where !n = uncheckedIShiftL# i 1#

offset256_64 :: Offset Word256 -> (# Int#, Int#, Int#, Int# #)
offset256_64 (Offset (I# i)) = (# n , n +# 1#, n +# 2#, n +# 3# #)
  where !n = uncheckedIShiftL# i 2#

-- | Cast a CountOf linked to type A (CountOf A) to a CountOf linked to type B (CountOf B)
sizeRecast :: forall a b . (PrimType a, PrimType b) => CountOf a -> CountOf b
sizeRecast sz = CountOf (bytes `Prelude.quot` szB)
  where !szA             = primSizeInBytes (Proxy :: Proxy a)
        !(CountOf szB)   = primSizeInBytes (Proxy :: Proxy b)
        !(CountOf bytes) = sizeOfE szA sz
{-# INLINE [1] sizeRecast #-}
{-# RULES "sizeRecast from Word8" [2] forall a . sizeRecast a = sizeRecastBytes a #-}

sizeRecastBytes :: forall b . PrimType b => CountOf Word8 -> CountOf b
sizeRecastBytes (CountOf w) = CountOf (w `Prelude.quot` szB)
  where !(CountOf szB) = primSizeInBytes (Proxy :: Proxy b)
{-# INLINE [1] sizeRecastBytes #-}

sizeInBytes :: forall a . PrimType a => CountOf a -> CountOf Word8
sizeInBytes sz = sizeOfE (primSizeInBytes (Proxy :: Proxy a)) sz

offsetInBytes :: forall a . PrimType a => Offset a -> Offset Word8
offsetInBytes ofs = offsetShiftL (primShiftToBytes (Proxy :: Proxy a)) ofs
{-# INLINE [2] offsetInBytes #-}
{-# SPECIALIZE INLINE [3] offsetInBytes :: Offset Word64 -> Offset Word8 #-}
{-# SPECIALIZE INLINE [3] offsetInBytes :: Offset Word32 -> Offset Word8 #-}
{-# SPECIALIZE INLINE [3] offsetInBytes :: Offset Word16 -> Offset Word8 #-}
{-# RULES "offsetInBytes Bytes" [3] forall x . offsetInBytes x = x #-}

offsetInElements :: forall a . PrimType a => Offset Word8 -> Offset a
offsetInElements ofs = offsetShiftR (primShiftToBytes (Proxy :: Proxy a)) ofs
{-# INLINE [2] offsetInElements #-}
{-# SPECIALIZE INLINE [3] offsetInBytes :: Offset Word64 -> Offset Word8 #-}
{-# SPECIALIZE INLINE [3] offsetInBytes :: Offset Word32 -> Offset Word8 #-}
{-# SPECIALIZE INLINE [3] offsetInBytes :: Offset Word16 -> Offset Word8 #-}
{-# RULES "offsetInElements Bytes" [3] forall x . offsetInElements x = x #-}

primOffsetRecast :: forall a b . (PrimType a, PrimType b) => Offset a -> Offset b
primOffsetRecast !ofs =
    let !(Offset bytes) = offsetOfE szA ofs
     in Offset (bytes `Prelude.quot` szB)
  where
    !szA        = primSizeInBytes (Proxy :: Proxy a)
    !(CountOf szB) = primSizeInBytes (Proxy :: Proxy b)
{-# INLINE [1] primOffsetRecast #-}
{-# RULES "primOffsetRecast W8" [3] forall a . primOffsetRecast a = primOffsetRecastBytes a #-}

offsetIsAligned :: forall a . PrimType a => Proxy a -> Offset Word8 -> Bool
offsetIsAligned _ (Offset ofs) = (ofs .&. mask) == 0
   where (CountOf sz) = primSizeInBytes (Proxy :: Proxy a)
         mask = sz - 1
{-# INLINE [1] offsetIsAligned #-}
{-# SPECIALIZE [3] offsetIsAligned :: Proxy Word64 -> Offset Word8 -> Bool #-}
{-# RULES "offsetInAligned Bytes" [3] forall (prx :: Proxy Word8) x . offsetIsAligned prx x = True #-}

primOffsetRecastBytes :: forall b . PrimType b => Offset Word8 -> Offset b
primOffsetRecastBytes (Offset 0) = Offset 0
primOffsetRecastBytes (Offset o) = Offset (szA `Prelude.quot` o)
  where !(CountOf szA) = primSizeInBytes (Proxy :: Proxy b)
{-# INLINE [1] primOffsetRecastBytes #-}

primOffsetOfE :: forall a . PrimType a => Offset a -> Offset Word8
primOffsetOfE = offsetInBytes
{-# DEPRECATED primOffsetOfE "use offsetInBytes" #-}

primWordGetByteAndShift :: Word# -> (# Word#, Word# #)
primWordGetByteAndShift w = (# and# w 0xff##, uncheckedShiftRL# w 8# #)
{-# INLINE primWordGetByteAndShift #-}

#if WORD_SIZE_IN_BITS == 64
primWord64GetByteAndShift :: Word# -> (# Word#, Word# #)
primWord64GetByteAndShift = primWord64GetByteAndShift

primWord64GetHiLo :: Word# -> (# Word#, Word# #)
primWord64GetHiLo w = (# uncheckedShiftRL# w 32# , and# w 0xffffffff## #)
#else
primWord64GetByteAndShift :: Word64# -> (# Word#, Word64# #)
primWord64GetByteAndShift w = (# and# (word64ToWord# w) 0xff##, uncheckedShiftRL64# w 8# #)

primWord64GetHiLo :: Word64# -> (# Word#, Word# #)
primWord64GetHiLo w = (# word64ToWord# (uncheckedShiftRL64# w 32#), word64ToWord# w #)
#endif
{-# INLINE primWord64GetByteAndShift #-}

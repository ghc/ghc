-- Reproducer for T17334
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnboxedTuples         #-}

--Reproducer uses 64bit literals in reverseWord.
--It's ok to truncate those in x86
{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Bug (reverseInPlace) where

import Control.Monad.ST
import Data.Bits
import GHC.Exts
import GHC.ST (ST(..))
import Data.Kind

reverseInPlace :: PrimMonad m => UMVector (PrimState m) Bit -> m ()
reverseInPlace xs | len == 0  = pure ()
                  | otherwise = loop 0
 where
  len = ulength xs

  loop !i
    | i' <= j' = do
      x <- readWord xs i
      y <- readWord xs j'

      writeWord xs i  (reverseWord y)
      writeWord xs j' (reverseWord x)

      loop i'
    | i' < j = do
      let w = (j - i) `shiftR` 1
          k = j - w
      x <- readWord xs i
      y <- readWord xs k

      writeWord xs i (meld w (reversePartialWord w y) x)
      writeWord xs k (meld w (reversePartialWord w x) y)

      loop i'
    | otherwise = do
      let w = j - i
      x <- readWord xs i
      writeWord xs i (meld w (reversePartialWord w x) x)
   where
    !j  = len - i
    !i' = i + wordSize
    !j' = j - wordSize
{-# SPECIALIZE reverseInPlace :: UMVector s Bit -> ST s () #-}

newtype Bit = Bit { unBit :: Bool }

instance Unbox Bit

data instance UMVector s Bit = BitMVec !Int !Int !(MutableByteArray s)
data instance UVector    Bit = BitVec  !Int !Int !ByteArray

readWord :: PrimMonad m => UMVector (PrimState m) Bit -> Int -> m Word
readWord !(BitMVec _ 0 _) _ = pure 0
readWord !(BitMVec off len' arr) !i' = do
  let len  = off + len'
      i    = off + i'
      nMod = modWordSize i
      loIx = divWordSize i
  loWord <- readByteArray arr loIx

  if nMod == 0
    then pure loWord
    else if loIx == divWordSize (len - 1)
      then pure (loWord `unsafeShiftR` nMod)
      else do
        hiWord <- readByteArray arr (loIx + 1)
        pure
          $   (loWord `unsafeShiftR` nMod)
          .|. (hiWord `unsafeShiftL` (wordSize - nMod))
{-# SPECIALIZE readWord :: UMVector s Bit -> Int -> ST s Word #-}
{-# INLINE readWord #-}

writeWord :: PrimMonad m => UMVector (PrimState m) Bit -> Int -> Word -> m ()
writeWord !(BitMVec _ 0 _) _ _ = pure ()
writeWord !(BitMVec off len' arr@(MutableByteArray mba)) !i' !x@(W# x#) = do
  let len    = off + len'
      lenMod = modWordSize len
      i      = off + i'
      nMod   = modWordSize i
      loIx@(I# loIx#)   = divWordSize i

  if nMod == 0
    then if len >= i + wordSize
      then primitive $ \state ->
        (# atomicWriteIntArray# mba loIx# (word2Int# x#) state, () #)
      else do
        let W# andMask# = hiMask lenMod
            W# orMask#  = x .&. loMask lenMod
        primitive $ \state ->
          let !(# state',  _ #) = fetchAndIntArray# mba loIx# (word2Int# andMask#) state  in
            let !(# state'', _ #) = fetchOrIntArray#  mba loIx# (word2Int# orMask#)  state' in
              (# state'', () #)
    else if loIx == divWordSize (len - 1)
      then do
        loWord <- readByteArray arr loIx
        if lenMod == 0
          then
            writeByteArray arr loIx
            $   (loWord .&. loMask nMod)
            .|. (x `unsafeShiftL` nMod)
          else
            writeByteArray arr loIx
            $   (loWord .&. (loMask nMod .|. hiMask lenMod))
            .|. ((x `unsafeShiftL` nMod) .&. loMask lenMod)
      else do
        loWord <- readByteArray arr loIx
        writeByteArray arr loIx
          $   (loWord .&. loMask nMod)
          .|. (x `unsafeShiftL` nMod)
        hiWord <- readByteArray arr (loIx + 1)
        writeByteArray arr (loIx + 1)
          $   (hiWord .&. hiMask nMod)
          .|. (x `unsafeShiftR` (wordSize - nMod))
{-# SPECIALIZE writeWord :: UMVector s Bit -> Int -> Word -> ST s () #-}
{-# INLINE writeWord #-}

instance GMVector UMVector Bit where
  {-# INLINE basicLength #-}
  basicLength (BitMVec _ n _) = n

instance GVector UVector Bit where

wordSize :: Int
wordSize = finiteBitSize (0 :: Word)

lgWordSize :: Int
lgWordSize = case wordSize of
  32 -> 5
  64 -> 6
  _  -> error "wordsToBytes: unknown architecture"

divWordSize :: Bits a => a -> a
divWordSize x = unsafeShiftR x lgWordSize
{-# INLINE divWordSize #-}

modWordSize :: Int -> Int
modWordSize x = x .&. (wordSize - 1)
{-# INLINE modWordSize #-}

mask :: Int -> Word
mask b = m
 where
  m | b >= finiteBitSize m = complement 0
    | b < 0                = 0
    | otherwise            = bit b - 1

meld :: Int -> Word -> Word -> Word
meld b lo hi = (lo .&. m) .|. (hi .&. complement m) where m = mask b
{-# INLINE meld #-}

reverseWord :: Word -> Word
reverseWord x0 = x6
 where
  x1 = ((x0 .&. 0x5555555555555555) `shiftL`  1) .|. ((x0 .&. 0xAAAAAAAAAAAAAAAA) `shiftR`  1)
  x2 = ((x1 .&. 0x3333333333333333) `shiftL`  2) .|. ((x1 .&. 0xCCCCCCCCCCCCCCCC) `shiftR`  2)
  x3 = ((x2 .&. 0x0F0F0F0F0F0F0F0F) `shiftL`  4) .|. ((x2 .&. 0xF0F0F0F0F0F0F0F0) `shiftR`  4)
  x4 = ((x3 .&. 0x00FF00FF00FF00FF) `shiftL`  8) .|. ((x3 .&. 0xFF00FF00FF00FF00) `shiftR`  8)
  x5 = ((x4 .&. 0x0000FFFF0000FFFF) `shiftL` 16) .|. ((x4 .&. 0xFFFF0000FFFF0000) `shiftR` 16)
  x6 = ((x5 .&. 0x00000000FFFFFFFF) `shiftL` 32) .|. ((x5 .&. 0xFFFFFFFF00000000) `shiftR` 32)

reversePartialWord :: Int -> Word -> Word
reversePartialWord n w | n >= wordSize = reverseWord w
                       | otherwise     = reverseWord w `shiftR` (wordSize - n)

loMask :: Int -> Word
loMask n = 1 `unsafeShiftL` n - 1
{-# INLINE loMask #-}

hiMask :: Int -> Word
hiMask n = complement (1 `unsafeShiftL` n - 1)
{-# INLINE hiMask #-}

class GMVector v a where
  basicLength :: v s a -> Int

glength :: GMVector v a => v s a -> Int
{-# INLINE glength #-}
glength = basicLength

type family GMutable (v :: Type -> Type) :: Type -> Type -> Type
class GMVector (GMutable v) a => GVector v a
data family UMVector s a
data family UVector    a
class (GVector UVector a, GMVector UMVector a) => Unbox a
type instance GMutable UVector = UMVector

ulength :: Unbox a => UMVector s a -> Int
{-# INLINE ulength #-}
ulength = glength

data ByteArray = ByteArray ByteArray#
data MutableByteArray s = MutableByteArray (MutableByteArray# s)

readByteArray
  :: (Prim a, PrimMonad m) => MutableByteArray (PrimState m) -> Int -> m a
{-# INLINE readByteArray #-}
readByteArray (MutableByteArray arr#) (I# i#)
  = primitive (readByteArray# arr# i#)

writeByteArray
  :: (Prim a, PrimMonad m) => MutableByteArray (PrimState m) -> Int -> a -> m ()
{-# INLINE writeByteArray #-}
writeByteArray (MutableByteArray arr#) (I# i#) x
  = primitive_ (writeByteArray# arr# i# x)

class Prim a where
  readByteArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
  writeByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> State# s

instance Prim Word where
  readByteArray#  arr# i# s# = case readWordArray# arr# i# s# of
                                 (# s1#, x# #) -> (# s1#, W# x# #)
  writeByteArray# arr# i# (W# x#) s# = writeWordArray# arr# i# x# s#

class Monad m => PrimMonad m where
  type PrimState m
  primitive :: (State# (PrimState m) -> (# State# (PrimState m), a #)) -> m a

instance PrimMonad (ST s) where
  type PrimState (ST s) = s
  primitive = ST
  {-# INLINE primitive #-}

primitive_ :: PrimMonad m
              => (State# (PrimState m) -> State# (PrimState m)) -> m ()
{-# INLINE primitive_ #-}
primitive_ f = primitive (\s# ->
    case f s# of
        s'# -> (# s'#, () #))

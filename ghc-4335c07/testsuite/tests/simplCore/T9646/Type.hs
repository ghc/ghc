{-# LANGUAGE BangPatterns, CPP, MagicHash, UnboxedTuples #-}

#include "MachDeps.h"

module Type where

import GHC.Prim
import GHC.Types

import StrictPrim

data Natural = Natural {-# UNPACK #-} !Int {-# UNPACK #-} !WordArray

data WordArray = WA ByteArray#
data MutableWordArray m = MWA (MutableByteArray# (PrimState m))

{-# INLINE newWordArray #-}
newWordArray :: (Monad m, PrimMonad m) => Int -> m (MutableWordArray m)
newWordArray !len = do
    let !(I# n#) = len * sizeOfWord
    primitive (\s# -> case newByteArray# n# s# of
                        (# s'#, arr# #) -> (# s'#, MWA arr# #))

{-# INLINE unsafeFreezeWordArray #-}
unsafeFreezeWordArray :: (Monad m, PrimMonad m)
                        => MutableWordArray m -> m WordArray
unsafeFreezeWordArray !(MWA arr#) =
    primitive (\s# -> case unsafeFreezeByteArray# arr# s# of
                        (# s'#, arr'# #) -> (# s'#, WA arr'# #))

{-# INLINE indexWordArray #-}
indexWordArray :: WordArray -> Int -> Word
indexWordArray !(WA arr#) (I# i#) =
    let w# = indexWordArray# arr# i# in W# w#

{-# INLINE indexWordArrayM #-}
indexWordArrayM :: Monad m => WordArray -> Int -> m Word
indexWordArrayM !(WA arr#) (I# i#) =
    let w# = indexWordArray# arr# i# in
    case W# w# of x -> return x


{-# INLINE writeWordArray #-}
writeWordArray :: (Monad m, PrimMonad m)
                        => MutableWordArray m -> Int -> Word -> m ()
writeWordArray !(MWA arr#) (I# i#) (W# x#) =
    primitive (\s# ->
        case writeWordArray# arr# i# x# s# of
            s'# -> (# s'#, () #))


{-# INLINE plusWord #-}
plusWord :: Word -> Word -> Word
plusWord (W# a) (W# b) =
    let !s = plusWord# a b
    in W# s

{-# INLINE plusWord2 #-}
plusWord2 :: Word -> Word -> (# Word, Word #)
plusWord2 (W# a) (W# b) =
    let (# !c, !s #) = plusWord2# a b
    in (# W# c, W# s #)

{-# INLINE plusWord2C #-}
plusWord2C :: Word -> Word -> Word -> (# Word, Word #)
plusWord2C (W# a) (W# b) (W# c) =
    let (# !c1, !s1 #) = plusWord2# a b
        (# !c2, !s2 #) = plusWord2# s1 c
        !carry = plusWord# c1 c2
    in (# W# carry, W# s2 #)

{-# INLINE timesWord2 #-}
timesWord2 :: Word -> Word -> (# Word, Word #)
timesWord2 (W# a) (W# b) =
    let (# !ovf, !prod #) = timesWord2# a b
    in (# W# ovf, W# prod #)

sizeOfWord :: Int
sizeOfWord = WORD_SIZE_IN_BITS `div` 8

-- Reproducer for T17334
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnboxedTuples         #-}

module T17334 where

import Control.Monad.ST
import Data.Bits
import Data.Kind
import GHC.Exts
import GHC.ST (ST(..))

reverseInPlace :: UMVector s Bit -> ST s ()
reverseInPlace xs = loop 0
 where
  len = 4

  loop !i
    | i' < j = do
      let w = 1
          k = 2
      x <- return 1
      y <- return 2

      writeWord xs i (meld w (reversePartialWord w y) x)

      loop i'

   where
    !j  = 5
    !i' = i + wordSize

newtype Bit = Bit { unBit :: Bool }

instance Unbox Bit

data instance UMVector s Bit = BitMVec !Int !Int !(MutableByteArray s)
data instance UVector    Bit = BitVec  !Int !Int !ByteArray

-- {-# NOINLINE writeWord #-}
writeWord :: UMVector s Bit -> Int -> Word -> ST s ()
writeWord !(BitMVec _ 0 _) _ _ = pure ()
writeWord !(BitMVec off len' arr@(MutableByteArray mba)) !i' !x@(W# x#) = do
  let len    = 5
      lenMod = 6
      i      = 7
      nMod   = 8
      loIx@(I# loIx#)   = 9

  do
        let W# andMask# = hiMask lenMod
            W# orMask#  = x .&. loMask lenMod
        primitive $ \state ->
          let !(# state',  _ #) = fetchAndIntArray# mba loIx# (word2Int# andMask#) state  in
            let !(# state'', _ #) = fetchOrIntArray#  mba loIx# (word2Int# orMask#)  state' in
              (# state'', () #)

instance GMVector UMVector Bit where
  {-# INLINE basicLength #-}
  basicLength (BitMVec _ n _) = n

instance GVector UVector Bit where

wordSize :: Int
wordSize = 10

lgWordSize :: Int
lgWordSize = 11

modWordSize :: Int -> Int
modWordSize x = 12

mask :: Int -> Word
mask b = 13

meld :: Int -> Word -> Word -> Word
meld b lo hi = 14
{-# INLINE meld #-}

reverseWord :: Word -> Word
reverseWord x0 = 15

reversePartialWord :: Int -> Word -> Word
reversePartialWord n w = 16

loMask :: Int -> Word
loMask n = 17

hiMask :: Int -> Word
hiMask n = 18

class GMVector v a where
  basicLength :: v s a -> Int

type family GMutable (v :: Type -> Type) :: Type -> Type -> Type
class GMVector (GMutable v) a => GVector v a
data family UMVector s a
data family UVector    a
class (GVector UVector a, GMVector UMVector a) => Unbox a
type instance GMutable UVector = UMVector

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

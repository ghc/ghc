{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -fno-prof-auto #-}
module Basement.Numerical.Additive
    ( Additive(..)
    ) where

#include "MachDeps.h"

import           Basement.Compat.Base
import           Basement.Compat.C.Types
import           Basement.Compat.Natural
import           Basement.Numerical.Number
import qualified Prelude
import           GHC.Types
import           GHC.Prim
import           GHC.Int
import           GHC.Word
import           Basement.Bounded
import           Basement.Nat
import           Basement.Types.Word128 (Word128)
import           Basement.Types.Word256 (Word256)
import qualified Basement.Types.Word128 as Word128
import qualified Basement.Types.Word256 as Word256

#if WORD_SIZE_IN_BITS < 64
import           GHC.IntWord64
#endif

-- | Represent class of things that can be added together,
-- contains a neutral element and is commutative.
--
-- > x + azero = x
-- > azero + x = x
-- > x + y = y + x
--
class Additive a where
    {-# MINIMAL azero, (+) #-}
    azero :: a           -- the identity element over addition
    (+)   :: a -> a -> a -- the addition

    scale :: IsNatural n => n -> a -> a -- scale: repeated addition
    default scale :: (Enum n, IsNatural n) => n -> a -> a
    scale = scaleEnum

scaleEnum :: (Enum n, IsNatural n, Additive a) => n -> a -> a
scaleEnum 0 _ = azero
scaleEnum 1 a = a
scaleEnum 2 a = a + a
scaleEnum n a = a + scaleEnum (pred n) a -- TODO optimise. define by group of 2.

infixl 6 +

instance Additive Integer where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive Int where
    azero = 0
    (I# a) + (I# b) = I# (a +# b)
    scale = scaleNum
instance Additive Int8 where
    azero = 0
    (I8# a) + (I8# b) = I8# (narrow8Int# (a +# b))
    scale = scaleNum
instance Additive Int16 where
    azero = 0
    (I16# a) + (I16# b) = I16# (narrow16Int# (a +# b))
    scale = scaleNum
instance Additive Int32 where
    azero = 0
    (I32# a) + (I32# b) = I32# (narrow32Int# (a +# b))
    scale = scaleNum
instance Additive Int64 where
    azero = 0
#if WORD_SIZE_IN_BITS == 64
    (I64# a) + (I64# b) = I64# (a +# b)
#else
    (I64# a) + (I64# b) = I64# (a `plusInt64#` b)
#endif
    scale = scaleNum
instance Additive Word where
    azero = 0
    (W# a) + (W# b) = W# (a `plusWord#` b)
    scale = scaleNum
instance Additive Natural where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive Word8 where
    azero = 0
    (W8# a) + (W8# b) = W8# (narrow8Word# (a `plusWord#` b))
    scale = scaleNum
instance Additive Word16 where
    azero = 0
    (W16# a) + (W16# b) = W16# (narrow16Word# (a `plusWord#` b))
    scale = scaleNum
instance Additive Word32 where
    azero = 0
    (W32# a) + (W32# b) = W32# (narrow32Word# (a `plusWord#` b))
    scale = scaleNum
instance Additive Word64 where
    azero = 0
#if WORD_SIZE_IN_BITS == 64
    (W64# a) + (W64# b) = W64# (a `plusWord#` b)
#else
    (W64# a) + (W64# b) = W64# (int64ToWord64# (word64ToInt64# a `plusInt64#` word64ToInt64# b))
#endif
    scale = scaleNum
instance Additive Word128 where
    azero = 0
    (+) = (Word128.+)
    scale = scaleNum
instance Additive Word256 where
    azero = 0
    (+) = (Word256.+)
    scale = scaleNum

instance Additive Prelude.Float where
    azero = 0.0
    (F# a) + (F# b) = F# (a `plusFloat#` b)
    scale = scaleNum
instance Additive Prelude.Double where
    azero = 0.0
    (D# a) + (D# b) = D# (a +## b)
    scale = scaleNum
instance Additive Prelude.Rational where
    azero = 0.0
    (+) = (Prelude.+)
    scale = scaleNum

instance (KnownNat n, NatWithinBound Word64 n) => Additive (Zn64 n) where
    azero = zn64 0
    (+) = (Prelude.+)
    scale = scaleNum
instance KnownNat n => Additive (Zn n) where
    azero = zn 0
    (+) = (Prelude.+)
    scale = scaleNum

instance Additive CChar where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive CSChar where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive CUChar where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive CShort where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive CUShort where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive CInt where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive CUInt where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive CLong where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive CULong where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive CPtrdiff where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive CSize where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive CWchar where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive CSigAtomic where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive CLLong where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive CULLong where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive CIntPtr where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive CUIntPtr where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive CIntMax where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive CUIntMax where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive CClock where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive CTime where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive CUSeconds where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive CSUSeconds where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive COff where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum

instance Additive CFloat where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum
instance Additive CDouble where
    azero = 0
    (+) = (Prelude.+)
    scale = scaleNum

scaleNum :: (Prelude.Num a, IsNatural n) => n -> a -> a
scaleNum n a = (Prelude.fromIntegral $ toNatural n) Prelude.* a

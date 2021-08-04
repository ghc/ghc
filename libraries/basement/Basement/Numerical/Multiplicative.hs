{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DefaultSignatures #-}
module Basement.Numerical.Multiplicative
    ( Multiplicative(..)
    , IDivisible(..)
    , Divisible(..)
    , recip
    ) where

import           Basement.Compat.Base
import           Basement.Compat.C.Types
import           Basement.Compat.Natural
import           Basement.Compat.NumLiteral
import           Basement.Numerical.Number
import           Basement.Numerical.Additive
import           Basement.Types.Word128 (Word128)
import           Basement.Types.Word256 (Word256)
import qualified Basement.Types.Word128 as Word128
import qualified Basement.Types.Word256 as Word256
import qualified Prelude

-- | Represent class of things that can be multiplied together
--
-- > x * midentity = x
-- > midentity * x = x
class Multiplicative a where
    {-# MINIMAL midentity, (*) #-}
    -- | Identity element over multiplication
    midentity :: a

    -- | Multiplication of 2 elements that result in another element
    (*) :: a -> a -> a

    -- | Raise to power, repeated multiplication
    -- e.g.
    -- > a ^ 2 = a * a
    -- > a ^ 10 = (a ^ 5) * (a ^ 5) ..
    --(^) :: (IsNatural n) => a -> n -> a
    (^) :: (IsNatural n, Enum n, IDivisible n) => a -> n -> a
    (^) = power

-- | Represent types that supports an euclidian division
--
-- > (x ‘div‘ y) * y + (x ‘mod‘ y) == x
class (Additive a, Multiplicative a) => IDivisible a where
    {-# MINIMAL (div, mod) | divMod #-}
    div :: a -> a -> a
    div a b = fst $ divMod a b
    mod :: a -> a -> a
    mod a b = snd $ divMod a b
    divMod :: a -> a -> (a, a)
    divMod a b = (div a b, mod a b)

-- | Support for division between same types
--
-- This is likely to change to represent specific mathematic divisions
class Multiplicative a => Divisible a where
    {-# MINIMAL (/) #-}
    (/) :: a -> a -> a

infixl 7  *, /
infixr 8  ^

instance Multiplicative Integer where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative Int where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative Int8 where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative Int16 where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative Int32 where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative Int64 where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative Natural where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative Word where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative Word8 where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative Word16 where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative Word32 where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative Word64 where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative Word128 where
    midentity = 1
    (*) = (Word128.*)
instance Multiplicative Word256 where
    midentity = 1
    (*) = (Word256.*)

instance Multiplicative Prelude.Float where
    midentity = 1.0
    (*) = (Prelude.*)
instance Multiplicative Prelude.Double where
    midentity = 1.0
    (*) = (Prelude.*)
instance Multiplicative Prelude.Rational where
    midentity = 1.0
    (*) = (Prelude.*)

instance Multiplicative CChar where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative CSChar where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative CUChar where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative CShort where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative CUShort where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative CInt where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative CUInt where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative CLong where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative CULong where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative CPtrdiff where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative CSize where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative CWchar where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative CSigAtomic where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative CLLong where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative CULLong where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative CIntPtr where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative CUIntPtr where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative CIntMax where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative CUIntMax where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative CClock where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative CTime where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative CUSeconds where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative CSUSeconds where
    midentity = 1
    (*) = (Prelude.*)
instance Multiplicative COff where
    midentity = 1
    (*) = (Prelude.*)

instance Multiplicative CFloat where
    midentity = 1.0
    (*) = (Prelude.*)
instance Multiplicative CDouble where
    midentity = 1.0
    (*) = (Prelude.*)

instance IDivisible Integer where
    div = Prelude.div
    mod = Prelude.mod
instance IDivisible Int where
    div = Prelude.div
    mod = Prelude.mod
instance IDivisible Int8 where
    div = Prelude.div
    mod = Prelude.mod
instance IDivisible Int16 where
    div = Prelude.div
    mod = Prelude.mod
instance IDivisible Int32 where
    div = Prelude.div
    mod = Prelude.mod
instance IDivisible Int64 where
    div = Prelude.div
    mod = Prelude.mod
instance IDivisible Natural where
    div = Prelude.quot
    mod = Prelude.rem
instance IDivisible Word where
    div = Prelude.quot
    mod = Prelude.rem
instance IDivisible Word8 where
    div = Prelude.quot
    mod = Prelude.rem
instance IDivisible Word16 where
    div = Prelude.quot
    mod = Prelude.rem
instance IDivisible Word32 where
    div = Prelude.quot
    mod = Prelude.rem
instance IDivisible Word64 where
    div = Prelude.quot
    mod = Prelude.rem
instance IDivisible Word128 where
    div = Word128.quot
    mod = Word128.rem
instance IDivisible Word256 where
    div = Word256.quot
    mod = Word256.rem

instance IDivisible CChar where
    div = Prelude.quot
    mod = Prelude.rem
instance IDivisible CSChar where
    div = Prelude.quot
    mod = Prelude.rem
instance IDivisible CUChar where
    div = Prelude.quot
    mod = Prelude.rem
instance IDivisible CShort where
    div = Prelude.quot
    mod = Prelude.rem
instance IDivisible CUShort where
    div = Prelude.quot
    mod = Prelude.rem
instance IDivisible CInt where
    div = Prelude.quot
    mod = Prelude.rem
instance IDivisible CUInt where
    div = Prelude.quot
    mod = Prelude.rem
instance IDivisible CLong where
    div = Prelude.quot
    mod = Prelude.rem
instance IDivisible CULong where
    div = Prelude.quot
    mod = Prelude.rem
instance IDivisible CPtrdiff where
    div = Prelude.quot
    mod = Prelude.rem
instance IDivisible CSize where
    div = Prelude.quot
    mod = Prelude.rem
instance IDivisible CWchar where
    div = Prelude.quot
    mod = Prelude.rem
instance IDivisible CSigAtomic where
    div = Prelude.quot
    mod = Prelude.rem
instance IDivisible CLLong where
    div = Prelude.quot
    mod = Prelude.rem
instance IDivisible CULLong where
    div = Prelude.quot
    mod = Prelude.rem
instance IDivisible CIntPtr where
    div = Prelude.quot
    mod = Prelude.rem
instance IDivisible CUIntPtr where
    div = Prelude.quot
    mod = Prelude.rem
instance IDivisible CIntMax where
    div = Prelude.quot
    mod = Prelude.rem
instance IDivisible CUIntMax where
    div = Prelude.quot
    mod = Prelude.rem

instance Divisible Prelude.Rational where
    (/) = (Prelude./)
instance Divisible Float where
    (/) = (Prelude./)
instance Divisible Double where
    (/) = (Prelude./)

instance Divisible CFloat where
    (/) = (Prelude./)
instance Divisible CDouble where
    (/) = (Prelude./)

recip :: Divisible a => a -> a
recip x = midentity / x

power :: (Enum n, IsNatural n, IDivisible n, Multiplicative a) => a -> n -> a
power a n
    | n == 0    = midentity
    | otherwise = squaring midentity a n
  where
    squaring y x i
        | i == 0    = y
        | i == 1    = x * y
        | even i    = squaring y (x*x) (i`div`2)
        | otherwise = squaring (x*y) (x*x) (pred i`div` 2)

even :: (IDivisible n, IsIntegral n) => n -> Bool
even n = (n `mod` 2) == 0

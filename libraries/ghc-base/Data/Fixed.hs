{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Fixed
-- Copyright   :  (c) Ashley Yakeley 2005, 2006, 2009
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ashley Yakeley <ashley@semantic.org>
-- Stability   :  stable
-- Portability :  portable
--
-- This module defines a \"Fixed\" type for fixed-precision arithmetic.
-- The parameter to 'Fixed' is any type that's an instance of 'HasResolution'.
-- 'HasResolution' has a single method that gives the resolution of the 'Fixed'
-- type.
--
-- This module also contains generalisations of 'div', 'mod', and 'divMod' to
-- work with any 'Real' instance.
--
-----------------------------------------------------------------------------

module Data.Fixed
(
    div',mod',divMod',

    Fixed(..), HasResolution(..),
    showFixed,
    E0,Uni,
    E1,Deci,
    E2,Centi,
    E3,Milli,
    E6,Micro,
    E9,Nano,
    E12,Pico
) where

import Data.Data
import GHC.TypeLits (KnownNat, natVal)
import GHC.Read
import Text.ParserCombinators.ReadPrec
import Text.Read.Lex

default () -- avoid any defaulting shenanigans

-- | Generalisation of 'div' to any instance of 'Real'
div' :: (Real a,Integral b) => a -> a -> b
div' n d = floor ((toRational n) / (toRational d))

-- | Generalisation of 'divMod' to any instance of 'Real'
divMod' :: (Real a,Integral b) => a -> a -> (b,a)
divMod' n d = (f,n - (fromIntegral f) * d) where
    f = div' n d

-- | Generalisation of 'mod' to any instance of 'Real'
mod' :: (Real a) => a -> a -> a
mod' n d = n - (fromInteger f) * d where
    f = div' n d

-- | The type parameter should be an instance of 'HasResolution'.
newtype Fixed (a :: k) = MkFixed Integer
        deriving ( Eq  -- ^ @since 2.01
                 , Ord -- ^ @since 2.01
                 )

-- We do this because the automatically derived Data instance requires (Data a) context.
-- Our manual instance has the more general (Typeable a) context.
tyFixed :: DataType
tyFixed = mkDataType "Data.Fixed.Fixed" [conMkFixed]
conMkFixed :: Constr
conMkFixed = mkConstr tyFixed "MkFixed" [] Prefix

-- | @since 4.1.0.0
instance (Typeable k,Typeable a) => Data (Fixed (a :: k)) where
    gfoldl k z (MkFixed a) = k (z MkFixed) a
    gunfold k z _ = k (z MkFixed)
    dataTypeOf _ = tyFixed
    toConstr _ = conMkFixed

class HasResolution (a :: k) where
    resolution :: p a -> Integer

-- | For example, @Fixed 1000@ will give you a 'Fixed' with a resolution of 1000.
instance KnownNat n => HasResolution n where
    resolution _ = natVal (Proxy :: Proxy n)

withType :: (Proxy a -> f a) -> f a
withType foo = foo Proxy

withResolution :: (HasResolution a) => (Integer -> f a) -> f a
withResolution foo = withType (foo . resolution)

-- | @since 2.01
--
-- Recall that, for numeric types, 'succ' and 'pred' typically add and subtract
-- @1@, respectively. This is not true in the case of 'Fixed', whose successor
-- and predecessor functions intuitively return the "next" and "previous" values
-- in the enumeration. The results of these functions thus depend on the
-- resolution of the 'Fixed' value. For example, when enumerating values of
-- resolution @10^-3@ of @type Milli = Fixed E3@,
--
-- @
--   succ (0.000 :: Milli) == 0.001
-- @
--
--
-- and likewise
--
-- @
--   pred (0.000 :: Milli) == -0.001
-- @
--
--
-- In other words, 'succ' and 'pred' increment and decrement a fixed-precision
-- value by the least amount such that the value's resolution is unchanged.
-- For example, @10^-12@ is the smallest (positive) amount that can be added to
-- a value of @type Pico = Fixed E12@ without changing its resolution, and so
--
-- @
--   succ (0.000000000000 :: Pico) == 0.000000000001
-- @
--
--
-- and similarly
--
-- @
--   pred (0.000000000000 :: Pico) == -0.000000000001
-- @
--
--
-- This is worth bearing in mind when defining 'Fixed' arithmetic sequences. In
-- particular, you may be forgiven for thinking the sequence
--
-- @
--   [1..10] :: [Pico]
-- @
--
--
-- evaluates to @[1, 2, 3, 4, 5, 6, 7, 8, 9, 10] :: [Pico]@.
--
-- However, this is not true. On the contrary, similarly to the above
-- implementations of 'succ' and 'pred', @enumFromTo :: Pico -> Pico -> [Pico]@
-- has a "step size" of @10^-12@. Hence, the list @[1..10] :: [Pico]@ has
-- the form
--
-- @
--   [1.000000000000, 1.00000000001, 1.00000000002, ..., 10.000000000000]
-- @
--
--
-- and contains @9 * 10^12 + 1@ values.
instance Enum (Fixed a) where
    succ (MkFixed a) = MkFixed (succ a)
    pred (MkFixed a) = MkFixed (pred a)
    toEnum = MkFixed . toEnum
    fromEnum (MkFixed a) = fromEnum a
    enumFrom (MkFixed a) = fmap MkFixed (enumFrom a)
    enumFromThen (MkFixed a) (MkFixed b) = fmap MkFixed (enumFromThen a b)
    enumFromTo (MkFixed a) (MkFixed b) = fmap MkFixed (enumFromTo a b)
    enumFromThenTo (MkFixed a) (MkFixed b) (MkFixed c) = fmap MkFixed (enumFromThenTo a b c)

-- | @since 2.01
instance (HasResolution a) => Num (Fixed a) where
    (MkFixed a) + (MkFixed b) = MkFixed (a + b)
    (MkFixed a) - (MkFixed b) = MkFixed (a - b)
    fa@(MkFixed a) * (MkFixed b) = MkFixed (div (a * b) (resolution fa))
    negate (MkFixed a) = MkFixed (negate a)
    abs (MkFixed a) = MkFixed (abs a)
    signum (MkFixed a) = fromInteger (signum a)
    fromInteger i = withResolution (\res -> MkFixed (i * res))

-- | @since 2.01
instance (HasResolution a) => Real (Fixed a) where
    toRational fa@(MkFixed a) = (toRational a) / (toRational (resolution fa))

-- | @since 2.01
instance (HasResolution a) => Fractional (Fixed a) where
    fa@(MkFixed a) / (MkFixed b) = MkFixed (div (a * (resolution fa)) b)
    recip fa@(MkFixed a) = MkFixed (div (res * res) a) where
        res = resolution fa
    fromRational r = withResolution (\res -> MkFixed (floor (r * (toRational res))))

-- | @since 2.01
instance (HasResolution a) => RealFrac (Fixed a) where
    properFraction a = (i,a - (fromIntegral i)) where
        i = truncate a
    truncate f = truncate (toRational f)
    round f = round (toRational f)
    ceiling f = ceiling (toRational f)
    floor f = floor (toRational f)

chopZeros :: Integer -> String
chopZeros 0 = ""
chopZeros a | mod a 10 == 0 = chopZeros (div a 10)
chopZeros a = show a

-- only works for positive a
showIntegerZeros :: Bool -> Int -> Integer -> String
showIntegerZeros True _ 0 = ""
showIntegerZeros chopTrailingZeros digits a = replicate (digits - length s) '0' ++ s' where
    s = show a
    s' = if chopTrailingZeros then chopZeros a else s

withDot :: String -> String
withDot "" = ""
withDot s = '.':s

-- | First arg is whether to chop off trailing zeros
showFixed :: (HasResolution a) => Bool -> Fixed a -> String
showFixed chopTrailingZeros fa@(MkFixed a) | a < 0 = "-" ++ (showFixed chopTrailingZeros (asTypeOf (MkFixed (negate a)) fa))
showFixed chopTrailingZeros fa@(MkFixed a) = (show i) ++ (withDot (showIntegerZeros chopTrailingZeros digits fracNum)) where
    res = resolution fa
    (i,d) = divMod a res
    -- enough digits to be unambiguous
    digits = ceiling (logBase 10 (fromInteger res) :: Double)
    maxnum = 10 ^ digits
    -- read floors, so show must ceil for `read . show = id` to hold. See #9240
    fracNum = divCeil (d * maxnum) res
    divCeil x y = (x + y - 1) `div` y

-- | @since 2.01
instance (HasResolution a) => Show (Fixed a) where
    showsPrec p n = showParen (p > 6 && n < 0) $ showString $ showFixed False n

-- | @since 4.3.0.0
instance (HasResolution a) => Read (Fixed a) where
    readPrec     = readNumber convertFixed
    readListPrec = readListPrecDefault
    readList     = readListDefault

convertFixed :: forall a . HasResolution a => Lexeme -> ReadPrec (Fixed a)
convertFixed (Number n)
 | Just (i, f) <- numberToFixed e n =
    return (fromInteger i + (fromInteger f / (10 ^ e)))
    where r = resolution (Proxy :: Proxy a)
          -- round 'e' up to help make the 'read . show == id' property
          -- possible also for cases where 'resolution' is not a
          -- power-of-10, such as e.g. when 'resolution = 128'
          e = ceiling (logBase 10 (fromInteger r) :: Double)
convertFixed _ = pfail

data E0

-- | @since 4.1.0.0
instance HasResolution E0 where
    resolution _ = 1
-- | resolution of 1, this works the same as Integer
type Uni = Fixed E0

data E1

-- | @since 4.1.0.0
instance HasResolution E1 where
    resolution _ = 10
-- | resolution of 10^-1 = .1
type Deci = Fixed E1

data E2

-- | @since 4.1.0.0
instance HasResolution E2 where
    resolution _ = 100
-- | resolution of 10^-2 = .01, useful for many monetary currencies
type Centi = Fixed E2

data E3

-- | @since 4.1.0.0
instance HasResolution E3 where
    resolution _ = 1000
-- | resolution of 10^-3 = .001
type Milli = Fixed E3

data E6

-- | @since 2.01
instance HasResolution E6 where
    resolution _ = 1000000
-- | resolution of 10^-6 = .000001
type Micro = Fixed E6

data E9

-- | @since 4.1.0.0
instance HasResolution E9 where
    resolution _ = 1000000000
-- | resolution of 10^-9 = .000000001
type Nano = Fixed E9

data E12

-- | @since 2.01
instance HasResolution E12 where
    resolution _ = 1000000000000
-- | resolution of 10^-12 = .000000000001
type Pico = Fixed E12

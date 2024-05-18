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
-- This module defines a 'Fixed' type for working with fixed-point arithmetic.
-- Fixed-point arithmetic represents fractional numbers with a fixed number of
-- digits for their fractional part. This is different to the behaviour of the floating-point
-- number types 'Float' and 'Double', because the number of digits of the
-- fractional part of 'Float' and 'Double' numbers depends on the size of the number.
-- Fixed point arithmetic is frequently used in financial mathematics, where they
-- are used for representing decimal currencies.
--
-- The type 'Fixed' is used for fixed-point fractional numbers, which are internally
-- represented as an 'Integer'. The type 'Fixed' takes one parameter, which should implement
-- the typeclass 'HasResolution', to specify the number of digits of the fractional part.
-- This module provides instances of the `HasResolution` typeclass for arbitrary typelevel
-- natural numbers, and for some canonical important fixed-point representations.
--
-- This module also contains generalisations of 'div', 'mod', and 'divMod' to
-- work with any 'Real' instance.
--
-- Automatic conversion between different 'Fixed' can be performed through
-- 'realToFrac', bear in mind that converting to a fixed with a smaller
-- resolution will truncate the number, losing information.
--
-- >>> realToFrac (0.123456 :: Pico) :: Milli
-- 0.123
--
-----------------------------------------------------------------------------

module Data.Fixed
(   -- * The Fixed Type
    Fixed(..), HasResolution(..),
    showFixed,
    -- * Resolution \/ Scaling Factors
    -- | The resolution or scaling factor determines the number of digits in the fractional part.
    --
    -- +------------+----------------------+--------------------------+--------------------------+
    -- | Resolution | Scaling Factor       | Synonym for \"Fixed EX\" | show (12345 :: Fixed EX) |
    -- +============+======================+==========================+==========================+
    -- | E0         | 1\/1                 | Uni                      | 12345.0                  |
    -- +------------+----------------------+--------------------------+--------------------------+
    -- | E1         | 1\/10                | Deci                     | 1234.5                   |
    -- +------------+----------------------+--------------------------+--------------------------+
    -- | E2         | 1\/100               | Centi                    | 123.45                   |
    -- +------------+----------------------+--------------------------+--------------------------+
    -- | E3         | 1\/1 000             | Milli                    | 12.345                   |
    -- +------------+----------------------+--------------------------+--------------------------+
    -- | E6         | 1\/1 000 000         | Micro                    | 0.012345                 |
    -- +------------+----------------------+--------------------------+--------------------------+
    -- | E9         | 1\/1 000 000 000     | Nano                     | 0.000012345              |
    -- +------------+----------------------+--------------------------+--------------------------+
    -- | E12        | 1\/1 000 000 000 000 | Pico                     | 0.000000012345           |
    -- +------------+----------------------+--------------------------+--------------------------+
    --

    -- ** 1\/1
    E0,Uni,
    -- ** 1\/10
    E1,Deci,
    -- ** 1\/100
    E2,Centi,
    -- ** 1\/1 000
    E3,Milli,
    -- ** 1\/1 000 000
    E6,Micro,
    -- ** 1\/1 000 000 000
    E9,Nano,
    -- ** 1\/1 000 000 000 000
    E12,Pico,
    -- * Generalized Functions on Real's
    div',
    mod',
    divMod'
) where

import GHC.Internal.Data.Data
import GHC.Internal.TypeLits (KnownNat, natVal)
import GHC.Internal.Read
import GHC.Internal.Text.ParserCombinators.ReadPrec
import GHC.Internal.Text.Read.Lex
import Data.Typeable
import Prelude

-- $setup
-- >>> import Prelude

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

-- | The type of fixed-point fractional numbers.
--   The type parameter specifies the number of digits of the fractional part and should be an instance of the 'HasResolution' typeclass.
--
-- === __Examples__
--
-- @
--  MkFixed 12345 :: Fixed E3
-- @
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

-- | Types which can be used as a resolution argument to the 'Fixed' type constructor must implement the 'HasResolution'  typeclass.
class HasResolution (a :: k) where
    -- | Provide the resolution for a fixed-point fractional number.
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
-- >>> succ (0.000 :: Milli)
-- 0.001
--
-- and likewise
--
-- >>> pred (0.000 :: Milli)
-- -0.001
--
-- In other words, 'succ' and 'pred' increment and decrement a fixed-precision
-- value by the least amount such that the value's resolution is unchanged.
-- For example, @10^-12@ is the smallest (positive) amount that can be added to
-- a value of @type Pico = Fixed E12@ without changing its resolution, and so
--
-- >>> succ (0.000000000000 :: Pico)
-- 0.000000000001
--
-- and similarly
--
-- >>> pred (0.000000000000 :: Pico)
-- -0.000000000001
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
--
-- Multiplication is not associative or distributive:
--
-- >>> (0.2 * 0.6 :: Deci) * 0.9 == 0.2 * (0.6 * 0.9)
-- False
--
-- >>> (0.1 + 0.1 :: Deci) * 0.5 == 0.1 * 0.5 + 0.1 * 0.5
-- False
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
--
-- === __Examples__
--
-- >>> showFixed True  (MkFixed 10000 :: Fixed E3)
-- "10"
--
-- >>> showFixed False (MkFixed 10000 :: Fixed E3)
-- "10.000"
--
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

-- | Resolution of 1, this works the same as Integer.
data E0

-- | @since 4.1.0.0
instance HasResolution E0 where
    resolution _ = 1

-- | Resolution of 1, this works the same as Integer.
--
-- === __Examples__
--
-- >>> show (MkFixed 12345 :: Fixed E0)
-- "12345.0"
--
-- >>> show (MkFixed 12345 :: Uni)
-- "12345.0"
--
type Uni = Fixed E0

-- | Resolution of 10^-1 = .1
data E1

-- | @since 4.1.0.0
instance HasResolution E1 where
    resolution _ = 10

-- | Resolution of 10^-1 = .1
--
-- === __Examples__
--
-- >>> show (MkFixed 12345 :: Fixed E1)
-- "1234.5"
--
-- >>> show (MkFixed 12345 :: Deci)
-- "1234.5"
--
type Deci = Fixed E1

-- | Resolution of 10^-2 = .01, useful for many monetary currencies
data E2

-- | @since 4.1.0.0
instance HasResolution E2 where
    resolution _ = 100

-- | Resolution of 10^-2 = .01, useful for many monetary currencies
--
-- === __Examples__
--
-- >>> show (MkFixed 12345 :: Fixed E2)
-- "123.45"
--
-- >>> show (MkFixed 12345 :: Centi)
-- "123.45"
--
type Centi = Fixed E2

-- | Resolution of 10^-3 = .001
data E3

-- | @since 4.1.0.0
instance HasResolution E3 where
    resolution _ = 1000

-- | Resolution of 10^-3 = .001
--
-- === __Examples__
--
-- >>> show (MkFixed 12345 :: Fixed E3)
-- "12.345"
--
-- >>> show (MkFixed 12345 :: Milli)
-- "12.345"
--
type Milli = Fixed E3

-- | Resolution of 10^-6 = .000001
data E6

-- | @since 2.01
instance HasResolution E6 where
    resolution _ = 1000000

-- | Resolution of 10^-6 = .000001
--
-- === __Examples__
--
-- >>> show (MkFixed 12345 :: Fixed E6)
-- "0.012345"
--
-- >>> show (MkFixed 12345 :: Micro)
-- "0.012345"
--
type Micro = Fixed E6

-- | Resolution of 10^-9 = .000000001
data E9

-- | @since 4.1.0.0
instance HasResolution E9 where
    resolution _ = 1000000000

-- | Resolution of 10^-9 = .000000001
--
-- === __Examples__
--
-- >>> show (MkFixed 12345 :: Fixed E9)
-- "0.000012345"
--
-- >>> show (MkFixed 12345 :: Nano)
-- "0.000012345"
--
type Nano = Fixed E9

-- | Resolution of 10^-12 = .000000000001
data E12

-- | @since 2.01
instance HasResolution E12 where
    resolution _ = 1000000000000

-- | Resolution of 10^-12 = .000000000001
--
-- === __Examples__
--
-- >>> show (MkFixed 12345 :: Fixed E12)
-- "0.000000012345"
--
-- >>> show (MkFixed 12345 :: Pico)
-- "0.000000012345"
--
type Pico = Fixed E12

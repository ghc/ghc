{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, MagicHash, UnboxedTuples #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Num
-- Copyright   :  (c) The University of Glasgow 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The 'Num' class and the 'Integer' type.
--
-----------------------------------------------------------------------------


module GHC.Num
   ( module GHC.Num
   , module GHC.Num.Integer
   , module GHC.Num.Natural
    -- reexported for backward compatibility
   , module GHC.Natural
   , module GHC.Integer
   )
where

#include "MachDeps.h"

import qualified GHC.Natural
import qualified GHC.Integer

import GHC.Base
import GHC.Num.Integer
import GHC.Num.Natural

infixl 7  *
infixl 6  +, -

default ()              -- Double isn't available yet,
                        -- and we shouldn't be using defaults anyway

-- | Basic numeric class.
--
-- The Haskell Report defines no laws for 'Num'. However, @('+')@ and @('*')@ are
-- customarily expected to define a ring and have the following properties:
--
-- [__Associativity of @('+')@__]: @(x + y) + z@ = @x + (y + z)@
-- [__Commutativity of @('+')@__]: @x + y@ = @y + x@
-- [__@'fromInteger' 0@ is the additive identity__]: @x + fromInteger 0@ = @x@
-- [__'negate' gives the additive inverse__]: @x + negate x@ = @fromInteger 0@
-- [__Associativity of @('*')@__]: @(x * y) * z@ = @x * (y * z)@
-- [__@'fromInteger' 1@ is the multiplicative identity__]:
-- @x * fromInteger 1@ = @x@ and @fromInteger 1 * x@ = @x@
-- [__Distributivity of @('*')@ with respect to @('+')@__]:
-- @a * (b + c)@ = @(a * b) + (a * c)@ and @(b + c) * a@ = @(b * a) + (c * a)@
--
-- Note that it /isn't/ customarily expected that a type instance of both 'Num'
-- and 'Ord' implement an ordered ring. Indeed, in @base@ only 'Integer' and
-- 'Data.Ratio.Rational' do.
class  Num a  where
    {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}

    (+), (-), (*)       :: a -> a -> a
    -- | Unary negation.
    negate              :: a -> a
    -- | Absolute value.
    abs                 :: a -> a
    -- | Sign of a number.
    -- The functions 'abs' and 'signum' should satisfy the law:
    --
    -- > abs x * signum x == x
    --
    -- For real numbers, the 'signum' is either @-1@ (negative), @0@ (zero)
    -- or @1@ (positive).
    signum              :: a -> a
    -- | Conversion from an 'Integer'.
    -- An integer literal represents the application of the function
    -- 'fromInteger' to the appropriate value of type 'Integer',
    -- so such literals have type @('Num' a) => a@.
    fromInteger         :: Integer -> a

    {-# INLINE (-) #-}
    {-# INLINE negate #-}
    x - y               = x + negate y
    negate x            = 0 - x

-- | the same as @'flip' ('-')@.
--
-- Because @-@ is treated specially in the Haskell grammar,
-- @(-@ /e/@)@ is not a section, but an application of prefix negation.
-- However, @('subtract'@ /exp/@)@ is equivalent to the disallowed section.
{-# INLINE subtract #-}
subtract :: (Num a) => a -> a -> a
subtract x y = y - x

-- | @since 2.01
instance Num Int where
    I# x + I# y = I# (x +# y)
    I# x - I# y = I# (x -# y)
    negate (I# x) = I# (negateInt# x)
    I# x * I# y = I# (x *# y)
    abs n  = if n `geInt` 0 then n else negate n

    signum n | n `ltInt` 0 = negate 1
             | n `eqInt` 0 = 0
             | otherwise   = 1

    fromInteger i = I# (integerToInt# i)

-- | @since 2.01
instance Num Word where
    (W# x#) + (W# y#)      = W# (x# `plusWord#` y#)
    (W# x#) - (W# y#)      = W# (x# `minusWord#` y#)
    (W# x#) * (W# y#)      = W# (x# `timesWord#` y#)
    negate (W# x#)         = W# (int2Word# (negateInt# (word2Int# x#)))
    abs x                  = x
    signum 0               = 0
    signum _               = 1
    fromInteger i          = W# (integerToWord# i)

-- | @since 2.01
instance Num Integer where
    (+) = integerAdd
    (-) = integerSub
    (*) = integerMul
    negate         = integerNegate
    fromInteger i  = i

    abs    = integerAbs
    signum = integerSignum

-- | Note that `Natural`'s 'Num' instance isn't a ring: no element but 0 has an
-- additive inverse. It is a semiring though.
--
-- @since 4.8.0.0
instance Num Natural where
    (+)         = naturalAdd
    (-)         = naturalSubThrow
    (*)         = naturalMul
    negate      = naturalNegate
    fromInteger i = integerToNaturalThrow i
    abs         = id
    signum      = naturalSignum

{-# DEPRECATED quotRemInteger "Use integerQuotRem# instead" #-}
quotRemInteger :: Integer -> Integer -> (# Integer, Integer #)
quotRemInteger = integerQuotRem#


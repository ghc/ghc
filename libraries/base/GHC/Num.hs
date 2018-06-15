{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, MagicHash, UnboxedTuples #-}
{-# OPTIONS_HADDOCK hide #-}

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


module GHC.Num (module GHC.Num, module GHC.Integer, module GHC.Natural) where

#include "MachDeps.h"

import GHC.Base
import GHC.Integer
import GHC.Natural
#if !defined(MIN_VERSION_integer_gmp)
import {-# SOURCE #-} GHC.Exception.Type (underflowException)
#endif

infixl 7  *
infixl 6  +, -

default ()              -- Double isn't available yet,
                        -- and we shouldn't be using defaults anyway

-- | Basic numeric class.
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
instance  Num Int  where
    I# x + I# y = I# (x +# y)
    I# x - I# y = I# (x -# y)
    negate (I# x) = I# (negateInt# x)
    I# x * I# y = I# (x *# y)
    abs n  = if n `geInt` 0 then n else negate n

    signum n | n `ltInt` 0 = negate 1
             | n `eqInt` 0 = 0
             | otherwise   = 1

    {-# INLINE fromInteger #-}   -- Just to be sure!
    fromInteger i = I# (integerToInt i)

-- | @since 2.01
instance Num Word where
    (W# x#) + (W# y#)      = W# (x# `plusWord#` y#)
    (W# x#) - (W# y#)      = W# (x# `minusWord#` y#)
    (W# x#) * (W# y#)      = W# (x# `timesWord#` y#)
    negate (W# x#)         = W# (int2Word# (negateInt# (word2Int# x#)))
    abs x                  = x
    signum 0               = 0
    signum _               = 1
    fromInteger i          = W# (integerToWord i)

-- | @since 2.01
instance  Num Integer  where
    (+) = plusInteger
    (-) = minusInteger
    (*) = timesInteger
    negate         = negateInteger
    fromInteger x  =  x

    abs = absInteger
    signum = signumInteger

#if defined(MIN_VERSION_integer_gmp)
-- | @since 4.8.0.0
instance  Num Natural  where
    (+) = plusNatural
    (-) = minusNatural
    (*) = timesNatural
    negate      = negateNatural
    fromInteger = naturalFromInteger

    abs = id
    signum = signumNatural

#else
-- | @since 4.8.0.0
instance Num Natural where
  Natural n + Natural m = Natural (n + m)
  {-# INLINE (+) #-}
  Natural n * Natural m = Natural (n * m)
  {-# INLINE (*) #-}
  Natural n - Natural m
      | m > n     = raise# underflowException
      | otherwise = Natural (n - m)
  {-# INLINE (-) #-}
  abs (Natural n) = Natural n
  {-# INLINE abs #-}
  signum (Natural n) = Natural (signum n)
  {-# INLINE signum #-}
  fromInteger = naturalFromInteger
  {-# INLINE fromInteger #-}

#endif

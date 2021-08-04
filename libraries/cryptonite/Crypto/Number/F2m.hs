-- |
-- Module      : Crypto.Math.F2m
-- License     : BSD-style
-- Maintainer  : Danny Navarro <j@dannynavarro.net>
-- Stability   : experimental
-- Portability : Good
--
-- This module provides basic arithmetic operations over F₂m. Performance is
-- not optimal and it doesn't provide protection against timing
-- attacks. The 'm' parameter is implicitly derived from the irreducible
-- polynomial where applicable.

module Crypto.Number.F2m
    ( BinaryPolynomial
    , addF2m
    , mulF2m
    , squareF2m'
    , squareF2m
    , powF2m
    , modF2m
    , sqrtF2m
    , invF2m
    , divF2m
    ) where

import Data.Bits (xor, shift, testBit, setBit)
import Data.List
import Crypto.Number.Basic

-- | Binary Polynomial represented by an integer
type BinaryPolynomial = Integer

-- | Addition over F₂m. This is just a synonym of 'xor'.
addF2m :: Integer
       -> Integer
       -> Integer
addF2m = xor
{-# INLINE addF2m #-}

-- | Reduction by modulo over F₂m.
--
-- This function is undefined for negative arguments, because their bit
-- representation is platform-dependent. Zero modulus is also prohibited.
modF2m :: BinaryPolynomial -- ^ Modulus
       -> Integer
       -> Integer
modF2m fx i
    | fx < 0 || i < 0 = error "modF2m: negative number represent no binary polynomial"
    | fx == 0         = error "modF2m: cannot divide by zero polynomial"
    | fx == 1         = 0
    | otherwise       = go i
      where
        lfx = log2 fx
        go n | s == 0    = n `addF2m` fx
             | s < 0     = n
             | otherwise = go $ n `addF2m` shift fx s
                where s = log2 n - lfx
{-# INLINE modF2m #-}

-- | Multiplication over F₂m.
--
-- This function is undefined for negative arguments, because their bit
-- representation is platform-dependent. Zero modulus is also prohibited.
mulF2m :: BinaryPolynomial -- ^ Modulus
       -> Integer
       -> Integer
       -> Integer
mulF2m fx n1 n2
    |    fx < 0
      || n1 < 0
      || n2 < 0 = error "mulF2m: negative number represent no binary polynomial"
    | fx == 0   = error "mulF2m: cannot multiply modulo zero polynomial"
    | otherwise = modF2m fx $ go (if n2 `mod` 2 == 1 then n1 else 0) (log2 n2)
      where
        go n s | s == 0  = n
               | otherwise = if testBit n2 s
                                then go (n `addF2m` shift n1 s) (s - 1)
                                else go n (s - 1)
{-# INLINABLE mulF2m #-}

-- | Squaring over F₂m.
--
-- This function is undefined for negative arguments, because their bit
-- representation is platform-dependent. Zero modulus is also prohibited.
squareF2m :: BinaryPolynomial -- ^ Modulus
          -> Integer
          -> Integer
squareF2m fx = modF2m fx . squareF2m'
{-# INLINE squareF2m #-}

-- | Squaring over F₂m without reduction by modulo.
--
-- The implementation utilizes the fact that for binary polynomial S(x) we have
-- S(x)^2 = S(x^2). In other words, insert a zero bit between every bits of argument: 1101 -> 1010001.
--
-- This function is undefined for negative arguments, because their bit
-- representation is platform-dependent.
squareF2m' :: Integer
           -> Integer
squareF2m' n
    | n < 0     = error "mulF2m: negative number represent no binary polynomial"
    | otherwise = foldl' (\acc s -> if testBit n s then setBit acc (2 * s) else acc) 0 [0 .. log2 n]
{-# INLINE squareF2m' #-}

-- | Exponentiation in F₂m by computing @a^b mod fx@.
--
-- This implements an exponentiation by squaring based solution. It inherits the
-- same restrictions as 'squareF2m'. Negative exponents are disallowed.
powF2m :: BinaryPolynomial -- ^Modulus
       -> Integer          -- ^a
       -> Integer          -- ^b
       -> Integer
powF2m fx a b
  | b < 0     = error "powF2m: negative exponents disallowed"
  | b == 0    = if fx > 1 then 1 else 0
  | even b    = squareF2m fx x
  | otherwise = mulF2m fx a (squareF2m' x)
  where x = powF2m fx a (b `div` 2)

-- | Square rooot in F₂m.
--
-- We exploit the fact that @a^(2^m) = a@, or in particular, @a^(2^m - 1) = 1@
-- from a classical result by Lagrange. Thus the square root is simply @a^(2^(m
-- - 1))@.
sqrtF2m :: BinaryPolynomial -- ^Modulus
        -> Integer          -- ^a
        -> Integer
sqrtF2m fx a = go (log2 fx - 1) a
  where go 0 x = x
        go n x = go (n - 1) (squareF2m fx x)

-- | Extended GCD algorithm for polynomials. For @a@ and @b@ returns @(g, u, v)@ such that @a * u + b * v == g@.
--
-- Reference: https://en.wikipedia.org/wiki/Polynomial_greatest_common_divisor#B.C3.A9zout.27s_identity_and_extended_GCD_algorithm
gcdF2m :: Integer
       -> Integer
       -> (Integer, Integer, Integer)
gcdF2m a b = go (a, b, 1, 0, 0, 1)
  where
    go (g, 0, u, _, v, _)
        = (g, u, v)
    go (r0, r1, s0, s1, t0, t1)
        = go (r1, r0 `addF2m` shift r1 j, s1, s0 `addF2m` shift s1 j, t1, t0 `addF2m` shift t1 j)
            where j = max 0 (log2 r0 - log2 r1)

-- | Modular inversion over F₂m.
-- If @n@ doesn't have an inverse, 'Nothing' is returned.
--
-- This function is undefined for negative arguments, because their bit
-- representation is platform-dependent. Zero modulus is also prohibited.
invF2m :: BinaryPolynomial -- ^ Modulus
       -> Integer
       -> Maybe Integer
invF2m fx n = if g == 1 then Just (modF2m fx u) else Nothing
  where
    (g, u, _) = gcdF2m n fx
{-# INLINABLE invF2m #-}

-- | Division over F₂m. If the dividend doesn't have an inverse it returns
-- 'Nothing'.
--
-- This function is undefined for negative arguments, because their bit
-- representation is platform-dependent. Zero modulus is also prohibited.
divF2m :: BinaryPolynomial -- ^ Modulus
       -> Integer          -- ^ Dividend
       -> Integer          -- ^ Divisor
       -> Maybe Integer    -- ^ Quotient
divF2m fx n1 n2 = mulF2m fx n1 <$> invF2m fx n2
{-# INLINE divF2m #-}

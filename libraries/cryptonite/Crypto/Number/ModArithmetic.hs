{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Crypto.Number.ModArithmetic
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good

module Crypto.Number.ModArithmetic
    (
    -- * Exponentiation
      expSafe
    , expFast
    -- * Inverse computing
    , inverse
    , inverseCoprimes
    , inverseFermat
    -- * Squares
    , jacobi
    , squareRoot
    ) where

import Control.Exception (throw, Exception)
import Crypto.Number.Basic
import Crypto.Number.Compat

-- | Raised when two numbers are supposed to be coprimes but are not.
data CoprimesAssertionError = CoprimesAssertionError
    deriving (Show)

instance Exception CoprimesAssertionError

-- | Compute the modular exponentiation of base^exponent using
-- algorithms design to avoid side channels and timing measurement
--
-- Modulo need to be odd otherwise the normal fast modular exponentiation
-- is used.
--
-- When used with integer-simple, this function is not different
-- from expFast, and thus provide the same unstudied and dubious
-- timing and side channels claims.
--
-- Before GHC 8.4.2, powModSecInteger is missing from integer-gmp,
-- so expSafe has the same security as expFast.
expSafe :: Integer -- ^ base
        -> Integer -- ^ exponent
        -> Integer -- ^ modulo
        -> Integer -- ^ result
expSafe b e m
    | odd m     = gmpPowModSecInteger b e m `onGmpUnsupported`
                  (gmpPowModInteger b e m   `onGmpUnsupported`
                  exponentiation b e m)
    | otherwise = gmpPowModInteger b e m    `onGmpUnsupported`
                  exponentiation b e m

-- | Compute the modular exponentiation of base^exponent using
-- the fastest algorithm without any consideration for
-- hiding parameters.
--
-- Use this function when all the parameters are public,
-- otherwise 'expSafe' should be preferred.
expFast :: Integer -- ^ base
        -> Integer -- ^ exponent
        -> Integer -- ^ modulo
        -> Integer -- ^ result
expFast b e m = gmpPowModInteger b e m `onGmpUnsupported` exponentiation b e m

-- | @exponentiation@ computes modular exponentiation as /b^e mod m/
-- using repetitive squaring.
exponentiation :: Integer -> Integer -> Integer -> Integer
exponentiation b e m
    | b == 1    = b
    | e == 0    = 1
    | e == 1    = b `mod` m
    | even e    = let p = exponentiation b (e `div` 2) m `mod` m
                   in (p^(2::Integer)) `mod` m
    | otherwise = (b * exponentiation b (e-1) m) `mod` m

-- | @inverse@ computes the modular inverse as in /g^(-1) mod m/.
inverse :: Integer -> Integer -> Maybe Integer
inverse g m = gmpInverse g m `onGmpUnsupported` v
  where
    v
        | d > 1     = Nothing
        | otherwise = Just (x `mod` m)
    (x,_,d) = gcde g m

-- | Compute the modular inverse of two coprime numbers.
-- This is equivalent to inverse except that the result
-- is known to exists.
--
-- If the numbers are not defined as coprime, this function
-- will raise a 'CoprimesAssertionError'.
inverseCoprimes :: Integer -> Integer -> Integer
inverseCoprimes g m =
    case inverse g m of
        Nothing -> throw CoprimesAssertionError
        Just i  -> i

-- | Computes the Jacobi symbol (a/n).
-- 0 ≤ a < n; n ≥ 3 and odd.
--
-- The Legendre and Jacobi symbols are indistinguishable exactly when the
-- lower argument is an odd prime, in which case they have the same value.
--
-- See algorithm 2.149 in "Handbook of Applied Cryptography" by Alfred J. Menezes et al.
jacobi :: Integer -> Integer -> Maybe Integer
jacobi a n
    | n < 3 || even n  = Nothing
    | a == 0 || a == 1 = Just a
    | n <= a           = jacobi (a `mod` n) n
    | a < 0            =
      let b = if n `mod` 4 == 1 then 1 else -1
       in fmap (*b) (jacobi (-a) n)
    | otherwise        =
      let (e, a1) = asPowerOf2AndOdd a
          nMod8   = n `mod` 8
          nMod4   = n `mod` 4
          a1Mod4  = a1 `mod` 4
          s'      = if even e || nMod8 == 1 || nMod8 == 7 then 1 else -1
          s       = if nMod4 == 3 && a1Mod4 == 3 then -s' else s'
          n1      = n `mod` a1
       in if a1 == 1 then Just s
          else fmap (*s) (jacobi n1 a1)

-- | Modular inverse using Fermat's little theorem.  This works only when
-- the modulus is prime but avoids side channels like in 'expSafe'.
inverseFermat :: Integer -> Integer -> Integer
inverseFermat g p = expSafe g (p - 2) p

-- | Raised when the assumption about the modulus is invalid.
data ModulusAssertionError = ModulusAssertionError
    deriving (Show)

instance Exception ModulusAssertionError

-- | Modular square root of @g@ modulo a prime @p@.
--
-- If the modulus is found not to be prime, the function will raise a
-- 'ModulusAssertionError'.
--
-- This implementation is variable time and should be used with public
-- parameters only.
squareRoot :: Integer -> Integer -> Maybe Integer
squareRoot p
    | p < 2     = throw ModulusAssertionError
    | otherwise =
        case p `divMod` 8 of
           (v, 3) -> method1 (2 * v + 1)
           (v, 7) -> method1 (2 * v + 2)
           (u, 5) -> method2 u
           (_, 1) -> tonelliShanks p
           (0, 2) -> \a -> Just (if even a then 0 else 1)
           _      -> throw ModulusAssertionError

  where
    x `eqMod` y = (x - y) `mod` p == 0

    validate g y | (y * y) `eqMod` g = Just y
                 | otherwise         = Nothing

    -- p == 4u + 3 and u' == u + 1
    method1 u' g =
        let y = expFast g u' p
         in validate g y

    -- p == 8u + 5
    method2 u g =
        let gamma = expFast (2 * g) u p
            g_gamma = g * gamma
            i = (2 * g_gamma * gamma) `mod` p
            y = (g_gamma * (i - 1)) `mod` p
         in validate g y

tonelliShanks :: Integer -> Integer -> Maybe Integer
tonelliShanks p a
    | aa == 0   = Just 0
    | otherwise =
        case expFast aa p2 p of
            b | b == p1   -> Nothing
              | b == 1    -> Just $ go (expFast aa ((s + 1) `div` 2) p)
                                       (expFast aa s p)
                                       (expFast n  s p)
                                       e
              | otherwise -> throw ModulusAssertionError
  where
    aa = a `mod` p
    p1 = p - 1
    p2 = p1 `div` 2
    n  = findN 2

    x `mul` y = (x * y) `mod` p

    pow2m 0 x = x
    pow2m i x = pow2m (i - 1) (x `mul` x)

    (e, s) = asPowerOf2AndOdd p1

    -- find a quadratic non-residue
    findN i
        | expFast i p2 p == p1 = i
        | otherwise            = findN (i + 1)

    -- find m such that b^(2^m) == 1 (mod p)
    findM b i
        | b == 1    = i
        | otherwise = findM (b `mul` b) (i + 1)

    go !x b g !r
        | b == 1    = x
        | otherwise =
            let r' = findM b 0
                z = pow2m (r - r' - 1) g
                x' = x `mul` z
                b' = b `mul` g'
                g' = z `mul` z
             in go x' b' g' r'

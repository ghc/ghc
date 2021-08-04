-- |
-- Module      : Crypto.Number.Basic
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good

{-# LANGUAGE BangPatterns #-}
module Crypto.Number.Basic
    ( sqrti
    , gcde
    , areEven
    , log2
    , numBits
    , numBytes
    , asPowerOf2AndOdd
    ) where

import Data.Bits

import Crypto.Number.Compat

-- | @sqrti@ returns two integers @(l,b)@ so that @l <= sqrt i <= b@.
-- The implementation is quite naive, use an approximation for the first number
-- and use a dichotomy algorithm to compute the bound relatively efficiently.
sqrti :: Integer -> (Integer, Integer)
sqrti i
    | i < 0     = error "cannot compute negative square root"
    | i == 0    = (0,0)
    | i == 1    = (1,1)
    | i == 2    = (1,2)
    | otherwise = loop x0
        where
            nbdigits = length $ show i
            x0n = (if even nbdigits then nbdigits - 2 else nbdigits - 1) `div` 2
            x0  = if even nbdigits then 2 * 10 ^ x0n else 6 * 10 ^ x0n
            loop x = case compare (sq x) i of
                LT -> iterUp x
                EQ -> (x, x)
                GT -> iterDown x
            iterUp lb = if sq ub >= i then iter lb ub else iterUp ub
                where ub = lb * 2
            iterDown ub = if sq lb >= i then iterDown lb else iter lb ub
                where lb = ub `div` 2
            iter lb ub
                | lb == ub   = (lb, ub)
                | lb+1 == ub = (lb, ub)
                | otherwise  =
                    let d = (ub - lb) `div` 2 in
                    if sq (lb + d) >= i
                        then iter lb (ub-d)
                        else iter (lb+d) ub
            sq a = a * a

-- | Get the extended GCD of two integer using integer divMod
--
-- gcde 'a' 'b' find (x,y,gcd(a,b)) where ax + by = d
--
gcde :: Integer -> Integer -> (Integer, Integer, Integer)
gcde a b = onGmpUnsupported (gmpGcde a b) $
    if d < 0 then (-x,-y,-d) else (x,y,d)
  where
    (d, x, y)                     = f (a,1,0) (b,0,1)
    f t              (0, _, _)    = t
    f (a', sa, ta) t@(b', sb, tb) =
        let (q, r) = a' `divMod` b' in
        f t (r, sa - (q * sb), ta - (q * tb))

-- | Check if a list of integer are all even
areEven :: [Integer] -> Bool
areEven = and . map even

-- | Compute the binary logarithm of a integer
log2 :: Integer -> Int
log2 n = onGmpUnsupported (gmpLog2 n) $ imLog 2 n
  where
    -- http://www.haskell.org/pipermail/haskell-cafe/2008-February/039465.html
    imLog b x = if x < b then 0 else (x `div` b^l) `doDiv` l
      where
        l = 2 * imLog (b * b) x
        doDiv x' l' = if x' < b then l' else (x' `div` b) `doDiv` (l' + 1)
{-# INLINE log2 #-}

-- | Compute the number of bits for an integer
numBits :: Integer -> Int
numBits n = gmpSizeInBits n `onGmpUnsupported` (if n == 0 then 1 else computeBits 0 n)
  where computeBits !acc i
            | q == 0 =
                if r >= 0x80 then acc+8
                else if r >= 0x40 then acc+7
                else if r >= 0x20 then acc+6
                else if r >= 0x10 then acc+5
                else if r >= 0x08 then acc+4
                else if r >= 0x04 then acc+3
                else if r >= 0x02 then acc+2
                else if r >= 0x01 then acc+1
                else acc -- should be catch by previous loop
            | otherwise = computeBits (acc+8) q
          where (q,r) = i `divMod` 256

-- | Compute the number of bytes for an integer
numBytes :: Integer -> Int
numBytes n = gmpSizeInBytes n `onGmpUnsupported` ((numBits n + 7) `div` 8)

-- | Express an integer as an odd number and a power of 2
asPowerOf2AndOdd :: Integer -> (Int, Integer)
asPowerOf2AndOdd a
    | a == 0       = (0, 0)
    | odd a        = (0, a)
    | a < 0        = let (e, a1) = asPowerOf2AndOdd $ abs a in (e, -a1)
    | isPowerOf2 a = (log2 a, 1)
    | otherwise    = loop a 0
        where      
          isPowerOf2 n = (n /= 0) && ((n .&. (n - 1)) == 0)
          loop n pw = if n `mod` 2 == 0 then loop (n `div` 2) (pw + 1)
                      else (pw, n)
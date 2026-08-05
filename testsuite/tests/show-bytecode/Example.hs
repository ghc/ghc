{-# LANGUAGE StaticPointers #-}

-- | This module uses in particular the following features:
--
--     * Local variables defined in `where` clauses
--     * Integer literals
--     * Infix operators
--     * Recursion
--     * Static pointers
--     * Algebraic-datatype declarations
--     * Foreign import declarations
module Example where

import Numeric.Natural (Natural)
import Foreign.Ptr (Ptr)
import Foreign.C.Types (CChar, CSize (CSize))
import GHC.StaticPtr (StaticPtr)

fibonaccis :: [Natural]
fibonaccis = 0 : positiveFibonaccis where

    positiveFibonaccis :: [Natural]
    positiveFibonaccis = 1 : zipWith (+) fibonaccis positiveFibonaccis

fibonaccisPtr :: StaticPtr [Natural]
fibonaccisPtr = static fibonaccis

divides :: Integral a => a -> a -> Bool
k `divides` n = n `mod` k == 0

primes :: [Natural]
primes = 2 : filter isPrime [3 ..] where

    isPrime :: Natural -> Bool
    isPrime n = not (any (`divides` n) (takeWhile ((<= n) . (^ 2)) primes))

primesPtr :: StaticPtr [Natural]
primesPtr = static primes

data BinTree a b = Leaf a | Node (BinTree a b) b (BinTree a b)

data PerfectTree a = PerfectTree a | Nested (PerfectTree (a, a))

foreign import ccall "string.h strlen"
    cstrlen :: Ptr CChar -> IO CSize

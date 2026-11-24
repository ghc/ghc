-- | A reduced variant of the example used by the @show-bytecode@ tests.
module Example where

import Numeric.Natural (Natural)

fibonaccis :: [Natural]
fibonaccis = 0 : positiveFibonaccis where

    positiveFibonaccis :: [Natural]
    positiveFibonaccis = 1 : zipWith (+) fibonaccis positiveFibonaccis

divides :: Integral a => a -> a -> Bool
k `divides` n = n `mod` k == 0

primes :: [Natural]
primes = 2 : filter isPrime [3 ..] where

    isPrime :: Natural -> Bool
    isPrime n = not (any (`divides` n) (takeWhile ((<= n) . (^ 2)) primes))

data BinTree a b = Leaf a | Node (BinTree a b) b (BinTree a b)

data PerfectTree a = PerfectTree a | Nested (PerfectTree (a, a))

module SumEulerPrimes where

import Data.List

sumPhi n = sum (map phiOpt [1..n])

phiOpt :: Int -> Int
phiOpt 1 = 0
phiOpt n = foldl (*) 1  [ (p-1)*p^(k-1)
                          | (p,k) <- primefactors n ]

-- factorise n to a list of (prime, multiplicity)
primefactors :: Int -> [(Int,Int)]
primefactors n | n <= 1    = []
               | otherwise = primeList (primesIn primes n)

-- gather identical primes in the list
primeList :: [Int] -> [(Int,Int)]
primeList ps = [ (x, length (filter (==x) ps))
                 | x <- nub ps ]

-- brute-force factorisation, using precomputed prime list
primesIn :: [Int] -> Int -> [Int]
primesIn [] _ = error "no primes left!"
primesIn ps@(p:rest) n | p > n          = []
                       | n `mod` p == 0 = p:primesIn ps (n `div` p)
                       | otherwise      = primesIn rest n

-- prime numbers, by sieve of Eratosthenes
primes :: [Int]
primes = sieve [2..]
sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = x: (sieve (filter (not . multiple) xs))
    where multiple y = rem y x == 0


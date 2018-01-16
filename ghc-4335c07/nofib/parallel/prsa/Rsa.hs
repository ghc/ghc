{-# LANGUAGE BangPatterns,CPP #-}
module Rsa (encrypt, decrypt, makeKeys)
where

import Control.Parallel
import Control.Parallel.Strategies

encrypt, decrypt :: Integer -> Integer -> String -> String
encrypt n e = unlines . parallel . map (show . power e n . code) . collect (size n)
decrypt n d = concat . parallel . map (decode . power d n . read) . lines

-------- Parallelism -----------

#ifdef STRATEGIES_2
parallel = parBuffer 100 rwhnf
#else
parallel = withStrategy (parBuffer 100 rseq)
#endif


parmap :: (String -> String) -> [String] -> [String]
parmap f [] = []
parmap f (x:xs) = fx `par` (pmxs `par` (fx:pmxs))
   where fx = f x
         pmxs = parmap f xs


-------- Converting between Strings and Integers -----------

code :: String -> Integer
code = foldl accum 0
  where accum x y = (128 * x) + fromIntegral (fromEnum y)

decode :: Integer -> String
decode n = reverse (expand n)
   where expand 0 = []
         expand x = toEnum (fromIntegral (x `mod` 128)) : expand (x `div` 128)

collect :: Int -> [a] -> [[a]]
collect 0 xs = []
collect n [] = []
collect n xs = take n xs : collect n (drop n xs)

size :: Integer -> Int
size n = (length (show n) * 47) `div` 100	-- log_128 10 = 0.4745


------- Constructing keys -------------------------

makeKeys :: Integer -> Integer -> (Integer, Integer, Integer)
makeKeys r s = (p*q, d, invert ((p-1)*(q-1)) d)
   where   p = nextPrime r
           q = nextPrime s
	   d = nextPrime (p+q+1)

nextPrime :: Integer -> Integer
nextPrime a = head (filter prime [odd,odd+2..])
  where  odd | even a = a+1
             | True   = a
         prime p = and [power (p-1) p x == 1 | x <- [3,5,7]]

invert :: Integer -> Integer -> Integer
invert n a = if e<0 then e+n else e
  where  e=iter n 0 a 1

iter :: Integer -> Integer -> Integer -> Integer -> Integer
iter g v 0 w = v
iter g v h w = iter h w (g `mod` h) (v - (g `div` h)*w)

------- Fast exponentiation, mod m -----------------

power :: Integer -> Integer -> Integer -> Integer
power 0 m x          = 1
power n m x | even n = sqr (power (n `div` 2) m x) `mod` m
	    | True   = (x * power (n-1) m x) `mod` m

sqr :: Integer -> Integer
sqr x = x * x



> module Fourier

>  (fft, fftinv,  dft, dftinv,  sft, sct)

> where

> import Data.Complex--1.3
> import Data.List(transpose)--1.3
> import Complex_Vectors
                
> fft:: [ComplexF] -> [ComplexF] -- Warning: works only for n=2^km
>                                -- time=O(n log(n)) algorithm
> fft xs = map((1/(fromInt n))*) (ffth xs us)   where
>   us = map conjugate (rootsOfUnity n)
>   n = length xs
>   fromInt = fromInteger . toInteger -- partain addition

> fftinv:: [ComplexF] -> [ComplexF] -- Warning: works only for n=2^km
>                                   -- time=O(n log(n)) algorithm
> fftinv xs = ffth xs us   where
>   us = rootsOfUnity n
>   n = length xs

> ffth:: [ComplexF] -> [ComplexF] -> [ComplexF]
> ffth xs us
>  | n>1    =             (replikate fftEvn) `plus` 
>             (us `times` (replikate fftOdd))
>  | n==1   = xs
>  where
>    fftEvn = ffth (evns xs) uEvns
>    fftOdd = ffth (odds xs) uEvns
>    uEvns = evns us
>    evns = everyNth 2
>    odds = everyNth 2 . tail
>    n = length xs


  Discrete Fourier Transform (fft generalized to non-binary orders)

> dft:: [ComplexF] -> [ComplexF] 
>      -- time=O(n*sum(map (^2) (factors n))) algorithm
>      --     =O(n*log(n)) when n is a product of small primes
> dft xs
>  = map((1/(fromInt n))*) (dfth fs xs us)
>  where
>    us = replikate(map conjugate (rootsOfUnity n))
>    fs = factors n
>    n = length xs
>    fromInt = fromInteger . toInteger -- partain addition

> dftinv:: [ComplexF] -> [ComplexF]
>      -- time=O(n*sum(map (^2) (factors n))) algorithm
>      --     =O(n*log(n)) when n is a product of small primes
> dftinv xs
>  = dfth fs xs us
>  where
>    us = replikate(rootsOfUnity n)
>    fs = factors n
>    n = length xs

> dfth:: [Int] -> [ComplexF] -> [ComplexF] -> [ComplexF]
> dfth (f:fs) xs us
>  | fs==[]      = sfth f xs us
>  | otherwise   = map sum (transpose pfts)
>  where
>    pfts = [(dftOfSection k) `times` (us `toThe` k)| k<-[0..f-1]]
>    dftOfSection k = repl f
>                          (dfth fs (fsectionOfxs k) (us `toThe` f))
>    fsectionOfxs k = everyNth f (drop k xs)


  Slow Fourier Transform

> sft:: [ComplexF] -> [ComplexF]     -- time=O(n^2) algorithm
> sft xs
>  = map((1/(fromInt n))*) (sfth n xs us)
>  where
>    us = replikate(map conjugate (rootsOfUnity n))
>    n = length xs
>    fromInt = fromInteger . toInteger -- partain addition

> sftinv:: [ComplexF] -> [ComplexF]  -- time=O(n^2) algorithm
> sftinv xs
>  = sfth n xs us
>  where
>    us = replikate(rootsOfUnity n)
>    n = length xs

> sfth:: Int -> [ComplexF] -> [ComplexF] -> [ComplexF]
> sfth n xs us
>  = [sum(xs `times` upowers)| upowers<-[us `toThe` k| k<-[0..n-1]]]


  Slow Cosine Transform

> sct:: [Double] -> [Double]  -- time=O(n^2) algorithm
> sct xs                    -- computes n^2 cosines
>  = [xs_dot (map (cos.((fromInt k)*)) (thetas n))| k<-[0 .. n-1]]
>  where
>    n = length xs
>    xs_dot = sum.(zipWith (*) xs)
>    fromInt = fromInteger . toInteger -- partain addition


   Utilities

> plus  = zipWith (+)
> times = zipWith (*)
> replikate = cycle
> repl n = concat . take n . repeat
> everyNth n = (map head).(takeWhile (/=[])).(iterate (drop n))

> toThe:: [ComplexF] -> Int -> [ComplexF]
> rootsOfUnity `toThe` k = everyNth k rootsOfUnity

> factors:: Int -> [Int]
> factors n = factorsh n primes

> factorsh:: Int -> [Int] -> [Int]
> factorsh n (p:ps)
>  | n==1             = []
>  | n `mod` p == 0   = p: factorsh (n `div` p) (p:ps)
>  | otherwise        = factorsh n ps

> primes:: [Int]
> primes = map head (iterate sieve [2..])
> sieve(p:xs) = [x| x<-xs, x `mod` p /= 0]



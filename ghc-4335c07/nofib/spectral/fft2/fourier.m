> || Fourier Transforms, Floating Point Benchmark - Miranda

            Rex Page (rpage@trc.amoco.com)
            Amoco Production Research, Sep 1992

  Miranda timings (in seconds)    Miranda version 2.014
                                  running on a SparcStation 1+

       result of expressions tst...(...)
       are approximately zero in all cases tested

  rampWave                                            (28Sep92)
                n = 256         512          1024
 tstfft               9sec       21sec         48sec
 tstsct              16          64           257
 tstdft              20          46           111     (29Sep92) 
               Timing method for above figures is to apply
               the Unix time function to a Unix "here document"
               that invokes a Miranda expression with this
               script as its environment.

  rampWave                                            (22Sep92)
                n = 256         512          1024
 tstfft              11sec       23sec         51sec
 tstsct              19          69           258
               Timing method for above figures is to use
               wallclock time via  system "date" from inside
               Miranda session (using rlplib timer function)

> %export fft fftinv   dft dftinv   sct 
>         tstfft timefft   tstdft   tstsct timesct
>         squareWave sineWave   rampWave sqwC snwC rmwC


   Fast Fourier Transform

> fft:: [complex] -> [complex] || Warning: works only for n=2^k
> fft xs                       || time=O(n*log(n)) algorithm
>  = scale (1/n) (ffth xs us)
>    where
>    us = map conjugate (rootsOfUnity n)
>    n = #xs

> fftinv:: [complex] -> [complex] || Warning: works only for n=2^k
> fftinv xs                       || time=O(n*log(n)) algorithm
>  = ffth xs us
>    where
>    us = rootsOfUnity n
>    n = #xs

> ffth:: [complex] -> [complex] -> [complex]
> ffth xs us
>  = (repl 2 fftEvn) $plus (us $times (repl 2 fftOdd)), if n>1
>  = xs, if n=1
>    where
>    fftEvn = ffth (evns xs) uEvns
>    fftOdd = ffth (odds xs) uEvns
>    uEvns = evns us
>    evns = everyNth 2
>    odds = everyNth 2 . tl
>    n = #xs


  Discrete Fourier Transform (fft generalized to non-binary orders)

> dft:: [complex] -> [complex]
>      || time=O(n*sum(map (^2) (factors n))) algorithm
>      ||     =O(n*log(n)) when n is a product of small primes
> dft xs
>  = scale (1/n) (dfth fs xs us)
>    where
>    us = replicate(map conjugate (rootsOfUnity n))
>    fs = factors n
>    n = #xs

> dftinv:: [complex] -> [complex]  || time=O(n*log(n)) algorithm
>      || time=O(n*sum(map (^2) (factors n))) algorithm
>      ||     =O(n*log(n)) when n is a product of small primes
> dftinv xs
>  = dfth fs xs us
>    where
>    us = replicate(rootsOfUnity n)
>    fs = factors n
>    n = #xs

> dfth:: [num] -> [complex] -> [complex] -> [complex]
> dfth (f:fs) xs us
>  = sfth f xs us, if fs=[]
>  = map sumC (transpose pfts), otherwise
>    where
>    pfts = [(dftOfSection k) $times (us $toThe k)| k<-[0..f-1]]
>    dftOfSection k = repl f (dfth fs (fsectionOfxs k) (us $toThe f))
>    fsectionOfxs k = everyNth f (drop k xs)


  Slow Fourier Transform

> sft:: [complex] -> [complex]     || time=O(n^2) algorithm
> sft xs
>  = scale (1/n) (sfth n xs us)
>    where
>    us = replicate(map conjugate (rootsOfUnity n))
>    n = #xs

> sftinv:: [complex] -> [complex]  || time=O(n^2) algorithm
> sftinv xs
>  = sfth n xs us
>    where
>    us = replicate(rootsOfUnity n)
>    n = #xs

> sfth:: num -> [complex] -> [complex] -> [complex]
> sfth n xs us
>  = [sumC(xs $times upowers)| upowers<-[us $toThe k| k<-[0..n-1]]]


  Slow Cosine Transform

> sct:: [num] -> [num]  || time=O(n^2), computes n^2 cosines
> sct xs
>  = [xs_dot (map (cos.(k*)) (thetas n))| k<-[0 .. n-1]]
>    where
>    n = #xs
>    xs_dot = sum.(map2 (*) xs)


    Utilities

> replicate:: [*] -> [*]
> replicate = concat.repeat

> repl:: num -> [*] -> [*]
> repl n = concat.(rep n)

>  everyNth:: num -> [*] -> [*]
>  everyNth n = (map hd).(takewhile (~=[])).(iterate (drop n))

>  toThe:: [complex] -> num -> [complex]
>  rootsOfUnity $toThe k = everyNth k rootsOfUnity

> factors:: num -> [num]
> factors n = factorsh n primes

> factorsh:: num -> [num] -> [num]
> factorsh n (p:ps)
>  = [], if n=1
>  = p: factorsh (n div p) (p:ps), if n mod p = 0
>  = factorsh n ps, otherwise

> primes:: [num]
> primes = map hd (iterate sieve [2..])
> sieve(p:xs) = [x| x<-xs; x mod p ~= 0]


    Test Apparatus

> tstfft  zs = distance zs (fftinv(fft zs))
> timefft zs = showtimer(distance zs (fftinv(fft zs)))
> valfft zs  = distance (fft zs) (sft zs)

> tstdft zs  = distance zs (dftinv(dft zs))
> valdft zs  = distance (dft zs) (sft zs)

> tstsct  xs = (sum.sct) xs
> timesct xs = (showtimer.sum.sct) xs

> squareWave n = rep n 1
> sqwC = (map cmpx).squareWave

> rampWave n = [0 .. n-1]
> rmwC = (map cmpx).rampWave

> sineWave = (map sin).thetas
> snwC = (map cmpx).sineWave

> %include "complex_numbers"
> %include "rlplib"  || for timer "functions"

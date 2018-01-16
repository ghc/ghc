

DEVLOG: A collection of notes accumulated during development.
=============================================================


[2011.06.24] (transient) Regression in stdGen performance.
----------------------------------------------------------

I just added a simple benchmark to make sure that whatever fix I
introduce for trac ticket #5133 does not regress performance.  Yet in
doing so I discovered that I'm getting much worse performance out of
rev 130e421e912d than I'm seeing in my installed random-1.0.0.3 package.

Current version:
    How many random numbers can we generate in a second on one thread?
      Cost of rdtsc (ffi call):    100
      Approx getCPUTime calls per second: 234,553
      Approx clock frequency:  3,335,220,196
      First, timing with System.Random interface:
	 68,550,189 random ints generated [constant zero gen]         ~ 48.65 cycles/int
	    900,889 random ints generated [System.Random stdGen]      ~ 3,702 cycles/int

random-1.0.0.3 version:
    How many random numbers can we generate in a second on one thread?
      Cost of rdtsc (ffi call):    75
      Approx getCPUTime calls per second: 215,332
      Approx clock frequency:  3,334,964,738
      First, timing with System.Random interface:
	 71,683,748 random ints generated [constant zero gen]         ~ 46.52 cycles/int
	 13,609,559 random ints generated [System.Random stdGen]      ~ 245 cycles/int

A >13X difference!! 
Both are compiled with the same options.  The only difference is which
System.Random is used.

When did the regression occur?  

 * e059ed15172585310f9c -- 10/13/2010 perf still good
 * 6c43f80f48178ac617   -- SplittableGen introduced, still good perf
 * 130e421e912d394653a4 -- most recent, bad performance

Ok... this is very odd.  It was a heisenbug becuase it's disappeared
now!  I'll leave this note here to help remember to look for it in the
future.
  -Ryan


[2011.06.24] Timing non-int types
---------------------------------

The results are highly uneven:

    Cost of rdtsc (ffi call):    84
    Approx getCPUTime calls per second: 220,674
    Approx clock frequency:  3,336,127,273
    First, timing with System.Random interface:
      112,976,933 randoms generated [constant zero gen]         ~ 29.53 cycles/int
       14,415,176 randoms generated [System.Random stdGen]      ~ 231 cycles/int
	   70,751 randoms generated [System.Random Floats]      ~ 47,153 cycles/int
	   70,685 randoms generated [System.Random CFloats]     ~ 47,197 cycles/int
	2,511,635 randoms generated [System.Random Doubles]     ~ 1,328 cycles/int
	   70,494 randoms generated [System.Random CDoubles]    ~ 47,325 cycles/int
	  858,012 randoms generated [System.Random Integers]    ~ 3,888 cycles/int
	4,756,213 randoms generated [System.Random Bools]       ~ 701 cycles/int

As you can see, all the types that use the generic randomIvalFrac /
randomFrac definitions perform badly.  What's more, the above results
INCLUDE an attempt to inline: 

    {-# INLINE randomIvalFrac #-}
    {-# INLINE randomFrac #-}
    {-# INLINE randomIvalDouble #-}

After reimplementing random/Float these are the new results:

  Cost of rdtsc (ffi call):    100
  Approx getCPUTime calls per second: 200,582
  Approx clock frequency:  3,334,891,942
  First, timing with System.Random interface:
    105,266,949 randoms generated [constant zero gen]         ~ 31.68 cycles/int
     13,593,392 randoms generated [System.Random stdGen]      ~ 245 cycles/int
     10,962,597 randoms generated [System.Random Floats]      ~ 304 cycles/int
     11,926,573 randoms generated [System.Random CFloats]     ~ 280 cycles/int
      2,421,520 randoms generated [System.Random Doubles]     ~ 1,377 cycles/int
      2,535,087 randoms generated [System.Random CDoubles]    ~ 1,315 cycles/int
        856,276 randoms generated [System.Random Integers]    ~ 3,895 cycles/int
      4,976,373 randoms generated [System.Random Bools]       ~ 670 cycles/int

(But I still need to propagate these changes throughout all types / API calls.)



[2011.06.28] Integer Generation via random and randomR
-------------------------------------------------------

Back on the master branch I notice that while randomIvalInteger does
well for small ranges, it's advantage doesn't scale to larger ranges:

  range (-100,100):
      5,105,290 randoms generated [System.Random Integers]    ~ 653 cycles/int

  range (0,2^5000):
          8,969 randoms generated [System.Random BIG Integers] ~ 371,848 cycles/int



[2011.08.25] Validating release version 1.0.1.0 rev 40bbfd2867
--------------------------------------------------------------

This is a bugfix release without SplittableGen.  It passed (cd tests;
make test) on my Mac Os 10.6 machine.

I ran GHC validate using the following fingerprint

    .|c5056b932a06b4adce5167a5cb69f1f0768d28ec
    ghc-tarballs|e7b7b152083f7c3e3559e557a239757d41ac02a6
    libraries/Cabal|3dcc425495523ab6142027097cb598a4d2ad810a
    libraries/Win32|085b11285b6adbc6484d9c21f5e0b8105556869c
    libraries/array|fa295423e7404d3d1d3d82655b2b44d50f045a44
    libraries/base|a57369f54bd25a1de5d477f3c363b3bafd17d168
    libraries/binary|9065df2120254601c68c3a28264fd062abde9354
    libraries/bytestring|caad22630f73e0e9b1b61f4da34f8aefcc6001d8
    libraries/containers|667591b168c804d3eeae503dff1c848ed6852412
    libraries/directory|d44f52926278319286804d8333149dd13d4ecc82
    libraries/dph|b23b45a9e8fce985327b076997d61ab0ddc7b2f7
    libraries/extensible-exceptions|e77722871a5812d52c467e3a8fd9c7b97cdec521
    libraries/filepath|fd381017dca45de5c94dac85a6233516a6b6963d
    libraries/ghc-prim|0a84a755e1248b4d50f6535a0ce75af0bb21b0ad
    libraries/haskeline|8787a64417500efffc9c48032ee7c37315fb2547
    libraries/haskell2010|470b34b6c0336339eac9fbcfb6020e46b5154bfe
    libraries/haskell98|5590c0f042d6d07352e0bf49cedeef5ba0821f23
    libraries/hoopl|b98db91cd0c53ddb2c275c04823f9c379774104b
    libraries/hpc|7c726abec939b11af1ecf89740ca8d04e6a1360d
    libraries/integer-gmp|65c73842bca2f075b65f418a5ff34753b185e0d7
    libraries/integer-simple|10202111c59f0695ef782d1ec9e6fc992933fc9a
    libraries/mtl|a41470c1890073e678f0dca2a9ef4c02d55bf7c6
    libraries/old-locale|104e7e5a7b33424f34e98825a0d4ccb7614ca7c2
    libraries/old-time|81e0c8a4b98d4b084eefe75bedf91a44edd31888
    libraries/pretty|036fb8dfbb9d4a787fcd150c2756b4899be4e942
    libraries/primitive|74479e07b92b8859eae473e5cc86b40decae1d6e
    libraries/process|68ba490d6691f55eab19a249379144831055e2ac
    libraries/random|3fb0e9e42b54d7b01b794fc27d4d678d7d74ff0e
    libraries/template-haskell|02362d12e5ae0af20d637eec97db51f6827a1625
    libraries/terminfo|baec6aff59d13ba294b370f9563e8068706392ce
    libraries/unix|f55638fb5c6badd385c51a41de7ff96ef106de42
    libraries/utf8-string|ec2b85940a256dbc8771e5e2065ca8f76cc802d0
    libraries/vector|1e72d46bdc8488a84558b64ac63632cef1d8a695
    libraries/xhtml|cb2cf6c34d815fdf4ed74efeb65e1993e7bda514
    testsuite|26c608a0c31d56917099e4f48bf58c1d1e92e61c
    utils/haddock|d54959189f33105ed09a59efee5ba34f53369282
    utils/hsc2hs|f8cbf37ab28ab4512d932678c08c263aa412e008



First validating in the context of a slightly stale GHC head
(7.3.20110727) on a mac.


[2011.09.30] Redoing timings after bugfix in version 1.0.1.1
------------------------------------------------------------

It looks like there has been serious performance regression (3.33ghz
nehalem still).

    How many random numbers can we generate in a second on one thread?
      Cost of rdtsc (ffi call):    38
      Approx getCPUTime calls per second: 7,121
      Approx clock frequency:  96,610,524
      First, timing System.Random.next:
	148,133,038 randoms generated [constant zero gen]         ~ 0.65 cycles/int
	 12,656,455 randoms generated [System.Random stdGen/next] ~ 7.63 cycles/int

      Second, timing System.Random.random at different types:
	    676,066 randoms generated [System.Random Ints]        ~ 143 cycles/int
	  3,917,247 randoms generated [System.Random Word16]      ~ 24.66 cycles/int
	  2,231,460 randoms generated [System.Random Floats]      ~ 43.29 cycles/int
	  2,269,993 randoms generated [System.Random CFloats]     ~ 42.56 cycles/int
	    686,363 randoms generated [System.Random Doubles]     ~ 141 cycles/int
	  2,165,679 randoms generated [System.Random CDoubles]    ~ 44.61 cycles/int
	    713,702 randoms generated [System.Random Integers]    ~ 135 cycles/int
	  3,647,551 randoms generated [System.Random Bools]       ~ 26.49 cycles/int
	  4,296,919 randoms generated [System.Random Chars]       ~ 22.48 cycles/int

      Next timing range-restricted System.Random.randomR:
	  4,307,214 randoms generated [System.Random Ints]        ~ 22.43 cycles/int
	  4,068,982 randoms generated [System.Random Word16s]     ~ 23.74 cycles/int
	  2,059,264 randoms generated [System.Random Floats]      ~ 46.92 cycles/int
	  1,960,359 randoms generated [System.Random CFloats]     ~ 49.28 cycles/int
	    678,978 randoms generated [System.Random Doubles]     ~ 142 cycles/int
	  2,009,665 randoms generated [System.Random CDoubles]    ~ 48.07 cycles/int
	  4,296,452 randoms generated [System.Random Integers]    ~ 22.49 cycles/int
	  3,689,999 randoms generated [System.Random Bools]       ~ 26.18 cycles/int
	  4,367,577 randoms generated [System.Random Chars]       ~ 22.12 cycles/int
	      6,650 randoms generated [System.Random BIG Integers] ~ 14,528 cycles/int


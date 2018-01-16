-- Time-stamp: <Wed May 22 1996 19:05:56 Stardate: [-31]7543.77 hwloidl>
--
-- My favorite random number module.
-- Based on Lennart's Random.hs module (in the hbc syslib).
-- Added some functions using Glaswegian extensions to get random numbers
-- based on system clock. This is mainly used for getting seed values
-- for the main random number generator.
-----------------------------------------------------------------------------

module GranRandom where

import Random (randomInts, randomDoubles, normalRandomDoubles)      -- this requires -syslib hbc
import Time       -- this requires -fhaskell-1.3
import GlaExts    -- this requires -fglasgow-exts


getRandIntList :: Int -> Int -> IO [Int]
getRandIntList len bound = getRandomInts bound >>= \ l ->
                           return (take len l)

getRandomInts :: Int -> IO [Int]
getRandomInts bound = getRandInt 2147483561 >>= \ s1 ->
                      getRandInt 2147483397 >>= \ s2 ->
                      let 
		        randomList = randomInts (s1+1) (s2+1)
                      in
                      return (map (`mod` bound) randomList )

getRandomDoubles :: Double -> IO [Double]
getRandomDoubles bound = getRandInt 2147483561 >>= \ s1 ->
                      	 getRandInt 2147483397 >>= \ s2 ->
                      	 let 
                           -- Doubles uniformly distibuted in (0,1)
		      	   randomList = randomDoubles (s1+1) (s2+1)
                      	 in
                      	 return (map (* bound) randomList)
                       
getNormalRandomDoubles :: Double -> IO [Double]
getNormalRandomDoubles bound = getRandInt 2147483561 >>= \ s1 ->
                      	       getRandInt 2147483397 >>= \ s2 ->
                      	       let 
		      	 	 randomList = normalRandomDoubles (s1+1) (s2+1)
                      	       in
                      	       return (map (* bound) randomList)

getRandInt :: Int -> IO Int
getRandInt bound = 
    getClockTime >>= \ t ->
    let
     CalendarTime _ _ _ _ _ _ x _ _ _ _ _  = toCalendarTime b
    in
    return (((fromInteger x) `mod` bound) :: Int )

unsafeGetRandInt :: Int -> Int
unsafeGetRandInt = unsafePerformIO . getRandInt

unsafeGetRandIntList :: Int -> Int -> [Int]
unsafeGetRandIntList len bound =
 unsafePerformIO ( getRandIntList len bound )

unsafeGetRandomInts :: Int -> [Int]
unsafeGetRandomInts = unsafePerformIO . getRandomInts  

unsafeGetRandomDoubles :: Double -> [Double]
unsafeGetRandomDoubles = unsafePerformIO . getRandomDoubles

unsafeGetNormalRandomDoubles :: Double -> [Double]
unsafeGetNormalRandomDoubles = unsafePerformIO . getNormalRandomDoubles

-----------------------------------------------------------------------------
-- Converting the current system time into a stardate.
-----------------------------------------------------------------------------

#if 1
type Stardate = (Int, Int, Int)

unsafeGetStardate :: Int -> Stardate
unsafeGetStardate = unsafePerformIO . getStardate

getStardate :: Int -> IO Stardate
getStardate prec = 
    _casm_ ``%r = time((time_t *)0);'' `thenIO_Prim` \ tm ->
    let 
      (iss, int, frac) = stardate tm
    in
    return (iss, int, (frac `div` (10^prec)))

stardate :: Int -> Stardate
stardate tm = (issue, integer, fraction)
              where -- fraction = ( (tm%17280) *1000000) / 17280
	            (tm_quot, tm_rem) = quotRem tm 17280 
	            fraction  = tm_rem * 3125 `div` 54
                    integer'  = tm_quot + 9350
                    (int_quot, int_rem) = quotRem integer' 10000
                    integer   = int_rem
                    issue     = int_quot - 36

#endif

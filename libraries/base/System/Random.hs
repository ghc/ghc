-----------------------------------------------------------------------------
-- |
-- Module      :  System.Random
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Random numbers.
--
-----------------------------------------------------------------------------

module System.Random
	(

	-- $intro

	-- * The 'RandomGen' class, and the 'StdGen' generator

	  RandomGen(next, split, genRange)
	, StdGen
	, mkStdGen

	-- * The 'Random' class
	, Random ( random,   randomR,
		   randoms,  randomRs,
		   randomIO, randomRIO )

	-- * The global random number generator

	-- $globalrng

	, getStdRandom
	, getStdGen
	, setStdGen
	, newStdGen

	-- * References
	-- $references

	) where

import Prelude

#ifdef __NHC__
import CPUTime		( getCPUTime )
import Foreign.Ptr      ( Ptr, nullPtr )
#else
import System.CPUTime	( getCPUTime )
import System.Time	( getClockTime, ClockTime(..) )
#endif
import Data.Char	( isSpace, chr, ord )
import System.IO.Unsafe ( unsafePerformIO )
import Data.IORef
import Numeric		( readDec )

-- The standard nhc98 implementation of Time.ClockTime does not match
-- the extended one expected in this module, so we lash-up a quick
-- replacement here.
#ifdef __NHC__
data ClockTime = TOD Integer ()
foreign import ccall "time.h time" readtime :: Ptr () -> IO Int
getClockTime :: IO ClockTime
getClockTime = do t <- readtime nullPtr;  return (TOD (toInteger t) ())
#endif

{- $intro

This library deals with the common task of pseudo-random
number generation. The library makes it possible to generate
repeatable results, by starting with a specified initial random
number generator; or to get different results on each run by using the 
system-initialised generator, or by supplying a seed from some other
source.

The library is split into two layers: 

* A core /random number generator/ provides a supply of bits. The class
'RandomGen' provides a common interface to such generators.

* The class 'Random' provides a way to extract particular values from
a random number generator. For example, the 'Float' instance of 'Random'
allows one to generate random values of type 'Float'.

[Comment found in this file when merging with Library Report:]

The June 1988 (v31 \#6) issue of the Communications of the ACM has an
article by Pierre L'Ecuyer called, /Efficient and Portable Combined
Random Number Generators/.  Here is the Portable Combined Generator of
L'Ecuyer for 32-bit computers.  It has a period of roughly 2.30584e18.

Transliterator: Lennart Augustsson

-}

-- |RandomGen
-- The class 'RandomGen' provides a common interface to random number generators.

class RandomGen g where

   -- |The 'next' operation allows one to extract at least 30 bits (one 'Int''s
   -- worth) from the generator, returning a new generator as well.  The
   -- integer returned may be positive or negative.
   next     :: g -> (Int, g)

   -- |The 'split' operation allows one to obtain two distinct random number
   -- generators. This is very useful in functional programs (for example, when
   -- passing a random number generator down to recursive calls), but very
   -- little work has been done on statistically robust implementations of
   -- @split ([1,4]@ are the only examples we know of).
   split    :: g -> (g, g)

   genRange :: g -> (Int,Int)

   -- default mathod
   genRange g = (minBound,maxBound)

{- |The "System.Random" library provides one instance of 'RandomGen', the
abstract data type 'StdGen'.

The result of repeatedly using next should be at least as statistically robust
as the /Minimal Standard Random Number Generator/ described by
["System.Random\#Park", "System.Random\#Carta"]. Until more
is known about implementations of 'split', all we require is that 'split' deliver
generators that are (a) not identical and (b) independently robust in the sense
just given.

The 'show'\/'Read' instances of 'StdGen' provide a primitive way to save the
state of a random number generator. It is required that @read (show g) == g@.

In addition, 'read' may be used to map an arbitrary string (not necessarily one
produced by 'show') onto a value of type 'StdGen'. In general, the 'read'
instance of 'StdGen' has the following properties: 

* It guarantees to succeed on any string. 

*It guarantees to consume only a finite portion of the string. 

* Different argument strings are likely to result in different results.

The function 'mkStdGen' provides an alternative way of producing an initial
generator, by mapping an 'Int' into a generator. Again, distinct arguments
should be likely to produce distinct generators.

Programmers may, of course, supply their own instances of 'RandomGen'.

-}

data StdGen 
 = StdGen Int Int

instance RandomGen StdGen where
  next  = stdNext
  split = stdSplit

instance Show StdGen where
  showsPrec p (StdGen s1 s2) = 
     showsPrec p s1 . 
     showChar ' ' .
     showsPrec p s2

instance Read StdGen where
  readsPrec _p = \ r ->
     case try_read r of
       r@[_] -> r
       _   -> [stdFromString r] -- because it shouldn't ever fail.
    where 
      try_read r = do
         (s1, r1) <- readDec (dropWhile isSpace r)
	 (s2, r2) <- readDec (dropWhile isSpace r1)
	 return (StdGen s1 s2, r2)

{-
 If we cannot unravel the StdGen from a string, create
 one based on the string given.
-}
stdFromString         :: String -> (StdGen, String)
stdFromString s        = (mkStdGen num, rest)
	where (cs, rest) = splitAt 6 s
              num        = foldl (\a x -> x + 3 * a) 1 (map ord cs)


mkStdGen :: Int -> StdGen -- why not Integer ?
mkStdGen s
 | s < 0     = mkStdGen (-s)
 | otherwise = StdGen (s1+1) (s2+1)
      where
	(q, s1) = s `divMod` 2147483562
	s2      = q `mod` 2147483398

createStdGen :: Integer -> StdGen
createStdGen s
 | s < 0     = createStdGen (-s)
 | otherwise = StdGen (fromInteger (s1+1)) (fromInteger (s2+1))
      where
	(q, s1) = s `divMod` 2147483562
	s2      = q `mod` 2147483398

-- FIXME: 1/2/3 below should be ** (vs@30082002) XXX

{- |The 'Random' class
With a source of random number supply in hand, the 'Random' class allows the
programmer to extract random values of a variety of types.

* 'randomR' takes a range /(lo,hi)/ and a random number generator /g/, and returns
a random value uniformly distributed in the closed interval /[lo,hi]/, together
with a new generator. It is unspecified what happens if /lo>hi/. For continuous
types there is no requirement that the values /lo/ and /hi/ are ever produced,
but they may be, depending on the implementation and the interval.

* 'random' does the same as 'randomR', but does not take a range.

(1) For bounded types (instances of 'Bounded', such as 'Char'), the range is
normally the whole type.

(2) For fractional types, the range is normally the semi-closed interval @[0,1)@.

(3) For 'Integer', the range is (arbitrarily) the range of 'Int'.

* The plural versions, 'randomRs' and 'randoms', produce an infinite list of
random values, and do not return a new generator.

* The 'IO' versions, 'randomRIO' and 'randomIO', use the global random number
generator (see Section 17.3
<http://www.haskell.org/onlinelibrary/random.html#global-rng>).
-}

class Random a where
  -- |Minimal complete definition: 'random' and 'randomR'
  random  :: RandomGen g => g -> (a, g)
  randomR :: RandomGen g => (a,a) -> g -> (a,g)

  -- |Default methods  
  randoms  :: RandomGen g => g -> [a]
  randoms  g      = (\(x,g') -> x : randoms g') (random g)

  randomRs :: RandomGen g => (a,a) -> g -> [a]
  randomRs ival g = x : randomRs ival g' where (x,g') = randomR ival g

  randomIO  :: IO a
  randomIO	   = getStdRandom random

  randomRIO :: (a,a) -> IO a
  randomRIO range  = getStdRandom (randomR range)


instance Random Int where
  randomR (a,b) g = randomIvalInteger (toInteger a, toInteger b) g
  random g        = randomR (minBound,maxBound) g

instance Random Char where
  randomR (a,b) g = 
      case (randomIvalInteger (toInteger (ord a), toInteger (ord b)) g) of
        (x,g) -> (chr x, g)
  random g	  = randomR (minBound,maxBound) g

instance Random Bool where
  randomR (a,b) g = 
      case (randomIvalInteger (toInteger (bool2Int a), toInteger (bool2Int b)) g) of
        (x, g) -> (int2Bool x, g)
       where
         bool2Int False = 0
         bool2Int True  = 1

	 int2Bool 0	= False
	 int2Bool _	= True

  random g	  = randomR (minBound,maxBound) g
 
instance Random Integer where
  randomR ival g = randomIvalInteger ival g
  random g	 = randomR (toInteger (minBound::Int), toInteger (maxBound::Int)) g

instance Random Double where
  randomR ival g = randomIvalDouble ival id g
  random g       = randomR (0::Double,1) g
  
-- hah, so you thought you were saving cycles by using Float?
instance Random Float where
  random g        = randomIvalDouble (0::Double,1) realToFrac g
  randomR (a,b) g = randomIvalDouble (realToFrac a, realToFrac b) realToFrac g

mkStdRNG :: Integer -> IO StdGen
mkStdRNG o = do
    ct          <- getCPUTime
    (TOD sec _) <- getClockTime
    return (createStdGen (sec * 12345 + ct + o))

randomIvalInteger :: (RandomGen g, Num a) => (Integer, Integer) -> g -> (a, g)
randomIvalInteger (l,h) rng
 | l > h     = randomIvalInteger (h,l) rng
 | otherwise = case (f n 1 rng) of (v, rng') -> (fromInteger (l + v `mod` k), rng')
     where
       k = h - l + 1
       b = 2147483561
       n = iLogBase b k

       f 0 acc g = (acc, g)
       f n acc g = 
          let
	   (x,g')   = next g
	  in
	  f (n-1) (fromIntegral x + acc * b) g'

randomIvalDouble :: (RandomGen g, Fractional a) => (Double, Double) -> (Double -> a) -> g -> (a, g)
randomIvalDouble (l,h) fromDouble rng 
  | l > h     = randomIvalDouble (h,l) fromDouble rng
  | otherwise = 
       case (randomIvalInteger (toInteger (minBound::Int), toInteger (maxBound::Int)) rng) of
         (x, rng') -> 
	    let
	     scaled_x = 
		fromDouble ((l+h)/2) + 
                fromDouble ((h-l) / realToFrac intRange) *
		fromIntegral (x::Int)
	    in
	    (scaled_x, rng')

intRange :: Integer
intRange  = toInteger (maxBound::Int) - toInteger (minBound::Int)

iLogBase :: Integer -> Integer -> Integer
iLogBase b i = if i < b then 1 else 1 + iLogBase b (i `div` b)

stdNext :: StdGen -> (Int, StdGen)
stdNext (StdGen s1 s2) = (z', StdGen s1'' s2'')
	where	z'   = if z < 1 then z + 2147483562 else z
		z    = s1'' - s2''

		k    = s1 `quot` 53668
		s1'  = 40014 * (s1 - k * 53668) - k * 12211
		s1'' = if s1' < 0 then s1' + 2147483563 else s1'
    
		k'   = s2 `quot` 52774
		s2'  = 40692 * (s2 - k' * 52774) - k' * 3791
		s2'' = if s2' < 0 then s2' + 2147483399 else s2'

stdSplit            :: StdGen -> (StdGen, StdGen)
stdSplit std@(StdGen s1 s2)
                     = (left, right)
                       where
                        -- no statistical foundation for this!
                        left    = StdGen new_s1 t2
                        right   = StdGen t1 new_s2

                        new_s1 | s1 == 2147483562 = 1
                               | otherwise        = s1 + 1

                        new_s2 | s2 == 1          = 2147483398
                               | otherwise        = s2 - 1

                        StdGen t1 t2 = snd (next std)

-- The global random number generator

{- $globalrng

There is a single, implicit, global random number generator of type
'StdGen', held in some global variable maintained by the 'IO' monad. It is
initialised automatically in some system-dependent fashion, for example, by
using the time of day, or Linux's kernel random number generator. To get
deterministic behaviour, use 'setStdGen'.
-}

-- |'setStdGen' sets the global random number generator.
setStdGen :: StdGen -> IO ()
setStdGen sgen = writeIORef theStdGen sgen

-- |'getStdGen' gets the global random number generator.
getStdGen :: IO StdGen
getStdGen  = readIORef theStdGen

-- |'newStdGen' applies 'split' to the current global random generator, updates it
-- with one of the results, and returns the other.
theStdGen :: IORef StdGen
theStdGen  = unsafePerformIO $ do
   rng <- mkStdRNG 0
   newIORef rng

newStdGen :: IO StdGen
newStdGen = do
  rng <- getStdGen
  let (a,b) = split rng
  setStdGen a
  return b

{- |'getStdRandom' uses the supplied function to get a value from the current
global random generator, and updates the global generator with the new generator
returned by the function. For example, @rollDice@ gets a random integer between 1 and 6: 

>  rollDice :: IO Int
>  rollDice = getStdRandom (randomR (1,6))

-}

getStdRandom :: (StdGen -> (a,StdGen)) -> IO a
getStdRandom f = do
   rng		<- getStdGen
   let (v, new_rng) = f rng
   setStdGen new_rng
   return v

{- $references

* [1] FW Burton and RL Page, /Distributed random number generation/,
Journal of Functional Programming, 2(2):203-212, April 1992.

* [2] SK #Park# Park, and KW Miller, /Random number generators -
good ones are hard to find/, Comm ACM 31(10), Oct 1988, pp1192-1201.

* [3] DG #Carta# Carta, /Two fast implementations of the minimal standard
random number generator/, Comm ACM, 33(1), Jan 1990, pp87-88.

* [4] P Hellekalek, /Don\'t trust parallel Monte Carlo/,
Department of Mathematics, University of Salzburg,
<http://random.mat.sbg.ac.at/~peter/pads98.ps>, 1998.

The Web site <http://random.mat.sbg.ac.at/> is a great source of information.

-}

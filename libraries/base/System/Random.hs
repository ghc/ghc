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
	  RandomGen(next, split, genRange)
	, StdGen
	, mkStdGen
	, Random ( random,   randomR,
		   randoms,  randomRs,
		   randomIO, randomRIO )
	, getStdRandom
	, getStdGen
	, setStdGen
	, newStdGen
	) where

-- The June 1988 (v31 #6) issue of the Communications of the ACM has an
-- article by Pierre L'Ecuyer called, "Efficient and Portable Combined
-- Random Number Generators".  Here is the Portable Combined Generator of
-- L'Ecuyer for 32-bit computers.  It has a period of roughly 2.30584e18.

-- Transliterator: Lennart Augustsson

-- sof 1/99 - code brought (kicking and screaming) into the new Random
-- world..

import Prelude

import System.CPUTime	( getCPUTime )
import Data.Char	( isSpace, chr, ord )
import System.IO.Unsafe ( unsafePerformIO )
import Data.IORef

#ifdef __GLASGOW_HASKELL__
import GHC.Show		( showSignedInt, showSpace )
import Numeric		( readDec )
import GHC.IOBase	( unsafePerformIO, stToIO )
import System.Time	( getClockTime, ClockTime(..) )
#endif

class RandomGen g where
   next     :: g -> (Int, g)
   split    :: g -> (g, g)
   genRange :: g -> (Int,Int)

   -- default mathod
   genRange g = (minBound,maxBound)


data StdGen 
 = StdGen Int Int

instance RandomGen StdGen where
  next  = stdNext
  split = stdSplit

#ifdef __GLASGOW_HASKELL__
instance Show StdGen where
  showsPrec p (StdGen s1 s2) = 
     showSignedInt p s1 . 
     showSpace          . 
     showSignedInt p s2
#endif

#ifdef __HUGS__
instance Show StdGen where
  showsPrec p (StdGen s1 s2) = 
     showsPrec p s1 . 
     showChar ' ' .
     showsPrec p s2
#endif

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


-- The class definition - see library report for details.

class Random a where
  -- Minimal complete definition: random and randomR
  random  :: RandomGen g => g -> (a, g)
  randomR :: RandomGen g => (a,a) -> g -> (a,g)
  
  randoms  :: RandomGen g => g -> [a]
  randoms  g      = x : randoms g' where (x,g') = random g

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

#ifdef __GLASGOW_HASKELL__
mkStdRNG :: Integer -> IO StdGen
mkStdRNG o = do
    ct          <- getCPUTime
    (TOD sec _) <- getClockTime
    return (createStdGen (sec * 12345 + ct + o))
#endif

#ifdef __HUGS__
mkStdRNG :: Integer -> IO StdGen
mkStdRNG o = do
    ct          <- getCPUTime
    return (createStdGen (ct + o))
#endif

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


setStdGen :: StdGen -> IO ()
setStdGen sgen = writeIORef theStdGen sgen

getStdGen :: IO StdGen
getStdGen  = readIORef theStdGen

theStdGen :: IORef StdGen
theStdGen  = unsafePerformIO (newIORef (createStdGen 0))

newStdGen :: IO StdGen
newStdGen = do
  rng <- getStdGen
  let (a,b) = split rng
  setStdGen a
  return b

getStdRandom :: (StdGen -> (a,StdGen)) -> IO a
getStdRandom f = do
   rng		<- getStdGen
   let (v, new_rng) = f rng
   setStdGen new_rng
   return v

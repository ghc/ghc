%
% (c) The GRASP/AQUA Project, Glasgow University, 1995-99
%


The June 1988 (v31 #6) issue of the Communications of the ACM has an
article by Pierre L'Ecuyer called, "Efficient and Portable Combined
Random Number Generators".  Here is the Portable Combined Generator of
L'Ecuyer for 32-bit computers.  It has a period of roughly 2.30584e18.

Transliterator: Lennart Augustsson

sof 1/99 - code brought (kicking and screaming) into the new Random
world..

\begin{code}
module Random
	(
	  RandomGen(next, split)
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

import CPUTime (getCPUTime)
import PrelST
import PrelRead
import PrelIOBase
import PrelNumExtra ( float2Double, double2Float )
import PrelBase
import PrelArr
import Char ( isSpace, chr, ord )
import Time (getClockTime, ClockTime(..))

\end{code}

\begin{code}
class RandomGen g where
   next  :: g -> (Int, g)
   split :: g -> (g, g)

\end{code}

\begin{code}
data StdGen 
 = StdGen Int Int

instance RandomGen StdGen where
  next  = rand1
  split = splitStdGen

instance Show StdGen where
  showsPrec p (StdGen s1 s2) = 
     showSignedInt p s1 . 
     showSpace          . 
     showSignedInt p s2

instance Read StdGen where
  readsPrec p = \ r ->
     case try_read r of
       r@[_] -> r
       _   -> [(unsafePerformIO mkStdRNG,r)] -- because it shouldn't ever fail.
    where 
      try_read r = do
         (s1, r1) <- readDec (dropWhile isSpace r)
	 (s2, r2) <- readDec (dropWhile isSpace r1)
	 return (StdGen s1 s2, r2)

\end{code}

\begin{code}
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
 | otherwise = StdGen (toInt (s1+1)) (toInt (s2+1))
      where
	(q, s1) = s `divMod` 2147483562
	s2      = q `mod` 2147483398

\end{code}

\begin{code}

-- Q: do all of these merit class membership?
class Random a where
  randomR :: RandomGen g => (a,a) -> g -> (a,g)
  random  :: RandomGen g => g -> (a, g)
  
  randomRs :: RandomGen g => (a,a) -> g -> [a]
  randoms  :: RandomGen g => g -> [a]

  randomRIO :: (a,a) -> IO a
  randomIO  :: IO a
  
  randoms  g      = x : randoms g' where (x,g') = random g
  randomRs ival g = x : randomRs ival g' where (x,g') = randomR ival g
  
  randomIO	   = getStdRandom random
  randomRIO range  = getStdRandom (randomR range)

\end{code}

\begin{code}
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
  randomR (a,b) g = randomIvalDouble (float2Double a, float2Double b) double2Float g
  random g        = randomIvalDouble (0::Double,1) double2Float g

\end{code}


\begin{code}
mkStdRNG :: IO StdGen
mkStdRNG = do
    ct          <- getCPUTime
    (TOD sec _) <- getClockTime
    return (createStdGen (sec * 12345 + ct))

randomIvalInteger :: (RandomGen g, Num a) => (Integer, Integer) -> g -> (a, g)
randomIvalInteger (l,h) rng
 | l > h     = randomIvalInteger (h,l) rng
 | otherwise = case (f n 1 rng) of (v, rng') -> (fromInteger (v `mod` (k+1)), rng')
     where
       k = h - l + 1
       b = 2147483561
       n = iLogBase b k

       f 0 acc g = (acc, g)
       f n acc g = 
          let
	   (x,g')   = next g
	  in
	  f (n-1) (fromInt x + acc * b) g'

randomIvalDouble :: (RandomGen g, Fractional a) => (Double, Double) -> (Double -> a) -> g -> (a, g)
randomIvalDouble (l,h) fromDouble rng 
  | l > h     = randomIvalDouble (h,l) fromDouble rng
  | otherwise = 
       case (randomIvalInteger (toInteger (minBound::Int), toInteger (maxBound::Int)) rng) of
         (x, rng') -> 
	    let
	     scaled_x = 
		fromDouble l +
		fromDouble (h-l) *
		 (fromIntegral (x::Int) * 4.6566130638969828e-10)
	          -- magic number stolen from old HBC code (Random.randomDoubles.)
	    in
	    (scaled_x, rng')

iLogBase :: Integer -> Integer -> Integer
iLogBase b i = if i < b then 1 else 1 + iLogBase b (i `div` b)

rand1 :: StdGen -> (Int, StdGen)
rand1 (StdGen s1 s2) = (z', StdGen s1'' s2'')
	where	z'   = if z < 1 then z + 2147483562 else z
		z    = s1'' - s2''

		k    = s1 `quot` 53668
		s1'  = 40014 * (s1 - k * 53668) - k * 12211
		s1'' = if s1' < 0 then s1' + 2147483563 else s1'
    
		k'   = s2 `quot` 52774
		s2'  = 40692 * (s2 - k' * 52774) - k' * 3791
		s2'' = if s2' < 0 then s2' + 2147483399 else s2'

splitStdGen :: StdGen -> (StdGen, StdGen)
splitStdGen std@(StdGen s1 s2) = (std, StdGen new_s1 new_s2)
   where
       -- simple in the extreme..
      new_s1
        | s1 == 2147483562 = 1
	| otherwise	   = s1 + 1

      new_s2
        | s2 == 1	   = 2147483398
	| otherwise	   = s2 - 1

   
	
\end{code}


\begin{code}
global_rng :: MutableVar RealWorld StdGen
global_rng = unsafePerformIO $ do
   rng <- mkStdRNG
   stToIO (newVar rng)

setStdGen :: StdGen -> IO ()
setStdGen sgen = stToIO (writeVar global_rng sgen)

getStdGen :: IO StdGen
getStdGen = stToIO (readVar global_rng)

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
\end{code}

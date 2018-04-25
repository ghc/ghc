-- Simple computation of the dot product in Haskell (using various array
-- implementations)
--
-- Compile and run with 
--
--   ghc -ffi -O2 -fliberate-case-threshold100 -o dotprod DotProd.hs dotprod.o\
--     && ./dotprod +RTS -K10M

-- standard libraries
import CPUTime
import Random

-- FFI
import Foreign
import Foreign.C

-- GHC libraries
import Data.Array
import Data.Array.Unboxed (UArray)
import qualified
       Data.Array.Unboxed as U
import Control.Exception  (evaluate)
import System.Mem	  (performGC)


-- arrays types
--
type Vector  = Array  Int Float
type UVector = UArray Int Float
type CVector = Ptr Float

-- generates a random vector of the given length in NF
--
generateVector :: Int -> IO Vector
generateVector n =
  do
    rg <- newStdGen
    let fs  = take n $ randomRs (-100, 100) rg
	arr = listArray (0, n - 1) fs
    evaluate $ sum (elems arr)    -- make sure it is brought in NF
    return arr

-- convert a vector into an UVector in NF
--
vectorToUVector :: Vector -> IO UVector
vectorToUVector v = 
  do
    let uv = U.listArray (bounds v) . elems $ v
    evaluate $ sum (U.elems uv)
    return uv

-- convert a vector into a CVector in NF
--
vectorToCVector :: Vector -> IO CVector
vectorToCVector v = newArray (elems v)

-- compute the dot product 
--

-- vanilla
vectorDP1a :: Vector -> Vector -> IO Float
{-# NOINLINE vectorDP1a #-}
vectorDP1a v1 v2 = do
		     let r = sum [x * y | x <- elems v1 | y <- elems v2]
		     evaluate r

-- vanilla
vectorDP1b :: Vector -> Vector -> IO Float
{-# NOINLINE vectorDP1b #-}
vectorDP1b v1 v2 = do
		     let r = sum [v1!i * v2!i | i <- indices v1]
		     evaluate r

-- array combinators
vectorDP2 :: Vector -> Vector -> IO Float
{-# NOINLINE vectorDP2 #-}
vectorDP2 v1 v2 = do
		    let r = sumA (zipWithA (*) v1 v2)
		    evaluate r
  where
    zipWithA f v1 v2 = listArray (0, n1) (loop 0)
      where
      n1 = snd (U.bounds v1)
      loop i | i > n1    = []
	     | otherwise = f (v1!i) (v2!i) : loop (i + 1)
    --
    sumA v = loop 0
	     where
	       n1 = snd (U.bounds v)
	       loop i | i > n1    = 0
		      | otherwise = v!i + loop (i + 1)

-- explicit loop
vectorDP3 :: Vector -> Vector -> IO Float
{-# NOINLINE vectorDP3 #-}
vectorDP3 v1 v2 = 
  do
    let n1 = snd (U.bounds v1)
	r  = loop 0
	     where
	       loop i | i > n1    = 0
		      | otherwise = v1!i * v2!i + loop (i + 1)
    evaluate r

-- explicit loop w/ acc
vectorDP4 :: Vector -> Vector -> IO Float
{-# NOINLINE vectorDP4 #-}
vectorDP4 v1 v2 = 
  do
    let n1 = snd (U.bounds v1)
	r  = loop 0 0
	     where
	       loop i a | i > n1    = a
			| otherwise = loop (i + 1) (v1!i * v2!i + a)
    evaluate r

-- vanilla
uvectorDP1a :: UVector -> UVector -> IO Float
{-# NOINLINE uvectorDP1a #-}
uvectorDP1a v1 v2 = do
		      let r = sum $ zipWith (*) (U.elems v1) (U.elems v2)
		      evaluate r

-- vanilla
uvectorDP1b :: UVector -> UVector -> IO Float
{-# NOINLINE uvectorDP1b #-}
uvectorDP1b v1 v2 = do
		      let r = sum [v1 U.!i * v2 U.!i | i <- U.indices v1]
		      evaluate r

-- array combinators
uvectorDP2 :: UVector -> UVector -> IO Float
{-# NOINLINE uvectorDP2 #-}
uvectorDP2 v1 v2 = do
		     let r = sumA (zipWithA (*) v1 v2)
		     evaluate r
		       where
    zipWithA :: (Float -> Float -> Float) -> UVector -> UVector -> UVector
    zipWithA f v1 v2 = U.listArray (0, n1) (loop 0)
      where
        n1 = snd (U.bounds v1)
	loop i | i > n1    = []
	       | otherwise = f (v1 U.!i) (v2 U.!i) : loop (i + 1)
    --
    sumA v = loop 0
	     where
	       n1 = snd (U.bounds v)
	       loop i | i > n1    = 0
		      | otherwise = v U.!i + loop (i + 1)

-- explicit loop
uvectorDP3 :: UVector -> UVector -> IO Float
{-# NOINLINE uvectorDP3 #-}
uvectorDP3 v1 v2 = 
  do
    let n1 = snd (U.bounds v1)
	r  = loop 0
	     where
	       loop i | i > n1    = 0
		      | otherwise = v1 U.!i * v2 U.!i + loop (i + 1)
    evaluate r
    -- NB: main difference in Core to vectorDP3 is that here the compiler
    -- decided to first go into the recursion and then do the indexing of v1
    -- and v2, whereas in vectorDP3 it's the other way around

-- explicit loop w/ acc
uvectorDP4 :: UVector -> UVector -> IO Float
{-# NOINLINE uvectorDP4 #-}
uvectorDP4 v1 v2 = 
  do
    let n1 = snd (U.bounds v1)
	r  = loop 0 0
	     where
	       loop i a | i > n1    = a
			| otherwise = loop (i + 1) (v1 U.!i * v2 U.!i + a)
    evaluate r
    -- NB: this generates perfect code

-- merciless C code
foreign import ccall "dotprod.h" 
  cvectorDP :: CVector -> CVector -> Int -> IO Float

-- execute a function and print the result and execution time
--
execAndTime :: String	       -- description
	    -> IO Float        -- benchmarked computation
	    -> IO ()
execAndTime desc comp =
  do
    putStrLn $ "\n*** " ++ desc
    performGC
    start  <- getCPUTime
    result <- comp
    end    <- getCPUTime
    let duration = (end - start) `div` 1000000000
    putStrLn $ "Result      : " ++ show result
    putStrLn $ "Running time: " ++ show duration ++ "ms"

main :: IO ()
main  = do
  putStrLn "Dot product benchmark"
  putStrLn "====================="
  putStrLn $ "[time resolution: " ++ show (cpuTimePrecision `div` 1000000000)++
	     "ms]"
  --
  v1 <- generateVector 10000
  v2 <- generateVector 10000
  execAndTime "H98 arrays (ind'd compr) [n = 10000]" (vectorDP1b v1 v2)
  --
  v1 <- generateVector 20000
  v2 <- generateVector 20000
  execAndTime "H98 arrays (ind'd compr) [n = 20000]" (vectorDP1b v1 v2)
  --
  v1 <- generateVector 50000
  v2 <- generateVector 50000
  execAndTime "H98 arrays (par compr) [n = 50000]" (vectorDP1a v1 v2)
  execAndTime "H98 arrays (ind'd compr) [n = 50000]" (vectorDP1b v1 v2)
  execAndTime "H98 arrays (combinator-based) [n = 50000]" (vectorDP2 v1 v2)
  execAndTime "H98 arrays (explicit loop) [n = 50000]" (vectorDP3 v1 v2)
  execAndTime "H98 arrays (explicit loop w/ acc) [n = 50000]" (vectorDP4 v1 v2)
  uv1 <- vectorToUVector v1
  uv2 <- vectorToUVector v2
  execAndTime "UArray (par compr) [n = 50000]" (uvectorDP1a uv1 uv2)
  execAndTime "UArray (ind'd compr) [n = 50000]" (uvectorDP1b uv1 uv2)
  execAndTime "UArray (combinator-based) [n = 50000]" (uvectorDP2 uv1 uv2)
  execAndTime "UArray (explicit loop) [n = 50000]" (uvectorDP3 uv1 uv2)
  execAndTime "UArray (explicit loop w/ acc) [n = 50000]" (uvectorDP4 uv1 uv2)
  --
  v1 <- generateVector 100000
  v2 <- generateVector 100000
  execAndTime "H98 arrays (par compr) [n = 100000]" (vectorDP1a v1 v2)
  execAndTime "H98 arrays (ind'd compr) [n = 100000]" (vectorDP1b v1 v2)
  execAndTime "H98 arrays (combinator-based) [n = 100000]" (vectorDP2 v1 v2)
  execAndTime "H98 arrays (explicit loop) [n = 100000]" (vectorDP3 v1 v2)
  execAndTime "H98 arrays (explicit loop w/ acc) [n = 100000]"(vectorDP4 v1 v2)
  uv1 <- vectorToUVector v1
  uv2 <- vectorToUVector v2
  execAndTime "UArray (par compr) [n = 100000]" (uvectorDP1a uv1 uv2)
  execAndTime "UArray (ind'd compr) [n = 100000]" (uvectorDP1b uv1 uv2)
  execAndTime "UArray (combinator-based) [n = 100000]" (uvectorDP2 uv1 uv2)
  execAndTime "UArray (explicit loop) [n = 100000]" (uvectorDP3 uv1 uv2)
  execAndTime "UArray (explicit loop w/ acc) [n = 100000]" (uvectorDP4 uv1 uv2)
  cv1 <- vectorToCVector v1
  cv2 <- vectorToCVector v2
  execAndTime "C [n = 100000]" (cvectorDP cv1 cv2 100000)
  --
  v1 <- generateVector 500000
  v2 <- generateVector 500000
  uv1 <- vectorToUVector v1
  uv2 <- vectorToUVector v2
  execAndTime "UArray (explicit loop w/ acc) [n = 500000]" (uvectorDP4 uv1 uv2)
  cv1 <- vectorToCVector v1
  cv2 <- vectorToCVector v2
  execAndTime "C [n = 500000]" (cvectorDP cv1 cv2 500000)

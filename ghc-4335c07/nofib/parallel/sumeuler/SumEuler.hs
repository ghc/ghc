{-# OPTIONS -Wall #-}
--
-- Euler totient function (strategic version).
-- Orig taken from "Research Directions in Parallel Functional Programming",
-- Chapter "Performance Monitoring", Nathan Charles and Colin Runciman.
--
-- (c) 2001 Hans-Wolfgang Loidl
--
-- modifications by Jost Berthold, 07/2008:
--    ported to current GHC = hierarchical libraries
--    removed some dead (unused) code
--    simplified/secured usage
--    included a reference computation (using prime numbers)
--    tested best version (JFP_Final) against two "equivalents" using
--          strategies
--
---------------------------------------------------------------------------

module Main where

import System.Environment (getArgs)
import Control.Parallel.Strategies
import Control.Parallel
import Control.Monad (when)

import ListAux -- split/join functions, put in new module
import SumEulerPrimes

import Data.List(foldl1')

---------------------------------------------------------------------------
-- Generic clustering routines

-- Classes
-- maybe: class (Functor c) => MMonad c where { ... mmap = fmap } 
class MMonad c where
  munit  :: a -> c a
  mjoin  :: c (c a) ->  c a
  mmap   :: (a -> b) -> c a -> c b

class (MMonad c) => MMonadPlus c where
  mzero :: c a
  mplus :: c a -> c a -> c a

class (MMonad c) => Cluster c where
  cluster    :: Int -> c a -> c (c a)
  decluster  :: c (c a) -> c a
  lift       :: (c a -> b) -> (c (c a) -> (c b))
  -- default defs
  --cluster = ???
  decluster = mjoin
  lift      = mmap

-- Instances
instance MMonad [] where
  munit x = [x]
  mjoin   = concat
  mmap    = map

instance Cluster [] where
  cluster   = splitAtN

---------------------------------------------------------------------------

usage :: String
usage = "Usage: <program> version size chunksize"
        ++"\nFor versions see source code."

main :: IO ()
main = do args <- getArgs
          let 
            lenArgs = length args
          when (lenArgs < 3) 
                   (putStrLn (usage ++ "\n(using defaults: 38,5000,100)"))
          let 
            argDef :: Read a => Int -> a -> a
            argDef m defVal | m < lenArgs = read (args!!m)
                            | otherwise   = defVal
            x, n, c :: Int
            x = argDef 0 38   -- which sumEuler to use
            n = argDef 1 5000 -- size of the interval
            c = argDef 2 100  -- chunksize
            -- parallel computation
            (res, _str) = case x of
                    ------------------
                    -- BEST VERSION:
                    38 -> (sumEulerJFP_Final c n, "JFP_Final paper version (splitAtN)")
                    -- VERSIONS TO TEST ADDITIONALLY:
--		    48 -> (sumEulerS8 c n,   "parallel w/ parChunkFoldMap strategy")
--		    58 -> (sumEulerS8' c n,   "parallel w/ parChunkFold'Map strategy")                    
                    8 -> (sumEulerJFP c n, "JFP paper version (splitAtN)")
                    ------------------
                    0 -> (sumEuler_seq n,   "sequential")
                    1 -> (sumEulerS1 n,     "parallel w/ parList strategy")
                    -- not bad:
                    2 -> (sumEulerS2 c n,   "parallel w/ parListChunk")
		    3 -> (sumEulerChunk c n,"parallel w/ chunkify")
		    4 -> (sumEulerShuffle c n,"parallel w/ shuffle")
		    5 -> (sumEulerCluster c n,"parallel w/ generic clustering")
                    -- not bad:
		    6 -> (sumEulerS6 c n,   "parallel w/ parListChunk over reversed list")
--		    7 -> (sumEulerS7 c n,   "parallel w/ parChunkFoldMap strategy")
                    18 -> (sumEulerJFP1 c n, "JFP1 paper version (splitIntoChunks)")
                    28 -> (sumEulerJFP0 c n, "JFP0 paper version (explicit list comprh)")
--                    9 -> (sumEulerStepList c n, "parallel w/ seqStepList for strategic shuffling")
                    _ -> error "undefined version."

          putStrLn ("sumEuler [" ++ show base ++ ".." ++ show (base+n) ++ "] = " ++ show res)

          -- reference implementation (which is rather fast)
          let expected = sumPhi n
          when False $ putStrLn ("Expected result: " ++ show expected)

---------------------------------------------------------------------------
-- main computation function in many variants

-- HERE: best versions in contrast

sumEulerJFP  :: Int -> Int -> Int
sumEulerJFP c n = sum (map (sum . map euler) (splitAtN c (mkList n))
                       `using` parList rdeepseq)   

sumEulerJFP_Final  :: Int -> Int -> Int
sumEulerJFP_Final c n = sum ([(sum . map euler) x | x <- splitAtN c [n,n-1..0]]
                            `using` parList rdeepseq)   

-- -- using a fold-of-map strategy w/ folding inside a chunk
-- sumEulerS8 :: Int -> Int -> Int
-- sumEulerS8 c n  = parChunkFoldMap c rnf (+) euler (mkList n)
-- 
-- -- using a fold-of-map strategy w/ STRICT LEFT-folding inside a chunk
-- sumEulerS8' :: Int -> Int -> Int
-- sumEulerS8' c n  = parChunkFoldMap' c rnf (+) euler (mkList n)
-- 
-- -- parallel fold-of-map with chunking over fold and map
-- parChunkFoldMap :: (NFData b) => Int -> Strategy b -> 
--                                  (b -> b -> b) -> (a -> b) -> [a] -> b
-- parChunkFoldMap c s f g xs = foldl1 f (map (foldl1 f . map g) 
-- 		                           (splitAtN c xs)
-- 		                       `using` parList s)	
-- 
-- -- parallel fold-of-map with chunking over fold and map
-- parChunkFoldMap' :: (NFData b) => Int -> Strategy b -> 
--                                  (b -> b -> b) -> (a -> b) -> [a] -> b
-- parChunkFoldMap' c s f g xs = foldl1' f (map (foldl1' f . map g) 
-- 		                           (splitAtN c xs)
-- 		                       `using` parList s)	

-----------------------------------------------------------------------
-- OTHER VARIANTS

-- strategic function application
sumEulerS1 :: Int -> Int
sumEulerS1 n  = sum ( map euler (mkList n)
                        `using` 
	                parList rdeepseq )

-- NUKED:
-- sumEulerS1 c n  = sum $|| (parListChunk c rnf) $ map euler $ mkList $ n

-- naive parallel version w/ parList
sumEulerS2 :: Int -> Int -> Int
sumEulerS2 c n  = sum ( map euler (mkList n)
                        `using` 
	                parListChunk c rdeepseq )

-- using a parallel fold over a chunkified list
sumEulerS6 :: Int -> Int -> Int
sumEulerS6 c n  = sum (map (sum . map euler) (splitAtN c (mkList n))
		       `using` parList rdeepseq)	

-- -- using a fold-of-map strategy over a chunkified list
-- sumEulerS7 :: Int -> Int -> Int
-- sumEulerS7 c n  = parFoldChunkMap c rnf (+) euler (mkList n)

-- explicit restructuring
sumEulerChunk :: Int -> Int -> Int
sumEulerChunk c n  = sum (parMap rdeepseq ( \ xs -> sum (map euler xs)) 
                                     (splitAtN c (mkList n)))

-- using generic clustering functions
sumEulerCluster :: Int -> Int -> Int
sumEulerCluster c n = sum ((lift worker) (cluster c (mkList n)) 
                           `using` parList rdeepseq)
                      where worker = sum . map euler

-- using a shuffling to improve load balance
sumEulerShuffle :: Int -> Int -> Int
sumEulerShuffle c n  = sum ((map worker) (unshuffle (noFromSize c n) (mkList n))
                           `using` parList rdeepseq)
                       where worker = sum . map euler

noFromSize :: Int -> Int -> Int
noFromSize c n | n `mod` c == 0 = n `div` c 
               | otherwise      = n `div` c + 1

-- -- Evaluates every n-th element in the list starting with the first elem
-- seqStepList :: Int -> Strategy a -> Strategy [a]
-- seqStepList _ _strat []    = ()
-- seqStepList n strat (x:xs) = strat x `pseq` (seqStepList n strat (drop (n-1) xs))
-- 
-- seqStepList' :: Int -> Strategy a -> Strategy [a]
-- -- seqStepList' _ strat [] = ()
-- seqStepList' n strat xs = parList (\ i -> seqStepList n strat (drop i xs)) [0..n-1]
-- 
-- sumEulerStepList :: Int -> Int -> Int
-- sumEulerStepList c n  = sum ( map euler (mkList n)
--                               `using` 
-- 	                      seqStepList' n' rnf )
--                        where --worker = sum . map euler
--                              n' = if n `mod` c == 0 then n `div` c else (n `div` c)+1

-- ---------------------------------------------------------------------------
-- Variants of the code for the JFP paper
-- ---------------------------------------------------------------------------

sumEulerJFP0  :: Int -> Int -> Int
sumEulerJFP0 c n = sum ([ (sum . map euler) [ c*i+j | j <- [0..c-1], c*i+j<=n ]
                        | i <- [0..(n+c-1) `div` c - 1] ]
                       `using` parList rdeepseq)   

sumEulerJFP1  :: Int -> Int -> Int
sumEulerJFP1 c n = sum (map (sum . map euler) (splitIntoChunks c n)
                        `using` parList rdeepseq)   

splitIntoChunks :: Int -> Int -> [[Int]]
splitIntoChunks c n = [ [ c*i+j | j <- [0..c-1], c*i+j<=n ]
                      | i <- [0..(n+c-1) `div` c - 1] ]

-- boring sequential version
sumEuler_seq :: Int -> Int
sumEuler_seq = sum . map euler . mkList 

---------------------------------------------------------------------------
-- smallest input for euler
base :: Int
base = 0

-- produce a list of input values
mkList :: Int -> [Int]
mkList = reverse . enumFromTo base . (+ base)
-- random numbers
-- mkList seed n = take n (randoms seed)

---------------------------------------------------------------------------
-- main fct

euler :: Int -> Int
euler n = length (filter (relprime n) [1..(n-1)])

---------------------------------------------------------------------------
-- orig code from Nathan
{-
euler :: Int -> Int
euler n = let
            relPrimes = let
                          numbers = [1..(n-1)]
                        in
                          numbers `par` (filter (relprime n) numbers)
          in
            (spine relPrimes) `par` (length relPrimes)
-}

---------------------------------------------------------------------------
-- aux fcts

hcf :: Int -> Int -> Int
hcf x 0 = x
hcf x y = hcf y (rem x y)

relprime :: Int -> Int -> Bool
relprime x y = hcf x y == 1

---------------------------------------------------------------------------
-- Strategy code
---------------------------------------------------------------------------

-- Strategy combining fold and map
parChunkFold :: Int -> Strategy a -> (a -> a -> a) -> [a] -> a
parChunkFold c s f xs = foldl1 f (map (foldl1 f) yss `using` parList s)
                        where yss = splitAtN c xs

parFoldMap :: Strategy b -> (b -> b -> b) -> (a -> b) -> [a] -> b
parFoldMap s f g xs = foldl1 f (map g xs `using` parList s)

---- parallel fold-of-map with chunking over map only
--parFoldChunkMap :: (NFData b) => Int -> Strategy b -> 
--                                 (b -> b -> b) -> (a -> b) -> [a] -> b
--parFoldChunkMap c s f g xs = foldl1 f (map g xs `using` parListChunk c s)


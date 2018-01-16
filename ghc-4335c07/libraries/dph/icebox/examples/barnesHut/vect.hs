module Main where
import qualified BarnesHutVect as V

import Data.Array.Parallel.PArray (PArray)
import qualified Data.Array.Parallel.PArray as P
import Data.Array.Parallel.Base ( (:*:)(..) )
import Random (Random, RandomGen, getStdGen, randoms, randomRs)
import Control.Monad
import Control.Exception (evaluate)
import System.Console.GetOpt

import Bench.Benchmark
import Bench.Options

import Debug.Trace

type MassPoint = (Double, Double, Double)

bhStepVect (dx, dy, particles) = trace (show  accs) accs  
  where
    accs = V.oneStep 0.0 0.0 dx dy particles

mapData:: IO (Bench.Benchmark.Point (PArray Double))
mapData = do
  evaluate (P.nf testData)
  return $ ("N = " ) `mkPoint` testData
  where
    testData:: PArray Double
    testData = P.fromList $ map fromIntegral [0..10000000]



-- simpleTest:: 
simpleTest:: [Int] -> Double -> Double -> IO (Bench.Benchmark.Point (Double, Double, PArray MassPoint))
simpleTest _ _ _=
  do
    evaluate testData
    return $ ("N = " ) `mkPoint` testData
  where
    testData = (1.0, 1.0, testParticles)
    -- particles in the bounding box 0.0 0.0 1.0 1.0
    testParticles:: PArray MassPoint
    testParticles = P.fromList [
       (0.3, 0.2, 5.0),
       (0.2, 0.1, 5.0),
       (0.1, 0.2, 5.0),
       (0.8, 0.8, 5.0),
       (0.7, 0.9, 5.0), 
       (0.8, 0.9, 5.0),
       (0.6, 0.6, 5.0),
       (0.7, 0.7, 5.0),
       (0.8, 0.7, 5.0),
       (0.9, 0.9, 5.0)]

main = ndpMain "BarnesHut"
               "[OPTION] ... SIZES ..."
               run [] ()

run opts () sizes =
    case map read sizes of
      []   -> failWith ["No sizes specified"]
      [sz] -> do 
               benchmark opts bhStepVect [randomDistTest sz 1 1] P.nf show
               return ()

randomDistTest :: Int -> Double -> Double -> IO (Bench.Benchmark.Point (Double, Double, PArray MassPoint))
randomDistTest n dx dy = 
 do
    testParticles <- liftM (take n) $ randomMassPointsIO dx dy 
    evaluate testParticles
    print testParticles
    return $ ("N = " ) `mkPoint` (dy, dx, P.fromList testParticles)



{-
run opts alg sizes =
  case lookup alg algs of
    Nothing -> failWith ["Unknown algorithm"]
    Just f  -> case map read sizes of
                 []    -> failWith ["No sizes specified"]
                 [sz]  -> do
                            benchmark opts f [randomDistTest sz 1000 1000]
                                             (`seq` ()) show
                            return ()
-}

epsilon = 0.05

randomTo, randomFrom :: Integer
randomTo    = 2^30
randomFrom  = - randomTo

randomRIOs       :: Random a => (a, a) -> IO [a]
randomRIOs range  = liftM (randomRs range) getStdGen 

randomIOs :: Random a => IO [a]
randomIOs  = liftM randoms getStdGen 

--  generate a stream of random numbers in [0, 1)
--
randomDoubleIO :: IO [Double]
randomDoubleIO  = randomIOs

-- generate an infinite list of random mass points located with a homogeneous
-- distribution around the origin within the given bounds
--
randomMassPointsIO       :: Double -> Double -> IO [MassPoint]
randomMassPointsIO dx dy  = do
			    rs <- randomRIOs (randomFrom, randomTo)
			    return (massPnts rs)
		            	  where
			    to    = fromIntegral randomTo
			    from  = fromIntegral randomFrom
			    xmin  = - (dx / 2.0)
			    ymin  = - (dy / 2.0)
			    xfrac = (to - from) / dx
			    yfrac = (to - from) / dy

			    massPnts               :: [Integer] -> [MassPoint]
			    massPnts (xb:yb:mb:rs)  = 
			      (x, y, m) : massPnts rs
			      where
				m = (fromInteger . abs) mb + epsilon
				x = xmin + (fromInteger xb) / xfrac
				y = ymin + (fromInteger yb) / yfrac


{-# LANGUAGE TypeOperators #-}
module Main where
import qualified BarnesHutSeq as Seq
import qualified BarnesHutPar as Par
import qualified BarnesHutList as L
import BarnesHutGen

import Control.Exception (evaluate)
import System.Console.GetOpt

import Data.Array.Parallel.Unlifted.Sequential
import Data.Array.Parallel.Unlifted.Parallel
import Data.Array.Parallel.Base ( (:*:)(..) )

import Bench.Benchmark
import Bench.Options

import Debug.Trace



algs = [("seq", Seq.bhStep), ("par", Par.bhStep), ("list", bhStepList)]

bhStepList (dx, dy, particles) = trace (show  accs) accs  
  where
    accs       = zipU (toU xs) (toU ys) 
    (xs, ys)   = L.oneStep 0.0 0.0 dx dy particles'
    (p1 :*: p2 :*: p3) = unzip3U $ concatSU particles
    particles' = zip3 (fromU p1) (fromU p2) (fromU p3) 



mapData:: IO (Bench.Benchmark.Point (UArr Double))
mapData = do
  evaluate testData
  return $ ("N = " ) `mkPoint` testData
  where
    testData:: UArr Double
    testData = toU $ map fromIntegral [0..10000000]



-- simpleTest:: 
simpleTest:: [Int] -> Double -> Double -> IO (Bench.Benchmark.Point (Double, Double, SUArr MassPoint))
simpleTest _ _ _=
  do
    evaluate testData
    return $ ("N = " ) `mkPoint` testData
  where
    testData = (1.0, 1.0,  singletonSU testParticles)
    -- particles in the bounding box 0.0 0.0 1.0 1.0
    testParticles:: UArr MassPoint
    testParticles = toU [
       0.3 :*: 0.2 :*: 5.0,
       0.2 :*: 0.1 :*: 5.0,
       0.1 :*: 0.2 :*: 5.0,
       0.8 :*: 0.8 :*: 5.0,
       0.7 :*: 0.9 :*: 5.0, 
       0.8 :*: 0.9 :*: 5.0,
       0.6 :*: 0.6 :*: 5.0,
       0.7 :*: 0.7 :*: 5.0,
       0.8 :*: 0.7 :*: 5.0,
       0.9 :*: 0.9 :*: 5.0]


randomDistTest n dx dy = 
  do
    testParticles <- randomMassPointsIO dx dy 
    let su = singletonSU . toU $ take n testParticles
    evaluate (segdSU su)
    evaluate (concatSU su)
    return $ ("N = " ) `mkPoint` (dx, dy, su)

main = ndpMain "BarnesHut"
               "[OPTION] ... SIZES ..."
               run [Option ['a'] ["algo"] (ReqArg const "ALGORITHM")
                     "use the specified algorithm"]
                   "seq" 

run opts alg sizes =
  case lookup alg algs of
    Nothing -> failWith ["Unknown algorithm"]
    Just f  -> case map read sizes of
                 []    -> failWith ["No sizes specified"]
                 [sz]  -> do
                            benchmark opts f [randomDistTest sz 1000 1000]
                                             (`seq` ()) show
                            return ()
{-
                 szs -> do 
                          benchmark opts f [simpleTest szs 0  0] (`seq` ()) show
                          return ()
-}


r

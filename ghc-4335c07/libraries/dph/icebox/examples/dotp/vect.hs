import DotPVect ( dotp )

import Control.Exception (evaluate)
import System.Console.GetOpt
import qualified System.Random as R

import qualified Data.Array.Parallel.Unlifted as U
import qualified Data.Array.Parallel.PArray   as P
import Data.Array.Parallel.PArray (PArray)

import Bench.Benchmark
import Bench.Options

generateVectorU :: Int -> IO (U.Array Double)
generateVectorU n =
  do
    rg <- R.newStdGen
    let -- The std random function is too slow to generate really big vectors
        -- with.  Instead, we generate a short random vector and repeat that.
        randvec = U.randomRs k (-100, 100) rg
        vec     = U.map (\i -> randvec U.!: (i `mod` k)) (U.enumFromTo 0 (n-1))
    evaluate vec
    return vec
  where
    k = 1000

generateVector :: Int -> IO (PArray Double)
generateVector n 
  = do
      vec <- generateVectorU n
      return $ P.fromUArrPA' vec

generateVectors :: Int -> IO (Point (PArray Double, PArray Double))
generateVectors n =
  do
    v <- generateVector n
    w <- generateVector n
    return $ ("N = " ++ show n) `mkPoint` (v,w)

main = ndpMain "Dot product"
               "[OPTION] ... SIZES ..."
               run [] ()

run opts () sizes =
  case map read sizes of
    []  -> failWith ["No sizes specified"]
    szs -> do
             benchmark opts (uncurry dotp)
                (map generateVectors szs)
                (`seq` ()) show
             return ()


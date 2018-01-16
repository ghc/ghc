import qualified Types as T
import Delaunay

import qualified Data.Array.Parallel.Unlifted as U
import qualified Data.Array.Parallel.Prelude  as P
import Data.Array.Parallel.PArray as PA

import Bench.Benchmark
import Bench.Options

import System.IO
import Control.Exception (evaluate)

loadPoints :: String -> IO (Point (PArray T.Point))
loadPoints file
  = do
      h <- openBinaryFile file ReadMode
      pts <- U.hGet h
      hClose h
      let points = P.fromUArrPA_2' pts
      evaluate $ nf points
      return $ ("N = " ++ show (U.length pts)) `mkPoint` points

main = ndpMain "Delaunay"
               "[OPTION] ... FILE"
               run [] ()

run opts () [] = failWith ["No sizes or input files specified"]
run opts () args =
  do
    benchmark opts delaunayPoints
        (map loadPoints args)
        nf
        (\ps -> "Result length = " ++ show (PA.length ps))
    return ()


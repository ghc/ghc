module Main where
import QSortVect

import Control.Exception (evaluate)
import System.Console.GetOpt
import qualified System.Random as R

import Data.Array.Parallel.PArray (PArray)
import qualified Data.Array.Parallel.PArray as P

import Bench.Benchmark
import Bench.Options

import Debug.Trace

generateVector :: Int -> IO (Point (PArray Double))
generateVector n =
  do
    evaluate (P.nf vec)
    return $ ("N = " ++ show n) `mkPoint` vec
  where
    vec = P.fromList (reverse [1..fromInteger (toInteger n)])

main = ndpMain "QSort"
               "[OPTION] ... SIZES ..."
               run [] ()

run opts () sizes =
  case map read sizes of
    []  -> failWith ["No sizes specified"]
    szs -> do
             benchmark opts qsortVect (map generateVector szs) P.nf show
             return ()


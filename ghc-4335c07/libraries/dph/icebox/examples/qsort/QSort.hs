{-# OPTIONS -fno-spec-constr-count #-}
module Main where
import QSortSeq
import QSortPar
import QSortVect

import Control.Exception (evaluate      )
import System.Console.GetOpt

import Data.Array.Parallel.Unlifted
import Data.Array.Parallel.Unlifted.Parallel
import Data.Array.Parallel.Prelude (toUArrPA, fromUArrPA')

import Bench.Benchmark
import Bench.Options

import Debug.Trace

algs = [("seq", qsortSeq), ("par", qsortPar), ("list", toU. qsortList . fromU), ("vect", qsortVect')]



qsortVect':: UArr Double -> UArr Double
qsortVect' xs =  trace (show res) 
  res
  where  
    res = toUArrPA $ qsortVect $ fromUArrPA' xs


generateVector :: Int -> IO (Point (UArr Double))
generateVector n =
  do
    evaluate vec
    return $ ("N = " ++ show n) `mkPoint` vec
  where
    vec = toU (reverse [1..fromInteger (toInteger n)])

main = ndpMain "QSort"
               "[OPTION] ... SIZES ..."
               run [Option ['a'] ["algo"] (ReqArg const "ALGORITHM")
                     "use the specified algorithm"]
                   "seq"

run opts alg sizes =
  case lookup alg algs of
    Nothing -> failWith ["Unknown algorithm"]
    Just f  -> case map read sizes of
                 []  -> failWith ["No sizes specified"]
                 szs -> do
                          benchmark opts f (map generateVector szs) show
                          return ()


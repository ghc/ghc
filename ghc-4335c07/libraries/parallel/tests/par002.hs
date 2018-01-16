-- test for a bug in blackhole handling in the garbage collector,
-- fixed on 7/4/2006.  The symptom is that stdout gets finalized too
-- early, and the main thread fails when writing to it.  Only happens
-- with +RTS -N2, and must be compiled without optimisation.

module Main (main) where

import Control.Parallel.Strategies
import Control.Exception

f x 0 = x
f x y = f ((x+y)-y) (y-1)

bigcomputation :: Int -> Int
bigcomputation x = f x 50000

main = do 
  let seeds = [1..12]
  let seeds2 = map bigcomputation seeds
  mapM (\v -> putStr ((show v) ++ " " )) seeds
  putStr "\n"
  evaluate (seeds2 `using` parList rwhnf)
  mapM (\v -> putStr ((show v) ++ " " )) seeds2
  putStr "\n"

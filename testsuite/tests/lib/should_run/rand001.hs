module Main(main) where

import System.Random

tstRnd rng = checkRange rng (genRnd 50 rng)
 
genRnd n rng = take n (randomRs rng (mkStdGen 2))
 
checkRange (lo,hi) = all pred
  where
   pred
    | lo <= hi  = \ x -> x >= lo && x <= hi
    | otherwise = \ x -> x >= hi && x <= lo

main :: IO ()
main = do
  print (tstRnd (1,5::Double))
  print (tstRnd (1,5::Int))
  print (tstRnd (10,54::Integer))
  print (tstRnd ((-6),2::Int))
  print (tstRnd (2,(-6)::Int))


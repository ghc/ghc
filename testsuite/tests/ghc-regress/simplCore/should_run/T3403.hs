{-# LANGUAGE BangPatterns #-}


-- See Trac #3403: interaction of pattern match failure and CPR
-- The point is that this should run in constant space, with no
-- stack growth.  In GHC 6.10 the tail call optimisation didn't work.

module Main (main) where

import qualified Data.Set as Set
import Data.Set (Set)

data Result = Result !S1 !S2

type S1 = Set ()
type S2 = Set ()

input :: [[(Int, ())]]
input = replicate 1000 (replicate 400 (100, ()))

main :: IO ()
main = do let Result s1 s2 = doAll Set.empty Set.empty () input
          print $ Set.size s1
          print $ Set.size s2

doAll :: S1 -> S2 -> () -> [[(Int, ())]] -> Result
doAll !s1 !s2 !_    [] = Result s1 s2
doAll !s1 !s2 !unit ([] : xs) = doAll s1 s2 unit xs
doAll !s1 !s2 !unit (((t, _) : x1) : x2 : xs)
 | t >= 99999 = doAll s1 s2 unit (x1 : x2 : xs)
doAll !s1 !s2 !unit (((_, ()) : x) : xs)
    = doAll s1 s2 unit (x : xs)

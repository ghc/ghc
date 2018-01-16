{-# LANGUAGE RankNTypes #-}
module Main (main) where

import Data.List

-- A simple implementation of inits that should be obviously correct.
{-# NOINLINE initsR #-}
initsR :: [a] -> [[a]]
initsR = map reverse . scanl (flip (:)) []

-- The inits implementation added in 7.10 uses a queue rotated around
-- powers of 2, starting the rotation only at size 255, so we want to check
-- around powers of 2 and around the switch.
ranges :: [Int]
ranges = [0..20] ++ [252..259] ++ [508..515]

simple :: (forall a . [a] -> [[a]]) -> [[[Int]]]
simple impl = [impl [1..n] | n <- ranges]

-- We want inits (xs ++ undefined) = inits xs ++ undefined
laziness :: Bool
laziness = [take (n+1) (inits $ [1..n] ++ undefined) | n <- ranges]
              == simple inits

main :: IO ()
main | simple initsR /= simple inits = error "inits failed simple test"
     | not laziness = error "inits failed laziness test"
     | otherwise = return ()

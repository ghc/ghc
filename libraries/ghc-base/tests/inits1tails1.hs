{-# LANGUAGE RankNTypes #-}
module Main (main) where

import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NEL

-- The inits implementation added in 7.10 uses a queue rotated around
-- powers of 2, starting the rotation only at size 255, so we want to check
-- around powers of 2 and around the switch.
ranges :: [Int]
ranges = [1..20] ++ [252..259] ++ [508..515]

nonEmptyUpTo :: Int -> NonEmpty Int
nonEmptyUpTo n | n >= 1 = NEL.fromList [1..n]
nonEmptyUpTo n = error $ "nonEmptyUpTo: invalid argument: " ++ show n

simple :: (forall a . NonEmpty a -> [[a]]) -> [[[Int]]]
simple impl = [impl (nonEmptyUpTo n) | n <- ranges]

nonEmptyInits1 :: NonEmpty a -> [[a]]
nonEmptyInits1 = map NEL.toList . NEL.toList . NEL.inits1

-- inits1 should be the same as inits on nonempty lists, except that the first
-- element should not be included
alternativeInits1 :: NonEmpty a -> [[a]]
alternativeInits1 = tail . List.inits . NEL.toList

nonEmptyTails1 :: NonEmpty a -> [[a]]
nonEmptyTails1 = map NEL.toList . NEL.toList . NEL.tails1

-- tails1 should be the same as tails on nonempty lists, except that the last
-- element should not be included
alternativeTails1 :: NonEmpty a -> [[a]]
alternativeTails1 = init . List.tails . NEL.toList

-- We want inits1 (xs <> undefined) = inits1 xs <> undefined
-- (there's no similar property for tails1 because that function starts with the
-- longest suffix)
lazinessInits1 :: Bool
lazinessInits1 = [take n (nonEmptyInits1 (nonEmptyUpTo n <> undefined)) | n <- ranges]
                  == simple nonEmptyInits1

main :: IO ()
main | simple nonEmptyInits1 /= simple alternativeInits1 = error "inits1 failed simple test"
     | simple nonEmptyTails1 /= simple alternativeTails1 = error "tails1 failed simple test"
     | not lazinessInits1 = error "inits1 failed laziness test"
     | otherwise = return ()

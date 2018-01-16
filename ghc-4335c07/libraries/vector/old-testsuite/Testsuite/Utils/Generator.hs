module Testsuite.Utils.Generator (
  indices, index_value_pairs
) where

import Test.QuickCheck

indices :: Int -> Gen [Int]
indices 0 = return []
indices m = sized $ \n ->
  do
    len <- choose (0,n)
    sequence [choose (0,m-1) | i <- [1..len]]

index_value_pairs :: Arbitrary a => Int -> Gen [(Int,a)]
index_value_pairs 0 = return [] 
index_value_pairs m = sized $ \n ->
  do
    len <- choose (0,n)
    is <- sequence [choose (0,m-1) | i <- [1..len]]
    xs <- vector len
    return $ zip is xs


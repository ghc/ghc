module Y where

import Data.Array

fibonacci :: Int -> Array Int Int
fibonacci n = worker
  where
    worker = array (1, n) $ (1, 0) : (2, 1) : [ (i, worker ! (i - 2) + worker ! (i - 1)) | i <- [3..n]]

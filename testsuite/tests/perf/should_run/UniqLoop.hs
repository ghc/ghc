{-# LANGUAGE BangPatterns #-}

module Main where

import GHC.Types.Unique.Supply
import GHC.Types.Unique

-- Generate a lot of uniques
main = do
    us <- mkSplitUniqSupply 'v'
    seq (churn us 10000000) (return ())

churn :: UniqSupply -> Int -> Int
churn !us 0 = getKey $ uniqFromSupply us
churn us n =
  let (!x,!us') = takeUniqFromSupply us
  in churn us' (n-1)

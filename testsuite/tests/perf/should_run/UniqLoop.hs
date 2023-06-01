{-# LANGUAGE BangPatterns #-}

module Main where

import GHC.Types.Unique.Supply
import GHC.Types.Unique
import Data.Word

-- Generate a lot of uniques
main = do
    us <- mkSplitUniqSupply 'v'
    seq (churn us 10000000) (return ())

churn :: UniqSupply -> Word64 -> Word64
churn !us 0 = getKey $ uniqFromSupply us
churn us n =
  let (!x,!us') = takeUniqFromSupply us
  in churn us' (n-1)

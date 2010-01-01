-- !!! Expressions as patterns inside do stmt blocks
module ShouldFail where

f :: Int -> IO Int
f x = do
  (2+2) <- 2
  return x


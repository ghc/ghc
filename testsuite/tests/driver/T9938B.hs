module Main where

import Control.Monad
import Control.Monad.Trans.State

solve :: Int -> StateT () [] ()
solve 0 = mzero
solve carry | carry > 0 =
  do guard (0 == carry)
     solve (carry -1)

main :: IO ()
main = return ()

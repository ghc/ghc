module Main where

import Control.Monad
import Control.Monad.Trans.State

solve :: Int -> StateT () [] ()
solve carry | carry > 0 =
  do guard (0 == carry)
     solve (carry -1)
solve 0 = mzero

main :: IO ()
main = return ()

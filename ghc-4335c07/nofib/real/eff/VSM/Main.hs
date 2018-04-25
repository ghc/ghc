module Main (main) where

import EffBench
import Control.Exception.Base

n :: Int
n = 10000000

main = do

  putStrLn "VSM"
  evaluate $ vmrunState (times n $ vmmodify (+1)) 0


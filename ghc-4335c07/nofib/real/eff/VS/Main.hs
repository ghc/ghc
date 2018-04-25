module Main (main) where

import EffBench
import Control.Exception.Base

n :: Int
n = 10000000

main = do

  putStrLn "VS"
  evaluate $ vrunState (times n $ (vmodify (+1))) 0


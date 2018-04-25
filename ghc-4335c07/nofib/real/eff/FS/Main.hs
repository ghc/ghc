module Main (main) where

import EffBench
import Control.Exception.Base

n :: Int
n = 10000000

main = do

  putStrLn "FS"
  evaluate $ frunState (times n $ (fmodify (+1))) 0


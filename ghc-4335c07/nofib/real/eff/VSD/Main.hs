module Main (main) where

import EffBench
import Control.Exception.Base
import qualified Control.Monad.State.Strict as S

n :: Int
n = 10000000

main = do

  putStrLn "VSD"
  evaluate $ S.runState (times n $ (vmdmodify :: (Int -> Int) -> S.State Int ())  (+1)) (0 :: Int)


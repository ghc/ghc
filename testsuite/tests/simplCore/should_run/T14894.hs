{-# OPTIONS_GHC -g1 -O #-}
import System.Environment
summap :: (Int -> Int) -> (Int -> Int)
summap f n = f 10
{-# NOINLINE summap #-}

main = do
  n <- length `fmap` getArgs
  print $ summap (+ n) n

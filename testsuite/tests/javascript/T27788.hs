module Main where

import Control.Exception (evaluate)
import System.Environment (getArgs)

{-# NOINLINE mkInner #-}
mkInner :: Int -> (Int, Int)
mkInner n = (n + 1, n + 2)

{-# NOINLINE mkOuter #-}
mkOuter :: (Int, Int) -> Int -> ((Int, Int), Int)
mkOuter a n = (a, n)

main :: IO ()
main = do
  n <- length <$> getArgs
  let inner = mkInner n
      outer = mkOuter inner n
      (inner', _) = outer
      (p, _) = inner
  _ <- evaluate inner'
  print (fst inner')
  print (fst inner)
  print p
  print (snd outer)

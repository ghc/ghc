-- The control for JoinPointLoopResume.hs: the same program with
-- -fno-bc-join-points-as-labels, where the interrupted thread is suspended at
-- an ordinary BCO entry instead of at a resume frame.
{-# OPTIONS_GHC -fno-full-laziness #-}
module Main (main) where

import Control.Exception (evaluate)
import System.Timeout (timeout)

import LoopSlow (grow)

main :: IO ()
main = do
  x <- interrupted 20
  print x

interrupted :: Int -> IO Int
interrupted n = do
  let x = grow 4000000
  r <- timeout 2000 (evaluate x)
  case r of
    Nothing -> return x
    Just _ | n > 0     -> interrupted (n - 1)
           | otherwise -> return x

{-# LANGUAGE CPP          #-}
{-# LANGUAGE BangPatterns #-}
module Main
  ( main -- :: IO ()
  ) where

import           Criterion.Main (defaultMain, bgroup)

import qualified Micro        as Micro
import qualified SimpleVersus as Versus

--------------------------------------------------------------------------------

-- A simple driver, for running every set of benchmarks.
main :: IO ()
main = defaultMain
  [ bgroup "micro"  Micro.benchmarks
  , bgroup "versus" Versus.benchmarks
  ]

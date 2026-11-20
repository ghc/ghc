{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main where

import GHC.Sigma.Projection (second)
import NativeCSEProjectionLib (lazyWitness)

main :: IO ()
main = let pair = lazyWitness in print (second pair)

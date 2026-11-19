{-# LANGUAGE NoExperimentalDependentTypes #-}

module Main where

import NativeLocalOpenProducer (Backward, Forward, Prior)

forward :: Forward Char
forward = 'x'

backward :: Backward Bool Char
backward = True

prior :: Prior Int
prior = 7

main :: IO ()
main = print (forward, backward, prior)

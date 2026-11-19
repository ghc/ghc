{-# LANGUAGE NoExperimentalDependentTypes #-}

module Main where

import NativeLocalOpenProducer (Apply, Backward, Forward, Prior)

forward :: Forward Char
forward = 'x'

backward :: Backward Bool Char
backward = True

prior :: Apply Maybe (Prior Int)
prior = Just 7

main :: IO ()
main = print (forward, backward, prior)

{-# LANGUAGE ImplicitParams #-}

-- Killed GHC 5.04.1

module Main where

type CTPar = ([Double],Int)

us :: (?ctPar :: CTPar) => [Double]
us = let (d,_) = ?ctPar in d

main = let ?ctPar = ([3.4],2) in print us 

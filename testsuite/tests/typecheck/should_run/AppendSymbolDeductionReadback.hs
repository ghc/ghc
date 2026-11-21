{-# LANGUAGE GHC2024 #-}
module Main where

import AppendSymbolDeductionProvider

prefix :: Mark "fix"
prefix = removePrefix @"fix" (Mark 47)

suffix :: Mark "pre"
suffix = removeSuffix @"pre" (Mark 53)

main :: IO ()
main = case (prefix, suffix) of
    (Mark a, Mark b) -> print (a, b)

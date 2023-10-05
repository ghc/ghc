{-# LANGUAGE OverloadedStrings #-}
module T12921 (stat) where

{-# ANN module "HLint: ignore Reduce duplication" #-}

stat :: Int -> Int
stat = choice []

-- 'choice' is deliberately out of scope in this test

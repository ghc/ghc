{-# LANGUAGE BangPatterns #-}
-- Test that SLIDE works correctly when it crosses a stack chunk boundary.
-- See #27001.
module Main where

go :: Int -> Double -> Double
go 0 !acc = acc
go n !acc = go (n - 1) (acc + 1.0)

result :: Double
result = go 100000 0.0

main :: IO ()
main = print result

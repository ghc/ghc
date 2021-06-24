{-# OPTIONS_GHC -O2 -ddump-rule-firings #-}

module T20021M where

import T20021

test :: Int -> Int
test n = length ([0..(10^n)] :: [Int])

main :: IO ()
main = putStrLn $ show $ test 3

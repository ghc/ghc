{-# OPTIONS_GHC -O2 -ddump-rule-firings #-}
-- There should be multiple rules firing from modules such as GHC.Real
module T20021M where

import T20021

test :: Int -> Int
test n = length ([0..(10^n)] :: [Int])

main :: IO ()
main = putStrLn $ show $ test 3

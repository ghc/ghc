{-# LANGUAGE ScopedTypeVariables #-}

module Main
where

import TH_repPatSig_asserts

assertFoo [d| foo :: Int -> Int
              foo (x :: Int) = x
            |]

assertCon [| \(x :: Either Char Int -> (Char, Int)) -> x |]

assertVar [| \(x :: Maybe a) -> case x of Just y -> (y :: a) |]

main :: IO ()
main = return ()


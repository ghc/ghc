{-# LANGUAGE ImplicitParams #-}

module Main where

import Data.Kind ( Constraint )

type IPInt =
  ( (?ipInt :: Int)
  , ( () :: Constraint) -- Comment this out and main prints 7 instead of 3 on GHC 9.8
  )

hasIPInt :: IPInt => Int
hasIPInt = ?ipInt

let_ipInt_7_in :: IPInt => (IPInt => r) -> r
let_ipInt_7_in x = let ?ipInt = 7 in x

main :: IO ()
main = print (let ?ipInt = 3 in let_ipInt_7_in hasIPInt)
  -- the 7 in 'let_ipInt_eq_7_in' should shadow the outer 3

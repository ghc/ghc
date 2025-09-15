-- | Test that GHC produces links to the Haskell Foundation Error Index Pretty
-- straight forward, we just induce a type error and track the link as a golden
-- test.

module Main where

main = 1 + "hello HF! from GHC"

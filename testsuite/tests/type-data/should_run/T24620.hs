{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeData #-}

module Main where

type data Nat = Zero | Succ Nat

main :: IO ()
main = pure ()

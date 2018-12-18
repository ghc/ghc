{-# LANGUAGE TemplateHaskell, TypeOperators, DataKinds #-}

module T15362 where

data Nat = Zero | Succ Nat

$( [d| type family a + b where
         Maybe Zero b = b
         Succ a + b = Succ (a + b) |] )

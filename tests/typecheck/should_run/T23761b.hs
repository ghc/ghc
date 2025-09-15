{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Main where

class    c => IdCt c
instance c => IdCt c

type IPInt = IdCt (?ipInt :: Int)

hasIPInt :: IPInt => Int
hasIPInt = ?ipInt

let_ipInt_eq_7_in :: IPInt => (IPInt => r) -> r
let_ipInt_eq_7_in x = let ?ipInt = 7 in x

test :: Int
test = let ?ipInt = 3 in let_ipInt_eq_7_in hasIPInt

main :: IO ()
main = print test
  -- the 7 in 'let_ipInt_eq_7_in' should shadow the outer 3

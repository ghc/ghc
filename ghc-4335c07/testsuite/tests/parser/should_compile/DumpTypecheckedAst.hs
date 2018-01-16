{-# LANGUAGE DataKinds, PolyKinds, TypeOperators, TypeFamilies #-}

module DumpTypecheckedAst where

data Peano = Zero | Succ Peano

type family Length (as :: [k]) :: Peano where
  Length (a : as) = Succ (Length as)
  Length '[]      = Zero

main = putStrLn "hello"

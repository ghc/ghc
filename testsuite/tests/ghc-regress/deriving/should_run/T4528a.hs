-- Crashed older GHCs when loaded into GHCi

module Main where

data T a = A | B | C deriving( Enum, Show )

main = print [A ..]

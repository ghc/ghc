-- Run with -funbox-strict-fields
-- Bug in GHC 5.04.3

module Main where

data Foo = Foo Int String
data Bar = Bar Int Foo

instance Ord Bar where 
    compare (Bar i _) (Bar j _) = compare i j

instance Eq Bar where
    (Bar i _) == (Bar j _) = i == j


data Zot = Zot !Bar !String deriving (Ord,Eq)

main = putStrLn "Success"

--!!! buggy deriving with function type, reported by Sigbjorn Finne
module ShouldSucceed where

data Foo = Foo (Int -> Int) deriving Eq

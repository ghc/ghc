-- !!! buggy deriving with function type, reported by Sigbjorn Finne
module ShouldFail where

data Foo = Foo (Int -> Int) deriving Eq

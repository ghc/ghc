--!!! buggy deriving with function type, reported by Sigbjorn Finne

data Foo = Foo (Int -> Int) deriving Eq

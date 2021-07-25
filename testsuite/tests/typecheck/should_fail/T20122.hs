module T20122 where

class Testable a where
    test :: a -> Bool

data A = A

instance Testable A where
    test _ = True

foo = test

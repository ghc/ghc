-- A missing kind check made GHC 6.4 crash on this one

module ShouldFail where

class Foo f where
        baa :: f a -> f

instance Foo Maybe where
        baa z = z

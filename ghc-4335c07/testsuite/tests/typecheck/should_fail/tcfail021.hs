-- !!! Illegally giving methods in a pattern binding (for no v good reason...)

module ShouldFail where

data Foo = MkFoo Int

instance Eq Foo where
    ((==), (/=)) = (\x -> \y -> True, \x -> \y -> False)

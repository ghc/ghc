module ShouldFail where

data Foo = MkFoo Bool

instance Eq Foo where
    (MkFoo x) == (MkFoo y) = x == y

instance Eq Foo where
    -- forgot to type "Ord" above
    (MkFoo x) <= (MkFoo y) = x <= y

